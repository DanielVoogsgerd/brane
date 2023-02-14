//  RESOLVE.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 17:46:03
//  Last edited:
//    14 Feb 2023, 15:54:21
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the traversal that will resolve the identifiers to entries
//!   in a symbol table.
// 

use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use enum_debug::EnumDebug as _;
use log::warn;

use specifications::version::Version;

pub use crate::errors::ResolveError as Error;
use crate::errors::DslError;
pub use crate::warnings::ResolveWarning as Warning;
use crate::warnings::{DslWarning, Warning as _};
use crate::ast::types::DataType;
use crate::ast::symbol_tables::{ClassEntry, ClassEntryMember, DelayedEntry, DelayedEntryPtr, ExternalFuncEntry, LocalFuncEntry, PackageEntry, SymbolTable, VarEntry};
use crate::ast::auxillary::Identifier;
use crate::ast::expressions::{BinaryOperatorKind, Block, Expression, ExpressionKind, LiteralKind};
use crate::ast::statements::{self, ClassMemberDefKind, FunctionDef, Statement, StatementKind};
use crate::ast::toplevel::Program;
use crate::compiler::annot_stack::AnnotationStack;


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use brane_shr::utilities::test_on_dsl_files;
    use crate::compile_module;
    use crate::warnings::PrettyWarning as _;
    use crate::compiler::traversals::{print_ast, CompilerPhase};
    use super::{DslWarning, Program};


    /// Tests the parser by print all files
    #[test]
    fn test_resolve() {
        test_on_dsl_files("BraneScript", |path: PathBuf, raw: String| {
            println!("{}", (0..80).map(|_| '-').collect::<String>());
            println!("File '{}' gave us:", path.display());

            // Run the traversals up to this one
            let file: String = path.display().to_string();
            let (ast, warns): (Program, Vec<DslWarning>) = match compile_module(&file, &raw, CompilerPhase::Resolve) {
                Ok(res)  => res,
                Err(err) => {
                    // Prettyprint the errors
                    eprintln!("{}", err.display());
                    panic!("Failed to compile the file (see the output above)");
                },
            };

            // Print any warnings
            for w in warns {
                w.display_with_source(&path.display().to_string(), &raw);
            }

            // Print the symbol tables
            print_ast::traverse_st(&mut std::io::stdout(), &ast).unwrap();
            println!("{}\n\n", (0..80).map(|_| '-').collect::<String>());
        });
    }
}





/***** HELPER FUNCTIONS *****/
/// Recursively replaces the given expression with a `LocalFunctionRef` if it's a `VarRef`.
/// 
/// # Arguments
/// - `expr`: The expression to replace varrefs in.
/// - `errors`: A list that we will populate with errors if they occur. If they do, then this function might early quit before properly traversing the tree (since it is malformed anyway).
fn funcify(expr: &mut Expression, errors: &mut Vec<Error>) {
    // Match on the expression
    use ExpressionKind::*;
    match &mut expr.kind {
        // Replace if it's a variable reference
        VarRef{ name, .. } => {
            // Pull the name
            let mut fname: Identifier = Identifier{ name: String::new(), range: None };
            mem::swap(&mut fname, name);

            // Update the expression
            // Note: we can leave the entry empty, since we assume this pass happens _before_ we resolve the entries
            expr.kind = ExpressionKind::LocalFunctionRef{ name: fname, st_entry: None };
        },

        // Checks out if it's projection (because then we are referencing a variable again) or a function reference of any kind
        Proj{ .. }                |
        LocalFunctionRef{ .. }    |
        ExternalFunctionRef{ .. } => {},

        // For the rest, we will emit an error
        _ => { errors.push(Error::IllegalFunctionExpr{ variant: expr.kind.variant().to_string(), range: expr.range }); },
    }
}

/// Recursively asserts the given expression is suitable for an assignment.
/// 
/// # Arguments
/// - `expr`: The expression to replace varrefs in.
/// - `errors`: A list that we will populate with errors if they occur. If they do, then this function might early quit before properly traversing the tree (since it is malformed anyway).
fn assert_assign(expr: &Expression, errors: &mut Vec<Error>) {
    // Match on the expression
    use ExpressionKind::*;
    match &expr.kind {
        // Operators we expect
        Index { to_index, .. } => {
            assert_assign(to_index, errors);
        },
        Proj { to_proj, .. } => {
            assert_assign(to_proj, errors);
        },

        // Checks out if it's a variable reference
        VarRef{ .. } => {},

        // For the rest, we will emit an error
        _ => { errors.push(Error::IllegalAssignExpr{ variant: expr.kind.variant().to_string(), range: expr.range }) },
    }
}





/***** TRAVERSAL FUNCTIONS *****/
/// Traverses a statement to find if it has any declaration and/or whether we can resolve any.
/// 
/// # Arguments
/// - `stmt`: The Statement to traverse.
/// - `table`: The SymbolTable of the current scope that we need to declare any new declarations in.
/// -` stack`: The AnnotationStack that we use to keep track of active annotations.
/// - `warnings`: A list that we will populate with warnings if they occur.
/// - `errors`: A list that we will populate with errors if they occur. If they do, then this function might early quit before properly traversing the tree (since it is malformed anyway).
pub fn trav_stmt(stmt: &mut Statement, table: &Rc<RefCell<SymbolTable>>, stack: &mut AnnotationStack, warnings: &mut Vec<Warning>, errors: &mut Vec<Error>) {
    // Match on the statement
    use StatementKind::*;
    match &mut stmt.kind {
        // Definitions
        Import { name, version, st_entry } => {
            // Push to the stack
            stack.push(&stmt.annots);

            // Do nothing if we've already processed this import.
            if st_entry.is_some() { return; }

            // Resolve the version
            let version: Version = if let Some(version) = version {
                // Extract the numbers
                let major : i64 = if let LiteralKind::Integer{ value } = version.0.kind { value } else { unreachable!(); };
                let minor : i64 = if let LiteralKind::Integer{ value } = version.1.kind { value } else { unreachable!(); };
                let patch : i64 = if let LiteralKind::Integer{ value } = version.2.kind { value } else { unreachable!(); };

                // Mold that into a version
                Version::new(major as u64, minor as u64, patch as u64)
            } else {
                Version::latest()
            };

            // Generate the entry
            let entry: PackageEntry = PackageEntry {
                name : name.name.clone(),
                version,

                // Note that these are always phantom until linking, since we don't have the package metadata before then
                funcs   : DelayedEntry::Phantom(HashMap::new()),
                classes : DelayedEntry::Phantom(HashMap::new()),

                range : stmt.range,
            };

            // Register it
            let ptr: DelayedEntryPtr<PackageEntry> = DelayedEntryPtr::resolved(entry);
            *st_entry = Some(ptr.clone());
            {
                let mut table: RefMut<SymbolTable> = table.borrow_mut();

                // Assert the package does not already exist
                if let Some(prev) = table.packages.get(&name.name) {
                    // Only emit if not phantom to avoid confusing warning messages about duplicate declarations and then not declared
                    // Note that we make imports imperative, so we don't fix missing declarations using later imports.
                    let prev: Ref<DelayedEntry<PackageEntry>> = prev.borrow();
                    if !prev.is_phantom() {
                        let warn: Warning = Warning::DuplicatePackageImport{ name: name.name.clone(), range: stmt.range, prev: prev.range };
                        if !stack.is_allowed(warn.code()) { warnings.push(warn); }
                    }
                } else {
                    // Otherwise we add it
                    table.packages.insert(name.name.clone(), ptr);
                }
            }
        },

        FunctionDef(def) => {
            trav_func_def(def, table, stack, warnings, errors);
        },

        ClassDef { name, defs, st_entry } => {
            // Do nothing if we've already processed this class.
            if let Some(entry) = st_entry {
                // Well, I say nothing, but we do recurse in methods to find other things we can update
                for def in defs {
                    match &def.kind {
                        ClassMemberDefKind::Method(def) => {
                            trav_block(&mut def.body, table, false, stack, warnings, errors);
                        }
                        _ => {},
                    }
                }
                return;
            }

            // Create the entries for the definitions
            let mut entry_defs : HashMap<String, ClassEntryMember> = HashMap::new();
            for d in defs.iter() {
                // Push the definition's annotations
                stack.push(&d.annots);

                // Match on the definition itself
                match &d.kind {
                    ClassMemberDefKind::Property { name: pname, data_type } => {
                        // Assert it does not already exist
                        if let Some(prev) = entry_defs.get(&pname.name) {
                            let warn: Warning = Warning::DuplicateClassMemberDefinition{ name: pname.name.clone(), class: name.name.clone(), range: d.range, prev: prev.range(), prev_variant: prev.what() };
                            if !stack.is_allowed(warn.code()) { warnings.push(warn); }
                        } else {
                            entry_defs.insert(pname.name.clone(), ClassEntryMember::Property(VarEntry {
                                name      : pname.name.clone(),
                                data_type : data_type.data_type.clone(),

                                shadowed : false,

                                range : d.range,
                            }));
                        }
                    },
                    ClassMemberDefKind::Method(def) => {
                        // Assert it does not already exist
                        if let Some(prev) = entry_defs.get(&def.name.name) {
                            let warn: Warning = Warning::DuplicateClassMemberDefinition{ name: def.name.name.clone(), class: name.name.clone(), range: def.range, prev: prev.range(), prev_variant: prev.what() };
                            if !stack.is_allowed(warn.code()) { warnings.push(warn); }
                        } else {
                            entry_defs.insert(def.name.name.clone(), ClassEntryMember::Method(LocalFuncEntry {
                                name     : def.name.name.clone(),
                                args     : def.args.iter().map(|a| DelayedEntryPtr::resolved(VarEntry{ name: a.name.name.clone(), data_type: a.data_type.data_type.clone(), shadowed: false, range: a.range })).collect(),
                                ret_type : def.ret.data_type.clone(),

                                range : def.range,
                            }));
                        }
                    },

                    // These should not occur anymore
                    ClassMemberDefKind::Annotation { .. }       |
                    ClassMemberDefKind::ParentAnnotation { .. } => { unreachable!(); },
                }

                // Pop the annotations
                stack.pop();
            }

            // Create the class entry around it
            let class: ClassEntry = ClassEntry {
                name : name.name.clone(),
                defs : entry_defs,

                range : stmt.range,
            };

            // Define it in the symbol table
            {
                let mut table: RefMut<SymbolTable> = table.borrow_mut();

                // Assert the class does not already exist
                if let Some(prev_ptr) = table.classes.get(&name.name) {
                    // If it's phantom, then mark as resolved instead with this definition
                    let mut prev: RefMut<DelayedEntry<ClassEntry>> = prev_ptr.borrow_mut();
                    match &*prev {
                        DelayedEntry::Resolved(prev) => {
                            let warn: Warning = Warning::DuplicateClassDefinition{ name: name.name.clone(), range: stmt.range, prev: prev.range };
                            if !stack.is_allowed(warn.code()) { warnings.push(warn); }
                        },

                        DelayedEntry::Phantom(_) => {
                            *prev = DelayedEntry::Resolved(class);
                        }
                    }

                    // Set this entry for ourselves as well
                    *st_entry = Some(prev_ptr.clone());
                } else {
                    // Otherwise, add it
                    let class: DelayedEntryPtr<ClassEntry> = DelayedEntryPtr::resolved(class);
                    *st_entry = Some(class.clone());
                    table.classes.insert(name.name.clone(), class);
                }
            }

            // With that complete, do a full pass of the methods
            {
                for d in defs {
                    // Skip non-methods
                    let def: &mut statements::FunctionDef = match &mut d.kind {
                        ClassMemberDefKind::Method(d) => d,
                        _                             => { continue; },
                    };

                    // Convert the arguments to entries
                    let mut args: Vec<(String, DelayedEntryPtr<VarEntry>)> = Vec::with_capacity(def.args.len());
                    for (i, a) in def.args.iter().enumerate() {
                        // Assert the first one is self
                        if i == 0 && a.name.name != "self" {
                            errors.push(Error::MethodWithNonSelf{ name: a.name.name.clone(), arg: a.range, func: def.range, class: stmt.range });
                        }

                        // Add the pointer equivalent
                        args.push((
                            a.name.name.clone(),
                            DelayedEntryPtr::resolved(VarEntry {
                                name      : a.name.name.clone(),
                                data_type : a.data_type.data_type.clone(),

                                shadowed : false,

                                range : a.range,
                            }),
                        ));
                    }
                    if def.args.is_empty() { errors.push(Error::MethodWithoutSelf{ name: def.name.name.clone(), func: def.range, class: stmt.range }); }

                    // Add the arguments to the symbol table of the body, after which we can recurse
                    def.body.table.borrow_mut().vars.extend(args);
                    trav_block(&mut def.body, table, false, stack, warnings, errors);
                }
            }
        },

        VarDef { name, data_type, value, st_entry, .. } => {
            // Recurse into the value, if any
            if let Some(value) = value { trav_expr(value, table, stack, warnings, errors); }

            // Do nothing else if we've already processed this let.
            if st_entry.is_some() { return; }

            // Create an entry for the variable
            let entry: DelayedEntryPtr<VarEntry> = DelayedEntryPtr::resolved(VarEntry {
                name      : name.name.clone(),
                data_type : data_type.data_type.clone(),

                shadowed : false,

                range : stmt.range,
            });

            // Store it
            {
                let mut table: RefMut<SymbolTable> = table.borrow_mut();

                // Make sure it is unique
                *st_entry = Some(entry.clone());
                if let Some(prev) = table.vars.get(&name.name) {
                    // We first changed the old one to be shadowed, and we change its name to have the entries co-exist
                    let new_name: String = {
                        let mut prev: RefMut<DelayedEntry<VarEntry>> = prev.borrow_mut();
                        prev.name     = format!("{}'", prev.name);
                        prev.shadowed = true;
                        prev.name.clone()
                    };

                    // Now rename the old one
                    let prev: DelayedEntryPtr<VarEntry> = table.vars.remove(&name.name).unwrap();
                    table.vars.insert(new_name, prev);
                }

                // We can add it now without worries
                table.vars.insert(name.name.clone(), entry);
            }
        },



        // Control flow
        For { name, start, stop, block, st_entry, .. } => {
            // Recurse into the expressions first
            trav_expr(start, table, stack, warnings, errors);
            trav_expr(stop, table, stack, warnings, errors);

            // Only annotate new things if we haven't already
            if st_entry.is_none() {
                // Create the iterator variable entry
                let entry: DelayedEntryPtr<VarEntry> = DelayedEntryPtr::resolved(VarEntry {
                    name      : name.name.clone(),
                    data_type : DataType::Integer,

                    shadowed : false,

                    range : name.range,
                });

                // Note it down in the block's table
                *st_entry = Some(entry.clone());
                {
                    let mut btable: RefMut<SymbolTable> = block.table.borrow_mut();

                    // It's empty, so trivial
                    if !btable.vars.is_empty() { warn!("For-loop block table is not empty"); }
                    btable.vars.insert(name.name.clone(), entry);
                }
            }

            // Recurse into the block with this table
            trav_block(block, table, true, stack, warnings, errors);
        },

        While { cond, block } => {
            // Recurse into the expression first
            trav_expr(cond, table, stack, warnings, errors);
            // Then recurse into the block
            trav_block(block, table, true, stack, warnings, errors);
        },

        Return { value } => {
            if let Some(value) = value { trav_expr(value, table, stack, warnings, errors); }
        },



        // Miscellaneous
        Expression(expr) => {
            trav_expr(expr, table, stack, warnings, errors);
        },

        // Should not occur anymore
        Annotation{ .. }       |
        ParentAnnotation{ .. } => { unreachable!(); },
    }

    // Don't forget to pop the stack again
    stack.pop();
}

/// Traverses a function definition to add its entry and resolve the entries in its own table.
/// 
/// # Arguments
/// - `def`: The FunctionDef to traverse.
/// - `table`: The SymbolTable of the current scope that we need to declare any new declarations in.
/// -` stack`: The AnnotationStack that we use to keep track of active annotations.
/// - `warnings`: A list that we will populate with warnings if they occur.
/// - `errors`: A list that we will populate with errors if they occur. If they do, then this function might early quit before properly traversing the tree (since it is malformed anyway).
fn trav_func_def(def: &mut FunctionDef, table: &Rc<RefCell<SymbolTable>>, stack: &mut AnnotationStack, warnings: &mut Vec<Warning>, errors: &mut Vec<Error>) {
    // We only generate the entries if we didn't already
    if def.st_entry.is_none() {
        // Create the entries first; arguments...
        let args: Vec<DelayedEntryPtr<VarEntry>> = def.args.iter().map(|a| DelayedEntryPtr::resolved(VarEntry {
            name      : a.name.name.clone(),
            data_type : a.data_type.data_type.clone(),

            shadowed : false,

            range : a.range,
        })).collect();

        // ...then the function itself
        let func: LocalFuncEntry = LocalFuncEntry {
            name     : def.name.name.clone(),
            args     : args.clone(),
            ret_type : def.ret.data_type.clone(),

            range : def.range,
        };

        // Next up, register the function entry in the table and this def
        {
            let mut table: RefMut<SymbolTable> = table.borrow_mut();

            // Assert the function does not already exist
            if let Some(prev_ptr) = table.funcs.get(&def.name.name) {
                // If it's phantom, then mark as resolved instead with this definition
                let mut prev: RefMut<DelayedEntry<LocalFuncEntry>> = prev_ptr.borrow_mut();
                match &*prev {
                    DelayedEntry::Resolved(prev) => {
                        let warn: Warning = Warning::DuplicateFuncDefinition{ name: def.name.name.clone(), range: def.range, prev: prev.range };
                        if !stack.is_allowed(warn.code()) { warnings.push(warn); }
                    },

                    DelayedEntry::Phantom(_) => {
                        *prev = DelayedEntry::Resolved(func);
                    }
                }

                // Set this entry for ourselves as well
                def.st_entry = Some(prev_ptr.clone());
            } else {
                // Otherwise, add it
                let func: DelayedEntryPtr<LocalFuncEntry> = DelayedEntryPtr::resolved(func);
                def.st_entry = Some(func.clone());
                table.funcs.insert(def.name.name.clone(), func);
            }
        }

        // Then we add the arguments to the symbol table of the body, after which we can recurse
        def.body.table.borrow_mut().vars.extend(args.into_iter().map(|a| {
            // Get the name, then return (done to avoid lifetime issues)
            let name: String = { a.borrow().name.clone() };
            (name, a)
        }));
    }

    // We do always traverse, however
    trav_block(&mut def.body, table, false, stack, warnings, errors);
}



/// Traverse an expression to resolve any entries in its statements.
/// 
/// # Arguments
/// - `expr`: The Expression to traverse.
/// - `table`: The SymbolTable of the current scope that we need to declare any new declarations in.
/// -` stack`: The AnnotationStack that we use to keep track of active annotations.
/// - `warnings`: A list that we will populate with warnings if they occur.
/// - `errors`: A list that we will populate with errors if they occur. If they do, then this function might early quit before properly traversing the tree (since it is malformed anyway).
fn trav_expr(expr: &mut Expression, table: &Rc<RefCell<SymbolTable>>, stack: &mut AnnotationStack, warnings: &mut Vec<Warning>, errors: &mut Vec<Error>) {
    use ExpressionKind::*;
    match &mut expr.kind {
        // Statement-carrying expressions
        Block(block, annots) => {
            // Push the annotations for this block
            stack.push(annots.iter());
            // Do the block
            trav_block(block, table, true, stack, warnings, errors);
            // Pop the annotations again
            stack.pop();
        },

        If { cond, block, block_else, annots, annots_else } => {
            // Recurse into the condition
            trav_expr(cond, table, stack, warnings, errors);

            // Then do the blocks
            stack.push(annots.iter());
            trav_block(block, table, true, stack, warnings, errors);
            stack.pop();
            if let Some(block_else) = block_else {
                stack.push(annots_else.iter());
                trav_block(block_else, table, true, stack, warnings, errors);
                stack.pop();
            }
        },
        Parallel { branches, .. } => {
            // Do each of the branches
            for (b, a) in branches {
                stack.push(a.iter());
                trav_block(b, table, true, stack, warnings, errors);
                stack.pop();
            }
        },



        // Operators
        Cast { expr, .. } => {
            trav_expr(expr, table, stack, warnings, errors);
        },
        Discard { expr } => {
            trav_expr(expr, table, stack, warnings, errors);
        },

        Index { to_index, index } => {
            // Recurse them both
            trav_expr(to_index, table, stack, warnings, errors);
            trav_expr(index, table, stack, warnings, errors);
        },
        Proj { to_proj, .. } => {
            trav_expr(to_proj, table, stack, warnings, errors);
        },
        Call { to_call, args } => {
            // We can replace any varref in the call with funcrefs
            funcify(to_call, errors);

            // Recurse the call, then the arguments
            trav_expr(to_call, table, stack, warnings, errors);
            for a in args {
                trav_expr(a, table, stack, warnings, errors);
            }
        },



        // Arithmetic operators
        Unary { expr, .. } => {
            trav_expr(expr, table, stack, warnings, errors);
        },

        Binary { lhs, rhs, op } => {
            // If it's the assign operator, try to assert the LHS first
            if let BinaryOperatorKind::Assign = op.kind {
                assert_assign(lhs, errors);
            }

            // Then just recurse normally
            trav_expr(lhs, table, stack, warnings, errors);
            trav_expr(rhs, table, stack, warnings, errors);
        },



        // Values
        Array { elems } => {
            for e in elems {
                trav_expr(e, table, stack, warnings, errors);
            }
        },
        Instance { name, props, st_entry } => {
            // Attempt to find the class with this name
            {
                let mut table: RefMut<SymbolTable> = table.borrow_mut();
                if let Some(entry) = table.resolve_class(&name.name) {
                    // Set the entry only to link this instance
                    // Note that at this point we don't care yet if the entry is resolved or phantom.
                    *st_entry = Some(entry);
                } else {
                    // Create a new entry for this instance
                    let entry: DelayedEntryPtr<ClassEntry> = DelayedEntryPtr::phantom(ClassEntry {
                        name : name.name.clone(),
                        defs : HashMap::new(),

                        range : None,
                    });

                    // Set it internally to at least guarantee consistence between repeated calls.
                    *st_entry = Some(entry.clone());
                    table.classes.insert(name.name.clone(), entry);
                }
            }

            // With the class resolved, iterate over the property expressions
            for p in props {
                trav_expr(&mut p.value, table, stack, warnings, errors);
            }
        },

        VarRef { name, st_entry } => {
            // Attempt to search the table for an entry with this name
            let mut table: RefMut<SymbolTable> = table.borrow_mut();
            if let Some(entry) = table.resolve_var(&name.name) {
                // We set this entry internally to link it, not caring about phantom or resolved because we are cool like that
                *st_entry = Some(entry);
            } else {
                // We create a new entry
                let entry: DelayedEntryPtr<VarEntry> = DelayedEntryPtr::phantom(VarEntry {
                    name      : name.name.clone(),
                    data_type : DataType::Any,

                    shadowed : false,

                    range : None,
                });

                // We update it in ourselves and the table to guarantee consistency between usages.
                *st_entry = Some(entry.clone());
                table.vars.insert(name.name.clone(), entry);
            }
        },
        LocalFunctionRef { name, st_entry } => {
            // Attempt to search the table for an entry with this name
            let mut table: RefMut<SymbolTable> = table.borrow_mut();
            if let Some(entry) = table.resolve_func(&name.name) {
                // We set this entry internally to link it, not caring about phantom or resolved because we are cool like that
                *st_entry = Some(entry);
            } else {
                // We create a new entry
                let entry: DelayedEntryPtr<LocalFuncEntry> = DelayedEntryPtr::phantom(LocalFuncEntry {
                    name     : name.name.clone(),
                    args     : vec![],
                    ret_type : DataType::Any,

                    range : None,
                });

                // We update it in ourselves and the table to guarantee consistency between usages.
                *st_entry = Some(entry.clone());
                table.funcs.insert(name.name.clone(), entry);
            }
        },
        ExternalFunctionRef { name, package, st_entry } => {
            // Attempt to search the table for an entry with this name
            let mut table: RefMut<SymbolTable> = table.borrow_mut();
            if let Some(entry_ptr) = table.resolve_package(&package.name) {
                // We update the package with the function called if it not yet contained
                // Note: We don this even if resolved, because even though it itself is phantom, its functions are never at this stage.
                {
                    let mut entry: RefMut<DelayedEntry<PackageEntry>> = entry_ptr.borrow_mut();
                    if entry.funcs.is_phantom() && !entry.funcs.contains_key(&name.name) {
                        entry.funcs.insert(name.name.clone(), ExternalFuncEntry{
                            name     : name.name.clone(),
                            args     : vec![],
                            ret_type : DataType::Any,

                            package : entry_ptr.clone(),
                        });
                    }
                }
                *st_entry = Some(entry_ptr);
            } else {
                // We create a new entry for this package call
                let entry: DelayedEntryPtr<PackageEntry> = DelayedEntryPtr::phantom(PackageEntry {
                    name    : package.name.clone(),
                    version : Version::latest(),

                    funcs   : DelayedEntry::Phantom(HashMap::new()),
                    classes : DelayedEntry::Phantom(HashMap::new()),

                    range : None,
                });

                // Inject the function afterwards to be able to refer to the package entry itself
                let pptr: DelayedEntryPtr<PackageEntry> = entry.clone();
                {
                    entry.borrow_mut().funcs.insert(name.name.clone(), ExternalFuncEntry {
                        name     : name.name.clone(),
                        args     : vec![],
                        ret_type : DataType::Any,

                        package : pptr,
                    });
                }

                // We update it in ourselves and the table to guarantee consistency between usages.
                *st_entry = Some(entry.clone());
                table.packages.insert(name.name.clone(), entry);
            }
        },

        // Nothing to be done for the rest
        Literal(_) => {},
    }
}

/// Traverse a block to resolve any entries in its statements.
/// 
/// The statements will inherit the block's symbol table as new parent.
/// 
/// # Arguments
/// - `block`: The Block to traverse.
/// - `table`: The SymbolTable of the current scope that we need to declare any new declarations in.
/// - `nests`: Whether the block has access to the parent scope or nah.
/// -` stack`: The AnnotationStack that we use to keep track of active annotations.
/// - `warnings`: A list that we will populate with warnings if they occur.
/// - `errors`: A list that we will populate with errors if they occur. If they do, then this function might early quit before properly traversing the tree (since it is malformed anyway).
fn trav_block(block: &mut Block, table: &Rc<RefCell<SymbolTable>>, nests: bool, stack: &mut AnnotationStack, warnings: &mut Vec<Warning>, errors: &mut Vec<Error>) {
    // Set the parent symbol table for this block
    {
        let mut btable: RefMut<SymbolTable> = block.table.borrow_mut();
        btable.parent = Some(table.clone());
        btable.nests  = nests;
    }

    // Set this block as child in the given symbol table
    table.borrow_mut().childs.push(block.table.clone());

    // Recurse into the statements
    for s in &mut block.stmts {
        trav_stmt(s, &block.table, stack, warnings, errors);
    }
}





/***** LIBRARY *****/
/// Runs a full traversal on the given AST to resolve identifiers to symbol table entries _as much as possible_.
/// 
/// Note that, due to delayed linking, we have no choice but to accept unlinked variables. However, to fix this, the traversal is built such that it can be run repeatedly on the same workflow if more information becomes available. Put differently, the process in this traversal is "best-effort".
/// 
/// # Arguments
/// - `tree`: The AST to resolve.
/// - `warnings`: A list of DslWarnings to populate whenever an error occurs in this traversal.
pub fn traverse(tree: &mut Program, warnings: &mut Vec<DslWarning>) -> Result<(), Vec<DslError<'static>>> {
    // We start populating the program's symbol table
    let Program{ stmts, annots, table, .. } = tree;

    // Prepare the annotation stack
    let mut stack: AnnotationStack = AnnotationStack::new();
    stack.push(annots.iter());

    // Do all the statements to populate it
    let mut errs  : Vec<Error>   = vec![];
    let mut warns : Vec<Warning> = vec![];
    for s in stmts {
        trav_stmt(s, table, &mut stack, &mut warns, &mut errs);
    }

    // Done, return the warnings and errors (if any)
    warnings.extend(warns.into_iter().map(|w| w.into()));
    if errs.is_empty() {
        Ok(())
    } else {
        Err(errs.into_iter().map(|e| e.into()).collect())
    }
}
