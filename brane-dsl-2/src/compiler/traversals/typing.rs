//  TYPING.rs
//    by Lut99
// 
//  Created:
//    14 Feb 2023, 13:33:32
//  Last edited:
//    30 May 2023, 10:41:09
//  Auto updated?
//    Yes
// 
//  Description:
//!   Implements the traversal that will fill in all the type information
//!   we have.
// 

use std::cell::{Ref, RefMut};

use enum_debug::EnumDebug as _;
use log::{debug, trace};

pub use crate::errors::TypingError as Error;
pub use crate::warnings::TypingWarning as Warning;
use crate::errors::DslError;
use crate::warnings::{DslWarning, Warning as _};
use crate::ast::spec::TextRange;
use crate::ast::types::DataType;
use crate::ast::symbol_tables::{DelayedEntry, LocalClassEntry, LocalClassEntryMember, LocalFuncEntry, VarEntry};
use crate::ast::expressions::{Block, Expression, ExpressionKind};
use crate::ast::statements::{ClassMemberDefKind, FunctionDef, Statement, StatementKind};
use crate::ast::toplevel::Program;
use crate::compiler::utils::trace_trap;
use crate::compiler::annot_stack::AnnotationStack;


/***** TRAVERSAL FUNCTIONS *****/
/// Traverses a single [`Statement`].
/// 
/// # Arguments
/// - `stmt`: The [`Statement`] to traverse.
/// -` stack`: The [`AnnotationStack`] that we use to keep track of active annotations.
/// - `warnings`: A list of [`Warning`]s that we will populate as and if they occur.
/// - `errors`: A list of [`Error`]s that we will populate as and if they occur.
/// 
/// # Returns
/// A tuple with a boolean that indicates whether any type information was updated and a [`DataType`] indicating if this statement evalutes to a value (or, if [`None`], it does not). The former can be used to detect whether we've "out-deduced".
/// 
/// # Errors
/// This function may throw errors by pushing them to the given list of errors. The function will still continue if possible, however, in order to accumulate as much of them as possible.
fn trav_stmt(stmt: &mut Statement, stack: &mut AnnotationStack, warnings: &mut Vec<Warning>, errors: &mut Vec<Error>) -> (bool, Option<DataType>) {
    trace!(target: "typing", "Traversing {:?}", stmt.kind.variant());
    let _trap = trace_trap!(target: "typing", "Exiting {:?}", stmt.kind.variant());

    // Push the statement's annotations
    let mut stack = stack.frame(&stmt.annots);

    // Match on the specific kind of statement
    use StatementKind::*;
    match &mut stmt.kind {
        // Definitions
        Import { .. } => { /* Nothing to do */ (false, None) },

        FunctionDef(def) => {
            // This is not a method (hence the `None`)
            let changed: bool = trav_func_def(def, None, &mut *stack, warnings, errors);

            // Return whether we've changed, but a definition never evaluates
            (changed, None)
        },

        ClassDef { name: cname, defs, st_entry } => {
            // Go through the definitions to find if anything needs resolving there
            let mut changed: bool = false;
            for def in defs {
                // Push the annotations for these definitions onto the stack
                let mut stack = stack.frame(&def.annots);

                // Match on the kind itself
                match &mut def.kind {
                    ClassMemberDefKind::Property { name, data_type } => {
                        // Simply populate the property's type as necessary
                        let mut entry: RefMut<DelayedEntry<LocalClassEntry>> = st_entry.as_ref().unwrap().borrow_mut();
                        let pentry: &mut LocalClassEntryMember = entry.defs.get_mut(&name.name).unwrap_or_else(|| panic!("Property '{}' in class '{}' has not been assigned an entry after resolve phase", name.name, cname.name));
                        if let LocalClassEntryMember::Property(property) = pentry {
                            if property.data_type.is_any() {
                                property.data_type = data_type.data_type.clone();
                                changed = true;
                            } else if !data_type.data_type.is_any() && property.data_type != data_type.data_type {
                                panic!("Property '{}' already has a type at definition ({}) but that does not match annotated type ({})", name.name, property.data_type, data_type.data_type);
                            }
                        } else {
                            panic!("Property '{}' in class '{}' has been assigned a non-property member entry ({:?})", name.name, cname.name, pentry.variant());
                        }
                    },

                    ClassMemberDefKind::Method(method) => {
                        // For methods, we do the same as for normal functions - which is just calling this bad boy
                        changed |= trav_func_def(method, Some((DataType::Class(st_entry.as_ref().unwrap().borrow().name.clone()), cname.range)), &mut *stack, warnings, errors);
                    },

                    // Annotations shouldn't occur anymore
                    ClassMemberDefKind::Annotation { .. }       |
                    ClassMemberDefKind::ParentAnnotation { .. } => { unreachable!(); },
                }
            }

            // Done (a definition never evaluates)
            (changed, None)
        },

        VarDef { name, data_type, value, st_entry } => {
            // Attempt to resolve the variable definition
            let (mut var_changed, var_type): (bool, DataType) = {
                let mut entry: RefMut<DelayedEntry<VarEntry>> = st_entry.as_ref().unwrap().borrow_mut();
                let changed: bool = if entry.data_type.is_any() {
                    entry.data_type = data_type.data_type.clone();
                    true
                } else if !data_type.data_type.is_any() && entry.data_type != data_type.data_type {
                    panic!("Variable '{}' already has a type at definition ({}) but that does not match annotated type ({})", name.name, entry.data_type, data_type.data_type);
                } else {
                    false
                };
                (changed, entry.data_type.clone())
            };

            // Then, recurse into the value expression
            if let Some(value) = value {
                let (expr_changed, expr_type): (bool, DataType) = trav_expr(value, &mut *stack, warnings, errors);
                if var_type.is_any() {
                    let mut entry: RefMut<DelayedEntry<VarEntry>> = st_entry.as_ref().unwrap().borrow_mut();
                    entry.data_type = expr_type;
                    var_changed = true;
                } else if !expr_type.is_any() && var_type != expr_type {
                    errors.push(Error::VariableAssign { name: name.name.clone(), def_type: var_type, got_type: expr_type, source: st_entry.as_ref().unwrap().borrow().range, range: name.range });
                }

                // Then, recurse into the value expression (a definition never evaluates)
                (var_changed | expr_changed, None)
            } else {
                (var_changed, None)
            }
        },



        // Control flow
        For { name, start, stop, step, block, st_entry } => {
            // Resolve the loop variable
            let mut changed: bool = false;
            {
                let mut entry: RefMut<DelayedEntry<VarEntry>> = st_entry.as_ref().unwrap().borrow_mut();
                if entry.data_type.is_any() {
                    entry.data_type = DataType::Integer;
                    changed = true;
                } else if entry.data_type != DataType::Integer {
                    panic!("For-loop loop variable '{}' has non-Integer type {}", name.name, entry.data_type);
                }
            }

            // Next, recurse into each of the expressions:
            // start...
            let (echanged, etype): (bool, DataType) = trav_expr(start, &mut *stack, warnings, errors);
            changed |= echanged;
            if etype != DataType::Integer {
                errors.push(Error::VariableAssign { name: name.name.clone(), def_type: DataType::Integer, got_type: etype, source: name.range, range: start.range });
            }
            // ...stop...
            let (echanged, etype): (bool, DataType) = trav_expr(stop, &mut *stack, warnings, errors);
            changed |= echanged;
            if etype != DataType::Integer {
                errors.push(Error::VariableAssign { name: name.name.clone(), def_type: DataType::Integer, got_type: etype, source: name.range, range: stop.range });
            }
            // ...and step
            if let Some(step) = step {
                let (echanged, etype): (bool, DataType) = trav_expr(step, &mut *stack, warnings, errors);
                changed |= echanged;
                if etype != DataType::Integer {
                    errors.push(Error::VariableAssign { name: name.name.clone(), def_type: DataType::Integer, got_type: etype, source: name.range, range: step.range });
                }
            }

            // Now recurse into the block
            let (bchanged, btype): (bool, Option<(DataType, Option<TextRange>)>) = trav_block(block, &mut *stack, warnings, errors);
            if let Some((btype, brange)) = btype {
                // Only return if it returned non-void
                if !btype.is_any() && !btype.is_void() {
                    // Emit the warning if it's not surpressed
                    let warn: Warning = Warning::NonVoidBlock { got_type: btype, because_what: "for-loop", because: stmt.range, range: brange };
                    if stack.is_allowed(warn.code()) { warnings.push(warn); }

                    // Add the semicolon for the user instead to fix it (and avoid repetitions of this warning)
                    fix_nonvoid_block();
                    // NOTE: This does not count as a state change, as we can behave as if this has been applied all along
                }
            }

            // Done (a for-loop never evaluates)
            (changed | bchanged, None)
        },

        While { cond, block } => {
            // Recurse into the expresion first
            let (cchanged, ctype): (bool, DataType) = trav_expr(cond, &mut *stack, warnings, errors);
            if ctype != DataType::Boolean {
                errors.push(Error::WhileCondition { got_type: ctype, range: cond.range });
            }

            // Then recurse into the block
            let (bchanged, btype): (bool, Option<(DataType, Option<TextRange>)>) = trav_block(block, &mut *stack, warnings, errors);
            if let Some((btype, brange)) = btype {
                // Only return if it returned non-void
                if !btype.is_any() && !btype.is_void() {
                    // Emit the warning if it's not surpressed
                    let warn: Warning = Warning::NonVoidBlock { got_type: btype, because_what: "while-loop", because: stmt.range, range: brange };
                    if stack.is_allowed(warn.code()) { warnings.push(warn); }

                    // Add the semicolon for the user instead to fix it (and avoid repetitions of this warning)
                    fix_nonvoid_block();
                    // NOTE: This does not count as a state change, as we can behave as if this has been applied all along
                }
            }

            // Done (a while-loop never evaluates)
            (cchanged | bchanged, None)
        },

        Return { value } => {
            // Recurse into the return statement to process the value
            let changed: bool = if let Some(value) = value {
                trav_expr(value, &mut *stack, warnings, errors).0
            } else {
                false
            };

            // NOTE: We do nothing with the return value for the simple fact that we do return type checking in a separate pass.

            // Return (the `Return` never evalutes)
            (changed, None)
        },



        // Miscellaneous
        Expression(expr) => {
            // Evaluate the expression, which we can directly return (since the expression type _does_ evaluate)
            let (changed, data_type): (bool, DataType) = trav_expr(expr, &mut *stack, warnings, errors);
            (changed, Some(data_type))
        },

        // Should not occur anymore
        Annotation{ .. }       |
        ParentAnnotation{ .. } => { unreachable!(); },
    }
}

/// Traverses a function definition.
/// 
/// # Arguments
/// - `def`: The [`FunctionDef`] to traverse.
/// - `parent_class`: If not [`None`], then this function is a method of a class (who's type (i.e., [`DataType::Class(name)`]) is given).
/// -` stack`: The [`AnnotationStack`] that we use to keep track of active annotations.
/// - `warnings`: A list of [`Warning`]s that we will populate as and if they occur.
/// - `errors`: A list of [`Error`]s that we will populate as and if they occur.
/// 
/// # Returns
/// Whether any type information was updated or not. This can be used to detect whether we've "out-deduced".
/// 
/// # Errors
/// This function may throw errors by pushing them to the given list of errors. The function will still continue if possible, however, in order to accumulate as much of them as possible.
fn trav_func_def(def: &mut FunctionDef, mut parent_class: Option<(DataType, Option<TextRange>)>, stack: &mut AnnotationStack, warnings: &mut Vec<Warning>, errors: &mut Vec<Error>) -> bool {
    trace!(target: "typing", "Traversing FunctionDef");
    let _trap = trace_trap!(target: "typing", "Exiting FunctionDef");

    // Note: annotations have been processed on statement level (or class member level, if this is a method)

    // Resolve arguments
    let mut changed: bool = false;
    {
        let entry: Ref<DelayedEntry<LocalFuncEntry>> = def.st_entry.as_ref().unwrap().borrow();

        // There isn't anything to do if the arguments are emtpy
        if def.args.is_empty() { panic!("Missing 'self' argument for method '{}'", entry.name); }

        // Otherwise (or consequently), go through the rest of the arguments to assign their data types
        for (i, a) in def.args.iter_mut().enumerate() {
            // Propagate the argument's type
            let mut a_entry: RefMut<DelayedEntry<VarEntry>> = entry.args[i].borrow_mut();
            if a_entry.data_type.is_any() {
                a_entry.data_type = a.data_type.data_type.clone();
                changed = true;
            } else if !a.data_type.data_type.is_any() && a_entry.data_type != a.data_type.data_type {
                panic!("Argument {} (in method '{}') already has a type at definition ({}) but that does not match annotated type ({})", i, entry.name, a_entry.data_type, a.data_type.data_type);
            }

            // Now check if the first 'self' argument actually resolved to Any or the Class type
            if let Some((ctype, crange)) = parent_class.take() {
                if a_entry.name != "self" { panic!("Encountered non-self first argument '{}' in method '{}'", a_entry.name, entry.name); }
                if a_entry.data_type.is_any() {
                    // If it's still empty, we can resolve it to the class type
                    a_entry.data_type = ctype;
                    changed = true;
                } else if !ctype.is_any() && a_entry.data_type != ctype {
                    // Otherwise, we can error if it's not the class type
                    errors.push(Error::SelfInvalidType { method: entry.name.clone(), class_type: ctype, got_type: a_entry.data_type.clone(), class: crange, range: a.name.range });
                }
            }
        }
    }

    // Recurse into the body now we've resolved the arguments as much as possible
    for s in &mut def.body.stmts {
        changed |= trav_stmt(s, stack, warnings, errors).0;
    }

    // Return whether anything was updated
    changed
}



/// Traverses an expression, returning its evaluated type (if we have enough info to deduce it)
/// 
/// # Arguments
/// - `expr`: The [`Expression`] to traverse.
/// - `stack`: The [`AnnotationStack`] that we use to keep track of active annotations.
/// - `warnings`: A list of [`Warning`]s that we will populate as and if they occur.
/// - `errors`: A list of [`Error`]s that we will populate as and if they occur.
/// 
/// # Returns
/// A tuple with a boolean describing whether any type information was updated and the evaluated type (or [`DataType::Any`]). The first value can be used to detect whether we've "out-deduced".
/// 
/// # Errors
/// This function may throw errors by pushing them to the given list of errors. The function will still continue if possible, however, in order to accumulate as much of them as possible.
fn trav_expr(expr: &mut Expression, stack: &mut AnnotationStack, warnings: &mut Vec<Warning>, errors: &mut Vec<Error>) -> (bool, DataType) {
    trace!(target: "typing", "Traversing {:?}", expr.kind.variant());
    let _trap = trace_trap!(target: "typing", "Exiting {:?}", expr.kind.variant());

    // Done!
    (false, DataType::Any)
}

/// Traverses a block, returning its evaluated type if there was any.
/// 
/// # Arguments
/// - `clock`: The [`Block`] to traverse.
/// - `stack`: The [`AnnotationStack`] that we use to keep track of active annotations.
/// - `warnings`: A list of [`Warning`]s that we will populate as and if they occur.
/// - `errors`: A list of [`Error`]s that we will populate as and if they occur.
/// 
/// # Returns
/// A tuple with a boolean describing whether any type information was updated and the evaluated type (or [`None`] if this block did not contain any non-definition statements). The type information is itself a tuple of the type and which statement produces it. Note that if there _is_ a statement but it does not return anything, [`DataType::Void`] is returned instead.
/// 
/// # Errors
/// This function may throw errors by pushing them to the given list of errors. The function will still continue if possible, however, in order to accumulate as much of them as possible.
fn trav_block(block: &mut Block, stack: &mut AnnotationStack, warnings: &mut Vec<Warning>, errors: &mut Vec<Error>) -> (bool, Option<(DataType, Option<TextRange>)>) {
    trace!(target: "typing", "Traversing Block");
    let _trap = trace_trap!(target: "typing", "Exiting Block");

    // Note: annotations have been processed on statement/function level

    // These structures keep track of the return type of the entire block
    let mut ret: Option<(DataType, Option<TextRange>)> = None;

    // Simply recurse into the statements
    let mut changed: bool = false;
    for s in &mut block.stmts {
        let (schanged, stype): (bool, Option<DataType>) = trav_stmt(s, stack, warnings, errors);

        // Update the changed and the type
        changed |= schanged;
        if let Some(stype) = stype { ret = Some((stype, s.range)); }
    }

    // Done!
    (changed, ret)
}





/***** LIBRARY *****/
// Runs a full traversal on the given AST to resolve identifiers to symbol table entries _as much as possible_.
/// 
/// Note that, due to delayed linking, we have no choice but to accept unlinked variables. However, to fix this, the traversal is built such that it can be run repeatedly on the same workflow if more information becomes available. Put differently, the process in this traversal is "best-effort".
/// 
/// # Arguments
/// - `tree`: The AST to resolve.
/// - `warnings`: A list of DslWarnings to populate whenever an error occurs in this traversal.
pub fn traverse(tree: &mut Program, warnings: &mut Vec<DslWarning>) -> Result<(), Vec<DslError<'static>>> {
    debug!(target: "typing", "Starting traversal");

    let Program{ stmts, annots, .. } = tree;

    // Prepare the annotation stack
    let mut stack: AnnotationStack = AnnotationStack::new();
    stack.push(annots.iter());

    // Prepare warnings & errors lists
    let mut warnings : Vec<Warning> = vec![];
    let mut errors   : Vec<Error>   = vec![];

    // We start to traverse the tree to find as much as we can about type information on one hand, while making sure that what we find is consistent on the other.
    let mut changed: bool = true;
    while changed {
        // Go through all the statements again, hoping we won't do anything anymore
        changed = false;
        for s in stmts.iter_mut() {
            // Traverse into the statement
            let (schanged, stype): (bool, Option<DataType>) = trav_stmt(s, &mut stack, &mut warnings, &mut errors);
            changed |= schanged;

            // Throw a warning if this returns a non-void
            if let Some(stype) = stype {
                if !stype.is_any() && !stype.is_void() {
                    // Emit the warning if it's not surpressed
                    let warn: Warning = Warning::NonVoidStatement { got_type: stype, range: s.range };
                    if stack.frame(&s.annots).is_allowed(warn.code()) {
                        warnings.push(warn);
                    }

                    // Add the semicolon for the user instead to fix it (and avoid repetitions of this warning)
                    fix_nonvoid_stmt();
                    // NOTE: This does not count as a state change, as we can behave as if this has been applied all along
                }
            }
        }

        // Do return statement analysis
        for s in stmts.iter_mut() {
            // let (update_occurred, fully_returns): (bool, bool) = trav_stmt_ret(s, &mut stack, table);
            todo!();
        }
    }

    // Done!
    debug!(target: "typing", "Traversal complete");
    Ok(())
}
