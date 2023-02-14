//  ANNOTATIONS.rs
//    by Lut99
// 
//  Created:
//    13 Feb 2023, 11:54:08
//  Last edited:
//    14 Feb 2023, 10:05:48
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines a compiler traversal that will resolve annotations in the
//!   AST.
//!   
//!   Will remove them as separate statements, and instead annotate the desired
//!   statements with the value of the annotations (so effectively parses the)
//!   RawAnnotations). Additionally, might already prune parts of the AST to
//!   support conditional compilation.
// 

use std::mem;

pub use crate::warnings::AnnotationWarning as Warning;
use crate::warnings::DslWarning;
use crate::ast::spec::{self, Annotation, TextRange};
use crate::ast::expressions::{Block, Expression, ExpressionKind};
use crate::ast::statements::{ClassMemberDef, ClassMemberDefKind, Statement, StatementKind};
use crate::ast::toplevel::Program;


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
    fn test_annotations() {
        test_on_dsl_files("BraneScript", |path: PathBuf, raw: String| {
            println!("{}", (0..80).map(|_| '-').collect::<String>());
            println!("File '{}' gave us:", path.display());

            // Run the traversals up to this one
            let file: String = path.display().to_string();
            let (ast, warns): (Program, Vec<DslWarning>) = match compile_module(&file, &raw, CompilerPhase::Annotations) {
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

            // Print the tree
            print_ast::traverse(&mut std::io::stdout(), &ast).unwrap();
            println!("{}\n\n", (0..80).map(|_| '-').collect::<String>());
        });
    }
}





/***** TRAVERSAL FUNCTIONS *****/
/// Runs the traversal for a statement.
/// 
/// # Arguments
/// - `stmt`: The Statement to run the traversal for.
/// - `annot_buffer`: A temporary buffer of annotations we collected for the first statement we see.
/// - `range_buffer`: A temporary buffer of ranges matching the collected `annot_buffer`. Does not map one-to-one, but rather, can be used to emit errors about groups of annotations instead.
/// - `parent_annots`: The list of annotations to populate for _parent_ annotations.
/// - `warnings`: A list of warnings to populate.
/// 
/// # Returns
/// Returns whether this statement should be preserved (the same statement) or not (None).
fn trav_stmt(mut stmt: Statement, annot_buffer: &mut Vec<Annotation>, range_buffer: &mut Vec<Option<TextRange>>, parent_annots: &mut Vec<Annotation>, warnings: &mut Vec<Warning>) -> Option<Statement> {
    // Match on the statement
    use StatementKind::*;
    match &mut stmt.kind {
        // Annotations
        Annotation { annots } => {
            // Parse each of the annotations and add them to the temporary buffer for the next, non-annotation statement
            for a in annots {
                let (annots, warns): (Vec<spec::Annotation>, Vec<Warning>) = spec::Annotation::from_ref(a);
                annot_buffer.extend(annots);
                range_buffer.push(stmt.range);
                warnings.extend(warns);
            }

            // We don't want to keep this statement itself, though
            None
        },

        ParentAnnotation { annots } => {
            // Parse each of the annotations and add them to the parent statement
            for a in annots {
                let (annots, warns): (Vec<spec::Annotation>, Vec<Warning>) = spec::Annotation::from_ref(a);
                parent_annots.extend(annots);
                warnings.extend(warns);
            }

            // We don't want to keep this statement itself, though
            None
        },



        // Definitions
        Import { .. } => {
            // Inject the annotation buffer into the import
            stmt.annots = mem::take(annot_buffer);
            range_buffer.clear();

            // Nothing to recurse for an import, so just hit it
            Some(stmt)
        },

        FunctionDef(def) => {
            // Inject the annotation buffer into the function definition itself
            stmt.annots = mem::take(annot_buffer);
            range_buffer.clear();

            // Now recurse to find annotations in the function body
            trav_block(&mut def.body, annot_buffer, range_buffer, &mut stmt.annots, warnings);

            // We wanna keep the function definition (at least until we implement conditional compilation)
            Some(stmt)
        },

        ClassDef { defs, .. } => {
            // Inject the annotation buffer into the function definition itself
            stmt.annots = mem::take(annot_buffer);
            range_buffer.clear();

            // Recurse into the class definitions
            let old_defs: Vec<ClassMemberDef> = mem::take(defs);
            *defs = old_defs.into_iter().filter_map(|d| trav_member_def(d, annot_buffer, range_buffer, &mut stmt.annots, warnings)).collect();
            if !range_buffer.is_empty() {
                warnings.extend(range_buffer.drain(..).map(|r| Warning::UnusedAnnotation{ range: r }));
                annot_buffer.clear();
            }

            // We wanna keep the class definition (at least until we implement conditional compilation)
            Some(stmt)
        },

        VarDef { value, .. } => {
            // Inject the annotation buffer into the variable definition itself
            stmt.annots = mem::take(annot_buffer);
            range_buffer.clear();

            // Recurse into the expression
            if let Some(value) = value {
                trav_expr(value, annot_buffer, range_buffer, warnings);
            }

            // We wanna keep the variable definition (at least until we implement conditional compilation)
            Some(stmt)
        },



        // Control flow
        For { start, stop, block, .. } => {
            // Inject the annotation buffer into the for-loop itself
            stmt.annots = mem::take(annot_buffer);
            range_buffer.clear();

            // Recurse into the start & stop expressions, asserting that the buffers are empty afterwards
            trav_expr(start, annot_buffer, range_buffer, warnings);
            trav_expr(stop, annot_buffer, range_buffer, warnings);

            // Then recurse into the for's code
            trav_block(block, annot_buffer, range_buffer, &mut stmt.annots, warnings);

            // Then keep the statement
            Some(stmt)
        },

        While { cond, block } => {
            // Inject the annotation buffer into the for-loop itself
            stmt.annots = mem::take(annot_buffer);
            range_buffer.clear();

            // Recurse into the condition, asserting that the buffers are empty afterwards
            trav_expr(cond, annot_buffer, range_buffer, warnings);

            // Finally recurse into the for's code
            trav_block(block, annot_buffer, range_buffer, &mut stmt.annots, warnings);

            // Then keep the statement
            Some(stmt)
        },

        Return { value } => {
            // Inject the annotation buffer into the variable definition itself
            stmt.annots = mem::take(annot_buffer);
            range_buffer.clear();

            // Recurse into the expression
            if let Some(value) = value {
                trav_expr(value, annot_buffer, range_buffer, warnings);
            }

            // We wanna keep the variable definition (at least until we implement conditional compilation)
            Some(stmt)
        },



        // Miscellaneous
        Expression(expr) => {
            // Inject the annotation buffer into the variable definition itself
            stmt.annots = mem::take(annot_buffer);
            range_buffer.clear();

            // Recurse into the expression
            trav_expr(expr, annot_buffer, range_buffer, warnings);

            // We wanna keep the variable definition (at least until we implement conditional compilation)
            Some(stmt)
        },
    }
}

/// Runs the traversal for a class member definition.
/// 
/// # Arguments
/// - `def`: The `ClassMemberDef`initin to traverse.
/// - `annot_buffer`: A temporary buffer of annotations we collected for the first statement we see. Note that we assume these are already empty (but we re-use them for optimization purposes).
/// - `range_buffer`: A temporary buffer of ranges matching the collected `annot_buffer`. Does not map one-to-one, but rather, can be used to emit errors about groups of annotations instead. Note that we assume these are already empty (but we re-use them for optimization purposes).
/// - `parent_annots`: The list of annotations to populate for _parent_ annotations.
/// - `warnings`: A list of warnings to populate.
/// 
/// # Returns
/// Returns whether this definition should be preserved (the same definition) or not (None).
fn trav_member_def(mut def: ClassMemberDef, annot_buffer: &mut Vec<Annotation>, range_buffer: &mut Vec<Option<TextRange>>, parent_annots: &mut Vec<Annotation>, warnings: &mut Vec<Warning>) -> Option<ClassMemberDef> {
    // Match on the definition
    use ClassMemberDefKind::*;
    match &mut def.kind {
        // Annotations
        Annotation{ annots } => {
            // Parse each of the annotations and add them to the temporary buffer for the next, non-annotation statement
            for a in annots {
                let (annots, warns): (Vec<spec::Annotation>, Vec<Warning>) = spec::Annotation::from_ref(a);
                annot_buffer.extend(annots);
                range_buffer.push(def.range);
                warnings.extend(warns);
            }

            // We don't want to keep this statement itself, though
            None
        },

        ParentAnnotation{ annots } => {
            // Parse each of the annotations and add them to the parent statement
            for a in annots {
                let (annots, warns): (Vec<spec::Annotation>, Vec<Warning>) = spec::Annotation::from_ref(a);
                parent_annots.extend(annots);
                warnings.extend(warns);
            }

            // We don't want to keep this statement itself, though
            None
        },



        // Definitions
        Property{ .. } => {
            // Inject the annotation buffer into the property
            def.annots = mem::take(annot_buffer);
            range_buffer.clear();

            // Nothing to recurse for a property, so just return it
            Some(def)
        },

        Method(func_def) => {
            // Inject the annotation buffer into the function definition itself
            def.annots = mem::take(annot_buffer);
            range_buffer.clear();

            // Now recurse to find annotations in the function body
            trav_block(&mut func_def.body, annot_buffer, range_buffer, &mut def.annots, warnings);

            // We wanna keep the function definition (at least until we implement conditional compilation)
            Some(def)
        },
    }
}



/// Runs the traversal for an expression.
/// 
/// Note that this one just recurses to search for blocks.
/// 
/// # Arguments
/// - `expr`: The Expression to traverse.
/// - `annot_buffer`: A temporary buffer of annotations we collected for the first statement we see. Note that we assume these are already empty (but we re-use them for optimization purposes).
/// - `range_buffer`: A temporary buffer of ranges matching the collected `annot_buffer`. Does not map one-to-one, but rather, can be used to emit errors about groups of annotations instead. Note that we assume these are already empty (but we re-use them for optimization purposes).
/// - `warnings`: A list of warnings to populate.
fn trav_expr(expr: &mut Expression, annot_buffer: &mut Vec<Annotation>, range_buffer: &mut Vec<Option<TextRange>>, warnings: &mut Vec<Warning>) {
    // Switch on the expression
    use ExpressionKind::*;
    match &mut expr.kind {
        // Statement-carrying expressions
        Block(block, annots) => {
            trav_block(block, annot_buffer, range_buffer, annots, warnings);
        },

        If { cond, block, block_else, annots, annots_else } => {
            // Traverse the condition, first
            trav_expr(cond, annot_buffer, range_buffer, warnings);
            // Traverse the blocks
            trav_block(block, annot_buffer, range_buffer, annots, warnings);
            if let Some(block_else) = block_else { trav_block(block_else, annot_buffer, range_buffer, annots_else, warnings); }
        },
        Parallel { branches, .. } => {
            // Traverse each branch
            for (b, a) in branches {
                // Traverse the blocks
                trav_block(b, annot_buffer, range_buffer, a, warnings);
            }
        },



        // Operators
        Cast { expr, .. } => { trav_expr(expr, annot_buffer, range_buffer, warnings); },
        Discard { expr }  => { trav_expr(expr, annot_buffer, range_buffer, warnings); },

        Index { to_index, index } => {
            trav_expr(to_index, annot_buffer, range_buffer, warnings);
            trav_expr(index, annot_buffer, range_buffer, warnings);
        },
        Proj { to_proj, .. } => {
            trav_expr(to_proj, annot_buffer, range_buffer, warnings);
        },
        Call { to_call, args } => {
            trav_expr(to_call, annot_buffer, range_buffer, warnings);
            for a in args {
                trav_expr(a, annot_buffer, range_buffer, warnings);
            }
        },



        // Arithmetic operators
        Unary { expr, .. } => {
            trav_expr(expr, annot_buffer, range_buffer, warnings);
        },

        Binary { lhs, rhs, .. } => {
            trav_expr(lhs, annot_buffer, range_buffer, warnings);
            trav_expr(rhs, annot_buffer, range_buffer, warnings);
        },



        // Values
        Array { elems } => {
            for e in elems {
                trav_expr(e, annot_buffer, range_buffer, warnings);
            }
        },
        Instance { props, .. } => {
            for p in props {
                trav_expr(&mut p.value, annot_buffer, range_buffer, warnings);
            }
        },

        // The rest does not need to recurse
        VarRef { .. }              |
        LocalFunctionRef { .. }    |
        ExternalFunctionRef { .. } |
        Literal(_)                 => {},
    }
}

/// Runs the traversal for a codeblock.
/// 
/// # Arguments
/// - `block`: The Block to run the traversal for.
/// - `annot_buffer`: A temporary buffer of annotations we collected for the first statement we see. Note that we assume these are already empty (but we re-use them for optimization purposes).
/// - `range_buffer`: A temporary buffer of ranges matching the collected `annot_buffer`. Does not map one-to-one, but rather, can be used to emit errors about groups of annotations instead. Note that we assume these are already empty (but we re-use them for optimization purposes).
/// - `parent_annots`: The list of annotations to populate for _parent_ annotations.
/// - `warnings`: A list of warnings to populate.
fn trav_block(block: &mut Block, annot_buffer: &mut Vec<Annotation>, range_buffer: &mut Vec<Option<TextRange>>, parent_annots: &mut Vec<Annotation>, warnings: &mut Vec<Warning>) {
    // Hit it for the statements again
    let old_stmts: Vec<Statement> = mem::take(&mut block.stmts);
    block.stmts = old_stmts.into_iter().filter_map(|s| trav_stmt(s, annot_buffer, range_buffer, parent_annots, warnings)).collect();
    if !range_buffer.is_empty() {
        warnings.extend(range_buffer.drain(..).map(|r| Warning::UnusedAnnotation{ range: r }));
        annot_buffer.clear();
    }
}





/***** LIBRARY *****/
/// Runs a full traversal on the given AST to resolve annotations and prune parts that will not be compiled based on conditional compilation.
/// 
/// # Arguments
/// - `tree`: The AST to resolve.
/// - `warnings`: A list of DslWarning to populate whenever warnings occur in this traversal.
pub fn traverse(tree: &mut Program, warnings: &mut Vec<DslWarning>) {
    // We start populating the program's symbol table
    let Program{ stmts, annots, .. } = tree;

    // Prepare the statement by pulling it out the Program and the buffers
    let old_stmts: Vec<Statement> = mem::take(stmts);
    let (mut annot_buffer, mut range_buffer): (Vec<Annotation>, Vec<Option<TextRange>>) = (vec![], vec![]);

    // Run the convertion of each statement
    let mut warns: Vec<Warning> = vec![];
    *stmts = old_stmts.into_iter().filter_map(|s| trav_stmt(s, &mut annot_buffer, &mut range_buffer, annots, &mut warns)).collect();

    // Catch any annotations that have not been consumed
    if !range_buffer.is_empty() {
        warns.extend(range_buffer.drain(..).map(|r| Warning::UnusedAnnotation{ range: r }));
        annot_buffer.clear();
    }

    // Done, return the warnings
    warnings.extend(warns.into_iter().map(|w| w.into()));
}
