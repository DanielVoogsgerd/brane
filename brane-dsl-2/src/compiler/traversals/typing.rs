//  TYPING.rs
//    by Lut99
// 
//  Created:
//    14 Feb 2023, 13:33:32
//  Last edited:
//    23 May 2023, 11:48:20
//  Auto updated?
//    Yes
// 
//  Description:
//!   Implements the traversal that will fill in all the type information
//!   we have.
// 

use std::cell::RefCell;
use std::rc::Rc;

use enum_debug::EnumDebug as _;
use log::{debug, trace};

use crate::errors::DslError;
use crate::warnings::DslWarning;
use crate::ast::symbol_tables::SymbolTable;
use crate::ast::statements::{FunctionDef, Statement, StatementKind};
use crate::ast::toplevel::Program;
use crate::compiler::utils::trace_trap;
use crate::compiler::annot_stack::AnnotationStack;


/***** TRAVERSAL FUNCTIONS *****/
/// Traverses a single [`Statement`].
/// 
/// # Arguments
/// - `stmt`: The [`Statement`] to traverse.
/// -` stack`: The [`AnnotationStack`] that we use to keep track of active annotations.
/// - `table`: The [`SymbolTable`] of the current scope.
/// 
/// # Returns
/// Whether any type information was updated or not. This can be used to detect whether we've "out-deduced".
fn trav_stmt(stmt: &mut Statement, stack: &mut AnnotationStack, table: &Rc<RefCell<SymbolTable>>) -> bool {
    trace!(target: "typing", "Traversing {:?}", stmt.kind.variant());
    let _trap = trace_trap!(target: "typing", "Exiting {:?}", stmt.kind.variant());

    // Push the statement's annotations
    let mut stack = stack.frame(&stmt.annots);

    // Match on the specific kind of statement
    use StatementKind::*;
    match &mut stmt.kind {
        // Definitions
        Import { .. } => { /* Nothing to do */ false },

        FunctionDef(def) => {
            trav_func_def(def, &mut *stack)
        },

        ClassDef { name, defs, st_entry } => {
            false
        },

        VarDef { name, data_type, value, st_entry } => {
            false
        },



        // Control flow
        For { name, start, stop, step, block, st_entry } => {
            false
        },

        While { cond, block } => {
            false
        },

        Return { value } => {
            false
        },



        // Miscellaneous
        Expression(expr) => {
            false
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
/// 
fn trav_func_def(def: &mut FunctionDef, stack: &mut AnnotationStack) -> bool {
    trace!(target: "typing", "Traversing FunctionDef");
    let _trap = trace_trap!(target: "typing", "Exiting FunctionDef");

    // We don't have a lot to do here, since we leave return value detection as a second, smaller pass, and argument deduction is for the body's contents

    // Recurse into the body
    let mut change: bool = false;
    for s in &mut def.body.stmts {
        change |= trav_stmt(s, stack, &def.body.table);
    }

    // Return whether anything was updated
    change
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

    let Program{ stmts, annots, table, .. } = tree;

    // Prepare the annotation stack
    let mut stack: AnnotationStack = AnnotationStack::new();
    stack.push(annots.iter());

    // We start to traverse the tree to find as much as we can about type information on one hand, while making sure that what we find is consistent on the other.
    let mut updated: bool = true;
    while updated {
        // Go through all the statements again, hoping we won't do anything anymore
        updated = false;
        for s in stmts.iter_mut() {
            updated |= trav_stmt(s, &mut stack, table);
        }
    }

    // Done!
    debug!(target: "typing", "Traversal complete");
    Ok(())
}
