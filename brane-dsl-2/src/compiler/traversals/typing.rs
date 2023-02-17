//  TYPING.rs
//    by Lut99
// 
//  Created:
//    14 Feb 2023, 13:33:32
//  Last edited:
//    17 Feb 2023, 15:20:06
//  Auto updated?
//    Yes
// 
//  Description:
//!   Implements the traversal that will fill in all the type information
//!   we have.
// 

use crate::errors::DslError;
use crate::warnings::DslWarning;
use crate::ast::toplevel::Program;


/***** LIBRARY *****/
// Runs a full traversal on the given AST to resolve identifiers to symbol table entries _as much as possible_.
/// 
/// Note that, due to delayed linking, we have no choice but to accept unlinked variables. However, to fix this, the traversal is built such that it can be run repeatedly on the same workflow if more information becomes available. Put differently, the process in this traversal is "best-effort".
/// 
/// # Arguments
/// - `tree`: The AST to resolve.
/// - `warnings`: A list of DslWarnings to populate whenever an error occurs in this traversal.
pub fn traverse(tree: &mut Program, warnings: &mut Vec<DslWarning>) -> Result<(), Vec<DslError<'static>>> {
    Ok(())
}
