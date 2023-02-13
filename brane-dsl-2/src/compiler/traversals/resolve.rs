//  RESOLVE.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 17:46:03
//  Last edited:
//    13 Feb 2023, 11:01:50
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the traversal that will resolve the identifiers to entries
//!   in a symbol table.
// 

use crate::ast::Program;


/***** LIBRARY *****/
/// Runs a full traversal on the given AST to populate it with symbol tables as much as possible.
/// 
/// # Arguments
/// - `tree`: The AST to resolve.
#[allow(dead_code)]
pub fn traverse(tree: &mut Program) -> Result<(), std::io::Error> {
    // We start populating the program's symbol table
    let Program{ stmts, table, .. } = tree;

    // Done
    Ok(())
}
