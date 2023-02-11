//  TOPLEVEL.rs
//    by Lut99
// 
//  Created:
//    08 Feb 2023, 13:18:17
//  Last edited:
//    11 Feb 2023, 18:12:31
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the toplevel struct(s) in the BraneScript/Bakery AST.
// 

use super::spec::{Node, TextRange};
use super::symbol_tables::SymbolTable;
use super::statements::Statement;


/***** LIBRARY *****/
/// Defines the toplevel program.
#[derive(Clone, Debug)]
pub struct Program {
    /// The vector of statements in this program.
    pub stmts : Vec<Statement>,
    /// The toplevel symbol table for the program.
    pub table : SymbolTable,
    /// The toplevel range of the entire program.
    pub range : Option<TextRange>,
}
impl Node for Program {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}
