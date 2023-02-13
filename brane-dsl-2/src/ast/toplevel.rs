//  TOPLEVEL.rs
//    by Lut99
// 
//  Created:
//    08 Feb 2023, 13:18:17
//  Last edited:
//    13 Feb 2023, 11:39:55
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the toplevel struct(s) in the BraneScript/Bakery AST.
// 

use super::spec::{Annotation, Node, TextRange};
use super::symbol_tables::SymbolTable;
use super::statements::Statement;


/***** LIBRARY *****/
/// Defines the toplevel program.
#[derive(Clone, Debug)]
pub struct Program {
    /// The vector of statements in this program.
    pub stmts : Vec<Statement>,
    /// The toplevel range of the entire program.
    pub range : Option<TextRange>,

    /// The toplevel list of annotations for the program.
    pub annots : Vec<Annotation>,
    /// The toplevel symbol table for the program.
    pub table  : SymbolTable,
}
impl Node for Program {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}
