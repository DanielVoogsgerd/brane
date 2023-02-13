//  SYMBOL TABLES.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 17:54:32
//  Last edited:
//    13 Feb 2023, 11:03:17
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines SymbolTables, which are not nodes in the AST but rather a
//!   separate data structure for keeping track of definitions.
// 


/***** LIBRARY *****/
/// Defines a SymbolTable, which maps information we are interested in to an identifier, per-scope.
#[derive(Clone, Debug)]
pub struct SymbolTable {
    
}

impl SymbolTable {
    /// Constructor for the SymbolTable that does not populate any entries yet.
    /// 
    /// # Returns
    /// A new SymbolTable instance without any entries.
    #[inline]
    pub fn empty() -> Self {
        Self {
            
        }
    }
}
