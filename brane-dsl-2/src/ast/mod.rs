//  MOD.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:32:54
//  Last edited:
//    17 Feb 2023, 16:36:02
//  Auto updated?
//    Yes
// 
//  Description:
//!   The `ast` submodule defines the abstract syntax tree for the
//!   BraneScript and Bakery languages.
//!   
//!   This AST maps 1-to-1 to the formal grammar specified on the
//!   [wiki](https://wiki.enablingpersonalizedinterventions.nl/specification/branescript/grammar.html).
// 

// Declare nested modules
pub mod spec;
pub mod types;
pub mod symbol_tables;
pub mod auxillary;
pub mod expressions;
pub mod statements;
pub mod toplevel;

// Bring some of it into the module namespace
pub use toplevel::Program;
