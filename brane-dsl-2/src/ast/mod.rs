//  MOD.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:32:54
//  Last edited:
//    08 Feb 2023, 13:30:35
//  Auto updated?
//    Yes
// 
//  Description:
//!   The `ast` submodule defines the abstract syntax tree for the
//!   BraneScript and Bakery languages.
// 

// Declare nested modules
pub mod spec;
pub mod types;
pub mod auxillary;
pub mod expressions;
pub mod statements;
pub mod toplevel;

// Bring some of it into the module namespace
pub use toplevel::Program;
