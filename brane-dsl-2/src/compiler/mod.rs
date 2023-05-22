//  MOD.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:29:23
//  Last edited:
//    22 May 2023, 18:35:04
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the compiler for the DSLs, that takes the unified AST and
//!   outputs, after multiple traversals, the shared workflow
//!   representation.
// 

// Declare submodules
mod utils;
mod annot_stack;
pub mod traversals;


// Pull some stuff into this module's namespace
pub use traversals::CompilerPhase;
