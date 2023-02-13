//  MOD.rs
//    by Lut99
// 
//  Created:
//    10 Feb 2023, 19:24:13
//  Last edited:
//    13 Feb 2023, 11:53:50
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the traversals that happen in the compiler.
// 

// Declare the subsubmodules
pub mod print_ast;
pub mod annotations;
pub mod resolve;


// Module-wide enums
/// Defines the traversals for the BraneScript/Bakery compiler and, more importantly, their order.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum CompilerPhase {
    /// The print one is optional, and not run by default
    Print,

    /// Resolves annotations in the AST, effectively removing them as statements and instead adding them to other statements.
    /// 
    /// May also prune parts of the AST based on conditional compilation.
    Annotations,
    /// The resolve one is the first
    Resolve,
}
