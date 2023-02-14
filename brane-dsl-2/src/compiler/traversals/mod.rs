//  MOD.rs
//    by Lut99
// 
//  Created:
//    10 Feb 2023, 19:24:13
//  Last edited:
//    14 Feb 2023, 13:37:43
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
pub mod typing;


// Module-wide enums
/// Defines the traversals for the BraneScript/Bakery compiler and, more importantly, their order.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum CompilerPhase {
    /// Resolves annotations in the AST, effectively removing them as statements and instead adding them to other statements.
    /// 
    /// May also prune parts of the AST based on conditional compilation.
    Annotations,
    /// Resolves identifiers by populating the symbol tables as much as possible. Note, however, that its effort is imperfect, since any external function (package, other module, previous snippet) is not yet presented here.
    /// 
    /// Designed to be run multiple times as more declarations become available.
    Resolve,
    /// Resolves type information by annotating the symbol tables with proper types. Note, however, that its effort is imperfect, since the previous traversal is also imperfect. Moreover, the user is only required to provide very minimal type annotations, so we might not be able to deduce anything.
    /// 
    /// Designed to be run multiple times as more types and information becomes available.
    Typing,
}
