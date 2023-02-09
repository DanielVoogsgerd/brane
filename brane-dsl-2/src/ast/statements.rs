//  TOPLEVEL.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:33:27
//  Last edited:
//    09 Feb 2023, 15:09:58
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines toplevel things in the AST such as a program or a statement.
// 

use enum_debug::EnumDebug;

use super::spec::{Node, TextRange};
use super::auxillary::{Annotation, DataType, Identifier};
use super::expressions::{Block, Expression, Literal};


/***** LIBRARY *****/
/// A Statement is the smallest, still-valid snippet of a BraneScript/Bakery program.
#[derive(Clone, Debug)]
pub struct Statement {
    /// Any specific implementations of a statement.
    pub kind   : StatementKind,
    /// Any annotations that are applied to this statement.
    pub annots : Vec<Annotation>,
    /// The range in the source text for this statement.
    pub range  : Option<TextRange>,
}
impl Node for Statement {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Defines the StatementKind, which implements the specifics for each of the various statements.
#[derive(Clone, Debug, EnumDebug)]
pub enum StatementKind {
    // Definitions
    /// An import-statement defines an external package.
    Import {
        /// The name of the package which we import.
        name    : Identifier,
        /// An optional version, specified as a (major, minor, patch) triplet of integer literals.
        version : Option<(Literal, Literal, Literal)>,
    },

    /// A definition of a function.
    FunctionDef(FunctionDef),

    /// A definition of a class.
    ClassDef {
        /// The name of the class.
        name : Identifier,
        /// The definitions in this class, which may be either properties or methods.
        defs : Vec<ClassMemberDef>,
    },

    /// A definition of a variable (previously known as a let-expression).
    VarDef {
        /// The name of the variable.
        name      : Identifier,
        /// The data type of the variable.
        data_type : DataType,

        /// An optional value to assign to the variable.
        value : Option<Expression>,
    },



    // Control flow
    /// A for-loop defines a series of statements that should be executed in succession, but with a predictable amount.
    For {
        /// The expression that computes the start value.
        start : Expression,
        /// The expression that computes the stop value.
        stop  : Expression,
        /// The expression that computes the step value.
        step  : Option<Literal>,

        /// The block of statements to be executed repeatedly.
        block : Block,
    },

    /// A while-loop defines a series of statements that should be executed in succession, but with an unpredictable amount.
    While {
        /// The condition to compute at the _start_ of every loop.
        cond  : Expression,
        /// The block of statements to be executed repeatedly.
        block : Block,
    },

    /// A return-statement escapes from the parent function or workflow.
    Return {
        /// The (optional) value to return.
        value : Option<Expression>,
    },



    // Miscellaneous
    /// An assigned assigns a value to some variable.
    Assign {
        /// The name of the variable to assign a value to.
        name  : Identifier,
        /// The value to assign to it.
        value : Expression,
    },

    /// An Expression defines some thing executable right here, right now.
    Expression(Expression),
}



/// Defines how a function definition looks like.
#[derive(Clone, Debug)]
pub struct FunctionDef {
    /// The name of the function.
    pub name : Identifier,
    /// The list of arguments for this function.
    pub args : Vec<ArgDef>,
    /// The return type for this function.
    pub ret  : DataType,

    /// The body for this function.
    pub body : Block,

    /// The range in the source text for this definition (as a whole).
    pub range : Option<TextRange>,
}
impl Node for FunctionDef {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Defines the definition of a function argument.
/// 
/// May either be a normal argument or the special `Self` one.
#[derive(Clone, Debug)]
pub struct ArgDef {
    /// The name of the argument
    pub name      : Identifier,
    /// The data type of this argument.
    pub data_type : DataType,

    /// The range in the source text for this argument definition.
    pub range : Option<TextRange>,
}
impl Node for ArgDef {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}



/// Defines either one of two things: either a property definition or a function (method) definition.
#[derive(Clone, Debug, EnumDebug)]
pub enum ClassMemberDef {
    /// It's a property
    Property(PropertyDef),
    /// It's a method.
    Method(FunctionDef),
}
impl Node for ClassMemberDef {
    #[inline]
    fn range(&self) -> Option<TextRange> {
        use ClassMemberDef::*;
        match self {
            Property(prop) => prop.range(),
            Method(method) => method.range(),
        }
    }
}



/// Defines a property definition.
#[derive(Clone, Debug)]
pub struct PropertyDef {
    /// The name of the property.
    pub name      : Identifier,
    /// The data type of the property.
    pub data_type : DataType,

    /// The TextRange of the property.
    pub range : Option<TextRange>,
}
impl Node for PropertyDef {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}
