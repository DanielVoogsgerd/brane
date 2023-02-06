//  EXPRESSIONS.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:34:18
//  Last edited:
//    06 Feb 2023, 16:25:34
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines expressions within the BraneScript/Bakery AST.
// 

use enum_debug::EnumDebug;

use super::spec::{MergeStrategy, Node, TextRange};
use super::auxillary::{DataType, Identifier};
use super::statements::Statement;


/***** LIBRARY *****/
/// An Expression is a composable _and_ computable value that lives within statements.
#[derive(Clone, Debug)]
pub struct Expression {
    /// Any specific implementations of an expression.
    pub kind  : ExpressionKind,
    /// The range in the source text for this expression.
    pub range : TextRange,
}
impl Node for Expression {
    #[inline]
    fn range(&self) -> TextRange { self.range }
}

/// Defines the ExpressionKind, which implements the specifics for each of the various expressions.
#[derive(Clone, Debug, EnumDebug)]
pub enum ExpressionKind {
    // Statement-carrying expressions
    /// A Block carries a nested series of statements that evaluates to the same result.
    Block(Box<Block>),

    /// An If-statement branches between two values. It may evaluate to either of it, so both branches should always return the same type.
    If {
        /// The condition, as an expression.
        cond       : Box<Expression>,
        /// The true-part of the statement.
        block      : Box<Block>,
        /// The false-part of the statement, which may be optional.
        block_else : Option<Box<Block>>,
    },
    /// A Parallel-statement executes multiple values in parallel, returning according to some specific _merge strategy_.
    Parallel {
        /// The branches to execute in parallel.
        branches : Vec<Block>,
        /// The strategy to use to merge the branches once they are done.
        strategy : MergeStrategy,
    },



    // Operators
    /// Casts from one data type to another.
    Cast {
        /// The expression to cast.
        expr      : Box<Expression>,
        /// The new data type to cast to.
        data_type : DataType,
    },

    /// Indexes some object, typically an Array.
    Index {
        /// The expression of the thing to index.
        to_index : Box<Expression>,
        /// The expression that computes the index.
        index    : Box<Expression>,
    },
    /// Calls some object, typically a(n external) Function.
    Function {
        /// The expression of the thing to call.
        to_call : Box<Expression>,
        /// The list of arguments to call the thing with.
        args    : Vec<Expression>,
    },



    // Arithmetic operators
    /// Performs some arithmetic operation on a single value.
    Unary {
        /// The expression to perform the operation on.
        expr : Box<Expression>,
        /// The operation to perform.
        op   : UnaryOperator,
    },

    /// Performs some arithmetic operation on two values.
    Binary {
        /// The lefthand-side expression to perform the operation on.
        lhs : Box<Expression>,
        /// The righthand-side expression to perform the operation on.
        rhs : Box<Expression>,
        /// The operation to perform.
        op  : BinaryOperator,
    },

    /* TODO: Patterns */



    // Values
    /// Instantiates a class, returning the new instance.
    Instance {
        /// The identifier of the class instantiated.
        name  : Identifier,
        /// The values for each of the properties of the class.
        props : Vec<PropertyExpr>,
    },

    /// Refers to some statically declared _local_ function.
    LocalFunctionRef {
        /// The identifier of the function to which we refer.
        name : Identifier,
    },
    /// Refers to some statically declared _external_ function.
    ExternalFunctionRef {
        /// The identifier of the function to which we refer.
        name    : Identifier,
        /// The name of the package where we can find the function.
        package : Identifier,
    },
    /// Refers to some statically declared variable (which may be a dynamic value).
    VarRef {
        /// The name of the variable to which we refer.
        name : Identifier,
    },

    /// A literal value to push upon the stack.
    Literal(Literal),
}



/// A Block is a scoped series of statements.
#[derive(Clone, Debug)]
pub struct Block {
    /// The series of statements to execute.
    pub stmts : Vec<Statement>,
    /// The range in the source text for this block.
    pub range : TextRange,
}
impl Node for Block {
    #[inline]
    fn range(&self) -> TextRange { self.range }
}



/// Defines the unary operations supported by BraneScript/Bakery.
#[derive(Clone, Copy, Debug, EnumDebug)]
pub enum UnaryOperator {
    /// A logical negation.
    LogNeg,
    /// An arithmetic negation.
    ArhNeg,
}

/// Defines the binary operations supported by BraneScript/Bakery.
#[derive(Clone, Copy, Debug, EnumDebug)]
pub enum BinaryOperator {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Modulo
    Mod,

    /// Disjuntion,
    Or,
    /// Conjunction,
    And,

    /// Equals
    Eq,
    /// Not-equals
    Neq,
    /// Smaller than
    Lt,
    /// Smaller than or equal to
    Le,
    /// Greater than
    Gt,
    /// Smaller than or requal to
    Ge,
}



/// Defines a PropertyExpression, which is assigned the value of a property in an instance expression.
#[derive(Clone, Debug)]
pub struct PropertyExpr {
    /// The name of the property.
    pub name  : Identifier,
    /// The value of the property.
    pub value : Box<Expression>,

    /// The range in the source text for this property expression.
    pub range : TextRange,
}
impl Node for PropertyExpr {
    #[inline]
    fn range(&self) -> TextRange { self.range }
}



/// A Literal defines a specific kind of Expression that always evaluates to a constant value.
#[derive(Clone, Debug)]
pub struct Literal {
    /// Any specific implementations of a literal.
    pub kind  : LiteralKind,
    /// The range in the source text for this literal.
    pub range : TextRange,
}
impl Node for Literal {
    #[inline]
    fn range(&self) -> TextRange { self.range }
}

/// Defines a LiteralKind, which implements the specifics for each of the various literals.
#[derive(Clone, Debug, EnumDebug)]
pub enum LiteralKind {
    /// Defines a boolean literal.
    Boolean {
        value : bool,
    },

    /// Defines an integer literal.
    Integer {
        value : i64,
    },

    /// Defines a real literal.
    Real {
        value : f64,
    },

    /// Defines a string literal.
    String {
        value : String,
    },



    /// Defines a void literal.
    Void {},
}
