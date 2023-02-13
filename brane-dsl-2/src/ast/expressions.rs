//  EXPRESSIONS.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:34:18
//  Last edited:
//    13 Feb 2023, 13:03:08
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines expressions within the BraneScript/Bakery AST.
// 

use std::fmt::{Display, Formatter, Result as FResult};

use enum_debug::EnumDebug;

use super::spec::{Annotation, BindingPower, Node, TextRange};
use super::auxillary::{DataType, Identifier, MergeStrategy};
use super::statements::Statement;


/***** LIBRARY *****/
/// An Expression is a composable _and_ computable value that lives within statements.
#[derive(Clone, Debug)]
pub struct Expression {
    /// Any specific implementations of an expression.
    pub kind  : ExpressionKind,
    /// The range in the source text for this expression.
    pub range : Option<TextRange>,
}
impl Node for Expression {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Defines the ExpressionKind, which implements the specifics for each of the various expressions.
#[derive(Clone, Debug, EnumDebug)]
pub enum ExpressionKind {
    // Statement-carrying expressions
    /// A Block carries a nested series of statements that evaluates to the same result, as well as any scope-specific annotations.
    Block(Box<Block>, Vec<Annotation>),

    /// An If-statement branches between two values. It may evaluate to either of it, so both branches should always return the same type.
    If {
        /// The condition, as an expression.
        cond : Box<Expression>,

        /// The true-part of the statement.
        block      : Box<Block>,
        /// The false-part of the statement, which may be optional.
        block_else : Option<Box<Block>>,

        /// The annotations for the true-block.
        annots      : Vec<Annotation>,
        /// The annotations for the else-block.
        annots_else : Vec<Annotation>,
    },
    /// A Parallel-statement executes multiple values in parallel, returning according to some specific _merge strategy_.
    Parallel {
        /// The branches to execute in parallel.
        branches : Vec<(Block, Vec<Annotation>)>,
        /// The strategy to use to merge the branches once they are done.
        strategy : Option<MergeStrategy>,
    },



    // Operators
    /// Casts from one data type to another.
    Cast {
        /// The expression to cast.
        expr      : Box<Expression>,
        /// The new data type to cast to.
        data_type : DataType,
    },
    /// Discards the value of the expression it wraps, i.e., "casts" to void. Essentially `;`.
    Discard {
        /// The expression to discard.
        expr : Box<Expression>,
    },

    /// Indexes some object, typically an Array.
    Index {
        /// The expression of the thing to index.
        to_index : Box<Expression>,
        /// The expression that computes the index.
        index    : Box<Expression>,
    },
    /// Projects some object, typically a Class.
    Proj {
        /// The expression of the thing to project on.
        to_proj : Box<Expression>,
        /// The identifier that is the field to project.
        field   : Identifier,
    },
    /// Calls some object, typically a(n external) Function.
    Call {
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
    /// Instantiates an array, returning a new one.
    Array {
        /// The elements in this array.
        elems : Vec<Expression>,
    },
    /// Instantiates a class, returning the new instance.
    Instance {
        /// The identifier of the class instantiated.
        name  : Identifier,
        /// The values for each of the properties of the class.
        props : Vec<PropertyExpr>,
    },

    /// Refers to some statically declared variable (which may be a dynamic value).
    VarRef {
        /// The name of the variable to which we refer.
        name : Identifier,
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

    /// A literal value to push upon the stack.
    Literal(Literal),
}



/// A Block is a scoped series of statements.
#[derive(Clone, Debug)]
pub struct Block {
    /// The series of statements to execute.
    pub stmts : Vec<Statement>,
    /// The range in the source text for this block.
    pub range : Option<TextRange>,
}
impl Node for Block {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}



// /// Defines a generalisation over any kind of operator.
// #[derive(Clone, Copy, Debug, EnumDebug)]
// pub enum Operator {
//     /// It's a unary operator
//     Unary(UnaryOperator),
//     /// It's a binary operator
//     Binary(BinaryOperator),
// }
// impl Operator {
//     /// Returns the binding power for this operator.
//     /// 
//     /// # Returns
//     /// A `BindingPower` struct that describes the asynchronous binding power for side of the expression.
//     #[inline]
//     pub fn binding_power(&self) -> BindingPower {
//         use Operator::*;
//         match self {
//             Unary(op)  => op.binding_power(),
//             Binary(op) => op.binding_power(),
//         }
//     }
// }
// impl Node for Operator {
//     #[inline]
//     fn range(&self) -> Option<TextRange> {
//         use Operator::*;
//         match self {
//             Unary(op)  => op.range(),
//             Binary(op) => op.range(),
//         }
//     }
// }

/// Defines the unary operations supported by BraneScript/Bakery.
#[derive(Clone, Copy, Debug)]
pub struct UnaryOperator {
    /// The specific variant itself.
    pub kind  : UnaryOperatorKind,
    /// The range where we found the operator.
    pub range : Option<TextRange>,
}
impl UnaryOperator {
    /// Returns the binding power for this operator.
    /// 
    /// One of the two sides of the returned BindingPower will be `None`, since, clearly, a unary operator only binds on one side.
    /// 
    /// # Returns
    /// A `BindingPower` struct that describes the asynchronous binding power for side of the expression.
    #[inline]
    pub fn binding_power(&self) -> BindingPower { self.kind.binding_power() }
}
impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use UnaryOperatorKind::*;
        match &self.kind {
            Not => write!(f, "!"),
            Neg => write!(f, "-"),
        }
    }
}
impl Node for UnaryOperator {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Defines the specifics of the different unary operator variants.
#[derive(Clone, Copy, Debug, EnumDebug)]
pub enum UnaryOperatorKind {
    /// A logical negation.
    Not,
    /// An arithmetic negation.
    Neg,
}
impl UnaryOperatorKind {
    /// Returns the binding power for this operator.
    /// 
    /// One of the two sides of the returned BindingPower will be `None`, since, clearly, a unary operator only binds on one side.
    /// 
    /// # Returns
    /// A `BindingPower` struct that describes the asynchronous binding power for side of the expression.
    #[inline]
    pub fn binding_power(&self) -> BindingPower {
        // BP derived from the C operator precedence, <https://en.cppreference.com/w/c/language/operator_precedence>.
        // Also see `BinaryOperator::binding_power()` and `ExpressionPostfix::binding_power()`.
        use UnaryOperatorKind::*;
        match self {
            Not => BindingPower{ left: None, right: Some(16) },
            Neg => BindingPower{ left: None, right: Some(16) },
        }
    }
}

/// Defines the binary operations supported by BraneScript/Bakery.
#[derive(Clone, Copy, Debug)]
pub struct BinaryOperator {
    /// The actual variant
    pub kind  : BinaryOperatorKind,
    /// The range where we found it
    pub range : Option<TextRange>,
}
impl BinaryOperator {
    /// Returns the binding power for this operator.
    /// 
    /// # Returns
    /// A `BindingPower` struct that describes the asynchronous binding power for side of the expression.
    #[inline]
    pub fn binding_power(&self) -> BindingPower { self.kind.binding_power() }
}
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use BinaryOperatorKind::*;
        match &self.kind {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Mod => write!(f, "%"),

            Or  => write!(f, "||"),
            And => write!(f, "&&"),

            Eq => write!(f, "=="),
            Ne => write!(f, "!="),
            Lt => write!(f, "<"),
            Le => write!(f, "<="),
            Gt => write!(f, ">"),
            Ge => write!(f, ">="),

            Assign => write!(f, ":="),
        }
    }
}
impl Node for BinaryOperator {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Defines the specifics of the different binary operator variants.
#[derive(Clone, Copy, Debug, EnumDebug)]
pub enum BinaryOperatorKind {
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
    Ne,
    /// Smaller than
    Lt,
    /// Smaller than or equal to
    Le,
    /// Greater than
    Gt,
    /// Smaller than or equal to
    Ge,

    /// Assignment
    Assign,
}
impl BinaryOperatorKind {
    /// Returns the binding power for this operator.
    /// 
    /// # Returns
    /// A `BindingPower` struct that describes the asynchronous binding power for side of the expression.
    pub fn binding_power(&self) -> BindingPower {
        // BP derived from the C operator precedence, <https://en.cppreference.com/w/c/language/operator_precedence>.
        // Also see `UnaryOperator::binding_power()` and `ExpressionPostfix::binding_power()`.
        use BinaryOperatorKind::*;
        match self {
            Add => BindingPower{ left: Some(11), right: Some(12) },
            Sub => BindingPower{ left: Some(11), right: Some(12) },
            Mul => BindingPower{ left: Some(13), right: Some(14) },
            Div => BindingPower{ left: Some(13), right: Some(14) },
            Mod => BindingPower{ left: Some(13), right: Some(14) },

            Or  => BindingPower{ left: Some(3), right: Some(4) },
            And => BindingPower{ left: Some(5), right: Some(6) },

            Eq => BindingPower{ left: Some(7), right: Some(8) },
            Ne => BindingPower{ left: Some(7), right: Some(8) },
            Lt => BindingPower{ left: Some(9), right: Some(10) },
            Le => BindingPower{ left: Some(9), right: Some(10) },
            Gt => BindingPower{ left: Some(9), right: Some(10) },
            Ge => BindingPower{ left: Some(9), right: Some(10) },

            Assign => BindingPower{ left: Some(2), right: Some(1) },
        }
    }
}

/// An extension to a BinaryOperator that defines binding-power enabled operators or other syntax that can postfix expressions.
#[derive(Clone, Copy, Debug)]
pub struct ExpressionPostfix {
    /// The specific variant.
    pub kind  : ExpressionPostfixKind,
    /// The range where this postfix originates from.
    pub range : Option<TextRange>,
}
impl ExpressionPostfix {
    /// Returns the binding power for this postfix operator.
    /// 
    /// # Returns
    /// A `BindingPower` struct that describes the asynchronous binding power for side of the expression.
    pub fn binding_power(&self) -> BindingPower { self.kind.binding_power() }
}
impl Node for ExpressionPostfix {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}

/// Defines the possible variants of an expression postfix.
#[derive(Clone, Copy, Debug, EnumDebug)]
pub enum ExpressionPostfixKind {
    /// A binary operator can always postfix an operator, since it sits in between two.
    BinaryOperator(BinaryOperator),
    /// A cast operator postfixes an expression.
    Cast,
    /// An array index postfixes an expression.
    ArrayIndex,
    /// A function call postfixes an expression.
    Call,
    // My comments are the best
    /// And (wait for it) a projection operator _also_ postfixes an expression.
    Proj,
}
impl ExpressionPostfixKind {
    /// Returns the binding power for this postfix operator.
    /// 
    /// # Returns
    /// A `BindingPower` struct that describes the asynchronous binding power for side of the expression.
    pub fn binding_power(&self) -> BindingPower {
        // BP derived from the C operator precedence, <https://en.cppreference.com/w/c/language/operator_precedence>.
        // Also see `UnaryOperator::binding_power()` and `BinaryOperator::binding_power()`.
        use ExpressionPostfixKind::*;
        match self {
            BinaryOperator(op) => op.binding_power(),
            Cast               => BindingPower{ left: Some(15), right: None },
            ArrayIndex         => BindingPower{ left: Some(17), right: None },
            Call               => BindingPower{ left: Some(17), right: None },
            Proj               => BindingPower{ left: Some(17), right: None },
        }
    }
}



/// Defines a PropertyExpression, which is assigned the value of a property in an instance expression.
#[derive(Clone, Debug)]
pub struct PropertyExpr {
    /// The name of the property.
    pub name  : Identifier,
    /// The value of the property.
    pub value : Box<Expression>,

    /// The range in the source text for this property expression.
    pub range : Option<TextRange>,
}
impl Node for PropertyExpr {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
}



/// A Literal defines a specific kind of Expression that always evaluates to a constant value.
#[derive(Clone, Debug)]
pub struct Literal {
    /// Any specific implementations of a literal.
    pub kind  : LiteralKind,
    /// The range in the source text for this literal.
    pub range : Option<TextRange>,
}
impl Node for Literal {
    #[inline]
    fn range(&self) -> Option<TextRange> { self.range }
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

    /// Defines a null literal.
    Null,
}
