//  EXPRESSIONS.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 12:54:14
//  Last edited:
//    07 Feb 2023, 19:46:23
//  Auto updated?
//    Yes
// 
//  Description:
//!   Implements a Pratt parser
//!   (<https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>)
//!   for parsing expressions.
// 

use enum_debug::EnumDebug;
use nom::IResult;
use nom::{branch, combinator as comb, multi, sequence as seq};

use crate::ast::spec::{BindingPower, TextPos, TextRange};
use crate::ast::expressions::{BinaryOperator, BinaryOperatorKind, Expression, ExpressionKind, UnaryOperator};
use crate::scanner::Token;
use crate::parser::{Error, Input};
use crate::parser::utils::tag_token;
use super::{auxillary, instances, literals, operators};


/***** HELPER ENUMS *****/
/// Generalizes over things that are parsed as the first expression.
#[derive(Clone, Debug, EnumDebug)]
enum FirstExpression {
    /// It's a unary operator (they are all prefixed for now).
    UnaryOperator(UnaryOperator),
    /// It's an atomic expression, i.e., one we can parse without worry for binding power and whatnot.
    Atomic(Expression),
}





/***** SCANNING FUNCTIONS *****/
/// The actual pratt parser, since it takes a different input.
/// 
/// # Arguments
/// - `input`: The Input list of tokens to parse from.
/// - `min_bp`: The minimum binding power that any expression should have lest its LHS will be stolen away from it.
/// 
/// # Returns
/// A tuple with the remaining input and a parsed expression, as an `Expr`.
/// 
/// # Errors
///  This function errors if we failed to parse an expression for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn pratt_parser<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>, min_bp: BindingPower) -> IResult<Input<'t, 's>, Expression, E> {
    // First step: parse the main expression, either directory or through an operator if there is one
    let (mut rem, mut lhs): (Input, Expression) = match branch::alt((
        comb::map(operators::unary, FirstExpression::UnaryOperator),
        comb::map(atomic, FirstExpression::Atomic),
    ))(input)? {
        (rem, FirstExpression::UnaryOperator(operator)) => {
            // It's an operator; now parse again using that operator's binding power.
            let bp: BindingPower = operator.binding_power();
            if bp.right.is_none() { panic!("Encountered a prefix unary operator that does not bind to the right"); }
            let (r, expr): (Input, Expression) = pratt_parser(rem, bp)?;

            // Find the range if we can
            let range: Option<TextRange> = match (operator.range, expr.range) {
                (Some(TextRange{ start, .. }), Some(TextRange{ end, .. })) => Some(TextRange::new(start, end)),
                _                                                          => None,
            };

            // Return them wrapped in the operator expression
            (
                r,
                Expression {
                    kind : ExpressionKind::Unary {
                        expr : Box::new(expr),
                        op   : operator,
                    },
                    range,
                }
            )
        },

        (rem, FirstExpression::Atomic(expr)) => (rem, expr),
    };

    // Loop to find any operators
    loop {
        // Attempt to read one
        match operators::binary(rem) {
            Ok((rem, operator)) => {
                // See if we should parse, or if we should yield to a stronger binding operator to the left
                let bp: BindingPower = operator.binding_power();
                match (bp.left, min_bp.right) {
                    // The other operator binds stronger
                    (Some(left), Some(right)) => { if left < right { return Ok((rem, lhs)); } },
                    // We don't bind at all, so cannot be for us
                    (None, Some(_)) => { return Ok((rem, lhs)); },
                    // The guy is requesting more expressions but does not bind?
                    (_, None) => { panic!("Processing expression after expression {} that does not bind to the right", lhs.kind.variant()); },
                }

                // If we do parse, however, then decide what to do next based on the specific operator
                use BinaryOperatorKind::*;
                match operator.kind {
                    Add | Sub | Mul | Div | Mod |
                    And | Or | Eq | Ne | Lt | Le | Gt | Ge => {

                    },

                    
                }
            },

            Err(nom::Err::Error(_)) => {
                // No operator found, we can return what we have
                return Ok((rem, lhs));
            },
            Err(err) => {
                // Fatal error, or at least, not a valid state
                return Err(err);
            }
        }
    }
}

/// Parses an "atomic" expression, which is an expression that does not bind like an operator.
/// 
/// # Arguments
/// - `input`: The input list of tokens to parse from.
/// 
/// # Returns
/// A tuple with the remaining input and a parsed expression, as an `Expr`.
/// 
/// # Errors
///  This function errors if we failed to parse an expression for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn atomic<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, E> {
    branch::alt((
        // Literals
        comb::map(literals::parse, |l| {
            let range: Option<TextRange> = l.range;
            Expression{ kind: ExpressionKind::Literal(l), range }
        }),
        // Variable references (should be converted to a function reference based on the expression context)
        comb::map(auxillary::parse_ident, |i| {
            let range: Option<TextRange> = i.range;
            Expression{ kind: ExpressionKind::VarRef{ name: i }, range }
        }),

        // Parenthesis
        seq::delimited(tag_token!(Token::LeftParen), |input| pratt_parser(input, BindingPower::none()), tag_token!(Token::RightParen)),
        // Array expression
        comb::map(
            seq::tuple((tag_token!(Token::LeftBrace), multi::separated_list1(tag_token!(Token::Comma), |input| pratt_parser(input, BindingPower::none())), tag_token!(Token::RightBrace))),
            |(l, elems, r): (&Token<'s>, Vec<Expression>, &Token<'s>)| -> Expression {
                // The range of the array expression is that of the braces
                let range: Option<TextRange> = Some(TextRange::new(TextPos::start_of(l.span()), TextPos::end_of(l.span())));
                Expression {
                    kind : ExpressionKind::Array{ elems },
                    range,
                }
            },
        ),
        // Instance expression
        instances::parse,
    ))(input)
}





/***** LIBRARY *****/
/// Attempts to parse an expression from the head of the input stream.
/// 
/// # Arguments
/// - `input`: The Input list of tokens to parse from.
/// 
/// # Returns
/// A tuple with the remaining input and a parsed expression, as an `Expr`.
/// 
/// # Errors
///  This function errors if we failed to parse an expression for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
#[inline]
pub fn parse<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, E> {
    pratt_parser(input, BindingPower::none())
}
