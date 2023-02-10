//  EXPRESSIONS.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 12:54:14
//  Last edited:
//    10 Feb 2023, 12:51:39
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

use crate::ast::spec::{BindingPower, TextRange};
use crate::ast::auxillary::{DataType, Identifier, MergeStrategy};
use crate::ast::expressions::{Block, ExpressionPostfixKind, Expression, ExpressionKind};
use crate::scanner::Token;
use crate::parser::{Error, Input};
use crate::parser::utils::tag_token;
use super::{auxillary, blocks, instances, literals, operators};


/***** SCANNING FUNCTIONS *****/
/// The actual pratt parser, since it takes a different input.
/// 
/// # Arguments
/// - `min_bp`: The minimum binding power that any expression should have lest its LHS will be stolen away from it.
/// 
/// # Returns
/// A function that implements the pratt parser using the given minimum binding power.
fn pratt_parser<'t, 's: 't>(min_bp: BindingPower) -> impl FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    nom::error::context("an expression", move |input: Input<'t, 's>| -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
        println!("Parsing {}", input.first().map(|t| t.variant().to_string()).unwrap_or("EOF".into()));

        // First step: parse the main expression, either directly or through an operator if there is one
        let (mut rem, mut lhs): (Input, Expression) = match operators::unary(input) {
            Ok((rem, operator)) => {
                // It's an operator; now parse again using that operator's binding power.
                let bp: BindingPower = operator.binding_power();
                if bp.right.is_none() { panic!("Encountered a prefix unary operator that does not bind to the right"); }
                let (r, expr): (Input, Expression) = nom::error::context("a unary operator", comb::cut(pratt_parser(bp)))(rem)?;

                // Find the range if we can
                let range: Option<TextRange> = match (operator.range, expr.range) {
                    (Some(TextRange{ start, .. }), Some(TextRange{ end, .. })) => Some(TextRange::new(start, end)),
                    _                                                          => None,
                };

                // Return it wrapped in the operator expression
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

            // We did not find an operator, so instead retry as a simple expression
            Err(nom::Err::Error(_)) => atomic(input)?,

            // Otherwise, fatal error
            Err(err) => { return Err(err); },
        };

        // Loop to find any operators
        let mut i: usize = 0;
        loop {
            println!("Test {}", i);
            i += 1;

            // Attempt to read one
            match operators::postfix(rem) {
                Ok((r, operator)) => {
                    // See if we should parse, or if we should yield to a stronger binding operator to the left
                    let bp: BindingPower = operator.binding_power();
                    match (min_bp.right, bp.left) {
                        // The other operator might bind stronger
                        (Some(right), Some(left)) => { if left < right { return Ok((r, lhs)); } },
                        // It's very weird if we don't bind to the left since what are we doing here then
                        (_, None) => { panic!("Expression postfix {} does not bind to the left, but here we are", operator.kind.variant()); },
                        // The rest means that the guy does not bind but we do, so that's easy
                        _ => {},
                    }

                    // If we do parse, however, then decide what to do next based on the specific operator
                    use ExpressionPostfixKind::*;
                    match operator.kind {
                        BinaryOperator(op) => {
                            // We already yielded to operators with stronger binding power; so now recurse to found our RHS
                            println!("Parsing RHS for {}", op.kind.variant());
                            let (r, rhs): (Input, Expression) = nom::error::context("the righthand-side of a binary operator", comb::cut(pratt_parser(bp)))(r)?;
                            rem = r;
                            println!("Finished RHS for {}", op.kind.variant());

                            // Find the range for this expression
                            let range: Option<TextRange> = match (lhs.range, rhs.range) {
                                (Some(TextRange{ start, .. }), Some(TextRange{ end, .. })) => Some(TextRange::new(start, end)),
                                _                                                          => None,
                            };

                            // Don't quit, though, but retry for more postfix things
                            lhs = Expression {
                                kind : ExpressionKind::Binary {
                                    lhs : Box::new(lhs),
                                    rhs : Box::new(rhs),
                                    op,
                                },
                                range,
                            }
                        },

                        Cast => {
                            // Parse te 'as', then the type
                            let (r, data_type): (Input, DataType) = nom::error::context("the type in a typecast", comb::cut(auxillary::parse_type))(r)?;
                            rem = r;

                            // Find the range for this expression
                            let range: Option<TextRange> = match (lhs.range, data_type.range) {
                                (Some(TextRange{ start, .. }), Some(TextRange{ end, .. })) => Some(TextRange::new(start, end)),
                                _                                                          => None,
                            };

                            // Create the cast expression from that
                            lhs = Expression {
                                kind : ExpressionKind::Cast {
                                    expr : Box::new(lhs),
                                    data_type,
                                },
                                range,
                            }
                        },

                        ArrayIndex => {
                            // Parse the index expression
                            let (r, (index, rbrack)): (Input, (Expression, &Token)) = nom::error::context("the index in an array index", comb::cut(
                                seq::pair(
                                    pratt_parser(BindingPower::none()),
                                    tag_token!('t, 's, Token::RightBracket),
                                )),
                            )(r)?;
                            rem = r;

                            // Find the range for this expression
                            let range: Option<TextRange> = lhs.range.map(|r| TextRange::new(r.start, rbrack.end_of()));

                            // Return the array index expression
                            lhs = Expression {
                                kind : ExpressionKind::Index {
                                    to_index : Box::new(lhs),
                                    index    : Box::new(index),
                                },
                                range,
                            }
                        },

                        Call => {
                            // Parse the arguments in the call
                            let (r, (args, rparen)): (Input, (Vec<Expression>, &Token)) = nom::error::context("the arguments in a function call", comb::cut(
                                seq::pair(
                                    multi::separated_list0(tag_token!('t, 's, Token::Comma), pratt_parser(BindingPower::none())),
                                    tag_token!('t, 's, Token::RightParen),
                                )
                            ))(r)?;
                            rem = r;

                            // Find the range for this expression
                            let range: Option<TextRange> = lhs.range.map(|r| TextRange::new(r.start, rparen.end_of()));

                            // Return the call expression
                            lhs = Expression {
                                kind : ExpressionKind::Call {
                                    to_call : Box::new(lhs),
                                    args,
                                },
                                range,
                            }
                        },

                        Proj => {
                            // Parse the identifier that determines the field
                            let (r, ident): (Input, Identifier) = nom::error::context("the field of a projection", comb::cut(auxillary::parse_ident))(r)?;
                            rem = r;

                            // Find the range for this expression
                            let range: Option<TextRange> = match (lhs.range, ident.range) {
                                (Some(TextRange{ start, .. }), Some(TextRange{ end, .. })) => Some(TextRange::new(start, end)),
                                _                                                          => None,
                            };

                            // Return the expression
                            println!("Adding projection");
                            lhs = Expression {
                                kind : ExpressionKind::Proj {
                                    to_proj : Box::new(lhs),
                                    field   : ident,
                                },
                                range,
                            }
                        },
                    }
                },

                Err(nom::Err::Error(_)) => {
                    // No more postfix operators found, we can return what we have
                    return Ok((rem, lhs));
                },
                Err(err) => {
                    // Fatal error, or at least, not a valid state
                    return Err(err);
                }
            }
        }
    })
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
fn atomic<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    branch::alt((
        // Literals
        comb::map(literals::parse, |l| {
            let range: Option<TextRange> = l.range;
            Expression{ kind: ExpressionKind::Literal(l), range }
        }),
        // Variable/function references (a variable reference may still be converted to a local reference when done).
        reference,

        // Parenthesis
        paren,

        // Array expression
        array,
        // Instance expression
        instances::parse,

        // Block (nested scope)
        comb::map( blocks::parse, |block: Block| {
            let range: Option<TextRange> = block.range;
            Expression { kind : ExpressionKind::Block(Box::new(block)), range, }
        }),
        // If-statements
        if_stmt,
        // Parallel statements
        parallel_stmt,
    ))(input)
}

/// Attempts to parse a variable or _external_ function reference off the top of the given tokenstream.
/// 
/// The variable reference can then be converted to a local function reference if the context determines it so (no difference in syntax).
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed reference (as an expression).
/// 
/// # Errors
/// This function errors if we failed to parse a reference for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn reference<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    comb::map(
        nom::error::context("a reference", seq::pair(
            auxillary::parse_ident,
            comb::opt(seq::preceded(
                seq::pair(tag_token!('t, 's, Token::Colon), tag_token!('t, 's, Token::Colon)),
                comb::cut(auxillary::parse_ident),
            )),
        )),
        |(first, second): (Identifier, Option<Identifier>)| {
            // It's a variable reference if there is only one identifier, else it's an external function reference
            match (first, second) {
                (var_ref, None) => {
                    // Compute the range and return the variable reference expression
                    let range: Option<TextRange> = var_ref.range;
                    Expression {
                        kind : ExpressionKind::VarRef { name: var_ref },
                        range,
                    }
                },

                (package_ref, Some(func_ref)) => {
                    // Compute the range
                    let range: Option<TextRange> = match (package_ref.range, func_ref.range) {
                        (Some(TextRange{ start, .. }), Some(TextRange{ end, .. })) => Some(TextRange::new(start, end)),
                        _                                                          => None,
                    };

                    // Return the function reference
                    Expression {
                        kind : ExpressionKind::ExternalFunctionRef { name: func_ref, package: package_ref },
                        range,
                    }
                },
            }
        },
    )(input)
}

// Attempts to parse an expression wrapped in parenthesis off the top of the given tokenstream.
/// 
/// Essentially just parses it, and the returns the inner expression with a range of the outer parenthesis included.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed expression.
/// 
/// # Errors
/// This function errors if we failed to parse an paranethesis-wrapped expression for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn paren<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    comb::map(
        nom::error::context("a parenthesis expression", seq::pair(
            tag_token!('t, 's, Token::LeftParen),
            comb::cut(seq::pair(
                pratt_parser(BindingPower::none()),
                tag_token!('t, 's, Token::RightParen)
            )),
        )),
        |(lparen, (expr, rparen)): (&Token, (Expression, &Token))| {
            Expression {
                kind  : expr.kind,
                range : Some(TextRange::new(lparen.start_of(), rparen.end_of())),
            }
        },
    )(input)
}

/// Attempts to parse an array expression off the top of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed array.
/// 
/// # Errors
/// This function errors if we failed to parse an array for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn array<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    comb::map(
        nom::error::context("an array expression", seq::pair(
            tag_token!('t, 's, Token::LeftBracket),
            comb::cut(seq::pair(
                multi::separated_list1(tag_token!('t, 's, Token::Comma), pratt_parser(BindingPower::none())),
                tag_token!('t, 's, Token::RightBracket),
            )),
        )),
        |(l, (elems, r)): (&Token<'s>, (Vec<Expression>, &Token<'s>))| -> Expression {
            // The range of the array expression is that of the braces
            let range: Option<TextRange> = Some(TextRange::new(l.start_of(), r.end_of()));
            Expression {
                kind : ExpressionKind::Array{ elems },
                range,
            }
        },
    )(input)
}

/// Attempts to parse an if-statement off the top of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed if-statement.
/// 
/// # Errors
/// This function errors if we failed to parse an if-statement for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn if_stmt<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    comb::map(
        nom::error::context("an if statement", seq::pair(
            tag_token!('t, 's, Token::If),
            comb::cut(seq::tuple((
                seq::delimited(tag_token!('t, 's, Token::LeftParen), pratt_parser(BindingPower::none()), tag_token!('t, 's, Token::RightParen)),
                blocks::parse,
                comb::opt(seq::preceded(
                    tag_token!('t, 's, Token::Else),
                    blocks::parse,
                )),
            ))),
        )),
        |(if_kw, (cond, block, block_else)): (&Token, (Expression, Block, Option<Block>))| {
            // Find the range of the if-statement, where end at either the false block or the true block.
            let range: Option<TextRange> = match &block_else {
                Some(block) => block.range.map(|r| TextRange::new(if_kw.start_of(), r.end)),
                None        => block.range.map(|r| TextRange::new(if_kw.start_of(), r.end)),
            };

            // Return the new expression
            Expression {
                kind : ExpressionKind::If {
                    cond       : Box::new(cond),
                    block      : Box::new(block),
                    block_else : block_else.map(Box::new),
                },
                range,
            }
        }
    )(input)
}

/// Attempts to parse a parallel-statement off the top of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed parallel-statement.
/// 
/// # Errors
/// This function errors if we failed to parse an parallel-statement for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn parallel_stmt<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    comb::map(
        nom::error::context("parallel statement", seq::tuple((
            tag_token!('t, 's, Token::Parallel),
            comb::cut(seq::tuple((
                comb::opt(nom::error::context("merge strategy", seq::preceded(
                    tag_token!('t, 's, Token::LeftBracket),
                    comb::cut(seq::terminated(
                        auxillary::parse_merge,
                        tag_token!('t, 's, Token::RightBracket),
                    ))),
                )),
                seq::preceded(
                    tag_token!('t, 's, Token::LeftBracket),
                    multi::separated_list0(tag_token!('t, 's, Token::Comma), blocks::parse),
                ),
                tag_token!('t, 's, Token::RightBracket),
            )),
        )))),
        |(parallel, (merge, branches, rbrack)): (&Token, (Option<MergeStrategy>, Vec<Block>, &Token))| {
            Expression {
                kind : ExpressionKind::Parallel {
                    branches,
                    strategy : merge,
                },
                range : Some(TextRange::new(parallel.start_of(), rbrack.end_of())),
            }
        },
    )(input)
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
pub(crate) fn parse<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    pratt_parser(BindingPower::none())(input)
}
