//  INSTANCES.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 19:07:08
//  Last edited:
//    17 Feb 2023, 15:47:31
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines how to parse an instance for the BraneScript/Bakery AST.
// 

use nom::IResult;
use nom::{branch, combinator as comb, sequence as seq};

use crate::ast::spec::TextRange;
use crate::ast::auxillary::Identifier;
use crate::ast::expressions::{Expression, ExpressionKind, PropertyExpr};
use crate::scanner::Token;
use crate::parser::{Error, Input};
use crate::parser::utils::{self, tag_token};
use super::{auxillary, expressions};


/***** SCANNING FUNCTIONS *****/
/// Parses a property expression from the top of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed property expression.
/// 
/// # Errors
/// This function errors if we failed to parse a property expression for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn prop_expr<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, PropertyExpr, Error<'t, 's>> {
    comb::map(
        nom::error::context("a property/field expression", seq::pair(
            auxillary::parse_ident,
            comb::cut(seq::preceded(
                tag_token!('t, 's, Token::Assign),
                expressions::parse,
            )),
        )),
        |(name, value): (Identifier, Expression)| {
            // Compute the range
            let range: Option<TextRange> = match (name.range, value.range) {
                (Some(TextRange{ start, .. }), Some(TextRange{ end, .. })) => Some(TextRange::new(start, end)),
                _                                                          => None,
            };

            // Create a property expression from that
            PropertyExpr {
                name,
                value : Box::new(value),
                range,
            }
        },
    )(input)
}

// Parses a local instance expression from the top off the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed instance expression.
/// 
/// # Errors
/// This function errors if we failed to parse an instance expression for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn local<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    comb::map(
        nom::error::context("a local instance expression", seq::pair(
            tag_token!('t, 's, Token::New),
            comb::cut(seq::tuple((
                seq::terminated(
                    auxillary::parse_ident,
                    tag_token!('t, 's, Token::LeftBrace),
                ),
                utils::separated_list0(tag_token!('t, 's, Token::Comma), prop_expr),
                tag_token!('t, 's, Token::RightBrace),
            ))),
        )),
        |(new, (ident, props, rbrace)): (&Token, (Identifier, Vec<PropertyExpr>, &Token))| {
            Expression {
                kind : ExpressionKind::LocalInstance {
                    name : ident,
                    props,

                    st_entry : None,
                },
                range : Some(TextRange::new(new.start_of(), rbrace.end_of())),
            }
        }
    )(input)
}

// Parses a remote instance expression from the top off the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed instance expression.
/// 
/// # Errors
/// This function errors if we failed to parse an instance expression for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn remote<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    comb::map(
        nom::error::context("a remote instance expression", seq::tuple((
            tag_token!('t, 's, Token::New),
            seq::terminated(
                auxillary::parse_ident,
                seq::pair(tag_token!('t, 's, Token::Colon), tag_token!('t, 's, Token::Colon)),
            ),
            comb::cut(seq::tuple((
                seq::terminated(
                    auxillary::parse_ident,
                    tag_token!('t, 's, Token::LeftBrace),
                ),
                utils::separated_list0(tag_token!('t, 's, Token::Comma), prop_expr),
                tag_token!('t, 's, Token::RightBrace),
            ))),
        ))),
        |(new, package, (ident, props, rbrace)): (&Token, Identifier, (Identifier, Vec<PropertyExpr>, &Token))| {
            Expression {
                kind : ExpressionKind::RemoteInstance {
                    name : ident,
                    package,
                    props,

                    st_entry : None,
                },
                range : Some(TextRange::new(new.start_of(), rbrace.end_of())),
            }
        }
    )(input)
}





/***** LIBRARY *****/
/// Attempts to parse an instance off of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed instance expression.
/// 
/// # Errors
/// This function errors if we failed to parse an instance expression for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, Error<'t, 's>> {
    // Do them in reverse order to separate between remote and local expressions
    branch::alt((
        remote,
        local,
    ))(input)
}
