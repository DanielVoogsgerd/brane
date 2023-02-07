//  INSTANCES.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 19:07:08
//  Last edited:
//    07 Feb 2023, 19:29:43
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines how to parse an instance for the BraneScript/Bakery AST.
// 

use nom::IResult;
use nom::{branch, combinator as comb, multi, sequence as seq};

use crate::ast::spec::TextRange;
use crate::ast::auxillary::Identifier;
use crate::ast::expressions::{Expression, ExpressionKind, PropertyExpr};
use crate::scanner::Token;
use crate::parser::{Error, Input};
use crate::parser::utils::tag_token;
use super::{auxillary, expressions};


/***** SCANNING FUNCTIONS *****/
/// Parses a property expression from the top of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed instance expression.
/// 
/// # Errors
/// This function errors if we failed to parse an instance expression for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn prop_expr<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, PropertyExpr, E> {
    comb::map(
        branch::alt((
            seq::separated_pair(
                auxillary::parse_ident,
                tag_token!(Token::Assign),
                expressions::parse,
            ),
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
pub fn parse<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Expression, E> {
    comb::map(
        seq::tuple((
            tag_token!(Token::New),
            seq::terminated(
                auxillary::parse_ident,
                tag_token!(Token::LeftBrace),
            ),
            multi::separated_list0(tag_token!(Token::Comma), prop_expr),
            tag_token!(Token::RightBrace),
        )),
        |(new, ident, props, rbrace): (&Token, Identifier, Vec<PropertyExpr>, &Token)| {
            Expression {
                kind : ExpressionKind::Instance {
                    name : ident,
                    props,
                },
                range : Some(TextRange::new(new.start_of(), rbrace.end_of())),
            }
        }
    )(input)
}
