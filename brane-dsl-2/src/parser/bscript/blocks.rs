//  BLOCKS.rs
//    by Lut99
// 
//  Created:
//    08 Feb 2023, 10:17:31
//  Last edited:
//    10 Feb 2023, 08:42:19
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines parsers for blocks (nested scope, if-statements,
//!   parallel-statements) that can be given in expression position.
// 

use nom::IResult;
use nom::{combinator as comb, multi, sequence as seq};

use crate::ast::spec::TextRange;
use crate::ast::expressions::Block;
use crate::ast::statements::Statement;
use crate::scanner::Token;
use crate::parser::{Error, Input};
use crate::parser::utils::tag_token;
use super::statements;


/***** LIBRARY *****/
/// Attempts to parse a nested scope off the top of the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed block.
/// 
/// # Errors
/// This function errors if we failed to parse a block for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Block, E> {
    // A block is separated by curly brackets
    comb::map(
        nom::error::context("a block", seq::pair(
            tag_token!('t, 's, Token::LeftBrace),
            comb::cut(seq::pair(
                multi::many0(statements::parse),
                tag_token!('t, 's, Token::RightBrace),
            )),
        )),
        |(lbrace, (stmts, rbrace)): (&Token, (Vec<Statement>, &Token))| {
            Block {
                stmts,
                range : Some(TextRange::new(lbrace.start_of(), rbrace.end_of())),
            }
        }
    )(input)
}
