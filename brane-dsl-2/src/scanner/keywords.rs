//  KEYWORDS.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 17:10:24
//  Last edited:
//    13 Feb 2023, 11:51:26
//  Auto updated?
//    Yes
// 
//  Description:
//!   Parses the keywords in BraneScript / Bakery.
// 

use nom::IResult;
use nom::{branch, bytes::complete as bc, character::complete as cc, combinator as comb, sequence as seq};

use super::{Error, Input};
use super::tokens::Token;


/***** HELPER FUNCTIONS *****/
/// Matches a separator for keywords.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and nothing.
/// 
/// # Errors
/// This function errors if we failed to parse a keyword for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
#[inline]
pub(crate) fn separator<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, (), E> {
    comb::not(branch::alt((cc::alphanumeric1, bc::tag("_"))))(input)
}





/***** LIBRARY *****/
/// Attempts to parse a keyword from the head of the given input.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and a token representing the parsed keyword.
/// 
/// # Errors
/// This function errors if we failed to parse a keyword for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    branch::alt((
        // Declarations
        comb::map(seq::terminated(bc::tag("import"), comb::peek(separator)), Token::Import),
        comb::map(seq::terminated(bc::tag("func"),   comb::peek(separator)), Token::Func),
        comb::map(seq::terminated(bc::tag("class"),  comb::peek(separator)), Token::Class),

        // Control flow
        comb::map(seq::terminated(bc::tag("if"),       comb::peek(separator)), Token::If),
        comb::map(seq::terminated(bc::tag("else"),     comb::peek(separator)), Token::Else),
        comb::map(seq::terminated(bc::tag("for"),      comb::peek(separator)), Token::For),
        comb::map(seq::terminated(bc::tag("from"),     comb::peek(separator)), Token::From),
        comb::map(seq::terminated(bc::tag("to"),       comb::peek(separator)), Token::To),
        comb::map(seq::terminated(bc::tag("step"),     comb::peek(separator)), Token::Step),
        comb::map(seq::terminated(bc::tag("while"),    comb::peek(separator)), Token::While),
        comb::map(seq::terminated(bc::tag("parallel"), comb::peek(separator)), Token::Parallel),
        comb::map(seq::terminated(bc::tag("return"),   comb::peek(separator)), Token::Return),

        // Miscellaneous
        comb::map(seq::terminated(bc::tag("let"), comb::peek(separator)), Token::Let),
        comb::map(seq::terminated(bc::tag("new"), comb::peek(separator)), Token::New),
        comb::map(seq::terminated(bc::tag("as"),  comb::peek(separator)), Token::As),
    ))(input)
}
