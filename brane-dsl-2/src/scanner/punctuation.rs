//  PUNCTUATION.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 17:42:41
//  Last edited:
//    07 Feb 2023, 11:37:34
//  Auto updated?
//    Yes
// 
//  Description:
//!   Parses punctiation characters for BraneScript and Bakery.
// 

use nom::IResult;
use nom::{branch, bytes::complete as bc, combinator as comb};

use super::{Error, Input};
use super::tokens::Token;


/***** LIBRARY *****/
/// Attempts to parse punctuation from the head of the given input.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and a token representing the parsed punctuation.
/// 
/// # Errors
/// This function errors if we failed to parse punctuation for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Token, E> {
    branch::alt((
        // Dot-related
        comb::map(bc::tag("."), Token::Dot),
        comb::map(bc::tag(","), Token::Comma),
        comb::map(bc::tag(":"), Token::Colon),
        comb::map(bc::tag(";"), Token::Semicolon),

        // Brackets
        comb::map(bc::tag("("), Token::LeftParen),
        comb::map(bc::tag(")"), Token::RightParen),
        comb::map(bc::tag("["), Token::LeftBracket),
        comb::map(bc::tag("]"), Token::RightBracket),
        comb::map(bc::tag("{"), Token::LeftBrace),
        comb::map(bc::tag("}"), Token::RightBrace),

        // Miscellaneous
        comb::map(bc::tag("#"), Token::Hashtag),
    ))(input)
}
