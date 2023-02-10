//  UTILS.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 13:09:48
//  Last edited:
//    10 Feb 2023, 19:20:19
//  Auto updated?
//    Yes
// 
//  Description:
//!   Implements various utilities to use while parsing.
// 

use nom::IResult;
use nom::{combinator as comb, multi};

use crate::parser::{Error, Input};


/***** LIBRARY *****/
/// Creates a parser for parsing a given token.
/// 
/// # Arguments
/// - `token`: The token to match.
/// 
/// # Returns
/// A function that will attempt to parse the token from its input.
macro_rules! tag_token {
    ($t:lifetime, $s:lifetime, Token::$token:ident) => {
        // Create the closure that does the work
        |input: crate::parser::Input<$t, $s>| -> nom::IResult<crate::parser::Input<$t, $s>, &$t crate::scanner::Token<$s>, Error<'t, 's>> {
            use nom::error::ParseError as _;

            // Make sure there is a token to match
            if input.is_empty() { return Err(nom::Err::Error(Error::from_error_kind(input, nom::error::ErrorKind::Tag))) }

            // See if the head matches
            if matches!(input[0], crate::scanner::Token::$token(_)) {
                // Return it
                Ok((&input[1..], &input[0]))
            } else {
                Err(nom::Err::Error(Error::from_error_kind(input, nom::error::ErrorKind::Tag)))
            }
        }
    };
}
pub(crate) use tag_token;



/// Generates a more Rust-y version of the default `separated_list0` that supports and optional comma after the list.
/// 
/// # Arguments
/// - `sep`: The separator to search for that separates the list, and provides an optional end separator.
/// - `elem`: The parser for each element in the list.
/// 
/// # Returns
/// A new parser that can parse a list of `elem` elements, separated by `sep`, where it may end (but doesn't have to) in a `sep`.
pub(crate) fn separated_list0<'t, 's: 't, F1, R1, F2, R2>(mut sep: F1, mut elem: F2) -> impl FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Vec<R2>, Error<'t, 's>>
where
    F1: FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, R1, Error<'t, 's>>,
    F2: FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, R2, Error<'t, 's>>,
{
    move |input| {
        // First parse the separated list like normal
        let (rem, list): (Input, Vec<R2>) = multi::separated_list0(&mut sep, &mut elem)(input)?;
        // Before we return, if there is a separator left, consume that as well
        let (rem, _): (Input, _) = comb::opt(&mut sep)(rem)?;
        // Done
        Ok((rem, list))
    }
}

/// Generates a more Rust-y version of the default `separated_list1` that supports and optional comma after the list.
/// 
/// # Arguments
/// - `sep`: The separator to search for that separates the list, and provides an optional end separator.
/// - `elem`: The parser for each element in the list.
/// 
/// # Returns
/// A new parser that can parse a list of `elem` elements, separated by `sep`, where it may end (but doesn't have to) in a `sep`.
/// 
/// In contrast to `separated_list0`, at least one element will have to be present.
pub(crate) fn separated_list1<'t, 's: 't, F1, R1, F2, R2>(mut sep: F1, mut elem: F2) -> impl FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, Vec<R2>, Error<'t, 's>>
where
    F1: FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, R1, Error<'t, 's>>,
    F2: FnMut(Input<'t, 's>) -> IResult<Input<'t, 's>, R2, Error<'t, 's>>,
{
    move |input| {
        // First parse the separated list like normal
        let (rem, list): (Input, Vec<R2>) = multi::separated_list1(&mut sep, &mut elem)(input)?;
        // Before we return, if there is a separator left, consume that as well
        let (rem, _): (Input, _) = comb::opt(&mut sep)(rem)?;
        // Done
        Ok((rem, list))
    }
}
