//  UTILS.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 13:09:48
//  Last edited:
//    07 Feb 2023, 13:23:47
//  Auto updated?
//    Yes
// 
//  Description:
//!   Implements various utilities to use while parsing.
// 


/***** LIBRARY *****/
/// Creates a parser for parsing a given token.
/// 
/// # Arguments
/// - `token`: The token to match.
/// 
/// # Returns
/// A function that will attempt to parse the token from its input.
macro_rules! tag_token {
    (Token::$token:ident) => {
        // Create the closure that does the work
        |input: crate::parser::Input| -> nom::IResult<crate::parser::Input, &crate::scanner::Token, E> {
            // Make sure there is a token to match
            if input.is_empty() { return Err(nom::Err::Incomplete(nom::Needed::Size(std::num::NonZeroUsize::new(1).unwrap()))) }

            // See if the head matches
            if matches!(input[0], crate::scanner::Token::$token(_)) {
                // Return it
                Ok((&input[1..], &input[0]))
            } else {
                Err(nom::Err::Error(E::from_error_kind(input, nom::error::ErrorKind::Tag)))
            }
        }
    };
}
pub(crate) use tag_token;
