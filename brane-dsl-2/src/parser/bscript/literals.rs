//  LITERALS.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 15:04:30
//  Last edited:
//    09 Feb 2023, 14:24:19
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines parses for literals in the BraneScript/Bakery AST.
// 

use std::str::FromStr as _;

use nom::IResult;
use nom::{branch, combinator as comb};

use crate::ast::spec::TextRange;
use crate::ast::expressions::{Literal, LiteralKind};
use crate::parser::{Error, Input};
use crate::parser::utils::tag_token;


/***** HELPER FUNCTIONS *****/
/// Processes the matched string to get a value that has escaped characters already resolved.
/// 
/// # Arguments
/// - `raw`: The raw string that was matched.
/// 
/// # Returns
/// The compatible value for the string.
fn process_string_value(raw: &str) -> String {
    // Loop to add
    let mut res     : String = String::with_capacity(raw.len());
    let mut escaped : bool   = false;
    for c in raw.chars() {
        // Check if escaped
        if escaped {
            // We are; match a specific set of characters
            if c == 'n' {
                res.push('\n');
            } else if c == 'r' {
                res.push('\r');
            } else if c == 't' {
                res.push('\t');
            } else {
                // Other characters are just passed "as-is", but without the slash (unless this is a slash aof course).
                res.push(c);
            }
            escaped = false;
        } else if c == '\\' {
            // Going into escape mode
            escaped = true;
        } else {
            res.push(c);
        }
    }

    // Done
    res
}





/***** LIBRARY *****/
/// Parses a literal from the head of the given token stream.
/// 
/// # Arguments
/// - `input`: The Input list of tokens to parse from.
/// 
/// # Returns
/// A tuple with the remaining input and the parsed literal.
/// 
/// # Errors
///  This function errors if we failed to parse an operator for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'t, 's, E: Error<'t, 's>>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Literal, E> {
    branch::alt((
        comb::map(tag_token!('t, 's, Token::Boolean), |t| Literal{ kind: LiteralKind::Boolean{ value: bool::from_str(t.span().fragment()).unwrap() }, range: Some(TextRange::from(t.span())) }),
        comb::map(tag_token!('t, 's, Token::Integer), |t| Literal{ kind: LiteralKind::Integer{ value: i64::from_str(t.span().fragment()).unwrap() }, range: Some(TextRange::from(t.span())) }),
        comb::map(tag_token!('t, 's, Token::Real),    |t| Literal{ kind: LiteralKind::Real{ value: f64::from_str(t.span().fragment()).unwrap() }, range: Some(TextRange::from(t.span())) }),
        comb::map(tag_token!('t, 's, Token::String),  |t| Literal{ kind: LiteralKind::String{ value: process_string_value(*t.span().fragment()) }, range: Some(TextRange::from(t.span())) }),
    ))(input)
}
