//  SCANNER.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 16:43:54
//  Last edited:
//    10 Feb 2023, 09:08:41
//  Auto updated?
//    Yes
// 
//  Description:
//!   Implements the main scanner logic.
// 

use nom::IResult;
use nom::{branch, character::complete as bc, combinator as comb, multi};
use nom::error::VerboseError;

use super::{Error, Input};
use super::tokens::Token;
use super::{comments, auxillary, literals, keywords, operators, punctuation};


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use enum_debug::EnumDebug as _;
    use brane_shr::utilities::test_on_dsl_files;
    use crate::errors::{DslError, ErrorTrace, PrettyError as _};
    use super::{scan_tokens, Input, Token};


    #[test]
    fn test_scanner() {
        test_on_dsl_files("BraneScript", |path: PathBuf, raw: String| {
            println!("{}", (0..80).map(|_| '-').collect::<String>());
            println!("File '{}' gave us:", path.display());

            // Scan the tokens
            let tokens: Vec<Token> = match scan_tokens(Input::new(&raw)) {
                Ok((remain, tokens)) => {
                    if !remain.is_empty() {
                        eprintln!("{}", DslError::ScanLeftoverError{ remainder: remain }.display_with_source(&path.display().to_string(), &raw));
                        panic!("Scanning failed (see above)");
                    }
                    tokens
                },
                Err(err) => {
                    eprintln!("{}", ErrorTrace::from_nom_err_scan(&path.display().to_string(), &raw, err).display());
                    panic!("Scanning failed (see above)");
                },
            };

            // Show the tokens
            println!("{}", tokens.into_iter().map(|t| format!("{}", t.variant())).collect::<Vec<String>>().join(" "));
            println!("{}\n\n", (0..80).map(|_| '-').collect::<String>());
        });
    }
}





/***** SCANNING FUNCTIONS *****/
/// Attempts to parse a single token from the head of the given input.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and the token that we parsed.
/// 
/// # Errors
/// This function errors if we failed to parse a comment for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
fn scan_token<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, Option<Token>, E> {
    // Keep trying until: eof or non-comment
    branch::alt((
        comb::value(None, comments::parse),
        comb::map(operators::parse, Some),
        comb::map(punctuation::parse, Some),
        comb::map(keywords::parse, Some),
        comb::map(literals::parse, Some),
        comb::map(auxillary::parse_array_type, Some),
        comb::map(auxillary::parse_ident, Some),
        comb::value(None, whitespace),
    ))(input)
}



/// Parses whitespace off the head of the given input.
/// 
/// # Arguments
/// - `input`: The input to parse off of.
/// 
/// # Returns
/// A tuple of the remaining input we did not parse and nothing, since we don't care about whitespace.
/// 
/// # Errors
/// This function errors if we failed to parse a comment for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
#[inline]
fn whitespace<'s, E: Error<'s>>(input: Input<'s>) -> IResult<Input<'s>, (), E> {
    comb::value((), bc::multispace1)(input)
}





/***** LIBRARY *****/
/// Scans all of the tokens in the given input to a serial list of tokens.
/// 
/// # Arguments
/// - `input`: The input to tokenize.
/// 
/// # Returns
/// A tuple with any input we failed to tokenize, and a vector with parsed tokens.
/// 
/// # Errors
/// This function errors with a VerboseError if we failed to tokenize everything.
#[inline]
pub fn scan_tokens<'s>(input: Input<'s>) -> IResult<Input<'s>, Vec<Token>, VerboseError<Input<'s>>> {
    multi::many0(scan_token)(input).map(|(rem, tok)| (rem, tok.into_iter().filter_map(|t| t).collect()))
}
