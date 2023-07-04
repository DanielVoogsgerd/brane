//  PARSER.rs
//    by Lut99
// 
//  Created:
//    08 Feb 2023, 13:28:35
//  Last edited:
//    10 Feb 2023, 11:15:46
//  Auto updated?
//    Yes
// 
//  Description:
//!   Implements the main parser logic.
// 

use nom::IResult;

use crate::errors::NomError;
use crate::ast::Program;
use super::Input;
use super::bscript::toplevel;


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use brane_shr::utilities::test_on_dsl_files;
    use crate::errors::{DslError, ErrorTrace, PrettyError as _};
    use crate::ast::Program;
    use crate::scanner::{scan_tokens, Input as ScanInput};
    use super::parse_tokens;


    #[test]
    fn test_parser() {
        test_on_dsl_files("BraneScript", |path: PathBuf, raw: String| {
            println!("{}", (0..80).map(|_| '-').collect::<String>());
            println!("File '{}' gave us:", path.display());

            // Scan the tokens
            let ast: Program = match parse_tokens(&scan_tokens(ScanInput::new(&raw)).unwrap().1) {
                Ok((remain, ast)) => {
                    if !remain.is_empty() {
                        eprintln!("{}", DslError::ParseLeftoverError{ remainder: remain.into() }.display_with_source(&path.display().to_string(), &raw));
                        panic!("Scanning failed (see above)");
                    }
                    ast
                },
                Err(err) => {
                    eprintln!("{}", ErrorTrace::from_nom_err_parse(&path.display().to_string(), &raw, err).display());
                    panic!("Scanning failed (see above)");
                },
            };

            // Show the tokens
            println!("{:#?}", ast);
            println!("{}\n\n", (0..80).map(|_| '-').collect::<String>());
        });
    }
}





/***** LIBRARY *****/
/// Builds an AST from the given tokenstream.
/// 
/// # Arguments
/// - `input`: The input to parse.
/// 
/// # Returns
/// A tuple with any input we failed to tokenize, and the toplevel Program node.
/// 
/// # Errors
/// This function errors with a NomError if we failed to parse something.
#[inline]
pub fn parse_tokens<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Program, NomError<'s, Input<'t, 's>>> {
    toplevel::parse(input)
}