//  TOPLEVEL.rs
//    by Lut99
// 
//  Created:
//    08 Feb 2023, 13:20:01
//  Last edited:
//    17 Jun 2023, 10:13:01
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines functions to parse everything.
// 

use std::cell::RefCell;
use std::rc::Rc;

use nom::IResult;
use nom::{combinator as comb, multi};
use nom::error::{ErrorKind, FromExternalError as _};

use crate::errors::ParseError;
use crate::ast::spec::TextRange;
use crate::ast::symbol_tables::SymbolTable;
use crate::ast::statements::Statement;
use crate::ast::toplevel::Program;
use crate::parser::{Error, Input};
use super::statements;


/***** LIBRARY *****/
/// Attempts to parse a program from the given tokenstream.
/// 
/// # Arguments
/// - `input`: The new tokenstream to parse from.
/// 
/// # Returns
/// A tuple of the remaining, unparsed tokenstream and the parsed program.
/// 
/// # Errors
/// This function errors if we failed to parse a program for whatever reason. A `nom::Err::Error` means that it may be something else on top of there, but `nom::Err::Failure` means that the stream will never be valid.
pub(crate) fn parse<'t, 's>(input: Input<'t, 's>) -> IResult<Input<'t, 's>, Program, Error<'t, 's>> {
    // NEW
    // Parse the first statement if any, where we accept it if there was something else/nothing.
    let (mut input, mut stmts): (Input, Vec<Statement>) = match statements::parse(input) {
        // Parsing success
        Ok((input, stmt)) => (input, vec![ stmt ]),

        // Handles the 'multi*0*` part
        Err(nom::Err::Error(_)) => { return Ok((input, Program{
            stmts : vec![],
            range : None,

            annots : vec![],
            table  : Rc::new(RefCell::new(SymbolTable::empty())),
        })); },

        // Incomplete output
        Err(nom::Err::Incomplete(needed)) => { return Err(nom::Err::Error(Error::from_external_error(input, ErrorKind::Eof, ParseError::IncompleteStatement { needed }))) },
        // Parsing error (not just failed)
        Err(nom::Err::Failure(err))       => { return Err(nom::Err::Failure(err)); },
    };

    // Now we do the remainder of a bit of a custom multi::many0, which expects a separating `;` or not based on the statement just parsed
    loop {
        
    }



    // // OLD
    // comb::map(
    //     multi::many0(statements::parse),
    //     |stmts: Vec<Statement>| {
    //         // Compute the entire program's range
    //         let range: Option<TextRange> = match (stmts.first(), stmts.last()) {
    //             (Some(Statement{ range: Some(TextRange{ start, .. }), .. }), Some(Statement{ range: Some(TextRange{ end, .. }), .. })) => Some(TextRange::new(start, end)),
    //             _                                                                                                                      => None,
    //         };

    //         // Return the program
    //         Program {
    //             stmts,
    //             range,

    //             table  : Rc::new(RefCell::new(SymbolTable::empty())),
    //             annots : vec![],
    //         }
    //     }
    // )(input)
}
