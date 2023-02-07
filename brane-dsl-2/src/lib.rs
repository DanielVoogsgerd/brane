//  LIB.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:25:18
//  Last edited:
//    07 Feb 2023, 11:38:25
//  Auto updated?
//    Yes
// 
//  Description:
//!   The `brane-dsl` crate provides a compiler for BraneScript and/or
//!   Bakery to Brane's internal workflow representation defined in
//!   `brane-ast`.
// 

// Declare modules
pub mod errors;
pub mod ast;
mod scanner;
mod parser;
mod compiler;

// Define some useful abstraction over a DslError
pub use errors::DslError as Error;



/***** LIBRARY *****/
/// Toplevel comilation function that drives the entire compilation process.
/// 
/// # Arguments
/// - ``
/// 
/// # Returns
/// A fully parsed module.
/// 
/// # Errors
/// 
pub fn compile_module(source: &str) -> Result<(), Error> {
    use scanner::tokens::Token;

    // Scan the input text to a string of tokens
    let tokens: Vec<Token> = match scanner::scan_tokens(scanner::Input::new(&source)) {
        Ok((rem, tokens)) => {
            if !rem.is_empty() { return Err(Error::ScanLeftoverError{ remainder: rem }); }
            tokens
        },
        Err(err) => { return Err(Error::ScanError{ err }); },
    };

    // Done
    Ok(())
}
