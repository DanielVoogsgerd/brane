//  LIB.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 15:25:18
//  Last edited:
//    11 Feb 2023, 18:13:28
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
pub mod warnings;
pub mod notes;
pub mod ast;
mod scanner;
mod parser;
mod compiler;

// Define some useful abstraction over a DslError
pub use errors::{DslError as Error, ErrorTrace};



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
pub fn compile_module<'f, 's>(file: &'f str, source: &'s str, phase: compiler::CompilerPhase) -> Result<(), ErrorTrace<'f, 's>> {
    use ast::toplevel::Program;
    use scanner::tokens::Token;
    use compiler::{traversals, CompilerPhase};

    // Scan the input text to a string of tokens
    let tokens: Vec<Token> = match scanner::scan_tokens(scanner::Input::new(&source)) {
        Ok((rem, tokens)) => {
            if !rem.is_empty() { return Err(ErrorTrace::from_error(file, source, Error::ScanLeftoverError{ remainder: rem })); }
            tokens
        },
        Err(err) => { return Err(ErrorTrace::from_nom_err_scan(file, source, err)); },
    };

    // Parse the token stream to an AST
    let ast: Program = match parser::parse_tokens(&tokens) {
        Ok((rem, ast)) => {
            if !rem.is_empty() { return Err(ErrorTrace::from_error(file, source, Error::ParseLeftoverError{ remainder: rem.into() })); }
            ast
        },
        Err(err) => { return Err(ErrorTrace::from_nom_err_parse(file, source, err)); }
    };

    // We print and done if we're told to do that phase
    if phase == CompilerPhase::Print { traversals::print_ast::traverse(&mut std::io::stdout(), &ast).unwrap_or_else(|err| panic!("Failed to write to stderr: {}", err)); return Ok(()); }

    // Else, match the phases to do
    if phase >= CompilerPhase::Resolve {
        traversals::resolve::traverse(&mut ast);
    }

    // Done
    Ok(())
}
