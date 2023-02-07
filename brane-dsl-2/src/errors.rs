//  ERRORS.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 10:10:18
//  Last edited:
//    07 Feb 2023, 12:02:04
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines errors originating from the `brane-dsl` crate.
// 

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};

use console::{style, Style};
use nom::error::VerboseError;

use crate::ast::spec::TextRange;
use crate::scanner::Input;


/***** HELPER FUNCTIONS *****/
/// Computes the length of the number as if it was a string.
/// 
/// # Generic arguments
/// - `N`: The f64-like type of `n`.
/// 
/// # Arguments
/// - `n`: The number to compute the length of.
/// 
/// # Returns
/// The number of digits in the number.
#[inline]
fn num_len<N: Into<usize>>(n: N) -> usize {
    ((n.into() as f64).log10() + 1.0) as usize
}

/// Pads the given number by adding enough spaced prefix to reach the desired length.
/// 
/// # Generic arguments
/// - `N`: The usize-like type of `n`.
/// 
/// # Arguments
/// - `n`: The number to pad.
/// - `l`: The to-be-padded-to length.
/// 
/// # Returns
/// The number as a string with appropriate padding.
#[inline]
fn pad_num<N: Copy + Into<usize>>(n: N, l: usize) -> String {
    format!("{}{}", (0..l - num_len(n)).map(|_| ' ').collect::<String>(), n.into())
}

/// Writes the given range of the given source to the given formatter.
/// 
/// # Arguments
/// - `f`: The Formatter to write to.
/// - `range`: The TextRange to print in the given source text.
/// - `source`: The source text to print.
/// - `colour`: The Style to print the markers with (i.e., red for error, yellow for warning, etc).
/// 
/// # Errors
/// This function errors if we failed to write to the given writer.
fn print_range(f: &mut Formatter<'_>, range: TextRange, source: &str, colour: Style) -> FResult {
    // Find the start of the range in the source text
    let mut line_i     : usize        = 0;
    let mut line_start : usize        = 0;
    let mut line       : Option<&str> = None;
    for (i, c) in source.char_indices() {
        // Search until the end of the line
        if c == '\n' {
            if line_i == range.start.line0() {
                // It's the correct line; take it
                line = Some(&source[line_start..i]);
                break;
            }
            line_start  = i + 1;
            line_i     += 1;
        }
        
    }
    if line.is_none() && line_start < source.len() && line_i == range.start.line0() { line = Some(&source[line_start..]); }
    let line: &str = line.unwrap_or_else(|| panic!("A range of {} is out-of-bounds for given source text.", range));

    // Now print the line up until the correct position
    let red_start : usize = range.start.col0();
    let red_end   : usize = if range.start.line == range.end.line { range.end.col1() } else { line.len() };
    write!(f, "{} {}", style(format!(" {} |", if range.start.line == range.end.line { format!("{}", range.start.line1()) } else { pad_num(range.start.line1(), num_len(range.end.line1())) })).blue().bright(), &line[0..red_start])?;
    // Print the red part
    write!(f, "{}", colour.apply_to(&line[red_start..red_end]))?;
    // Print the rest (if any)
    writeln!(f, "{}", &line[red_end..])?;

    // Print the red area
    writeln!(f, " {} {} {}{}",
        (0..(if range.start.line == range.end.line { num_len(range.start.line1()) } else { num_len(range.end.line1()) })).map(|_| ' ').collect::<String>(),
        style("|").blue().bright(),
        (0..red_start).map(|_| ' ').collect::<String>(),
        colour.apply_to((red_start..red_end).map(|_| '^').collect::<String>()),
    )?;

    // If the range is longer, print dots
    if range.start.line != range.end.line {
        writeln!(f, "{} {}", style(format!(" {} |", range.start.line1() + 1)).blue().bright(), colour.apply_to("..."))?;
        writeln!(f, "{} {}", style(format!(" {} |", (0..num_len(range.end.line1())).map(|_| ' ').collect::<String>())).blue().bright(), colour.apply_to("^^^"))?;
    }

    // Done
    Ok(())
}





/***** AUXILLARY *****/
/// The pretty formatter for most errors.
#[derive(Debug)]
pub struct PrettyErrorFormatter<'e, 'f, 's> {
    /// The error to format.
    err    : &'e dyn PrettyError,

    /// The name of the file we are compiling.
    file   : &'f str,
    /// The source text to use as context.
    source : &'s str,
}
impl<'e, 'f, 's> Display for PrettyErrorFormatter<'e, 'f, 's> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Get the ranges to print
        let (main, notes): ((String, Option<TextRange>), Vec<(String, TextRange)>) = self.err.ranges();

        // Print the main error
        if let Some(range) = main.1 {
            // Write the top line
            writeln!(f, "{}: {}: {}", style(format!("{}:{}:{}", self.file, range.start.line, range.start.col)).bold(), style("error").red().bold(), main.0)?;

            // Write the range
            print_range(f, range, self.source, Style::new().red().bold())?;
            writeln!(f)?;
        } else {
            // Write the top line without context
            writeln!(f, "{}: {}: {}", style(self.file), style("error").red().bold(), main.0)?;
            writeln!(f)?;
        }

        // Now print any additional notes
        for (msg, range) in notes {
            // Write the top line
            writeln!(f, "{}: {}: {}", style(format!("{}:{}:{}", self.file, range.start.line, range.start.col)).bold(), style("error").red().bold(), msg)?;

            // Write the range
            print_range(f, range, self.source, Style::new().green().bold())?;
            writeln!(f)?;
        }

        // Done
        Ok(())
    }
}



/// Trait for an error that can print itself very prettily from some source text.
pub trait PrettyError: Error {
    // Child-implemented
    /// Returns the ranges that this error concerns itself with.
    /// 
    /// Specifically, it can return one "main error range", and then zero or more "note ranges" that provide additional context.
    /// 
    /// # Returns
    /// A tuple with the main error range (if any) and the vector with note texts and note ranges, respectively.
    /// 
    /// Note that an empty main error range implies this error variant does not relate to source.
    fn ranges(&self) -> ((String, Option<TextRange>), Vec<(String, TextRange)>);


    // Globally provided
    /// Returns a formatter for an Error that writes it to stderr with some additional context information attached to it.
    /// 
    /// # Arguments
    /// - `file`: Some name that represents the source. Typically the filename for a file, or something like "<stdin>" for stdin.
    /// - `source`: The source text to use for context. We assume that the positions in this error match that of the given source text.
    /// 
    /// # Returns
    /// A `PrettyErrorFormatter` that implements Display.
    #[inline]
    fn display_with_source<'e, 'f, 's>(&'e self, file: &'f str, source: &'s str) -> PrettyErrorFormatter<'e, 'f, 's> where Self: Sized { PrettyErrorFormatter{ err: self, file, source } }
}





/***** LIBRARY *****/
/// Wraps any pretty error to show context without having to explicitly mention the filename or the source name.
#[derive(Debug)]
pub struct ErrContext<'f, 's, E> {
    /// The error to wrap.
    err : E,

    /// Some name for the source text.
    file   : &'f str,
    /// The source text.
    source : &'s str,
}
impl<'f, 's, E: PrettyError> ErrContext<'f, 's, E> {
    /// Returns a formatter for an Error that writes it to stderr with some additional context information attached to it.
    /// 
    /// # Returns
    /// A `PrettyErrorFormatter` that implements Display.
    #[inline]
    pub fn display<'e>(&'e self) -> PrettyErrorFormatter<'e, 'f, 's> { PrettyErrorFormatter{ err: &self.err, file: self.file, source: self.source } }
}
impl<'f, 's, E: PrettyError> PrettyError for ErrContext<'f, 's, E> {
    #[inline]
    fn ranges(&self) -> ((String, Option<TextRange>), Vec<(String, TextRange)>) {
        self.err.ranges()
    }
}
impl<'f, 's, E: Display> Display for ErrContext<'f, 's, E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "{}", self.err)
    }
}
impl<'f, 's, E: Error> Error for ErrContext<'f, 's, E> {}



/// Defines the most toplevel errors for this crate.
#[derive(Debug)]
pub enum DslError<'s> {
    /// Failed to scan the input.
    ScanError{ err: nom::Err<VerboseError<Input<'s>>> },
    /// Not all input was able to be parsed.
    ScanLeftoverError{ remainder: Input<'s> },
}
impl<'s> PrettyError for DslError<'s> {
    fn ranges(&self) -> ((String, Option<TextRange>), Vec<(String, TextRange)>) {
        use DslError::*;
        match self {
            ScanError{ .. }                => ((self.to_string(), None), vec![]),
            ScanLeftoverError{ remainder } => ((self.to_string(), Some(TextRange::from(remainder))), vec![]),
        }
    }
}
impl<'s> Display for DslError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DslError::*;
        match self {
            ScanError{ err }        => write!(f, "Syntax error: {}", err),
            ScanLeftoverError{ .. } => write!(f, "Syntax error: Cannot parse remainder of source"),
        }
    }
}
impl<'s> Error for DslError<'s> {}
