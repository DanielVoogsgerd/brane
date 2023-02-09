//  ERRORS.rs
//    by Lut99
// 
//  Created:
//    07 Feb 2023, 10:10:18
//  Last edited:
//    09 Feb 2023, 18:41:31
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines errors originating from the `brane-dsl` crate.
// 

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};

use console::{style, Style};
use enum_debug::EnumDebug;
use nom::error::{ErrorKind, VerboseError, VerboseErrorKind};
use unicode_segmentation::UnicodeSegmentation as _;

use crate::notes::{NomNote, PrettyNote};
use crate::ast::spec::{TextPos, TextRange};
use crate::scanner::{Input as ScanInput, Token};
use crate::parser::Input as ParseInput;


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
pub(crate) fn print_range(f: &mut Formatter<'_>, range: TextRange, source: &str, colour: Style) -> FResult {
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

/// Attempts to find a range that points to the end of the given string.
/// 
/// # Arguments
/// - `text`: The string to find the end of.
/// 
/// # Returns
/// The range pointing to the last character in it, or else `None` if it was empty.
fn last_char_range(text: &str) -> Option<TextRange> {
    // We can early quit if the text is empty
    if text.is_empty() { return None; }

    // The find the number of lines and the position of the last line
    let mut n_lines : usize         = 0;
    let mut last_nl : Option<usize> = None;
    for (i, c) in text.char_indices() {
        if c == '\n' {
            n_lines += 1;
            last_nl  = Some(i);
        }
    }

    // Return a range with that
    Some(TextRange::new(
        TextPos::new0(0, 0),
        TextPos::new0(
            n_lines,
            if let Some(last_nl) = last_nl {
                text.len() - 1 - last_nl
            } else {
                text.len() - 1
            }
        )
    ))
}





/***** HELPER STRUCTS/ENUMS *****/
/// Abstraction over errors or notes.
#[derive(Debug, EnumDebug)]
enum TraceElem<'s> {
    /// It's a hard error
    Error(Box<dyn 's + PrettyError>),
    /// It's a note providing context to some error.
    Note(Box<dyn 's + PrettyNote>),
}
impl<'s> TraceElem<'s> {
    /// Returns the TraceElem's internal error.
    /// 
    /// # Returns
    /// The `dyn PrettyError` we wrap.
    /// 
    /// # Panics
    /// This function panics if we were not a `TraceElem::Error`, oh no, but a `TraceElem::Note` instead.
    fn error(&self) -> &dyn PrettyError {
        if let Self::Error(err) = self {
            &**err
        } else {
            panic!("Cannot unwrap TraceElem::{} as a TraceElem::Error", self.variant());
        }
    }
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
        let range: Option<TextRange> = self.err.range();

        // Print the main error
        if let Some(range) = range {
            // Write the top line
            writeln!(f, "{}: {}: {}", style(format!("{}:{}:{}", self.file, range.start.line, range.start.col)).bold(), style("error").red().bold(), self.err)?;

            // Write the range
            print_range(f, range, self.source, Style::new().red().bold())?;
            writeln!(f)?;
        } else {
            // Write the top line without context
            writeln!(f, "{}: {}: {}", style(self.file), style("error").red().bold(), self.err)?;
            writeln!(f)?;
        }
        writeln!(f)?;

        // Done
        Ok(())
    }
}

/// The pretty formatter for the error trace.
#[derive(Debug)]
pub struct ErrorTraceFormatter<'t, 'f, 's> {
    /// The trace to format.
    trace : &'t [TraceElem<'s>],

    /// The name (or other description) for the source.
    file   : &'f str,
    /// The source text itself.
    source : &'s str,
}
impl<'t, 'f, 's> Display for ErrorTraceFormatter<'t, 'f, 's> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Iterate over the trace elements to print them
        for elem in self.trace {
            // The easy part is that we can leave most of it up to the individual error's formatters
            match elem {
                TraceElem::Error(err) => { writeln!(f, "{}", err.display_with_source(self.file, self.source))?; },
                TraceElem::Note(note) => { writeln!(f, "{}", note.display_with_source(self.file, self.source))?; },
            }
        }

        // Done
        Ok(())
    }
}



/// A helper trait that we can use to cast a PrettyError-implemented (sized) type to a `&dyn PrettyError`.
pub trait PrettyErrorAsDyn {
    /// Returns this struct as a type-erased dynamic trait object reference.
    fn as_dyn(&self) -> &dyn PrettyError;
}
impl<T: PrettyError + Sized> PrettyErrorAsDyn for T {
    fn as_dyn(&self) -> &dyn PrettyError { self }
}

/// Trait for an error that can print itself very prettily from some source text.
pub trait PrettyError: Error + PrettyErrorAsDyn {
    // Child-implemented
    /// Returns the range of this error that relates it to the source text.
    /// 
    /// # Returns
    /// The error's TextRange. If there is no range associated with this error (i.e., it does not relate to the source), then `None` is returned instead.
    fn range(&self) -> Option<TextRange>;

    /// Returns any notes that should be attached when showing this error.
    /// 
    /// # Returns
    /// A vector of `PrettyNote`-implementing `Note`-structures that describe the notes to add.
    fn notes(&self) -> Vec<Box<dyn PrettyNote>>;


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
    fn display_with_source<'e, 'f, 's>(&'e self, file: &'f str, source: &'s str) -> PrettyErrorFormatter<'e, 'f, 's> { PrettyErrorFormatter{ err: self.as_dyn(), file, source } }
}





/***** LIBRARY *****/
/// Defines a trace of DslErrors and NomNotes that may occur during scanning or parsing.
#[derive(Debug)]
pub struct ErrorTrace<'f, 's> {
    /// Reference to the name of the source.
    file   : &'f str,
    /// Reference to the source itself.
    source : &'s str,

    /// The errors and notes in this trace.
    trace : Vec<TraceElem<'s>>,
}
impl<'f, 's> ErrorTrace<'f, 's> {
    /// Constructor that constructs the trace from the given DSL error.
    /// 
    /// This will extract the given DSL error
    /// 
    /// # Arguments
    /// - `file`: The name or other description of the input source.
    /// - `source`: A reference to the physical source we (attempted to) parsed.
    /// - `err`: The DslError to wrap.
    /// 
    /// # Returns
    /// A new `ErrorTrace` instance.
    pub fn from_error(file: &'f str, source: &'s str, err: impl 's + PrettyError) -> Self {
        // Wrap the error in an element
        let mut trace: Vec<TraceElem> = vec![ TraceElem::Error(Box::new(err)) ];

        // Adds the error's notes as well
        trace.extend(trace[0].error().notes().into_iter().map(|n| TraceElem::Note(n)));

        // That's enough to return ourselves
        Self {
            file,
            source,

            trace,
        }
    }



    /// Constructor for the trace that constructs it from the given nom error.
    /// 
    /// It assumes the given error is a scanner-level error.
    /// 
    /// # Arguments
    /// - `file`: The name or other description of the input source.
    /// - `source`: A reference to the physical source we (attempted to) parsed.
    /// - `err`: The `nom::error::VerboseError` that contains the trace we want to copy.
    /// 
    /// # Returns
    /// A new `ErrorTrace` instance.
    pub fn from_nom_scan(file: &'f str, source: &'s str, err: VerboseError<ScanInput<'s>>) -> Self {
        // Iterate over the trace in the verbose error
        let mut trace: Vec<TraceElem> = Vec::with_capacity(err.errors.len());
        for (matched, kind) in err.errors {
            // Match on the found kind
            match kind {
                // Something about expecting a character but seeing something else
                VerboseErrorKind::Char(c) => {
                    if matched.is_empty() {
                        // We encountered EOF

                        // Push the error with a range pointing at the end _if_ there is any input source
                        trace.push(TraceElem::Error(Box::new(DslError::ScanUnexpectedChar{
                            expected : c,
                            got      : None,
                            range    : last_char_range(source),
                        })));
                    } else {
                        // We encountered some character

                        // Get the character and a range pointing to it
                        let got   : &str      = matched.fragment().graphemes(true).next().unwrap();
                        let range : TextRange = TextRange::new(TextPos::start_of(&matched), TextPos::start_of(&matched));

                        // Wrap that in an error and Bobert's your father's brother
                        trace.push(TraceElem::Error(Box::new(DslError::ScanUnexpectedChar {
                            expected : c,
                            got      : Some(got),
                            range    : Some(range),
                        })));
                    }
                },

                // Some nom combinator failed in such a way we need to report to the user
                VerboseErrorKind::Nom(kind) => {
                    // Match to find a kind we know what to do with
                    let err: DslError = match kind {
                        ErrorKind::Tag => {
                            // We simply match the matched match based on if we have anything or not (EOF, in that case).
                            DslError::ScanUnexpectedTag {
                                got   : if !matched.is_empty() { Some(*matched.fragment()) } else { None },
                                range : TextRange::from(&matched),
                            }
                        },

                        // Otherwise, mark as unknown error
                        kind => DslError::ScanUnknownError{ kind, range: TextRange::from(&matched) },
                    };

                    // Don't forget to add it to the list
                    trace.push(TraceElem::Error(Box::new(err)));
                },

                // It's some context, we which can interpret as a note
                VerboseErrorKind::Context(context) => {
                    // We add it as a context note
                    trace.push(TraceElem::Note(Box::new(NomNote::ScanContext {
                        context,
                        range : TextRange::from(&matched),
                    })));
                },
            }
        }

        // Done, we can use that to return a full trace
        Self {
            file,
            source,

            trace,
        }
    }

    /// Constructor for the trace that constructs it from the given nom error wrapped in a `nom::Err`.
    /// 
    /// It assumes the given error is a scanner-level error.
    /// 
    /// # Arguments
    /// - `file`: The name or other description of the input source.
    /// - `source`: A reference to the physical source we (attempted to) parsed.
    /// - `err`: The `nom::Err<nom::error::VerboseError>` that contains the trace we want to copy.
    /// 
    /// # Returns
    /// A new `ErrorTrace` instance.
    pub fn from_nom_err_scan(file: &'f str, source: &'s str, err: nom::Err<VerboseError<ScanInput<'s>>>) -> Self {
        // Match the outer err, then go to `Self::from_nom_scan()` to do the actual work
        match err {
            nom::Err::Error(err)    |
            nom::Err::Failure(err)  => Self::from_nom_scan(file, source, err),
            nom::Err::Incomplete(_) => Self::from_error(file, source, DslError::ScanIncompleteError{ range: last_char_range(source) }),
        }
    }



    /// Constructor for the trace that constructs it from the given nom error.
    /// 
    /// It assumes the given error is a parser-level error.
    /// 
    /// # Arguments
    /// - `file`: The name or other description of the input source.
    /// - `source`: A reference to the physical source we (attempted to) parsed.
    /// - `err`: The `nom::error::VerboseError` that contains the trace we want to copy.
    /// 
    /// # Returns
    /// A new `ErrorTrace` instance.
    pub fn from_nom_parse<'t>(file: &'f str, source: &'s str, err: VerboseError<ParseInput<'t, 's>>) -> Self {
        // Iterate over the trace in the verbose error
        let mut trace: Vec<TraceElem> = Vec::with_capacity(err.errors.len());
        for (matched, kind) in err.errors {
            // Match on the found kind
            match kind {
                // Something about expecting a character but seeing something else
                VerboseErrorKind::Char(_) => {
                    // This should NEVER happen, since we're dealing with tokens
                    unreachable!();
                },

                // Some nom combinator failed in such a way we need to report to the user
                VerboseErrorKind::Nom(kind) => {
                    // Derive a range covering the entire matched range
                    let range: Option<TextRange> = match (matched.first(), matched.last()) {
                        (Some(start), Some(end)) => Some(TextRange::new(start.start_of(), end.end_of())),
                        _                        => None,
                    };

                    // Match to find a kind we know what to do with
                    let err: DslError = match kind {
                        ErrorKind::Tag => {
                            // We simply match the matched match based on if we have anything or not (EOF, in that case).
                            let token: Option<&Token> = matched.first();
                            DslError::ParseUnexpectedTag {
                                got   : token.map(|t| *t),
                                range : token.map(|t| t.range()),
                            }
                        },

                        // Otherwise, mark as unknown error
                        kind => DslError::ParseUnknownError{ kind, range },
                    };

                    // Don't forget to add it to the list
                    trace.push(TraceElem::Error(Box::new(err)));
                },

                // It's some context, we which can interpret as a note
                VerboseErrorKind::Context(context) => {
                    // Derive a range covering the entire matched range
                    let range: Option<TextRange> = match (matched.first(), matched.last()) {
                        (Some(start), Some(end)) => Some(TextRange::new(start.start_of(), end.end_of())),
                        _                        => None,
                    };

                    // We add it as a context note
                    trace.push(TraceElem::Note(Box::new(NomNote::ParseContext {
                        context,
                        range,
                    })));
                },
            }
        }

        // Done, we can use that to return a full trace
        Self {
            file,
            source,

            trace,
        }
    }

    /// Constructor for the trace that constructs it from the given nom error wrapped in a `nom::Err`.
    /// 
    /// It assumes the given error is a parser-level error.
    /// 
    /// # Arguments
    /// - `file`: The name or other description of the input source.
    /// - `source`: A reference to the physical source we (attempted to) parsed.
    /// - `err`: The `nom::Err<nom::error::VerboseError>` that contains the trace we want to copy.
    /// 
    /// # Returns
    /// A new `ErrorTrace` instance.
    pub fn from_nom_err_parse<'t>(file: &'f str, source: &'s str, err: nom::Err<VerboseError<ParseInput<'t, 's>>>) -> Self {
        // Match the outer err, then go to `Self::from_nom_parse()` to do the actual work
        match err {
            nom::Err::Error(err)    |
            nom::Err::Failure(err)  => Self::from_nom_parse(file, source, err),
            nom::Err::Incomplete(_) => Self::from_error(file, source, DslError::ParseIncompleteError{ range: last_char_range(source) }),
        }
    }



    /// Returns a formatter that can pretty-print all of the errors and notes in the ErrorTrace.
    /// 
    /// # Returns
    /// A new ErrorTraceFormatter that implements Display.
    #[inline]
    pub fn display<'t>(&'t self) -> ErrorTraceFormatter<'t, 'f, 's> { ErrorTraceFormatter{ trace: &self.trace, file: self.file, source: self.source } }
}



/// Defines the most toplevel errors for this crate.
#[derive(Debug)]
pub enum DslError<'s> {
    /// Expected a certain character which we did not find.
    /// 
    /// We say 'char', by the way, but since we iterate over graphemes we use a '&str' at `got` too.
    ScanUnexpectedChar{ expected: char, got: Option<&'s str>, range: Option<TextRange> },
    /// Expected a certain keyword or string which we did not find.
    ScanUnexpectedTag{ got: Option<&'s str>, range: TextRange },
    /// Some unhandled scanner error happened.
    ScanUnknownError{ kind: ErrorKind, range: TextRange },
    /// Not all input was able to be scanned.
    ScanLeftoverError{ remainder: ScanInput<'s> },
    /// We needed more characters.
    ScanIncompleteError{ range: Option<TextRange> },

    /// Expected a certain (string of) tokens which we did not find.
    ParseUnexpectedTag{ got: Option<Token<'s>>, range: Option<TextRange> },
    /// Some unhandled parser error happened.
    ParseUnknownError{ kind: ErrorKind, range: Option<TextRange> },
    /// Not all input was able to be parsed.
    ParseLeftoverError{ remainder: Vec<Token<'s>> },
    /// We needed more tokens.
    ParseIncompleteError{ range: Option<TextRange> },
}
impl<'s> Display for DslError<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DslError::*;
        match self {
            ScanUnexpectedChar{ expected, got, .. } => write!(f, "Syntax error: Expected character '{}', got {}", expected, if let Some(got) = got { format!("'{}'", got) } else { "EOF".into() }),
            ScanUnexpectedTag{ got, .. }            => write!(f, "Syntax error: Unexpected token '{}'", got.unwrap_or("EOF")),
            ScanUnknownError{ kind, .. }            => write!(f, "Syntax error: Scanner returned unknown error '{:?}'", kind),
            ScanLeftoverError{ .. }                 => write!(f, "Syntax error: Failed to scan input"),
            ScanIncompleteError{ .. }               => write!(f, "Syntax error: Unexpected end-of-file (did you close all brackets, added all semicolons?)"),

            ParseUnexpectedTag{ got, .. } => write!(f, "Syntax error: Unexpected {}", if let Some(got) = got { got.variant().to_string() } else { "EOF".into() }),
            ParseUnknownError{ kind, .. } => write!(f, "Syntax error: Parser returned unknown error '{:?}'", kind),
            ParseLeftoverError{ .. }      => write!(f, "Syntax error: Failed to parse input"),
            ParseIncompleteError{ .. }    => write!(f, "Syntax error: Unexpected end-of-file (did you close all brackets, added all semicolons?)"),
        }
    }
}
impl<'s> Error for DslError<'s> {}
impl<'s> PrettyError for DslError<'s> {
    fn range(&self) -> Option<TextRange> {
        use DslError::*;
        match self {
            ScanUnexpectedChar{ range, .. }  => *range,
            ScanUnexpectedTag{ range, .. }   |
            ScanUnknownError{ range, .. }    => Some(*range),
            ScanLeftoverError{ remainder }   => Some(TextRange::from(remainder)),
            ScanIncompleteError{ range, .. } => *range,

            ParseUnexpectedTag{ range, .. } |
            ParseUnknownError{ range, .. }  => *range,
            ParseLeftoverError{ remainder } => match (remainder.first(), remainder.last()) {
                (Some(start), Some(end)) => Some(TextRange::new(start.start_of(), end.end_of())),
                _                        => None,
            },
            ParseIncompleteError{ range, .. } => *range,
        }
    }

    #[inline]
    fn notes(&self) -> Vec<Box<dyn PrettyNote>> { vec![] }
}
