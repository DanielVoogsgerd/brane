//  PRINT AST.rs
//    by Lut99
// 
//  Created:
//    10 Feb 2023, 19:25:20
//  Last edited:
//    11 Feb 2023, 17:44:58
//  Auto updated?
//    Yes
// 
//  Description:
//!   Compiler traversal for printing the current AST.
// 

use std::fmt::{Display, Formatter, Result as FResult};
use std::io::Write;

use crate::ast::spec::Node;
use crate::ast::auxillary::{Annotation, AnnotationKind};
use crate::ast::expressions::{Block, Expression, ExpressionKind, Literal, LiteralKind, UnaryOperatorKind};
use crate::ast::statements::{ArgDef, ClassMemberDef, FunctionDef, PropertyDef, Statement, StatementKind};
use crate::ast::toplevel::Program;


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use brane_shr::utilities::test_on_dsl_files;
    use crate::errors::{DslError, ErrorTrace, PrettyError as _};
    use crate::scanner::{scan_tokens, Input as ScanInput};
    use crate::parser::parse_tokens;
    use super::{traverse, Program};


    /// Tests the parser by print all files
    #[test]
    fn test_print() {
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

            // Show the tokens using the formatter
            traverse(&mut std::io::stdout(), &ast).unwrap();
            println!("{}\n\n", (0..80).map(|_| '-').collect::<String>());
        });
    }
}





/***** CONSTANTS *****/
/// The indentation increase size.
const INDENT_SIZE: usize = 4;





/***** HELPER MACROS *****/
/// Overrides write macros with an optional indentation thing.
macro_rules! write {
    ($f:expr, indent = $indent:expr) => {
        ::core::write!($f, "{}", (0..$indent).map(|_| ' ').collect::<String>())
    };
    ($f:expr, indent = $indent:expr, $fmt:literal) => {
        ::core::write!($f, ::core::concat!("{}", $fmt), (0..$indent).map(|_| ' ').collect::<String>())
    };
    ($f:expr, indent = $indent:expr, $fmt:literal, $($t:tt)*) => {
        ::core::write!($f, ::core::concat!("{}", $fmt), (0..$indent).map(|_| ' ').collect::<String>(), $($t)*)
    };

    ($f:expr) => {
        ::core::write!($f)
    };
    ($f:expr, $fmt:literal) => {
        ::core::write!($f, $fmt)
    };
    ($f:expr, $fmt:literal, $($t:tt)*) => {
        ::core::write!($f, $fmt, $($t)*)
    };
}

/// Overrides writeln macros with an optional indentation thing.
macro_rules! writeln {
    ($f:expr, indent = $indent:expr) => {
        ::core::writeln!($f, "{}", (0..$indent).map(|_| ' ').collect::<String>())
    };
    ($f:expr, indent = $indent:expr, $fmt:literal) => {
        ::core::writeln!($f, ::core::concat!("{}", $fmt), (0..$indent).map(|_| ' ').collect::<String>())
    };
    ($f:expr, indent = $indent:expr, $fmt:literal, $($t:tt)*) => {
        ::core::writeln!($f, ::core::concat!("{}", $fmt), (0..$indent).map(|_| ' ').collect::<String>(), $($t)*)
    };

    ($f:expr) => {
        ::core::writeln!($f)
    };
    ($f:expr, $fmt:literal) => {
        ::core::writeln!($f, $fmt)
    };
    ($f:expr, $fmt:literal, $($t:tt)*) => {
        ::core::writeln!($f, $fmt, $($t)*)
    };
}





/***** HELPER TRAITS *****/
/// Provides the convience 'trav()` function on each element in the tree
trait TravFormattable {
    /// Returns a TravFormatter for this struct.
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self>;

    /// Returns a TravFormatter for this struct with the specified indentation.
    /// 
    /// # Arguments
    /// - `indent`: The indentation to print this element with. Mostly relevant for statements or multi-line expressions.
    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self>;
}
impl<T: Node> TravFormattable for T {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}





/***** TRAVERSAL STRUCTS *****/
/// Implements Display for one of the AST types.
#[derive(Debug)]
struct TravFormatter<'e, E: ?Sized> {
    /// The element to format
    elem   : &'e E,
    /// The indentation to format it with, if any.
    indent : usize,
}



/// Formatter for Programs
impl<'e> Display for TravFormatter<'e, Program> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Simply print all statements with zero indentation
        for s in &self.elem.stmts {
            write!(f, "{}", s.trav())?;
        }

        // Done
        Ok(())
    }
}



/// Formatter for Statements
impl<'e> Display for TravFormatter<'e, Statement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Print annotations, if any
        for a in &self.elem.annots {
            writeln!(f, indent = self.indent, "#[{}]", a.trav_indent(self.indent))?;
        }

        // Then match on the specific kind of statement
        use StatementKind::*;
        match &self.elem.kind {
            // Definitions
            Import{ name, version } => {
                // Print the statement
                writeln!(f, indent = self.indent, "import {}{};", name.name, if let Some(version) = version { format!("[{}.{}.{}]", version.0.trav(), version.1.trav(), version.2.trav()) } else { String::new() })
            },

            FunctionDef(def) => write!(f, "{}", def.trav_indent(self.indent)),

            ClassDef { name, defs } => {
                // Write the class header
                writeln!(f, indent = self.indent, "class {} {{", name.name)?;
                // Write all the definitions
                for def in defs {
                    // Match on the kind
                    match def {
                        ClassMemberDef::Property(def) => { write!(f, "{}", def.trav_indent(self.indent + INDENT_SIZE))?; },
                        ClassMemberDef::Method(def)   => { write!(f, "{}", def.trav_indent(self.indent + INDENT_SIZE))?; },
                    }
                }
                // Write the footer
                writeln!(f, indent = self.indent, "}}")?;

                // Done
                Ok(())
            },
        
            VarDef { name, data_type, value } => {
                // Write the let statement thingy in one line
                writeln!(f, indent = self.indent, "let {}{}{};",
                    name.name,
                    if !data_type.data_type.is_any() { format!(": {}", data_type.data_type) } else { String::new() },
                    if let Some(value) = value { format!(" := {}", value.trav_indent(self.indent)) } else { String::new() },
                )
            },



            // Control flow
            For { name, start, stop, step, block } => {
                // Write the for header
                write!(f, indent = self.indent, "for ({} from {} to {}{}) ",
                    name.name,
                    start.trav_indent(self.indent),
                    stop.trav_indent(self.indent),
                    if let Some(step) = step { format!(" step {}", step.trav_indent(self.indent)) } else { String::new() },
                )?;

                // Write the block, done
                writeln!(f, "{}", block.trav_indent(self.indent))?;

                // Done
                Ok(())
            },
        
            While { cond, block } => {
                // Write the while header
                write!(f, indent = self.indent, "while ({}) ", cond.trav_indent(self.indent))?;

                // Write the block, done
                writeln!(f, "{}", block.trav_indent(self.indent))?;

                // Done
                Ok(())
            },
        
            Return { value } => {
                // It's all one line
                writeln!(f, indent = self.indent, "return{};", if let Some(value) = value { format!(" {}", value.trav_indent(self.indent)) } else { String::new() })
            },



            // Miscellaneous
            Expression(expr) => {
                writeln!(f, indent = self.indent, "{}", expr.trav_indent(self.indent))
            },
        }
    }
}

/// The formatter for indentations.
impl<'e> Display for TravFormatter<'e, Annotation> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Match on the kind of annotaiton
        use AnnotationKind::*;
        match &self.elem.kind {
            Identifier(id) => {
                write!(f, "{}", id.name)
            },

            KeyValue(id, value) => {
                write!(f, "{} = {}", id.name, value.trav_indent(self.indent))
            },

            KeyList(id, annots) => {
                write!(f, "{}({})", id.name, annots.iter().map(|a| a.trav_indent(self.indent).to_string()).collect::<Vec<String>>().join(", "))
            },
        }
    }
}

/// The formatter for property definitions.
impl<'e> Display for TravFormatter<'e, PropertyDef> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        writeln!(f, indent = self.indent, "{}: {};", self.elem.name.name, self.elem.data_type.data_type)
    }
}

/// The formatter for argument definitions.
impl<'e> Display for TravFormatter<'e, ArgDef> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "{}{}", self.elem.name.name, if !self.elem.data_type.data_type.is_any() { format!(": {}", self.elem.data_type.data_type) } else { String::new() })
    }
}

/// The formatter for function definitions.
impl<'e> Display for TravFormatter<'e, FunctionDef> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Write the function header
        write!(f, indent = self.indent, "func {}({}){} ",
            self.elem.name.name,
            self.elem.args.iter().map(|a| format!("{}", a.trav())).collect::<Vec<String>>().join(", "),
            if !self.elem.ret.data_type.is_void() { format!(" -> {}", self.elem.ret.data_type) } else { String::new() },
        )?;

        // Write all of the statements with additional indentation (which also does the curly brackets)
        writeln!(f, "{}", self.elem.body.trav_indent(self.indent))?;

        // Done
        Ok(())
    }
}



// The formatter for Expressions.
impl<'e> Display for TravFormatter<'e, Expression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Match on the specific kind of statement
        use ExpressionKind::*;
        match &self.elem.kind {
            // Statement-carrying expressions
            Block(block) => {
                // We can just print as block
                write!(f, "{}", block.trav_indent(self.indent))
            },

            If { cond, block, block_else } => {
                write!(f, "if ({}) {}{}",
                    cond.trav_indent(self.indent),
                    block.trav_indent(self.indent),
                    if let Some(block_else) = block_else { format!(" else {}", block_else.trav_indent(self.indent)) } else { String::new() }
                )
            },
            Parallel { branches, strategy } => {
                write!(f, "parallel{} [{}]",
                    if let Some(strat) = strategy { format!(" [{}]", strat) } else { String::new() },
                    branches.iter().map(|b| format!("{}", b.trav_indent(self.indent))).collect::<Vec<String>>().join(", "),
                )
            },



            // Operators
            Cast { expr, data_type } => {
                write!(f, "({} as {})", expr.trav_indent(self.indent), data_type.data_type)
            },
            Discard { expr } => {
                write!(f, "{};", expr.trav_indent(self.indent))
            },

            Index { to_index, index } => {
                write!(f, "({}[{}])", to_index.trav_indent(self.indent), index.trav_indent(self.indent))
            },
            Proj { to_proj, field } => {
                write!(f, "({}.{})", to_proj.trav_indent(self.indent), field.name)
            },
            Call { to_call, args } => {
                write!(f, "{}({})",
                    to_call.trav_indent(self.indent),
                    args.iter().map(|a| a.trav_indent(self.indent).to_string()).collect::<Vec<String>>().join(", ")
                )
            },



            // Arithmetic operators
            Unary { expr, op } => {
                // Sort between prefix and postfix
                match op.kind {
                    // Prefix
                    UnaryOperatorKind::Not |
                    UnaryOperatorKind::Neg => {
                        write!(f, "({}{})", op, expr.trav_indent(self.indent))
                    }

                    // Postfix
                }
            },

            Binary { lhs, rhs, op } => {
                write!(f, "({} {} {})", lhs.trav_indent(self.indent), op, rhs.trav_indent(self.indent))
            },



            // Values
            Array { elems } => {
                write!(f, "[{}]", elems.iter().map(|e| e.trav_indent(self.indent).to_string()).collect::<Vec<String>>().join(", "))
            },
            Instance { name, props } => {
                // Write the header
                writeln!(f, "new {} {{", name.name)?;
                // Write the properties
                for p in props {
                    writeln!(f, indent = self.indent + INDENT_SIZE, "{} := {},", p.name.name, p.value.trav_indent(self.indent))?;
                }
                // Write the footer
                write!(f, "}}")?;

                // Done
                Ok(())
            },

            VarRef { name } => {
                write!(f, "{}", name.name)
            },
            LocalFunctionRef { name } => {
                write!(f, "{}", name.name)
            },
            ExternalFunctionRef { name, package } => {
                write!(f, "{}::{}", package.name, name.name)
            },

            Literal(literal) => {
                write!(f, "{}", literal.trav_indent(self.indent))
            },
        }
    }
}

// The formatter for Blocks.
impl<'e> Display for TravFormatter<'e, Block> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Special display for if no statements
        if self.elem.stmts.is_empty() { return write!(f, "{{}}"); }

        // Don't write an indentation at the start cuz this is an expression
        writeln!(f, "{{")?;
        for s in &self.elem.stmts {
            write!(f, "{}", s.trav_indent(self.indent + INDENT_SIZE))?;
        }
        write!(f, indent = self.indent, "}}")?;

        // Done
        Ok(())
    }
}

// The formatter for Literals.
impl<'e> Display for TravFormatter<'e, Literal> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Match on the specific kind of literal
        use LiteralKind::*;
        match &self.elem.kind {
            Boolean{ value } => write!(f, "{}", value),
            Integer{ value } => write!(f, "{}", value),
            Real{ value }    => write!(f, "{}", value),
            String{ value }  => write!(f, "{}", value),
            Null             => write!(f, "null"),
        }
    }
}





/***** LIBRARY *****/
/// Runs a full traversal on the given AST to print it to the given formatter.
/// 
/// This traversal does not mutate or 
/// 
/// # Arguments
/// - `out`: The `Write`r on which to print the given tree.
/// - `tree`: The AST to print.
/// 
/// # Errors
/// This function errors if we failed to write to the given writer.
#[allow(dead_code)]
pub fn traverse(out: &mut impl Write, tree: &Program) -> Result<(), std::io::Error> {
    // Simply call the toplevel formatter
    write!(out, "{}", tree.trav())?;

    // Done
    Ok(())
}
