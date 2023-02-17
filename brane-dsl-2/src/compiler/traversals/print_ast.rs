//  PRINT AST.rs
//    by Lut99
// 
//  Created:
//    10 Feb 2023, 19:25:20
//  Last edited:
//    17 Feb 2023, 15:48:07
//  Auto updated?
//    Yes
// 
//  Description:
//!   Compiler traversal for printing the current AST.
// 

use std::cell::Ref;
use std::fmt::{Display, Formatter, Result as FResult};
use std::io::Write;

use crate::ast::spec::{Annotation, Node};
use crate::ast::types::DataType;
use crate::ast::symbol_tables::{DelayedEntry, ExternalClassEntry, ExternalClassEntryMember, ExternalFuncEntry, LocalClassEntry, LocalClassEntryMember, LocalFuncEntry, PackageEntry, SymbolTable, VarEntry};
use crate::ast::expressions::{Block, Expression, ExpressionKind, Literal, LiteralKind, UnaryOperatorKind};
use crate::ast::statements::{ArgDef, ClassMemberDefKind, FunctionDef, RawAnnotation, RawAnnotationKind, Statement, StatementKind};
use crate::ast::toplevel::Program;


/***** TESTS *****/
#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use brane_shr::utilities::test_on_dsl_files;
    use crate::errors::{DslError, ErrorTrace, PrettyError as _};
    use crate::scanner::{scan_tokens, Input as ScanInput};
    use crate::parser::parse_tokens;
    use super::{traverse, traverse_st, Program};


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

    /// Tests the parser by printing all symbol tables
    #[test]
    fn test_print_st() {
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
            traverse_st(&mut std::io::stdout(), &ast).unwrap();
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
impl TravFormattable for Annotation {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl TravFormattable for SymbolTable {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl TravFormattable for DelayedEntry<PackageEntry> {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl TravFormattable for ExternalFuncEntry {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl TravFormattable for DelayedEntry<LocalFuncEntry> {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl TravFormattable for LocalFuncEntry {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl TravFormattable for ExternalClassEntry {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl TravFormattable for DelayedEntry<LocalClassEntry> {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl TravFormattable for DelayedEntry<VarEntry> {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl TravFormattable for VarEntry {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl TravFormattable for (&Block, &Vec<Annotation>) {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}
impl<T: Node> TravFormattable for T {
    #[inline]
    fn trav<'e>(&'e self) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent: 0 } }

    fn trav_indent<'e>(&'e self, indent: usize) -> TravFormatter<'e, Self> { TravFormatter{ elem: self, indent } }
}





/***** SYMBOL TABLE TRAVERSALS *****/
/// Formatter for a symbol table.
impl<'e> Display for TravFormatter<'e, SymbolTable> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Write the opening
        writeln!(f, indent = self.indent, "SymbolTable [")?;

        // Write the package namespace
        for p in self.elem.packages.values() {
            write!(f, "{}", p.borrow().trav_indent(self.indent + INDENT_SIZE))?;
        }
        // Write the function namespace
        if !self.elem.packages.is_empty() && !self.elem.funcs.is_empty() { writeln!(f)?; }
        for fun in self.elem.funcs.values() {
            write!(f, "{}", fun.borrow().trav_indent(self.indent + INDENT_SIZE))?;
        }
        // Write the class namespace
        if (!self.elem.packages.is_empty() || !self.elem.funcs.is_empty()) && !self.elem.classes.is_empty() { writeln!(f)?; }
        for c in self.elem.classes.values() {
            write!(f, "{}", c.borrow().trav_indent(self.indent + INDENT_SIZE))?;
        }
        // Write the variable namespace
        if (!self.elem.packages.is_empty() || !self.elem.funcs.is_empty() || !self.elem.classes.is_empty()) && !self.elem.vars.is_empty() { writeln!(f)?; }
        for v in self.elem.vars.values() {
            write!(f, "{}", v.borrow().trav_indent(self.indent + INDENT_SIZE))?;
        }

        // Now write any child symbol tables
        for (i, child) in self.elem.childs.iter().enumerate() {
            // Write a newline
            if i > 0 || ((!self.elem.packages.is_empty() || !self.elem.funcs.is_empty() || !self.elem.classes.is_empty()) && !self.elem.vars.is_empty()) {
                writeln!(f)?;
            }

            // Write the table
            write!(f, "{}", child.borrow().trav_indent(self.indent + INDENT_SIZE))?;
        }

        // Write the close
        writeln!(f, indent = self.indent, "]")?;

        // Done
        Ok(())
    }
}

/// Formatter for package entries.
impl<'e> Display for TravFormatter<'e, DelayedEntry<PackageEntry>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Write the header which carries the name
        writeln!(f, indent = self.indent, "{}Package '{}' [",
            if self.elem.is_phantom() { "(P) " } else { "" },
            self.elem.name,
        )?;

        // Write the functions
        for fun in self.elem.funcs.values() {
            writeln!(f, indent = self.indent + INDENT_SIZE, "{}{}", if self.elem.funcs.is_phantom() { "(P) " } else { "" }, fun.trav_indent(self.indent + INDENT_SIZE))?;
        }
        // Write the classes
        for c in self.elem.classes.values() {
            writeln!(f, indent = self.indent + INDENT_SIZE, "{}{}", if self.elem.classes.is_phantom() { "(P) " } else { "" }, c.trav_indent(self.indent + INDENT_SIZE))?;
        }

        // Write the close
        writeln!(f, indent = self.indent, "]")?;

        // Done
        Ok(())
    }
}

/// Formatter for external function entries.
impl<'e> Display for TravFormatter<'e, ExternalFuncEntry> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Note that we write without newline and indent, since the parent (PackageEntry) wants to potentially prefix us
        write!(f, "func {}::{}({}){}",
            self.elem.package.borrow().name,
            self.elem.name,
            self.elem.args.iter().map(|a| format!("{}{}", a.name, if a.data_type != DataType::Any { format!(" -> {}", a.data_type) } else { String::new() })).collect::<Vec<String>>().join(","),
            if self.elem.ret_type != DataType::Any { format!(" -> {}", self.elem.ret_type) } else { String::new() },
        )
    }
}

/// Formatter for local function entries.
impl<'e> Display for TravFormatter<'e, DelayedEntry<LocalFuncEntry>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        writeln!(f, indent = self.indent, "{}{}",
            if self.elem.is_phantom() { "(P) " } else { "" },
            self.elem.entry().trav_indent(self.indent),
        )
    }
}
/// Formatter for local function entries that does not do indent and newlines and whatever.
impl<'e> Display for TravFormatter<'e, LocalFuncEntry> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "func {}({}){}",
            self.elem.name,
            self.elem.args.iter().map(|a| {
                let a: Ref<DelayedEntry<VarEntry>> = a.borrow();
                format!("{}{}", a.name, if a.data_type != DataType::Any { format!(" -> {}", a.data_type) } else { String::new() })
            }).collect::<Vec<String>>().join(","),
            if self.elem.ret_type != DataType::Any { format!(" -> {}", self.elem.ret_type) } else { String::new() },
        )
    }
}

/// Formatter for external classes.
impl<'e> Display for TravFormatter<'e, ExternalClassEntry> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        writeln!(f, "class {} [", self.elem.name)?;

        // Write the properties
        for p in self.elem.defs.values() {
            match p {
                ExternalClassEntryMember::Property(prop) => { writeln!(f, indent = self.indent + INDENT_SIZE, "{}", prop.trav_indent(self.indent + INDENT_SIZE))?; },
                ExternalClassEntryMember::Method(method) => { writeln!(f, indent = self.indent + INDENT_SIZE, "{}", method.trav_indent(self.indent + INDENT_SIZE))?; },
            }
        }

        // Write the end
        writeln!(f, indent = self.indent, "]")?;
        Ok(())
    }
}

/// Formatter for classes wrapped in delayed entries.
impl<'e> Display for TravFormatter<'e, DelayedEntry<LocalClassEntry>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        writeln!(f, indent = self.indent, "{}class {} [", if self.elem.is_phantom() { "(P) " } else { "" }, self.elem.name)?;

        // Write the properties
        for p in self.elem.defs.values() {
            match p {
                LocalClassEntryMember::Property(prop) => { writeln!(f, indent = self.indent + INDENT_SIZE, "{}", prop.trav_indent(self.indent + INDENT_SIZE))?; },
                LocalClassEntryMember::Method(method) => { writeln!(f, indent = self.indent + INDENT_SIZE, "{}", method.trav_indent(self.indent + INDENT_SIZE))?; },
            }
        }

        // Write the end
        writeln!(f, indent = self.indent, "]")?;
        Ok(())
    }
}

/// Formatter for variable entries.
impl<'e> Display for TravFormatter<'e, DelayedEntry<VarEntry>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        writeln!(f, indent = self.indent, "{}{}",
            if self.elem.is_phantom() { "(P) " } else { "" },
            self.elem.entry().trav_indent(self.indent),
        )
    }
}
impl<'e> Display for TravFormatter<'e, VarEntry> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "var {}{}",
            self.elem.name,
            if self.elem.data_type != DataType::Any { format!(": {}", self.elem.data_type) } else { String::new() },
        )
    }
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
        // Print annotations, if any
        for a in &self.elem.annots {
            writeln!(f, indent = self.indent, "#![{}]", a.trav_indent(self.indent))?;
        }

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
            // Annotations
            Annotation{ annots } => {
                for a in annots {
                    writeln!(f, indent = self.indent, "#[{}]", a.trav_indent(self.indent))?;
                }
                Ok(())
            },

            ParentAnnotation{ annots } => {
                for a in annots {
                    writeln!(f, indent = self.indent, "#![{}]", a.trav_indent(self.indent))?;
                }
                Ok(())
            },



            // Definitions
            Import{ name, version, .. } => {
                // Print the statement
                writeln!(f, indent = self.indent, "import {}{};", name.name, if let Some(version) = version { format!("[{}.{}.{}]", version.0.trav(), version.1.trav(), version.2.trav()) } else { String::new() })
            },

            FunctionDef(def) => write!(f, "{}", def.trav_indent(self.indent)),

            ClassDef { name, defs, .. } => {
                // Write the class header
                writeln!(f, indent = self.indent, "class {} {{", name.name)?;
                // Write all the definitions
                for def in defs {
                    // Match on the kind
                    match &def.kind {
                        ClassMemberDefKind::Annotation{ annots } => {
                            for a in annots {
                                writeln!(f, indent = self.indent, "#[{}]", a.trav_indent(self.indent))?;
                            }
                        },
                        ClassMemberDefKind::ParentAnnotation{ annots } => {
                            for a in annots {
                                writeln!(f, indent = self.indent, "#![{}]", a.trav_indent(self.indent))?;
                            }
                        },

                        ClassMemberDefKind::Property{ name, data_type } => { writeln!(f, indent = self.indent, "{}: {};", name.name, data_type.data_type)?; },
                        ClassMemberDefKind::Method(def)                 => { write!(f, "{}", def.trav_indent(self.indent + INDENT_SIZE))?; },
                    }
                }
                // Write the footer
                writeln!(f, indent = self.indent, "}}")?;

                // Done
                Ok(())
            },
        
            VarDef { name, data_type, value, .. } => {
                // Write the let statement thingy in one line
                writeln!(f, indent = self.indent, "let {}{}{};",
                    name.name,
                    if !data_type.data_type.is_any() { format!(": {}", data_type.data_type) } else { String::new() },
                    if let Some(value) = value { format!(" := {}", value.trav_indent(self.indent)) } else { String::new() },
                )
            },



            // Control flow
            For { name, start, stop, step, block, .. } => {
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

/// The formatter for annotations.
impl<'e> Display for TravFormatter<'e, Annotation> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Match on the kind of annotation
        use Annotation::*;
        match &self.elem {
            Allow(code) => write!(f, "allow({})", code),
            On(loc)     => write!(f, "on = {}", loc.trav_indent(self.indent)),
        }
    }
}

/// The formatter for raw annotations.
impl<'e> Display for TravFormatter<'e, RawAnnotation> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Match on the kind of annotation
        use RawAnnotationKind::*;
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
            Block(block, annots) => {
                // We can just print as block, except we note annotations inside
                write!(f, "{}", (block.as_ref(), annots).trav_indent(self.indent))
            },

            If { cond, block, block_else, annots, annots_else } => {
                write!(f, "if ({}) {}{}",
                    cond.trav_indent(self.indent),
                    (block.as_ref(), annots).trav_indent(self.indent),
                    if let Some(block_else) = block_else { format!(" else {}", (block_else.as_ref(), annots_else).trav_indent(self.indent)) } else { String::new() }
                )
            },
            Parallel { branches, strategy } => {
                write!(f, "parallel{} [{}]",
                    if let Some(strat) = strategy { format!(" [{}]", strat) } else { String::new() },
                    branches.iter().map(|(b, a)| format!("{}", (b, a).trav_indent(self.indent))).collect::<Vec<String>>().join(", "),
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
            LocalInstance { name, props, .. } => {
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
            RemoteInstance{ name, package, props, .. } => {
                // Write the header
                writeln!(f, "new {}::{} {{", package.name, name.name)?;
                // Write the properties
                for p in props {
                    writeln!(f, indent = self.indent + INDENT_SIZE, "{} := {},", p.name.name, p.value.trav_indent(self.indent))?;
                }
                // Write the footer
                write!(f, "}}")?;

                // Done
                Ok(())
            },

            VarRef { name, .. } => {
                write!(f, "{}", name.name)
            },
            LocalFunctionRef { name, .. } => {
                write!(f, "{}", name.name)
            },
            ExternalFunctionRef { name, package, .. } => {
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
// The formatter for blocks with their own annotations
impl<'e> Display for TravFormatter<'e, (&Block, &Vec<Annotation>)> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        // Special display for if no statements
        if self.elem.0.stmts.is_empty() && self.elem.1.is_empty() { return write!(f, "{{}}"); }

        // Don't write an indentation at the start cuz this is an expression
        writeln!(f, "{{")?;
        for a in self.elem.1 {
            writeln!(f, indent = self.indent + INDENT_SIZE, "#![{}]", a.trav_indent(self.indent))?;
        }
        if !self.elem.1.is_empty() && self.elem.0.stmts.is_empty() { writeln!(f)?; }
        for s in &self.elem.0.stmts {
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
/// This traversal does not mutate or change the given AST in any way.
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



/// Runs a full traversal on the given AST to print its _symbol tables_ to the given formatter.
/// 
/// # Arguments
/// - `out`: The `Write`r on which to print the given tree.
/// - `tree`: The AST who's symbol tables to print.
/// 
/// # Errors
/// This function errors if we failed to write to the given writer.
#[allow(dead_code)]
pub fn traverse_st(out: &mut impl Write, tree: &Program) -> Result<(), std::io::Error> {
    // Simply call the formatter on the toplevel symbol table
    write!(out, "{}", tree.table.borrow().trav())?;

    // Done
    Ok(())
}
