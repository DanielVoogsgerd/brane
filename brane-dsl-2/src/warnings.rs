//  WARNINGS.rs
//    by Lut99
// 
//  Created:
//    11 Feb 2023, 18:14:09
//  Last edited:
//    11 Feb 2023, 18:28:07
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines warnings that may occur in the BraneScript/Bakery compiler.
// 

use std::fmt::{Debug, Display, Formatter, Result as FResult};


/***** AUXILLARY *****/
/// Provides a template for writing the WarningCode struct
macro_rules! warning_codes {
    (
        $(
            $(#[$annot:ident $($args:tt)*])*
            $name:ident => $code:literal
        ),*
        $(,)?
    ) => {
        /// Defines shorthand (string) codes for referring to specific warnings. Mostly used to disable them.
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        pub enum WarningCode {
            $(
                $(#[$annot $($args)*])*
                $name,
            )*
        }

        impl WarningCode {
            /// Returns the string representation of this code.
            pub const fn code(&self) -> &'static str {
                match self {
                    $(
                        Self::$name => $code,
                    )*
                }
            }
        }

        impl Display for WarningCode {
            #[inline]
            fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
                write!(f, "{}", self.code())
            }
        }
        impl From<&str> for WarningCode {
            fn from(value: &str) -> Self {
                match value {
                    $(
                        $code => Self::$name,
                    )*
                }
            }
        }
    };
}

warning_codes!{
    /// Occurs when some unknown annotation is used.
    UnknownAnnotation => "unknown_annot",

    /// Occurs when some code can never be reached.
    DeadCode => "dead_code",
}
