//  UTILS.rs
//    by Lut99
// 
//  Created:
//    22 May 2023, 18:35:21
//  Last edited:
//    22 May 2023, 19:20:16
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines a few compile-phase-local utilities.
// 

use std::fmt::{Display, Formatter, Result as FResult};


/***** HELPER MACROS *****/
macro_rules! match_log {
    // Let's hope optimization will get rid of this match
    ($log:ident, $($t:tt)*) => { match $log {
        'd' => ::log::debug!($($t)*),
        't' => ::log::trace!($($t)*),
        _   => { unreachable!(); },
    } };
}





/***** AUXILLARY MACROS *****/
/// Shorthand for creating a new [`TraceTrap`].
macro_rules! trace_trap {
    (target: $target:expr $(, $($t:tt)+)?) => {
        crate::compiler::utils::TraceTrap::with_target($target, format!($($($t)+)?))
    };

    ($($t:tt)*) => {
        crate::compiler::utils::TraceTrap<&'static str, _>::new(format!($($($t)+)?))
    };
}
pub(super) use trace_trap;

/// Shorthand for creating a new [`DebugTrap`].
macro_rules! debug_trap {
    (target: $target:expr $(, $($t:tt)+)?) => {
        crate::compiler::utils::DebugTrap::with_target($target, format!($($($t)+)?))
    };

    ($($t:tt)*) => {
        crate::compiler::utils::DebugTrap<&'static str, _>::new(format!($($($t)+)?))
    };
}
pub(super) use debug_trap;





/***** LIBRARY *****/
/// Defines a struct that print a logging statement as soon as it goes out-of-scope.
#[derive(Debug)]
pub(super) struct LogTrap<const LOG: char, T: AsRef<str>, D: Display> {
    /// Any optional targets to specify.
    target  : Option<T>,
    /// The actual thing to print with [`trace!`].
    message : D,
}

impl<const LOG: char, T: AsRef<str>, D: Display> LogTrap<LOG, T, D> {
    /// Constructor for the LogTrap.
    /// 
    /// This constructor does not specify a target. To do so, see [`Self::with_target()`].
    /// 
    /// # Arguments
    /// - `message`: The [`Display`]-like message to trap.
    /// 
    /// # Returns
    /// A new LogTrap instance that will [`trace!`] the given `message` as soon as it goes out-of-scope.
    #[inline]
    pub fn new(message: D) -> Self {
        Self {
            target : None,
            message,
        }
    }

    /// Constructor for the LogTrap that also gives it a target.
    /// 
    /// To not pass a target, see [`Self::new()`].
    /// 
    /// # Arguments
    /// - `target`: The target to add to the message.
    /// - `message`: The [`Display`]-like message to trap.
    /// 
    /// # Returns
    /// A new LogTrap instance that will [`trace!`] the given `message` as soon as it goes out-of-scope.
    #[inline]
    pub fn with_target(target: T, message: D) -> Self {
        Self {
            target : Some(target),
            message,
        }
    }



    /// Shortcuts the tracetrap by dropping it immediately.
    #[inline]
    pub fn print(self) {}
}
impl<const LOG: char, T: AsRef<str>, D: Display> Drop for LogTrap<LOG, T, D> {
    #[inline]
    fn drop(&mut self) {
        // Do the whole point
        if let Some(target) = self.target.take() {
            match_log!(LOG, target: target.as_ref(), "{self}");
        } else {
            match_log!(LOG, "{self}");
        }
    }
}

impl<const LOG: char, T: AsRef<str>, D: Display> Display for LogTrap<LOG, T, D> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "{}{}", if let Some(target) = &self.target { target.as_ref() } else { "" }, self.message)
    }
}

/// Defines a struct that print a [`log::trace!`] statement as soon as it goes out-of-scope.
pub(super) type TraceTrap<T, D> = LogTrap<'t', T, D>;
/// Defines a struct that print a [`log::debug!`] statement as soon as it goes out-of-scope.
pub(super) type DebugTrap<T, D> = LogTrap<'d', T, D>;
