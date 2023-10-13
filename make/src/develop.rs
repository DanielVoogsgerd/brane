//  DEVELOP.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 12:06:40
//  Last edited:
//    13 Oct 2023, 15:49:55
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the toplevel functions of building stuff in `develop`-mode.
// 

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};

use console::style;
use log::{debug, info};

use crate::args::{DevelopArguments, TargetArguments, TargetSubcommand, TestArguments, TestOption};
use crate::commands;
use crate::dependencies;


/***** ERRORS *****/
/// Defines errors that occur in development mode.
#[derive(Debug)]
pub enum Error {
    /// Failed to check Cargo availability
    CargoAvailability { err: crate::dependencies::Error },
    /// Failed to run Cargo unit tests.
    TestUnit { err: crate::commands::cargo::Error },
    /// Failed to run Cargo clippy tests.
    TestClippy { err: crate::commands::cargo::Error },
}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use Error::*;
        match self {
            CargoAvailability { .. } => write!(f, "Failed to check Cargo availability"),
            TestUnit { .. }          => write!(f, "Failed to run unit tests"),
            TestClippy { .. }        => write!(f, "Failed to run clippy tests"),
        }
    }
}
impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        use Error::*;
        match self {
            CargoAvailability { err } => Some(err),
            TestUnit { err }          => Some(err),
            TestClippy { err }        => Some(err),
        }
    }
}





/***** LIBRARY *****/
/// Builds a particular target in development mode.
/// 
/// # Arguments
/// - `args`: The [`DevelopArguments`] the user gave us to tweak this build.
/// 
/// # Errors
/// This function may error if we failed to build the target.
pub fn build_target(args: DevelopArguments) -> Result<(), Error> {
    info!("Building in `development`-mode");

    // Match on the target to execute
    match args.target {
        TargetSubcommand::Test(test) => {
            info!("Running tests");

            // Assert Cargo exists
            if let Err(err) = dependencies::assert_cargo(!args.no_install_cargo) {
                return Err(Error::CargoAvailability { err });
            }

            // Match on the specific tests
            match test.test {
                Some(TestOption::Unit) => {
                    debug!("Running unit tests...");

                    // Call the command
                    if let Err(err) = commands::cargo::test() {
                        return Err(Error::TestUnit { err });
                    }

                    // Neat
                    println!("Successfully built {}", style("unit tests").bold().green());
                },
                Some(TestOption::Clippy) => {
                    debug!("Running clippy tests...");

                    // Call the command
                    if let Err(err) = commands::cargo::clippy() {
                        return Err(Error::TestClippy { err });
                    }

                    // Neat
                    println!("Successfully built {}", style("clippy tests").bold().green());
                },

                None => {
                    // Build the tests recursively
                    build_target(DevelopArguments {
                        no_install       : args.no_install,
                        no_install_cargo : args.no_install_cargo,

                        target : TargetSubcommand::Test(TestArguments {
                            test : Some(TestOption::Unit),

                            common : TargetArguments {},
                        }),
                    })?;
                    build_target(DevelopArguments {
                        no_install       : args.no_install,
                        no_install_cargo : args.no_install_cargo,

                        target : TargetSubcommand::Test(TestArguments {
                            test : Some(TestOption::Clippy),

                            common : TargetArguments {},
                        }),
                    })?;
                },
            }
        },

        _ => { todo!(); },
    }

    // Done!
    Ok(())
}
