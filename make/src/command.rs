//  COMMAND.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 16:32:45
//  Last edited:
//    12 Oct 2023, 16:56:39
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines abstractions over [`std::process::Command`] to make it
//!   easier to work with.
// 

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::process::Command;


/***** ERRORS *****/
/// Defines errors relating to running commands.
#[derive(Debug)]
pub enum Error {
    /// Raised when we failed to spawn a command.
    Spawn { cmd: Command, err: std::io::Error },
}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use Error::*;
        match self {
            Spawn { cmd, .. } => write!(f, "Failed to spawn command '{cmd:?}'"),
        }
    }
}
impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        use Error::*;
        match self {
            Spawn { err, .. } => Some(err),
        }
    }
}





/***** LIBRARY *****/
/// Defines cargo-related commands.
pub mod cargo {
    use std::borrow::Cow;
    use std::error;
    use std::fmt::{Display, Formatter, Result as FResult};
    use std::process::{Command, Output};
    use std::str::FromStr as _;

    use log::{debug, info};
    use semver::Version;


    /***** ERRORS *****/
    /// Defines errors relating to running commands.
    #[derive(Debug)]
    pub enum Error {
        /// Raised when there is an issue with spawning the command itself.
        Command { err: super::Error },
        /// The version returned by Cargo is invalid.
        InvalidVersion { err: VersionError },
    }
    impl Display for Error {
        fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
            use Error::*;
            match self {
                Command { .. }        => write!(f, "Failed to run `cargo`-command"),
                InvalidVersion { .. } => write!(f, "Invalid version string returned from `cargo --version`"),
            }
        }
    }
    impl error::Error for Error {
        fn source(&self) -> Option<&(dyn error::Error + 'static)> {
            use Error::*;
            match self {
                Command { err }        => Some(err),
                InvalidVersion { err } => Some(err),
            }
        }
    }

    /// Details why we failed to parse a Cargo version number.
    #[derive(Debug)]
    pub enum VersionError {
        /// The string returned by cargo did not have at least two parts.
        NotEnoughParts { got: usize },
        /// Failed to parse the version as a semantic version.
        InvalidVersion { raw: String, err: semver::Error },
    }
    impl Display for VersionError {
        fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
            use VersionError::*;
            match self {
                NotEnoughParts { got }     => write!(f, "String returned by Cargo is not consisting of at least two space-separated parts (got {got})"),
                InvalidVersion { raw, .. } => write!(f, "Failed to parse '{raw}' as a semantic version number"),
            }
        }
    }
    impl error::Error for VersionError {
        fn source(&self) -> Option<&(dyn error::Error + 'static)> {
            use VersionError::*;
            match self {
                NotEnoughParts { .. }      => None,
                InvalidVersion { err, .. } => Some(err),
            }
        }
    }





    /***** LIBRARY FUNCTIONS *****/
    /// Runs `cargo --version`.
    /// 
    /// # Returns
    /// The version as reported by Cargo.
    /// 
    /// # Errors
    /// This function errors if we failed to spawn the command or the command returned a non-zero exit code.
    pub fn version() -> Result<Version, Error> {
        info!("Calling `cargo --version`");

        // Build the command to execute
        // let mut cmd: Command = Command::new("cargo");
        let mut cmd: Command = Command::new("carga");
        cmd.arg("--version");

        // Attempt to execute it
        debug!("Running '{cmd:?}'...");
        let output: Output = match cmd.output() {
            Ok(output) => output,
            Err(err)   => { return Err(Error::Command { err: super::Error::Spawn { cmd, err } }); },
        };
        let stdout: Cow<str> = String::from_utf8_lossy(&output.stdout);
        let stdout: &str = stdout.trim();

        // Parse the stdout as a quintuplet of things
        debug!("Parsing '{stdout}' as version number...");
        let parts: Vec<&str> = stdout.split(' ').collect();
        if parts.len() < 2 {
            return Err(Error::InvalidVersion { err: VersionError::NotEnoughParts { got: parts.len() } });
        }
        // Attempt to parse the second one as a version number
        let part: &str = parts[1].trim();
        let version: Version = match Version::from_str(part) {
            Ok(version) => version,
            Err(err)    => { return Err(Error::InvalidVersion { err: VersionError::InvalidVersion { raw: part.into(), err } }); },
        };

        // OK, that's it!
        debug!("Cargo version: v{version}");
        Ok(version)
    }
}
