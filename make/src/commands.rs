//  COMMANDS.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 16:32:45
//  Last edited:
//    13 Oct 2023, 15:51:05
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines abstractions over [`std::process::Command`] to make it
//!   easier to work with.
// 

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::process::{Command, ExitStatus};


/***** ERRORS *****/
/// Defines errors relating to running commands.
#[derive(Debug)]
pub enum Error {
    /// Raised when we failed to spawn a command.
    Spawn { cmd: Command, err: std::io::Error },
    /// Raised when a command returned a non-zero exit code.
    Fail { cmd: Command, code: ExitStatus, stdout: String, stderr: String },
}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use Error::*;
        match self {
            Spawn { cmd, .. }                  => write!(f, "Failed to spawn command '{cmd:?}'"),
            Fail { cmd, code, stdout, stderr } => write!(f, "Command {:?} failed with exit code {}\n\nstdout:\n{}\n{}\n{}\n\nstderr:\n{}\n{}\n{}\n\n", cmd, code.code().unwrap_or(-1), (0..80).map(|_| '-').collect::<String>(), stdout, (0..80).map(|_| '-').collect::<String>(), (0..80).map(|_| '-').collect::<String>(), stderr, (0..80).map(|_| '-').collect::<String>()),
        }
    }
}
impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        use Error::*;
        match self {
            Spawn { err, .. } => Some(err),
            Fail { .. }       => None,
        }
    }
}





/***** LIBRARY *****/
/// Defines cargo-related commands.
pub mod cargo {
    use std::borrow::Cow;
    use std::error;
    use std::fmt::{Display, Formatter, Result as FResult};
    use std::process::{Command, Output, Stdio};
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
        let mut cmd: Command = Command::new("cargo");
        cmd.arg("--version");

        // Attempt to execute it
        debug!("Running '{cmd:?}'...");
        let output: Output = match cmd.output() {
            Ok(output) => output,
            Err(err)   => { return Err(Error::Command { err: super::Error::Spawn { cmd, err } }); },
        };
        if !output.status.success() {
            return Err(Error::Command { err: super::Error::Fail { cmd, code: output.status, stdout: String::from_utf8_lossy(&output.stdout).into(), stderr: String::from_utf8_lossy(&output.stderr).into() } });
        }

        // Get the stdout
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

    /// Runs `cargo tests` with appropriate flags.
    /// 
    /// I.e., runs all unit tests for everything.
    /// 
    /// # Errors
    /// This function errors if we failed to spawn the command or the command returned a non-zero exit code.
    pub fn test() -> Result<(), Error> {
        info!("Calling `cargo test`");

        // Build the command to execute
        let mut cmd: Command = Command::new("cargo");
        cmd.stdout(Stdio::inherit());
        cmd.stderr(Stdio::inherit());
        cmd.args(["test", "--all-features", "--all-targets"]);

        // Attempt to execute it
        debug!("Running {cmd:?}...");
        let output: Output = match cmd.output() {
            Ok(output) => output,
            Err(err)   => { return Err(Error::Command { err: super::Error::Spawn { cmd, err } }); },
        };
        if !output.status.success() {
            return Err(Error::Command { err: super::Error::Fail { cmd, code: output.status, stdout: String::from_utf8_lossy(&output.stdout).into(), stderr: String::from_utf8_lossy(&output.stderr).into() } });
        }

        // Alright
        Ok(())
    }

    /// Runs `cargo clippy` with appropriate flags.
    /// 
    /// Can be thought of as running "code quality" tests.
    /// 
    /// # Errors
    /// This function errors if we failed to spawn the command or the command returned a non-zero exit code.
    pub fn clippy() -> Result<(), Error> {
        info!("Calling `cargo clippy`");

        // Build the command to execute
        let mut cmd: Command = Command::new("cargo");
        cmd.stdout(Stdio::inherit());
        cmd.stderr(Stdio::inherit());
        cmd.args(["clippy", "--all-features", "--all-targets", "--", "-D", "warnings", "--allow", "clippy::manual_range_contains"]);

        // Attempt to execute it
        debug!("Running {cmd:?}...");
        let output: Output = match cmd.output() {
            Ok(output) => output,
            Err(err)   => { return Err(Error::Command { err: super::Error::Spawn { cmd, err } }); },
        };
        if !output.status.success() {
            return Err(Error::Command { err: super::Error::Fail { cmd, code: output.status, stdout: String::from_utf8_lossy(&output.stdout).into(), stderr: String::from_utf8_lossy(&output.stderr).into() } });
        }

        // Alright
        Ok(())
    }
}
