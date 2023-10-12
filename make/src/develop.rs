//  DEVELOP.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 12:06:40
//  Last edited:
//    12 Oct 2023, 17:54:45
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the toplevel functions of building stuff in `develop`-mode.
// 

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::process::{Command, Output};

use dialoguer::Confirm;
use dialoguer::theme::ColorfulTheme;
use error_trace::ErrorTrace as _;
use lazy_static::lazy_static;
use log::{debug, info, warn};

use crate::command;
use crate::utils::download_file;


/***** CONSTANTS *****/
lazy_static! {
    /// Defines the default colour theme for our prompts.
    static ref COLOUR_THEME: ColorfulTheme = ColorfulTheme::default();
}





/***** ERRORS *****/
/// Defines errors that occur in development mode.
#[derive(Debug)]
pub enum Error {
    /// Failed to ask the user something.
    Input { what: &'static str, err: dialoguer::Error },

    /// Failed to download the rustup binary/script.
    DownloadRustup { err: crate::utils::DownloadError },

    /// Failed to get the version of the local cargo.
    CargoVersion { err: command::cargo::Error },
}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use Error::*;
        match self {
            Input { what, .. } => write!(f, "Failed to ask the user (you!) {what}"),

            #[cfg(target_family = "windows")]
            DownloadRustup { .. } => write!(f, "Failed to download rustup initialization binary"),
            #[cfg(target_family = "unix")]
            DownloadRustup { .. } => write!(f, "Failed to download rustup initialization script"),

            CargoVersion { .. } => write!(f, "Failed to get version of local Cargo installation"),
        }
    }
}
impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        use Error::*;
        match self {
            Input { err, .. } => Some(err),

            DownloadRustup { err } => Some(err),

            CargoVersion { err } => Some(err),
        }
    }
}





/***** HELPER FUNCTIONS *****/
/// Asks the user a particular yes/no question.
/// 
/// # Arguments
/// - `prompt`: The question to ask.
/// 
/// # Returns
/// True if the user answer yes, false if the user answered no.
/// 
/// # Errors
/// This function fails if we couldn't reach the user.
pub fn prompt_yes_no(prompt: impl Into<String>) -> Result<bool, Error> {
    // Build the prompt
    let confirm: Confirm = Confirm::with_theme(&*COLOUR_THEME)
        .with_prompt(prompt);

    // Interact!
    match confirm.interact() {
        Ok(res)  => Ok(res),
        Err(err) => Err(Error::Input { what: "to install Cargo", err }),
    }
}

/// Installs Cargo using Rustup.
/// 
/// # Errors
/// This function may error if we failed to run the Rustup command.
#[cfg(target_family = "windows")]
pub fn install_cargo() -> Result<(), Error> {
    info!("Installing Cargo using Rustup on Windows");
    todo!();
}
#[cfg(target_family = "unix")]
pub fn install_cargo() -> Result<(), Error> {
    info!("Installing Cargo using Rustup on Unix");

    // Download the script
    if let Err(err) = download_file("https://sh.rustup.rs", "/tmp/rustup-init.sh") {
        return Err(Error::DownloadRustup { err });
    }

    Ok(())
}
#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Non-Windows, non-Unix OS not supported");





/***** LIBRARY *****/
/// Builds a particular target in development mode.
/// 
/// # Errors
/// This function may error if we failed to build the target.
pub fn build_target() -> Result<(), Error> {
    info!("Building in `development`-mode");

    // Check that `cargo` is available
    debug!("Checking `cargo` availability...");
    if let Err(err) = command::cargo::version() {
        // See if we can assert that the command wasn't found
        if let command::cargo::Error::Command { err: command::Error::Spawn { err: io_err, .. }, .. } = &err {
            if io_err.kind() == std::io::ErrorKind::NotFound {
                // Ask if the user wants to install cargo
                if prompt_yes_no("Cargo is not found. Do you want to install it using rustup?")? {
                    install_cargo()?;
                } else {
                    // Else, just give it a shot
                    warn!("Not installing Cargo, assuming subsequent cargo commands magically succeed");
                }
            } else {
                warn!("Failed to check `cargo` availability: {}", err.trace());
                warn!("Assuming cargo is installed");
            }
        } else {
            warn!("Failed to check `cargo` availability: {}", err.trace());
            warn!("Assuming cargo is installed");
        }
    }

    // Done!
    Ok(())
}
