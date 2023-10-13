//  DEPENDENCIES.rs
//    by Lut99
// 
//  Created:
//    13 Oct 2023, 11:06:54
//  Last edited:
//    13 Oct 2023, 15:41:07
//  Auto updated?
//    Yes
// 
//  Description:
//!   Contains scripts for install dependencies of the `make` system and
//!   its targets.
// 

use std::error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::process::{Command, ExitStatus, Output, Stdio};

use dialoguer::Confirm;
use dialoguer::theme::ColorfulTheme;
use error_trace::ErrorTrace as _;
use lazy_static::lazy_static;
use log::{debug, info, warn};

use crate::commands;
use crate::utils::download_file;


/***** CONSTANTS *****/
lazy_static! {
    /// Defines the default colour theme for our prompts.
    static ref COLOUR_THEME: ColorfulTheme = ColorfulTheme::default();
}





/***** ERRORS *****/
/// Defines errors that occur when asserting/installing dependencies.
#[derive(Debug)]
pub enum Error {
    /// Failed to ask the user something.
    Input { what: &'static str, err: dialoguer::Error },
    /// Failed to spawn a command.
    Spawn { cmd: Command, err: std::io::Error },

    /// Failed to download the rustup binary/script.
    DownloadRustup { err: crate::utils::DownloadError },
    /// Failed to run the Rustup install command.
    InstallRustup { cmd: Command, code: ExitStatus, stdout: String, stderr: String },

    /// Failed to get the version of the local cargo.
    CargoVersion { err: commands::cargo::Error },
}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use Error::*;
        match self {
            Input { what, .. } => write!(f, "Failed to ask the user (you!) {what}"),
            Spawn { cmd, .. }  => write!(f, "Failed to spawn command {cmd:#?}"),

            #[cfg(target_family = "windows")]
            DownloadRustup { .. } => write!(f, "Failed to download rustup initialization binary"),
            #[cfg(target_family = "unix")]
            DownloadRustup { .. } => write!(f, "Failed to download rustup initialization script"),
            InstallRustup { cmd, code, stdout, stderr } => write!(f, "Command {:?} to install rustup failed with exit code {}\n\nstdout:\n{}\n{}\n{}\n\nstderr:\n{}\n{}\n{}\n\n", cmd, code.code().unwrap_or(-1), (0..80).map(|_| '-').collect::<String>(), stdout, (0..80).map(|_| '-').collect::<String>(), (0..80).map(|_| '-').collect::<String>(), stderr, (0..80).map(|_| '-').collect::<String>()),

            CargoVersion { .. } => write!(f, "Failed to get version of local Cargo installation"),
        }
    }
}
impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        use Error::*;
        match self {
            Input { err, .. } => Some(err),
            Spawn { err, .. } => Some(err),

            DownloadRustup { err } => Some(err),
            InstallRustup { .. }   => None,

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





/***** INSTALL FUNCTIONS *****/
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
    if let Err(err) = download_file("https://sh.rustup.rs", "/tmp/rustup-init.sh", None) {
        return Err(Error::DownloadRustup { err });
    }

    // Prepare the command to run the file using sh
    let mut cmd: Command = Command::new("/bin/sh");
    cmd.stdout(Stdio::inherit());
    cmd.stderr(Stdio::inherit());
    cmd.arg("/tmp/rustup-init.sh");
    cmd.args([ "-y", "--profile", "default" ]);

    // Install it
    let output: Output = match cmd.output() {
        Ok(output) => output,
        Err(err)   => { return Err(Error::Spawn { cmd, err }); },
    };
    if !output.status.success() {
        return Err(Error::InstallRustup { cmd, code: output.status, stdout: String::from_utf8_lossy(&output.stdout).into(), stderr: String::from_utf8_lossy(&output.stderr).into() });
    }

    Ok(())
}
#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Non-Windows, non-Unix OS not supported");





/***** LIBRARY *****/
/// Asserts that Cargo is installed.
/// 
/// # Arguments
/// - `ask_install`: Whether we can ask the user to install Cargo if we didn't find it.
/// 
/// # Errors
/// This function may error if there's solid grounds to believe that Cargo isn't available.
/// 
/// Note that this is _solid_ grounds; experience teaches us that checking for availability and actually using it is never the same, so it's just here to give an opportunity to install Cargo if missing.
pub fn assert_cargo(ask_install: bool) -> Result<(), Error> {
    debug!("Checking `cargo` availability...");
    if let Err(err) = commands::cargo::version() {
        // See if we can assert that the command wasn't found
        if let commands::cargo::Error::Command { err: commands::Error::Spawn { err: io_err, .. }, .. } = &err {
            if io_err.kind() == std::io::ErrorKind::NotFound {
                // Ask if the user wants to install cargo
                if ask_install && prompt_yes_no("Cargo is not found. Do you want to install it using rustup?")? {
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

    // Nice done
    Ok(())
}
