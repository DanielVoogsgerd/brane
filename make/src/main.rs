//  MAIN.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 10:25:30
//  Last edited:
//    13 Oct 2023, 11:05:27
//  Auto updated?
//    Yes
// 
//  Description:
//!   Entrypoint for the Brane Buildsystem executable (`make`).
// 

use clap::Parser as _;
use error_trace::ErrorTrace as _;
use humanlog::{DebugMode, HumanLogger};
use log::{error, info};

use make::args::{BuildModeSubcommand, ToplevelArguments};
use make::{develop, release};


/***** ENTRYPOINT *****/
fn main() {
    // Parse the arguments
    let args: ToplevelArguments = ToplevelArguments::parse();

    // Setup the logger
    if let Err(err) = HumanLogger::terminal(DebugMode::from_flags(args.trace, args.debug)).init() {
        eprintln!("WARNING: Failed to setup logger: {err} (no logging enabled for this session)");
    }
    info!("`{}` v{} - The Brane Build System", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));



    // Alright well switch on the build mode first
    match args.build_mode {
        BuildModeSubcommand::Release(release) => {
            // Delegate to the proper module
            if let Err(err) = release::build_target() { error!("{}", err.trace()); std::process::exit(1); }
        },

        BuildModeSubcommand::Develop(mut develop) => {
            // Propagate some alias stuff
            if !develop.no_install {
                develop.no_install_cargo = false;
            }

            // Delegate to the proper module
            if let Err(err) = develop::build_target(develop) { error!("{}", err.trace()); std::process::exit(1); }
        },
    }
}
