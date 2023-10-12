//  MAIN.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 10:25:30
//  Last edited:
//    12 Oct 2023, 10:54:53
//  Auto updated?
//    Yes
// 
//  Description:
//!   Entrypoint for the Brane Buildsystem executable (`make`).
// 

use clap::Parser as _;
use humanlog::{DebugMode, HumanLogger};
use log::{error, info};

use make::args::{BuildModeSubcommand, ToplevelArguments};


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

        },

        BuildModeSubcommand::Develop(develop) => {
            
        },
    }
}
