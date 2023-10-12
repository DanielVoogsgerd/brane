//  ARGS.rs
//    by Lut99
// 
//  Created:
//    12 Oct 2023, 10:28:47
//  Last edited:
//    12 Oct 2023, 10:54:13
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the CLI arguments of the `make` executable.
// 

use clap::{Parser, Subcommand};


/***** LIBRARY *****/
/// Defines the toplevel arguments of the `make`-executable.
#[derive(Debug, Parser)]
pub struct ToplevelArguments {
    /// Whether to enable INFO- and DEBUG-logging.
    #[clap(long, global=true, help="If given, enables additional INFO- and DEBUG-level logging prints.")]
    pub debug : bool,
    /// Whether to enable TRACE-logging.
    #[clap(long, global=true, help="If given, enables additional TRACE-level logging prints. Implies `--debug`.")]
    pub trace : bool,

    /// Defines the toplevel builds modes.
    #[clap(subcommand)]
    pub build_mode : BuildModeSubcommand,
}

/// Defines the subcommands given at toplevel, which define the "building mode" we're in.
#[derive(Debug, Subcommand)]
pub enum BuildModeSubcommand {
    /// Building for user experience
    #[clap(name = "release", about = "Any targets built in this mode will be prepped for release, which implies most optimisations and generally compatible building methods.")]
    Release(ReleaseArguments),

    /// Building for dev experience
    #[clap(alias = "development", alias = "develop", alias = "dev", about = "Any targets built in this mode will be prepped for development work, which implies optimising compilation times and adding debug symbols.")]
    Develop(DevelopArguments),
}



/// Defines the arguments of the `release`-subcommand.
#[derive(Debug, Parser)]
pub struct ReleaseArguments {
    
}



/// Defines the arguments of the `develop`-subcommand.
#[derive(Debug, Parser)]
pub struct DevelopArguments {
    
}



/// Defines the targets
#[derive(Debug, Subcommand)]
pub enum TargetSubcommand {
    /// Runs all sorts of tests
    #[clap(name = "test", about = "Runs tests also run by GitHub CI/CD.")]
    Test(TestArguments),

    /// Builds the CLI tool.
    #[clap(name = "cli", about = "Builds the `brane` CLI-executable for users of the framework.")]
    Cli(TargetArguments),
    /// Builds the CTL tool.
    #[clap(name = "ctl", about = "Builds the `branectl` tool for administrators of nodes in the framework.")]
    Ctl(TargetArguments),
    /// Builds the CC tool.
    #[clap(name = "cc", about = "Builds the offline BraneScript -> WIR compiler.")]
    Cc(TargetArguments),

    /// Builds the `branelet` binary.
    #[clap(name = "branelet", about = "Builds the container delegate binary `branelet`.")]
    Branelet(TargetArguments),
    /// Builds the `libbrane_cli.so` library.
    #[clap(name = "libbrane_cli", about = "Builds the `libbrane_cli.so` dynamic C-library for interfacing with client code from a C/C++ application.")]
    LibBraneCli(TargetArguments),

    /// Builds the instance binaries.
    #[clap(name = "instance", about = "Builds the Brane central node service images.")]
    Instance(InstanceArguments),
    /// Builds the worker instance binaries.
    #[clap(name = "worker-instance", about = "Builds the Brane worker node service images.")]
    WorkerInstance(WorkerInstanceArguments),
    /// Builds the proxy instance binaries.
    #[clap(name = "proxy-instance", about = "Builds the Brane proxy node service images.")]
    ProxyInstance(ProxyInstanceArguments),
}

/// Defines common arguments for all targets.
#[derive(Debug, Parser)]
pub struct TargetArguments {
    
}



/// Defines additional arguments for the `test`-target.
#[derive(Debug, Parser)]
pub struct TestArguments {
    /// The remainder of the common arguments
    #[clap(flatten)]
    pub common : TargetArguments,
}

/// Defines additional arguments for the `instance`-target.
#[derive(Debug, Parser)]
pub struct InstanceArguments {
    /// The remainder of the common arguments
    #[clap(flatten)]
    pub common : TargetArguments,
}

/// Defines additional arguments for the `worker-instance`-target.
#[derive(Debug, Parser)]
pub struct WorkerInstanceArguments {
    /// The remainder of the common arguments
    #[clap(flatten)]
    pub common : TargetArguments,
}

/// Defines additional arguments for the `proxy-instance`-target.
#[derive(Debug, Parser)]
pub struct ProxyInstanceArguments {
    /// The remainder of the common arguments
    #[clap(flatten)]
    pub common : TargetArguments,
}
