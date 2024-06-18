//  MAIN.rs
//    by Lut99
//
//  Created:
//    21 Sep 2022, 14:34:28
//  Last edited:
//    08 Feb 2024, 17:15:18
//  Auto updated?
//    Yes
//
//  Description:
//!   Entrypoint to the CLI binary.
//

#[macro_use]
extern crate human_panic;

use std::path::PathBuf;
use std::process;
use std::str::FromStr;

use anyhow::Result;
use brane_cli::errors::{CliError, ImportError};
use brane_cli::spec::{Hostname, VersionFix, API_DEFAULT_VERSION};
use brane_cli::{build_ecu, build_oas, certs, check, data, instance, packages, registry, repl, run, test, upgrade, verify, version};
use brane_dsl::Language;
use brane_shr::fs::DownloadSecurity;
use brane_tsk::docker::{ClientVersion, DockerOptions};
use brane_tsk::spec::AppId;
use clap::Parser;
use dotenvy::dotenv;
use error_trace::ErrorTrace as _;
use humanlog::{DebugMode, HumanLogger};
// use git2::Repository;
use log::{error, info};
use specifications::arch::Arch;
use specifications::package::PackageKind;
use specifications::version::Version as SemVersion;
use tempfile::TempDir;

/***** ARGUMENTS *****/
#[derive(Parser)]
#[clap(name = "brane", about = "The Brane command-line interface.")]
struct Cli {
    #[clap(long, global = true, action, help = "Enable debug mode")]
    debug: bool,
    #[clap(long, action, help = "Skip dependencies check")]
    skip_check: bool,
    #[clap(subcommand)]
    sub_command: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    #[clap(name = "build", about = "Build a package")]
    Build {
        #[clap(short, long, help = "The architecture for which to compile the image.")]
        arch: Option<Arch>,
        #[clap(
            short,
            long,
            help = "Path to the directory to use as container working directory (defaults to the folder of the package file itself)"
        )]
        workdir: Option<PathBuf>,
        #[clap(name = "FILE", help = "Path to the file to build")]
        file: PathBuf,
        #[clap(short, long, help = "Kind of package: cwl, dsl, ecu or oas")]
        kind: Option<String>,
        #[clap(short, long, help = "Path to the init binary to use (override Brane's binary)")]
        init: Option<PathBuf>,
        #[clap(long, action, help = "Don't delete build files")]
        keep_files: bool,
        #[clap(
            short,
            long,
            help = "If given, does not ask permission to convert CRLF (Windows-style line endings) to LF (Unix-style line endings), but just does \
                    it."
        )]
        crlf_ok: bool,
    },

    #[clap(name = "certs", about = "Manage certificates for connecting to remote instances.")]
    Certs {
        // We subcommand further
        #[clap(subcommand)]
        subcommand: CertsSubcommand,
    },

    #[clap(
        name = "check",
        about = "Checks a workflow against the policy in the current remote instance. You can think of this as using `brane run --remote`, except \
                 that the Workflow won't be executed - only policy is checked."
    )]
    Check {
        #[clap(name = "FILE", help = "Path to the file to run. Use '-' to run from stdin instead.")]
        file:   String,
        #[clap(short, long, action, help = "Use Bakery instead of BraneScript")]
        bakery: bool,

        #[clap(short, long, help = "If given, uses the given user as end user of a workflow instead of the one in the instance file.")]
        user: Option<String>,

        #[clap(long, help = "If given, shows profile times if they are available.")]
        profile: bool,
    },

    #[clap(name = "data", about = "Data-related commands.")]
    Data {
        // We subcommand further
        #[clap(subcommand)]
        subcommand: DataSubcommand,
    },

    #[clap(name = "import", about = "Import a package")]
    Import {
        #[clap(short, long, help = "The architecture for which to compile the image.")]
        arch:    Option<Arch>,
        #[clap(name = "REPO", help = "Name of the GitHub repository containing the package")]
        repo:    String,
        #[clap(short, long, default_value = "main", help = "Name of the GitHub branch containing the package")]
        branch:  String,
        #[clap(
            short,
            long,
            help = "Path to the directory to use as container working directory, relative to the repository (defaults to the folder of the package \
                    file itself)"
        )]
        workdir: Option<PathBuf>,
        #[clap(name = "FILE", help = "Path to the file to build, relative to the repository")]
        file:    Option<PathBuf>,
        #[clap(short, long, help = "Kind of package: cwl, dsl, ecu or oas")]
        kind:    Option<String>,
        #[clap(short, long, help = "Path to the init binary to use (override Brane's binary)")]
        init:    Option<PathBuf>,

        #[clap(
            short,
            long,
            help = "If given, does not ask permission to convert CRLF (Windows-style line endings) to LF (Unix-style line endings), but just does \
                    it."
        )]
        crlf_ok: bool,
    },

    #[clap(name = "inspect", about = "Inspect a package")]
    Inspect {
        #[clap(name = "NAME", help = "Name of the package")]
        name:    String,
        #[clap(name = "VERSION", default_value = "latest", help = "Version of the package")]
        version: SemVersion,

        // Alternative syntax to use.
        #[clap(
            short,
            long,
            default_value = "custom",
            help = "Any alternative syntax to use for printed classes and functions. Can be 'bscript', 'bakery' or 'custom'."
        )]
        syntax: String,
    },

    #[clap(name = "instance", about = "Commands that relate to connecting to remote instances.")]
    Instance {
        /// Subcommand further
        #[clap(subcommand)]
        subcommand: InstanceSubcommand,
    },

    #[clap(name = "list", about = "List packages")]
    List {
        #[clap(short, long, action, help = "If given, only print the latest version of each package instead of all versions")]
        latest: bool,
    },

    #[clap(name = "load", about = "Load a package locally")]
    Load {
        #[clap(name = "NAME", help = "Name of the package")]
        name:    String,
        #[clap(short, long, default_value = "latest", help = "Version of the package")]
        version: SemVersion,
    },

    // #[clap(name = "logout", about = "Log out from a registry")]
    // Logout {},
    #[clap(name = "pull", about = "Pull a package from a registry")]
    Pull {
        #[clap(
            name = "PACKAGES",
            help = "Specify one or more packages to pull from a remote. You can either give a package as 'NAME' or 'NAME:VERSION', where VERSION is \
                    assumed to be 'latest' if omitted."
        )]
        packages: Vec<String>,
    },

    #[clap(name = "push", about = "Push a package to a registry")]
    Push {
        #[clap(
            name = "PACKAGES",
            help = "Specify one or more packages to push to a remote. You can either give a package as 'NAME' or 'NAME:VERSION', where VERSION is \
                    assumed to be 'latest' if omitted."
        )]
        packages: Vec<String>,
    },

    #[clap(name = "remove", about = "Remove a local package.")]
    Remove {
        #[clap(short, long, help = "Don't ask for confirmation before removal.")]
        force:    bool,
        #[clap(
            name = "PACKAGES",
            help = "Specify one or more packages to remove to a remote. You can either give a package as 'NAME' or 'NAME:VERSION', where ALL \
                    versions of the packages will be removed if VERSION is omitted.."
        )]
        packages: Vec<String>,

        /// The Docker socket location.
        #[cfg(unix)]
        #[clap(
            short = 's',
            long,
            default_value = "/var/run/docker.sock",
            help = "The path to the Docker socket with which we communicate with the dameon."
        )]
        docker_socket:  PathBuf,
        /// The Docker socket location.
        #[cfg(windows)]
        #[clap(
            short = 's',
            long,
            default_value = "//./pipe/docker_engine",
            help = "The path to the Docker socket with which we communicate with the dameon."
        )]
        docker_socket:  PathBuf,
        /// The Docker socket location.
        #[cfg(not(any(unix, windows)))]
        #[clap(short = 's', long, help = "The path to the Docker socket with which we communicate with the dameon.")]
        docker_socket:  PathBuf,
        /// The Docker client version.
        #[clap(short='v', long, default_value = API_DEFAULT_VERSION.as_str(), help = "The API version with which we connect.")]
        client_version: ClientVersion,
    },

    #[clap(name = "repl", about = "Start an interactive DSL session")]
    Repl {
        #[clap(short, long, value_names = &["address[:port]"], help = "If given, proxies any data transfers to this machine through the proxy at the given address. Irrelevant if not running remotely.")]
        proxy_addr: Option<String>,

        #[clap(short, long, help = "Create a remote REPL session to the instance you are currently logged-in to (see `brane login`)")]
        remote: bool,
        #[clap(short, long, value_names = &["uid"], help = "Attach to an existing remote session")]
        attach: Option<AppId>,

        #[clap(short, long, action, help = "Use Bakery instead of BraneScript")]
        bakery: bool,
        #[clap(short, long, action, help = "Clear history before session")]
        clear:  bool,

        #[clap(long, help = "If given, shows profile times if they are available.")]
        profile: bool,

        /// The Docker socket location.
        #[cfg(unix)]
        #[clap(
            short = 's',
            long,
            default_value = "/var/run/docker.sock",
            help = "The path to the Docker socket with which we communicate with the dameon."
        )]
        docker_socket:   PathBuf,
        /// The Docker socket location.
        #[cfg(windows)]
        #[clap(
            short = 's',
            long,
            default_value = "//./pipe/docker_engine",
            help = "The path to the Docker socket with which we communicate with the dameon."
        )]
        docker_socket:   PathBuf,
        /// The Docker socket location.
        #[cfg(not(any(unix, windows)))]
        #[clap(short = 's', long, help = "The path to the Docker socket with which we communicate with the dameon.")]
        docker_socket:   PathBuf,
        /// The Docker client version.
        #[clap(short='v', long, default_value = API_DEFAULT_VERSION.as_str(), help = "The API version with which we connect.")]
        client_version:  ClientVersion,
        /// Whether to keep container after running or not.
        #[clap(short = 'k', long, help = "If given, does not remove containers after execution. This is useful for debugging them.")]
        keep_containers: bool,
    },

    #[clap(name = "run", about = "Run a DSL script locally")]
    Run {
        #[clap(short, long, value_names = &["address[:port]"], help = "If given, proxies any data transfers to this machine through the proxy at the given address. Irrelevant if not running remotely.")]
        proxy_addr: Option<String>,

        #[clap(short, long, action, help = "Use Bakery instead of BraneScript")]
        bakery: bool,

        #[clap(name = "FILE", help = "Path to the file to run. Use '-' to run from stdin instead.")]
        file:    PathBuf,
        #[clap(
            long,
            conflicts_with = "remote",
            help = "If given, uses a dummy VM in the background which never actually runs any jobs. It only returns some default value for the \
                    task's return type. Use this to run only the BraneScript part of your workflow."
        )]
        dry_run: bool,
        #[clap(
            short,
            long,
            conflicts_with = "dry_run",
            help = "Create a remote session to the instance you are currently logged-in to (see `brane login`)"
        )]
        remote:  bool,

        #[clap(long, help = "If given, shows profile times if they are available.")]
        profile: bool,

        /// The Docker socket location.
        #[cfg(unix)]
        #[clap(
            short = 's',
            long,
            default_value = "/var/run/docker.sock",
            help = "The path to the Docker socket with which we communicate with the dameon."
        )]
        docker_socket:   PathBuf,
        /// The Docker socket location.
        #[cfg(windows)]
        #[clap(
            short = 's',
            long,
            default_value = "//./pipe/docker_engine",
            help = "The path to the Docker socket with which we communicate with the dameon."
        )]
        docker_socket:   PathBuf,
        /// The Docker socket location.
        #[cfg(not(any(unix, windows)))]
        #[clap(short = 's', long, help = "The path to the Docker socket with which we communicate with the dameon.")]
        docker_socket:   PathBuf,
        /// The Docker client version.
        #[clap(short='v', long, default_value = API_DEFAULT_VERSION.as_str(), help = "The API version with which we connect.")]
        client_version:  ClientVersion,
        /// Whether to keep container after running or not.
        #[clap(short = 'k', long, help = "If given, does not remove containers after execution. This is useful for debugging them.")]
        keep_containers: bool,
    },

    #[clap(name = "test", about = "Test a package locally")]
    Test {
        #[clap(name = "NAME", help = "Name of the package")]
        name: String,
        #[clap(name = "VERSION", default_value = "latest", help = "Version of the package")]
        version: SemVersion,
        #[clap(
            short = 'r',
            long,
            help = "If given, prints the intermediate result returned by the tested function (if any). The given path should be relative to the \
                    'result' folder."
        )]
        show_result: Option<PathBuf>,

        /// The Docker socket location.
        #[cfg(unix)]
        #[clap(
            short = 's',
            long,
            default_value = "/var/run/docker.sock",
            help = "The path to the Docker socket with which we communicate with the dameon."
        )]
        docker_socket:   PathBuf,
        /// The Docker socket location.
        #[cfg(windows)]
        #[clap(
            short = 's',
            long,
            default_value = "//./pipe/docker_engine",
            help = "The path to the Docker socket with which we communicate with the dameon."
        )]
        docker_socket:   PathBuf,
        /// The Docker socket location.
        #[cfg(not(any(unix, windows)))]
        #[clap(short = 's', long, help = "The path to the Docker socket with which we communicate with the dameon.")]
        docker_socket:   PathBuf,
        /// The Docker client version.
        #[clap(short='v', long, default_value = API_DEFAULT_VERSION.as_str(), help = "The API version with which we connect.")]
        client_version:  ClientVersion,
        /// Whether to keep container after running or not.
        #[clap(short = 'k', long, help = "If given, does not remove containers after execution. This is useful for debugging them.")]
        keep_containers: bool,
    },

    #[clap(name = "search", about = "Search a registry for packages")]
    Search {
        #[clap(name = "TERM", help = "Term to use as search criteria")]
        term: Option<String>,
    },

    #[clap(name = "unpublish", about = "Remove a package from a registry")]
    Unpublish {
        #[clap(name = "NAME", help = "Name of the package")]
        name:    String,
        #[clap(name = "VERSION", help = "Version of the package")]
        version: SemVersion,
        #[clap(short, long, action, help = "Don't ask for confirmation")]
        force:   bool,
    },

    #[clap(name = "upgrade", about = "Upgrades outdated configuration files to this Brane version")]
    Upgrade {
        // We subcommand further
        #[clap(subcommand)]
        subcommand: UpgradeSubcommand,
    },

    #[clap(name = "verify", about = "Verifies parts of Brane's configuration (useful mostly if you are in charge of an instance.")]
    Verify {
        // We subcommand further
        #[clap(subcommand)]
        subcommand: VerifySubcommand,
    },

    #[clap(name = "version", about = "Shows the version number for this Brane CLI tool and (if logged in) the remote Driver.")]
    Version {
        #[clap(short, long, action, help = "If given, shows the architecture instead of the version when using '--local' or '--remote'.")]
        arch:   bool,
        #[clap(
            short,
            long,
            action,
            help = "If given, shows the local version in an easy-to-be-parsed format. Note that, if given in combination with '--remote', this one \
                    is always reported first."
        )]
        local:  bool,
        #[clap(
            short,
            long,
            action,
            help = "If given, shows the remote Driver version in an easy-to-be-parsed format. Note that, if given in combination with '--local', \
                    this one is always reported second."
        )]
        remote: bool,
    },
}

/// Defines the subcommands for the `instance certs` subommand
#[derive(Parser)]
enum CertsSubcommand {
    #[clap(
        name = "add",
        about = "Adds a new CA/client certificate pair to this instance. If there are already certificates defined for this domain, will override \
                 them."
    )]
    Add {
        /// The path(s) to the certificate(s) to load.
        #[clap(
            name = "PATHS",
            help = "The path(s) to the certificate(s) to load. This should include at least the CA certificate for this domain, as well as a signed \
                    client certificate. Since a single certificate file may contain multiple certificates, however, specify how many you need."
        )]
        paths: Vec<PathBuf>,

        /// The instance for which to add it.
        #[clap(
            short,
            long,
            help = "The name of the instance to add the certificate to. If omitted, will add to the active instance instead (i.e., the one set with \
                    `brane instance select`). Use 'brane instance list' for an overview."
        )]
        instance: Option<String>,
        /// Any custom domain name.
        #[clap(
            short,
            long,
            help = "If given, overrides the location name found in the certificates. Note, however, that this name is used when we need to download \
                    from the domain, so should match the name of the location for which the certificates are valid."
        )]
        domain:   Option<String>,

        /// Whether to ask for permission before overwriting old certificates (but negated).
        #[clap(short, long, help = "If given, does not ask for permission before overwriting old certificates. Use at your own risk.")]
        force: bool,
    },
    #[clap(name = "remove", about = "Removes the certificates for a certain domain within this instance.")]
    Remove {
        /// The name(s) of the certificate(s) to remove.
        #[clap(
            name = "DOMAINS",
            help = "The name(s) of the domain(s) for which to remove the certificates. If in doubt, consult `brane certs list`."
        )]
        domains: Vec<String>,

        /// The instance from which to remove them.
        #[clap(
            short,
            long,
            help = "The name of the instance to remove the certificates from. If omitted, will be removed from the active instance instead (i.e., \
                    the one set with `brane instance select`). Use 'brane instance list' for an overview."
        )]
        instance: Option<String>,

        /// Whether to query for permission or not (but negated).
        #[clap(short, long, help = "If given, does not ask for permission before removing the certificates. Use at your own risk.")]
        force: bool,
    },

    #[clap(name = "list", about = "Lists the domains for which certificates are given.")]
    List {
        /// The instance from which to show the certificates
        #[clap(
            short,
            long,
            conflicts_with = "all",
            help = "The name of the instance to show the registered certificates in. If omitted, will list in the active instance instead (i.e., \
                    the one set with `brane instance select`). Use 'brane instance list' for an overview."
        )]
        instance: Option<String>,
        /// Whether to show all instances or only the given/active one.
        #[clap(short, long, conflicts_with = "instance", help = "If given, shows all certificates across all instances.")]
        all:      bool,
    },
}

/// Defines the subsubcommands for the data subcommand.
#[derive(Parser)]
enum DataSubcommand {
    #[clap(name = "build", about = "Builds a locally available dataset from the given data.yml file and associated files (if any).")]
    Build {
        #[clap(name = "FILE", help = "Path to the file to build.")]
        file: PathBuf,
        #[clap(short, long, help = "Path to the directory to use as the 'working directory' (defaults to the folder of the package file itself)")]
        workdir: Option<PathBuf>,
        #[clap(long, action, help = "if given, doesn't delete intermediate build files when done.")]
        keep_files: bool,
        #[clap(
            long,
            action,
            help = "If given, copies the dataset to the Brane data folder. Otherwise, merely soft links it (until the dataset is pushed to a remote \
                    repository). This is much more space efficient, but requires you to leave the original dataset in place."
        )]
        no_links: bool,
    },

    #[clap(name = "download", about = "Attempts to download one (or more) dataset(s) from the remote instance.")]
    Download {
        /// The name of the datasets to download.
        #[clap(name = "DATASETS", help = "The datasets to attempt to download.")]
        names: Vec<String>,
        /// The locations where to download each dataset. The user should make this list as long as the names, if any.
        #[clap(short, long, help = "The location identifiers from which we download each dataset, as `name=location` pairs.")]
        locs:  Vec<String>,

        /// The address to proxy the transfer through.
        #[clap(short, long, help = "If given, proxies the transfer through the given proxy.")]
        proxy_addr: Option<String>,
        /// If given, forces the data transfer even if it's locally available.
        #[clap(short, long, action, help = "If given, will always attempt to transfer data remotely, even if it's already available locally.")]
        force:      bool,
    },

    #[clap(name = "list", about = "Shows the locally known datasets.")]
    List {},

    #[clap(name = "search", about = "Shows the datasets known in the remote instance.")]
    Search {},

    #[clap(
        name = "path",
        about = "Returns the path to the dataset of the given datasets (one returned per line), if it has a path. Returns '<none>' in that latter \
                 case."
    )]
    Path {
        #[clap(name = "DATASETS", help = "The name(s) of the dataset(s) to list the paths of.")]
        names: Vec<String>,
    },

    #[clap(name = "remove", about = "Removes a locally known dataset.")]
    Remove {
        #[clap(name = "DATASETS", help = "The name(s) of the dataset(s) to remove.")]
        names: Vec<String>,
        #[clap(short, long, action, help = "If given, does not ask the user for confirmation but just removes the dataset (use at your own risk!)")]
        force: bool,
    },
}

/// Defines the subcommands for the instance subommand
#[derive(Parser)]
enum InstanceSubcommand {
    #[clap(name = "add", about = "Defines a new instance to connect to.")]
    Add {
        /// The instance's hostname.
        #[clap(
            name = "HOSTNAME",
            help = "The hostname of the instance to connect to. Should not contain any ports or paths, and any scheme (e.g., 'http://') is ignored."
        )]
        hostname: Hostname,
        /// The port of the API service.
        #[clap(
            short,
            long,
            default_value = "50051",
            help = "The port of the API service on the remote instance. You should probably only specify this if the system administrator told you \
                    to change it."
        )]
        api_port: u16,
        /// The port of the driver service.
        #[clap(
            short,
            long,
            default_value = "50053",
            help = "The port of the driver service on the remote instance. You should probably only specify this if the system administrator told \
                    you to change it."
        )]
        drv_port: u16,
        /// The name of the user as which we login.
        #[clap(
            short = 'U',
            long,
            help = "The name as which to login to the instance. This is used to tell checkers who will download the result, but only tentatively; a \
                    final check happens using domain-specific credentials. Will default to a random name when omitted."
        )]
        user:     Option<String>,

        /// Any custom name for this instance.
        #[clap(short, long, help = "Some name to set for this instance. If omitted, will set the hostname instead.")]
        name: Option<String>,
        /// Whether to use this instance immediately or not.
        #[clap(
            short,
            long = "use",
            help = "If given, immediately uses this instance (i.e., acts as if `brane instance switch <name>` is called for this instance)"
        )]
        use_immediately: bool,
        /// Whether to skip checking if the instance is alive or not.
        #[clap(long, help = "If given, skips checking if the instance is reachable.")]
        unchecked: bool,
        /// Whether to ask for permission before overwriting old certificates (but negated).
        #[clap(short, long, help = "If given, does not ask for permission before overwriting old certificates. Use at your own risk.")]
        force: bool,
    },
    #[clap(name = "remove", about = "Deletes a registered instance.")]
    Remove {
        /// The name(s) of the instance(s) to remove.
        #[clap(name = "NAMES", help = "The name(s) of the instance(s) to remove. If in doubt, consult `brane instance list`.")]
        names: Vec<String>,

        /// Whether to query for permission or not (but negated).
        #[clap(short, long, help = "If given, does not ask for permission before removing the instances. Use at your own risk.")]
        force: bool,
    },

    #[clap(name = "list", about = "Lists the registered instances.")]
    List {
        /// If given, shows an additional column in the table that shows whether this instance is online or not.
        #[clap(short, long, help = "If given, shows an additional column in the table that shows whether this instance is online or not.")]
        show_status: bool,
    },
    #[clap(name = "select", about = "Switches to the registered instance with the given name.")]
    Select {
        /// The instnace's name to switch to.
        #[clap(name = "NAME", help = "The name of the instance to switch to. If in doubt, consult `brane instance list`.")]
        name: String,
    },

    #[clap(name = "edit", about = "Changes some properties of an instance.")]
    Edit {
        /// The instance's name to edit.
        #[clap(
            name = "NAME",
            help = "The name of the instance to edit if you don't want to edit the active instance. f in doubt, consult `brane instance list`."
        )]
        name: Option<String>,

        /// Change the hostname to this.
        #[clap(short = 'H', long, help = "If given, changes the hostname of this instance to the given one.")]
        hostname: Option<Hostname>,
        /// Change the API port to this.
        #[clap(short, long, help = "If given, changes the port of the API service for this instance to this.")]
        api_port: Option<u16>,
        /// Change the driver port to this.
        #[clap(short, long, help = "If given, changes the port of the driver service for this instance to this.")]
        drv_port: Option<u16>,
        /// The name of the user as which we login.
        #[clap(
            short,
            long,
            help = "If given, changes the name as which to login to the instance. This is used to tell checkers who will download the result, but \
                    only tentatively; a final check happens using domain-specific credentials."
        )]
        user:     Option<String>,
    },
}

/// Defines the subcommands for the upgrade subcommand.
#[derive(Parser)]
enum UpgradeSubcommand {
    #[clap(name = "data", about = "Upgrades old data.yml files to this Brane version.")]
    Data {
        /// The file or folder to upgrade.
        #[clap(
            name = "PATH",
            default_value = "./",
            help = "The path to the file or folder (recursively traversed) of files to upgrade to this version. If a directory, will consider any \
                    YAML files (*.yml or *.yaml) that are successfully parsed with an old data.yml parser."
        )]
        path: PathBuf,

        /// Whether to run dryly or not
        #[clap(short, long, help = "If given, does not do anything but instead just reports which files would be updated.")]
        dry_run:   bool,
        /// Whether to keep old versions
        #[clap(
            short = 'O',
            long,
            help = "If given, will not keep the old versions alongside the new ones but instead overwrite them. Use them only if you are certain no \
                    unrelated files are converted or converted incorrectly! (see '--dry-run')"
        )]
        overwrite: bool,
        /// Fixes the version from which we are converting.
        #[clap(
            short,
            long,
            default_value = "all",
            help = "Whether to consider only one version when examining a file. Can be any valid BRANE version or 'auto' to use all supported \
                    versions."
        )]
        version:   VersionFix,
    },
}

/// Defines the subcommands for the verify subcommand.
#[derive(Parser)]
enum VerifySubcommand {
    #[clap(name = "config", about = "Verifies the configuration, e.g., an `infra.yml` files")]
    Config {
        #[clap(short, long, default_value = "./config/infra.yml", help = "The location of the infra.yml file to validate")]
        infra: PathBuf,
    },
}

/***** ENTRYPOINT *****/
#[tokio::main]
async fn main() -> Result<()> {
    // Parse the CLI arguments
    dotenv().ok();
    let options = Cli::parse();

    // Prepare the logger
    if let Err(err) = HumanLogger::terminal(if options.debug { DebugMode::Debug } else { DebugMode::HumanFriendly }).init() {
        eprintln!("WARNING: Failed to setup logger: {err} (no logging for this session)");
    }
    info!("{} - v{}", env!("CARGO_BIN_NAME"), env!("CARGO_PKG_VERSION"));

    // Also setup humanpanic
    if !options.debug {
        setup_panic!(Metadata {
            name:     "Brane CLI".into(),
            version:  env!("CARGO_PKG_VERSION").into(),
            authors:  env!("CARGO_PKG_AUTHORS").replace(':', ", ").into(),
            homepage: env!("CARGO_PKG_HOMEPAGE").into(),
        });
    }

    // Check dependencies if not withheld from doing so
    if !options.skip_check {
        match brane_cli::utils::check_dependencies().await {
            Ok(Ok(())) => {},
            Ok(Err(err)) => {
                eprintln!("Dependencies not met: {err}");
                process::exit(1);
            },
            Err(err) => {
                eprintln!("Could not check for dependencies: {err}");
                process::exit(1);
            },
        }
    }

    // Run the subcommand given
    match run(options).await {
        Ok(_) => process::exit(0),
        Err(err) => {
            error!("{}", err.trace());
            process::exit(1);
        },
    }
}

/// **Edited: now returning CliErrors.**
///
/// Runs one of the subcommand as given on the Cli.
///
/// **Arguments**
///  * `options`: The struct with (parsed) Cli-options and subcommands.
///
/// **Returns**  
/// Nothing if the subcommand executed successfully (they are self-contained), or a CliError otherwise.
async fn run(options: Cli) -> Result<(), CliError> {
    use SubCommand::*;
    match options.sub_command {
        Build { arch, workdir, file, kind, init, keep_files, crlf_ok } => {
            // Resolve the working directory
            let workdir = match workdir {
                Some(workdir) => workdir,
                None => match std::fs::canonicalize(&file) {
                    Ok(file) => file.parent().unwrap().to_path_buf(),
                    Err(err) => {
                        return Err(CliError::PackageFileCanonicalizeError { path: file, err });
                    },
                },
            };
            let workdir = match std::fs::canonicalize(workdir) {
                Ok(workdir) => workdir,
                Err(err) => {
                    return Err(CliError::WorkdirCanonicalizeError { path: file, err });
                },
            };

            // Resolve the kind of the file
            let kind = if let Some(kind) = kind {
                match PackageKind::from_str(&kind) {
                    Ok(kind) => kind,
                    Err(err) => {
                        return Err(CliError::IllegalPackageKind { kind, err });
                    },
                }
            } else {
                match brane_cli::utils::determine_kind(&file) {
                    Ok(kind) => kind,
                    Err(err) => {
                        return Err(CliError::UtilError { err });
                    },
                }
            };

            // Build a new package with it
            match kind {
                PackageKind::Ecu => build_ecu::handle(arch.unwrap_or(Arch::HOST), workdir, file, init, keep_files, crlf_ok)
                    .await
                    .map_err(|err| CliError::BuildError { err })?,
                PackageKind::Oas => build_oas::handle(arch.unwrap_or(Arch::HOST), workdir, file, init, keep_files)
                    .await
                    .map_err(|err| CliError::BuildError { err })?,
                _ => eprintln!("Unsupported package kind: {kind}"),
            }
        },
        Certs { subcommand } => {
            use CertsSubcommand::*;
            match subcommand {
                Add { paths, domain, instance, force } => {
                    if let Err(err) = certs::add(instance, paths, domain, force) {
                        return Err(CliError::CertsError { err });
                    }
                },
                Remove { domains, instance, force } => {
                    if let Err(err) = certs::remove(domains, instance, force) {
                        return Err(CliError::CertsError { err });
                    }
                },

                List { instance, all } => {
                    if let Err(err) = certs::list(instance, all) {
                        return Err(CliError::CertsError { err });
                    }
                },
            }
        },
        Check { file, bakery, user, profile } => {
            if let Err(err) = check::handle(file, if bakery { Language::Bakery } else { Language::BraneScript }, user, profile).await {
                return Err(CliError::CheckError { err });
            };
        },
        Data { subcommand } => {
            // Match again
            use DataSubcommand::*;
            match subcommand {
                Build { file, workdir, keep_files, no_links } => {
                    if let Err(err) = data::build(
                        &file,
                        workdir.unwrap_or_else(|| file.parent().map(|p| p.into()).unwrap_or_else(|| PathBuf::from("./"))),
                        keep_files,
                        no_links,
                    )
                    .await
                    {
                        return Err(CliError::DataError { err });
                    }
                },
                Download { names, locs, proxy_addr, force } => {
                    if let Err(err) = data::download(names, locs, &proxy_addr, force).await {
                        return Err(CliError::DataError { err });
                    }
                },

                List {} => {
                    if let Err(err) = data::list() {
                        return Err(CliError::DataError { err });
                    }
                },
                Search {} => {
                    eprintln!("search is not yet implemented.");
                    std::process::exit(1);
                },
                Path { names } => {
                    if let Err(err) = data::path(names) {
                        return Err(CliError::DataError { err });
                    }
                },

                Remove { names, force } => {
                    if let Err(err) = data::remove(names, force) {
                        return Err(CliError::DataError { err });
                    }
                },
            }
        },
        Import { arch, repo, branch, workdir, file, kind, init, crlf_ok } => {
            // Prepare the input URL and output directory
            let url = format!("https://api.github.com/repos/{repo}/tarball/{branch}");
            let dir = match TempDir::new() {
                Ok(dir) => dir,
                Err(err) => {
                    return Err(CliError::ImportError { err: ImportError::TempDirError { err } });
                },
            };

            // Download the file
            let tar_path: PathBuf = dir.path().join("repo.tar.gz");
            let dir_path: PathBuf = dir.path().join("repo");
            if let Err(err) = brane_shr::fs::download_file_async(&url, &tar_path, DownloadSecurity { checksum: None, https: true }, None).await {
                return Err(CliError::ImportError { err: ImportError::RepoCloneError { repo: url, target: dir_path, err } });
            }
            if let Err(err) = brane_shr::fs::unarchive_async(&tar_path, &dir_path).await {
                return Err(CliError::ImportError { err: ImportError::RepoCloneError { repo: url, target: dir_path, err } });
            }
            // Resolve that one weird folder in there
            let dir_path: PathBuf = match brane_shr::fs::recurse_in_only_child_async(&dir_path).await {
                Ok(path) => path,
                Err(err) => {
                    return Err(CliError::ImportError { err: ImportError::RepoCloneError { repo: url, target: dir_path, err } });
                },
            };

            // Try to get which file we need to use as package file
            let file = match file {
                Some(file) => dir_path.join(file),
                None => dir_path.join(brane_cli::utils::determine_file(&dir_path).map_err(|err| CliError::UtilError { err })?),
            };
            let file = match std::fs::canonicalize(&file) {
                Ok(file) => file,
                Err(err) => {
                    return Err(CliError::PackageFileCanonicalizeError { path: file, err });
                },
            };
            if !file.starts_with(&dir_path) {
                return Err(CliError::ImportError { err: ImportError::RepoEscapeError { path: file } });
            }

            // Try to resolve the working directory relative to the repository
            let workdir = match workdir {
                Some(workdir) => dir.path().join(workdir),
                None => file.parent().unwrap().to_path_buf(),
            };
            let workdir = match std::fs::canonicalize(workdir) {
                Ok(workdir) => workdir,
                Err(err) => {
                    return Err(CliError::WorkdirCanonicalizeError { path: file, err });
                },
            };
            if !workdir.starts_with(&dir_path) {
                return Err(CliError::ImportError { err: ImportError::RepoEscapeError { path: file } });
            }

            // Resolve the kind of the file
            let kind = if let Some(kind) = kind {
                match PackageKind::from_str(&kind) {
                    Ok(kind) => kind,
                    Err(err) => {
                        return Err(CliError::IllegalPackageKind { kind, err });
                    },
                }
            } else {
                match brane_cli::utils::determine_kind(&file) {
                    Ok(kind) => kind,
                    Err(err) => {
                        return Err(CliError::UtilError { err });
                    },
                }
            };

            // Build a new package with it
            match kind {
                PackageKind::Ecu => build_ecu::handle(arch.unwrap_or(Arch::HOST), workdir, file, init, false, crlf_ok)
                    .await
                    .map_err(|err| CliError::BuildError { err })?,
                PackageKind::Oas => {
                    build_oas::handle(arch.unwrap_or(Arch::HOST), workdir, file, init, false).await.map_err(|err| CliError::BuildError { err })?
                },
                _ => eprintln!("Unsupported package kind: {kind}"),
            }
        },
        Inspect { name, version, syntax } => {
            if let Err(err) = packages::inspect(name, version, syntax) {
                return Err(CliError::OtherError { err });
            };
        },
        Instance { subcommand } => {
            // Switch on the subcommand
            use InstanceSubcommand::*;
            match subcommand {
                Add { hostname, api_port, drv_port, user, name, use_immediately, unchecked, force } => {
                    if let Err(err) = instance::add(
                        name.unwrap_or_else(|| hostname.hostname.clone()),
                        hostname,
                        api_port,
                        drv_port,
                        user.unwrap_or_else(|| names::three::lowercase::rand().into()),
                        use_immediately,
                        unchecked,
                        force,
                    )
                    .await
                    {
                        return Err(CliError::InstanceError { err });
                    }
                },
                Remove { names, force } => {
                    if let Err(err) = instance::remove(names, force) {
                        return Err(CliError::InstanceError { err });
                    }
                },

                List { show_status } => {
                    if let Err(err) = instance::list(show_status).await {
                        return Err(CliError::InstanceError { err });
                    }
                },
                Select { name } => {
                    if let Err(err) = instance::select(name) {
                        return Err(CliError::InstanceError { err });
                    }
                },

                Edit { name, hostname, api_port, drv_port, user } => {
                    if let Err(err) = instance::edit(name, hostname, api_port, drv_port, user) {
                        return Err(CliError::InstanceError { err });
                    }
                },
            }
        },
        List { latest } => {
            if let Err(err) = packages::list(latest) {
                return Err(CliError::OtherError { err: anyhow::anyhow!(err) });
            };
        },
        Load { name, version } => {
            if let Err(err) = packages::load(name, version).await {
                return Err(CliError::OtherError { err });
            };
        },
        Pull { packages } => {
            // Parse the NAME:VERSION pairs into a name and a version
            if packages.is_empty() {
                println!("Nothing to do.");
                return Ok(());
            }
            let mut parsed: Vec<(String, SemVersion)> = Vec::with_capacity(packages.len());
            for package in &packages {
                parsed.push(match SemVersion::from_package_pair(package) {
                    Ok(pair) => pair,
                    Err(err) => {
                        return Err(CliError::PackagePairParseError { raw: package.into(), err });
                    },
                })
            }

            // Now delegate the parsed pairs to the actual pull() function
            if let Err(err) = registry::pull(parsed).await {
                return Err(CliError::RegistryError { err });
            };
        },
        Push { packages } => {
            // Parse the NAME:VERSION pairs into a name and a version
            if packages.is_empty() {
                println!("Nothing to do.");
                return Ok(());
            }
            let mut parsed: Vec<(String, SemVersion)> = Vec::with_capacity(packages.len());
            for package in packages {
                parsed.push(match SemVersion::from_package_pair(&package) {
                    Ok(pair) => pair,
                    Err(err) => {
                        return Err(CliError::PackagePairParseError { raw: package, err });
                    },
                })
            }

            // Now delegate the parsed pairs to the actual push() function
            if let Err(err) = registry::push(parsed).await {
                return Err(CliError::RegistryError { err });
            };
        },
        Remove { force, packages, docker_socket, client_version } => {
            // Parse the NAME:VERSION pairs into a name and a version
            if packages.is_empty() {
                println!("Nothing to do.");
                return Ok(());
            }
            let mut parsed: Vec<(String, SemVersion)> = Vec::with_capacity(packages.len());
            for package in packages {
                parsed.push(match SemVersion::from_package_pair(&package) {
                    Ok(pair) => pair,
                    Err(err) => {
                        return Err(CliError::PackagePairParseError { raw: package, err });
                    },
                })
            }

            // Now delegate the parsed pairs to the actual remove() function
            if let Err(err) = packages::remove(force, parsed, DockerOptions { socket: docker_socket, version: client_version }).await {
                return Err(CliError::PackageError { err });
            };
        },
        Repl { proxy_addr, bakery, clear, remote, attach, profile, docker_socket, client_version, keep_containers } => {
            if let Err(err) = repl::start(
                proxy_addr,
                remote,
                attach,
                if bakery { Language::Bakery } else { Language::BraneScript },
                clear,
                profile,
                DockerOptions { socket: docker_socket, version: client_version },
                keep_containers,
            )
            .await
            {
                return Err(CliError::ReplError { err });
            };
        },
        Run { proxy_addr, bakery, file, dry_run, remote, profile, docker_socket, client_version, keep_containers } => {
            if let Err(err) = run::handle(
                proxy_addr,
                if bakery { Language::Bakery } else { Language::BraneScript },
                file,
                dry_run,
                remote,
                profile,
                DockerOptions { socket: docker_socket, version: client_version },
                keep_containers,
            )
            .await
            {
                return Err(CliError::RunError { err });
            };
        },
        Test { name, version, show_result, docker_socket, client_version, keep_containers } => {
            if let Err(err) =
                test::handle(name, version, show_result, DockerOptions { socket: docker_socket, version: client_version }, keep_containers).await
            {
                return Err(CliError::TestError { err });
            };
        },
        Search { term } => {
            if let Err(err) = registry::search(term).await {
                return Err(CliError::OtherError { err });
            };
        },
        Unpublish { name, version, force } => {
            if let Err(err) = registry::unpublish(name, version, force).await {
                return Err(CliError::OtherError { err });
            };
        },
        Upgrade { subcommand } => {
            // Match the subcommand in question
            use UpgradeSubcommand::*;
            match subcommand {
                Data { path, dry_run, overwrite, version } => {
                    // Upgrade the file
                    if let Err(err) = upgrade::data(path, dry_run, overwrite, version) {
                        return Err(CliError::UpgradeError { err });
                    }
                },
            }
        },
        Verify { subcommand } => {
            // Match the subcommand in question
            use VerifySubcommand::*;
            match subcommand {
                Config { infra } => {
                    // Verify the configuration
                    if let Err(err) = verify::config(infra) {
                        return Err(CliError::VerifyError { err });
                    }
                    println!("OK");
                },
            }
        },
        Version { arch, local, remote } => {
            if local || remote {
                // If any of local or remote is given, do those
                if arch {
                    if local {
                        if let Err(err) = version::handle_local_arch() {
                            return Err(CliError::VersionError { err });
                        }
                    }
                    if remote {
                        if let Err(err) = version::handle_remote_arch().await {
                            return Err(CliError::VersionError { err });
                        }
                    }
                } else {
                    if local {
                        if let Err(err) = version::handle_local_version() {
                            return Err(CliError::VersionError { err });
                        }
                    }
                    if remote {
                        if let Err(err) = version::handle_remote_version().await {
                            return Err(CliError::VersionError { err });
                        }
                    }
                }
            } else {
                // Print neatly
                if let Err(err) = version::handle().await {
                    return Err(CliError::VersionError { err });
                }
            }
        },
    }

    Ok(())
}
