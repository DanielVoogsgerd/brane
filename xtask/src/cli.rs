#[macro_export]
macro_rules! include_cli {
    ($x:ident) => {
        pub(crate) mod $x {
            //! test
            include!(concat!("../../brane-", stringify!($x), "/src/cli.rs"));
        }
    };
}

include_cli!(ctl);
include_cli!(cli);
include_cli!(cc);
include_cli!(reg);
include_cli!(api);
include_cli!(plr);
include_cli!(prx);
include_cli!(job);
include_cli!(drv);

// who named one of our packages 'let'...?
pub(crate) mod blet {
    include!("../../brane-let/src/cli.rs");
}

pub(crate) mod xtask {
    use clap::{Parser, Subcommand, ValueEnum};
    use clap_complete::Shell;

    use crate::{Binary, Target};

    #[derive(Debug, Parser)]
    #[clap(name = "xtask")]
    pub(crate) struct Cli {
        #[clap(subcommand)]
        pub(crate) subcommand: XTaskSubcommand,
    }

    #[derive(Debug, Subcommand)]
    pub(crate) enum XTaskSubcommand {
        Completions {
            #[clap(short, long)]
            shell:  Option<Shell>,
            #[clap(short, long)]
            binary: Option<Binary>,
        },
        Man {
            #[clap(short, long)]
            target: Option<Target>,
        },
        Install {
            #[clap(short, long, help = "Create all necessary directories")]
            force: bool,
        },
        Package {
            kind: PackageKind,
        },
    }

    #[derive(ValueEnum, Debug, Clone)]
    pub(crate) enum PackageKind {
        Release,
    }
}
