#![allow(dead_code)]

mod cli;
mod completions;
mod install;
mod man;
mod package;

use std::path::PathBuf;
use std::sync::OnceLock;

use clap::builder::PossibleValue;
use clap::{CommandFactory, Parser, ValueEnum};
use clap_complete::shells::Shell;
use strum::{EnumIter, IntoEnumIterator};
use tokio;

const SHELLS: [Shell; 3] = [Shell::Bash, Shell::Fish, Shell::Zsh];

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let opts = cli::xtask::Cli::parse();
    use cli::xtask::XTaskSubcommand;
    match opts.subcommand {
        XTaskSubcommand::Completions { binary, shell } => {
            completions::generate(binary, shell);
        },
        XTaskSubcommand::Man { target } => {
            let targets = match target {
                Some(target) => &[target][..],
                None => Target::value_variants(),
            };
            for target in targets {
                man::create_recursive(target.to_command(), "", true)?;
            }
        },
        XTaskSubcommand::Install { force } => {
            install::completions(force)?;
            install::binaries(force)?;
        },
        XTaskSubcommand::Package { kind } => {
            package::create_package(kind).await?;
        },
    }

    Ok(())
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Target {
    Binary(Binary),
    ContainerBinary(ContainerBinary),
    Image(Image),
}

impl ValueEnum for Target {
    fn value_variants<'a>() -> &'a [Self] {
        static INSTANCE: OnceLock<Box<[Target]>> = OnceLock::new();

        INSTANCE.get_or_init(|| {
            std::iter::empty()
                .chain(Binary::iter().map(|x| Self::Binary(x)))
                .chain(ContainerBinary::iter().map(|x| Self::ContainerBinary(x)))
                .chain(Image::iter().map(|x| Self::Image(x)))
                .collect::<Box<[_]>>()
        })
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        match self {
            Target::Binary(b) => b.to_possible_value(),
            Target::ContainerBinary(c) => c.to_possible_value(),
            Target::Image(i) => i.to_possible_value(),
        }
    }
}

impl Target {
    pub(crate) fn to_command(self) -> clap::Command {
        match self {
            Target::Binary(x) => x.to_command(),
            Target::ContainerBinary(x) => x.to_command(),
            Target::Image(x) => x.to_command(),
        }
    }

    // pub(crate) fn to_binary_name(self) -> &'static str {
    //     match self {
    //         Target::Binary(x) => x.to_binary_name(),
    //         Target::ContainerBinary(_) => {
    //             todo!()
    //         },
    //         Target::Image(_) => {
    //             todo!()
    //         },
    //     }
    // }
}

#[derive(Debug, Clone, Copy, ValueEnum, EnumIter)]
pub(crate) enum Binary {
    // Binaries
    #[clap(name = "branectl")]
    Branectl,
    #[clap(name = "brane")]
    Brane,
    #[clap(name = "branec")]
    BraneC,

    #[clap(name = "xtask")]
    XTask,
}

#[derive(Debug, Clone, Copy, ValueEnum, EnumIter)]
pub(crate) enum ContainerBinary {
    // Images
    #[clap(name = "branelet")]
    BraneLet,
}

#[derive(Debug, Clone, Copy, ValueEnum, EnumIter)]
pub(crate) enum Image {
    #[clap(name = "brane-api")]
    BraneAPI,
    #[clap(name = "brane-drv")]
    BraneDrv,
    #[clap(name = "brane-job")]
    BraneJob,
    #[clap(name = "brane-plr")]
    BranePlr,
    #[clap(name = "brane-prx")]
    BranePrx,
    #[clap(name = "brane-reg")]
    BraneReg,
}

impl Binary {
    // pub(crate) fn to_binary_name(self) -> &'static str {
    //     use Binary::*;
    //     match self {
    //         Branectl => "branectl",
    //         Brane => "brane",
    //         BraneC => "branec",
    //
    //         XTask => "xtask",
    //     }
    // }

    pub(crate) fn to_command(self) -> clap::Command {
        use Binary::*;
        match self {
            Branectl => crate::cli::ctl::Cli::command(),
            Brane => crate::cli::cli::Cli::command(),
            BraneC => crate::cli::cc::Cli::command(),

            XTask => crate::cli::xtask::Cli::command(),
        }
    }
}

impl ContainerBinary {
    pub(crate) fn to_command(self) -> clap::Command {
        match self {
            ContainerBinary::BraneLet => crate::cli::blet::Cli::command(),
        }
    }
}

impl Image {
    pub(crate) fn to_command(self) -> clap::Command {
        match self {
            Image::BraneAPI => crate::cli::api::Cli::command(),
            Image::BraneDrv => crate::cli::drv::Cli::command(),
            Image::BraneJob => crate::cli::job::Cli::command(),
            Image::BranePlr => crate::cli::plr::Cli::command(),
            Image::BranePrx => crate::cli::prx::Cli::command(),
            Image::BraneReg => crate::cli::reg::Cli::command(),
        }
    }
}
