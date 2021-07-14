#![allow(unused)]

pub mod commands;
pub mod config;
pub mod utils;

pub use config::Config;

pub mod prelude {
    pub use super::{utils, Config};
    pub use clap::{AppSettings, Clap};
    pub use dotup::{Archive, Depot, DepotConfig, Link, LinkCreateParams, LinkID};
}

use clap::{AppSettings, Clap};
use flexi_logger::Logger;
use std::{
    collections::HashMap,
    iter::FromIterator,
    path::{Path, PathBuf},
};

use prelude::*;

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    /// Path to the depot file.
    ///
    /// By default it will try to find a file named "depot.toml" in the current directory or any of
    /// the parent directories.
    #[clap(long)]
    depot: Option<PathBuf>,

    /// Disable output to the console
    #[clap(short, long)]
    quiet: bool,

    /// A level of verbosity, and can be used multiple times
    ///
    /// Level 1 - Info
    ///
    /// Level 2 - Debug
    ///
    /// Level 3 - Trace
    #[clap(short, long, parse(from_occurrences))]
    verbose: i32,

    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    Init(commands::init::Opts),
    Link(commands::link::Opts),
    Unlink(commands::unlink::Opts),
    Install(commands::install::Opts),
    Uninstall(commands::uninstall::Opts),
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();

    if !opts.quiet {
        let log_level = match opts.verbose {
            0 => "info",
            1 => "debug",
            _ => "trace",
        };

        Logger::try_with_env_or_str(log_level)?
            .format(flexi_logger::colored_default_format)
            .set_palette("196;208;32;198;15".to_string())
            .start()?;
    }

    let archive_path = match opts.depot {
        Some(path) => path,
        None => utils::find_archive_path()?,
    };
    log::debug!("Archive path : {}", archive_path.display());

    let config = Config { archive_path };

    match opts.subcmd {
        SubCommand::Init(opts) => commands::init::main(config, opts),
        SubCommand::Link(opts) => commands::link::main(config, opts),
        SubCommand::Unlink(opts) => commands::unlink::main(config, opts),
        SubCommand::Install(opts) => commands::install::main(config, opts),
        SubCommand::Uninstall(opts) => commands::uninstall::main(config, opts),
    }
}
