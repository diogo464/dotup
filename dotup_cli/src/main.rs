#![allow(unused)]

pub mod commands;
pub mod config;
pub mod utils;

pub use config::Config;

pub mod prelude {
    pub use super::{utils, Config};
    pub use clap::{AppSettings, Parser};
    pub use dotup::{Archive, Depot, DepotConfig, Link, LinkCreateParams, LinkID};
}

use clap::{AppSettings, Parser};
use flexi_logger::Logger;
use std::{
    collections::HashMap,
    iter::FromIterator,
    path::{Path, PathBuf},
};

use prelude::*;

#[derive(Parser)]
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

    /// The location where links will be installed to.
    /// Defaults to the home directory.
    #[clap(short, long)]
    install_path: Option<PathBuf>,

    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    Init(commands::init::Opts),
    Link(commands::link::Opts),
    Status(commands::status::Opts),
    Unlink(commands::unlink::Opts),
    Install(commands::install::Opts),
    Uninstall(commands::uninstall::Opts),
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();

    if !opts.quiet {
        let log_level = match opts.verbose {
            0 => "warn",
            1 => "info",
            2 => "debug",
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
    let install_path = match opts.install_path {
        Some(path) => path,
        None => utils::home_directory()?,
    };
    log::debug!("Archive path : {}", archive_path.display());

    let config = Config {
        archive_path,
        install_path,
    };

    match opts.subcmd {
        SubCommand::Init(opts) => commands::init::main(config, opts),
        SubCommand::Link(opts) => commands::link::main(config, opts),
        SubCommand::Status(opts) => commands::status::main(config, opts),
        SubCommand::Unlink(opts) => commands::unlink::main(config, opts),
        SubCommand::Install(opts) => commands::install::main(config, opts),
        SubCommand::Uninstall(opts) => commands::uninstall::main(config, opts),
    }
}
