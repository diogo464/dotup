#![allow(unused)]

pub mod commands;
pub mod config;

pub use config::Config;

pub mod prelude {
    pub use super::Config;
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

const DEFAULT_DEPOT_NAME: &str = "depot.toml";

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    /// Path to the depot file.
    #[clap(long, default_value = DEFAULT_DEPOT_NAME)]
    depot: PathBuf,

    /// Disable output to the console
    #[clap(short, long)]
    quiet: bool,

    /// A level of verbosity, and can be used multiple times
    ///
    /// Level 0 - Warnings (Default)
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
            0 => "warn",
            1 => "info",
            2 => "debug",
            3 | _ => "trace",
        };

        Logger::try_with_env_or_str(log_level)?
            .format(flexi_logger::colored_default_format)
            //.set_palette("196;208;32;198;15".to_string())
            .start()?;
    }

    let config = Config {
        archive_path: opts.depot,
    };

    match opts.subcmd {
        SubCommand::Init(opts) => commands::init::main(config, opts),
        SubCommand::Link(opts) => commands::link::main(config, opts),
        SubCommand::Unlink(opts) => commands::unlink::main(config, opts),
        SubCommand::Install(opts) => commands::install::main(config, opts),
        SubCommand::Uninstall(opts) => commands::uninstall::main(config, opts),
    }
}
