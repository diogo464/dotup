#![feature(drain_filter)]

//pub mod config;
pub mod dotup;

use std::path::PathBuf;

use anyhow::Context;
use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
struct GlobalFlags {
    #[clap(long)]
    base: Option<PathBuf>,

    #[clap(long, default_value = "./dotup")]
    config: PathBuf,
}

#[derive(Subcommand, Debug)]
enum SubCommand {
    Install(InstallArgs),
    Uninstall(UninstallArgs),
    Status(StatusArgs),
    Format(FormatArgs),
}

#[derive(Parser, Debug)]
struct InstallArgs {
    groups: Vec<String>,
}

#[derive(Parser, Debug)]
struct UninstallArgs {
    groups: Vec<String>,
}

#[derive(Parser, Debug)]
struct StatusArgs {
    groups: Vec<String>,
}

#[derive(Parser, Debug)]
struct FormatArgs {}

#[derive(Parser, Debug)]
struct Args {
    #[clap(flatten)]
    globals: GlobalFlags,
    #[clap(subcommand)]
    command: SubCommand,
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let args = Args::parse();
    match args.command {
        SubCommand::Install(install) => command_install(args.globals, install),
        SubCommand::Uninstall(uninstall) => command_uninstall(args.globals, uninstall),
        SubCommand::Status(status) => command_status(args.globals, status),
        SubCommand::Format(format) => command_format(args.globals, format),
    }
}

impl GlobalFlags {
    fn base_path_or_default(&self) -> PathBuf {
        self.base.clone().unwrap_or_else(|| {
            PathBuf::from(std::env::var("HOME").expect("failed to get HOME directory"))
        })
    }
}

fn command_install(globals: GlobalFlags, args: InstallArgs) -> anyhow::Result<()> {
    let dotup = dotup::load_file(&globals.config).context("failed to parse config")?;
    let cwd = std::env::current_dir().context("failed to get current directory")?;
    let install_params = dotup::InstallParams {
        cwd: &cwd,
        home: &globals.base_path_or_default(),
    };
    for group in args.groups {
        match dotup.find_group_by_name(&group) {
            Some(group_id) => dotup.install(install_params, group_id)?,
            None => log::error!("group not found: {}", group),
        };
    }
    Ok(())
}

fn command_uninstall(globals: GlobalFlags, args: UninstallArgs) -> anyhow::Result<()> {
    let dotup = dotup::load_file(&globals.config).context("failed to parse config")?;
    let cwd = std::env::current_dir().context("failed to get current directory")?;
    let uninstall_params = dotup::UninstallParams {
        cwd: &cwd,
        home: &globals.base_path_or_default(),
    };
    for group in args.groups {
        match dotup.find_group_by_name(&group) {
            Some(group_id) => dotup.uninstall(uninstall_params, group_id)?,
            None => log::error!("group not found: {}", group),
        };
    }
    Ok(())
}

fn command_status(globals: GlobalFlags, args: StatusArgs) -> anyhow::Result<()> {
    let dotup = dotup::load_file(&globals.config).context("failed to parse config")?;
    let cwd = std::env::current_dir().context("failed to get current directory")?;
    let install_params = dotup::InstallParams {
        cwd: &cwd,
        home: &globals.base_path_or_default(),
    };
    for group in args.groups {
        match dotup.find_group_by_name(&group) {
            Some(group_id) => dotup.status(install_params, group_id)?,
            None => log::error!("group not found: {}", group),
        };
    }
    Ok(())
}

fn command_format(globals: GlobalFlags, _args: FormatArgs) -> anyhow::Result<()> {
    dotup::format_file_inplace(&globals.config).context("failed to format config")?;
    Ok(())
}
