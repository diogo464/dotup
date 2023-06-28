#![feature(extract_if)]
#![feature(io_error_other)]

pub mod dotup;

use std::path::PathBuf;

use anyhow::Context;
use clap::{Parser, Subcommand};
use dotup::InstallParams;

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
    #[clap(short, long)]
    force: bool,

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
    fn get_working_dir(&self) -> PathBuf {
        self.config.parent().unwrap().to_path_buf()
    }

    fn base_path_or_default(&self) -> PathBuf {
        self.base.clone().unwrap_or_else(|| {
            PathBuf::from(std::env::var("HOME").expect("failed to get HOME directory"))
        })
    }
}

fn command_install(globals: GlobalFlags, args: InstallArgs) -> anyhow::Result<()> {
    let context = helper_new_context(&globals)?;
    let dotup = dotup::load_file(context, &globals.config).context("failed to parse config")?;
    let params = InstallParams { force: args.force };
    for group in args.groups {
        dotup::install(&dotup, &params, &group)?;
    }
    Ok(())
}

fn command_uninstall(globals: GlobalFlags, args: UninstallArgs) -> anyhow::Result<()> {
    let context = helper_new_context(&globals)?;
    let dotup = dotup::load_file(context, &globals.config).context("failed to parse config")?;
    for group in args.groups {
        dotup::uninstall(&dotup, &group)?;
    }
    Ok(())
}

fn command_status(globals: GlobalFlags, args: StatusArgs) -> anyhow::Result<()> {
    let context = helper_new_context(&globals)?;
    let dotup = dotup::load_file(context, &globals.config).context("failed to parse config")?;
    for group in args.groups {
        dotup::status(&dotup, &group)?;
    }
    Ok(())
}

fn command_format(globals: GlobalFlags, _args: FormatArgs) -> anyhow::Result<()> {
    dotup::format_file_inplace(&globals.config).context("failed to format config")?;
    Ok(())
}

fn helper_new_context(globals: &GlobalFlags) -> anyhow::Result<dotup::Context> {
    let cwd = globals.get_working_dir();
    let home = globals.base_path_or_default();
    Ok(dotup::Context::new(cwd, home)?)
}
