#![feature(try_blocks)]

mod depot;
mod dotup;
mod utils;

use std::path::PathBuf;

use clap::Parser;
use flexi_logger::Logger;
use utils::DEFAULT_DEPOT_FILE_NAME;

#[derive(Parser, Debug)]
pub struct Flags {
    #[clap(long)]
    depot: Option<PathBuf>,
    #[clap(long)]
    install_base: Option<PathBuf>,
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// A level of verbosity, and can be used multiple times
    ///
    /// Level 1 - Info
    ///
    /// Level 2 - Debug
    ///
    /// Level 3 - Trace
    #[clap(short, long, parse(from_occurrences))]
    verbose: i32,

    #[clap(flatten)]
    flags: Flags,

    #[clap(subcommand)]
    command: SubCommand,
}

#[derive(Parser, Debug)]
enum SubCommand {
    Init(InitArgs),
    Link(LinkArgs),
    Unlink(UnlinkArgs),
    Install(InstallArgs),
    Uninstall(UninstallArgs),
    Mv(MvArgs),
    Status(StatusArgs),
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let log_level = match args.verbose {
        0 => "warn",
        1 => "info",
        2 => "debug",
        _ => "trace",
    };

    Logger::try_with_env_or_str(log_level)?
        .format(flexi_logger::colored_default_format)
        .set_palette("196;208;32;198;15".to_string())
        .start()?;

    match args.command {
        SubCommand::Init(cmd_args) => command_init(args.flags, cmd_args),
        SubCommand::Link(cmd_args) => command_link(args.flags, cmd_args),
        SubCommand::Unlink(cmd_args) => command_unlink(args.flags, cmd_args),
        SubCommand::Install(cmd_args) => command_install(args.flags, cmd_args),
        SubCommand::Uninstall(cmd_args) => command_uninstall(args.flags, cmd_args),
        SubCommand::Mv(cmd_args) => command_mv(args.flags, cmd_args),
        SubCommand::Status(cmd_args) => command_status(args.flags, cmd_args),
    }
}

#[derive(Parser, Debug)]
struct InitArgs {
    path: Option<PathBuf>,
}

fn command_init(_global_flags: Flags, args: InitArgs) -> anyhow::Result<()> {
    let depot_path = {
        let mut path = args.path.unwrap_or_else(utils::default_depot_path);
        if path.is_dir() {
            path = path.join(DEFAULT_DEPOT_FILE_NAME);
        }
        path
    };

    if depot_path.exists() {
        println!("Depot at {} already exists", depot_path.display());
    } else {
        depot::write(&depot_path, &Default::default())?;
        println!("Depot initialized at {}", depot_path.display());
    }

    Ok(())
}

#[derive(Parser, Debug)]
struct LinkArgs {
    #[clap(long)]
    directory: bool,

    origin: PathBuf,

    destination: PathBuf,
}

fn command_link(global_flags: Flags, args: LinkArgs) -> anyhow::Result<()> {
    let mut dotup = utils::read_dotup(&global_flags)?;
    let origins = if args.directory {
        vec![args.origin]
    } else if args.origin.is_dir() {
        utils::collect_files_in_dir_recursive(args.origin)?
    } else {
        vec![args.origin]
    };
    for origin in origins {
        dotup.link(origin, &args.destination);
    }
    utils::write_dotup(&dotup)?;
    Ok(())
}

#[derive(Parser, Debug)]
struct UnlinkArgs {
    #[clap(long)]
    uninstall: bool,

    paths: Vec<PathBuf>,
}

fn command_unlink(global_flags: Flags, args: UnlinkArgs) -> anyhow::Result<()> {
    let mut dotup = utils::read_dotup(&global_flags)?;
    dotup.unlink(args.paths.into_iter(), args.uninstall);
    utils::write_dotup(&dotup)?;
    Ok(())
}

#[derive(Parser, Debug)]
struct InstallArgs {
    #[clap(long)]
    directory: bool,

    paths: Vec<PathBuf>,
}

fn command_install(global_flags: Flags, args: InstallArgs) -> anyhow::Result<()> {
    let dotup = utils::read_dotup(&global_flags)?;
    dotup.install(args.paths.into_iter());
    Ok(())
}

#[derive(Parser, Debug)]
struct UninstallArgs {
    paths: Vec<PathBuf>,
}

fn command_uninstall(global_flags: Flags, args: UninstallArgs) -> anyhow::Result<()> {
    let dotup = utils::read_dotup(&global_flags)?;
    dotup.uninstall(args.paths.into_iter());
    Ok(())
}

#[derive(Parser, Debug)]
struct MvArgs {
    paths: Vec<PathBuf>,
}

fn command_mv(global_flags: Flags, args: MvArgs) -> anyhow::Result<()> {
    let mut dotup = utils::read_dotup(&global_flags)?;
    let mut paths = args.paths;
    if paths.len() < 2 {
        return Err(anyhow::anyhow!("mv requires atleast 2 arguments"));
    }
    let to = paths.pop().unwrap();
    let from = paths;
    dotup.mv(from.iter(), &to);
    utils::write_dotup(&dotup)?;
    Ok(())
}

#[derive(Parser, Debug)]
struct StatusArgs {}

fn command_status(global_flags: Flags, _args: StatusArgs) -> anyhow::Result<()> {
    let dotup = utils::read_dotup(&global_flags)?;
    dotup.status();
    Ok(())
}
