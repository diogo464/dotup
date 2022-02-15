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
    /// Path to the depot file, default to `.depot`.
    #[clap(long)]
    depot: Option<PathBuf>,

    /// Path to the install base, defaults to the home directory.
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

/// Creates an empty depot file if one doesnt already exist.
///
/// By default this will create the file in the current directory
/// but the `path` option can be used to change this path.
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

/// Creates links
///
/// If a link is created for a file that already had a link then the old link will be overwritten.
/// By default creating a link to a directory will recursively link all files under that
/// directory, to actually link a directory use the --directory flag.
#[derive(Parser, Debug)]
struct LinkArgs {
    #[clap(long)]
    directory: bool,

    #[clap(min_values = 1)]
    origins: Vec<PathBuf>,

    destination: PathBuf,
}

fn command_link(global_flags: Flags, args: LinkArgs) -> anyhow::Result<()> {
    let mut dotup = utils::read_dotup(&global_flags)?;
    for origin in args.origins {
        if !args.directory && origin.is_dir() {
            let directory = origin;
            let origins = utils::collect_files_in_dir_recursive(&directory)?;
            for origin in origins {
                // unwrap: origin is under directory so stripping should not fail
                let path_extra = origin.strip_prefix(&directory).unwrap();
                let destination = args.destination.join(path_extra);
                dotup.link(&origin, &destination);
            }
        } else {
            dotup.link(&origin, &args.destination);
        };
    }
    utils::write_dotup(&dotup)?;
    Ok(())
}

/// Unlinks files/directories.
///
/// This will recursively remove links. If a path is a directory then it will remove all links
/// recursively.
/// The links are not uninstall by default, see the --uninstall parameter.
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

/// Install links. (Creates symlinks).
///
/// Installing a link will create the necessary directories.
/// If a file or directory already exists at the location a link would be installed this command will fail.
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

/// Uninstalls links. (Removes symlinks).
///
/// Uninstalling a link for a file that didn't have a link will do nothing.
/// Uninstalling a directory will recursively uninstall all files under it.
/// Symlinks are only deleted if they were pointing to the correct file.
#[derive(Parser, Debug)]
struct UninstallArgs {
    paths: Vec<PathBuf>,
}

fn command_uninstall(global_flags: Flags, args: UninstallArgs) -> anyhow::Result<()> {
    let dotup = utils::read_dotup(&global_flags)?;
    dotup.uninstall(args.paths.into_iter());
    Ok(())
}

/// Moves files/directories and updates links.
#[derive(Parser, Debug)]
struct MvArgs {
    #[clap(min_values = 1)]
    origins: Vec<PathBuf>,

    destination: PathBuf,
}

fn command_mv(global_flags: Flags, args: MvArgs) -> anyhow::Result<()> {
    let mut dotup = utils::read_dotup(&global_flags)?;
    dotup.mv(args.origins.into_iter(), args.destination);
    utils::write_dotup(&dotup)?;
    Ok(())
}

/// Shows information about links
#[derive(Parser, Debug)]
struct StatusArgs {
    #[clap(default_value = ".")]
    paths: Vec<PathBuf>,
}

fn command_status(global_flags: Flags, args: StatusArgs) -> anyhow::Result<()> {
    let dotup = utils::read_dotup(&global_flags)?;
    dotup.status(args.paths.into_iter());
    Ok(())
}
