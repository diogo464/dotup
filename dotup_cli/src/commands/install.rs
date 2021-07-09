use std::path::PathBuf;

use super::prelude::*;

/// Install links. (Creates symlinks).
///
/// Installing a link will create the necessary directories.
/// If a file or directory already exists at the location a link would be installed this command will fail.
#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
pub struct Opts {
    /// The location where links will be installed to.
    /// Defaults to the home directory.
    #[clap(long)]
    install_base: Option<PathBuf>,

    /// The files/directories to install.
    #[clap(required = true, min_values = 1)]
    paths: Vec<PathBuf>,
}

pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    let install_base = match opts.install_base {
        Some(path) => path,
        None => utils::home_directory()?,
    };
    let depot = utils::read_depot(&config.archive_path)?;

    for link in utils::collect_links_by_base_paths(&depot, &opts.paths) {
        log::info!("Installing link {}", link);
        depot.install_link(link, &install_base)?;
    }

    Ok(())
}
