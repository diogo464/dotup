use clap::Clap;
use std::path::PathBuf;

use super::prelude::*;

#[derive(Clap)]
pub struct Opts {
    /// The location where links will be installed to.
    /// Defaults to home directory.
    #[clap(long)]
    install_base: Option<PathBuf>,

    /// The files/directories to install
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
