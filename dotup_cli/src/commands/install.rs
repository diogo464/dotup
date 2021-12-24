use std::path::PathBuf;

use super::prelude::*;

/// Install links. (Creates symlinks).
///
/// Installing a link will create the necessary directories.
/// If a file or directory already exists at the location a link would be installed this command will fail.
#[derive(Parser)]
pub struct Opts {
    /// The files/directories to install.
    #[clap(min_values = 1, default_value = ".")]
    paths: Vec<PathBuf>,
}

pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    let depot = utils::read_depot(&config.archive_path)?;

    for link in utils::collect_links_by_base_paths(&depot, &opts.paths) {
        log::info!("Installing link {}", link);
        depot.install_link(link, &config.install_path)?;
    }

    Ok(())
}
