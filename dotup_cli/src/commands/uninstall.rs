use std::path::PathBuf;

use super::prelude::*;

/// Uninstalls links. (Removes symlinks).
///
/// Uninstalling a link for a file that didnt have a link will do nothing.
/// Uninstalling a directory will recursively uninstall all files under it.
/// Symlinks are only deleted if they were pointing to the correct file.
#[derive(Parser)]
pub struct Opts {
    /// The files/directories to uninstall.
    #[clap(min_values = 1, default_value = ".")]
    paths: Vec<PathBuf>,
}

pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    let depot = utils::read_depot(&config.archive_path)?;

    for link in utils::collect_links_by_base_paths(&depot, &opts.paths) {
        log::info!("Uninstalling link : {}", link);
        depot.uninstall_link(link, &config.install_path)?;
    }

    Ok(())
}
