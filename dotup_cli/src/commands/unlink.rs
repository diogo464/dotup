use clap::Clap;
use std::{
    fs::{DirEntry, Metadata},
    path::{Path, PathBuf},
};

use super::prelude::*;

#[derive(Clap)]
pub struct Opts {
    /// Specifies the install base if the links are also to be uninstalled.
    #[clap(long)]
    uninstall: Option<PathBuf>,

    #[clap(required = true, min_values = 1)]
    paths: Vec<PathBuf>,
}

pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    let mut depot = utils::read_depot(&config.archive_path)?;

    for link_id in utils::collect_link_ids_by_base_paths(&depot, &opts.paths) {
        let link = depot.get_link(link_id).unwrap();
        log::info!(
            "Unlinking(uninstall = {}) : {}",
            opts.uninstall.is_some(),
            link
        );
        if let Some(ref install_base) = opts.uninstall {
            depot.uninstall_link(link, &install_base)?;
        }
        depot.remove_link(link_id);
    }

    utils::write_depot(&depot)?;

    Ok(())
}
