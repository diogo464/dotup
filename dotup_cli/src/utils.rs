use std::path::{Path, PathBuf};

use crate::prelude::*;

const DEFAULT_DEPOT_NAME: &str = "depot.toml";

pub fn home_directory() -> anyhow::Result<PathBuf> {
    match std::env::var("HOME") {
        Ok(val) => Ok(PathBuf::from(val)),
        Err(e) => {
            log::error!("Failed to get home directory from enviornment variable");
            Err(e.into())
        }
    }
}

pub fn find_archive_path() -> anyhow::Result<PathBuf> {
    let mut start = PathBuf::new();
    while {
        start.push(DEFAULT_DEPOT_NAME);
        !start.is_file()
    } {
        start.pop();
        start.push("..");
    }
    Ok(start.canonicalize()?)
}

pub fn write_archive(path: impl AsRef<Path>, archive: &Archive) -> anyhow::Result<()> {
    let path = path.as_ref();
    log::debug!("Writing archive to {}", path.display());
    match dotup::archive_write(path, archive) {
        Ok(_) => Ok(()),
        Err(e) => {
            log::error!(
                "Failed to write archive to : {}\nError : {}",
                path.display(),
                e
            );
            Err(e.into())
        }
    }
}

pub fn write_depot(depot: &Depot) -> anyhow::Result<()> {
    let write_path = depot.archive_path();
    let archive = depot.archive();
    match dotup::archive_write(write_path, &archive) {
        Ok(_) => Ok(()),
        Err(e) => {
            log::error!(
                "Failed to write depot archive to : {}\nError : {}",
                write_path.display(),
                e
            );
            Err(e.into())
        }
    }
}

pub fn read_archive(path: impl AsRef<Path>) -> anyhow::Result<Archive> {
    let path = path.as_ref();
    match dotup::archive_read(path) {
        Ok(archive) => Ok(archive),
        Err(e) => {
            log::error!(
                "Failed to read archive from : {}\nError : {}",
                path.display(),
                e
            );
            Err(e.into())
        }
    }
}

pub fn read_depot(archive_path: impl AsRef<Path>) -> anyhow::Result<Depot> {
    let archive_path = archive_path.as_ref().to_path_buf();
    let archive = read_archive(&archive_path)?;
    let depot_config = DepotConfig {
        archive_path,
        archive,
    };
    let depot = Depot::new(depot_config)?;
    Ok(depot)
}

pub fn collect_links_by_base_paths(
    depot: &Depot,
    paths: impl IntoIterator<Item = impl AsRef<Path>>,
) -> Vec<&Link> {
    let canonical_paths: Vec<_> = paths
        .into_iter()
        .map(|p| p.as_ref().canonicalize().unwrap())
        .collect();

    depot
        .links()
        .filter(|&l| {
            canonical_paths
                .iter()
                .any(|p| l.origin_canonical().starts_with(p))
        })
        .collect()
}

pub fn collect_link_ids_by_base_paths(
    depot: &Depot,
    paths: impl IntoIterator<Item = impl AsRef<Path>>,
) -> Vec<LinkID> {
    collect_links_by_base_paths(depot, paths)
        .into_iter()
        .map(|l| l.id())
        .collect()
}
