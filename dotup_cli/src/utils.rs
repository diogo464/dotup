use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
};

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
    let cwd = std::env::current_dir()?;
    let compn = cwd.components().count();
    let mut start = PathBuf::new();
    for _ in 0..=compn {
        start.push(DEFAULT_DEPOT_NAME);
        if dotup::archive_exists(&start) {
            return Ok(start);
        }
        start.pop();
        start.push("..");
    }
    Ok(PathBuf::from(DEFAULT_DEPOT_NAME))
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
        archive: Default::default(),
        archive_path,
    };
    let mut depot = Depot::new(depot_config)?;

    for archive_link in archive.links {
        let link_params = LinkCreateParams::from(archive_link);
        if let Err(e) = depot.create_link(link_params) {
            log::warn!("Error while adding link : {}", e);
        }
    }

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

/// Returns a list of canonical paths to all the files in `dir`. This includes files in
/// subdirectories.
/// Fails if dir isnt a directory or if there is some other io error.
pub fn collect_files_in_dir(dir: impl Into<PathBuf>) -> anyhow::Result<Vec<PathBuf>> {
    let mut paths = Vec::new();
    let mut dirs = VecDeque::new();
    dirs.push_back(dir.into());

    while let Some(dir) = dirs.pop_front() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let filetype = entry.file_type()?;
            if filetype.is_dir() {
                dirs.push_back(entry.path());
            } else {
                paths.push(entry.path());
            }
        }
    }

    Ok(paths)
}

/// Collects the result of std::fs::read_dir into two vecs
/// The first one contains all the directories and the second one all the files
pub fn collect_read_dir_split(
    dir: impl AsRef<Path>,
) -> anyhow::Result<(Vec<PathBuf>, Vec<PathBuf>)> {
    Ok(std::fs::read_dir(dir)?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .partition(|p| p.is_dir()))
}

/// Checks if `path` is inside a git repository
pub fn path_is_in_git_repo(path: &Path) -> bool {
    let mut path = if !path.is_absolute() {
        dbg!(dotup::utils::weakly_canonical(path))
    } else {
        path.to_owned()
    };
    let recurse = path.pop();
    path.push(".git");
    if path.is_dir() {
        return true;
    }
    if recurse {
        path.pop();
        return path_is_in_git_repo(&path);
    } else {
        return false;
    }
}
