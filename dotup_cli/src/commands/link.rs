use clap::Clap;
use std::{
    fs::{DirEntry, Metadata},
    path::{Path, PathBuf},
};

use super::prelude::*;

#[derive(Clap)]
pub struct Opts {
    #[clap(long)]
    directory: bool,

    paths: Vec<PathBuf>,
}

pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    let mut depot = utils::read_depot(&config.archive_path)?;

    let (origins, destination) = match opts.paths.as_slice() {
        p @ [] | p @ [_] => (p, None),
        [o @ .., dest] => (o, Some(dest)),
        _ => unreachable!(),
    };

    if let Some(destination) = destination {
        let collected_paths = if opts.directory {
            origins.to_vec()
        } else {
            collect_file_type(origins, FileType::File)?
        };

        for path in collected_paths {
            let link_desc = LinkCreateParams {
                origin: path,
                destination: destination.clone(),
            };
            log::info!("Creating link : {}", link_desc);
            depot.create_link(link_desc)?;
        }
    } else {
        let base_path = match origins {
            [] => std::env::current_dir()?,
            [path] => path.clone(),
            _ => unreachable!(),
        };

        for link in utils::collect_links_by_base_paths(&depot, std::iter::once(base_path)) {
            log::info!("{}", link);
        }
    }

    utils::write_depot(&depot)?;

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum FileType {
    File,
    Directory,
}

/// Collects canonical files of the given type starting from, and including, entry_paths
fn collect_file_type(
    entry_paths: impl IntoIterator<Item = impl AsRef<Path>>,
    collect_type: FileType,
) -> anyhow::Result<Vec<PathBuf>> {
    let entry_paths: Vec<PathBuf> = entry_paths
        .into_iter()
        .map(|p| p.as_ref().to_path_buf())
        .collect();
    let mut collected = Vec::new();
    let mut pending: Vec<_> = entry_paths.iter().cloned().filter(|p| p.is_dir()).collect();

    for path in entry_paths {
        let path = path.canonicalize()?;
        if (path.is_file() && collect_type == FileType::File)
            || (path.is_dir() && collect_type == FileType::Directory)
        {
            collected.push(path);
        }
    }

    while let Some(dir_path) = pending.pop() {
        for entry in dir_path.read_dir()? {
            let entry = entry?;
            let filetype = entry.file_type()?;

            if filetype.is_file() && collect_type == FileType::File {
                collected.push(entry.path());
            } else if filetype.is_dir() {
                if collect_type == FileType::Directory {
                    collected.push(entry.path());
                }
                pending.push(entry.path());
            }
        }
    }

    Ok(collected)
}
