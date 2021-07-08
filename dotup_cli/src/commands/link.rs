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

/*
   config/
       nvim/
           init.vim
           lua/
               setup.lua
       bash/
           .bashrc

   link nvim .config/nvim
       nvim/init.vim -> .config/nvim/init.vim
       nvim/lua/setup.lua -> config/nvim/lua/setup.lua

   link bash .
       bash/.bashrc -> ./.bashrc

    link --directory scripts .scripts
        scripts/ -> ./.scripts
*/

pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    let mut depot = utils::read_depot(&config.archive_path)?;

    let (origins, destination) = match opts.paths.as_slice() {
        p @ [] | p @ [_] => (p, None),
        [o @ .., dest] => (o, Some(dest)),
        _ => unreachable!(),
    };

    if let Some(destination) = destination {
        for path in collect_file_type(origins, FileType::File)? {
            let link_desc = LinkDesc {
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

    //if let Some(destination) = destination {
    //    for origin in origins {
    //        let origin_canonical = origin.canonicalize()?;
    //        let base = if origin_canonical.is_file() {
    //            origin_canonical.parent().unwrap().to_path_buf()
    //        } else {
    //            origin_canonical.to_path_buf()
    //        };

    //        link(&mut depot, origin.as_path(), destination.as_path(), &base)?;
    //    }
    //} else {
    //    log::warn!("Missing destination");
    //}

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

fn link(depot: &mut Depot, origin: &Path, destination: &Path, base: &Path) -> anyhow::Result<()> {
    let metadata = std::fs::metadata(origin)?;
    if metadata.is_file() {
        link_file(depot, origin, destination, base)?;
    } else if metadata.is_dir() {
        link_directory_recursive(depot, origin, destination, base)?;
    } else {
        unimplemented!()
    }
    Ok(())
}

fn link_file(
    depot: &mut Depot,
    origin: &Path,
    destination: &Path,
    base: &Path,
) -> anyhow::Result<()> {
    let origin_canonical = origin
        .canonicalize()
        .expect("Failed to canonicalize origin path");
    let partial = origin_canonical
        .strip_prefix(base)
        .expect("Failed to remove prefix from origin path");
    let destination = destination.join(partial);

    let link_desc = LinkDesc {
        origin: origin_canonical,
        destination,
    };

    log::debug!("Linking file {:#?}", link_desc);
    depot.create_link(link_desc)?;

    Ok(())
}

fn link_directory_recursive(
    depot: &mut Depot,
    dir_path: &Path,
    destination: &Path,
    base: &Path,
) -> anyhow::Result<()> {
    for origin in dir_path.read_dir()? {
        let origin = origin?.path();
        link(depot, &origin, destination, base)?;
    }
    Ok(())
}
