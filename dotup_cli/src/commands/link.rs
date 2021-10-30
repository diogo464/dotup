use std::{
    fs::{DirEntry, Metadata},
    path::{Path, PathBuf},
};

use super::prelude::*;

/// Creates links
///
/// If a link is created for a file that already had a link then the old link will be overwritten.
/// By default creating a link to a directory will recursively link all files under that
/// directory, to actually link a directory use the --directory flag.
#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
pub struct Opts {
    /// Treats the paths as directories. This will create links to the actual directories instead
    /// of recursively linking all files under them.
    #[clap(long)]
    directory: bool,

    /// The paths to link. The last path is the destination.
    paths: Vec<PathBuf>,
}

// TODO: require destination
// remove else branch
pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    let mut depot = utils::read_depot(&config.archive_path)?;

    let (origins, destination) = match opts.paths.as_slice() {
        p @ [] | p @ [_] => (p, None),
        [o @ .., dest] => (o, Some(dest)),
        _ => unreachable!(),
    };

    if let Some(destination) = destination {
        let params = if opts.directory {
            origins
                .iter()
                .map(|p| LinkCreateParams {
                    origin: p.to_path_buf(),
                    destination: destination.clone(),
                })
                .collect()
        } else {
            let mut params = Vec::new();
            for origin in origins {
                generate_link_params(&depot, origin, destination, origin, &mut params)?;
            }
            params
        };

        for link_params in params {
            log::info!("Creating link : {}", link_params);
            depot.create_link(link_params)?;
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

fn generate_link_params(
    depot: &Depot,
    origin: &Path,
    destination: &Path,
    base: &Path,
    params: &mut Vec<LinkCreateParams>,
) -> anyhow::Result<()> {
    let metadata = std::fs::metadata(origin)?;
    if metadata.is_file() {
        generate_file_link_params(depot, origin, destination, base, params)?;
    } else if metadata.is_dir() {
        generate_directory_link_params_recursive(depot, origin, destination, base, params)?;
    }
    Ok(())
}

fn generate_file_link_params(
    depot: &Depot,
    origin: &Path,
    destination: &Path,
    base: &Path,
    params: &mut Vec<LinkCreateParams>,
) -> anyhow::Result<()> {
    let origin_canonical = origin
        .canonicalize()
        .expect("Failed to canonicalize origin path");
    let base_canonical = base
        .canonicalize()
        .expect("Failed to canonicalize base path");

    log::debug!("Origin canonical : {}", origin_canonical.display());
    log::debug!("Base : {}", base.display());

    let partial = origin_canonical
        .strip_prefix(base_canonical)
        .expect("Failed to remove prefix from origin path");
    let destination = destination.join(partial);
    let origin = origin_canonical
        .strip_prefix(depot.base_path())
        .unwrap_or(&origin_canonical);

    let link_params = LinkCreateParams {
        origin: origin.to_path_buf(),
        destination,
    };
    params.push(link_params);
    Ok(())
}

fn generate_directory_link_params_recursive(
    depot: &Depot,
    dir_path: &Path,
    destination: &Path,
    base: &Path,
    params: &mut Vec<LinkCreateParams>,
) -> anyhow::Result<()> {
    for origin in dir_path.read_dir()? {
        let origin = origin?.path();
        generate_link_params(depot, &origin, destination, base, params)?;
    }
    Ok(())
}
