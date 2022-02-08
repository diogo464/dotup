use std::path::{Path, PathBuf};

use super::prelude::*;

/// Install links. (Creates symlinks).
///
/// Installing a link will create the necessary directories.
/// If a file or directory already exists at the location a link would be installed this command will fail.
#[derive(Parser)]
pub struct Opts {
    /// The files/directories to move
    #[clap(min_values = 2)]
    paths: Vec<PathBuf>,
}

pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    let mut depot = utils::read_depot(&config.archive_path)?;

    let (sources, destination) = match opts.paths.as_slice() {
        [source, destination] => {}
        [sources @ .., destination] => {
            let mut curr_destination = destination.to_owned();
            for source in sources {
                let filename = match source.file_name() {
                    Some(filename) => filename,
                    None => {
                        log::warn!("Ignoring '{}', unknown file name", source.display());
                        continue;
                    }
                };
                curr_destination.push(filename);
                std::fs::rename(source, &curr_destination)?;
                if let Some(id) = depot.get_link_id_by_path(&source) {
                    depot.rename_link(id, &curr_destination);
                }
                curr_destination.pop();
            }
        }
        _ => unreachable!(),
    };

    utils::write_depot(&depot)?;

    Ok(())
}

fn rename(depot: &mut Depot, source: &Path, destination: &Path) -> anyhow::Result<()> {
    std::fs::rename(source, &destination)?;
    if let Some(id) = depot.get_link_id_by_path(&source) {
        depot.rename_link(id, &destination);
    }
    Ok(())
}
