use std::path::{Path, PathBuf};

use ansi_term::Colour;
use clap::Parser;
use dotup::{Depot, Link};

use crate::{utils, Config};

/// Shows information about links
///
/// If a link is created for a file that already had a link then the old link will be overwritten.
/// By default creating a link to a directory will recursively link all files under that
/// directory, to actually link a directory use the --directory flag.
#[derive(Parser)]
pub struct Opts {
    /// The location where links will be installed to.
    /// Defaults to the home directory.
    #[clap(long)]
    install_base: Option<PathBuf>,

    /// The paths to show the status of
    paths: Vec<PathBuf>,
}

#[derive(Debug)]
struct State {
    max_depth: u32,
    install_path: PathBuf,
}

pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    let mut depot = utils::read_depot(&config.archive_path)?;

    // walk dir
    // if node is file:
    //      if linked
    //          print name in green and destination blue
    //      if  invalid
    //          print name and destination red
    //      if not linked
    //          print name in gray
    // if node is directory:
    //      if linked
    //          print name in green and destination blue
    //      if invalid
    //          print name and destination red
    //      if not linked:
    //          print name in gray
    //      if contains files that are linked/invalid:
    //          recurse into directory
    //

    let depot_base = depot.base_path();
    let mut paths = Vec::new();
    for path in opts.paths {
        let canonical = dotup::utils::weakly_canonical(&path);
        if canonical.starts_with(depot_base) {
            paths.push(canonical);
        } else {
            log::warn!("Path '{}' is outside the depot", path.display());
        }
    }

    if paths.is_empty() {
        paths.push(PathBuf::from("."));
    }

    let state = State {
        max_depth: u32::MAX,
        install_path: config.install_path,
    };

    let (directories, files) = utils::collect_read_dir_split(".")?;
    for path in directories.into_iter().chain(files.into_iter()) {
        display_status_path(&depot, &state, &path, 0);
    }

    utils::write_depot(&depot)?;

    Ok(())
}

fn display_status_path(depot: &Depot, state: &State, path: &Path, depth: u32) {
    if depth == state.max_depth {
        return;
    }

    if path.is_dir() {
        display_status_directory(depot, state, path, depth);
    } else {
        display_status_file(depot, state, path, depth);
    }
}

fn display_status_directory(depot: &Depot, state: &State, path: &Path, depth: u32) {
    assert!(path.is_dir());
    if depth == state.max_depth || !depot.subpath_has_links(path) {
        return;
    }

    if let Some(link) = depot.get_link_by_path(path) {
        print_link(depot, state, link, depth);
    } else {
        for entry in std::fs::read_dir(path).unwrap() {
            let entry = match entry {
                Ok(entry) => entry,
                Err(_) => continue,
            };
            let entry_path = entry.path().canonicalize().unwrap();
            let entry_path_stripped = entry_path
                .strip_prefix(std::env::current_dir().unwrap())
                .unwrap();

            print_identation(depth);
            println!(
                "{}",
                Colour::Yellow.paint(&format!("{}", path.file_name().unwrap().to_string_lossy()))
            );

            display_status_path(depot, state, &entry_path_stripped, depth + 1);
        }
    }
}

fn display_status_file(depot: &Depot, state: &State, path: &Path, depth: u32) {
    print_identation(depth);
    if let Some(link) = depot.get_link_by_path(path) {
        print_link(depot, state, link, depth);
    } else {
        print_unlinked(path, depth);
    }
}

fn print_link(depot: &Depot, state: &State, link: &Link, depth: u32) {
    let origin = link.origin();
    let dest = link.destination();
    let filename = match origin.file_name() {
        Some(filename) => filename,
        None => return,
    };

    print_identation(depth);
    if depot.is_link_installed(link, &state.install_path) {
        println!(
            "{} -> {}",
            Colour::Green.paint(&format!("{}", filename.to_string_lossy())),
            Colour::Blue.paint(&format!("{}", dest.display())),
        );
    } else {
        println!(
            "{}",
            Colour::Red.paint(&format!(
                "{} -> {}",
                filename.to_string_lossy(),
                dest.display()
            ))
        );
    }
}

fn print_unlinked(path: &Path, depth: u32) {
    let filename = match path.file_name() {
        Some(filename) => filename,
        None => return,
    };

    print_identation(depth);
    println!("{}", filename.to_string_lossy());
}

fn print_identation(depth: u32) {
    for i in 0..depth {
        print!("  ");
    }
}
