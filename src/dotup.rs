use std::{
    cmp::Ordering,
    collections::HashSet,
    path::{Path, PathBuf},
};

use ansi_term::Color;
use anyhow::Context;

use crate::{
    depot::{self, Depot, DirNode, LinkID},
    utils,
};

#[derive(Debug)]
struct CanonicalPair {
    origin: PathBuf,
    destination: PathBuf,
}

#[derive(Debug, Clone)]
enum StatusItem {
    Link {
        origin: PathBuf,
        destination: PathBuf,
        is_directory: bool,
    },
    Directory {
        origin: PathBuf,
        items: Vec<StatusItem>,
    },
    Unlinked {
        origin: PathBuf,
        is_directory: bool,
    },
}

impl StatusItem {
    fn display_ord_cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (
                StatusItem::Link {
                    origin: l_origin, ..
                },
                StatusItem::Link {
                    origin: r_origin, ..
                },
            ) => l_origin.cmp(r_origin),
            (StatusItem::Link { .. }, StatusItem::Directory { .. }) => Ordering::Less,
            (
                StatusItem::Link {
                    is_directory: l_is_dir,
                    ..
                },
                StatusItem::Unlinked {
                    is_directory: u_is_dir,
                    ..
                },
            ) => {
                if *u_is_dir && !*l_is_dir {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            }
            (StatusItem::Directory { .. }, StatusItem::Link { .. }) => Ordering::Greater,
            (
                StatusItem::Directory {
                    origin: l_origin, ..
                },
                StatusItem::Directory {
                    origin: r_origin, ..
                },
            ) => l_origin.cmp(r_origin),
            (StatusItem::Directory { .. }, StatusItem::Unlinked { .. }) => Ordering::Greater,
            (
                StatusItem::Unlinked {
                    is_directory: u_is_dir,
                    ..
                },
                StatusItem::Link {
                    is_directory: l_is_dir,
                    ..
                },
            ) => {
                if *u_is_dir && !*l_is_dir {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            }
            (StatusItem::Unlinked { .. }, StatusItem::Directory { .. }) => Ordering::Less,
            (
                StatusItem::Unlinked {
                    origin: l_origin, ..
                },
                StatusItem::Unlinked {
                    origin: r_origin, ..
                },
            ) => l_origin.cmp(r_origin),
        }
    }
}

#[derive(Debug)]
pub struct Dotup {
    depot: Depot,
    depot_dir: PathBuf,
    depot_path: PathBuf,
    install_base: PathBuf,
}

impl Dotup {
    fn new(depot: Depot, depot_path: PathBuf, install_base: PathBuf) -> anyhow::Result<Self> {
        assert!(depot_path.is_absolute());
        assert!(depot_path.is_file());
        assert!(install_base.is_absolute());
        assert!(install_base.is_dir());
        let depot_dir = {
            let mut d = depot_path.clone();
            d.pop();
            d
        };
        Ok(Self {
            depot,
            depot_dir,
            depot_path,
            install_base,
        })
    }

    pub fn link(&mut self, origin: impl AsRef<Path>, destination: impl AsRef<Path>) {
        let link_result: anyhow::Result<()> = try {
            let origin = self.prepare_relative_path(origin.as_ref())?;
            let destination = destination.as_ref();
            self.depot.link_create(origin, destination)?;
        };
        match link_result {
            Ok(_) => {}
            Err(e) => println!("Failed to create link : {e}"),
        }
    }

    pub fn unlink(&mut self, paths: impl Iterator<Item = impl AsRef<Path>>, uninstall: bool) {
        for origin in paths {
            let unlink_result: anyhow::Result<()> = try {
                let origin = self.prepare_relative_path(origin.as_ref())?;
                let links_under: Vec<_> = self.depot.links_under(&origin)?.collect();
                for link_id in links_under {
                    if uninstall && self.symlink_is_installed_by_link_id(link_id)? {
                        self.symlink_uninstall_by_link_id(link_id)?;
                    }
                    self.depot.link_remove(link_id);
                }
            };
            match unlink_result {
                Ok(_) => {}
                Err(e) => println!("Failed to unlink {} : {e}", origin.as_ref().display()),
            }
        }
    }

    pub fn install(&self, paths: impl Iterator<Item = impl AsRef<Path>>) {
        let install_result: anyhow::Result<()> = try {
            let link_ids = self.link_ids_from_paths_iter(paths)?;
            self.depot.links_verify_install(link_ids.iter().copied())?;

            for link_id in link_ids {
                self.symlink_install_by_link_id(link_id)?;
            }
        };
        if let Err(e) = install_result {
            println!("error while installing : {e}");
        }
    }

    pub fn uninstall(&self, paths: impl Iterator<Item = impl AsRef<Path>>) {
        let uninstall_result: anyhow::Result<()> = try {
            let link_ids = self.link_ids_from_paths_iter(paths)?;
            for link_id in link_ids {
                if self.symlink_is_installed_by_link_id(link_id)? {
                    self.symlink_uninstall_by_link_id(link_id)?;
                }
            }
        };
        if let Err(e) = uninstall_result {
            println!("error while uninstalling {e}",);
        }
    }

    pub fn mv(
        &mut self,
        origins: impl Iterator<Item = impl AsRef<Path>>,
        destination: impl AsRef<Path>,
    ) {
        let mv_result: anyhow::Result<()> = try {
            let origins = {
                let mut v = Vec::new();
                for origin in origins {
                    v.push(
                        origin
                            .as_ref()
                            .canonicalize()
                            .context("failed to canonicalize origin path")?,
                    );
                }
                v
            };
            let destination = utils::weakly_canonical(destination.as_ref());
            log::debug!("mv destination : {}", destination.display());

            // if we are moving multiple links then the destination must be a directory
            if origins.len() > 1 && !destination.is_dir() {
                println!("destination must be a directory");
                return;
            }

            for origin in origins {
                let destination = if destination.is_dir() {
                    // unwrap: origin must have a filename
                    destination.join(origin.file_name().unwrap())
                } else {
                    destination.to_owned()
                };
                self.mv_one(&origin, &destination)?;
            }
        };
        if let Err(e) = mv_result {
            println!("error moving : {e}");
        }
    }

    fn mv_one(&mut self, origin: &Path, destination: &Path) -> anyhow::Result<()> {
        log::debug!("mv_one : {} to {}", origin.display(), destination.display());

        let relative_origin = self.prepare_relative_path(origin)?;
        let relative_destination = self.prepare_relative_path(destination)?;
        match self.depot.link_find(&relative_origin)? {
            Some(link_id) => {
                let is_installed = self.symlink_is_installed_by_link_id(link_id)?;
                let original_origin = self.depot.link_view(link_id).origin().to_owned();
                log::debug!("is_installed = {is_installed}",);
                log::debug!("original_origin = {}", original_origin.display());
                log::debug!("link_destination = {}", relative_destination.display());

                self.depot.link_move(link_id, relative_destination)?;
                if let Err(e) = std::fs::rename(origin, destination).context("Failed to move file")
                {
                    // unwrap: moving the link back to its origin place has to work
                    self.depot.link_move(link_id, original_origin).unwrap();
                    return Err(e);
                }
                // reinstall because we just moved the origin
                if is_installed {
                    self.symlink_install_by_link_id(link_id)
                        .context("failed to reinstall link while moving")?;
                }
            }
            None => {
                if origin.is_dir() {
                    let mut links_installed: HashSet<_> = Default::default();
                    if self.depot.has_links_under(&relative_origin)? {
                        let links_under: Vec<_> =
                            self.depot.links_under(&relative_origin)?.collect();
                        for &link_id in links_under.iter() {
                            let link_view = self.depot.link_view(link_id);
                            if self.symlink_is_installed_by_link_id(link_id)? {
                                links_installed.insert(link_id);
                            }
                            // unwrap: the link is under `origin` so stripping the prefix should
                            // not fail
                            let origin_extra =
                                link_view.origin().strip_prefix(&relative_origin).unwrap();
                            let new_destination = relative_destination.join(origin_extra);
                            self.depot.link_move(link_id, new_destination)?;
                        }
                    }
                    std::fs::rename(origin, destination)?;
                    for link_id in links_installed {
                        self.symlink_install_by_link_id(link_id)?;
                    }
                } else {
                    std::fs::rename(origin, destination)?;
                }
            }
        }
        Ok(())
    }

    pub fn status(&self) {
        let status_result: anyhow::Result<()> = try {
            let canonical_dir = utils::current_working_directory();
            let item = self.status_path_to_item(&canonical_dir)?;
            self.status_print_item(item, 0)?;
        };
        if let Err(e) = status_result {
            println!("error while displaying status : {e}");
        }
    }
    fn status_path_to_item(&self, canonical_path: &Path) -> anyhow::Result<StatusItem> {
        debug_assert!(canonical_path.is_absolute());
        debug_assert!(canonical_path.exists());
        let relative_path = self.prepare_relative_path(canonical_path)?;

        let item = if canonical_path.is_dir() {
            if let Some(link_id) = self.depot.link_find(&relative_path)? {
                let destination = self.depot.link_view(link_id).destination().to_owned();
                StatusItem::Link {
                    origin: relative_path,
                    destination,
                    is_directory: true,
                }
            } else if self.depot.has_links_under(&relative_path)? {
                let mut items = Vec::new();
                let mut collected_rel_paths = HashSet::<PathBuf>::new();
                let directory_paths = utils::collect_paths_in_dir(&canonical_path)?;
                for canonical_item_path in directory_paths {
                    let item = self.status_path_to_item(&canonical_item_path)?;
                    match &item {
                        StatusItem::Link { origin, .. } | StatusItem::Directory { origin, .. } => {
                            collected_rel_paths.insert(origin.to_owned());
                        }
                        _ => {}
                    }
                    items.push(item);
                }

                for dir_node in self.depot.read_dir(&relative_path)? {
                    match dir_node {
                        DirNode::Link(link_id) => {
                            let link_view = self.depot.link_view(link_id);
                            let link_rel_path = link_view.origin();
                            let link_rel_dest = link_view.destination();
                            if !collected_rel_paths.contains(link_rel_path) {
                                items.push(StatusItem::Link {
                                    origin: link_rel_path.to_owned(),
                                    destination: link_rel_dest.to_owned(),
                                    is_directory: false,
                                });
                            }
                        }
                        DirNode::Directory(_) => {}
                    }
                }

                StatusItem::Directory {
                    origin: relative_path,
                    items,
                }
            } else {
                StatusItem::Unlinked {
                    origin: relative_path,
                    is_directory: true,
                }
            }
        } else if let Some(link_id) = self.depot.link_find(&relative_path)? {
            let destination = self.depot.link_view(link_id).destination().to_owned();
            StatusItem::Link {
                origin: relative_path,
                destination,
                is_directory: false,
            }
        } else {
            StatusItem::Unlinked {
                origin: relative_path,
                is_directory: false,
            }
        };
        Ok(item)
    }
    fn status_print_item(&self, item: StatusItem, depth: u32) -> anyhow::Result<()> {
        fn print_depth(d: u32) {
            for _ in 0..d.saturating_sub(1) {
                print!("    ");
            }
        }
        fn origin_color(exists: bool, is_installed: bool) -> Color {
            if !exists {
                Color::Red
            } else if is_installed {
                Color::Green
            } else {
                Color::RGB(255, 127, 0)
            }
        }

        let destination_color = Color::Blue;

        print_depth(depth);
        match item {
            StatusItem::Link {
                origin,
                destination,
                is_directory,
            } => {
                let canonical_origin = self.depot_dir.join(&origin);
                let canonical_destination = self.install_base.join(&destination);
                let file_name = Self::status_get_filename(&canonical_origin);
                let is_installed =
                    self.symlink_is_installed(&canonical_origin, &canonical_destination)?;
                let exists = canonical_origin.exists();
                let origin_color = origin_color(exists, is_installed);
                let directory_extra = if is_directory { "/" } else { "" };
                println!(
                    "{}{} -> {}",
                    origin_color.paint(file_name),
                    directory_extra,
                    destination_color.paint(destination.display().to_string())
                );
            }
            StatusItem::Directory { origin, mut items } => {
                items.sort_by(|a, b| StatusItem::display_ord_cmp(a, b).reverse());
                let directory_name = Self::status_get_filename(&origin);
                if depth != 0 {
                    println!("{}/", directory_name);
                }
                for item in items {
                    self.status_print_item(item, depth + 1)?;
                }
            }
            StatusItem::Unlinked {
                origin,
                is_directory,
            } => {
                let file_name = Self::status_get_filename(&origin);
                let directory_extra = if is_directory { "/" } else { "" };
                println!("{}{}", file_name, directory_extra);
            }
        }
        Ok(())
    }
    fn status_get_filename(path: &Path) -> &str {
        path.file_name()
            .and_then(|s| s.to_str())
            .unwrap_or_default()
    }

    fn prepare_relative_path(&self, origin: &Path) -> anyhow::Result<PathBuf> {
        let canonical = utils::weakly_canonical(origin);
        let relative = canonical
            .strip_prefix(&self.depot_dir)
            .context("Invalid origin path, not under depot directory")?;
        Ok(relative.to_owned())
    }

    fn link_ids_from_paths_iter(
        &self,
        paths: impl Iterator<Item = impl AsRef<Path>>,
    ) -> anyhow::Result<Vec<LinkID>> {
        let mut link_ids = HashSet::<LinkID>::default();
        for path in paths {
            let path = self.prepare_relative_path(path.as_ref())?;
            link_ids.extend(self.depot.links_under(&path)?);
        }
        Ok(Vec::from_iter(link_ids.into_iter()))
    }

    fn symlink_is_installed_by_link_id(&self, link_id: LinkID) -> anyhow::Result<bool> {
        let canonical_pair = self.canonical_pair_from_link_id(link_id);
        self.symlink_is_installed(&canonical_pair.origin, &canonical_pair.destination)
    }

    fn symlink_is_installed(&self, origin: &Path, destination: &Path) -> anyhow::Result<bool> {
        debug_assert!(origin.is_absolute());
        debug_assert!(destination.is_absolute());

        if destination.is_symlink() {
            let symlink_destination = destination.read_link()?;
            match symlink_destination.canonicalize() {
                Ok(canonicalized) => Ok(origin == canonicalized),
                Err(_) => Ok(false),
            }
        } else {
            Ok(false)
        }
    }

    fn symlink_install_by_link_id(&self, link_id: LinkID) -> anyhow::Result<()> {
        let canonical_pair = self.canonical_pair_from_link_id(link_id);
        self.symlink_install(&canonical_pair.origin, &canonical_pair.destination)
    }

    fn symlink_install(&self, origin: &Path, destination: &Path) -> anyhow::Result<()> {
        debug_assert!(origin.is_absolute());
        debug_assert!(destination.is_absolute());
        log::debug!(
            "symlink_install : {} -> {}",
            origin.display(),
            destination.display()
        );

        let destination_parent = destination
            .parent()
            .ok_or_else(|| anyhow::anyhow!("destination has no parent component"))?;
        std::fs::create_dir_all(destination_parent).context("Failed to create directories")?;
        // need to do this beacause if the destination path ends in '/' because the symlink
        // functions will treat it as a directory but we want a file with that name.
        let destination = destination.with_file_name(destination.file_name().unwrap());

        let destination_exists = destination.exists();
        let destination_is_symlink = destination.is_symlink();

        if destination_exists && !destination_is_symlink {
            return Err(anyhow::anyhow!("destination already exists"));
        }

        if destination_is_symlink {
            log::debug!("symlink already exists, removing before recreating");
            std::fs::remove_file(&destination)?;
        }

        log::debug!(
            "creating filesystem symlink {} -> {}",
            origin.display(),
            destination.display()
        );
        std::os::unix::fs::symlink(origin, destination).context("failed to create symlink")?;

        Ok(())
    }

    fn symlink_uninstall(&self, origin: &Path, destination: &Path) -> anyhow::Result<()> {
        debug_assert!(origin.is_absolute());
        debug_assert!(destination.is_absolute());
        let destination = destination.with_file_name(destination.file_name().unwrap());

        if destination.is_symlink() {
            let symlink_destination = destination.read_link()?.canonicalize()?;
            if symlink_destination == origin {
                std::fs::remove_file(&destination)?;
            }
        }

        Ok(())
    }

    fn symlink_uninstall_by_link_id(&self, link_id: LinkID) -> anyhow::Result<()> {
        let canonical_pair = self.canonical_pair_from_link_id(link_id);
        self.symlink_uninstall(&canonical_pair.origin, &canonical_pair.destination)
    }

    fn canonical_pair_from_link_id(&self, link_id: LinkID) -> CanonicalPair {
        let link_view = self.depot.link_view(link_id);
        let relative_origin = link_view.origin();
        let relative_destination = link_view.destination();
        let canonical_origin = self.depot_dir.join(relative_origin);
        let canonical_destination = self.install_base.join(relative_destination);
        CanonicalPair {
            origin: canonical_origin,
            destination: canonical_destination,
        }
    }
}

pub fn read(depot_path: PathBuf, install_base: PathBuf) -> anyhow::Result<Dotup> {
    let depot_path = depot_path
        .canonicalize()
        .context("Failed to canonicalize depot path")?;
    let install_base = install_base
        .canonicalize()
        .context("Failed to canonicalize install base")?;
    if !install_base.is_dir() {
        return Err(anyhow::anyhow!("Install base must be a directory"));
    }
    let depot = depot::read(&depot_path)?;
    Dotup::new(depot, depot_path, install_base)
}

pub fn write(dotup: &Dotup) -> anyhow::Result<()> {
    depot::write(&dotup.depot_path, &dotup.depot)?;
    Ok(())
}
