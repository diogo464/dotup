use slotmap::SlotMap;
use std::{
    collections::HashMap,
    fs::Metadata,
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::Error;

use crate::{internal_prelude::*, Archive, ArchiveLink};

#[derive(Debug, Error)]
pub enum LinkCreateError {
    #[error("Link origin is outside depot base\nDepot : {}\nLink : {}", .depot_base.display(), .origin.display())]
    LinkOriginOutsideDepot {
        depot_base: PathBuf,
        origin: PathBuf,
    },
    #[error("Link path is not relative : {}", .0.display())]
    LinkPathIsNotRelative(PathBuf),
    #[error("Link origin doesnt exist : {}", .0.display())]
    LinkOriginDoesntExist(PathBuf),
    #[error("Cannot create link for directory {} beacause it has a linked child", .0.display())]
    DirectoryHasLinkedChildren(PathBuf),
    #[error("Cannot create link for file {} beacause it has a linked parent", .0.display())]
    FileHasLinkedParent(PathBuf),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}

#[derive(Debug, Error)]
pub enum LinkInstallError {
    #[error(transparent)]
    IOError(#[from] std::io::Error),
    #[error("File already exists at {}", .0.display())]
    FileExists(PathBuf, Metadata),
    /// .0 = LinkPath , .1 = LinkDestination
    #[error("Link already exists {} -> {}", .0.display(), .1.display())]
    LinkExists(PathBuf, PathBuf),
}

#[derive(Debug)]
pub struct DepotConfig {
    /// The archive used to initialize the depot.
    /// A default archive can be create if one didnt already exist.
    pub archive: Archive,
    /// Path to the archive file. This path must be valid and must exist.
    pub archive_path: PathBuf,
}

slotmap::new_key_type! { pub struct LinkID; }

#[derive(Debug)]
pub struct LinkCreateParams {
    pub origin: PathBuf,
    /// This must be a relative path
    pub destination: PathBuf,
}

impl LinkCreateParams {
    pub fn new(origin: impl Into<PathBuf>, destination: impl Into<PathBuf>) -> Self {
        Self {
            origin: origin.into(),
            destination: destination.into(),
        }
    }
}

impl std::fmt::Display for LinkCreateParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "LinkDesc[{} -> {}]",
            self.origin.display(),
            self.destination.display()
        )
    }
}

impl From<ArchiveLink> for LinkCreateParams {
    fn from(archive_link: ArchiveLink) -> Self {
        Self {
            origin: archive_link.origin,
            destination: archive_link.destination,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum LinkType {
    File,
    Directory,
}

#[derive(Debug)]
pub struct Link {
    id: LinkID,
    ty: LinkType,
    /// The origin path, when joined with the depot base path, must be valid and it point to a file that exists.
    origin: PathBuf,
    /// Canonical version of origin
    origin_canonical: PathBuf,
    /// The destination path has to be a relative path.
    /// To install a link the destination path is joined with the
    /// install path and the file at base path + origin path is linked
    /// to this resulting destination path.
    destination: PathBuf,
}

impl Link {
    pub fn id(&self) -> LinkID {
        self.id
    }

    fn link_type(&self) -> LinkType {
        self.ty
    }

    /// The relative path to the origin file. Relative from depot folder.
    pub fn origin(&self) -> &Path {
        &self.origin
    }

    pub fn origin_canonical(&self) -> &Path {
        &self.origin_canonical
    }

    /// The relative path to the install destination.
    /// This path should be concatenated with an install destination to get the actual destination
    /// for this link.
    pub fn destination(&self) -> &Path {
        &self.destination
    }

    pub fn install_destination(&self, install_base: &Path) -> std::io::Result<PathBuf> {
        utils::weakly_canonical(install_base.join(self.destination()))
    }
}

impl std::fmt::Display for Link {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Link[{} -> {}]",
            self.origin().display(),
            self.destination().display()
        )
    }
}

#[derive(Debug)]
struct DepotShared {
    /// Must be canonical path
    base_path: PathBuf,
    /// Must be canonical path
    archive_path: PathBuf,
}

#[derive(Debug)]
pub struct Depot {
    shared: Arc<DepotShared>,
    // Maps the origin to the link
    links: SlotMap<LinkID, Link>,
    links_by_origin: HashMap<PathBuf, LinkID>,
}

impl Depot {
    pub fn new(config: DepotConfig) -> Result<Self> {
        depot_create(config)
    }

    /// Creates a new link from the description.
    /// The origin path must exist.
    pub fn create_link(&mut self, link_desc: LinkCreateParams) -> Result<LinkID, LinkCreateError> {
        let link = depot_create_link(self, link_desc)?;
        let link_id = depot_insert_link(self, link);
        Ok(link_id)
    }

    pub fn get_link(&self, link_id: LinkID) -> Option<&Link> {
        depot_get_link(self, link_id)
    }

    pub fn remove_link(&mut self, link_id: LinkID) {
        depot_remove_link(self, link_id)
    }

    /// Archives this depot so it can be serialized
    pub fn archive(&self) -> Archive {
        depot_archive(self)
    }

    pub fn links(&self) -> impl Iterator<Item = &Link> {
        depot_links(self)
    }

    pub fn install_link(
        &self,
        link: &Link,
        install_base: impl AsRef<Path>,
    ) -> Result<(), LinkInstallError> {
        depot_install_link(self, link, install_base.as_ref())
    }

    pub fn uninstall_link(&self, link: &Link, install_base: impl AsRef<Path>) -> Result<()> {
        depot_uninstall_link(self, link, install_base.as_ref())
    }

    pub fn base_path(&self) -> &Path {
        &self.shared.base_path
    }

    pub fn archive_path(&self) -> &Path {
        &self.shared.archive_path
    }
}

fn depot_create(config: DepotConfig) -> Result<Depot> {
    let archive_path = match config.archive_path.canonicalize() {
        Ok(canonicalized) => canonicalized,
        Err(e) => return Err(Error::ArchiveMissing(config.archive_path, e)),
    };
    if !archive_path.is_file() {
        return Err(Error::ArchivePathNotFile(archive_path));
    }
    let base_path = archive_path
        .parent()
        .expect("Failed to get parent of archive path")
        .to_path_buf();

    let depot_shared = DepotShared {
        base_path,
        archive_path,
    };

    let mut depot = Depot {
        shared: Arc::new(depot_shared),
        links: Default::default(),
        links_by_origin: Default::default(),
    };

    for archive_link in config.archive.links {
        let link_desc = LinkCreateParams::from(archive_link);
        let link = depot_create_link(&depot, link_desc)?;
        depot_insert_link(&mut depot, link);
    }

    Ok(depot)
}

fn depot_archive(depot: &Depot) -> Archive {
    let mut links = Vec::new();

    for link in depot_links(depot) {
        let archive_link = link_to_archive_link(link);
        links.push(archive_link);
    }

    Archive { links }
}

/// Create a valid link for that given Depot using the given link desc.
/// The link id is corrected when the link is inserted in the depot.
fn depot_create_link(depot: &Depot, link_desc: LinkCreateParams) -> Result<Link, LinkCreateError> {
    // link_ensure_relative_path(&link_desc.origin)?;
    link_ensure_relative_path(&link_desc.destination)?;
    debug_assert!(utils::is_canonical(depot.base_path())?);

    let origin_joined = depot.base_path().join(&link_desc.origin);
    let origin_result = origin_joined.canonicalize();
    let origin_canonical = match origin_result {
        Ok(canonical) => canonical,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => {
                return Err(LinkCreateError::LinkOriginDoesntExist(origin_joined))
            }
            _ => return Err(e.into()),
        },
    };

    if !origin_canonical.starts_with(depot.base_path()) {
        return Err(LinkCreateError::LinkOriginOutsideDepot {
            depot_base: depot.base_path().to_path_buf(),
            origin: origin_canonical,
        });
    }

    // unwrap should be fine, this path starts with the prefix
    let origin = origin_canonical
        .strip_prefix(depot.base_path())
        .unwrap()
        .to_path_buf();
    // let origin = origin_canonical;
    let destination = link_desc.destination;

    let ty = if origin.is_dir() {
        for link in depot.links() {
            if link.origin().starts_with(&origin) && link.origin() != origin {
                return Err(LinkCreateError::DirectoryHasLinkedChildren(origin));
            }
        }
        LinkType::Directory
    } else {
        for link in depot.links() {
            if origin.starts_with(link.origin()) && origin != link.origin() {
                assert_eq!(link.link_type(), LinkType::Directory);
                return Err(LinkCreateError::FileHasLinkedParent(origin));
            }
        }
        LinkType::File
    };

    Ok(Link {
        id: Default::default(),
        ty,
        origin,
        origin_canonical,
        destination,
    })
}

fn depot_get_link(depot: &Depot, link_id: LinkID) -> Option<&Link> {
    depot.links.get(link_id)
}

fn depot_remove_link(depot: &mut Depot, link_id: LinkID) {
    depot.links.remove(link_id);
}

fn depot_install_link(
    _depot: &Depot,
    link: &Link,
    install_base: &Path,
) -> Result<(), LinkInstallError> {
    let final_origin = link.origin_canonical();
    let final_destination = link.install_destination(install_base)?;

    log::debug!("Final origin : {}", final_origin.display());
    log::debug!("Final destination : {}", final_destination.display());

    if let Some(dest_base) = final_destination.parent() {
        std::fs::create_dir_all(dest_base)?;
    }

    // Exit early if there is some error or if the link already exists
    match std::fs::symlink_metadata(&final_destination) {
        Ok(metadata) => {
            let filetype = metadata.file_type();
            if filetype.is_symlink() {
                let symlink_destination = std::fs::read_link(&final_destination)?;
                if symlink_destination == final_origin {
                    return Ok(());
                }
                log::trace!(
                    "Symlink destinations where not equal : {} != {}",
                    final_origin.display(),
                    symlink_destination.display()
                );
                return Err(LinkInstallError::LinkExists(
                    final_destination,
                    symlink_destination,
                ));
            } else {
                return Err(LinkInstallError::FileExists(final_destination, metadata));
            }
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => return Err(e.into()),
    };

    log::debug!(
        "Creating symlink from {} to {}",
        final_origin.display(),
        final_destination.display()
    );
    std::os::unix::fs::symlink(&final_origin, &final_destination)?;

    Ok(())
}

fn depot_uninstall_link(_depot: &Depot, link: &Link, install_base: &Path) -> Result<()> {
    let origin_canonical = link.origin_canonical();
    let install_destination = link.install_destination(install_base)?;
    let link_target = match std::fs::read_link(&install_destination) {
        Ok(target) => target,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(e) => return Err(e.into()),
    };

    if link_target.canonicalize()? == origin_canonical {
        std::fs::remove_file(&install_destination)?;
    }

    Ok(())
}

fn depot_insert_link(depot: &mut Depot, mut link: Link) -> LinkID {
    let origin = link.origin().to_path_buf();
    if let Some(link_id) = depot.links_by_origin.remove(&origin) {
        depot.links.remove(link_id);
    }
    let link_id = depot.links.insert_with_key(move |k| {
        link.id = k;
        link
    });
    depot.links_by_origin.insert(origin, link_id);
    link_id
}

fn depot_links(depot: &Depot) -> impl Iterator<Item = &Link> {
    depot.links.values()
}

fn link_ensure_relative_path(path: &Path) -> Result<(), LinkCreateError> {
    if !path.is_relative() {
        return Err(LinkCreateError::LinkPathIsNotRelative(path.to_path_buf()));
    }
    Ok(())
}

fn link_to_archive_link(depot_link: &Link) -> ArchiveLink {
    ArchiveLink {
        origin: depot_link.origin().to_path_buf(),
        destination: depot_link.destination().to_path_buf(),
    }
}
