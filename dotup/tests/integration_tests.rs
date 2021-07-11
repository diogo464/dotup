use std::path::{Path, PathBuf};

use dotup::{ArchiveLink, Depot, DepotConfig, LinkCreateParams};
use tempfile::TempDir;

const TESTING_DEPOT_NAME: &str = "depot.toml";
const TESTING_DEPOT_CONTENTS: &str = include_str!("testing_depot.toml");

fn create_empty_file(path: impl AsRef<Path>) {
    let path = path.as_ref();
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).unwrap();
    }
    std::fs::write(path, "").unwrap();
}

fn prepare_empty_temp_dir() -> TempDir {
    TempDir::new().unwrap()
}

fn prepare_temp_dir() -> TempDir {
    let dir = TempDir::new().unwrap();
    std::fs::write(dir.path().join(TESTING_DEPOT_NAME), TESTING_DEPOT_CONTENTS).unwrap();
    create_empty_file(dir.path().join("o1/file1.txt"));
    create_empty_file(dir.path().join("o2/file2.txt"));
    dir
}

fn read_depot(dir: &TempDir) -> Depot {
    let archive_path = dir.path().join(TESTING_DEPOT_NAME);
    Depot::new(DepotConfig {
        archive: dotup::archive_read(&archive_path).unwrap(),
        archive_path,
    })
    .unwrap()
}

#[test]
fn test_archive_deserialize() {
    let archive = dotup::archive_deserialize(&TESTING_DEPOT_CONTENTS).unwrap();

    let link1 = ArchiveLink {
        origin: PathBuf::from("o1/file1.txt"),
        destination: PathBuf::from("d1/file.txt"),
    };
    let link2 = ArchiveLink {
        origin: PathBuf::from("o2/file2.txt"),
        destination: PathBuf::from("d2/d2/file.txt"),
    };

    assert_eq!(2, archive.links.len());
    assert!(archive.links.contains(&link1));
    assert!(archive.links.contains(&link2));
}

#[test]
fn test_archive_exists() {
    let empty_dir = prepare_empty_temp_dir();
    let dir = prepare_temp_dir();

    assert!(!dotup::archive_exists(
        empty_dir.path().join(TESTING_DEPOT_NAME)
    ));
    assert!(dotup::archive_exists(dir.path().join(TESTING_DEPOT_NAME)));
}

#[test]
fn test_depot_create() {
    let empty_dir = prepare_empty_temp_dir();
    let dir = prepare_temp_dir();

    let d1 = Depot::new(DepotConfig {
        archive: Default::default(),
        archive_path: empty_dir.path().join(TESTING_DEPOT_NAME),
    });
    assert!(d1.is_err());

    let archive_path = dir.path().join(TESTING_DEPOT_NAME);
    let d2 = Depot::new(DepotConfig {
        archive: dotup::archive_read(&archive_path).unwrap(),
        archive_path,
    });
    assert!(d2.is_ok());
}

#[test]
fn test_depot_create_link() {
    let dir = prepare_temp_dir();
    let mut depot = read_depot(&dir);

    create_empty_file(dir.path().join("o3/file.txt"));

    let l1 = depot.create_link(LinkCreateParams {
        origin: PathBuf::from("o3/file.txt"),
        destination: PathBuf::from(".config/file.txt"),
    });
    assert!(l1.is_ok());

    let l2 = depot.create_link(LinkCreateParams {
        origin: PathBuf::from("o4/file.txt"),
        destination: PathBuf::from(".config/file.txt"),
    });
    assert!(l2.is_err());
}

#[test]
fn test_depot_install_uninstall_link() {
    let dir = prepare_temp_dir();
    let depot = read_depot(&dir);
    let install_base = dir.path();

    for link in depot.links() {
        depot.install_link(link, install_base).unwrap();
    }

    for link in depot.links() {
        let link_path = link.install_destination(install_base).unwrap();
        let link_target = std::fs::read_link(&link_path).unwrap();
        assert_eq!(link_target.canonicalize().unwrap(), link.origin_canonical());
    }

    for link in depot.links() {
        depot.uninstall_link(link, install_base).unwrap();
        assert!(!link.install_destination(install_base).unwrap().exists());
    }
}
