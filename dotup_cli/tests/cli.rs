use assert_cmd::{assert::Assert, prelude::*};
use dotup::ArchiveLink;
use std::{
    path::{Path, PathBuf},
    process::Command,
};
use tempfile::TempDir;

const DEPOT_FILE_NAME: &str = "depot.toml";
const BIN_NAME: &str = "dotup";

fn create_empty_file(path: impl AsRef<Path>) {
    let path = path.as_ref();
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).unwrap();
    }
    std::fs::write(path, "").unwrap();
}

fn prepare_command(dir: &TempDir) -> Command {
    let mut cmd = Command::cargo_bin(BIN_NAME).unwrap();
    cmd.current_dir(dir.path());
    cmd
}

fn run_command(dir: &TempDir, cmd: &str) -> Assert {
    let mut c = prepare_command(dir);
    c.current_dir(dir.path());
    c.args(cmd.split_whitespace());
    c.assert()
}

fn prepare_dir() -> TempDir {
    let dir = TempDir::new().unwrap();
    create_empty_file(dir.path().join("o1/file.txt"));
    create_empty_file(dir.path().join("o1/dir/file.txt"));
    create_empty_file(dir.path().join("o2/file1.txt"));
    create_empty_file(dir.path().join("o2/file2.txt"));
    dir
}

#[test]
fn test_cli_init() {
    let dir = prepare_dir();
    let assert = run_command(&dir, "init");

    assert.success().code(0);
    assert!(dir.path().join(DEPOT_FILE_NAME).is_file());
}

#[test]
fn test_cli_link() {
    let dir = prepare_dir();
    run_command(&dir, "init").success();

    let assert = run_command(&dir, "link o1 .config");
    assert.success().code(0);

    let assert = run_command(&dir, "link --directory o2 .scripts");
    assert.success().code(0);

    let archive = dotup::archive_read(dir.path().join(DEPOT_FILE_NAME)).unwrap();
    let link1 = ArchiveLink {
        origin: PathBuf::from("o1/file.txt"),
        destination: PathBuf::from(".config/file.txt"),
    };
    let link2 = ArchiveLink {
        origin: PathBuf::from("o1/dir/file.txt"),
        destination: PathBuf::from(".config/dir/file.txt"),
    };
    let link3 = ArchiveLink {
        origin: PathBuf::from("o2"),
        destination: PathBuf::from(".scripts"),
    };

    assert!(archive.links.contains(&link1));
    assert!(archive.links.contains(&link2));
    assert!(archive.links.contains(&link3));
}

#[test]
fn test_cli_install_uninstall_unlink() {
    let dir = prepare_dir();
    run_command(&dir, "init").success();
    run_command(&dir, "link o1 .config").success();
    run_command(&dir, "link --directory o2 .scripts").success();

    let install_dir = TempDir::new().unwrap();
    let install_base = format!("{}", install_dir.path().display());
    run_command(
        &dir,
        &format!("install --install-base {} o1 o2", install_base),
    )
    .success();

    assert_eq!(
        std::fs::read_link(install_dir.path().join(".config/file.txt")).unwrap(),
        dir.path().join("o1/file.txt")
    );
    assert_eq!(
        std::fs::read_link(install_dir.path().join(".config/dir/file.txt")).unwrap(),
        dir.path().join("o1/dir/file.txt")
    );
    assert_eq!(
        std::fs::read_link(install_dir.path().join(".scripts")).unwrap(),
        dir.path().join("o2")
    );

    run_command(
        &dir,
        &format!("uninstall --install-base {} o1/file.txt", install_base),
    )
    .success();
    assert!(!install_dir.path().join(".config/file.txt").exists());
    assert!(install_dir.path().join(".config/dir/file.txt").exists());
    assert!(install_dir.path().join(".scripts").exists());

    run_command(
        &dir,
        &format!("uninstall --install-base {} o1", install_base),
    )
    .success();
    assert!(!install_dir.path().join(".config/file.txt").exists());
    assert!(!install_dir.path().join(".config/dir/file.txt").exists());
    assert!(install_dir.path().join(".scripts").exists());

    assert_eq!(
        3,
        dotup::archive_read(dir.path().join(DEPOT_FILE_NAME))
            .unwrap()
            .links
            .len()
    );

    run_command(&dir, &format!("unlink --uninstall {} o2", install_base)).success();
    assert!(!install_dir.path().join(".scripts").exists());

    assert_eq!(
        2,
        dotup::archive_read(dir.path().join(DEPOT_FILE_NAME))
            .unwrap()
            .links
            .len()
    );
}
