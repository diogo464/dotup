use std::path::PathBuf;

#[derive(Debug)]
pub struct Config {
    pub archive_path: PathBuf,
    pub install_path: PathBuf,
}
