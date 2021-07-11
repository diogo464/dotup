use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

use crate::internal_prelude::*;

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArchiveLink {
    pub origin: PathBuf,
    pub destination: PathBuf,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Archive {
    pub links: Vec<ArchiveLink>,
}

pub fn archive_exists(path: impl AsRef<Path>) -> bool {
    utils::is_file(path).unwrap_or_default()
}

pub fn archive_read(path: impl AsRef<Path>) -> Result<Archive> {
    let contents = std::fs::read_to_string(path)?;
    archive_deserialize(contents)
}

pub fn archive_write(path: impl AsRef<Path>, archive: &Archive) -> Result<()> {
    let serialized = archive_serialize(archive)?;
    std::fs::write(path, &serialized)?;
    Ok(())
}

pub fn archive_serialize(archive: &Archive) -> Result<String> {
    match toml::to_string_pretty(archive) {
        Ok(serialized) => Ok(serialized),
        Err(e) => Err(Error::SerializationError(Box::new(e))),
    }
}

pub fn archive_deserialize(contents: impl AsRef<str>) -> Result<Archive> {
    match toml::from_str(contents.as_ref()) {
        Ok(archive) => Ok(archive),
        Err(e) => Err(Error::SerializationError(Box::new(e))),
    }
}
