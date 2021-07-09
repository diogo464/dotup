use crate::{LinkCreateError, LinkInstallError};
use std::path::PathBuf;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Link install error : {0}")]
    LinkInstallError(#[from] LinkInstallError),
    #[error("Link create error : {0}")]
    LinkCreateError(#[from] LinkCreateError),
    #[error("Link origin doesnt exist : {}", .0.display())]
    ArchivePathNotFile(PathBuf),
    #[error("The archive path did not exist : {}\n{}", .0.display(), .1)]
    ArchiveMissing(PathBuf, std::io::Error),
    #[error("Deserialization error : {0}")]
    SerializationError(Box<dyn std::error::Error + Send + Sync + 'static>),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;
