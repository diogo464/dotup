use crate::LinkInstallError;
use std::path::PathBuf;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Link origin is outside depot base\nDepot : {}\nLink : {}", .depot_base.display(), .origin.display())]
    LinkOriginOutsideDepot {
        depot_base: PathBuf,
        origin: PathBuf,
    },
    #[error("Link install error : {0}")]
    LinkInstallError(#[from] LinkInstallError),
    #[error("Link path is not relative : {}", .0.display())]
    LinkPathIsNotRelative(PathBuf),
    #[error("Link origin is not a file exist : {}", .0.display())]
    LinkOriginIsNotFile(PathBuf),
    #[error("Link origin doesnt exist : {}", .0.display())]
    LinkOriginDoesntExist(PathBuf),
    #[error("The archive path is not a file. It many not exist or there could be a permission's problem.")]
    ArchivePathNotFile(PathBuf),
    #[error("The archive path did not exist : {}\n{}", .0.display(), .1)]
    ArchiveMissing(PathBuf, std::io::Error),
    #[error("Deserialization error : {0}")]
    SerializationError(Box<dyn std::error::Error + Send + Sync + 'static>),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;
