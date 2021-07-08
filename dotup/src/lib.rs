mod archive;
mod depot;
mod error;

pub mod utils;

pub use archive::{
    archive_deserialize, archive_exists, archive_read, archive_serialize, archive_write, Archive,
    ArchiveLink,
};
pub use depot::{Depot, DepotConfig, Link, LinkDesc, LinkID, LinkInstallError};
pub use error::{Error, Result};

pub(crate) mod internal_prelude {
    pub use super::{utils, Error, Result};
}
