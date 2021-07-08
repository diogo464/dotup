use clap::Clap;

use super::prelude::*;

#[derive(Clap)]
pub struct Opts {}

pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    if !dotup::utils::is_file(&config.archive_path)? {
        let archive = Archive::default();
        log::info!("Creating archive");
        utils::write_archive(&config.archive_path, &archive)?;
    } else {
        log::info!(
            "Archive file already exists : {}",
            config.archive_path.display()
        );
    }
    Ok(())
}
