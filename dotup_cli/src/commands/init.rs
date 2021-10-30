use super::prelude::*;

/// Creates an empty depot file if one doesnt already exist.
///
/// By default this will create the file in the current directory
/// but the --depot flag can be used to change this path.
#[derive(Parser)]
pub struct Opts {}

pub fn main(config: Config, opts: Opts) -> anyhow::Result<()> {
    if !dotup::utils::is_file(&config.archive_path)? {
        let archive = Archive::default();
        log::info!("Creating archive at {}", &config.archive_path.display());
        utils::write_archive(&config.archive_path, &archive)?;
    } else {
        log::warn!(
            "Archive file already exists : '{}'",
            config.archive_path.display()
        );
    }
    Ok(())
}
