use std::{
    collections::VecDeque,
    path::{Component, Path, PathBuf},
};

use crate::{
    dotup::{self, Dotup},
    Flags,
};

pub const DEFAULT_DEPOT_FILE_NAME: &str = ".depot";

/// Returns a list of canonical paths to all the files in `dir`. This includes files in
/// subdirectories.
/// Fails if dir isnt a directory or if there is some other io error.
pub fn collect_files_in_dir_recursive(dir: impl Into<PathBuf>) -> anyhow::Result<Vec<PathBuf>> {
    let mut paths = Vec::new();
    let mut dirs = VecDeque::new();
    dirs.push_back(dir.into());

    while let Some(dir) = dirs.pop_front() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let filetype = entry.file_type()?;
            if filetype.is_dir() {
                dirs.push_back(entry.path());
            } else {
                paths.push(entry.path());
            }
        }
    }

    Ok(paths)
}

pub fn collect_paths_in_dir(dir: impl AsRef<Path>) -> anyhow::Result<Vec<PathBuf>> {
    Ok(std::fs::read_dir(dir)?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .collect())
}

pub fn read_dotup(flags: &Flags) -> anyhow::Result<Dotup> {
    let depot_path = depot_path_from_flags(flags)?;
    let install_base = install_base_from_flags(flags);
    dotup::read(depot_path, install_base)
}

pub fn write_dotup(dotup: &Dotup) -> anyhow::Result<()> {
    dotup::write(dotup)
}

pub fn depot_path_from_flags(flags: &Flags) -> anyhow::Result<PathBuf> {
    match flags.depot {
        Some(ref path) => Ok(path.clone()),
        None => find_depot_path().ok_or_else(|| anyhow::anyhow!("Failed to find depot path")),
    }
}

pub fn default_depot_path() -> PathBuf {
    current_working_directory().join(DEFAULT_DEPOT_FILE_NAME)
}

pub fn find_depot_path() -> Option<PathBuf> {
    let mut cwd = current_working_directory();
    loop {
        let path = cwd.join(DEFAULT_DEPOT_FILE_NAME);
        if path.exists() {
            break Some(path);
        }
        if !cwd.pop() {
            break None;
        }
    }
}

pub fn install_base_from_flags(flags: &Flags) -> PathBuf {
    match flags.install_base {
        Some(ref path) => path.clone(),
        None => default_install_base(),
    }
}

pub fn default_install_base() -> PathBuf {
    PathBuf::from(std::env::var("HOME").expect("Failed to obtain HOME environment variable"))
}
pub fn weakly_canonical(path: impl AsRef<Path>) -> PathBuf {
    let cwd = current_working_directory();
    weakly_canonical_cwd(path, cwd)
}

fn weakly_canonical_cwd(path: impl AsRef<Path>, cwd: PathBuf) -> PathBuf {
    // Adapated from
    // https://github.com/rust-lang/cargo/blob/fede83ccf973457de319ba6fa0e36ead454d2e20/src/cargo/util/paths.rs#L61
    let path = path.as_ref();

    let mut components = path.components().peekable();
    let mut canonical = cwd;
    let prefix = if let Some(c @ Component::Prefix(..)) = components.peek().cloned() {
        components.next();
        PathBuf::from(c.as_os_str())
    } else {
        PathBuf::new()
    };

    for component in components {
        match component {
            Component::Prefix(_) => unreachable!(),
            Component::RootDir => {
                canonical = prefix.clone();
                canonical.push(component.as_os_str())
            }
            Component::CurDir => {}
            Component::ParentDir => {
                canonical.pop();
            }
            Component::Normal(p) => canonical.push(p),
        };
    }

    canonical
}

pub fn current_working_directory() -> PathBuf {
    std::env::current_dir().expect("Failed to obtain current working directory")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn weak_canonical_test() {
        let cwd = PathBuf::from("/home/user");
        assert_eq!(
            PathBuf::from("/home/dest"),
            weakly_canonical_cwd("../dest", cwd.clone())
        );
        assert_eq!(
            PathBuf::from("/home/dest/configs/init.vim"),
            weakly_canonical_cwd("../dest/configs/init.vim", cwd.clone())
        );
        assert_eq!(
            PathBuf::from("/dest/configs/init.vim"),
            weakly_canonical_cwd("/dest/configs/init.vim", cwd.clone())
        );
        assert_eq!(
            PathBuf::from("/home/user/configs/nvim/lua/setup.lua"),
            weakly_canonical_cwd("./configs/nvim/lua/setup.lua", cwd.clone())
        );
        assert_eq!(
            PathBuf::from("/home/user/configs/nvim/lua/setup.lua"),
            weakly_canonical_cwd("configs/nvim/lua/setup.lua", cwd)
        );
    }
}
