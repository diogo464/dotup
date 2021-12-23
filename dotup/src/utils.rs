use std::path::{Component, Path, PathBuf};

pub fn is_file(path: impl AsRef<Path>) -> std::io::Result<bool> {
    let metadata = match std::fs::metadata(path) {
        Ok(metadata) => metadata,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => return Ok(false),
            _ => return Err(e),
        },
    };
    Ok(metadata.is_file())
}

pub fn is_directory(path: impl AsRef<Path>) -> std::io::Result<bool> {
    let metadata = match std::fs::metadata(path) {
        Ok(metadata) => metadata,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => return Ok(false),
            _ => return Err(e),
        },
    };
    Ok(metadata.is_dir())
}

pub fn is_canonical(path: &Path) -> bool {
    path == weakly_canonical(path).as_path()
}

pub fn weakly_canonical(path: impl AsRef<Path>) -> PathBuf {
    let cwd = std::env::current_dir().expect("Failed to obtain current directory");
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
            weakly_canonical_cwd("configs/nvim/lua/setup.lua", cwd.clone())
        );
    }
}
