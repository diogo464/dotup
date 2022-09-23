use std::{
    borrow::Borrow,
    convert::TryFrom,
    ffi::OsStr,
    ops::Deref,
    path::{Component, Components, Display, Path, PathBuf},
};

use thiserror::Error;

#[derive(Debug, Error)]
#[error("invalid relative path")]
pub struct InvalidRelPath;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RelPathBuf(PathBuf);

#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RelPath(Path);

#[derive(Debug, Error)]
#[error("invalid absolute path")]
pub struct InvalidAbsPath;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbsPathBuf(PathBuf);

#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct AbsPath(Path);

pub struct AbsComponents<'p> {
    inner: std::path::Components<'p>,
}

impl<'p> Iterator for AbsComponents<'p> {
    type Item = &'p OsStr;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.inner.next()? {
                Component::RootDir => continue,
                Component::Normal(p) => break Some(p),
                _ => unreachable!(),
            }
        }
    }
}

// -------------------- RelPathBuf -------------------- //

impl From<RelPathBuf> for PathBuf {
    fn from(path: RelPathBuf) -> Self {
        path.0
    }
}

impl AsRef<Path> for RelPathBuf {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}

impl Deref for RelPathBuf {
    type Target = RelPath;

    fn deref(&self) -> &Self::Target {
        TryFrom::try_from(self.0.as_path()).unwrap()
    }
}

impl TryFrom<&Path> for RelPathBuf {
    type Error = InvalidRelPath;

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        if path.is_relative() {
            Ok(Self(path.to_owned()))
        } else {
            Err(InvalidRelPath)
        }
    }
}

impl Borrow<RelPath> for RelPathBuf {
    fn borrow(&self) -> &RelPath {
        self.deref()
    }
}

impl RelPathBuf {}

// -------------------- RelPath -------------------- //

impl ToOwned for RelPath {
    type Owned = RelPathBuf;

    fn to_owned(&self) -> Self::Owned {
        RelPathBuf(self.0.to_owned())
    }
}

impl<'p> AsRef<Path> for &'p RelPath {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}

impl<'p> TryFrom<&'p Path> for &'p RelPath {
    type Error = InvalidRelPath;

    fn try_from(value: &'p Path) -> Result<Self, Self::Error> {
        if value.is_relative() {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(InvalidRelPath)
        }
    }
}

impl RelPath {
    pub fn components(&self) -> Components {
        self.0.components()
    }

    pub fn display(&self) -> Display {
        self.0.display()
    }
}

// -------------------- AbsPathBuf -------------------- //

impl Default for AbsPathBuf {
    fn default() -> Self {
        Self(PathBuf::from("/"))
    }
}

impl From<AbsPathBuf> for PathBuf {
    fn from(p: AbsPathBuf) -> Self {
        p.0
    }
}

impl AsRef<Path> for AbsPathBuf {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}

impl AsRef<AbsPath> for AbsPathBuf {
    fn as_ref(&self) -> &AbsPath {
        AbsPath::new(&self.0)
    }
}

impl Deref for AbsPathBuf {
    type Target = AbsPath;

    fn deref(&self) -> &Self::Target {
        TryFrom::try_from(self.0.as_path()).unwrap()
    }
}

impl TryFrom<&Path> for AbsPathBuf {
    type Error = InvalidAbsPath;

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        if path.is_absolute() {
            Ok(Self(path.to_owned()))
        } else {
            Err(InvalidAbsPath)
        }
    }
}

impl TryFrom<PathBuf> for AbsPathBuf {
    type Error = InvalidAbsPath;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        if path.is_absolute() {
            Ok(Self(path))
        } else {
            Err(InvalidAbsPath)
        }
    }
}

impl Borrow<AbsPath> for AbsPathBuf {
    fn borrow(&self) -> &AbsPath {
        self.deref()
    }
}

impl AbsPathBuf {
    pub fn from_rel(root: &AbsPath, rel: &RelPath) -> Self {
        let p = weakly_canonical_cwd(rel, root.0.to_path_buf());
        Self::try_from(p).unwrap()
    }

    pub fn as_path(&self) -> &AbsPath {
        TryFrom::try_from(self.0.as_path()).unwrap()
    }
}

// -------------------- AbsPath -------------------- //

impl<'p> AsRef<Path> for &'p AbsPath {
    fn as_ref(&self) -> &'p Path {
        self.0.as_ref()
    }
}

impl ToOwned for AbsPath {
    type Owned = AbsPathBuf;

    fn to_owned(&self) -> Self::Owned {
        AbsPathBuf(self.0.to_owned())
    }
}

impl<'p> Default for &'p AbsPath {
    fn default() -> Self {
        Self::try_from(Path::new("/")).unwrap()
    }
}

impl<'p> TryFrom<&'p Path> for &'p AbsPath {
    type Error = InvalidAbsPath;

    fn try_from(value: &'p Path) -> Result<Self, Self::Error> {
        if value.is_absolute() {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(InvalidAbsPath)
        }
    }
}

impl<'p> TryFrom<&'p str> for &'p AbsPath {
    type Error = InvalidAbsPath;

    fn try_from(value: &'p str) -> Result<Self, Self::Error> {
        TryFrom::try_from(Path::new(value))
    }
}

impl AbsPath {
    pub fn new(path: &Path) -> &Self {
        TryFrom::try_from(path).unwrap()
    }

    pub fn join(&self, other: impl AsRef<Path>) -> AbsPathBuf {
        AbsPathBuf::try_from(weakly_canonical_cwd(other, self.0.to_path_buf())).unwrap()
    }

    pub fn parent(&self) -> Option<&AbsPath> {
        self.0.parent().map(|p| TryFrom::try_from(p).unwrap())
    }

    pub fn components(&self) -> AbsComponents {
        AbsComponents {
            inner: self.0.components(),
        }
    }

    pub fn display(&self) -> Display {
        self.0.display()
    }
}

// -------------------- Utils -------------------- //

pub fn current_working_directory() -> PathBuf {
    std::env::current_dir().expect("Failed to obtain current working directory")
}

pub fn weakly_canonical(path: impl AsRef<Path>) -> PathBuf {
    let cwd = current_working_directory();
    weakly_canonical_cwd(path, cwd)
}

pub fn weakly_canonical_cwd(path: impl AsRef<Path>, cwd: PathBuf) -> PathBuf {
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

pub fn ends_with_slash(path: impl AsRef<Path>) -> bool {
    path.as_ref()
        .to_str()
        .map(|s| s.ends_with('/'))
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_weakly_canonical() {
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

    #[test]
    fn test_path_ends_with_slash() {
        assert!(!ends_with_slash(""));
        assert!(!ends_with_slash("/f1"));
        assert!(!ends_with_slash("/f1/f2"));
        assert!(!ends_with_slash("./f1/f2"));
        assert!(!ends_with_slash("./f1/f2/../f3"));

        assert!(ends_with_slash("/"));
        assert!(ends_with_slash("/f1/"));
        assert!(ends_with_slash("f1/"));
        assert!(ends_with_slash("f1/f2/"));
        assert!(ends_with_slash("f1/f2/../f3/"));
    }
}
