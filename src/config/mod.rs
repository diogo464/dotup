mod parse;

use std::path::Path;

pub struct Config {
    groups: Vec<Group>,
}

pub struct Group {
    includes: Vec<IncludeAction>,
    links: Vec<LinkAction>,
    copies: Vec<CopyAction>,
}

pub struct IncludeAction {
    group: String,
}

pub struct LinkAction {
    source: String,
    target: String,
}

pub struct CopyAction {
    source: String,
    target: String,
}

pub fn parse(content: &str) -> std::io::Result<Config> {
    todo!()
}

pub fn parse_path(path: impl AsRef<Path>) -> std::io::Result<Config> {
    todo!()
}

pub fn format(content: &str) -> std::io::Result<String> {
    todo!()
}

pub fn format_path(path: impl AsRef<Path>) -> std::io::Result<String> {
    todo!()
}

pub fn format_inplace(path: impl AsRef<Path>) -> std::io::Result<()> {
    todo!()
}

impl Config {
    pub fn groups(&self) -> impl Iterator<Item = &Group> {
        std::iter::empty()
    }

    pub fn group(&self, name: &str) -> Option<&Group> {
        todo!()
    }
}

impl Group {
    pub fn groups(&self) -> impl Iterator<Item = &IncludeAction> {
        std::iter::empty()
    }

    pub fn links(&self) -> impl Iterator<Item = &LinkAction> {
        std::iter::empty()
    }

    pub fn copies(&self) -> impl Iterator<Item = &CopyAction> {
        std::iter::empty()
    }
}

impl IncludeAction {
    pub fn name(&self) -> &str {
        todo!()
    }
}

impl LinkAction {
    pub fn source(&self) -> &str {
        todo!()
    }

    pub fn dest(&self) -> &str {
        todo!()
    }
}

impl CopyAction {
    pub fn source(&self) -> &str {
        todo!()
    }

    pub fn dest(&self) -> &str {
        todo!()
    }
}
