mod action_tree;
mod cfg;
mod paths;

use std::collections::HashSet;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use slotmap::{Key, SlotMap};
use thiserror::Error;

pub use paths::*;

type Result<T, E = Error> = std::result::Result<T, E>;

slotmap::new_key_type! { pub struct GroupID; }

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    ParseError(#[from] cfg::ParseError),
    #[error("error: {0}")]
    Custom(String),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}

#[derive(Debug, Default)]
pub struct Group {
    name: String,
    parent: GroupID,
    children: HashMap<String, GroupID>,
    actions: Vec<Action>,
}

#[derive(Debug)]
pub struct Dotup {
    root_id: GroupID,
    groups: SlotMap<GroupID, Group>,
}

#[derive(Debug, Clone, Copy)]
pub struct InstallParams<'p> {
    pub cwd: &'p Path,
    pub home: &'p Path,
}

#[derive(Debug, Clone, Copy)]
pub struct UninstallParams<'p> {
    pub cwd: &'p Path,
    pub home: &'p Path,
}

#[derive(Debug)]
struct KeyValueParser {
    location: cfg::Location,
    keyvalues: Vec<cfg::KeyValue>,
}

#[derive(Debug, Clone)]
struct IncludeAction {
    group: String,
}

#[derive(Debug, Clone)]
struct LinkAction {
    source: PathBuf,
    target: PathBuf,
}

#[derive(Debug, Clone)]
struct CopyAction {
    source: PathBuf,
    target: PathBuf,
}

#[derive(Debug, Clone)]
enum Action {
    Include(IncludeAction),
    Link(LinkAction),
    Copy(CopyAction),
}

pub fn load(content: &str) -> Result<Dotup> {
    let config = cfg::parse(content)?;
    Dotup::from_config(config)
}

pub fn load_file(path: impl AsRef<Path>) -> Result<Dotup> {
    let content = std::fs::read_to_string(path)?;
    load(&content)
}

pub fn format(content: &str) -> Result<String> {
    Ok(cfg::format(content)?)
}

pub fn format_file(path: &Path) -> Result<String> {
    let content = std::fs::read_to_string(path)?;
    format(&content)
}

pub fn format_file_inplace(path: &Path) -> Result<()> {
    let content = std::fs::read_to_string(path)?;
    let formatted = format(&content)?;
    std::fs::write(path, formatted)?;
    Ok(())
}

// -------------------- Dotup -------------------- //

impl Dotup {
    pub fn find_group_by_name(&self, name: &str) -> Option<GroupID> {
        self.find_group_by_name_rooted(self.root_id, name)
    }

    pub fn install(&self, params: InstallParams, group_id: GroupID) -> Result<()> {
        let action_tree = self.build_action_tree(params.cwd, params.home, group_id)?;
        action_tree.install()?;
        Ok(())
    }

    pub fn uninstall(&self, params: UninstallParams, group_id: GroupID) -> Result<()> {
        let action_tree = self.build_action_tree(params.cwd, params.home, group_id)?;
        action_tree.uninstall()?;
        Ok(())
    }

    pub fn status(&self, params: InstallParams, group_id: GroupID) -> Result<()> {
        let action_tree = self.build_action_tree(params.cwd, params.home, group_id)?;
        for action_id in action_tree.action_ids() {
            let prefix = if action_tree.is_installed(action_id) {
                "INSTALLED"
            } else {
                "NOT INSTALLED"
            };
            let action = action_tree.action(action_id).unwrap();
            let source = match action.action() {
                action_tree::Action::Link { ref source } => source,
                action_tree::Action::Copy { ref source } => source,
            };
            let target = action.target();
            println!("{}: {} -> {}", prefix, source.display(), target.display());
        }
        Ok(())
    }
}

impl Dotup {
    fn from_config(config: cfg::Config) -> Result<Self> {
        let mut groups = SlotMap::default();
        let root_id = groups.insert(Default::default());
        let mut dotup = Self { root_id, groups };

        for group in config.groups {
            dotup.insert_group(root_id, group)?;
        }

        Ok(dotup)
    }

    fn find_group_by_name_rooted(&self, root: GroupID, name: &str) -> Option<GroupID> {
        let trimmed = name.trim_start_matches(".");
        let rel_levels = name.len() - trimmed.len();
        let mut current = self.root_id;

        if rel_levels != 0 {
            current = root;
            for _ in 0..rel_levels - 1 {
                current = self.groups[current].parent;
                if current == self.root_id {
                    break;
                }
            }
        }

        for comp in trimmed.split(".") {
            let group = &self.groups[current];
            let child_id = group.children.get(comp)?;
            current = *child_id;
        }
        Some(current)
    }

    fn insert_group(&mut self, parent_id: GroupID, mut group_cfg: cfg::Group) -> Result<()> {
        let parent = &mut self.groups[parent_id];
        if parent.children.contains_key(&group_cfg.name) {
            return Err(Error::Custom(format!(
                "group '{}' at {} already exists",
                group_cfg.name, group_cfg.location,
            )));
        }

        let mut group = Group {
            name: group_cfg.name.clone(),
            parent: parent_id,
            children: Default::default(),
            actions: Default::default(),
        };

        for item in group_cfg
            .items
            .drain_filter(|item| std::matches!(item, cfg::GroupItem::Action(_)))
        {
            match item {
                cfg::GroupItem::Action(action) => {
                    let action = cfg_action_to_action(action)?;
                    group.actions.push(action);
                }
                _ => {}
            }
        }

        let group_id = self.groups.insert(group);
        let parent = &mut self.groups[parent_id];
        parent.children.insert(group_cfg.name, group_id);

        for item in group_cfg.items {
            match item {
                cfg::GroupItem::Group(group) => {
                    self.insert_group(group_id, group)?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn build_action_tree(
        &self,
        cwd: &Path,
        home: &Path,
        group_id: GroupID,
    ) -> Result<action_tree::ActionTree> {
        fn inner_helper(
            dotup: &Dotup,
            cwd: &AbsPath,
            home: &AbsPath,
            group_id: GroupID,
            tree: &mut action_tree::ActionTree,
            visited: &mut HashSet<GroupID>,
        ) -> Result<()> {
            if visited.contains(&group_id) {
                return Ok(());
            }
            visited.insert(group_id);

            let group = &dotup.groups[group_id];
            for action in group.actions.iter() {
                match action {
                    Action::Include(action) => {
                        let include_id = dotup
                            .find_group_by_name_rooted(group_id, &action.group)
                            .ok_or_else(|| {
                            Error::Custom(format!(
                                "group '{}' not found in include from group '{}'",
                                action.group, dotup.groups[group_id].name,
                            ))
                        })?;
                        inner_helper(dotup, cwd, home, include_id, tree, visited)?;
                    }
                    Action::Link(action) => {
                        let source = make_absolute_path(cwd, &action.source).into();
                        let target = make_absolute_path(home, &action.target);
                        tree.insert(&target, action_tree::Action::Link { source });
                    }
                    Action::Copy(action) => {
                        let source = make_absolute_path(cwd, &action.source).into();
                        let target = make_absolute_path(home, &action.target);
                        tree.insert(&target, action_tree::Action::Copy { source });
                    }
                }
            }

            Ok(())
        }

        let cwd = AbsPathBuf::try_from(
            cwd.canonicalize()
                .expect("failed to canonicalize current workind directory path"),
        )
        .unwrap();
        let home = AbsPathBuf::try_from(
            home.canonicalize()
                .expect("failed to canonicalize home directory path"),
        )
        .unwrap();

        let mut tree = action_tree::ActionTree::new();
        inner_helper(
            self,
            &cwd,
            &home,
            group_id,
            &mut tree,
            &mut Default::default(),
        )?;
        Ok(tree)
    }
}

// -------------------- KeyValueParser -------------------- //

impl KeyValueParser {
    fn new(location: cfg::Location, keyvalues: Vec<cfg::KeyValue>) -> Self {
        Self {
            location,
            keyvalues,
        }
    }

    fn get(&mut self, key: &str) -> Option<String> {
        let position = self.keyvalues.iter().position(|kv| kv.key == key)?;
        let keyvalue = self.keyvalues.swap_remove(position);
        Some(keyvalue.value)
    }

    fn expect(&mut self, key: &str) -> Result<String> {
        self.get(key)
            .ok_or_else(|| Error::Custom(format!("expected key '{}' at {}", key, self.location)))
    }

    fn finalize(&mut self) -> Result<()> {
        if let Some(kv) = self.keyvalues.pop() {
            return Err(Error::Custom(format!(
                "unexpected key '{}' at {}",
                kv.key, self.location
            )));
        }
        Ok(())
    }
}

// -------------------- Misc -------------------- //

fn cfg_action_to_action(cfg_action: cfg::Action) -> Result<Action> {
    let mut parser = KeyValueParser::new(cfg_action.location, cfg_action.keyvalues);
    match cfg_action.kind.as_str() {
        "include" => {
            let group = parser.expect("group")?;
            parser.finalize()?;
            Ok(Action::Include(IncludeAction { group }))
        }
        "link" => {
            let source = parser.expect("source")?;
            let target = parser.expect("target")?;
            parser.finalize()?;
            Ok(Action::Link(LinkAction {
                source: PathBuf::from(source),
                target: PathBuf::from(target),
            }))
        }
        "copy" => {
            let source = parser.expect("source")?;
            let target = parser.expect("target")?;
            parser.finalize()?;
            Ok(Action::Copy(CopyAction {
                source: PathBuf::from(source),
                target: PathBuf::from(target),
            }))
        }
        _ => Err(Error::Custom(format!(
            "unknown action '{}' at {}",
            cfg_action.kind, cfg_action.location
        ))),
    }
}

/// Returns `path` if it is already absolute.
/// Otherwise makes it absolute by prepending `self.root`.
fn make_absolute_path(root: &AbsPath, path: &Path) -> AbsPathBuf {
    if path.is_absolute() {
        AbsPathBuf::try_from(path).unwrap()
    } else {
        AbsPathBuf::from_rel(root, TryFrom::try_from(path).unwrap())
    }
}
