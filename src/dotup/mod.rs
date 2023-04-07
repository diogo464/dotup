mod cfg;
mod paths;

use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::{Path, PathBuf},
};

use colored::Colorize;
use slotmap::SlotMap;
use thiserror::Error;

pub use paths::*;

type Result<T, E = Error> = std::result::Result<T, E>;

slotmap::new_key_type! { pub struct GroupID; }

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    InvalidConfig(#[from] cfg::ParseError),
    #[error("error: {0}")]
    Custom(String),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}

impl Error {
    fn custom(e: impl std::fmt::Display) -> Self {
        Self::Custom(e.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    working_directory: AbsPathBuf,
    destination_directory: AbsPathBuf,
}

impl Context {
    pub fn new(
        working_directory: impl Into<PathBuf>,
        destination_directory: impl Into<PathBuf>,
    ) -> std::io::Result<Self> {
        let working_directory = working_directory.into().canonicalize()?;
        let destination_directory = destination_directory.into().canonicalize()?;
        let working_directory =
            AbsPathBuf::try_from(working_directory).map_err(std::io::Error::other)?;
        let destination_directory =
            AbsPathBuf::try_from(destination_directory).map_err(std::io::Error::other)?;
        Ok(Self {
            working_directory,
            destination_directory,
        })
    }
}

#[derive(Debug)]
pub struct InstallParams {
    pub force: bool,
}

#[allow(clippy::derivable_impls)]
impl Default for InstallParams {
    fn default() -> Self {
        Self { force: false }
    }
}

#[derive(Debug)]
pub struct Dotup {
    context: Context,
    root_id: GroupID,
    groups: SlotMap<GroupID, Group>,
}

#[derive(Debug)]
struct KeyValueParser {
    location: cfg::Location,
    keyvalues: Vec<cfg::KeyValue>,
}

#[derive(Debug, Default)]
struct Group {
    name: String,
    parent: GroupID,
    children: HashMap<String, GroupID>,
    actions: Vec<Action>,
}

#[derive(Debug, Clone)]
struct IncludeAction {
    group: String,
    group_id: GroupID,
}

#[derive(Debug, Clone)]
struct LinkAction {
    source: AbsPathBuf,
    target: AbsPathBuf,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct CopyAction {
    source: AbsPathBuf,
    target: AbsPathBuf,
}

#[derive(Debug, Clone)]
enum Action {
    Include(IncludeAction),
    Link(LinkAction),
    Copy(CopyAction),
}

#[derive(Debug, Clone)]
enum ExecutableAction {
    Link(LinkAction),
    Copy(CopyAction),
}

pub fn load(context: Context, content: &str) -> Result<Dotup> {
    let config = cfg::parse(content)?;
    new(context, config)
}

pub fn load_file(context: Context, path: impl AsRef<Path>) -> Result<Dotup> {
    let content = std::fs::read_to_string(path)?;
    load(context, &content)
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

pub fn install(dotup: &Dotup, params: &InstallParams, group: &str) -> Result<()> {
    fn prompt_overwrite(params: &InstallParams, target: &AbsPath) -> Result<bool> {
        if params.force {
            return Ok(true);
        }

        let result = inquire::Confirm::new(&format!(
            "overwrite existing file/directory '{}'?",
            target.display()
        ))
        .with_default(false)
        .with_help_message("Delete the existing file/directory")
        .prompt();

        match result {
            Ok(overwrite) => Ok(overwrite),
            Err(e) => match e {
                inquire::InquireError::NotTTY => Ok(false),
                _ => Err(Error::custom(e)),
            },
        }
    }

    let group_id = get_group_by_name(dotup, group)?;
    let executable = collect_group_executable_actions(dotup, group_id)?;

    for action in executable {
        match action {
            ExecutableAction::Link(LinkAction { source, target }) => {
                log::debug!("linking '{}' to '{}'", source.display(), target.display());
                if fs_exists(&target)? {
                    let metadata = fs_symlink_metadata(&target)?;

                    // Early return if the symlink already points to the correct source
                    if metadata.is_symlink() && fs_symlink_points_to(&target, &source)? {
                        return Ok(());
                    }

                    if !prompt_overwrite(params, &target)? {
                        return Ok(());
                    }

                    fs_remove(&target)?;
                }

                fs_create_dir_all_upto(&target)?;
                fs_create_symlink(&source, &target)?;
            }
            ExecutableAction::Copy(_) => todo!(),
        }
    }

    Ok(())
}

pub fn uninstall(dotup: &Dotup, group: &str) -> Result<()> {
    let group_id = get_group_by_name(dotup, group)?;
    let executable = collect_group_executable_actions(dotup, group_id)?;

    for action in executable {
        match action {
            ExecutableAction::Link(LinkAction { source, target }) => {
                if !fs_exists(&target)? {
                    return Ok(());
                }

                if fs_symlink_points_to(&target, &source)? {
                    fs_remove(&target)?;
                }
            }
            ExecutableAction::Copy(_) => todo!(),
        }
    }

    Ok(())
}

pub fn status(dotup: &Dotup, group: &str) -> Result<()> {
    fn display_status(dotup: &Dotup, group_id: GroupID, depth: u32) -> Result<()> {
        let group = &dotup.groups[group_id];

        println!("{}{}", "  ".repeat(depth as usize), group.name.blue());
        log::trace!("displaying status for group '{}'", group.name);

        for action in group.actions.iter() {
            match action {
                Action::Include(include) => {
                    log::trace!("displaying status for included group '{}'", include.group);
                    display_status(dotup, include.group_id, depth + 1)?;
                }
                Action::Link(link) => {
                    log::trace!("displaying status for link '{}'", link.target.display());

                    let target = link.target.display();
                    let source = link.source.display();
                    let installed = is_link_installed(link)?;
                    let output = format!(
                        "{}{} -> {}",
                        "  ".repeat(depth as usize + 1),
                        target,
                        source
                    );
                    println!(
                        "{}",
                        if installed {
                            output.green()
                        } else {
                            output.red()
                        }
                    );
                }
                Action::Copy(_) => todo!(),
            }
        }
        Ok(())
    }

    let group_id = get_group_by_name(dotup, group)?;
    display_status(dotup, group_id, 0)
}

fn new(context: Context, config: cfg::Config) -> Result<Dotup> {
    let mut groups = SlotMap::default();
    let root_id = groups.insert(Default::default());
    let mut dotup = Dotup {
        context,
        root_id,
        groups,
    };

    for group_cfg in config.groups {
        insert_config_group(&mut dotup, root_id, group_cfg)?;
    }

    resolve_includes(&mut dotup)?;

    Ok(dotup)
}

fn insert_config_group(
    dotup: &mut Dotup,
    parent_id: GroupID,
    mut group_cfg: cfg::Group,
) -> Result<()> {
    let parent = &mut dotup.groups[parent_id];
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
        if let cfg::GroupItem::Action(action) = item {
            let action = convert_config_action(&dotup.context, action)?;
            group.actions.push(action);
        }
    }

    let group_id = dotup.groups.insert(group);
    let parent = &mut dotup.groups[parent_id];
    parent.children.insert(group_cfg.name, group_id);

    for item in group_cfg.items {
        if let cfg::GroupItem::Group(group) = item {
            insert_config_group(dotup, group_id, group)?;
        }
    }

    Ok(())
}

fn resolve_includes(dotup: &mut Dotup) -> Result<()> {
    struct Patch {
        group_id: GroupID,
        action_idx: usize,
        target_id: GroupID,
    }

    let mut patches = Vec::new();
    for group_id in dotup.groups.keys() {
        for idx in 0..dotup.groups[group_id].actions.len() {
            let action = &dotup.groups[group_id].actions[idx];
            let target = match action {
                Action::Include(include) => include.group.as_str(),
                _ => continue,
            };

            let target_id = match find_group_by_name_rooted(dotup, group_id, target) {
                Some(target_id) => target_id,
                None => {
                    return Err(Error::Custom(format!("group '{}' not found", target)));
                }
            };

            patches.push(Patch {
                group_id,
                action_idx: idx,
                target_id,
            });
        }
    }

    for patch in patches {
        let group = &mut dotup.groups[patch.group_id];
        let action = &mut group.actions[patch.action_idx];
        if let Action::Include(include) = action {
            include.group_id = patch.target_id;
        }
    }

    Ok(())
}

fn convert_config_action(context: &Context, cfg_action: cfg::Action) -> Result<Action> {
    let mut parser = KeyValueParser::new(cfg_action.location, cfg_action.keyvalues);
    match cfg_action.kind.as_str() {
        "include" => {
            let group = parser.expect("group")?;
            parser.finalize()?;
            Ok(Action::Include(IncludeAction {
                group,
                group_id: Default::default(),
            }))
        }
        "link" => {
            let source = PathBuf::from(parser.expect("source")?);
            let target = PathBuf::from(parser.expect("target")?);
            parser.finalize()?;
            Ok(Action::Link(LinkAction {
                source: make_path_absolute(&context.working_directory, &source),
                target: make_path_absolute(&context.destination_directory, &target),
            }))
        }
        "copy" => {
            let source = PathBuf::from(parser.expect("source")?);
            let target = PathBuf::from(parser.expect("target")?);
            parser.finalize()?;
            Ok(Action::Copy(CopyAction {
                source: make_path_absolute(&context.working_directory, &source),
                target: make_path_absolute(&context.destination_directory, &target),
            }))
        }
        _ => Err(Error::Custom(format!(
            "unknown action '{}' at {}",
            cfg_action.kind, cfg_action.location
        ))),
    }
}

fn get_group_by_name(dotup: &Dotup, name: &str) -> Result<GroupID> {
    find_group_by_name(dotup, name)
        .ok_or_else(|| Error::Custom(format!("group '{}' not found", name,)))
}

fn find_group_by_name(dotup: &Dotup, name: &str) -> Option<GroupID> {
    find_group_by_name_rooted(dotup, dotup.root_id, name)
}

fn find_group_by_name_rooted(dotup: &Dotup, root: GroupID, name: &str) -> Option<GroupID> {
    let trimmed = name.trim_start_matches('.');
    let rel_levels = name.len() - trimmed.len();
    let mut current = dotup.root_id;

    if rel_levels != 0 {
        current = root;
        for _ in 0..rel_levels - 1 {
            current = dotup.groups[current].parent;
            if current == dotup.root_id {
                break;
            }
        }
    }

    for comp in trimmed.split('.') {
        let group = &dotup.groups[current];
        let child_id = group.children.get(comp)?;
        current = *child_id;
    }
    Some(current)
}

fn collect_group_executable_actions(
    dotup: &Dotup,
    group_id: GroupID,
) -> Result<Vec<ExecutableAction>> {
    let mut executable = Vec::new();
    let mut visited = HashSet::new();
    let mut queue = VecDeque::from_iter(std::iter::once(group_id));

    while let Some(group_id) = queue.pop_front() {
        if !visited.insert(group_id) {
            continue;
        }

        let group = &dotup.groups[group_id];
        for action in &group.actions {
            match action {
                Action::Include(include) => {
                    queue.push_back(include.group_id);
                }
                Action::Link(action) => executable.push(ExecutableAction::Link(action.clone())),
                Action::Copy(action) => executable.push(ExecutableAction::Copy(action.clone())),
            }
        }
    }

    Ok(executable)
}

fn is_link_installed(link: &LinkAction) -> Result<bool> {
    if !fs_exists(&link.target)? {
        Ok(false)
    } else {
        fs_symlink_points_to(&link.target, &link.source)
    }
}

fn make_path_absolute(root: &AbsPath, path: &Path) -> AbsPathBuf {
    if path.is_absolute() {
        AbsPathBuf::try_from(path.to_owned()).unwrap()
    } else {
        root.join(path)
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

// -------------------- Filesystem -------------------- //

fn fs_exists(path: impl AsRef<Path>) -> Result<bool> {
    path.as_ref().try_exists().map_err(|err| {
        Error::Custom(format!(
            "failed to check existence of target '{}': {}",
            path.as_ref().display(),
            err
        ))
    })
}

#[allow(unused)]
fn fs_metadata(path: impl AsRef<Path>) -> Result<std::fs::Metadata> {
    let path = path.as_ref();
    std::fs::metadata(path).map_err(|err| {
        Error::Custom(format!(
            "failed to get metadata of target '{}': {}",
            path.display(),
            err
        ))
    })
}

fn fs_symlink_metadata(path: impl AsRef<Path>) -> Result<std::fs::Metadata> {
    let path = path.as_ref();
    std::fs::symlink_metadata(path).map_err(|err| {
        Error::Custom(format!(
            "failed to get metadata of target '{}': {}",
            path.display(),
            err
        ))
    })
}

fn fs_read_symlink(path: impl AsRef<Path>) -> Result<PathBuf> {
    let path = path.as_ref();
    std::fs::read_link(path).map_err(|err| {
        Error::Custom(format!(
            "failed to read symlink '{}': {}",
            path.display(),
            err
        ))
    })
}

fn fs_canonicalize(path: impl AsRef<Path>) -> Result<PathBuf> {
    let path = path.as_ref();
    path.canonicalize().map_err(|err| {
        Error::Custom(format!(
            "failed to canonicalize path '{}': {}",
            path.display(),
            err
        ))
    })
}

fn fs_symlink_points_to(path: impl AsRef<Path>, target: impl AsRef<Path>) -> Result<bool> {
    let path = path.as_ref();
    let target = target.as_ref();
    let link_target = fs_read_symlink(path)?;
    let target_canonical = fs_canonicalize(target)?;
    let link_target_canonical = fs_canonicalize(link_target)?;
    Ok(target_canonical == link_target_canonical)
}

fn fs_remove(path: impl AsRef<Path>) -> Result<()> {
    let path = path.as_ref();
    log::debug!("removing target '{}'", path.display());

    if !fs_exists(path)? {
        return Ok(());
    }

    let metadata = fs_symlink_metadata(path)?;
    if metadata.is_dir() {
        std::fs::remove_dir_all(path).map_err(|err| {
            Error::Custom(format!(
                "failed to remove target '{}': {}",
                path.display(),
                err
            ))
        })
    } else {
        std::fs::remove_file(path).map_err(|err| {
            Error::Custom(format!(
                "failed to remove target '{}': {}",
                path.display(),
                err
            ))
        })
    }
}

fn fs_create_symlink(source: impl AsRef<Path>, target: impl AsRef<Path>) -> Result<()> {
    let source = source.as_ref();
    let target = target.as_ref();
    log::debug!(
        "creating symlink '{}' -> '{}'",
        target.display(),
        source.display()
    );
    std::os::unix::fs::symlink(source, target).map_err(|err| {
        Error::Custom(format!(
            "failed to create symlink '{}' -> '{}': {}",
            target.display(),
            source.display(),
            err
        ))
    })
}

fn fs_create_dir_all_upto(path: impl AsRef<Path>) -> Result<()> {
    let path = path.as_ref();
    let parent = path.parent().ok_or_else(|| {
        Error::Custom(format!("failed to get parent of path '{}'", path.display()))
    })?;
    std::fs::create_dir_all(parent).map_err(|err| {
        Error::Custom(format!(
            "failed to create directory '{}': {}",
            parent.display(),
            err
        ))
    })
}
