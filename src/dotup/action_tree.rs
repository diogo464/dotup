use std::{collections::HashSet, ffi::OsString, ops::Index, path::PathBuf};

use slotmap::SlotMap;

use super::{AbsPath, AbsPathBuf};

slotmap::new_key_type! {
    pub struct NodeID;
    pub struct ActionID;
}

#[derive(Debug)]
pub enum Action {
    Link { source: PathBuf },
    Copy { source: PathBuf },
}

#[derive(Debug)]
pub struct TreeAction {
    path: AbsPathBuf,
    action: Action,
}

#[derive(Debug)]
enum TreeNodeKind {
    Action(ActionID),
    SubTree(HashSet<NodeID>),
}

#[derive(Debug)]
struct TreeNode {
    path: AbsPathBuf,
    component: OsString,
    kind: TreeNodeKind,
}

#[derive(Debug)]
pub struct ActionTree {
    root_id: NodeID,
    nodes: SlotMap<NodeID, TreeNode>,
    actions: SlotMap<ActionID, TreeAction>,
}

// -------------------- TreeAction -------------------- //

impl TreeAction {
    pub fn target(&self) -> &AbsPath {
        &self.path
    }

    pub fn action(&self) -> &Action {
        &self.action
    }
}

// -------------------- TreeNodeKind -------------------- //

impl TreeNodeKind {
    fn as_action(&self) -> ActionID {
        match self {
            Self::Action(id) => *id,
            _ => unreachable!(),
        }
    }

    fn as_action_mut(&mut self) -> &mut ActionID {
        match self {
            Self::Action(id) => id,
            _ => unreachable!(),
        }
    }

    fn as_subtree(&self) -> &HashSet<NodeID> {
        match self {
            Self::SubTree(ids) => ids,
            _ => unreachable!(),
        }
    }

    fn as_subtree_mut(&mut self) -> &mut HashSet<NodeID> {
        match self {
            Self::SubTree(ids) => ids,
            _ => unreachable!(),
        }
    }
}

// -------------------- ActionTree -------------------- //

impl Index<ActionID> for ActionTree {
    type Output = TreeAction;

    fn index(&self, index: ActionID) -> &Self::Output {
        self.action(index).unwrap()
    }
}

impl ActionTree {
    pub fn new() -> Self {
        let mut nodes = SlotMap::with_key();
        let root_id = nodes.insert(TreeNode {
            path: AbsPathBuf::default(),
            component: OsString::new(),
            kind: TreeNodeKind::SubTree(Default::default()),
        });

        Self {
            root_id,
            nodes,
            actions: Default::default(),
        }
    }

    pub fn insert(&mut self, target: &AbsPath, action: Action) -> ActionID {
        let action_id = self.actions.insert(TreeAction {
            path: target.to_owned(),
            action,
        });
        self.force_insert_at(&target, TreeNodeKind::Action(action_id));
        action_id
    }

    pub fn install(&self) -> std::io::Result<()> {
        for action_id in self.action_ids() {
            self.install_action(action_id)?;
        }
        Ok(())
    }

    pub fn is_installed(&self, action_id: ActionID) -> bool {
        let action = &self.actions[action_id];
        let target = action.target();
        match action.action() {
            Action::Link { source } => {
                let link = match std::fs::read_link(target) {
                    Ok(link) => link,
                    Err(_) => return false,
                };
                link.canonicalize().unwrap() == source.canonicalize().unwrap()
            }
            Action::Copy { .. } => target.as_ref().exists(),
        }
    }

    pub fn uninstall(&self) -> std::io::Result<()> {
        for action_id in self.action_ids() {
            self.uninstall_action(action_id)?;
        }
        Ok(())
    }

    pub fn install_action(&self, action_id: ActionID) -> std::io::Result<()> {
        let action = &self[action_id];
        match &action.action {
            Action::Link { source } => {
                let target = action.target();
                log::info!("Linking {:?} -> {:?}", source, target);
                if target.as_ref().is_symlink() {
                    log::trace!("{:?} is a symlink, removing it", target);
                    std::fs::remove_file(target)?;
                }
                if let Some(parent) = target.parent() {
                    log::trace!("creating all directories up to {:?}", parent);
                    std::fs::create_dir_all(parent.as_ref())?;
                }
                log::trace!("creating symlink {:?} -> {:?}", source, target);
                std::os::unix::fs::symlink(source, target)?;
            }
            Action::Copy { source } => todo!(),
        }
        Ok(())
    }

    pub fn uninstall_action(&self, action_id: ActionID) -> std::io::Result<()> {
        let action = &self[action_id];
        if let Action::Link { ref source } = action.action {
            let target = action.target();
            if target.as_ref().is_symlink() {
                log::trace!("{:?} is a symlink", target);
                let symlink_target = std::fs::read_link(target.as_ref())?;
                if symlink_target == *source {
                    log::info!("symlink target is {:?}, removing it", source);
                    std::fs::remove_file(target)?;
                } else {
                    log::trace!(
                        "symlink target is {:?}, not {:?}, not removing it",
                        symlink_target,
                        source
                    );
                }
            }
        }
        Ok(())
    }

    pub fn actions(&self) -> impl Iterator<Item = &TreeAction> {
        self.actions.values()
    }

    pub fn action_ids(&self) -> impl Iterator<Item = ActionID> + '_ {
        self.actions.keys()
    }

    pub fn action(&self, action_id: ActionID) -> Option<&TreeAction> {
        self.actions.get(action_id)
    }

    /// Creates all nodes up to the given path.
    /// If one of the nodes is an action node, it will be replaced with a subtree node.
    fn force_insert_at(&mut self, target: &AbsPath, kind: TreeNodeKind) -> NodeID {
        let mut curr = self.root_id;
        for comp in target.components() {
            {
                // Try to find node if it exists
                let curr_node = &mut self.nodes[curr];
                match curr_node.kind {
                    TreeNodeKind::Action(action) => {
                        self.actions.remove(action);
                        curr_node.kind = TreeNodeKind::SubTree(Default::default());
                        match curr_node.kind {
                            TreeNodeKind::SubTree(ref mut children) => children,
                            _ => unreachable!(),
                        }
                    }
                    TreeNodeKind::SubTree(ref mut children) => children,
                };

                let children = self.nodes[curr].kind.as_subtree();
                for &child_id in children.iter() {
                    let child_node = &self.nodes[child_id];
                    if child_node.component == comp {
                        curr = child_id;
                        break;
                    }
                }
            }
            {
                // Create new node
                let new_node = TreeNode {
                    path: self.nodes[curr].path.join(comp),
                    component: comp.to_owned(),
                    kind: TreeNodeKind::SubTree(Default::default()),
                };
                let new_id = self.nodes.insert(new_node);
                match &mut self.nodes[curr].kind {
                    TreeNodeKind::SubTree(children) => children.insert(new_id),
                    _ => unreachable!(),
                };
                curr = new_id;
            }
        }
        let prev_kind = std::mem::replace(&mut self.nodes[curr].kind, kind);
        match prev_kind {
            TreeNodeKind::SubTree(children) => {
                for &child in children.iter() {
                    self.remove_node(child);
                }
            }
            _ => {}
        }
        curr
    }

    /// Removes the given node.
    /// Does not remove it from the parent's children node.
    fn remove_node(&mut self, node_id: NodeID) {
        let node = self
            .nodes
            .remove(node_id)
            .expect("Node being removed does not exist");
        match node.kind {
            TreeNodeKind::Action(action) => {
                self.actions.remove(action);
            }
            TreeNodeKind::SubTree(children) => {
                for child in children {
                    self.remove_node(child);
                }
            }
        };
    }
}

#[cfg(test)]
mod tests {
    use std::{convert::TryFrom, path::Path};

    use super::*;

    #[test]
    fn empty_tree() {
        let _ = ActionTree::new();
    }

    #[test]
    fn single_action() {
        let mut tree = ActionTree::new();

        let action_id = tree.insert(
            TryFrom::try_from("/home/user/.config/nvim").unwrap(),
            Action::Link {
                source: PathBuf::from("nvim"),
            },
        );

        let action = &tree[action_id];
        assert_eq!(
            action.path.as_path(),
            AbsPath::new(Path::new("/home/user/.config/nvim"))
        );
    }

    #[test]
    fn subtree_replacement() {
        let mut tree = ActionTree::new();

        let action_id = tree.insert(
            TryFrom::try_from("/home/user/.config/nvim").unwrap(),
            Action::Link {
                source: PathBuf::from("nvim"),
            },
        );
        let action_id_original = action_id;

        let action = &tree[action_id];
        assert_eq!(
            action.path.as_path(),
            AbsPath::new(Path::new("/home/user/.config/nvim"))
        );

        let action_id = tree.insert(
            TryFrom::try_from("/home/user/.config/nvim/init.vim").unwrap(),
            Action::Link {
                source: PathBuf::from("nvim/init.vim"),
            },
        );

        let action = &tree[action_id];
        assert_eq!(
            action.path.as_path(),
            AbsPath::new(Path::new("/home/user/.config/nvim/init.vim"))
        );

        eprintln!("{:#?}", tree);
        assert!(tree.action(action_id_original).is_none());
    }
}
