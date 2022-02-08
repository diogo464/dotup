use anyhow::Context;
use std::{
    collections::HashSet,
    ffi::{OsStr, OsString},
    ops::Index,
    path::{Path, PathBuf},
};
use thiserror::Error;

use slotmap::{Key, SlotMap};

//pub type Result<T, E = DepotError> = std::result::Result<T, E>;
pub use anyhow::Result;
pub use disk::{read, write};

slotmap::new_key_type! {pub struct LinkID;}
slotmap::new_key_type! {struct NodeID;}

#[derive(Debug, Error)]
enum DepotError {
    #[error("path must be relative")]
    InvalidPath,
    #[error("path must be relative and not empty")]
    InvalidLinkPath,
}

#[derive(Debug, Clone)]
struct Node {
    comp: OsString,
    parent: NodeID,
    kind: NodeKind,
}

#[derive(Debug, Clone)]
enum NodeKind {
    Link(LinkID),
    Directory(HashSet<NodeID>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum NodeSearchResult {
    Found(NodeID),
    /// the closest NodeID up the the search point.
    NotFound(NodeID),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirNode {
    Link(LinkID),
    Directory(PathBuf),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SearchResult {
    Found(LinkID),
    Ancestor(LinkID),
    NotFound,
}

#[derive(Debug, Clone)]
struct Link {
    origin: PathBuf,
    destination: PathBuf,
    origin_id: NodeID,
}

#[derive(Debug)]
pub struct LinkView<'a> {
    link_id: LinkID,
    depot: &'a Depot,
}

impl<'a> LinkView<'a> {
    pub fn origin(&self) -> &Path {
        &self.depot.links[self.link_id].origin
    }

    pub fn destination(&self) -> &Path {
        &self.depot.links[self.link_id].destination
    }
}

#[derive(Debug, Clone)]
struct DepotTree {
    root: NodeID,
    nodes: SlotMap<NodeID, Node>,
}

impl Default for DepotTree {
    fn default() -> Self {
        let mut nodes = SlotMap::<NodeID, Node>::default();
        let root = nodes.insert(Node {
            comp: Default::default(),
            parent: Default::default(),
            kind: NodeKind::Directory(Default::default()),
        });
        Self { root, nodes }
    }
}

impl Index<NodeID> for DepotTree {
    type Output = Node;

    fn index(&self, index: NodeID) -> &Self::Output {
        self.nodes.index(index)
    }
}

impl DepotTree {
    /// create a node of kind [`NodeKind::Link`].
    pub fn link_create(&mut self, path: &Path, link_id: LinkID) -> Result<NodeID> {
        debug_assert!(path_verify_link(path).is_ok());

        let path_search_result = self.search(path);

        // handle the error cases
        match path_search_result {
            NodeSearchResult::Found(node_id) => {
                let node = &self.nodes[node_id];
                match &node.kind {
                    NodeKind::Link(_) => Err(anyhow::anyhow!("link already exists")),
                    NodeKind::Directory(_) => {
                        Err(anyhow::anyhow!("path already has links under it"))
                    }
                }
            }
            NodeSearchResult::NotFound(ancestor_node_id) => {
                let ancestor_node = &self.nodes[ancestor_node_id];
                match &ancestor_node.kind {
                    NodeKind::Link(_) => Err(anyhow::anyhow!(
                        "an ancestor of this path is already linked"
                    )),
                    NodeKind::Directory(_) => Ok(()),
                }
            }
        }?;

        // create the node
        // unwrap: this is a verfied link path, it must have atleast one component
        let filename = path.file_name().unwrap();
        let parent_path = path_parent_or_empty(path);
        let node_id = self.nodes.insert(Node {
            comp: filename.to_owned(),
            parent: Default::default(),
            kind: NodeKind::Link(link_id),
        });
        let parent_id = self.directory_get_or_create(parent_path, node_id);
        self.nodes[node_id].parent = parent_id;
        Ok(node_id)
    }

    pub fn link_update_id(&mut self, node_id: NodeID, link_id: LinkID) {
        let node = &mut self.nodes[node_id];
        match &mut node.kind {
            NodeKind::Link(lid) => *lid = link_id,
            NodeKind::Directory(_) => unreachable!(),
        }
    }

    /// attempts to moves a node of kind [`NodeKind::Link`] to `destination`.
    pub fn link_move(&mut self, node_id: NodeID, destination: &Path) -> Result<()> {
        let parent_id = self.nodes[node_id].parent;
        let parent = &mut self.nodes[parent_id];

        // remove the node from its parent temporarily so that the search never returns this
        // link and that way any link will find means an error.
        // if an error does happen then we re-add this node to its parent to keep the data
        // consistent.
        match &mut parent.kind {
            NodeKind::Link(_) => unreachable!(),
            NodeKind::Directory(children) => children.remove(&node_id),
        };

        let search_result = self.search(destination);
        // handle the error cases
        match search_result {
            NodeSearchResult::Found(found_id) => {
                assert!(found_id != node_id);
                self.directory_add_child(parent_id, node_id);
                return Err(anyhow::anyhow!("link already exists at that path"));
            }
            NodeSearchResult::NotFound(ancestor_id) => {
                let ancestor = &self.nodes[ancestor_id];
                match &ancestor.kind {
                    NodeKind::Link(_) => {
                        self.directory_add_child(parent_id, node_id);
                        return Err(anyhow::anyhow!("ancestor path is already linked"));
                    }
                    NodeKind::Directory(_) => {}
                }
            }
        };

        let destination_parent = path_parent_or_empty(destination);
        let new_parent_id = self.directory_get_or_create(destination_parent, node_id);
        if new_parent_id != parent_id {
            self.nodes[node_id].parent = new_parent_id;

            // we have to re-add and call the remove function because it could lead to the removal
            // of several directories if they become empty after this remove.
            self.directory_add_child(parent_id, node_id);
            self.directory_remove_child(parent_id, node_id);
        }

        // unwrap: destination is a verified link path so it has atleast 1 component
        let comp = destination.file_name().unwrap();
        let node = &mut self.nodes[node_id];
        if node.comp != comp {
            node.comp = comp.to_owned();
        }

        Ok(())
    }

    pub fn link_search(&self, path: &Path) -> SearchResult {
        match self.search(path) {
            NodeSearchResult::Found(node_id) => match &self.nodes[node_id].kind {
                NodeKind::Link(link_id) => SearchResult::Found(*link_id),
                NodeKind::Directory(_) => SearchResult::NotFound,
            },
            NodeSearchResult::NotFound(node_id) => match &self.nodes[node_id].kind {
                NodeKind::Link(link_id) => SearchResult::Ancestor(*link_id),
                NodeKind::Directory(_) => SearchResult::NotFound,
            },
        }
    }

    /// remove a node of kind [`NodeKind::Link`].
    pub fn link_remove(&mut self, node_id: NodeID) {
        let node = &self.nodes[node_id];
        assert!(std::matches!(node.kind, NodeKind::Link(_)));
        let parent_id = node.parent;
        self.nodes.remove(node_id);
        self.directory_remove_child(parent_id, node_id);
    }

    pub fn links_under(&self, path: &Path) -> impl Iterator<Item = LinkID> + '_ {
        let links = match self.search(path) {
            NodeSearchResult::Found(node_id) => {
                let node = &self.nodes[node_id];
                match &node.kind {
                    NodeKind::Link(link_id) => vec![*link_id],
                    NodeKind::Directory(children) => {
                        let mut links = Vec::new();
                        let mut node_ids = Vec::from_iter(children.iter().copied());
                        while let Some(child_id) = node_ids.pop() {
                            let child = &self.nodes[child_id];
                            match &child.kind {
                                NodeKind::Link(link_id) => links.push(*link_id),
                                NodeKind::Directory(extra_children) => {
                                    node_ids.extend(extra_children.iter().copied())
                                }
                            }
                        }
                        links
                    }
                }
            }
            NodeSearchResult::NotFound(_) => vec![],
        };
        links.into_iter()
    }

    pub fn has_links_under(&self, path: &Path) -> bool {
        // it does not matter what type of node is found. if a directory exists then there
        // must be atleast one link under it.
        match self.search(path) {
            NodeSearchResult::Found(_) => true,
            NodeSearchResult::NotFound(_) => false,
        }
    }

    pub fn read_dir(&self, path: &Path) -> Result<impl Iterator<Item = DirNode> + '_> {
        match self.search(path) {
            NodeSearchResult::Found(node_id) => match &self.nodes[node_id].kind {
                NodeKind::Link(_) => Err(anyhow::anyhow!("read dir called on a link")),
                NodeKind::Directory(children) => Ok(children.iter().map(|child_id| {
                    let child = &self.nodes[*child_id];
                    match &child.kind {
                        NodeKind::Link(link_id) => DirNode::Link(*link_id),
                        NodeKind::Directory(_) => DirNode::Directory(self.build_path(*child_id)),
                    }
                })),
            },
            NodeSearchResult::NotFound(_) => Err(anyhow::anyhow!("directory not found")),
        }
    }

    pub fn build_path(&self, node_id: NodeID) -> PathBuf {
        fn recursive_helper(nodes: &SlotMap<NodeID, Node>, nid: NodeID, pbuf: &mut PathBuf) {
            if nid.is_null() {
                return;
            }
            let parent_id = nodes[nid].parent;
            recursive_helper(nodes, parent_id, pbuf);
            pbuf.push(&nodes[nid].comp);
        }

        let mut node_path = PathBuf::default();
        recursive_helper(&self.nodes, node_id, &mut node_path);
        node_path
    }

    fn search(&self, path: &Path) -> NodeSearchResult {
        debug_assert!(path_verify(path).is_ok());

        let mut curr_node_id = self.root;
        let mut comp_iter = path_iter_comps(path).peekable();
        while let Some(comp) = comp_iter.next() {
            if let Some(child_id) = self.directory_search_children(curr_node_id, comp) {
                let child = &self.nodes[child_id];
                match &child.kind {
                    NodeKind::Link(_) => {
                        if comp_iter.peek().is_some() {
                            return NodeSearchResult::NotFound(child_id);
                        } else {
                            return NodeSearchResult::Found(child_id);
                        }
                    }
                    NodeKind::Directory(_) => curr_node_id = child_id,
                }
            } else {
                return NodeSearchResult::NotFound(curr_node_id);
            }
        }
        NodeSearchResult::Found(curr_node_id)
    }

    // creates directories all the way up to and including path.
    // there cannot be any links up to `path`.
    fn directory_get_or_create(&mut self, path: &Path, initial_child: NodeID) -> NodeID {
        // TODO: this could be replaced if the search function also returned the depth of the
        // node and we skip those components and just start creating directories up to the
        // path.
        let mut curr_node_id = self.root;
        for comp in path_iter_comps(path) {
            if let Some(child_id) = self.directory_search_children(curr_node_id, comp) {
                debug_assert!(std::matches!(
                    self.nodes[child_id].kind,
                    NodeKind::Directory(_)
                ));
                curr_node_id = child_id;
            } else {
                let new_node_id = self.nodes.insert(Node {
                    comp: comp.to_owned(),
                    parent: curr_node_id,
                    kind: NodeKind::Directory(Default::default()),
                });
                self.directory_add_child(curr_node_id, new_node_id);
                curr_node_id = new_node_id;
            }
        }
        self.directory_add_child(curr_node_id, initial_child);
        curr_node_id
    }

    fn directory_search_children(&self, node_id: NodeID, comp: &OsStr) -> Option<NodeID> {
        let node = &self.nodes[node_id];
        match &node.kind {
            NodeKind::Link(_) => unreachable!(),
            NodeKind::Directory(children) => {
                for &child_id in children {
                    let child = &self.nodes[child_id];
                    if child.comp == comp {
                        return Some(child_id);
                    }
                }
            }
        }
        None
    }

    fn directory_add_child(&mut self, node_id: NodeID, child_id: NodeID) {
        let node = &mut self.nodes[node_id];
        match &mut node.kind {
            NodeKind::Link(_) => unreachable!(),
            NodeKind::Directory(children) => children.insert(child_id),
        };
    }

    fn directory_remove_child(&mut self, node_id: NodeID, child_id: NodeID) {
        let node = &mut self.nodes[node_id];
        match &mut node.kind {
            NodeKind::Link(_) => unreachable!(),
            NodeKind::Directory(children) => {
                children.remove(&child_id);
                if children.is_empty() && !node.parent.is_null() {
                    let parent_id = node.parent;
                    self.directory_remove_child(parent_id, node_id);
                }
            }
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Depot {
    links: SlotMap<LinkID, Link>,
    origin: DepotTree,
}

impl Depot {
    pub fn link_create(
        &mut self,
        origin: impl AsRef<Path>,
        destination: impl AsRef<Path>,
    ) -> Result<LinkID> {
        let origin = origin.as_ref();
        let destination = destination.as_ref();
        path_verify_link(origin)?;
        path_verify_link(destination)?;
        self.link_create_unchecked(origin, destination)
    }

    pub fn link_remove(&mut self, link_id: LinkID) {
        let node_id = self.links[link_id].origin_id;
        self.links.remove(link_id);
        self.origin.link_remove(node_id);
    }

    /// moves the link specified by `link_id` to the path at `destination`.
    /// if the link is already at the destination nothing is done.
    /// if the destination is another link that that link is removed.
    /// if the destination is under another link then an error is returned.
    /// `destination` will be the link's new origin.
    pub fn link_move(&mut self, link_id: LinkID, destination: impl AsRef<Path>) -> Result<()> {
        let destination = destination.as_ref();
        path_verify_link(destination)?;
        self.link_move_unchecked(link_id, destination)
    }

    #[allow(unused)]
    pub fn link_search(&self, path: impl AsRef<Path>) -> Result<SearchResult> {
        let path = path.as_ref();
        path_verify(path)?;
        Ok(self.link_search_unchecked(path))
    }

    pub fn link_find(&self, path: impl AsRef<Path>) -> Result<Option<LinkID>> {
        let path = path.as_ref();
        path_verify(path)?;
        Ok(self.link_find_unchecked(path))
    }

    pub fn links_under(&self, path: impl AsRef<Path>) -> Result<impl Iterator<Item = LinkID> + '_> {
        let path = path.as_ref();
        path_verify(path)?;
        Ok(self.links_under_unchecked(path))
    }

    pub fn has_links_under(&self, path: impl AsRef<Path>) -> Result<bool> {
        let path = path.as_ref();
        path_verify(path)?;
        Ok(self.has_links_under_unchecked(path))
    }

    pub fn links_verify_install(&self, link_ids: impl Iterator<Item = LinkID>) -> Result<()> {
        let mut destination = DepotTree::default();
        for link_id in link_ids {
            let link = &self.links[link_id];
            destination
                .link_create(&link.destination, link_id)
                .context("link destinations overlap")?;
        }
        Ok(())
    }

    pub fn link_view(&self, link_id: LinkID) -> LinkView {
        LinkView {
            link_id,
            depot: self,
        }
    }

    pub fn read_dir(&self, path: impl AsRef<Path>) -> Result<impl Iterator<Item = DirNode> + '_> {
        let path = path.as_ref();
        path_verify(path)?;
        self.read_dir_unchecked(path)
    }

    fn link_create_unchecked(&mut self, origin: &Path, destination: &Path) -> Result<LinkID> {
        let node_id = self.origin.link_create(origin, Default::default())?;
        let link_id = self.links.insert(Link {
            origin: origin.to_owned(),
            destination: destination.to_owned(),
            origin_id: node_id,
        });
        self.origin.link_update_id(node_id, link_id);
        Ok(link_id)
    }

    fn link_move_unchecked(&mut self, link_id: LinkID, destination: &Path) -> Result<()> {
        let link = &self.links[link_id];
        if link.origin == destination {
            return Ok(());
        }
        let node_id = link.origin_id;
        self.origin.link_move(node_id, destination)?;
        self.links[link_id].origin = destination.to_owned();
        Ok(())
    }

    fn link_search_unchecked(&self, path: &Path) -> SearchResult {
        self.origin.link_search(path)
    }

    fn link_find_unchecked(&self, path: &Path) -> Option<LinkID> {
        match self.link_search_unchecked(path) {
            SearchResult::Found(link_id) => Some(link_id),
            _ => None,
        }
    }

    fn links_under_unchecked(&self, path: &Path) -> impl Iterator<Item = LinkID> + '_ {
        self.origin.links_under(path)
    }

    fn has_links_under_unchecked(&self, path: &Path) -> bool {
        self.origin.has_links_under(path)
    }

    fn read_dir_unchecked(&self, path: &Path) -> Result<impl Iterator<Item = DirNode> + '_> {
        self.origin.read_dir(path)
    }
}

/// a verified link path is a path that:
/// + is not empty
/// + is relative
/// + does not contain Prefix/RootDir/ParentDir
fn path_verify_link(path: &Path) -> Result<()> {
    // make sure the path is not empty
    if path.components().next().is_none() {
        return Err(DepotError::InvalidLinkPath.into());
    }
    path_verify(path).map_err(|_| DepotError::InvalidLinkPath.into())
}

/// a verified path is a path that:
/// + is not empty
/// + is relative
/// + does not contain Prefix/RootDir/ParentDir
fn path_verify(path: &Path) -> Result<()> {
    // make sure the path is relative
    // make sure the path does not contain '.' or '..'
    for component in path.components() {
        match component {
            std::path::Component::Prefix(_)
            | std::path::Component::RootDir
            | std::path::Component::CurDir
            | std::path::Component::ParentDir => return Err(DepotError::InvalidPath.into()),
            std::path::Component::Normal(_) => {}
        }
    }
    Ok(())
}

fn path_parent_or_empty(path: &Path) -> &Path {
    path.parent().unwrap_or_else(|| Path::new(""))
}

/// Iterate over the components of a path.
/// # Pre
/// The path can only have "Normal" components.
fn path_iter_comps(path: &Path) -> impl Iterator<Item = &OsStr> {
    debug_assert!(path_verify(path).is_ok());
    path.components().map(|component| match component {
        std::path::Component::Normal(comp) => comp,
        _ => unreachable!(),
    })
}

mod disk {
    use std::path::{Path, PathBuf};

    use anyhow::Context;
    use serde::{Deserialize, Serialize};

    use super::Depot;

    #[derive(Debug, Serialize, Deserialize)]
    struct DiskLink {
        origin: PathBuf,
        destination: PathBuf,
    }

    #[derive(Debug, Serialize, Deserialize)]
    struct DiskLinks {
        links: Vec<DiskLink>,
    }

    pub fn read(path: &Path) -> anyhow::Result<Depot> {
        let contents = std::fs::read_to_string(path).context("Failed to read depot file")?;
        let disk_links = toml::from_str::<DiskLinks>(&contents)
            .context("Failed to parse depot file")?
            .links;
        let mut depot = Depot::default();
        for disk_link in disk_links {
            depot
                .link_create(disk_link.origin, disk_link.destination)
                .context("Failed to build depot from file. File is in an invalid state")?;
        }
        Ok(depot)
    }

    pub fn write(path: &Path, depot: &Depot) -> anyhow::Result<()> {
        let mut links = Vec::with_capacity(depot.links.len());
        for (_, link) in depot.links.iter() {
            links.push(DiskLink {
                origin: link.origin.clone(),
                destination: link.destination.clone(),
            });
        }
        let contents =
            toml::to_string_pretty(&DiskLinks { links }).context("Failed to serialize depot")?;
        std::fs::write(path, contents).context("Failed to write depot to file")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_depot_link_create() {
        let mut depot = Depot::default();
        let f1 = depot.link_create("f1", "f1").unwrap();
        let f2 = depot.link_create("f2", "f2").unwrap();
        let f3 = depot.link_create("d1/f3", "d1/f3").unwrap();
        let f4 = depot.link_create("d1/d2/f4", "d1/d2/d4").unwrap();

        assert_eq!(depot.link_find("f1").unwrap(), Some(f1));
        assert_eq!(depot.link_find("f2").unwrap(), Some(f2));
        assert_eq!(depot.link_find("d1/f3").unwrap(), Some(f3));
        assert_eq!(depot.link_find("d1/d2/f4").unwrap(), Some(f4));

        depot.link_create("f2", "").unwrap_err();
        depot.link_create("", "d4").unwrap_err();
        depot.link_create("f1/f3", "f3").unwrap_err();
    }

    #[test]
    fn test_depot_link_remove() {
        let mut depot = Depot::default();
        let f1 = depot.link_create("d1/f1", "d1/f1").unwrap();
        let f2 = depot.link_create("d1/f2", "d1/f2").unwrap();
        let _f3 = depot.link_create("d1/f3", "d1/f3").unwrap();
        let f4 = depot.link_create("d1/d2/f4", "d2/f4").unwrap();
        let d3 = depot.link_create("d3", "d3").unwrap();

        depot.link_remove(f2);
        assert_eq!(depot.link_find("d1/f1").unwrap(), Some(f1));
        assert_eq!(depot.link_find("d1/f2").unwrap(), None);
        depot.link_remove(f4);
        assert_eq!(depot.link_find("d1/d2/f4").unwrap(), None);
        depot.link_remove(d3);
        assert_eq!(depot.link_find("d3").unwrap(), None);
    }

    #[test]
    fn test_depot_link_move() {
        let mut depot = Depot::default();
        let f1 = depot.link_create("d1/f1", "d1/f1").unwrap();
        let _f2 = depot.link_create("d1/f2", "d1/f2").unwrap();

        depot.link_move(f1, "").unwrap_err();
        depot.link_move(f1, "d1/f2/f1").unwrap_err();
        depot.link_move(f1, "d1/f2").unwrap_err();

        depot.link_move(f1, "f1").unwrap();
        assert_eq!(depot.link_view(f1).origin(), Path::new("f1"));
        depot.link_move(f1, "f2").unwrap();
        assert_eq!(depot.link_view(f1).origin(), Path::new("f2"));
        assert_eq!(depot.link_find("f2").unwrap(), Some(f1));
    }

    #[test]
    fn test_depot_link_search() {
        let mut depot = Depot::default();
        let f1 = depot.link_create("d1/f1", "d1/f1").unwrap();
        let _f2 = depot.link_create("d1/f2", "d1/f2").unwrap();
        let _f3 = depot.link_create("d1/f3", "d1/f3").unwrap();
        let _f4 = depot.link_create("d1/d2/f4", "d2/f4").unwrap();
        let _d3 = depot.link_create("d3", "d3").unwrap();

        assert_eq!(depot.link_search("d1/f1").unwrap(), SearchResult::Found(f1));
        assert_eq!(
            depot.link_search("d1/f1/f5").unwrap(),
            SearchResult::Ancestor(f1)
        );
        assert_eq!(depot.link_search("d1").unwrap(), SearchResult::NotFound);
        assert_eq!(
            depot.link_search("d1/d2/f5").unwrap(),
            SearchResult::NotFound
        );
    }

    #[test]
    fn test_depot_link_find() {
        let mut depot = Depot::default();
        let f1 = depot.link_create("d1/f1", "d1/f1").unwrap();
        let _f2 = depot.link_create("d1/f2", "d1/f2").unwrap();
        let f3 = depot.link_create("d1/f3", "d1/f3").unwrap();
        let f4 = depot.link_create("d1/d2/f4", "d2/f4").unwrap();
        let d3 = depot.link_create("d3", "d3").unwrap();

        assert_eq!(depot.link_find("d1/f1").unwrap(), Some(f1));
        assert_eq!(depot.link_find("d1/f3").unwrap(), Some(f3));
        assert_eq!(depot.link_find("d1/d2/f4").unwrap(), Some(f4));
        assert_eq!(depot.link_find("d3").unwrap(), Some(d3));

        assert_eq!(depot.link_find("d5").unwrap(), None);
        assert_eq!(depot.link_find("d3/d5").unwrap(), None);
        assert_eq!(depot.link_find("d1/d2/f5").unwrap(), None);
    }

    #[test]
    fn test_depot_links_under() {
        let mut depot = Depot::default();
        let f1 = depot.link_create("d1/f1", "d1/f1").unwrap();
        let f2 = depot.link_create("d1/f2", "d1/f2").unwrap();
        let f3 = depot.link_create("d1/f3", "d1/f3").unwrap();
        let f4 = depot.link_create("d1/d2/f4", "d2/f4").unwrap();
        let d3 = depot.link_create("d3", "d3").unwrap();

        let under_f1 = depot.links_under("d1/f1").unwrap().collect::<Vec<_>>();
        assert_eq!(under_f1, vec![f1]);

        let under_d1 = depot.links_under("d1").unwrap().collect::<Vec<_>>();
        let expected_under_d1 = vec![f1, f2, f3, f4];
        assert!(
            under_d1.len() == expected_under_d1.len()
                && expected_under_d1.iter().all(|x| under_d1.contains(x))
        );

        let under_d2 = depot.links_under("d2").unwrap().collect::<Vec<_>>();
        assert_eq!(under_d2, vec![]);

        let under_d3 = depot.links_under("d3").unwrap().collect::<Vec<_>>();
        assert_eq!(under_d3, vec![d3]);

        let under_root = depot.links_under("").unwrap().collect::<Vec<_>>();
        let expected_under_root = vec![f1, f2, f3, f4, d3];
        assert!(
            under_root.len() == expected_under_root.len()
                && expected_under_root.iter().all(|x| under_root.contains(x))
        );
    }

    #[test]
    fn test_depot_has_links_under() {
        let mut depot = Depot::default();
        let _f1 = depot.link_create("d1/f1", "d1/f1").unwrap();
        let _f2 = depot.link_create("d1/f2", "d1/f2").unwrap();
        let _f3 = depot.link_create("d1/f3", "d1/f3").unwrap();
        let _f4 = depot.link_create("d1/d2/f4", "d2/f4").unwrap();
        let _d3 = depot.link_create("d3", "d3").unwrap();

        assert!(depot.has_links_under("").unwrap());
        assert!(depot.has_links_under("d1").unwrap());
        assert!(depot.has_links_under("d3").unwrap());
        assert!(depot.has_links_under("d1/f1").unwrap());
        assert!(depot.has_links_under("d1/d2").unwrap());
        assert!(depot.has_links_under("d1/d2/f4").unwrap());

        assert!(!depot.has_links_under("d2").unwrap());
        assert!(!depot.has_links_under("d4").unwrap());
        assert!(!depot.has_links_under("d1/d2/f4/f5").unwrap());
    }

    #[test]
    fn test_depot_links_verify_install() {
        let mut depot = Depot::default();
        let f1 = depot.link_create("nvim", ".config/nvim").unwrap();
        let f2 = depot.link_create("alacritty", ".config/alacritty").unwrap();
        let f3 = depot.link_create("bash/.bashrc", ".bashrc").unwrap();
        let f4 = depot.link_create("bash_laptop/.bashrc", ".bashrc").unwrap();

        depot
            .links_verify_install(vec![f1, f2, f3].into_iter())
            .unwrap();
        depot
            .links_verify_install(vec![f1, f2, f3, f4].into_iter())
            .unwrap_err();
    }

    #[test]
    fn test_depot_read_dir() {
        let mut depot = Depot::default();
        let f1 = depot.link_create("d1/f1", "d1/f1").unwrap();
        let f2 = depot.link_create("d1/f2", "d1/f2").unwrap();
        let f3 = depot.link_create("d1/f3", "d1/f3").unwrap();
        let _f4 = depot.link_create("d1/d2/f4", "d2/f4").unwrap();
        let _d3 = depot.link_create("d3", "d3").unwrap();

        let read_dir = depot.read_dir("d1").unwrap().collect::<Vec<_>>();
        let expected_read_dir = vec![
            DirNode::Link(f1),
            DirNode::Link(f2),
            DirNode::Link(f3),
            DirNode::Directory(PathBuf::from("d1/d2")),
        ];
        assert!(
            read_dir.len() == expected_read_dir.len()
                && expected_read_dir.iter().all(|x| read_dir.contains(x))
        );
    }

    #[test]
    fn test_path_verify() {
        path_verify(Path::new("")).unwrap();
        path_verify(Path::new("f1")).unwrap();
        path_verify(Path::new("d1/f1")).unwrap();
        path_verify(Path::new("d1/f1.txt")).unwrap();
        path_verify(Path::new("d1/./f1.txt")).unwrap();

        path_verify(Path::new("/")).unwrap_err();
        path_verify(Path::new("./f1")).unwrap_err();
        path_verify(Path::new("/d1/f1")).unwrap_err();
        path_verify(Path::new("d1/../f1.txt")).unwrap_err();
        path_verify(Path::new("/d1/../f1.txt")).unwrap_err();
    }

    #[test]
    fn test_path_verify_link() {
        path_verify_link(Path::new("f1")).unwrap();
        path_verify_link(Path::new("d1/f1")).unwrap();
        path_verify_link(Path::new("d1/f1.txt")).unwrap();
        path_verify_link(Path::new("d1/./f1.txt")).unwrap();

        path_verify_link(Path::new("")).unwrap_err();
        path_verify_link(Path::new("/")).unwrap_err();
        path_verify_link(Path::new("./f1")).unwrap_err();
        path_verify_link(Path::new("/d1/f1")).unwrap_err();
        path_verify_link(Path::new("d1/../f1.txt")).unwrap_err();
        path_verify_link(Path::new("/d1/../f1.txt")).unwrap_err();
    }

    #[test]
    fn test_path_iter_comps() {
        let path = Path::new("comp1/comp2/./comp3/file.txt");
        let mut iter = path_iter_comps(path);
        assert_eq!(iter.next(), Some(OsStr::new("comp1")));
        assert_eq!(iter.next(), Some(OsStr::new("comp2")));
        assert_eq!(iter.next(), Some(OsStr::new("comp3")));
        assert_eq!(iter.next(), Some(OsStr::new("file.txt")));
        assert_eq!(iter.next(), None);
    }
}
