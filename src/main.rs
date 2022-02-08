#![feature(try_blocks)]

// TODO: rewrite all errors so they start with lower case

mod depot {
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
    pub enum NodeSearchResult {
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
                            NodeKind::Directory(_) => {
                                DirNode::Directory(self.build_path(*child_id))
                            }
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

        pub fn links_under(
            &self,
            path: impl AsRef<Path>,
        ) -> Result<impl Iterator<Item = LinkID> + '_> {
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

        pub fn read_dir(
            &self,
            path: impl AsRef<Path>,
        ) -> Result<impl Iterator<Item = DirNode> + '_> {
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
        path.parent().unwrap_or(Path::new(""))
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
            let contents = toml::to_string_pretty(&DiskLinks { links })
                .context("Failed to serialize depot")?;
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
}

pub mod dotup {
    use std::{
        cmp::Ordering,
        collections::HashSet,
        path::{Path, PathBuf},
    };

    use ansi_term::Color;
    use anyhow::Context;

    use crate::{
        depot::{self, Depot, DirNode, LinkID},
        utils,
    };

    #[derive(Debug)]
    struct CanonicalPair {
        link_id: LinkID,
        origin: PathBuf,
        destination: PathBuf,
    }

    #[derive(Debug, Clone)]
    enum StatusItem {
        Link {
            origin: PathBuf,
            destination: PathBuf,
            is_directory: bool,
        },
        Directory {
            origin: PathBuf,
            items: Vec<StatusItem>,
        },
        Unlinked {
            origin: PathBuf,
            is_directory: bool,
        },
    }

    impl StatusItem {
        fn display_ord_cmp(&self, other: &Self) -> Ordering {
            match (self, other) {
                (
                    StatusItem::Link {
                        origin: l_origin, ..
                    },
                    StatusItem::Link {
                        origin: r_origin, ..
                    },
                ) => l_origin.cmp(r_origin),
                (StatusItem::Link { .. }, StatusItem::Directory { .. }) => Ordering::Less,
                (
                    StatusItem::Link {
                        is_directory: l_is_dir,
                        ..
                    },
                    StatusItem::Unlinked {
                        is_directory: u_is_dir,
                        ..
                    },
                ) => {
                    if *u_is_dir && !*l_is_dir {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                }
                (StatusItem::Directory { .. }, StatusItem::Link { .. }) => Ordering::Greater,
                (
                    StatusItem::Directory {
                        origin: l_origin, ..
                    },
                    StatusItem::Directory {
                        origin: r_origin, ..
                    },
                ) => l_origin.cmp(r_origin),
                (StatusItem::Directory { .. }, StatusItem::Unlinked { .. }) => Ordering::Greater,
                (
                    StatusItem::Unlinked {
                        is_directory: u_is_dir,
                        ..
                    },
                    StatusItem::Link {
                        is_directory: l_is_dir,
                        ..
                    },
                ) => {
                    if *u_is_dir && !*l_is_dir {
                        Ordering::Greater
                    } else {
                        Ordering::Less
                    }
                }
                (StatusItem::Unlinked { .. }, StatusItem::Directory { .. }) => Ordering::Less,
                (
                    StatusItem::Unlinked {
                        origin: l_origin, ..
                    },
                    StatusItem::Unlinked {
                        origin: r_origin, ..
                    },
                ) => l_origin.cmp(r_origin),
            }
        }
    }

    #[derive(Debug)]
    pub struct Dotup {
        depot: Depot,
        depot_dir: PathBuf,
        depot_path: PathBuf,
        install_base: PathBuf,
    }

    impl Dotup {
        fn new(depot: Depot, depot_path: PathBuf, install_base: PathBuf) -> anyhow::Result<Self> {
            assert!(depot_path.is_absolute());
            assert!(depot_path.is_file());
            assert!(install_base.is_absolute());
            assert!(install_base.is_dir());
            let depot_dir = {
                let mut d = depot_path.clone();
                d.pop();
                d
            };
            Ok(Self {
                depot,
                depot_dir,
                depot_path,
                install_base,
            })
        }

        pub fn link(&mut self, origin: impl AsRef<Path>, destination: impl AsRef<Path>) {
            let link_result: anyhow::Result<()> = try {
                let origin = self.prepare_relative_path(origin.as_ref())?;
                let destination = destination.as_ref();
                self.depot.link_create(origin, destination)?;
            };
            match link_result {
                Ok(_) => {}
                Err(e) => println!("Failed to create link : {e}"),
            }
        }

        pub fn unlink(&mut self, paths: impl Iterator<Item = impl AsRef<Path>>, uninstall: bool) {
            for origin in paths {
                let unlink_result: anyhow::Result<()> = try {
                    let origin = self.prepare_relative_path(origin.as_ref())?;
                    let links_under: Vec<_> = self.depot.links_under(&origin)?.collect();
                    for link_id in links_under {
                        if uninstall && self.symlink_is_installed_by_link_id(link_id)? {
                            self.symlink_uninstall_by_link_id(link_id)?;
                        }
                        self.depot.link_remove(link_id);
                    }
                };
                match unlink_result {
                    Ok(_) => {}
                    Err(e) => println!("Failed to unlink {} : {e}", origin.as_ref().display()),
                }
            }
        }

        pub fn install(&self, paths: impl Iterator<Item = impl AsRef<Path>>) {
            let install_result: anyhow::Result<()> = try {
                let mut link_ids = HashSet::<LinkID>::default();
                for path in paths {
                    let path = self.prepare_relative_path(path.as_ref())?;
                    link_ids.extend(self.depot.links_under(&path)?);
                }
                self.depot.links_verify_install(link_ids.iter().copied())?;

                for link_id in link_ids {
                    self.symlink_install_by_link_id(link_id)?;
                }
            };
            if let Err(e) = install_result {
                println!("error while installing : {e}");
            }
        }

        pub fn uninstall(&self, paths: impl Iterator<Item = impl AsRef<Path>>) {
            for origin in paths {
                let uninstall_result: anyhow::Result<()> = try {
                    let origin = self.prepare_relative_path(origin.as_ref())?;
                    let canonical_pairs = self.canonical_pairs_under(&origin)?;
                    for pair in canonical_pairs {
                        self.symlink_uninstall(&pair.origin, &pair.destination)?;
                    }
                };
                if let Err(e) = uninstall_result {
                    println!(
                        "error while uninstalling {} : {e}",
                        origin.as_ref().display()
                    );
                }
            }
        }

        pub fn mv(
            &mut self,
            origins: impl Iterator<Item = impl AsRef<Path>>,
            destination: impl AsRef<Path>,
        ) {
            let origins = {
                let mut v = Vec::new();
                for origin in origins {
                    match self.prepare_relative_path(origin.as_ref()) {
                        Ok(origin) => v.push(origin),
                        Err(e) => {
                            println!("invalid link {} : {e}", origin.as_ref().display());
                            return;
                        }
                    }
                }
                v
            };
            let destination = destination.as_ref();

            // if we are moving multiple links then the destination must be a directory
            if origins.len() > 1 && destination.is_dir() {
                println!("destination must be a directory");
                return;
            }

            for origin in origins {
                if let Err(e) = self.mv_one(&origin, destination) {
                    println!("error moving link {} : {e}", origin.display());
                }
            }
        }

        fn mv_one(&mut self, origin: &Path, destination: &Path) -> anyhow::Result<()> {
            let link_id = match self.depot.link_find(origin)? {
                Some(link_id) => link_id,
                None => {
                    return Err(anyhow::anyhow!(format!(
                        "{} is not a link",
                        origin.display()
                    )))
                }
            };
            let is_installed = self.symlink_is_installed_by_link_id(link_id)?;
            let original_origin = self.depot.link_view(link_id).origin().to_owned();
            self.depot.link_move(link_id, destination)?;
            // move the actual file on disk
            if let Err(e) = std::fs::rename(origin, destination).context("Failed to move file") {
                // unwrap: moving the link back to its origin place has to work
                self.depot.link_move(link_id, original_origin).unwrap();
                return Err(e);
            }
            // reinstall because we just moved the origin
            if is_installed {
                self.symlink_install_by_link_id(link_id)
                    .context("failed to reinstall link while moving")?;
            }
            Ok(())
        }

        pub fn status(&self) {
            let status_result: anyhow::Result<()> = try {
                let canonical_dir = utils::current_working_directory();
                let item = self.status_path_to_item(&canonical_dir)?;
                self.status_print_item(item, 0)?;
            };
            if let Err(e) = status_result {
                println!("error while displaying status : {e}");
            }
        }
        fn status_path_to_item(&self, canonical_path: &Path) -> anyhow::Result<StatusItem> {
            debug_assert!(canonical_path.is_absolute());
            debug_assert!(canonical_path.exists());
            let relative_path = self.prepare_relative_path(&canonical_path)?;

            let item = if canonical_path.is_dir() {
                if let Some(link_id) = self.depot.link_find(&relative_path)? {
                    let destination = self.depot.link_view(link_id).destination().to_owned();
                    StatusItem::Link {
                        origin: relative_path,
                        destination,
                        is_directory: true,
                    }
                } else if self.depot.has_links_under(&relative_path)? {
                    let mut items = Vec::new();
                    let mut collected_rel_paths = HashSet::<PathBuf>::new();
                    let directory_paths = utils::collect_paths_in_dir(&canonical_path)?;
                    for canonical_item_path in directory_paths {
                        let item = self.status_path_to_item(&canonical_item_path)?;
                        match &item {
                            StatusItem::Link { origin, .. }
                            | StatusItem::Directory { origin, .. } => {
                                collected_rel_paths.insert(origin.to_owned());
                            }
                            _ => {}
                        }
                        items.push(item);
                    }

                    for dir_node in self.depot.read_dir(&relative_path)? {
                        match dir_node {
                            DirNode::Link(link_id) => {
                                let link_view = self.depot.link_view(link_id);
                                let link_rel_path = link_view.origin();
                                let link_rel_dest = link_view.destination();
                                if !collected_rel_paths.contains(link_rel_path) {
                                    items.push(StatusItem::Link {
                                        origin: link_rel_path.to_owned(),
                                        destination: link_rel_dest.to_owned(),
                                        is_directory: false,
                                    });
                                }
                            }
                            DirNode::Directory(_) => {}
                        }
                    }

                    StatusItem::Directory {
                        origin: relative_path,
                        items,
                    }
                } else {
                    StatusItem::Unlinked {
                        origin: relative_path,
                        is_directory: true,
                    }
                }
            } else {
                if let Some(link_id) = self.depot.link_find(&relative_path)? {
                    let destination = self.depot.link_view(link_id).destination().to_owned();
                    StatusItem::Link {
                        origin: relative_path,
                        destination,
                        is_directory: false,
                    }
                } else {
                    StatusItem::Unlinked {
                        origin: relative_path,
                        is_directory: false,
                    }
                }
            };
            Ok(item)
        }
        fn status_print_item(&self, item: StatusItem, depth: u32) -> anyhow::Result<()> {
            fn print_depth(d: u32) {
                for _ in 0..d.saturating_sub(1) {
                    print!("    ");
                }
            }
            fn origin_color(exists: bool, is_installed: bool) -> Color {
                if !exists {
                    return Color::Red;
                } else if is_installed {
                    Color::Green
                } else {
                    Color::RGB(255, 127, 0)
                }
            }

            let destination_color = Color::Blue;

            print_depth(depth);
            match item {
                StatusItem::Link {
                    origin,
                    destination,
                    is_directory,
                } => {
                    let canonical_origin = self.depot_dir.join(&origin);
                    let canonical_destination = self.install_base.join(&destination);
                    let file_name = Self::status_get_filename(&canonical_origin);
                    let is_installed =
                        self.symlink_is_installed(&canonical_origin, &canonical_destination)?;
                    let exists = canonical_origin.exists();
                    let origin_color = origin_color(exists, is_installed);
                    let directory_extra = if is_directory { "/" } else { "" };
                    println!(
                        "{}{} -> {}",
                        origin_color.paint(file_name),
                        directory_extra,
                        destination_color.paint(destination.display().to_string())
                    );
                }
                StatusItem::Directory { origin, mut items } => {
                    items.sort_by(|a, b| StatusItem::display_ord_cmp(a, b).reverse());
                    let directory_name = Self::status_get_filename(&origin);
                    if depth != 0 {
                        println!("{}/", directory_name);
                    }
                    for item in items {
                        self.status_print_item(item, depth + 1)?;
                    }
                }
                StatusItem::Unlinked {
                    origin,
                    is_directory,
                } => {
                    let file_name = Self::status_get_filename(&origin);
                    let directory_extra = if is_directory { "/" } else { "" };
                    println!("{}{}", file_name, directory_extra);
                }
            }
            Ok(())
        }
        fn status_get_filename(path: &Path) -> &str {
            path.file_name()
                .and_then(|s| s.to_str())
                .unwrap_or_default()
        }

        fn prepare_relative_path(&self, origin: &Path) -> anyhow::Result<PathBuf> {
            let canonical = utils::weakly_canonical(origin);
            let relative = canonical
                .strip_prefix(&self.depot_dir)
                .context("Invalid origin path, not under depot directory")?;
            Ok(relative.to_owned())
        }

        // returns the canonical pairs for all links under `path`.
        fn canonical_pairs_under(&self, path: &Path) -> anyhow::Result<Vec<CanonicalPair>> {
            let origin = self.prepare_relative_path(path)?;
            let mut canonical_pairs = Vec::new();
            for link_id in self.depot.links_under(origin)? {
                canonical_pairs.push(self.canonical_pair_from_link_id(link_id));
            }
            Ok(canonical_pairs)
        }

        fn symlink_is_installed_by_link_id(&self, link_id: LinkID) -> anyhow::Result<bool> {
            let canonical_pair = self.canonical_pair_from_link_id(link_id);
            self.symlink_is_installed(&canonical_pair.origin, &canonical_pair.destination)
        }

        fn symlink_is_installed(&self, origin: &Path, destination: &Path) -> anyhow::Result<bool> {
            debug_assert!(origin.is_absolute());
            debug_assert!(destination.is_absolute());

            if destination.is_symlink() {
                let symlink_destination = destination.read_link()?;
                match symlink_destination.canonicalize() {
                    Ok(canonicalized) => Ok(origin == canonicalized),
                    Err(_) => Ok(false),
                }
            } else {
                Ok(false)
            }
        }

        fn symlink_install_by_link_id(&self, link_id: LinkID) -> anyhow::Result<()> {
            let canonical_pair = self.canonical_pair_from_link_id(link_id);
            self.symlink_install(&canonical_pair.origin, &canonical_pair.destination)
        }

        fn symlink_install(&self, origin: &Path, destination: &Path) -> anyhow::Result<()> {
            debug_assert!(origin.is_absolute());
            debug_assert!(destination.is_absolute());
            log::debug!(
                "symlink_install : {} -> {}",
                origin.display(),
                destination.display()
            );

            let destination_parent = destination
                .parent()
                .ok_or_else(|| anyhow::anyhow!("destination has no parent component"))?;
            std::fs::create_dir_all(destination_parent).context("Failed to create directories")?;
            // need to do this beacause if the destination path ends in '/' because the symlink
            // functions will treat it as a directory but we want a file with that name.
            let destination = destination.with_file_name(destination.file_name().unwrap());

            let destination_exists = destination.exists();
            let destination_is_symlink = destination.is_symlink();

            if destination_exists && !destination_is_symlink {
                return Err(anyhow::anyhow!("destination already exists"));
            }

            if destination_is_symlink {
                log::debug!("symlink already exists, removing before recreating");
                std::fs::remove_file(&destination)?;
            }

            log::debug!(
                "creating filesystem symlink {} -> {}",
                origin.display(),
                destination.display()
            );
            std::os::unix::fs::symlink(origin, destination).context("failed to create symlink")?;

            Ok(())
        }

        fn symlink_uninstall(&self, origin: &Path, destination: &Path) -> anyhow::Result<()> {
            debug_assert!(origin.is_absolute());
            debug_assert!(destination.is_absolute());
            let destination = destination.with_file_name(destination.file_name().unwrap());

            if destination.is_symlink() {
                let symlink_destination = destination.read_link()?.canonicalize()?;
                if symlink_destination == origin {
                    std::fs::remove_file(&destination)?;
                }
            }

            Ok(())
        }

        fn symlink_uninstall_by_link_id(&self, link_id: LinkID) -> anyhow::Result<()> {
            let canonical_pair = self.canonical_pair_from_link_id(link_id);
            self.symlink_uninstall(&canonical_pair.origin, &canonical_pair.destination)
        }

        fn canonical_pair_from_link_id(&self, link_id: LinkID) -> CanonicalPair {
            let link_view = self.depot.link_view(link_id);
            let relative_origin = link_view.origin();
            let relative_destination = link_view.destination();
            let canonical_origin = self.depot_dir.join(relative_origin);
            let canonical_destination = self.install_base.join(relative_destination);
            CanonicalPair {
                link_id,
                origin: canonical_origin,
                destination: canonical_destination,
            }
        }
    }

    pub fn read(depot_path: PathBuf, install_base: PathBuf) -> anyhow::Result<Dotup> {
        let depot_path = depot_path
            .canonicalize()
            .context("Failed to canonicalize depot path")?;
        let install_base = install_base
            .canonicalize()
            .context("Failed to canonicalize install base")?;
        if !install_base.is_dir() {
            return Err(anyhow::anyhow!("Install base must be a directory"));
        }
        let depot = depot::read(&depot_path)?;
        Dotup::new(depot, depot_path, install_base)
    }

    pub fn write(dotup: &Dotup) -> anyhow::Result<()> {
        depot::write(&dotup.depot_path, &dotup.depot)?;
        Ok(())
    }
}

mod utils {
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
                weakly_canonical_cwd("configs/nvim/lua/setup.lua", cwd.clone())
            );
        }
    }
}

use std::path::PathBuf;

use clap::Parser;
use flexi_logger::Logger;
use utils::DEFAULT_DEPOT_FILE_NAME;

#[derive(Parser, Debug)]
pub struct Flags {
    #[clap(long)]
    depot: Option<PathBuf>,
    #[clap(long)]
    install_base: Option<PathBuf>,
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// A level of verbosity, and can be used multiple times
    ///
    /// Level 1 - Info
    ///
    /// Level 2 - Debug
    ///
    /// Level 3 - Trace
    #[clap(short, long, parse(from_occurrences))]
    verbose: i32,

    #[clap(flatten)]
    flags: Flags,

    #[clap(subcommand)]
    command: SubCommand,
}

#[derive(Parser, Debug)]
enum SubCommand {
    Init(InitArgs),
    Link(LinkArgs),
    Unlink(UnlinkArgs),
    Install(InstallArgs),
    Uninstall(UninstallArgs),
    Mv(MvArgs),
    Status(StatusArgs),
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let log_level = match args.verbose {
        0 => "warn",
        1 => "info",
        2 => "debug",
        _ => "trace",
    };
    let log_level = "trace";

    Logger::try_with_env_or_str(log_level)?
        .format(flexi_logger::colored_default_format)
        .set_palette("196;208;32;198;15".to_string())
        .start()?;

    match args.command {
        SubCommand::Init(cmd_args) => command_init(args.flags, cmd_args),
        SubCommand::Link(cmd_args) => command_link(args.flags, cmd_args),
        SubCommand::Unlink(cmd_args) => command_unlink(args.flags, cmd_args),
        SubCommand::Install(cmd_args) => command_install(args.flags, cmd_args),
        SubCommand::Uninstall(cmd_args) => command_uninstall(args.flags, cmd_args),
        SubCommand::Mv(cmd_args) => command_mv(args.flags, cmd_args),
        SubCommand::Status(cmd_args) => command_status(args.flags, cmd_args),
    }
}

#[derive(Parser, Debug)]
struct InitArgs {
    path: Option<PathBuf>,
}

fn command_init(_global_flags: Flags, args: InitArgs) -> anyhow::Result<()> {
    let depot_path = {
        let mut path = args.path.unwrap_or_else(utils::default_depot_path);
        if path.is_dir() {
            path = path.join(DEFAULT_DEPOT_FILE_NAME);
        }
        path
    };

    if depot_path.exists() {
        println!("Depot at {} already exists", depot_path.display());
    } else {
        depot::write(&depot_path, &Default::default())?;
        println!("Depot initialized at {}", depot_path.display());
    }

    Ok(())
}

#[derive(Parser, Debug)]
struct LinkArgs {
    #[clap(long)]
    directory: bool,

    origin: PathBuf,

    destination: PathBuf,
}

fn command_link(global_flags: Flags, args: LinkArgs) -> anyhow::Result<()> {
    let mut dotup = utils::read_dotup(&global_flags)?;
    let origins = if args.directory {
        vec![args.origin]
    } else {
        if args.origin.is_dir() {
            utils::collect_files_in_dir_recursive(args.origin)?
        } else {
            vec![args.origin]
        }
    };
    for origin in origins {
        dotup.link(origin, &args.destination);
    }
    utils::write_dotup(&dotup)?;
    Ok(())
}

#[derive(Parser, Debug)]
struct UnlinkArgs {
    #[clap(long)]
    uninstall: bool,

    paths: Vec<PathBuf>,
}

fn command_unlink(global_flags: Flags, args: UnlinkArgs) -> anyhow::Result<()> {
    let mut dotup = utils::read_dotup(&global_flags)?;
    dotup.unlink(args.paths.into_iter(), args.uninstall);
    utils::write_dotup(&dotup)?;
    Ok(())
}

#[derive(Parser, Debug)]
struct InstallArgs {
    #[clap(long)]
    directory: bool,

    paths: Vec<PathBuf>,
}

fn command_install(global_flags: Flags, args: InstallArgs) -> anyhow::Result<()> {
    let dotup = utils::read_dotup(&global_flags)?;
    dotup.install(args.paths.into_iter());
    Ok(())
}

#[derive(Parser, Debug)]
struct UninstallArgs {
    paths: Vec<PathBuf>,
}

fn command_uninstall(global_flags: Flags, args: UninstallArgs) -> anyhow::Result<()> {
    let dotup = utils::read_dotup(&global_flags)?;
    dotup.uninstall(args.paths.into_iter());
    Ok(())
}

#[derive(Parser, Debug)]
struct MvArgs {
    paths: Vec<PathBuf>,
}

fn command_mv(global_flags: Flags, args: MvArgs) -> anyhow::Result<()> {
    let mut dotup = utils::read_dotup(&global_flags)?;
    let mut paths = args.paths;
    if paths.len() < 2 {
        return Err(anyhow::anyhow!("mv requires atleast 2 arguments"));
    }
    let to = paths.pop().unwrap();
    let from = paths;
    dotup.mv(from.iter(), &to);
    utils::write_dotup(&dotup)?;
    Ok(())
}

#[derive(Parser, Debug)]
struct StatusArgs {}

fn command_status(global_flags: Flags, _args: StatusArgs) -> anyhow::Result<()> {
    let dotup = utils::read_dotup(&global_flags)?;
    dotup.status();
    Ok(())
}
