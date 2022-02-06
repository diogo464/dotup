#![feature(try_blocks)]
pub mod depot {
    use std::{
        collections::HashSet,
        ffi::{OsStr, OsString},
        ops::Deref,
        path::{Path, PathBuf},
    };

    use slotmap::SlotMap;

    pub use disk::{read, write};

    slotmap::new_key_type! {pub struct LinkID;}
    slotmap::new_key_type! {struct NodeID;}

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum SearchResult {
        Found(LinkID),
        Ancestor(LinkID),
        NotFound,
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

    // wrapper for a path under the depot
    // this path is relative and does not contain `..` or similar
    // Deref(Path)
    struct DepotPath(PathBuf);
    impl Deref for DepotPath {
        type Target = Path;

        fn deref(&self) -> &Self::Target {
            self.0.deref()
        }
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

    #[derive(Debug, Clone)]
    struct Link {
        origin: PathBuf,
        destination: PathBuf,
        node_id: NodeID,
    }

    #[derive(Debug, Clone)]
    pub struct Depot {
        links: SlotMap<LinkID, Link>,
        nodes: SlotMap<NodeID, Node>,
        root: NodeID,
    }

    impl Default for Depot {
        fn default() -> Self {
            let mut nodes = SlotMap::default();
            let root = nodes.insert(Node {
                comp: Default::default(),
                parent: Default::default(),
                kind: NodeKind::Directory(Default::default()),
            });
            Self {
                links: Default::default(),
                nodes,
                root,
            }
        }
    }

    impl Depot {
        pub fn create(
            &mut self,
            origin: impl AsRef<Path>,
            destination: impl AsRef<Path>,
        ) -> anyhow::Result<LinkID> {
            let origin = origin.as_ref();
            let destination = destination.as_ref();
            verify_path(origin)?;
            verify_path(destination)?;

            // example
            // origin = fish/config.fish
            // destination = .config/fish/config.fish

            // search
            // if ancestor - return error
            // if found - update destination
            // if not found - create

            match self.search_unchecked(&origin) {
                SearchResult::Found(link_id) => {
                    let link = &mut self.links[link_id];
                    link.destination = destination.to_owned();
                    Ok(link_id)
                }
                SearchResult::Ancestor(_) => Err(anyhow::anyhow!(
                    "An ancestor of this path is already linked"
                )),
                SearchResult::NotFound => {
                    let link_id = self.links.insert(Link {
                        origin: origin.to_owned(),
                        destination: destination.to_owned(),
                        node_id: Default::default(),
                    });
                    let node_id = self.node_create_link(origin, link_id);
                    self.links[link_id].node_id = node_id;
                    Ok(link_id)
                }
            }
        }

        pub fn move_link(
            &mut self,
            link_id: LinkID,
            destination: impl AsRef<Path>,
        ) -> anyhow::Result<()> {
            let destination = destination.as_ref();
            verify_path(destination)?;

            let link_node_id = self.links[link_id].node_id;
            let link_parent_node_id = self.nodes[link_node_id].parent;
            let (node_id, found) = self.node_search(destination);

            // the link is already at the destination
            if found && node_id == link_node_id {
                return Ok(());
            }

            if found {
                let node = &self.nodes[node_id];
                match &node.kind {
                    NodeKind::Link(node_link_id) => {
                        let node_parent_id = node.parent;
                        let node_link_id = *node_link_id;
                        assert_ne!(link_id, node_link_id);
                        self.remove(node_link_id);
                        self.node_child_remove(link_parent_node_id, link_node_id);
                        self.node_child_add(node_parent_id, link_node_id);
                        self.node_set_parent(link_node_id, node_parent_id);
                        Ok(())
                    }
                    NodeKind::Directory(..) => Err(anyhow::anyhow!(
                        "Cannot move link, other links exist under the destination"
                    )),
                }
            } else {
                let node = &self.nodes[node_id];
                match &node.kind {
                    NodeKind::Link(..) => Err(anyhow::anyhow!(
                        "Cannot move link, an ancestor is already linked"
                    )),
                    NodeKind::Directory(_) => {
                        let new_node_id = self.node_create_link(destination, link_id);
                        self.node_remove(link_node_id);
                        self.links[link_id].node_id = new_node_id;
                        Ok(())
                    }
                }
            }
        }

        pub fn remove(&mut self, link_id: LinkID) {
            let node_id = self.links[link_id].node_id;
            self.node_remove(node_id);
            self.links.remove(link_id);
        }

        pub fn link_view(&self, link_id: LinkID) -> LinkView {
            LinkView {
                link_id,
                depot: self,
            }
        }

        /// searchs for the link at `origin`.
        /// returns SearchResult::Found(..) if there is a link at `origin`.
        /// returns SearchResult::Ancestor(..) if an ancestor of `origin` is linked.
        /// returns SearchResult::NotFound otherwise.
        pub fn search(&self, origin: impl AsRef<Path>) -> anyhow::Result<SearchResult> {
            let origin = origin.as_ref();
            verify_path(origin)?;
            Ok(self.search_unchecked(&origin))
        }

        /// returns an iterator for all the links at or under the given path.
        pub fn links_under(
            &self,
            path: impl AsRef<Path>,
        ) -> anyhow::Result<impl Iterator<Item = LinkID> + '_> {
            let path = path.as_ref();
            verify_path(path)?;

            let mut link_ids = Vec::new();
            if let Some(node_id) = self.node_find(path) {
                let mut node_ids = vec![node_id];
                while let Some(node_id) = node_ids.pop() {
                    let node = &self.nodes[node_id];
                    match &node.kind {
                        NodeKind::Link(link_id) => link_ids.push(*link_id),
                        NodeKind::Directory(children) => node_ids.extend(children.iter().copied()),
                    }
                }
            }
            Ok(link_ids.into_iter())
        }

        /// returns true if the `path` is a link or contains an ancestor that is linked.
        /// returns false otherwise.
        pub fn is_linked(&self, path: impl AsRef<Path>) -> bool {
            match self.search(path) {
                Ok(SearchResult::Found(..)) | Ok(SearchResult::Ancestor(..)) => true,
                _ => false,
            }
        }

        fn search_unchecked(&self, origin: &Path) -> SearchResult {
            debug_assert!(verify_path(origin).is_ok());

            let mut origin_comps = iter_path_comps(&origin);
            let mut curr_node = self.root;
            'outer: loop {
                let node = &self.nodes[curr_node];
                let curr_comp = origin_comps.next();
                match &node.kind {
                    NodeKind::Link(link_id) => match curr_comp {
                        Some(_) => break SearchResult::Ancestor(*link_id),
                        None => break SearchResult::Found(*link_id),
                    },
                    NodeKind::Directory(children) => match curr_comp {
                        Some(curr_comp) => {
                            for &child_id in children.iter() {
                                let child = &self.nodes[child_id];
                                if &child.comp == curr_comp {
                                    curr_node = child_id;
                                    continue 'outer;
                                }
                            }
                            break SearchResult::NotFound;
                        }
                        None => break SearchResult::NotFound,
                    },
                }
            }
        }

        /// creates a new directory node with no children.
        /// the node specified by `parent` must be a directory node.
        fn node_create_dir_empty(&mut self, parent: NodeID, comp: OsString) -> NodeID {
            let node_id = self.nodes.insert(Node {
                comp,
                parent,
                kind: NodeKind::Directory(Default::default()),
            });
            self.node_child_add(parent, node_id);
            node_id
        }

        /// all the nodes up to the node to be created have to be directory nodes.
        /// `path` must be a verified path.
        fn node_create_link(&mut self, path: &Path, link_id: LinkID) -> NodeID {
            assert!(verify_path(path).is_ok());
            let mut curr_node_id = self.root;
            let mut path_comps = iter_path_comps(path).peekable();
            // unwrap: a verified path has atleast 1 component
            let mut curr_path_comp = path_comps.next().unwrap();

            while path_comps.peek().is_some() {
                let next_node = match self.node_children_search(curr_node_id, curr_path_comp) {
                    Some(child_id) => child_id,
                    None => self.node_create_dir_empty(curr_node_id, curr_path_comp.to_owned()),
                };
                curr_node_id = next_node;
                // unwrap: we known next is Some beacause of this loop's condition
                curr_path_comp = path_comps.next().unwrap();
            }

            let new_node = self.nodes.insert(Node {
                comp: curr_path_comp.to_owned(),
                parent: curr_node_id,
                kind: NodeKind::Link(link_id),
            });
            self.node_child_add(curr_node_id, new_node);
            new_node
        }

        /// finds the node at the given path.
        /// `path` must be a verified path.
        fn node_find(&self, path: &Path) -> Option<NodeID> {
            match self.node_search(path) {
                (node_id, true) => Some(node_id),
                _ => None,
            }
        }

        /// searches for the node at `path`. if that node does not exists then it returns the
        /// closest node. return (closest_node, found)
        fn node_search(&self, path: &Path) -> (NodeID, bool) {
            debug_assert!(verify_path(path).is_ok());

            let mut origin_comps = iter_path_comps(&path).peekable();
            let mut curr_node = self.root;
            'outer: loop {
                let node = &self.nodes[curr_node];
                match origin_comps.next() {
                    Some(curr_comp) => match &node.kind {
                        NodeKind::Link(..) => break (curr_node, false),
                        NodeKind::Directory(children) => {
                            for &child_id in children.iter() {
                                let child = &self.nodes[child_id];
                                if &child.comp == curr_comp {
                                    curr_node = child_id;
                                    continue 'outer;
                                }
                            }
                            break (curr_node, false);
                        }
                    },
                    None => break (curr_node, true),
                }
            }
        }

        /// adds `new_child` to `node_id`'s children.
        /// the node specified by `node_id` must be a directory node.
        fn node_child_add(&mut self, node_id: NodeID, new_child: NodeID) {
            let node = &mut self.nodes[node_id];
            match node.kind {
                NodeKind::Directory(ref mut children) => {
                    children.insert(new_child);
                }
                _ => unreachable!(),
            }
        }

        /// searchs for a child with the given comp and returns its id.
        /// the node specified by `node_id` must be a directory node.
        fn node_children_search(&self, node_id: NodeID, search_comp: &OsStr) -> Option<NodeID> {
            let child_ids = match &self.nodes[node_id].kind {
                NodeKind::Directory(c) => c,
                _ => unreachable!(),
            };
            for &child_id in child_ids {
                let child = &self.nodes[child_id];
                if child.comp == search_comp {
                    return Some(child_id);
                }
            }
            None
        }

        /// removes `child` from `node_id`'s children.
        /// the node specified by `node_id` must be a directory node and it must contain the node
        /// `child`.
        fn node_child_remove(&mut self, node_id: NodeID, child: NodeID) {
            let node = &mut self.nodes[node_id];
            let remove_node = match &mut node.kind {
                NodeKind::Directory(children) => {
                    let contained = children.remove(&child);
                    assert!(contained);
                    children.is_empty()
                }
                _ => unreachable!(),
            };
            if remove_node && node_id != self.root {
                self.node_remove(node_id);
            }
        }

        fn node_set_parent(&mut self, node_id: NodeID, parent: NodeID) {
            self.nodes[node_id].parent = parent;
        }

        fn node_remove(&mut self, node_id: NodeID) {
            debug_assert!(node_id != self.root);
            debug_assert!(self.nodes.contains_key(node_id));

            let node = self.nodes.remove(node_id).unwrap();
            match node.kind {
                NodeKind::Link(..) => {}
                NodeKind::Directory(children) => {
                    // Right now directory nodes are only removed from inside this function and
                    // we do not remove directories with children
                    assert!(children.is_empty());
                }
            }
            let parent_id = node.parent;
            self.node_child_remove(parent_id, node_id);
        }
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
                    .create(disk_link.origin, disk_link.destination)
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

    fn verify_path(path: &Path) -> anyhow::Result<()> {
        // make sure the path is not empty
        // make sure the path is relative
        // make sure the path does not contain '.' or '..'
        if path.components().next().is_none() {
            return Err(anyhow::anyhow!("Path cannot be empty"));
        }
        for component in path.components() {
            match component {
                std::path::Component::Prefix(_) => {
                    return Err(anyhow::anyhow!("Path cannot have prefix"))
                }
                std::path::Component::RootDir => {
                    return Err(anyhow::anyhow!("Path must be relative"))
                }
                std::path::Component::CurDir | std::path::Component::ParentDir => {
                    return Err(anyhow::anyhow!("Path cannot contain '.' or '..'"))
                }
                std::path::Component::Normal(_) => {}
            }
        }
        Ok(())
    }

    /// Iterate over the components of a path.
    /// # Pre
    /// The path can only have "Normal" components.
    fn iter_path_comps(path: &Path) -> impl Iterator<Item = &OsStr> {
        debug_assert!(verify_path(path).is_ok());
        path.components().map(|component| match component {
            std::path::Component::Normal(comp) => comp,
            _ => unreachable!(),
        })
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_depot_create() {
            let mut depot = Depot::default();
            depot.create("", "dest1.txt").unwrap_err();
            depot.create("comp1.txt", "").unwrap_err();
            depot.create("", "").unwrap_err();

            depot.create("comp1.txt", "dest1.txt").unwrap();
            depot.create("comp1.txt", "dest1_updated.txt").unwrap();
            depot
                .create("./comp1.txt", "dest1_updated.txt")
                .unwrap_err();
            depot.create("/comp1.txt", "dest1.txt").unwrap_err();
            depot.create("dir1/", "destdir1/").unwrap();
            depot.create("dir1/file1.txt", "destfile1.txt").unwrap_err();
        }

        #[test]
        fn test_depot_move_link() {
            let mut depot = Depot::default();
            let f1 = depot.create("d1/f1", "d1/f1").unwrap();
            let _f2 = depot.create("d1/f2", "d1/f2").unwrap();

            depot.move_link(f1, "d1/f2/f1").unwrap_err();
            depot.move_link(f1, "d1").unwrap_err();

            depot.move_link(f1, "").unwrap();
            depot.move_link(f1, "d2/f1").unwrap();
        }

        #[test]
        fn test_depot_remove() {
            let mut depot = Depot::default();
            let f1 = depot.create("d1/f1", "d1/f1").unwrap();
            assert_eq!(depot.search("d1/f1").unwrap(), SearchResult::Found(f1));
            depot.remove(f1);
            assert_eq!(depot.search("d1/f1").unwrap(), SearchResult::NotFound);
        }

        #[test]
        fn test_depot_search() {
            let mut depot = Depot::default();
            let f1 = depot.create("d1/f1", "d1/f1").unwrap();
            let f2 = depot.create("d1/f2", "d1/f2").unwrap();
            let f3 = depot.create("d1/f3", "d1/f3").unwrap();
            let f4 = depot.create("d1/d2/f4", "d2/f4").unwrap();
            let d3 = depot.create("d3", "d3").unwrap();

            assert_eq!(depot.search("d1").unwrap(), SearchResult::NotFound,);
            assert_eq!(depot.search("d1/f1").unwrap(), SearchResult::Found(f1),);
            assert_eq!(depot.search("d1/f2").unwrap(), SearchResult::Found(f2),);
            assert_eq!(depot.search("d1/f3").unwrap(), SearchResult::Found(f3),);
            assert_eq!(depot.search("d1/d2/f4").unwrap(), SearchResult::Found(f4),);
            assert_eq!(depot.search("d1/d2/f5").unwrap(), SearchResult::NotFound,);
            assert_eq!(depot.search("d3/f6").unwrap(), SearchResult::Ancestor(d3),);
        }

        #[test]
        fn test_iter_path_comps() {
            let path = Path::new("comp1/comp2/./comp3/file.txt");
            let mut iter = iter_path_comps(path);
            assert_eq!(iter.next(), Some(OsStr::new("comp1")));
            assert_eq!(iter.next(), Some(OsStr::new("comp2")));
            assert_eq!(iter.next(), Some(OsStr::new("comp3")));
            assert_eq!(iter.next(), Some(OsStr::new("file.txt")));
            assert_eq!(iter.next(), None);
        }
    }
}

mod dotup {
    use std::{
        collections::HashSet,
        path::{Path, PathBuf},
    };

    use anyhow::Context;

    use crate::depot::{self, Depot, LinkID};

    #[derive(Debug)]
    struct CanonicalPair {
        link_id: LinkID,
        origin: PathBuf,
        destination: PathBuf,
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
                let origin = self.prepare_origin_path(origin.as_ref())?;
                let destination = destination.as_ref();
                self.depot.create(origin, destination)?;
            };
            match link_result {
                Ok(_) => {}
                Err(e) => println!("Failed to create link : {e}"),
            }
        }

        pub fn unlink(&mut self, paths: impl Iterator<Item = impl AsRef<Path>>) {
            for origin in paths {
                let unlink_result: anyhow::Result<()> = try {
                    let origin = self.prepare_origin_path(origin.as_ref())?;
                    let search_results = self.depot.search(&origin)?;
                    match search_results {
                        depot::SearchResult::Found(link_id) => {
                            self.depot.remove(link_id);
                            println!("removed link {}", origin.display());
                        }
                        depot::SearchResult::Ancestor(_) | depot::SearchResult::NotFound => {
                            println!("{} is not linked", origin.display())
                        }
                    }
                };
                match unlink_result {
                    Ok(_) => {}
                    Err(e) => println!("Failed to unlink {} : {e}", origin.as_ref().display()),
                }
            }
        }

        pub fn install(&self, paths: impl Iterator<Item = impl AsRef<Path>>) {
            let mut already_linked: HashSet<LinkID> = Default::default();
            for origin in paths {
                let install_result: anyhow::Result<()> = try {
                    let origin = self.prepare_origin_path(origin.as_ref())?;
                    let canonical_pairs = self.canonical_pairs_under(&origin)?;
                    for pair in canonical_pairs {
                        if already_linked.contains(&pair.link_id) {
                            continue;
                        }
                        self.install_symlink(&pair.origin, &pair.destination)?;
                        already_linked.insert(pair.link_id);
                    }
                };
                if let Err(e) = install_result {
                    println!("error while installing {} : {e}", origin.as_ref().display());
                }
            }
        }

        pub fn uninstall(&self, paths: impl Iterator<Item = impl AsRef<Path>>) {
            for origin in paths {
                let uninstall_result: anyhow::Result<()> = try {
                    let origin = self.prepare_origin_path(origin.as_ref())?;
                    let canonical_pairs = self.canonical_pairs_under(&origin)?;
                    for pair in canonical_pairs {
                        self.uninstall_symlink(&pair.origin, &pair.destination)?;
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

        pub fn mv(&mut self, from: impl Iterator<Item = impl AsRef<Path>>, to: impl AsRef<Path>) {
            let to = to.as_ref();
            let from: Vec<_> = from.map(|p| p.as_ref().to_owned()).collect();
            match from.as_slice() {
                [] => unreachable!(),
                [from] => self.mv_one(from, to),
                [from @ ..] => self.mv_many(from, to),
            }
        }

        fn mv_one(&mut self, from: &Path, to: &Path) {}

        fn mv_many(&mut self, from: &[PathBuf], to: &Path) {}

        fn prepare_origin_path(&self, origin: &Path) -> anyhow::Result<PathBuf> {
            let canonical = origin
                .canonicalize()
                .context("Failed to canonicalize origin path")?;
            let relative = canonical
                .strip_prefix(&self.depot_dir)
                .context("Invalid origin path, not under depot directory")?;
            Ok(relative.to_owned())
        }

        // returns the canonical pairs (origin, destination) for all links under `path`.
        fn canonical_pairs_under(&self, path: &Path) -> anyhow::Result<Vec<CanonicalPair>> {
            let origin = self.prepare_origin_path(path)?;
            let mut paths = Vec::new();
            for link_id in self.depot.links_under(origin)? {
                let link_view = self.depot.link_view(link_id);
                let relative_origin = link_view.origin();
                let relative_destination = link_view.destination();
                let canonical_origin = self.depot_dir.join(relative_origin);
                let canonical_destination = self.install_base.join(relative_destination);
                paths.push(CanonicalPair {
                    link_id,
                    origin: canonical_origin,
                    destination: canonical_destination,
                });
            }
            Ok(paths)
        }

        fn install_symlink(&self, origin: &Path, destination: &Path) -> anyhow::Result<()> {
            debug_assert!(origin.is_absolute());
            debug_assert!(destination.is_absolute());

            if let Some(destination_parent) = destination.parent() {
                std::fs::create_dir_all(destination_parent)
                    .context("Failed to create directories")?;
            }

            let destination_exists = destination.exists();
            let destination_is_symlink = destination.is_symlink();

            if destination_exists && !destination_is_symlink {
                return Err(anyhow::anyhow!("destination already exists"));
            }

            if destination_is_symlink {
                std::fs::remove_file(&destination)?;
            }
            std::os::unix::fs::symlink(origin, destination).context("Failed to create symlink")?;

            Ok(())
        }

        fn uninstall_symlink(&self, origin: &Path, destination: &Path) -> anyhow::Result<()> {
            debug_assert!(origin.is_absolute());
            debug_assert!(destination.is_absolute());

            if destination.is_symlink() {
                let symlink_destination = destination.read_link()?.canonicalize()?;
                if symlink_destination == origin {
                    std::fs::remove_file(&destination)?;
                }
            }

            Ok(())
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
    use std::path::PathBuf;

    use crate::{
        depot::{self, Depot},
        dotup::{self, Dotup},
        Flags,
    };

    pub const DEFAULT_DEPOT_FILE_NAME: &str = ".depot";

    pub fn read_dotup(flags: &Flags) -> anyhow::Result<Dotup> {
        let depot_path = depot_path_from_flags(flags)?;
        let install_base = install_base_from_flags(flags);
        dotup::read(depot_path, install_base)
    }

    pub fn write_dotup(dotup: &Dotup) -> anyhow::Result<()> {
        dotup::write(dotup)
    }

    pub fn read_depot(flags: &Flags) -> anyhow::Result<Depot> {
        let depot_path = depot_path_from_flags(flags)?;
        let depot = depot::read(&depot_path)?;
        Ok(depot)
    }

    pub fn write_depot(flags: &Flags, depot: &Depot) -> anyhow::Result<()> {
        let depot_path = depot_path_from_flags(flags)?;
        depot::write(&depot_path, depot)?;
        Ok(())
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

    fn current_working_directory() -> PathBuf {
        std::env::current_dir().expect("Failed to obtain current working directory")
    }
}

use std::path::PathBuf;

use clap::Parser;
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
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    match args.command {
        SubCommand::Init(cmd_args) => command_init(args.flags, cmd_args),
        SubCommand::Link(cmd_args) => command_link(args.flags, cmd_args),
        SubCommand::Unlink(cmd_args) => command_unlink(args.flags, cmd_args),
        SubCommand::Install(cmd_args) => command_install(args.flags, cmd_args),
        SubCommand::Uninstall(cmd_args) => command_uninstall(args.flags, cmd_args),
        SubCommand::Mv(cmd_args) => command_mv(args.flags, cmd_args),
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
    origin: PathBuf,
    destination: PathBuf,
}

fn command_link(global_flags: Flags, args: LinkArgs) -> anyhow::Result<()> {
    let mut dotup = utils::read_dotup(&global_flags)?;
    dotup.link(args.origin, args.destination);
    utils::write_dotup(&dotup)?;
    Ok(())
}

#[derive(Parser, Debug)]
struct UnlinkArgs {
    paths: Vec<PathBuf>,
}

fn command_unlink(global_flags: Flags, args: UnlinkArgs) -> anyhow::Result<()> {
    let mut dotup = utils::read_dotup(&global_flags)?;
    dotup.unlink(args.paths.into_iter());
    utils::write_dotup(&dotup)?;
    Ok(())
}

#[derive(Parser, Debug)]
struct InstallArgs {
    paths: Vec<PathBuf>,
}

fn command_install(global_flags: Flags, args: InstallArgs) -> anyhow::Result<()> {
    let dotup = utils::read_dotup(&global_flags)?;
    dotup.install(args.paths.into_iter());
    utils::write_dotup(&dotup)?;
    Ok(())
}

#[derive(Parser, Debug)]
struct UninstallArgs {
    paths: Vec<PathBuf>,
}

fn command_uninstall(global_flags: Flags, args: UninstallArgs) -> anyhow::Result<()> {
    let dotup = utils::read_dotup(&global_flags)?;
    dotup.uninstall(args.paths.into_iter());
    utils::write_dotup(&dotup)?;
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
