use std::cmp::Eq;
use std::collections::HashSet;
use std::hash::Hash;

use petgraph::visit::{
    depth_first_search, Control, DfsEvent, IntoNeighbors, IntoNodeIdentifiers, NodeIndexable,
    Visitable,
};

pub fn tarjan_scc_variant<G, I>(g: G, starts: impl Fn() -> I) -> Vec<(G::NodeId, Vec<G::NodeId>)>
where
    G: IntoNodeIdentifiers + IntoNeighbors + NodeIndexable + Visitable,
    G::NodeId: Eq + Hash,
    I: IntoIterator<Item = G::NodeId>,
{
    let sccs = petgraph::algo::tarjan_scc(&g);
    sccs.into_iter()
        .filter(|scc| scc.len() > 1)
        .map(|scc| {
            let set = HashSet::<&G::NodeId>::from_iter(scc.iter());
            let head = depth_first_search(&g, starts(), |event| {
                if let DfsEvent::Discover(node, _) = event {
                    if set.contains(&node) {
                        return Control::Break(node);
                    }
                }
                Control::Continue
            })
            .break_value()
            .unwrap();
            (head, scc)
        })
        .collect()
}

pub fn reachable_nodes<G, I>(graph: G, starts: I) -> Vec<G::NodeId>
where
    G: IntoNeighbors + Visitable,
    G::NodeId: Eq + Hash,
    I: IntoIterator<Item = G::NodeId>,
{
    let mut v = vec![];
    depth_first_search(graph, starts, |event| {
        if let DfsEvent::Discover(node, _) = event {
            v.push(node);
        }
    });
    v
}

pub fn is_reachable_from<G, I>(graph: G, node: G::NodeId, starts: I) -> bool
where
    G: IntoNeighbors + Visitable,
    G::NodeId: Eq + Hash,
    I: IntoIterator<Item = G::NodeId>,
{
    depth_first_search(graph, starts, |event| {
        if let DfsEvent::Discover(this_node, _) = event {
            if this_node == node {
                return Control::Break(());
            }
        }
        Control::Continue
    })
    .break_value()
    .is_some()
}
