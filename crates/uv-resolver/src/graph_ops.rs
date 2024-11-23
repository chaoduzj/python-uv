use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph::visit::EdgeRef;
use petgraph::{Direction, Graph};
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;

use uv_normalize::{ExtraName, PackageName};

use crate::resolution::ResolutionGraphNode;
use crate::universal_marker::UniversalMarker;

/// Determine the markers under which a package is reachable in the dependency tree.
///
/// The algorithm is a variant of Dijkstra's algorithm for not totally ordered distances:
/// Whenever we find a shorter distance to a node (a marker that is not a subset of the existing
/// marker), we re-queue the node and update all its children. This implicitly handles cycles,
/// whenever we re-reach a node through a cycle the marker we have is a more
/// specific marker/longer path, so we don't update the node and don't re-queue it.
pub(crate) fn marker_reachability<T>(
    graph: &Graph<T, UniversalMarker>,
    fork_markers: &[UniversalMarker],
) -> FxHashMap<NodeIndex, UniversalMarker> {
    // Note that we build including the virtual packages due to how we propagate markers through
    // the graph, even though we then only read the markers for base packages.
    let mut reachability = FxHashMap::with_capacity_and_hasher(graph.node_count(), FxBuildHasher);

    // Collect the root nodes.
    //
    // Besides the actual virtual root node, virtual dev dependencies packages are also root
    // nodes since the edges don't cover dev dependencies.
    let mut queue: Vec<_> = graph
        .node_indices()
        .filter(|node_index| {
            graph
                .edges_directed(*node_index, Direction::Incoming)
                .next()
                .is_none()
        })
        .collect();

    // The root nodes are always applicable, unless the user has restricted resolver
    // environments with `tool.uv.environments`.
    let root_markers = if fork_markers.is_empty() {
        UniversalMarker::TRUE
    } else {
        fork_markers
            .iter()
            .fold(UniversalMarker::FALSE, |mut acc, marker| {
                acc.or(marker.clone());
                acc
            })
    };
    for root_index in &queue {
        reachability.insert(*root_index, root_markers.clone());
    }

    // Propagate all markers through the graph, so that the eventual marker for each node is the
    // union of the markers of each path we can reach the node by.
    while let Some(parent_index) = queue.pop() {
        let marker = reachability[&parent_index].clone();
        for child_edge in graph.edges_directed(parent_index, Direction::Outgoing) {
            // The marker for all paths to the child through the parent.
            let mut child_marker = child_edge.weight().clone();
            child_marker.and(marker.clone());
            match reachability.entry(child_edge.target()) {
                Entry::Occupied(mut existing) => {
                    // If the marker is a subset of the existing marker (A ⊆ B exactly if
                    // A ∪ B = A), updating the child wouldn't change child's marker.
                    child_marker.or(existing.get().clone());
                    if &child_marker != existing.get() {
                        existing.insert(child_marker);
                        queue.push(child_edge.target());
                    }
                }
                Entry::Vacant(vacant) => {
                    vacant.insert(child_marker.clone());
                    queue.push(child_edge.target());
                }
            }
        }
    }

    reachability
}

/// Traverse the given dependency graph and propagate activated markers.
///
/// For example, given an edge like `foo[x1] -> bar`, then it is known that
/// `x1` is activated. This in turn can be used to simplify any downstream
/// conflict markers with `extra == "x1"` in them.
pub(crate) fn simplify_conflict_markers(graph: &mut Graph<ResolutionGraphNode, UniversalMarker>) {
    // The set of activated extras (and TODO, in the future, groups)
    // for each node. The ROOT nodes don't have any extras activated.
    let mut activated: FxHashMap<NodeIndex, FxHashSet<(PackageName, ExtraName)>> =
        FxHashMap::with_capacity_and_hasher(graph.node_count(), FxBuildHasher);

    // Collect the root nodes.
    //
    // Besides the actual virtual root node, virtual dev dependencies packages are also root
    // nodes since the edges don't cover dev dependencies.
    let mut queue: Vec<_> = graph
        .node_indices()
        .filter(|node_index| {
            graph
                .edges_directed(*node_index, Direction::Incoming)
                .next()
                .is_none()
        })
        .collect();

    let mut assume_by_edge: FxHashMap<EdgeIndex, FxHashSet<(PackageName, ExtraName)>> =
        FxHashMap::default();
    let mut seen: FxHashSet<NodeIndex> = FxHashSet::default();
    while let Some(parent_index) = queue.pop() {
        for child_edge in graph.edges_directed(parent_index, Direction::Outgoing) {
            // TODO: The below seems excessively clone-y.
            // Consider tightening this up a bit.
            let target = child_edge.target();
            let mut extras: FxHashSet<(PackageName, ExtraName)> =
                activated.get(&parent_index).cloned().unwrap_or_default();
            if let Some((package, extra)) = graph[parent_index].package_extra_names() {
                extras.insert((package.clone(), extra.clone()));
            }
            if let Some((package, extra)) = graph[target].package_extra_names() {
                extras.insert((package.clone(), extra.clone()));
            }
            activated.entry(target).or_default().extend(extras.clone());
            assume_by_edge
                .entry(child_edge.id())
                .or_default()
                .extend(extras);
            if seen.insert(child_edge.target()) {
                queue.push(child_edge.target());
            }
        }
    }
    for (edge_id, extras) in assume_by_edge {
        for &(ref package, ref extra) in &extras {
            graph[edge_id].assume_extra(package, extra);
        }
    }
}
