use itertools::Itertools;

use uv_normalize::ExtraName;
use uv_pep508::{MarkerEnvironment, MarkerTree};
use uv_pypi_types::Conflicts;

/// A representation of a marker for use in universal resolution.
///
/// (This also degrades gracefully to a standard PEP 508 marker in the case of
/// non-universal resolution.)
///
/// This universal marker is meant to combine both a PEP 508 marker and a
/// marker for conflicting extras/groups. The latter specifically expresses
/// whether a particular edge in a dependency graph should be followed
/// depending on the activated extras and groups.
///
/// A universal marker evaluates to true only when *both* its PEP 508 marker
/// and its conflict marker evaluate to true.
#[derive(Debug, Default, Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct UniversalMarker {
    pep508_marker: MarkerTree,
    conflict_marker: MarkerTree,
}

impl UniversalMarker {
    /// A constant universal marker that always evaluates to `true`.
    pub(crate) const TRUE: UniversalMarker = UniversalMarker {
        pep508_marker: MarkerTree::TRUE,
        conflict_marker: MarkerTree::TRUE,
    };

    /// A constant universal marker that always evaluates to `false`.
    pub(crate) const FALSE: UniversalMarker = UniversalMarker {
        pep508_marker: MarkerTree::FALSE,
        conflict_marker: MarkerTree::FALSE,
    };

    /// Creates a new universal marker from its constituent pieces.
    pub(crate) fn new(pep508_marker: MarkerTree, conflict_marker: MarkerTree) -> UniversalMarker {
        UniversalMarker {
            pep508_marker,
            conflict_marker,
        }
    }

    /// Combine this universal marker with the one given in a way that unions
    /// them. That is, the updated marker will evaluate to `true` if `self` or
    /// `other` evaluate to `true`.
    pub(crate) fn or(&mut self, other: UniversalMarker) {
        self.pep508_marker.or(other.pep508_marker);
        self.conflict_marker.or(other.conflict_marker);
    }

    /// Combine this universal marker with the one given in a way that
    /// intersects them. That is, the updated marker will evaluate to `true` if
    /// `self` and `other` evaluate to `true`.
    pub(crate) fn and(&mut self, other: UniversalMarker) {
        self.pep508_marker.and(other.pep508_marker);
        self.conflict_marker.and(other.conflict_marker);
    }

    /// Imbibes the world knowledge expressed by `conflicts` into this marker.
    ///
    /// This will effectively simplify the conflict marker in this universal
    /// marker. In particular, it enables simplifying based on the fact that no
    /// two items from the same set in the given conflicts can be active at a
    /// given time.
    pub(crate) fn imbibe(&mut self, conflicts: &Conflicts) {
        if conflicts.is_empty() {
            return;
        }
        // TODO: This is constructing what could be a big
        // marker (depending on how many conflicts there are),
        // which is invariant throughout the lifetime of the
        // program. But it's doing it every time this routine
        // is called. We should refactor the caller to build
        // a marker from the `conflicts` once.
        let mut marker = MarkerTree::FALSE;
        for set in conflicts.iter() {
            for (item1, item2) in set.iter().tuple_combinations() {
                // FIXME: Account for groups here. And extra/group
                // combinations too.
                let (Some(extra1), Some(extra2)) = (item1.extra(), item2.extra()) else {
                    continue;
                };

                let operator = uv_pep508::ExtraOperator::Equal;
                let name = uv_pep508::MarkerValueExtra::Extra(extra1.clone());
                let expr = uv_pep508::MarkerExpression::Extra { operator, name };
                let marker1 = MarkerTree::expression(expr);

                let operator = uv_pep508::ExtraOperator::Equal;
                let name = uv_pep508::MarkerValueExtra::Extra(extra2.clone());
                let expr = uv_pep508::MarkerExpression::Extra { operator, name };
                let marker2 = MarkerTree::expression(expr);

                let mut pair = MarkerTree::TRUE;
                pair.and(marker1);
                pair.and(marker2);
                marker.or(pair);
            }
        }
        let mut marker = marker.negate();
        marker.implies(std::mem::take(&mut self.conflict_marker));
        self.conflict_marker = marker;
    }

    /// Assumes that a given extra is activated.
    ///
    /// This may simplify the conflicting marker component of this universal
    /// marker.
    pub(crate) fn assume_extra(&mut self, extra: &ExtraName) {
        self.conflict_marker = std::mem::take(&mut self.conflict_marker)
            .simplify_extras_with(|candidate| candidate == extra);
    }

    /// Returns true if this universal marker will always evaluate to `true`.
    pub(crate) fn is_true(&self) -> bool {
        self.pep508_marker.is_true() && self.conflict_marker.is_true()
    }

    /// Returns true if this universal marker will always evaluate to `false`.
    pub(crate) fn is_false(&self) -> bool {
        self.pep508_marker.is_false() || self.conflict_marker.is_false()
    }

    /// Returns true if this universal marker is disjoint with the one given.
    ///
    /// Two universal markers are disjoint when it is impossible for them both
    /// to evaluate to `true` simultaneously.
    pub(crate) fn is_disjoint(&self, other: &UniversalMarker) -> bool {
        self.pep508_marker.is_disjoint(&other.pep508_marker)
            || self.conflict_marker.is_disjoint(&other.conflict_marker)
    }

    /// Returns true if this universal marker is satisfied by the given
    /// marker environment and list of activated extras.
    ///
    /// FIXME: This also needs to accept a list of groups.
    pub(crate) fn evaluate(&self, env: &MarkerEnvironment, extras: &[ExtraName]) -> bool {
        self.pep508_marker.evaluate(env, extras) && self.conflict_marker.evaluate(env, extras)
    }

    /// Returns the PEP 508 marker for this universal marker.
    ///
    /// One should be cautious using this. Generally speaking, it should only
    /// be used when one knows universal resolution isn't in effect. When
    /// universal resolution is enabled (i.e., there may be multiple forks
    /// producing different versions of the same package), then one should
    /// always use a universal marker since it accounts for all possible ways
    /// for a package to be installed.
    pub fn pep508(&self) -> &MarkerTree {
        &self.pep508_marker
    }

    /// Returns the non-PEP 508 marker expression that represents conflicting
    /// extras/groups.
    ///
    /// Like with `UniversalMarker::pep508`, one should be cautious when using
    /// this. It is generally always wrong to consider conflicts in isolation
    /// from PEP 508 markers. But this can be useful for detecting failure
    /// cases. For example, the code for emitting a `ResolverOutput` (even a
    /// universal one) in a `requirements.txt` format checks for the existence
    /// of non-trivial conflict markers and fails if any are found. (Because
    /// conflict markers cannot be represented in the `requirements.txt`
    /// format.)
    pub fn conflict(&self) -> &MarkerTree {
        &self.conflict_marker
    }
}

impl std::fmt::Display for UniversalMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.pep508_marker.is_false() || self.conflict_marker.is_false() {
            return write!(f, "`false`");
        }
        match (
            self.pep508_marker.contents(),
            self.conflict_marker.contents(),
        ) {
            (None, None) => write!(f, "`true`"),
            (Some(pep508), None) => write!(f, "`{pep508}`"),
            (None, Some(conflict)) => write!(f, "`true` (conflict marker: `{conflict}`)"),
            (Some(pep508), Some(conflict)) => {
                write!(f, "`{pep508}` (conflict marker: `{conflict}`)")
            }
        }
    }
}
