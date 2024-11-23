#![allow(warnings)]

use itertools::Itertools;

use uv_configuration::{DevGroupsManifest, ExtrasSpecification};
use uv_normalize::{ExtraName, PackageName};
use uv_pep508::{MarkerEnvironment, MarkerEnvironmentBuilder, MarkerTree};
use uv_pypi_types::Conflicts;

// BREADCRUMBS: Work on a new `ConflictMarker` type instead of using
// `MarkerTree` directly below. And instead of storing only extra/group
// names, we need to store package names too. Wire everything up. This
// will also take us toward making groups work. But keep going with just
// extras for now.

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
    conflict_marker: ConflictMarker,
}

impl UniversalMarker {
    /// A constant universal marker that always evaluates to `true`.
    pub(crate) const TRUE: UniversalMarker = UniversalMarker {
        pep508_marker: MarkerTree::TRUE,
        conflict_marker: ConflictMarker::TRUE,
    };

    /// A constant universal marker that always evaluates to `false`.
    pub(crate) const FALSE: UniversalMarker = UniversalMarker {
        pep508_marker: MarkerTree::FALSE,
        conflict_marker: ConflictMarker::FALSE,
    };

    /// Creates a new universal marker from its constituent pieces.
    pub(crate) fn new(
        pep508_marker: MarkerTree,
        conflict_marker: ConflictMarker,
    ) -> UniversalMarker {
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
        self.conflict_marker = self.conflict_marker.or(other.conflict_marker);
    }

    /// Combine this universal marker with the one given in a way that
    /// intersects them. That is, the updated marker will evaluate to `true` if
    /// `self` and `other` evaluate to `true`.
    pub(crate) fn and(&mut self, other: UniversalMarker) {
        self.pep508_marker.and(other.pep508_marker);
        self.conflict_marker = self.conflict_marker.and(other.conflict_marker);
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
        let mut marker = ConflictMarker::FALSE;
        for set in conflicts.iter() {
            for (item1, item2) in set.iter().tuple_combinations() {
                // FIXME: Account for groups here. And extra/group
                // combinations too.
                let (Some(extra1), Some(extra2)) = (item1.extra(), item2.extra()) else {
                    continue;
                };
                let pair = ConflictMarker::extra(item1.package(), extra1)
                    .and(ConflictMarker::extra(item2.package(), extra2));
                marker = marker.or(pair);
            }
        }
        self.conflict_marker = marker
            .negate()
            .implies(std::mem::take(&mut self.conflict_marker));
    }

    /// Assumes that a given extra for the given package is activated.
    ///
    /// This may simplify the conflicting marker component of this universal
    /// marker.
    pub(crate) fn assume_extra(&mut self, package: &PackageName, extra: &ExtraName) {
        self.conflict_marker = self.conflict_marker.assume_extra(package, extra);
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
    pub(crate) fn evaluate(&self, env: &MarkerEnvironment, extras: &[ExtraName]) -> bool {
        // We specifically evaluate the conflict marker with an empty set of
        // extras because this seems to best capture the intent here. Namely,
        // in this context, we don't know the packages associated with the
        // given extras, which means we can't really evaluate the conflict
        // marker. But note that evaluation here is not vacuous. By providing
        // an empty list, conflict markers like `extra != 'package[x1]'`
        // will evaluate to `true`, while conflict markers like
        // `extra == 'package[x2]'` will evaluate to `false`.
        self.pep508_marker.evaluate(env, extras) && self.conflict_marker.evaluate(&[])
    }

    /// Returns true if this universal marker is satisfied by the given
    /// marker environment and list of activated extras and groups.
    ///
    /// TODO: Articulate the difference between this and `evaluate`.
    pub(crate) fn satisfies(
        &self,
        env: &MarkerEnvironment,
        activated_extras: &[(PackageName, ExtraName)],
        _dev: &DevGroupsManifest,
    ) -> bool {
        if !self.pep508_marker.evaluate(env, &[]) {
            return false;
        }
        // let extra_list = match *extras {
        // // TODO(ag): This should still evaluate `dev`.
        // ExtrasSpecification::All => return true,
        // ExtrasSpecification::None => &[][..],
        // ExtrasSpecification::Some(ref list) => list,
        // };
        self.conflict_marker.evaluate(activated_extras)
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
    pub fn conflict(&self) -> &ConflictMarker {
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
            self.conflict_marker.is_true(),
        ) {
            (None, true) => write!(f, "`true`"),
            (Some(pep508), true) => write!(f, "`{pep508}`"),
            (None, false) => write!(f, "`true` (conflict marker: `{}`)", self.conflict_marker),
            (Some(pep508), false) => {
                write!(
                    f,
                    "`{pep508}` (conflict marker: `{}`)",
                    self.conflict_marker
                )
            }
        }
    }
}

#[derive(Default, Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct ConflictMarker {
    marker: MarkerTree,
}

impl ConflictMarker {
    /// A constant conflict marker that always evaluates to `true`.
    pub const TRUE: ConflictMarker = ConflictMarker {
        marker: MarkerTree::TRUE,
    };

    /// A constant conflict marker that always evaluates to `false`.
    pub const FALSE: ConflictMarker = ConflictMarker {
        marker: MarkerTree::FALSE,
    };

    /// Create a conflict marker that is true only when the given extra for the
    /// given package is activated.
    pub fn extra(package: &PackageName, extra: &ExtraName) -> ConflictMarker {
        let operator = uv_pep508::ExtraOperator::Equal;
        let name = uv_pep508::MarkerValueExtra::Extra(encode_package_extra(package, extra));
        let expr = uv_pep508::MarkerExpression::Extra { operator, name };
        let marker = MarkerTree::expression(expr);
        ConflictMarker { marker }
    }

    /// Returns a new conflict marker that is the negation of this one.
    #[must_use]
    pub fn negate(&self) -> ConflictMarker {
        ConflictMarker {
            marker: self.marker.negate(),
        }
    }

    /// Returns a new conflict marker corresponding to the union of `self` and
    /// `other`.
    #[must_use]
    pub fn or(&self, other: ConflictMarker) -> ConflictMarker {
        let mut marker = self.marker.clone();
        marker.or(other.marker);
        ConflictMarker { marker }
    }

    /// Returns a new conflict marker corresponding to the intersection of
    /// `self` and `other`.
    #[must_use]
    pub fn and(&self, other: ConflictMarker) -> ConflictMarker {
        let mut marker = self.marker.clone();
        marker.and(other.marker);
        ConflictMarker { marker }
    }

    /// Returns a new conflict marker corresponding to the logical implication
    /// of `self` and the given consequent.
    ///
    /// If the conflict marker returned is always `true`, then it can be said
    /// that `self` implies `consequent`.
    #[must_use]
    pub fn implies(&self, other: ConflictMarker) -> ConflictMarker {
        let mut marker = self.marker.clone();
        marker.implies(other.marker);
        ConflictMarker { marker }
    }

    /// Returns a new conflict marker with the given extra assumed to be true.
    ///
    /// This may simplify the marker.
    #[must_use]
    pub(crate) fn assume_extra(&self, package: &PackageName, extra: &ExtraName) -> ConflictMarker {
        let extra = encode_package_extra(package, extra);
        let marker = self
            .marker
            .clone()
            .simplify_extras_with(|candidate| *candidate == extra);
        ConflictMarker { marker }
    }

    /// Returns true if this conflict marker will always evaluate to `true`.
    pub fn is_true(&self) -> bool {
        self.marker.is_true()
    }

    /// Returns true if this conflict marker will always evaluate to `false`.
    pub fn is_false(&self) -> bool {
        self.marker.is_false()
    }

    /// Returns true if this conflict marker is disjoint with the one given.
    ///
    /// Two conflict markers are disjoint when it is impossible for them both
    /// to evaluate to `true` simultaneously.
    pub(crate) fn is_disjoint(&self, other: &ConflictMarker) -> bool {
        self.marker.is_disjoint(&other.marker)
    }

    /// Returns true if this conflict marker is satisfied by the given
    /// list of activated extras.
    pub(crate) fn evaluate(&self, extras: &[(PackageName, ExtraName)]) -> bool {
        static DUMMY: std::sync::LazyLock<MarkerEnvironment> = std::sync::LazyLock::new(|| {
            MarkerEnvironment::try_from(MarkerEnvironmentBuilder {
                implementation_name: "",
                implementation_version: "3.7",
                os_name: "linux",
                platform_machine: "",
                platform_python_implementation: "",
                platform_release: "",
                platform_system: "",
                platform_version: "",
                python_full_version: "3.7",
                python_version: "3.7",
                sys_platform: "linux",
            })
            .unwrap()
        });
        let extras = extras
            .iter()
            .map(|&(ref package, ref extra)| encode_package_extra(package, extra))
            .collect::<Vec<ExtraName>>();
        self.marker.evaluate(&DUMMY, &extras)
    }
}

impl std::fmt::Display for ConflictMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO: This shouldn't be exposing the internal marker,
        // but instead transforming it.
        if self.marker.is_false() {
            return write!(f, "false");
        }
        let Some(contents) = self.marker.contents() else {
            return write!(f, "true");
        };
        write!(f, "{contents}")
    }
}

impl std::fmt::Debug for ConflictMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // This intentionally exposes the underlying PEP 508 marker
        // representation so that programmers can debug it if there's
        // something wrong with it.
        write!(f, "ConflictMarker({:?})", self.marker)
    }
}

// TODO: This should parse a custom format. But we expose
// the raw PEP 508 marker for now for simplicity. We'll
// write the custom parser later.
impl std::str::FromStr for ConflictMarker {
    type Err = uv_pep508::Pep508Error;

    fn from_str(markers: &str) -> Result<Self, Self::Err> {
        Ok(ConflictMarker {
            marker: markers.parse()?,
        })
    }
}

impl<'de> serde::Deserialize<'de> for ConflictMarker {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        std::str::FromStr::from_str(&s).map_err(serde::de::Error::custom)
    }
}

impl serde::Serialize for ConflictMarker {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

fn encode_package_extra(package: &PackageName, extra: &ExtraName) -> ExtraName {
    // This is OK because `PackageName` and `ExtraName` have the same
    // validation rules, and we combine them in a way that always results
    // in a valid name.
    ExtraName::new(format!("extra-{package}-{extra}")).unwrap()
}
