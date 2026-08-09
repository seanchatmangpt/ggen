//! Deterministic semantic-delta planning for Vision 2030 manufacture.
//!
//! This module is intentionally smaller than a generator framework. It turns an admitted RDF
//! delta into the minimum dependency-closed projection plan that must be manufactured next. It
//! does not write files, invoke processes, call networks, or actuate external systems. The output
//! is a deterministic construction plan plus the verification court every affected projection
//! must satisfy before any downstream authority boundary may execute consequence.

use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    fmt,
};

use ggen_graph::{parse_nquad, RdfDelta};
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Stable identifier for one target-specific projection compiler or pack.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ProjectionId(String);

impl ProjectionId {
    /// Construct a projection identifier.
    ///
    /// # Errors
    ///
    /// Returns [`ManufacturingRefusal::EmptyProjectionId`] when `value` is blank.
    pub fn new(value: impl Into<String>) -> Result<Self, ManufacturingRefusal> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(ManufacturingRefusal::EmptyProjectionId);
        }
        Ok(Self(value))
    }

    /// Borrow the identifier text.
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ProjectionId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

/// Verification obligations generated with every affected projection.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CourtObligation {
    /// Re-running manufacture from the same admitted subject must produce byte-identical output.
    Determinism,
    /// The artifact must identify the semantic subject and manufacturing plan that produced it.
    Provenance,
    /// At least one explicit failure condition must be executable against the artifact.
    Falsifier,
    /// The result must establish correspondence to the admitted semantic source.
    Correspondence,
    /// The resulting receipt must be independently replayable.
    ReceiptReplay,
    /// Any consequential intent remains non-authoritative until a downstream broker admits it.
    AuthorityBoundary,
}

impl CourtObligation {
    fn baseline() -> BTreeSet<Self> {
        BTreeSet::from([
            Self::Determinism,
            Self::Provenance,
            Self::Falsifier,
            Self::Correspondence,
            Self::ReceiptReplay,
            Self::AuthorityBoundary,
        ])
    }
}

/// Declarative manufacturing metadata for one projection.
///
/// `semantic_triggers` contains predicate IRIs whose additions or deletions can directly affect
/// this projection. `depends_on` expresses projection-to-projection derivation. The kernel first
/// selects direct trigger matches and then computes the transitive downstream closure, so an
/// unrelated branch is never regenerated merely because some other semantic atom changed.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProjectionSpec {
    /// Stable projection identity.
    pub id: ProjectionId,
    /// RDF predicate IRIs that directly affect this projection.
    #[serde(default)]
    pub semantic_triggers: BTreeSet<String>,
    /// Other projections whose manufactured meaning this projection consumes.
    #[serde(default)]
    pub depends_on: BTreeSet<ProjectionId>,
    /// Additional court obligations beyond the kernel baseline.
    #[serde(default)]
    pub additional_obligations: BTreeSet<CourtObligation>,
}

impl ProjectionSpec {
    /// Create an empty projection specification.
    ///
    /// # Errors
    ///
    /// Returns [`ManufacturingRefusal::EmptyProjectionId`] for a blank identifier.
    pub fn new(id: impl Into<String>) -> Result<Self, ManufacturingRefusal> {
        Ok(Self {
            id: ProjectionId::new(id)?,
            semantic_triggers: BTreeSet::new(),
            depends_on: BTreeSet::new(),
            additional_obligations: BTreeSet::new(),
        })
    }

    /// Declare an RDF predicate IRI that directly affects this projection.
    #[must_use]
    pub fn observing(mut self, predicate_iri: impl Into<String>) -> Self {
        self.semantic_triggers.insert(predicate_iri.into());
        self
    }

    /// Declare a projection dependency.
    ///
    /// # Errors
    ///
    /// Returns [`ManufacturingRefusal::EmptyProjectionId`] for a blank dependency identifier.
    pub fn depending_on(
        mut self, dependency: impl Into<String>,
    ) -> Result<Self, ManufacturingRefusal> {
        self.depends_on.insert(ProjectionId::new(dependency)?);
        Ok(self)
    }

    /// Add a target-specific verification obligation.
    #[must_use]
    pub fn requiring(mut self, obligation: CourtObligation) -> Self {
        self.additional_obligations.insert(obligation);
        self
    }
}

/// One affected projection in deterministic manufacturing order.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlannedProjection {
    /// Projection identity.
    pub id: ProjectionId,
    /// Full verification court required for this projection.
    pub court: BTreeSet<CourtObligation>,
}

/// Pure construction result of the semantic manufacturing kernel.
///
/// This type deliberately carries no execution method. It is an intent/plan that downstream code
/// may render, verify, admit, receipt, and eventually hand to BRCE; possession of a plan conveys no
/// ambient authority to change machine state.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManufacturingPlan {
    /// Canonical BLAKE3 identity of the admitted RDF delta.
    pub delta_hash_hex: String,
    /// Sorted RDF predicate IRIs changed by the delta.
    pub changed_predicates: BTreeSet<String>,
    /// Minimum dependency-closed projection sequence in stable topological order.
    pub projections: Vec<PlannedProjection>,
    /// Canonical BLAKE3 identity binding the delta and projection/court sequence.
    pub plan_hash_hex: String,
}

impl ManufacturingPlan {
    /// Whether the admitted delta requires no known projection work.
    #[must_use]
    pub fn is_noop(&self) -> bool {
        self.projections.is_empty()
    }
}

/// Typed refusal conditions for manufacture planning.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ManufacturingRefusal {
    /// A projection identity is blank and therefore cannot bind a receipt or dependency edge.
    #[error("REFUSED_MANUFACTURING_EMPTY_PROJECTION_ID")]
    EmptyProjectionId,
    /// Two projection specifications claim the same identity.
    #[error("REFUSED_MANUFACTURING_DUPLICATE_PROJECTION: {0}")]
    DuplicateProjection(ProjectionId),
    /// A projection depends on an identity absent from the admitted projection graph.
    #[error(
        "REFUSED_MANUFACTURING_UNKNOWN_DEPENDENCY: projection={projection} dependency={dependency}"
    )]
    UnknownDependency {
        /// Projection declaring the missing edge.
        projection: ProjectionId,
        /// Missing dependency identity.
        dependency: ProjectionId,
    },
    /// The projection dependency graph contains a cycle and therefore has no deterministic order.
    #[error("REFUSED_MANUFACTURING_PROJECTION_CYCLE: {members:?}")]
    ProjectionCycle {
        /// Identities remaining after deterministic Kahn reduction.
        members: Vec<ProjectionId>,
    },
    /// An RDF delta member is not a valid N-Quad and cannot be admitted as a semantic trigger.
    #[error("REFUSED_MANUFACTURING_INVALID_RDF_DELTA: {0}")]
    InvalidRdfDelta(String),
}

/// Universal-but-small semantic manufacturing planner.
#[derive(Debug, Default, Clone, Copy)]
pub struct ManufacturingKernel;

impl ManufacturingKernel {
    /// Compute the minimum deterministic projection closure for `delta`.
    ///
    /// The planner performs five bounded operations:
    ///
    /// 1. admit a unique acyclic projection graph;
    /// 2. extract changed RDF predicate IRIs from additions and deletions;
    /// 3. select projections whose semantic triggers intersect that delta;
    /// 4. close downstream dependencies transitively;
    /// 5. bind the resulting sequence and generated verification courts into a stable plan hash.
    ///
    /// # Errors
    ///
    /// Returns a typed [`ManufacturingRefusal`] when the projection graph or RDF delta is not
    /// admissible.
    pub fn plan(
        &self, delta: &RdfDelta, specs: impl IntoIterator<Item = ProjectionSpec>,
    ) -> Result<ManufacturingPlan, ManufacturingRefusal> {
        let specs = admit_projection_graph(specs)?;
        let (topological_order, downstream) = topology(&specs)?;
        let changed_predicates = changed_predicates(delta)?;

        let mut affected = BTreeSet::new();
        let mut queue = VecDeque::new();

        for spec in specs.values() {
            if !spec.semantic_triggers.is_disjoint(&changed_predicates)
                && affected.insert(spec.id.clone())
            {
                queue.push_back(spec.id.clone());
            }
        }

        while let Some(current) = queue.pop_front() {
            if let Some(children) = downstream.get(&current) {
                for child in children {
                    if affected.insert(child.clone()) {
                        queue.push_back(child.clone());
                    }
                }
            }
        }

        let projections = topological_order
            .into_iter()
            .filter(|id| affected.contains(id))
            .map(|id| {
                let mut court = CourtObligation::baseline();
                if let Some(spec) = specs.get(&id) {
                    court.extend(spec.additional_obligations.iter().copied());
                }
                PlannedProjection { id, court }
            })
            .collect::<Vec<_>>();

        let delta_hash_hex = canonical_delta_hash(delta);
        let plan_hash_hex = plan_hash(&delta_hash_hex, &changed_predicates, &projections);

        Ok(ManufacturingPlan {
            delta_hash_hex,
            changed_predicates,
            projections,
            plan_hash_hex,
        })
    }
}

fn admit_projection_graph(
    specs: impl IntoIterator<Item = ProjectionSpec>,
) -> Result<BTreeMap<ProjectionId, ProjectionSpec>, ManufacturingRefusal> {
    let mut admitted = BTreeMap::new();
    for spec in specs {
        let id = spec.id.clone();
        if admitted.insert(id.clone(), spec).is_some() {
            return Err(ManufacturingRefusal::DuplicateProjection(id));
        }
    }

    for spec in admitted.values() {
        for dependency in &spec.depends_on {
            if !admitted.contains_key(dependency) {
                return Err(ManufacturingRefusal::UnknownDependency {
                    projection: spec.id.clone(),
                    dependency: dependency.clone(),
                });
            }
        }
    }

    Ok(admitted)
}

fn topology(
    specs: &BTreeMap<ProjectionId, ProjectionSpec>,
) -> Result<
    (
        Vec<ProjectionId>,
        BTreeMap<ProjectionId, BTreeSet<ProjectionId>>,
    ),
    ManufacturingRefusal,
> {
    let mut indegree = specs
        .keys()
        .cloned()
        .map(|id| (id, 0_usize))
        .collect::<BTreeMap<_, _>>();
    let mut downstream = specs
        .keys()
        .cloned()
        .map(|id| (id, BTreeSet::new()))
        .collect::<BTreeMap<_, _>>();

    for spec in specs.values() {
        for dependency in &spec.depends_on {
            if let Some(value) = indegree.get_mut(&spec.id) {
                *value += 1;
            }
            if let Some(children) = downstream.get_mut(dependency) {
                children.insert(spec.id.clone());
            }
        }
    }

    let mut ready = indegree
        .iter()
        .filter_map(|(id, degree)| (*degree == 0).then_some(id.clone()))
        .collect::<BTreeSet<_>>();
    let mut order = Vec::with_capacity(specs.len());

    while let Some(next) = ready.iter().next().cloned() {
        ready.remove(&next);
        order.push(next.clone());

        if let Some(children) = downstream.get(&next) {
            for child in children {
                if let Some(value) = indegree.get_mut(child) {
                    *value -= 1;
                    if *value == 0 {
                        ready.insert(child.clone());
                    }
                }
            }
        }
    }

    if order.len() != specs.len() {
        let members = indegree
            .into_iter()
            .filter_map(|(id, degree)| (degree > 0).then_some(id))
            .collect();
        return Err(ManufacturingRefusal::ProjectionCycle { members });
    }

    Ok((order, downstream))
}

fn changed_predicates(delta: &RdfDelta) -> Result<BTreeSet<String>, ManufacturingRefusal> {
    delta
        .additions
        .iter()
        .chain(&delta.deletions)
        .try_fold(BTreeSet::new(), |mut predicates, nquad| {
            let quad = parse_nquad(nquad).map_err(|error| {
                ManufacturingRefusal::InvalidRdfDelta(format!("{nquad}: {error}"))
            })?;
            predicates.insert(quad.predicate.as_str().to_owned());
            Ok(predicates)
        })
}

fn canonical_delta_hash(delta: &RdfDelta) -> String {
    let mut additions = delta.additions.clone();
    let mut deletions = delta.deletions.clone();
    additions.sort();
    deletions.sort();

    let mut hasher = blake3::Hasher::new();
    hasher.update(b"ggen-semantic-delta/v1\0");
    for addition in additions {
        hasher.update(b"+\0");
        hasher.update(addition.as_bytes());
        hasher.update(b"\0");
    }
    for deletion in deletions {
        hasher.update(b"-\0");
        hasher.update(deletion.as_bytes());
        hasher.update(b"\0");
    }
    hasher.finalize().to_hex().to_string()
}

fn plan_hash(
    delta_hash_hex: &str, changed_predicates: &BTreeSet<String>,
    projections: &[PlannedProjection],
) -> String {
    let mut hasher = blake3::Hasher::new();
    hasher.update(b"ggen-manufacturing-plan/v1\0");
    hasher.update(delta_hash_hex.as_bytes());
    hasher.update(b"\0");

    for predicate in changed_predicates {
        hasher.update(b"predicate\0");
        hasher.update(predicate.as_bytes());
        hasher.update(b"\0");
    }

    for projection in projections {
        hasher.update(b"projection\0");
        hasher.update(projection.id.as_str().as_bytes());
        hasher.update(b"\0");
        for obligation in &projection.court {
            hasher.update(format!("{obligation:?}").as_bytes());
            hasher.update(b"\0");
        }
    }

    hasher.finalize().to_hex().to_string()
}
