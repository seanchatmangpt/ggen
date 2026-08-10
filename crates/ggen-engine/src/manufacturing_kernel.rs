//! Deterministic semantic-delta planning for Vision 2030 manufacture.
//!
//! This module is intentionally smaller than a generator framework. It consumes an already
//! admitted semantic-delta identity plus the semantic predicates changed by that delta, then
//! computes the minimum dependency-closed projection plan that must be manufactured next. The
//! concrete observation carrier (RDF, process log, protocol schema, or another admitted source)
//! remains outside this kernel.
//!
//! The kernel does not write files, invoke processes, call networks, or actuate external systems.
//! Its output is a deterministic construction plan plus the verification court every affected
//! projection must satisfy before any downstream authority boundary may execute consequence.

use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    fmt,
};

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

/// An admitted, content-addressed semantic change set.
///
/// The observation/admission layer owns extraction from concrete carriers. For example,
/// `ggen-graph` can derive changed RDF predicate IRIs from an `RdfDelta` and supply its canonical
/// transition identity here. The manufacturing kernel therefore depends on semantic meaning, not
/// the storage or wire format that happened to carry it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdmittedSemanticDelta {
    /// Stable identity of the admitted change set (normally a content hash or receipt identity).
    pub identity: String,
    /// Sorted semantic predicate identifiers changed by the admitted delta.
    pub changed_predicates: BTreeSet<String>,
}

impl AdmittedSemanticDelta {
    /// Construct an admitted semantic delta from its upstream identity and changed predicates.
    ///
    /// # Errors
    ///
    /// Refuses blank delta identity or blank semantic predicates. The same checks are repeated by
    /// [`ManufacturingKernel::plan`] so deserialized values cannot bypass admission.
    pub fn new<I, S>(
        identity: impl Into<String>, changed_predicates: I,
    ) -> Result<Self, ManufacturingRefusal>
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        let delta = Self {
            identity: identity.into(),
            changed_predicates: changed_predicates.into_iter().map(Into::into).collect(),
        };
        delta.validate()?;
        Ok(delta)
    }

    fn validate(&self) -> Result<(), ManufacturingRefusal> {
        if self.identity.trim().is_empty() {
            return Err(ManufacturingRefusal::EmptyDeltaIdentity);
        }
        if self
            .changed_predicates
            .iter()
            .any(|predicate| predicate.trim().is_empty())
        {
            return Err(ManufacturingRefusal::EmptyDeltaPredicate);
        }
        Ok(())
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
/// `semantic_triggers` contains semantic predicate identifiers that can directly affect this
/// projection. `depends_on` expresses projection-to-projection derivation. The kernel first
/// selects direct trigger matches and then computes the transitive downstream closure, so an
/// unrelated branch is never regenerated merely because some other semantic atom changed.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProjectionSpec {
    /// Stable projection identity.
    pub id: ProjectionId,
    /// Semantic predicate identifiers that directly affect this projection.
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

    /// Declare a semantic predicate identifier that directly affects this projection.
    #[must_use]
    pub fn observing(mut self, predicate: impl Into<String>) -> Self {
        self.semantic_triggers.insert(predicate.into());
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
    /// Upstream identity of the admitted semantic delta.
    pub delta_identity: String,
    /// Sorted semantic predicates changed by the admitted delta.
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
    /// An admitted semantic delta lacks a stable upstream identity.
    #[error("REFUSED_MANUFACTURING_EMPTY_DELTA_IDENTITY")]
    EmptyDeltaIdentity,
    /// An admitted semantic delta contains a blank changed predicate identifier.
    #[error("REFUSED_MANUFACTURING_EMPTY_DELTA_PREDICATE")]
    EmptyDeltaPredicate,
    /// A projection declares a blank semantic trigger.
    #[error("REFUSED_MANUFACTURING_EMPTY_PROJECTION_TRIGGER: {0}")]
    EmptyProjectionTrigger(ProjectionId),
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
}

/// Universal-but-small semantic manufacturing planner.
#[derive(Debug, Default, Clone, Copy)]
pub struct ManufacturingKernel;

impl ManufacturingKernel {
    /// Compute the minimum deterministic projection closure for `delta`.
    ///
    /// The planner performs five bounded operations:
    ///
    /// 1. re-admit the semantic-delta boundary and a unique acyclic projection graph;
    /// 2. select projections whose semantic triggers intersect the changed predicates;
    /// 3. close downstream dependencies transitively;
    /// 4. generate the required verification court for each selected projection;
    /// 5. bind the delta identity, predicates, sequence, and courts into a stable plan hash.
    ///
    /// # Errors
    ///
    /// Returns a typed [`ManufacturingRefusal`] when the admitted delta or projection graph is not
    /// valid for deterministic planning.
    pub fn plan(
        &self, delta: &AdmittedSemanticDelta,
        specs: impl IntoIterator<Item = ProjectionSpec>,
    ) -> Result<ManufacturingPlan, ManufacturingRefusal> {
        delta.validate()?;
        let specs = admit_projection_graph(specs)?;
        let (topological_order, downstream) = topology(&specs)?;

        let mut affected = BTreeSet::new();
        let mut queue = VecDeque::new();

        for spec in specs.values() {
            if !spec
                .semantic_triggers
                .is_disjoint(&delta.changed_predicates)
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

        let plan_hash_hex = plan_hash(
            &delta.identity,
            &delta.changed_predicates,
            &projections,
        );

        Ok(ManufacturingPlan {
            delta_identity: delta.identity.clone(),
            changed_predicates: delta.changed_predicates.clone(),
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
        if spec
            .semantic_triggers
            .iter()
            .any(|trigger| trigger.trim().is_empty())
        {
            return Err(ManufacturingRefusal::EmptyProjectionTrigger(
                spec.id.clone(),
            ));
        }
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

fn plan_hash(
    delta_identity: &str, changed_predicates: &BTreeSet<String>,
    projections: &[PlannedProjection],
) -> String {
    let mut hasher = blake3::Hasher::new();
    hasher.update(b"ggen-manufacturing-plan/v1\0");
    hasher.update(delta_identity.as_bytes());
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
