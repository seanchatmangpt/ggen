//! Deterministic enterprise-architecture calculus.
//!
//! This module models the enterprise graph above the Fortune 5 operational
//! profile.  It is deliberately IO-free and authority-free: it admits facts,
//! validates graph integrity, derives governance/impact/portfolio views,
//! manufactures transition orders and deterministic receipts, but never
//! actuates an external system.

use crate::building_block::{LifecycleState, Standing};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use thiserror::Error;

/// Receipt schema for deterministic enterprise-architecture snapshots.
pub const ENTERPRISE_ARCHITECTURE_RECEIPT_SCHEMA: &str = "ggen.enterprise-architecture.receipt.v1";

/// Stable architecture element identity.
pub type ElementId = String;
/// Stable architecture relationship identity.
pub type RelationId = String;
/// Stable transition architecture identity.
pub type TransitionId = String;
/// Stable work-package identity.
pub type WorkPackageId = String;

/// Public semantic vocabulary terms used for optional alignment.
///
/// These are identifiers only.  This crate never dereferences them and grants
/// no authority to data merely because it carries one of these types.
pub mod public_vocabulary {
    /// W3C PROV-O Entity.
    pub const PROV_ENTITY: &str = "http://www.w3.org/ns/prov#Entity";
    /// W3C PROV-O Activity.
    pub const PROV_ACTIVITY: &str = "http://www.w3.org/ns/prov#Activity";
    /// W3C Organization Ontology Organization.
    pub const ORG_ORGANIZATION: &str = "http://www.w3.org/ns/org#Organization";
    /// W3C Organization Ontology Role.
    pub const ORG_ROLE: &str = "http://www.w3.org/ns/org#Role";
    /// SKOS Concept.
    pub const SKOS_CONCEPT: &str = "http://www.w3.org/2004/02/skos/core#Concept";
    /// DCAT Dataset.
    pub const DCAT_DATASET: &str = "http://www.w3.org/ns/dcat#Dataset";
    /// ODRL Policy.
    pub const ODRL_POLICY: &str = "http://www.w3.org/ns/odrl/2/Policy";
    /// SOSA ObservableProperty.
    pub const SOSA_OBSERVABLE_PROPERTY: &str = "http://www.w3.org/ns/sosa/ObservableProperty";
}

/// Enterprise-architecture layer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ArchitectureLayer {
    Enterprise,
    Motivation,
    Strategy,
    Business,
    Information,
    Application,
    Technology,
    ImplementationMigration,
    Governance,
    Evidence,
}

/// Canonical enterprise-architecture element taxonomy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum EnterpriseElementKind {
    Enterprise,
    Boundary,
    Principle,
    Requirement,
    Standard,
    Capability,
    ValueStream,
    Product,
    Service,
    OrganizationUnit,
    BusinessActor,
    BusinessRole,
    BusinessProcess,
    InformationConcept,
    Dataset,
    Ontology,
    Application,
    Component,
    Interface,
    Technology,
    Platform,
    Environment,
    Repository,
    Pack,
    Query,
    Template,
    Projection,
    Plan,
    WorkPackage,
    TransitionArchitecture,
    Decision,
    Claim,
    Evidence,
    Receipt,
    Metric,
    Risk,
    Control,
    Exception,
    Migration,
}

impl EnterpriseElementKind {
    /// Whether a governed instance requires an owner or steward.
    #[must_use]
    pub const fn requires_accountability(self) -> bool {
        matches!(
            self,
            Self::Principle
                | Self::Requirement
                | Self::Standard
                | Self::Capability
                | Self::ValueStream
                | Self::Product
                | Self::Service
                | Self::BusinessProcess
                | Self::InformationConcept
                | Self::Dataset
                | Self::Ontology
                | Self::Application
                | Self::Component
                | Self::Interface
                | Self::Technology
                | Self::Platform
                | Self::Environment
                | Self::Repository
                | Self::Plan
                | Self::WorkPackage
                | Self::TransitionArchitecture
                | Self::Decision
                | Self::Risk
                | Self::Control
                | Self::Exception
                | Self::Migration
        )
    }

    /// Default public vocabulary alignment, when one is unambiguous.
    #[must_use]
    pub const fn public_semantic_type(self) -> Option<&'static str> {
        use public_vocabulary::{
            DCAT_DATASET, ODRL_POLICY, ORG_ORGANIZATION, ORG_ROLE, PROV_ACTIVITY, PROV_ENTITY,
            SKOS_CONCEPT, SOSA_OBSERVABLE_PROPERTY,
        };
        match self {
            Self::Enterprise | Self::OrganizationUnit => Some(ORG_ORGANIZATION),
            Self::BusinessRole => Some(ORG_ROLE),
            Self::InformationConcept | Self::Capability => Some(SKOS_CONCEPT),
            Self::Dataset => Some(DCAT_DATASET),
            Self::Principle | Self::Requirement | Self::Standard | Self::Control => {
                Some(ODRL_POLICY)
            }
            Self::Plan | Self::WorkPackage | Self::TransitionArchitecture | Self::Migration => {
                Some(PROV_ACTIVITY)
            }
            Self::Claim | Self::Evidence | Self::Receipt => Some(PROV_ENTITY),
            Self::Metric => Some(SOSA_OBSERVABLE_PROPERTY),
            _ => None,
        }
    }
}

/// One durable enterprise-architecture fact.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnterpriseElement {
    pub id: ElementId,
    pub name: String,
    pub kind: EnterpriseElementKind,
    pub layer: ArchitectureLayer,
    pub lifecycle: LifecycleState,
    pub standing: Standing,
    /// Owner identity.  When it references another element it should identify
    /// an OrganizationUnit, BusinessActor, or BusinessRole.
    #[serde(default)]
    pub owner: Option<ElementId>,
    /// Steward identity, used when day-to-day stewardship differs from ownership.
    #[serde(default)]
    pub steward: Option<ElementId>,
    /// Public/canonical semantic types carried by the admitted fact.
    #[serde(default)]
    pub semantic_types: BTreeSet<String>,
    /// Source observation identifiers (URI, commit, ticket, receipt, dataset, ...).
    #[serde(default)]
    pub source_refs: BTreeSet<String>,
    #[serde(default)]
    pub tags: BTreeMap<String, String>,
}

impl EnterpriseElement {
    /// Construct an element with deterministic defaults and its public semantic
    /// alignment, when the element kind has one.
    #[must_use]
    pub fn new(
        id: impl Into<String>, name: impl Into<String>, kind: EnterpriseElementKind,
        layer: ArchitectureLayer,
    ) -> Self {
        let mut semantic_types = BTreeSet::new();
        if let Some(term) = kind.public_semantic_type() {
            semantic_types.insert(term.to_owned());
        }
        Self {
            id: id.into(),
            name: name.into(),
            kind,
            layer,
            lifecycle: LifecycleState::Identified,
            standing: Standing::Unknown,
            owner: None,
            steward: None,
            semantic_types,
            source_refs: BTreeSet::new(),
            tags: BTreeMap::new(),
        }
    }

    fn is_governed(&self) -> bool {
        matches!(
            self.lifecycle,
            LifecycleState::Qualified
                | LifecycleState::Admitted
                | LifecycleState::Active
                | LifecycleState::Deprecated
        )
    }
}

/// Directional relationship.  Names read naturally from source to target.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum EnterpriseRelationKind {
    Contains,
    Owns,
    Stewards,
    Constrains,
    RealizedBy,
    ImplementedBy,
    Uses,
    DependsOn,
    FlowsTo,
    HostedBy,
    ManufacturedBy,
    Imports,
    DeployedTo,
    ObservedBy,
    EvidencedBy,
    Satisfies,
    Mitigates,
    Governs,
    Supersedes,
    MigratesTo,
    Delivers,
    Enables,
    Impacts,
    AssociatedWith,
}

impl EnterpriseRelationKind {
    /// Relationships that propagate architecture impact.
    #[must_use]
    pub const fn propagates_impact(self) -> bool {
        !matches!(
            self,
            Self::EvidencedBy | Self::Owns | Self::Stewards | Self::AssociatedWith
        )
    }

    /// Relationships admitted in cross-layer traceability.
    #[must_use]
    pub const fn is_traceability_edge(self) -> bool {
        matches!(
            self,
            Self::Constrains
                | Self::RealizedBy
                | Self::ImplementedBy
                | Self::Uses
                | Self::DependsOn
                | Self::HostedBy
                | Self::ManufacturedBy
                | Self::Imports
                | Self::DeployedTo
                | Self::ObservedBy
                | Self::EvidencedBy
                | Self::Satisfies
                | Self::Mitigates
                | Self::Governs
                | Self::Supersedes
                | Self::MigratesTo
                | Self::Delivers
                | Self::Enables
        )
    }
}

/// One admitted relationship between architecture elements.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnterpriseRelation {
    pub id: RelationId,
    pub from: ElementId,
    pub to: ElementId,
    pub kind: EnterpriseRelationKind,
    #[serde(default)]
    pub rationale: Option<String>,
    #[serde(default)]
    pub evidence: BTreeSet<String>,
}

/// Architecture viewpoint.  A view is disposable projection; the graph remains
/// the editing authority.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ArchitectureViewpoint {
    Motivation,
    Strategy,
    Business,
    Information,
    Application,
    Technology,
    ImplementationMigration,
    Governance,
    Evidence,
    Full,
}

impl ArchitectureViewpoint {
    fn includes(self, element: &EnterpriseElement) -> bool {
        match self {
            Self::Full => true,
            Self::Motivation => element.layer == ArchitectureLayer::Motivation,
            Self::Strategy => matches!(
                element.layer,
                ArchitectureLayer::Enterprise
                    | ArchitectureLayer::Motivation
                    | ArchitectureLayer::Strategy
            ),
            Self::Business => element.layer == ArchitectureLayer::Business,
            Self::Information => element.layer == ArchitectureLayer::Information,
            Self::Application => element.layer == ArchitectureLayer::Application,
            Self::Technology => element.layer == ArchitectureLayer::Technology,
            Self::ImplementationMigration => {
                element.layer == ArchitectureLayer::ImplementationMigration
            }
            Self::Governance => {
                matches!(
                    element.layer,
                    ArchitectureLayer::Motivation | ArchitectureLayer::Governance
                ) || matches!(
                    element.kind,
                    EnterpriseElementKind::Principle
                        | EnterpriseElementKind::Requirement
                        | EnterpriseElementKind::Standard
                        | EnterpriseElementKind::Risk
                        | EnterpriseElementKind::Control
                        | EnterpriseElementKind::Exception
                        | EnterpriseElementKind::Decision
                )
            }
            Self::Evidence => element.layer == ArchitectureLayer::Evidence,
        }
    }
}

/// Deterministic projected view.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArchitectureView {
    pub viewpoint: ArchitectureViewpoint,
    pub elements: BTreeMap<ElementId, EnterpriseElement>,
    pub relations: BTreeMap<RelationId, EnterpriseRelation>,
}

/// One traversal edge with depth from the requested subject.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TraceHop {
    pub depth: usize,
    pub relation_id: RelationId,
    pub from: ElementId,
    pub kind: EnterpriseRelationKind,
    pub to: ElementId,
}

/// Impact closure derived from the admitted architecture graph.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnterpriseImpactReport {
    pub subject: ElementId,
    pub impacted: BTreeSet<ElementId>,
    pub traversed_relations: BTreeSet<RelationId>,
}

/// One reversible implementation step in a transition architecture.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnterpriseWorkPackage {
    pub id: WorkPackageId,
    pub name: String,
    pub owner: ElementId,
    #[serde(default)]
    pub depends_on: BTreeSet<WorkPackageId>,
    #[serde(default)]
    pub delivers: BTreeSet<ElementId>,
    #[serde(default)]
    pub retires: BTreeSet<ElementId>,
    #[serde(default)]
    pub evidence_obligations: BTreeSet<String>,
}

/// Baseline → target transition with a dependency-closed work-package graph.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnterpriseTransition {
    pub id: TransitionId,
    pub name: String,
    #[serde(default)]
    pub baseline: BTreeSet<ElementId>,
    #[serde(default)]
    pub target: BTreeSet<ElementId>,
    #[serde(default)]
    pub work_packages: BTreeMap<WorkPackageId, EnterpriseWorkPackage>,
}

/// Severity of a static enterprise governance finding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum GovernanceSeverity {
    Advisory,
    Required,
    Blocking,
}

/// Enterprise governance finding.
///
/// A finding is not execution evidence and therefore never promotes a subject
/// to ALIVE.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GovernanceFinding {
    pub code: String,
    pub severity: GovernanceSeverity,
    pub subject: ElementId,
    pub message: String,
}

/// Structural model violation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnterpriseArchitectureViolation {
    pub code: String,
    pub subject: String,
    pub message: String,
}

/// Deterministic static governance assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GovernanceAssessment {
    pub structurally_valid: bool,
    pub findings: Vec<GovernanceFinding>,
    /// True means static architecture obligations are closed.  It does not mean
    /// runtime behavior has executed or reached ALIVE standing.
    pub statically_complete: bool,
}

/// Deterministic architecture snapshot receipt.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnterpriseArchitectureReceipt {
    pub schema: String,
    pub subject_digest: String,
    #[serde(default)]
    pub predecessor_digest: Option<String>,
    pub operation: String,
    pub consequence_digest: String,
}

/// Errors at explicit admission/query boundaries.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum EnterpriseArchitectureError {
    #[error("REFUSED_EA_EMPTY_ID: {kind} identity must not be empty")]
    EmptyId { kind: &'static str },
    #[error("REFUSED_EA_DUPLICATE: duplicate {kind} identity `{id}`")]
    Duplicate { kind: &'static str, id: String },
    #[error("REFUSED_EA_UNKNOWN_ELEMENT: `{0}` is not admitted")]
    UnknownElement(String),
    #[error("REFUSED_EA_UNKNOWN_TRANSITION: `{0}` is not admitted")]
    UnknownTransition(String),
    #[error("REFUSED_EA_TRANSITION_CYCLE: transition `{0}` contains a work-package cycle")]
    TransitionCycle(String),
    #[error(
        "REFUSED_EA_UNKNOWN_WORK_PACKAGE: transition `{transition}` references work package `{work_package}`"
    )]
    UnknownWorkPackage {
        transition: String,
        work_package: String,
    },
    #[error("REFUSED_EA_SERIALIZATION: {0}")]
    Serialization(String),
}

/// Canonical enterprise architecture graph.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct EnterpriseArchitectureModel {
    pub enterprise_id: ElementId,
    pub boundary_id: ElementId,
    #[serde(default)]
    pub elements: BTreeMap<ElementId, EnterpriseElement>,
    #[serde(default)]
    pub relations: BTreeMap<RelationId, EnterpriseRelation>,
    #[serde(default)]
    pub transitions: BTreeMap<TransitionId, EnterpriseTransition>,
}

impl EnterpriseArchitectureModel {
    /// Construct an empty model with stable enterprise and boundary identities.
    #[must_use]
    pub fn new(enterprise_id: impl Into<String>, boundary_id: impl Into<String>) -> Self {
        Self {
            enterprise_id: enterprise_id.into(),
            boundary_id: boundary_id.into(),
            elements: BTreeMap::new(),
            relations: BTreeMap::new(),
            transitions: BTreeMap::new(),
        }
    }

    /// Admit an element without overwriting an existing identity.
    pub fn admit_element(
        &mut self, element: EnterpriseElement,
    ) -> Result<(), EnterpriseArchitectureError> {
        if element.id.trim().is_empty() {
            return Err(EnterpriseArchitectureError::EmptyId { kind: "element" });
        }
        if self.elements.contains_key(&element.id) {
            return Err(EnterpriseArchitectureError::Duplicate {
                kind: "element",
                id: element.id,
            });
        }
        self.elements.insert(element.id.clone(), element);
        Ok(())
    }

    /// Admit a relationship only when both endpoints already exist.
    pub fn admit_relation(
        &mut self, relation: EnterpriseRelation,
    ) -> Result<(), EnterpriseArchitectureError> {
        if relation.id.trim().is_empty() {
            return Err(EnterpriseArchitectureError::EmptyId { kind: "relation" });
        }
        if self.relations.contains_key(&relation.id) {
            return Err(EnterpriseArchitectureError::Duplicate {
                kind: "relation",
                id: relation.id,
            });
        }
        if !self.elements.contains_key(&relation.from) {
            return Err(EnterpriseArchitectureError::UnknownElement(relation.from));
        }
        if !self.elements.contains_key(&relation.to) {
            return Err(EnterpriseArchitectureError::UnknownElement(relation.to));
        }
        self.relations.insert(relation.id.clone(), relation);
        Ok(())
    }

    /// Admit a transition architecture without replacing one with the same identity.
    pub fn admit_transition(
        &mut self, transition: EnterpriseTransition,
    ) -> Result<(), EnterpriseArchitectureError> {
        if transition.id.trim().is_empty() {
            return Err(EnterpriseArchitectureError::EmptyId { kind: "transition" });
        }
        if self.transitions.contains_key(&transition.id) {
            return Err(EnterpriseArchitectureError::Duplicate {
                kind: "transition",
                id: transition.id,
            });
        }
        self.transitions.insert(transition.id.clone(), transition);
        Ok(())
    }

    /// Validate structural graph integrity.  This is admission evidence, not
    /// runtime execution evidence.
    #[must_use]
    pub fn validate(&self) -> Vec<EnterpriseArchitectureViolation> {
        let mut violations = Vec::new();

        match self.elements.get(&self.enterprise_id) {
            Some(element) if element.kind == EnterpriseElementKind::Enterprise => {}
            Some(_) => violations.push(EnterpriseArchitectureViolation {
                code: "EA-STRUCT-001".to_owned(),
                subject: self.enterprise_id.clone(),
                message: "enterprise_id must identify an Enterprise element".to_owned(),
            }),
            None => violations.push(EnterpriseArchitectureViolation {
                code: "EA-STRUCT-001".to_owned(),
                subject: self.enterprise_id.clone(),
                message: "enterprise_id does not identify an admitted element".to_owned(),
            }),
        }

        match self.elements.get(&self.boundary_id) {
            Some(element) if element.kind == EnterpriseElementKind::Boundary => {}
            Some(_) => violations.push(EnterpriseArchitectureViolation {
                code: "EA-STRUCT-002".to_owned(),
                subject: self.boundary_id.clone(),
                message: "boundary_id must identify a Boundary element".to_owned(),
            }),
            None => violations.push(EnterpriseArchitectureViolation {
                code: "EA-STRUCT-002".to_owned(),
                subject: self.boundary_id.clone(),
                message: "boundary_id does not identify an admitted element".to_owned(),
            }),
        }

        for (id, element) in &self.elements {
            if element.id != *id {
                violations.push(EnterpriseArchitectureViolation {
                    code: "EA-STRUCT-003".to_owned(),
                    subject: id.clone(),
                    message: "element map key differs from element identity".to_owned(),
                });
            }
            if element.name.trim().is_empty() {
                violations.push(EnterpriseArchitectureViolation {
                    code: "EA-STRUCT-004".to_owned(),
                    subject: id.clone(),
                    message: "element name must not be empty".to_owned(),
                });
            }
            if element.kind.requires_accountability()
                && element.is_governed()
                && element.owner.is_none()
                && element.steward.is_none()
            {
                violations.push(EnterpriseArchitectureViolation {
                    code: "EA-STRUCT-005".to_owned(),
                    subject: id.clone(),
                    message: "governed element requires an owner or steward".to_owned(),
                });
            }
            for accountable in element.owner.iter().chain(element.steward.iter()) {
                match self.elements.get(accountable) {
                    Some(target)
                        if matches!(
                            target.kind,
                            EnterpriseElementKind::OrganizationUnit
                                | EnterpriseElementKind::BusinessActor
                                | EnterpriseElementKind::BusinessRole
                        ) => {}
                    Some(_) => violations.push(EnterpriseArchitectureViolation {
                        code: "EA-STRUCT-006".to_owned(),
                        subject: id.clone(),
                        message: format!(
                            "accountability target `{accountable}` must be an organization, actor, or role"
                        ),
                    }),
                    None => violations.push(EnterpriseArchitectureViolation {
                        code: "EA-STRUCT-006".to_owned(),
                        subject: id.clone(),
                        message: format!(
                            "accountability target `{accountable}` is not admitted"
                        ),
                    }),
                }
            }
        }

        for (id, relation) in &self.relations {
            if relation.id != *id {
                violations.push(EnterpriseArchitectureViolation {
                    code: "EA-STRUCT-007".to_owned(),
                    subject: id.clone(),
                    message: "relation map key differs from relation identity".to_owned(),
                });
            }
            if !self.elements.contains_key(&relation.from) {
                violations.push(EnterpriseArchitectureViolation {
                    code: "EA-STRUCT-008".to_owned(),
                    subject: id.clone(),
                    message: format!("source `{}` is not admitted", relation.from),
                });
            }
            if !self.elements.contains_key(&relation.to) {
                violations.push(EnterpriseArchitectureViolation {
                    code: "EA-STRUCT-009".to_owned(),
                    subject: id.clone(),
                    message: format!("target `{}` is not admitted", relation.to),
                });
            }
        }

        for (transition_id, transition) in &self.transitions {
            if transition.id != *transition_id {
                violations.push(EnterpriseArchitectureViolation {
                    code: "EA-STRUCT-010".to_owned(),
                    subject: transition_id.clone(),
                    message: "transition map key differs from transition identity".to_owned(),
                });
            }
            for element_id in transition
                .baseline
                .iter()
                .chain(transition.target.iter())
                .chain(
                    transition
                        .work_packages
                        .values()
                        .flat_map(|work| work.delivers.iter().chain(work.retires.iter())),
                )
            {
                if !self.elements.contains_key(element_id) {
                    violations.push(EnterpriseArchitectureViolation {
                        code: "EA-STRUCT-011".to_owned(),
                        subject: transition_id.clone(),
                        message: format!("transition references unknown element `{element_id}`"),
                    });
                }
            }
            for work in transition.work_packages.values() {
                if !self.elements.contains_key(&work.owner) {
                    violations.push(EnterpriseArchitectureViolation {
                        code: "EA-STRUCT-012".to_owned(),
                        subject: work.id.clone(),
                        message: format!("work-package owner `{}` is not admitted", work.owner),
                    });
                }
                for dependency in &work.depends_on {
                    if !transition.work_packages.contains_key(dependency) {
                        violations.push(EnterpriseArchitectureViolation {
                            code: "EA-STRUCT-013".to_owned(),
                            subject: work.id.clone(),
                            message: format!(
                                "work-package dependency `{dependency}` is not in transition"
                            ),
                        });
                    }
                }
            }
            if matches!(
                self.transition_order(transition_id),
                Err(EnterpriseArchitectureError::TransitionCycle(_))
            ) {
                violations.push(EnterpriseArchitectureViolation {
                    code: "EA-STRUCT-014".to_owned(),
                    subject: transition_id.clone(),
                    message: "work-package dependency graph contains a cycle".to_owned(),
                });
            }
        }

        violations.sort_by(|left, right| {
            (&left.code, &left.subject, &left.message).cmp(&(
                &right.code,
                &right.subject,
                &right.message,
            ))
        });
        violations
    }

    /// Static governance closure.  This does not execute any governed subject.
    #[must_use]
    pub fn assess_governance(&self) -> GovernanceAssessment {
        let structural = self.validate();
        let mut findings = Vec::new();

        for element in self
            .elements
            .values()
            .filter(|element| element.is_governed())
        {
            if element.kind == EnterpriseElementKind::Capability
                && !self.has_outgoing(element.id.as_str(), EnterpriseRelationKind::RealizedBy)
            {
                findings.push(GovernanceFinding {
                    code: "EA-GOV-001".to_owned(),
                    severity: GovernanceSeverity::Required,
                    subject: element.id.clone(),
                    message: "governed capability has no admitted realization".to_owned(),
                });
            }
            if element.kind == EnterpriseElementKind::Requirement
                && !self.has_incoming(element.id.as_str(), EnterpriseRelationKind::Satisfies)
            {
                findings.push(GovernanceFinding {
                    code: "EA-GOV-002".to_owned(),
                    severity: GovernanceSeverity::Required,
                    subject: element.id.clone(),
                    message: "governed requirement has no satisfying element".to_owned(),
                });
            }
            if element.kind == EnterpriseElementKind::Risk
                && !self.has_incoming(element.id.as_str(), EnterpriseRelationKind::Mitigates)
            {
                findings.push(GovernanceFinding {
                    code: "EA-GOV-003".to_owned(),
                    severity: GovernanceSeverity::Blocking,
                    subject: element.id.clone(),
                    message: "governed risk has no admitted mitigation".to_owned(),
                });
            }
            if element.kind == EnterpriseElementKind::Claim
                && !self.has_outgoing(element.id.as_str(), EnterpriseRelationKind::EvidencedBy)
            {
                findings.push(GovernanceFinding {
                    code: "EA-GOV-004".to_owned(),
                    severity: GovernanceSeverity::Required,
                    subject: element.id.clone(),
                    message: "governed claim has no evidence relation".to_owned(),
                });
            }
            if element.kind == EnterpriseElementKind::Service
                && !self.has_outgoing(element.id.as_str(), EnterpriseRelationKind::ObservedBy)
            {
                findings.push(GovernanceFinding {
                    code: "EA-GOV-005".to_owned(),
                    severity: GovernanceSeverity::Advisory,
                    subject: element.id.clone(),
                    message: "governed service has no architecture-level observability relation"
                        .to_owned(),
                });
            }
            if element.kind == EnterpriseElementKind::Exception
                && !self.has_outgoing(element.id.as_str(), EnterpriseRelationKind::EvidencedBy)
            {
                findings.push(GovernanceFinding {
                    code: "EA-GOV-006".to_owned(),
                    severity: GovernanceSeverity::Required,
                    subject: element.id.clone(),
                    message: "governed exception has no evidence/approval relation".to_owned(),
                });
            }
        }

        findings.sort_by(|left, right| {
            (&left.code, &left.subject, &left.message).cmp(&(
                &right.code,
                &right.subject,
                &right.message,
            ))
        });
        GovernanceAssessment {
            structurally_valid: structural.is_empty(),
            statically_complete: structural.is_empty()
                && !findings
                    .iter()
                    .any(|finding| finding.severity != GovernanceSeverity::Advisory),
            findings,
        }
    }

    /// Deterministic outgoing cross-layer traceability traversal.
    pub fn traceability(
        &self, subject: &str, max_depth: usize,
    ) -> Result<Vec<TraceHop>, EnterpriseArchitectureError> {
        if !self.elements.contains_key(subject) {
            return Err(EnterpriseArchitectureError::UnknownElement(
                subject.to_owned(),
            ));
        }
        let mut queue = VecDeque::from([(subject.to_owned(), 0usize)]);
        let mut visited = BTreeSet::from([subject.to_owned()]);
        let mut hops = Vec::new();

        while let Some((current, depth)) = queue.pop_front() {
            if depth >= max_depth {
                continue;
            }
            let mut outgoing: Vec<&EnterpriseRelation> = self
                .relations
                .values()
                .filter(|relation| relation.from == current && relation.kind.is_traceability_edge())
                .collect();
            outgoing.sort_by(|left, right| {
                (&left.kind, &left.to, &left.id).cmp(&(&right.kind, &right.to, &right.id))
            });
            for relation in outgoing {
                hops.push(TraceHop {
                    depth: depth + 1,
                    relation_id: relation.id.clone(),
                    from: relation.from.clone(),
                    kind: relation.kind,
                    to: relation.to.clone(),
                });
                if visited.insert(relation.to.clone()) {
                    queue.push_back((relation.to.clone(), depth + 1));
                }
            }
        }
        Ok(hops)
    }

    /// Compute bidirectional transitive impact across relationships whose
    /// semantics propagate architecture change.
    pub fn impact(
        &self, subject: &str,
    ) -> Result<EnterpriseImpactReport, EnterpriseArchitectureError> {
        if !self.elements.contains_key(subject) {
            return Err(EnterpriseArchitectureError::UnknownElement(
                subject.to_owned(),
            ));
        }
        let mut queue = VecDeque::from([subject.to_owned()]);
        let mut visited = BTreeSet::from([subject.to_owned()]);
        let mut traversed_relations = BTreeSet::new();

        while let Some(current) = queue.pop_front() {
            let mut edges: Vec<&EnterpriseRelation> = self
                .relations
                .values()
                .filter(|relation| {
                    relation.kind.propagates_impact()
                        && (relation.from == current || relation.to == current)
                })
                .collect();
            edges.sort_by(|left, right| left.id.cmp(&right.id));
            for relation in edges {
                traversed_relations.insert(relation.id.clone());
                let neighbor = if relation.from == current {
                    &relation.to
                } else {
                    &relation.from
                };
                if visited.insert(neighbor.clone()) {
                    queue.push_back(neighbor.clone());
                }
            }
        }
        visited.remove(subject);
        Ok(EnterpriseImpactReport {
            subject: subject.to_owned(),
            impacted: visited,
            traversed_relations,
        })
    }

    /// Build a capability → realization portfolio matrix.
    #[must_use]
    pub fn capability_portfolio(&self) -> BTreeMap<ElementId, BTreeSet<ElementId>> {
        self.elements
            .values()
            .filter(|element| element.kind == EnterpriseElementKind::Capability)
            .map(|capability| {
                let realizations = self
                    .relations
                    .values()
                    .filter(|relation| {
                        relation.from == capability.id
                            && relation.kind == EnterpriseRelationKind::RealizedBy
                    })
                    .map(|relation| relation.to.clone())
                    .collect();
                (capability.id.clone(), realizations)
            })
            .collect()
    }

    /// Project a disposable deterministic architecture view.
    #[must_use]
    pub fn project(&self, viewpoint: ArchitectureViewpoint) -> ArchitectureView {
        let elements: BTreeMap<_, _> = self
            .elements
            .iter()
            .filter(|(_, element)| viewpoint.includes(element))
            .map(|(id, element)| (id.clone(), element.clone()))
            .collect();
        let relations = self
            .relations
            .iter()
            .filter(|(_, relation)| {
                elements.contains_key(&relation.from) && elements.contains_key(&relation.to)
            })
            .map(|(id, relation)| (id.clone(), relation.clone()))
            .collect();
        ArchitectureView {
            viewpoint,
            elements,
            relations,
        }
    }

    /// Deterministically topologically order a transition's work packages.
    pub fn transition_order(
        &self, transition_id: &str,
    ) -> Result<Vec<WorkPackageId>, EnterpriseArchitectureError> {
        let transition = self.transitions.get(transition_id).ok_or_else(|| {
            EnterpriseArchitectureError::UnknownTransition(transition_id.to_owned())
        })?;

        let mut indegree: BTreeMap<&str, usize> = transition
            .work_packages
            .keys()
            .map(|id| (id.as_str(), 0usize))
            .collect();
        let mut dependents: BTreeMap<&str, BTreeSet<&str>> = BTreeMap::new();

        for work in transition.work_packages.values() {
            for dependency in &work.depends_on {
                if !transition.work_packages.contains_key(dependency) {
                    return Err(EnterpriseArchitectureError::UnknownWorkPackage {
                        transition: transition_id.to_owned(),
                        work_package: dependency.clone(),
                    });
                }
                if let Some(degree) = indegree.get_mut(work.id.as_str()) {
                    *degree += 1;
                }
                dependents
                    .entry(dependency.as_str())
                    .or_default()
                    .insert(work.id.as_str());
            }
        }

        let mut ready: BTreeSet<&str> = indegree
            .iter()
            .filter_map(|(id, degree)| (*degree == 0).then_some(*id))
            .collect();
        let mut ordered = Vec::with_capacity(transition.work_packages.len());

        while let Some(id) = ready.pop_first() {
            ordered.push(id.to_owned());
            if let Some(children) = dependents.get(id) {
                for child in children {
                    if let Some(degree) = indegree.get_mut(child) {
                        *degree -= 1;
                        if *degree == 0 {
                            ready.insert(child);
                        }
                    }
                }
            }
        }

        if ordered.len() != transition.work_packages.len() {
            return Err(EnterpriseArchitectureError::TransitionCycle(
                transition_id.to_owned(),
            ));
        }
        Ok(ordered)
    }

    /// Canonical BLAKE3 digest of the complete admitted model.
    pub fn digest(&self) -> Result<String, EnterpriseArchitectureError> {
        serde_json::to_vec(self)
            .map(|bytes| blake3::hash(&bytes).to_hex().to_string())
            .map_err(|error| EnterpriseArchitectureError::Serialization(error.to_string()))
    }

    /// Manufacture a deterministic receipt for this static architecture state.
    ///
    /// The receipt proves model identity/replay only; it is not proof that any
    /// represented workload or transition executed.
    pub fn receipt(
        &self, operation: impl Into<String>, predecessor_digest: Option<String>,
    ) -> Result<EnterpriseArchitectureReceipt, EnterpriseArchitectureError> {
        let operation = operation.into();
        let subject_digest = self.digest()?;
        let consequence_material = serde_json::to_vec(&(
            subject_digest.as_str(),
            operation.as_str(),
            &predecessor_digest,
        ))
        .map_err(|error| EnterpriseArchitectureError::Serialization(error.to_string()))?;
        let consequence_digest = blake3::hash(&consequence_material).to_hex().to_string();
        Ok(EnterpriseArchitectureReceipt {
            schema: ENTERPRISE_ARCHITECTURE_RECEIPT_SCHEMA.to_owned(),
            subject_digest,
            predecessor_digest,
            operation,
            consequence_digest,
        })
    }

    /// Verify a deterministic architecture receipt against this exact model.
    #[must_use]
    pub fn verify_receipt(&self, receipt: &EnterpriseArchitectureReceipt) -> bool {
        if receipt.schema != ENTERPRISE_ARCHITECTURE_RECEIPT_SCHEMA {
            return false;
        }
        self.receipt(
            receipt.operation.clone(),
            receipt.predecessor_digest.clone(),
        )
        .is_ok_and(|expected| expected == *receipt)
    }

    fn has_outgoing(&self, subject: &str, kind: EnterpriseRelationKind) -> bool {
        self.relations
            .values()
            .any(|relation| relation.from == subject && relation.kind == kind)
    }

    fn has_incoming(&self, subject: &str, kind: EnterpriseRelationKind) -> bool {
        self.relations
            .values()
            .any(|relation| relation.to == subject && relation.kind == kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn element(
        id: &str, kind: EnterpriseElementKind, layer: ArchitectureLayer,
    ) -> EnterpriseElement {
        let mut element = EnterpriseElement::new(id, id, kind, layer);
        element.lifecycle = LifecycleState::Admitted;
        element
    }

    fn accountable(
        id: &str, kind: EnterpriseElementKind, layer: ArchitectureLayer,
    ) -> EnterpriseElement {
        let mut element = element(id, kind, layer);
        element.owner = Some("org:architecture".to_owned());
        element
    }

    fn fixture() -> EnterpriseArchitectureModel {
        let mut model = EnterpriseArchitectureModel::new("enterprise:acme", "boundary:acme");
        model
            .admit_element(element(
                "enterprise:acme",
                EnterpriseElementKind::Enterprise,
                ArchitectureLayer::Enterprise,
            ))
            .unwrap();
        model
            .admit_element(element(
                "boundary:acme",
                EnterpriseElementKind::Boundary,
                ArchitectureLayer::Enterprise,
            ))
            .unwrap();
        model
            .admit_element(element(
                "org:architecture",
                EnterpriseElementKind::OrganizationUnit,
                ArchitectureLayer::Business,
            ))
            .unwrap();
        model
    }

    fn add_relation(
        model: &mut EnterpriseArchitectureModel, id: &str, from: &str,
        kind: EnterpriseRelationKind, to: &str,
    ) {
        model
            .admit_relation(EnterpriseRelation {
                id: id.to_owned(),
                from: from.to_owned(),
                to: to.to_owned(),
                kind,
                rationale: None,
                evidence: BTreeSet::new(),
            })
            .unwrap();
    }

    #[test]
    fn requirement_traceability_crosses_enterprise_layers() {
        let mut model = fixture();
        for value in [
            accountable(
                "req:residency",
                EnterpriseElementKind::Requirement,
                ArchitectureLayer::Motivation,
            ),
            accountable(
                "cap:data",
                EnterpriseElementKind::Capability,
                ArchitectureLayer::Strategy,
            ),
            accountable(
                "svc:data",
                EnterpriseElementKind::Service,
                ArchitectureLayer::Business,
            ),
            accountable(
                "cmp:data",
                EnterpriseElementKind::Component,
                ArchitectureLayer::Application,
            ),
            accountable(
                "env:prod",
                EnterpriseElementKind::Environment,
                ArchitectureLayer::Technology,
            ),
            EnterpriseElement::new(
                "metric:residency",
                "Residency evidence metric",
                EnterpriseElementKind::Metric,
                ArchitectureLayer::Evidence,
            ),
            EnterpriseElement::new(
                "receipt:1",
                "Exact deployment receipt",
                EnterpriseElementKind::Receipt,
                ArchitectureLayer::Evidence,
            ),
        ] {
            model.admit_element(value).unwrap();
        }
        add_relation(
            &mut model,
            "r1",
            "req:residency",
            EnterpriseRelationKind::Constrains,
            "cap:data",
        );
        add_relation(
            &mut model,
            "r2",
            "cap:data",
            EnterpriseRelationKind::RealizedBy,
            "svc:data",
        );
        add_relation(
            &mut model,
            "r3",
            "svc:data",
            EnterpriseRelationKind::ImplementedBy,
            "cmp:data",
        );
        add_relation(
            &mut model,
            "r4",
            "cmp:data",
            EnterpriseRelationKind::DeployedTo,
            "env:prod",
        );
        add_relation(
            &mut model,
            "r5",
            "env:prod",
            EnterpriseRelationKind::ObservedBy,
            "metric:residency",
        );
        add_relation(
            &mut model,
            "r6",
            "metric:residency",
            EnterpriseRelationKind::EvidencedBy,
            "receipt:1",
        );

        let hops = model.traceability("req:residency", 8).unwrap();
        assert_eq!(hops.len(), 6);
        assert_eq!(hops.last().unwrap().to, "receipt:1");
    }

    #[test]
    fn structural_validation_refuses_missing_accountability_and_dangling_state() {
        let mut model = fixture();
        model
            .admit_element(element(
                "cap:payments",
                EnterpriseElementKind::Capability,
                ArchitectureLayer::Strategy,
            ))
            .unwrap();
        model.relations.insert(
            "r:dangling".to_owned(),
            EnterpriseRelation {
                id: "r:dangling".to_owned(),
                from: "cap:payments".to_owned(),
                to: "missing".to_owned(),
                kind: EnterpriseRelationKind::RealizedBy,
                rationale: None,
                evidence: BTreeSet::new(),
            },
        );

        let violations = model.validate();
        assert!(violations
            .iter()
            .any(|violation| violation.code == "EA-STRUCT-005"));
        assert!(violations
            .iter()
            .any(|violation| violation.code == "EA-STRUCT-009"));
    }

    #[test]
    fn governance_assessment_distinguishes_static_closure_from_alive() {
        let mut model = fixture();
        model
            .admit_element(accountable(
                "cap:payments",
                EnterpriseElementKind::Capability,
                ArchitectureLayer::Strategy,
            ))
            .unwrap();
        model
            .admit_element(accountable(
                "svc:payments",
                EnterpriseElementKind::Service,
                ArchitectureLayer::Business,
            ))
            .unwrap();
        add_relation(
            &mut model,
            "r:realized",
            "cap:payments",
            EnterpriseRelationKind::RealizedBy,
            "svc:payments",
        );

        let assessment = model.assess_governance();
        assert!(assessment.structurally_valid);
        assert!(assessment.statically_complete);
        assert!(assessment
            .findings
            .iter()
            .any(|finding| finding.code == "EA-GOV-005"));
        assert_eq!(
            model.elements["svc:payments"].standing,
            Standing::Unknown,
            "static governance must not manufacture runtime ALIVE"
        );
    }

    #[test]
    fn impact_closure_is_transitive_and_bidirectional() {
        let mut model = fixture();
        for id in ["app:a", "app:b", "app:c"] {
            model
                .admit_element(accountable(
                    id,
                    EnterpriseElementKind::Application,
                    ArchitectureLayer::Application,
                ))
                .unwrap();
        }
        add_relation(
            &mut model,
            "r:ab",
            "app:a",
            EnterpriseRelationKind::DependsOn,
            "app:b",
        );
        add_relation(
            &mut model,
            "r:bc",
            "app:b",
            EnterpriseRelationKind::DependsOn,
            "app:c",
        );

        let report = model.impact("app:c").unwrap();
        assert_eq!(
            report.impacted,
            BTreeSet::from(["app:a".to_owned(), "app:b".to_owned()])
        );
    }

    #[test]
    fn transition_order_is_deterministic_and_cycles_are_refused() {
        let mut model = fixture();
        for id in ["app:old", "app:new"] {
            model
                .admit_element(accountable(
                    id,
                    EnterpriseElementKind::Application,
                    ArchitectureLayer::Application,
                ))
                .unwrap();
        }

        let mut transition = EnterpriseTransition {
            id: "transition:modernize".to_owned(),
            name: "Modernize".to_owned(),
            baseline: BTreeSet::from(["app:old".to_owned()]),
            target: BTreeSet::from(["app:new".to_owned()]),
            work_packages: BTreeMap::new(),
        };
        transition.work_packages.insert(
            "wp:build".to_owned(),
            EnterpriseWorkPackage {
                id: "wp:build".to_owned(),
                name: "Build".to_owned(),
                owner: "org:architecture".to_owned(),
                depends_on: BTreeSet::new(),
                delivers: BTreeSet::from(["app:new".to_owned()]),
                retires: BTreeSet::new(),
                evidence_obligations: BTreeSet::new(),
            },
        );
        transition.work_packages.insert(
            "wp:cutover".to_owned(),
            EnterpriseWorkPackage {
                id: "wp:cutover".to_owned(),
                name: "Cut over".to_owned(),
                owner: "org:architecture".to_owned(),
                depends_on: BTreeSet::from(["wp:build".to_owned()]),
                delivers: BTreeSet::new(),
                retires: BTreeSet::from(["app:old".to_owned()]),
                evidence_obligations: BTreeSet::new(),
            },
        );
        model.admit_transition(transition).unwrap();

        assert_eq!(
            model.transition_order("transition:modernize").unwrap(),
            vec!["wp:build".to_owned(), "wp:cutover".to_owned()]
        );

        model
            .transitions
            .get_mut("transition:modernize")
            .unwrap()
            .work_packages
            .get_mut("wp:build")
            .unwrap()
            .depends_on
            .insert("wp:cutover".to_owned());
        assert!(matches!(
            model.transition_order("transition:modernize"),
            Err(EnterpriseArchitectureError::TransitionCycle(_))
        ));
    }

    #[test]
    fn projection_contains_only_viewpoint_owned_edges() {
        let mut model = fixture();
        model
            .admit_element(accountable(
                "cap:payments",
                EnterpriseElementKind::Capability,
                ArchitectureLayer::Strategy,
            ))
            .unwrap();
        model
            .admit_element(accountable(
                "app:payments",
                EnterpriseElementKind::Application,
                ArchitectureLayer::Application,
            ))
            .unwrap();
        add_relation(
            &mut model,
            "r:cross",
            "cap:payments",
            EnterpriseRelationKind::RealizedBy,
            "app:payments",
        );

        let application = model.project(ArchitectureViewpoint::Application);
        assert_eq!(
            application.elements.keys().cloned().collect::<Vec<_>>(),
            vec!["app:payments".to_owned()]
        );
        assert!(application.relations.is_empty());

        let full = model.project(ArchitectureViewpoint::Full);
        assert!(full.relations.contains_key("r:cross"));
    }

    #[test]
    fn canonical_digest_is_insertion_order_independent_and_receipted() {
        let mut left = fixture();
        let mut right = fixture();

        let one = accountable(
            "app:a",
            EnterpriseElementKind::Application,
            ArchitectureLayer::Application,
        );
        let two = accountable(
            "app:b",
            EnterpriseElementKind::Application,
            ArchitectureLayer::Application,
        );
        left.admit_element(one.clone()).unwrap();
        left.admit_element(two.clone()).unwrap();
        right.admit_element(two).unwrap();
        right.admit_element(one).unwrap();

        assert_eq!(left.digest().unwrap(), right.digest().unwrap());
        let receipt = left.receipt("admit-enterprise-model", None).unwrap();
        assert!(left.verify_receipt(&receipt));

        let mut tampered = receipt.clone();
        tampered.operation = "different-operation".to_owned();
        assert!(!left.verify_receipt(&tampered));
    }

    #[test]
    fn canonical_public_vocabulary_alignment_is_explicit_and_non_authoritative() {
        let organization = EnterpriseElement::new(
            "org:architecture",
            "Architecture",
            EnterpriseElementKind::OrganizationUnit,
            ArchitectureLayer::Business,
        );
        assert!(organization
            .semantic_types
            .contains(public_vocabulary::ORG_ORGANIZATION));

        let dataset = EnterpriseElement::new(
            "data:customer",
            "Customer",
            EnterpriseElementKind::Dataset,
            ArchitectureLayer::Information,
        );
        assert!(dataset
            .semantic_types
            .contains(public_vocabulary::DCAT_DATASET));
    }
}
