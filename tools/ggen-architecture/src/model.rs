//! Core enterprise architecture types.

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

use serde::{Deserialize, Serialize};

/// Kinds of governed architecture assets.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AssetKind {
    /// A business or technical capability.
    Capability,
    /// A digital product.
    Product,
    /// An externally consumable service.
    Service,
    /// A source repository.
    Repository,
    /// A deployable or reusable component.
    Component,
    /// A semantic ontology or profile.
    Ontology,
    /// A ggen constituent or distribution pack.
    Pack,
    /// A generated artifact family or projection.
    Projection,
    /// A governing requirement.
    Requirement,
    /// A policy or architecture principle.
    Policy,
    /// A baseline, target, or intermediate architecture.
    ArchitectureState,
    /// A migration work package.
    WorkPackage,
    /// An executable transition architecture.
    TransitionArchitecture,
    /// A proof, benchmark, validation report, or other evidence.
    Evidence,
    /// A cryptographic or semantic receipt.
    Receipt,
}

/// Lifecycle of an architecture asset.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LifecycleState {
    /// Candidate known but not yet identified.
    Discovered,
    /// Identity, source, and version are known.
    Identified,
    /// Minimum syntax, provenance, and policy checks passed.
    Qualified,
    /// Admitted into the governed architecture boundary.
    Admitted,
    /// Available for normal enterprise use.
    Active,
    /// Still usable only under an explicit migration window.
    Deprecated,
    /// New use is refused.
    Retired,
    /// Preserved for replay and historical evidence only.
    Archived,
}

impl LifecycleState {
    /// Whether a direct transition is permitted by the lifecycle state machine.
    #[must_use]
    pub const fn allows(self, target: Self) -> bool {
        if self as u8 == target as u8 {
            return true;
        }

        matches!(
            (self, target),
            (Self::Discovered, Self::Identified | Self::Retired)
                | (Self::Identified, Self::Qualified | Self::Retired)
                | (Self::Qualified, Self::Admitted | Self::Retired)
                | (
                    Self::Admitted,
                    Self::Active | Self::Deprecated | Self::Retired
                )
                | (Self::Active, Self::Deprecated)
                | (Self::Deprecated, Self::Active | Self::Retired)
                | (Self::Retired, Self::Archived)
        )
    }
}

impl fmt::Display for LifecycleState {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            Self::Discovered => "discovered",
            Self::Identified => "identified",
            Self::Qualified => "qualified",
            Self::Admitted => "admitted",
            Self::Active => "active",
            Self::Deprecated => "deprecated",
            Self::Retired => "retired",
            Self::Archived => "archived",
        };
        formatter.write_str(value)
    }
}

/// Evidence standing used across the Chatman Ecosystem.
#[derive(
    Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize,
)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum Standing {
    /// No sufficient observation exists.
    #[default]
    Unknown,
    /// A bounded subset is alive; wider closure remains incomplete.
    PartialAlive,
    /// The declared bounded capability is demonstrated.
    Alive,
    /// Work is prevented by an external or constitutional dependency.
    Blocked,
    /// The admitted source does not currently build.
    BuildBroken,
    /// The requested capability is outside the supported boundary.
    Unsupported,
    /// The capability has been intentionally withdrawn.
    Retired,
}


/// Severity shared by diagnostics, doctor findings, and autonomic diagnoses.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Severity {
    /// Informational observation.
    Info,
    /// Corrective action is advisable.
    Warning,
    /// Promotion or operation should be refused until corrected.
    Error,
    /// Immediate containment is required.
    Critical,
}

/// One governed architecture asset.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArchitectureAsset {
    /// Stable ecosystem identifier.
    pub id: String,
    /// Human-readable name.
    pub name: String,
    /// Asset classification.
    pub kind: AssetKind,
    /// Optional semantic or product version.
    #[serde(default)]
    pub version: Option<String>,
    /// Accountable owner or steward.
    #[serde(default)]
    pub owner: Option<String>,
    /// Current lifecycle state.
    pub lifecycle: LifecycleState,
    /// Current evidentiary standing.
    #[serde(default)]
    pub standing: Standing,
    /// Assets required for this asset to realize its capability.
    #[serde(default)]
    pub dependencies: BTreeSet<String>,
    /// Assets superseded by this asset.
    #[serde(default)]
    pub replaces: BTreeSet<String>,
    /// Stable extension metadata.
    #[serde(default)]
    pub attributes: BTreeMap<String, String>,
}

impl ArchitectureAsset {
    /// Construct a minimally identified architecture asset.
    #[must_use]
    pub fn new(id: impl Into<String>, name: impl Into<String>, kind: AssetKind) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            kind,
            version: None,
            owner: None,
            lifecycle: LifecycleState::Discovered,
            standing: Standing::Unknown,
            dependencies: BTreeSet::new(),
            replaces: BTreeSet::new(),
            attributes: BTreeMap::new(),
        }
    }
}

/// One ordered lifecycle transition in a transition architecture.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TransitionStep {
    /// Asset being transitioned.
    pub asset_id: String,
    /// Baseline state.
    pub from: LifecycleState,
    /// Target state.
    pub to: LifecycleState,
    /// Dependencies that must already have standing.
    pub prerequisites: Vec<String>,
}
