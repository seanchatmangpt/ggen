//! Contract system (Σ): Versioned ontologies and decision contracts
//!
//! Contracts (Σ) define the shape of valid observations, decisions, and system states.
//! They are immutable, versioned, and subject to Q (invariant) constraints.

use crate::error::DoDResult;
use crate::observation::ObservationSchema;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt;
use uuid::Uuid;

/// Unique identifier for contracts
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ContractId(Uuid);

impl ContractId {
    /// Generate a new contract ID
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl Default for ContractId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ContractId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Semantic version for contracts (major.minor.patch)
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ContractVersion {
    major: u32,
    minor: u32,
    patch: u32,
}

impl ContractVersion {
    /// Create a new contract version
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self { major, minor, patch }
    }

    /// Increment major version (breaking change)
    pub fn bump_major(&self) -> Self {
        Self {
            major: self.major + 1,
            minor: 0,
            patch: 0,
        }
    }

    /// Increment minor version (backward-compatible feature)
    pub fn bump_minor(&self) -> Self {
        Self {
            major: self.major,
            minor: self.minor + 1,
            patch: 0,
        }
    }

    /// Increment patch version (bug fix)
    pub fn bump_patch(&self) -> Self {
        Self {
            major: self.major,
            minor: self.minor,
            patch: self.patch + 1,
        }
    }

    /// Check if this version is compatible with another
    /// (can upgrade from other to self without breaking)
    pub fn compatible_with(&self, other: &Self) -> bool {
        // Compatible if major version matches and this version is >= other
        self.major == other.major && self >= other
    }
}

impl fmt::Display for ContractVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

impl Default for ContractVersion {
    fn default() -> Self {
        Self::new(1, 0, 0)
    }
}

/// A contract (Σ) that defines system semantics
///
/// Contracts are:
/// - Immutable once published
/// - Versioned with semantic versioning
/// - Subject to invariant constraints (Q)
/// - Queryable via pattern matching
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Contract {
    /// Unique contract identifier
    id: ContractId,
    /// Semantic version
    version: ContractVersion,
    /// Human-readable name
    name: String,
    /// Detailed description
    description: String,
    /// Observation schemas that conform to this contract
    observation_schemas: BTreeMap<String, ObservationSchema>,
    /// Decision patterns this contract permits
    decision_patterns: BTreeMap<String, DecisionPattern>,
    /// Invariants that must hold (Q)
    invariants: Vec<InvariantConstraint>,
    /// When this contract was published
    published_at: DateTime<Utc>,
    /// Signature (for immutability guarantee)
    signature: Option<String>,
    /// Stability level (experimental, beta, stable)
    stability: StabilityLevel,
}

/// Stability level of a contract
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum StabilityLevel {
    /// Subject to breaking changes
    Experimental,
    /// Mostly stable, may have minor breaking changes
    Beta,
    /// Stable, backward-compatible changes only
    Stable,
}

impl fmt::Display for StabilityLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StabilityLevel::Experimental => write!(f, "experimental"),
            StabilityLevel::Beta => write!(f, "beta"),
            StabilityLevel::Stable => write!(f, "stable"),
        }
    }
}

/// A decision pattern that this contract permits
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecisionPattern {
    /// Name of the decision pattern (e.g., "promote", "rollback", "migrate")
    name: String,
    /// Preconditions that must be true
    preconditions: Vec<String>,
    /// Postconditions that will be true
    postconditions: Vec<String>,
    /// Idempotent flag (true if μ ∘ μ = μ for this pattern)
    idempotent: bool,
}

impl DecisionPattern {
    /// Create a new decision pattern
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            preconditions: Vec::new(),
            postconditions: Vec::new(),
            idempotent: false,
        }
    }

    /// Add a precondition
    pub fn with_precondition(mut self, condition: impl Into<String>) -> Self {
        self.preconditions.push(condition.into());
        self
    }

    /// Add a postcondition
    pub fn with_postcondition(mut self, condition: impl Into<String>) -> Self {
        self.postconditions.push(condition.into());
        self
    }

    /// Mark this pattern as idempotent
    pub fn idempotent(mut self) -> Self {
        self.idempotent = true;
        self
    }

    /// Get the pattern name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Is this pattern idempotent?
    pub fn is_idempotent(&self) -> bool {
        self.idempotent
    }
}

/// An invariant constraint (Q) that contracts must respect
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvariantConstraint {
    /// Human-readable name of the constraint
    name: String,
    /// Formal constraint description
    constraint: String,
    /// Whether this constraint is blocking (prevents promotion if violated)
    blocking: bool,
    /// Severity level
    severity: ConstraintSeverity,
}

/// Severity of an invariant constraint
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConstraintSeverity {
    Info,
    Warning,
    Error,
    Critical,
}

impl fmt::Display for ConstraintSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstraintSeverity::Info => write!(f, "info"),
            ConstraintSeverity::Warning => write!(f, "warning"),
            ConstraintSeverity::Error => write!(f, "error"),
            ConstraintSeverity::Critical => write!(f, "critical"),
        }
    }
}

impl Contract {
    /// Create a new contract
    pub fn new(
        name: impl Into<String>,
        description: impl Into<String>,
        stability: StabilityLevel,
    ) -> Self {
        Self {
            id: ContractId::new(),
            version: ContractVersion::default(),
            name: name.into(),
            description: description.into(),
            observation_schemas: BTreeMap::new(),
            decision_patterns: BTreeMap::new(),
            invariants: Vec::new(),
            published_at: Utc::now(),
            signature: None,
            stability,
        }
    }

    /// Get contract ID
    pub fn id(&self) -> ContractId {
        self.id
    }

    /// Get version
    pub fn version(&self) -> &ContractVersion {
        &self.version
    }

    /// Get name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get description
    pub fn description(&self) -> &str {
        &self.description
    }

    /// Add observation schema
    pub fn with_observation_schema(
        mut self,
        name: impl Into<String>,
        schema: ObservationSchema,
    ) -> Self {
        self.observation_schemas.insert(name.into(), schema);
        self
    }

    /// Add decision pattern
    pub fn with_decision_pattern(mut self, pattern: DecisionPattern) -> Self {
        self.decision_patterns
            .insert(pattern.name().to_string(), pattern);
        self
    }

    /// Add invariant constraint
    pub fn with_invariant(
        mut self,
        name: impl Into<String>,
        constraint: impl Into<String>,
        severity: ConstraintSeverity,
        blocking: bool,
    ) -> Self {
        self.invariants.push(InvariantConstraint {
            name: name.into(),
            constraint: constraint.into(),
            blocking,
            severity,
        });
        self
    }

    /// Get all observation schemas
    pub fn observation_schemas(&self) -> &BTreeMap<String, ObservationSchema> {
        &self.observation_schemas
    }

    /// Get a specific observation schema
    pub fn observation_schema(&self, name: &str) -> Option<&ObservationSchema> {
        self.observation_schemas.get(name)
    }

    /// Get all decision patterns
    pub fn decision_patterns(&self) -> &BTreeMap<String, DecisionPattern> {
        &self.decision_patterns
    }

    /// Get a specific decision pattern
    pub fn decision_pattern(&self, name: &str) -> Option<&DecisionPattern> {
        self.decision_patterns.get(name)
    }

    /// Get all invariants
    pub fn invariants(&self) -> &[InvariantConstraint] {
        &self.invariants
    }

    /// Get blocking invariants only
    pub fn blocking_invariants(&self) -> Vec<&InvariantConstraint> {
        self.invariants.iter().filter(|i| i.blocking).collect()
    }

    /// Sign the contract (makes it immutable)
    pub fn sign(mut self, key: &[u8]) -> Self {
        use hmac::Mac;
        let mut mac = hmac::Hmac::<sha2::Sha256>::new_from_slice(key)
            .expect("HMAC key length is valid");

        let payload = format!("{}{}{}", self.id, self.version, self.name);
        mac.update(payload.as_bytes());
        let signature = hex::encode(mac.finalize().into_bytes());
        self.signature = Some(signature);
        self
    }

    /// Get stability level
    pub fn stability(&self) -> StabilityLevel {
        self.stability
    }

    /// Is this contract stable?
    pub fn is_stable(&self) -> bool {
        self.stability == StabilityLevel::Stable
    }
}

impl fmt::Display for Contract {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Contract(id={}, name={}, version={}, stability={})",
            self.id, self.name, self.version, self.stability
        )
    }
}

/// Collection of contracts (ontology)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Ontology {
    /// Unique ontology ID
    id: String,
    /// All contracts in this ontology
    contracts: BTreeMap<ContractId, Contract>,
    /// Version of the ontology
    version: ContractVersion,
    /// When this ontology was created
    created_at: DateTime<Utc>,
}

impl Ontology {
    /// Create a new empty ontology
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            contracts: BTreeMap::new(),
            version: ContractVersion::default(),
            created_at: Utc::now(),
        }
    }

    /// Add a contract to the ontology
    pub fn with_contract(mut self, contract: Contract) -> Self {
        self.contracts.insert(contract.id(), contract);
        self
    }

    /// Get all contracts
    pub fn contracts(&self) -> &BTreeMap<ContractId, Contract> {
        &self.contracts
    }

    /// Get a specific contract
    pub fn contract(&self, id: ContractId) -> Option<&Contract> {
        self.contracts.get(&id)
    }

    /// Find contracts by name
    pub fn find_by_name(&self, name: &str) -> Vec<&Contract> {
        self.contracts.values().filter(|c| c.name() == name).collect()
    }

    /// Get ontology ID
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Get ontology version
    pub fn version(&self) -> &ContractVersion {
        &self.version
    }

    /// Bump ontology version (ΔΣ)
    pub fn bump_version(&mut self) {
        self.version = self.version.bump_minor();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_contract_version_compatibility() {
        let v1 = ContractVersion::new(1, 0, 0);
        let v1_1 = ContractVersion::new(1, 1, 0);
        let v2 = ContractVersion::new(2, 0, 0);

        assert!(v1_1.compatible_with(&v1));
        assert!(!v2.compatible_with(&v1));
    }

    #[test]
    fn test_contract_creation() {
        let contract = Contract::new("test", "test contract", StabilityLevel::Beta);
        assert_eq!(contract.name(), "test");
        assert_eq!(contract.stability(), StabilityLevel::Beta);
    }

    #[test]
    fn test_decision_pattern_idempotence() {
        let pattern = DecisionPattern::new("test").idempotent();
        assert!(pattern.is_idempotent());

        let non_idempotent = DecisionPattern::new("test2");
        assert!(!non_idempotent.is_idempotent());
    }

    #[test]
    fn test_ontology_operations() {
        let contract1 = Contract::new("c1", "desc1", StabilityLevel::Stable);
        let contract2 = Contract::new("c2", "desc2", StabilityLevel::Beta);

        let ontology = Ontology::new("test-ontology")
            .with_contract(contract1.clone())
            .with_contract(contract2.clone());

        assert_eq!(ontology.contracts().len(), 2);
        assert!(ontology.find_by_name("c1").len() > 0);
    }
}
