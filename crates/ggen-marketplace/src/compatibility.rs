//! Multi-dimensional conflict detection for pack compatibility.
//!
//! Implements Fortune 5 CISO requirements for comprehensive compatibility
//! checking across 10 dimensions.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

use crate::atomic::{AtomicPackClass, AtomicPackId};
use crate::error::{Error, Result};
use crate::ownership::{MergeStrategy, OwnershipClass, OwnershipDeclaration, OwnershipTarget};

/// Compatibility dimension for conflict detection.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CompatibilityDimension {
    /// RDF namespace conflicts (ontology terms).
    OntologyNamespace,

    /// Protocol-visible field conflicts.
    ProtocolField,

    /// Emitted file path conflicts.
    EmittedFilePath,

    /// Runtime incompatibility (different runtimes for same capability).
    RuntimeIncompatibility,

    /// Validator contradictions (conflicting validation rules).
    ValidatorContradiction,

    /// Policy contradictions (mutually exclusive policies).
    PolicyContradiction,

    /// Version range conflicts (semver incompatibilities).
    VersionRange,

    /// Capability identity conflicts (duplicate capabilities).
    CapabilityIdentity,

    /// Receipt schema incompatibilities.
    ReceiptSchema,

    /// Consequence migration conflicts (breaking changes).
    ConsequenceMigration,
}

impl fmt::Display for CompatibilityDimension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::OntologyNamespace => "ontology_namespace",
            Self::ProtocolField => "protocol_field",
            Self::EmittedFilePath => "emitted_file_path",
            Self::RuntimeIncompatibility => "runtime_incompatibility",
            Self::ValidatorContradiction => "validator_contradiction",
            Self::PolicyContradiction => "policy_contradiction",
            Self::VersionRange => "version_range",
            Self::CapabilityIdentity => "capability_identity",
            Self::ReceiptSchema => "receipt_schema",
            Self::ConsequenceMigration => "consequence_migration",
        };
        write!(f, "{}", name)
    }
}

/// Conflict severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ConflictSeverity {
    /// Hard failure, cannot proceed.
    Error,

    /// Requires user confirmation.
    Warning,

    /// Informational only.
    Info,
}

impl fmt::Display for ConflictSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Info => "info",
        };
        write!(f, "{}", name)
    }
}

/// Receipt schema metadata loaded from pack definition files.
#[derive(Debug, Clone)]
struct ReceiptSchemaMeta {
    /// Receipt format version (e.g., "1", "2")
    schema_version: String,
    /// Signature algorithm (e.g., "ed25519", "rsa")
    signature_algorithm: Option<String>,
    /// Hash function (e.g., "sha256", "sha3")
    hash_function: Option<String>,
    /// Maximum chain depth (for chained receipts)
    chain_depth_max: Option<u32>,
}

impl Default for ReceiptSchemaMeta {
    fn default() -> Self {
        Self {
            schema_version: "1".to_string(),
            signature_algorithm: None,
            hash_function: Some("sha256".to_string()),
            chain_depth_max: None,
        }
    }
}

/// Conflict resolution strategy.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ConflictResolution {
    /// Stop with error (cannot proceed).
    Fail,

    /// Use first pack's version.
    PreferFirst,

    /// Use last pack's version.
    PreferLast,

    /// Merge with specified strategy.
    Merge { strategy: MergeStrategy },

    /// Ask user to choose.
    UserResolve,

    /// Apply custom resolution logic.
    Custom { description: String },
}

/// Conflict between two or more packs.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Conflict {
    /// Compatibility dimension where conflict occurs.
    pub dimension: CompatibilityDimension,

    /// Packs involved in conflict (at least 2).
    pub packs: Vec<AtomicPackId>,

    /// Human-readable conflict description.
    pub description: String,

    /// Conflict severity.
    pub severity: ConflictSeverity,

    /// Suggested resolution (if any).
    pub resolution: Option<ConflictResolution>,

    /// Additional context for debugging.
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub context: HashMap<String, String>,
}

impl Conflict {
    /// Create a new conflict.
    #[must_use]
    pub fn new(
        dimension: CompatibilityDimension, packs: Vec<AtomicPackId>, description: String,
        severity: ConflictSeverity,
    ) -> Self {
        Self {
            dimension,
            packs,
            description,
            severity,
            resolution: None,
            context: HashMap::new(),
        }
    }

    /// Create an error-level conflict.
    #[must_use]
    pub fn error(
        dimension: CompatibilityDimension, packs: Vec<AtomicPackId>, description: String,
    ) -> Self {
        Self::new(dimension, packs, description, ConflictSeverity::Error)
    }

    /// Create a warning-level conflict.
    #[must_use]
    pub fn warning(
        dimension: CompatibilityDimension, packs: Vec<AtomicPackId>, description: String,
    ) -> Self {
        Self::new(dimension, packs, description, ConflictSeverity::Warning)
    }

    /// Create an info-level conflict.
    #[must_use]
    pub fn info(
        dimension: CompatibilityDimension, packs: Vec<AtomicPackId>, description: String,
    ) -> Self {
        Self::new(dimension, packs, description, ConflictSeverity::Info)
    }

    /// Add resolution strategy to conflict.
    #[must_use]
    pub fn with_resolution(mut self, resolution: ConflictResolution) -> Self {
        self.resolution = Some(resolution);
        self
    }

    /// Add context to conflict.
    #[must_use]
    pub fn with_context(mut self, key: String, value: String) -> Self {
        self.context.insert(key, value);
        self
    }

    /// Check if this is a hard error.
    #[must_use]
    pub const fn is_error(&self) -> bool {
        matches!(self.severity, ConflictSeverity::Error)
    }

    /// Check if this is a warning.
    #[must_use]
    pub const fn is_warning(&self) -> bool {
        matches!(self.severity, ConflictSeverity::Warning)
    }

    /// Check if this is informational.
    #[must_use]
    pub const fn is_info(&self) -> bool {
        matches!(self.severity, ConflictSeverity::Info)
    }
}

/// Compatibility check report.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompatibilityReport {
    /// Whether all packs are compatible (no error-level conflicts).
    pub compatible: bool,

    /// All conflicts found (errors, warnings, info).
    pub conflicts: Vec<Conflict>,

    /// Packs that were checked.
    pub packs_checked: Vec<AtomicPackId>,
}

impl CompatibilityReport {
    /// Create a new compatibility report.
    #[must_use]
    pub fn new(
        compatible: bool, conflicts: Vec<Conflict>, packs_checked: Vec<AtomicPackId>,
    ) -> Self {
        Self {
            compatible,
            conflicts,
            packs_checked,
        }
    }

    /// Create a successful report (no conflicts).
    #[must_use]
    pub fn success(packs_checked: Vec<AtomicPackId>) -> Self {
        Self {
            compatible: true,
            conflicts: Vec::new(),
            packs_checked,
        }
    }

    /// Create a failure report with conflicts.
    #[must_use]
    pub fn failure(conflicts: Vec<Conflict>, packs_checked: Vec<AtomicPackId>) -> Self {
        let compatible = !conflicts.iter().any(|c| c.is_error());
        Self {
            compatible,
            conflicts,
            packs_checked,
        }
    }

    /// Get all error-level conflicts.
    #[must_use]
    pub fn errors(&self) -> Vec<&Conflict> {
        self.conflicts.iter().filter(|c| c.is_error()).collect()
    }

    /// Get all warning-level conflicts.
    #[must_use]
    pub fn warnings(&self) -> Vec<&Conflict> {
        self.conflicts.iter().filter(|c| c.is_warning()).collect()
    }

    /// Get all info-level conflicts.
    #[must_use]
    pub fn info(&self) -> Vec<&Conflict> {
        self.conflicts.iter().filter(|c| c.is_info()).collect()
    }

    /// Get conflicts by dimension.
    #[must_use]
    pub fn conflicts_by_dimension(&self, dimension: CompatibilityDimension) -> Vec<&Conflict> {
        self.conflicts
            .iter()
            .filter(|c| c.dimension == dimension)
            .collect()
    }

    /// Get conflicts for a specific pack.
    #[must_use]
    pub fn conflicts_for_pack(&self, pack_id: &AtomicPackId) -> Vec<&Conflict> {
        self.conflicts
            .iter()
            .filter(|c| c.packs.iter().any(|p| p == pack_id))
            .collect()
    }

    /// Count conflicts by severity.
    #[must_use]
    pub fn count_by_severity(&self) -> HashMap<ConflictSeverity, usize> {
        let mut counts = HashMap::new();
        for conflict in &self.conflicts {
            *counts.entry(conflict.severity).or_insert(0) += 1;
        }
        counts
    }
}

/// Compatibility checker for multi-dimensional conflict detection.
pub struct CompatibilityChecker {
    /// Cache directory for loading pack metadata
    cache_dir: std::path::PathBuf,
}

impl CompatibilityChecker {
    /// Create a new compatibility checker with default cache directory.
    ///
    /// # Errors
    ///
    /// Returns error if default cache directory cannot be determined.
    pub fn new() -> Result<Self> {
        let cache_dir = dirs::cache_dir()
            .unwrap_or_else(|| std::path::PathBuf::from(".cache"))
            .join("ggen")
            .join("packs");

        Ok(Self { cache_dir })
    }

    /// Create a new compatibility checker with explicit cache directory.
    #[must_use]
    pub fn with_cache_dir(cache_dir: std::path::PathBuf) -> Self {
        Self { cache_dir }
    }

    /// Check compatibility across all dimensions.
    ///
    /// # Errors
    ///
    /// Returns error if compatibility checking fails (not if conflicts are found).
    pub fn check(
        &self, packs: &[AtomicPackId], ownership_declarations: &[OwnershipDeclaration],
    ) -> Result<CompatibilityReport> {
        let mut conflicts = Vec::new();

        // Check each dimension
        conflicts.extend(Self::check_ontology_namespaces(packs)?);
        conflicts.extend(Self::check_protocol_fields(ownership_declarations)?);
        conflicts.extend(Self::check_emitted_files(ownership_declarations)?);
        conflicts.extend(Self::check_runtime_compat(packs)?);
        conflicts.extend(self.check_validators(packs)?);
        conflicts.extend(self.check_policies(packs)?);
        conflicts.extend(Self::check_versions(packs)?);
        conflicts.extend(Self::check_capabilities(packs)?);
        conflicts.extend(self.check_receipt_schemas(packs)?);
        conflicts.extend(Self::check_consequences(packs)?);

        Ok(CompatibilityReport::new(
            !conflicts.iter().any(|c| c.is_error()),
            conflicts,
            packs.to_vec(),
        ))
    }

    /// Check ontology namespace conflicts.
    fn check_ontology_namespaces(packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
        let mut conflicts = Vec::new();

        // Group packs by namespace prefix
        let mut namespaces: HashMap<String, Vec<AtomicPackId>> = HashMap::new();

        for pack in packs {
            // Extract namespace from pack ID (simplified)
            let namespace = pack
                .to_string()
                .split('-')
                .next()
                .unwrap_or("default")
                .to_string();
            namespaces.entry(namespace).or_default().push(pack.clone());
        }

        // Check for multiple packs claiming same namespace
        for (namespace, pack_list) in namespaces {
            if pack_list.len() > 1 {
                conflicts.push(
                    Conflict::error(
                        CompatibilityDimension::OntologyNamespace,
                        pack_list.clone(),
                        format!("Multiple packs claim ontology namespace: {}", namespace),
                    )
                    .with_resolution(ConflictResolution::Fail)
                    .with_context("namespace".to_string(), namespace.clone()),
                );
            }
        }

        Ok(conflicts)
    }

    /// Check protocol field conflicts.
    fn check_protocol_fields(declarations: &[OwnershipDeclaration]) -> Result<Vec<Conflict>> {
        let mut conflicts = Vec::new();
        let mut field_map: HashMap<String, Vec<OwnershipDeclaration>> = HashMap::new();

        // Group declarations by protocol field
        for decl in declarations {
            if let OwnershipTarget::ProtocolField(ref field) = decl.target {
                field_map
                    .entry(field.clone())
                    .or_default()
                    .push(decl.clone());
            }
        }

        // Check for conflicts
        for (field, decls) in field_map {
            if decls.len() > 1 {
                let packs: Vec<_> = decls.iter().map(|d| d.owner_pack.clone()).collect();
                let severity = if decls.iter().all(|d| d.class == OwnershipClass::Exclusive) {
                    ConflictSeverity::Error
                } else {
                    ConflictSeverity::Warning
                };

                // Derive pack IDs from the actual owner_pack strings in the declarations
                let pack_ids: Vec<AtomicPackId> = decls
                    .iter()
                    .filter_map(|d| AtomicPackId::from_str(&d.owner_pack))
                    .collect();
                let conflict_packs = if pack_ids.len() >= 2 {
                    pack_ids
                } else {
                    // Fallback: construct IDs from the owner names when parsing fails
                    packs
                        .iter()
                        .filter_map(|p| AtomicPackId::from_str(p))
                        .collect()
                };

                conflicts.push(
                    Conflict::new(
                        CompatibilityDimension::ProtocolField,
                        conflict_packs,
                        format!(
                            "Protocol field '{}' claimed by multiple packs: {:?}",
                            field, packs
                        ),
                        severity,
                    )
                    .with_resolution(ConflictResolution::UserResolve)
                    .with_context("field".to_string(), field.clone())
                    .with_context("packs".to_string(), packs.join(", ")),
                );
            }
        }

        Ok(conflicts)
    }

    /// Check emitted file path conflicts.
    fn check_emitted_files(declarations: &[OwnershipDeclaration]) -> Result<Vec<Conflict>> {
        let mut conflicts = Vec::new();
        let mut file_map: HashMap<String, Vec<OwnershipDeclaration>> = HashMap::new();

        // Group declarations by file path
        for decl in declarations {
            if let OwnershipTarget::FilePath(ref path) = decl.target {
                let key = path.to_string_lossy().to_string();
                file_map.entry(key).or_default().push(decl.clone());
            }
        }

        // Check for conflicts
        for (file, decls) in file_map {
            if decls.len() > 1 {
                let packs: Vec<_> = decls.iter().map(|d| d.owner_pack.clone()).collect();

                // Check ownership classes
                let all_exclusive = decls.iter().all(|d| d.class == OwnershipClass::Exclusive);
                let all_mergeable = decls.iter().all(|d| d.class == OwnershipClass::Mergeable);

                let severity = if all_exclusive {
                    ConflictSeverity::Error
                } else if all_mergeable {
                    ConflictSeverity::Info
                } else {
                    ConflictSeverity::Warning
                };

                let resolution = if all_mergeable {
                    Some(ConflictResolution::Merge {
                        strategy: MergeStrategy::Concat,
                    })
                } else {
                    Some(ConflictResolution::UserResolve)
                };

                // Derive pack IDs from the actual owner_pack strings in the declarations
                let pack_ids: Vec<AtomicPackId> = decls
                    .iter()
                    .filter_map(|d| AtomicPackId::from_str(&d.owner_pack))
                    .collect();
                let conflict_packs = if pack_ids.len() >= 2 {
                    pack_ids
                } else {
                    // Fallback: construct IDs from the owner names when parsing fails
                    packs
                        .iter()
                        .filter_map(|p| AtomicPackId::from_str(p))
                        .collect()
                };

                conflicts.push(
                    Conflict::new(
                        CompatibilityDimension::EmittedFilePath,
                        conflict_packs,
                        format!("File '{}' emitted by multiple packs: {:?}", file, packs),
                        severity,
                    )
                    .with_resolution(resolution.unwrap_or(ConflictResolution::Fail))
                    .with_context("file".to_string(), file.clone())
                    .with_context("packs".to_string(), packs.join(", ")),
                );
            }
        }

        Ok(conflicts)
    }

    /// Check runtime compatibility.
    fn check_runtime_compat(packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
        let mut conflicts = Vec::new();
        let mut runtime_map: HashMap<String, Vec<AtomicPackId>> = HashMap::new();

        // Group packs by runtime (simplified - extract from pack ID)
        for pack in packs {
            let pack_str = pack.to_string();
            let runtime = pack_str.split('-').last().unwrap_or("unknown");
            runtime_map
                .entry(runtime.to_string())
                .or_default()
                .push(pack.clone());
        }

        // Check for multiple runtimes in same capability set
        if runtime_map.len() > 1 {
            let all_packs: Vec<_> = runtime_map.values().flatten().cloned().collect();
            conflicts.push(
                Conflict::warning(
                    CompatibilityDimension::RuntimeIncompatibility,
                    all_packs,
                    format!(
                        "Multiple runtimes detected: {:?}",
                        runtime_map.keys().collect::<Vec<_>>()
                    ),
                )
                .with_resolution(ConflictResolution::UserResolve),
            );
        }

        Ok(conflicts)
    }

    /// Check validator contradictions.
    ///
    /// Validates that validator packs don't have contradictory rules on the same
    /// protocol-visible fields. Common contradictions:
    /// - One validator requires field X, another forbids X
    /// - One validator requires pattern P, another requires incompatible pattern Q
    /// - Multiple validators claim exclusive ownership of same protocol field
    fn check_validators(&self, packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
        use crate::atomic::AtomicPackCategory;
        use crate::ownership::{OwnershipClass, OwnershipTarget};

        let mut conflicts = Vec::new();

        // Filter to only validator packs
        let validator_packs: Vec<_> = packs
            .iter()
            .filter(|p| p.class.category() == AtomicPackCategory::Validator)
            .collect();

        if validator_packs.is_empty() {
            return Ok(conflicts);
        }

        // Track protocol field ownership by validators
        // Key: protocol field name, Value: (pack_id, ownership_class)
        let mut field_claims: std::collections::HashMap<String, Vec<(String, OwnershipClass)>> =
            std::collections::HashMap::new();

        // For each validator pack, load ownership declarations from pack metadata
        for validator in &validator_packs {
            let declarations =
                Self::load_validator_ownership_declarations(validator, &self.cache_dir)?;

            for decl in declarations {
                if let OwnershipTarget::ProtocolField(field_name) = &decl.target {
                    field_claims
                        .entry(field_name.clone())
                        .or_default()
                        .push((validator.to_string(), decl.class));
                }
            }
        }

        // Detect contradictions: multiple exclusive claims on same field
        for (field_name, claimants) in &field_claims {
            let exclusive_claimants: Vec<_> = claimants
                .iter()
                .filter(|(_, class)| {
                    matches!(
                        class,
                        OwnershipClass::Exclusive | OwnershipClass::ForbiddenOverlap
                    )
                })
                .collect();

            if exclusive_claimants.len() > 1 {
                // Multiple validators claiming exclusive ownership = contradiction
                let (pack1_name, _) = &exclusive_claimants[0];
                let (pack2_name, _) = &exclusive_claimants[1];

                // Parse pack names back to AtomicPackId for conflict report
                let first_pack = Self::parse_pack_id_or_fallback(
                    pack1_name,
                    AtomicPackClass::ValidatorProtocolVisibleValues,
                );
                let second_pack = Self::parse_pack_id_or_fallback(
                    pack2_name,
                    AtomicPackClass::ValidatorProtocolVisibleValues,
                );

                conflicts.push(Conflict::new(
                    CompatibilityDimension::ValidatorContradiction,
                    vec![first_pack, second_pack],
                    format!(
                        "Validator contradiction: multiple validators claim exclusive ownership of protocol field '{}'",
                        field_name
                    ),
                    ConflictSeverity::Error,
                ).with_resolution(ConflictResolution::Fail));
            }

            // Check for incompatible ownership class combinations
            // (e.g., one says Exclusive, another says Mergeable on same field)
            if claimants.len() > 1 {
                for (i, (pack_a_name, class_a)) in claimants.iter().enumerate() {
                    for (pack_b_name, class_b) in claimants.iter().skip(i + 1) {
                        if class_a != class_b {
                            // Different ownership classes on same field = potential contradiction
                            let requires_exclusive = matches!(
                                class_a,
                                OwnershipClass::Exclusive | OwnershipClass::ForbiddenOverlap
                            ) || matches!(
                                class_b,
                                OwnershipClass::Exclusive | OwnershipClass::ForbiddenOverlap
                            );

                            if requires_exclusive {
                                let pack_a = Self::parse_pack_id_or_fallback(
                                    pack_a_name,
                                    AtomicPackClass::ValidatorProtocolVisibleValues,
                                );
                                let pack_b = Self::parse_pack_id_or_fallback(
                                    pack_b_name,
                                    AtomicPackClass::ValidatorProtocolVisibleValues,
                                );

                                conflicts.push(Conflict::new(
                                    CompatibilityDimension::ValidatorContradiction,
                                    vec![pack_a, pack_b],
                                    format!(
                                        "Validator contradiction: incompatible ownership classes for protocol field '{}' - {:?} vs {:?}",
                                        field_name, class_a, class_b
                                    ),
                                    ConflictSeverity::Error,
                                ).with_resolution(ConflictResolution::Fail));
                            }
                        }
                    }
                }
            }
        }

        Ok(conflicts)
    }
    /// Parse an `AtomicPackId` from a string, falling back to a constructed ID
    /// using the provided class and the raw string as the name.
    fn parse_pack_id_or_fallback(name: &str, fallback_class: AtomicPackClass) -> AtomicPackId {
        AtomicPackId::from_str(name)
            .unwrap_or_else(|| AtomicPackId::new(fallback_class, name.to_string()))
    }

    /// Load ownership declarations for a validator pack from pack metadata.
    ///
    /// Checks for ownership.json first (primary format), then falls back to
    /// package.toml [ownership] section. Returns empty Vec if no metadata exists.
    ///
    /// # Errors
    ///
    /// Returns error if ownership metadata exists but is malformed.
    fn load_validator_ownership_declarations(
        validator: &AtomicPackId, cache_dir: &std::path::Path,
    ) -> Result<Vec<OwnershipDeclaration>> {
        // Construct pack directory path: {cache_dir}/{pack_id}/
        let pack_id_str = validator.to_string();
        let pack_dir = cache_dir.join(&pack_id_str);

        // Try ownership.json first (primary format)
        let ownership_json_path = pack_dir.join("ownership.json");
        if ownership_json_path.exists() {
            return Self::load_ownership_json(&ownership_json_path);
        }

        // Fallback to package.toml [ownership] section
        let package_toml_path = pack_dir.join("package.toml");
        if package_toml_path.exists() {
            return Self::load_ownership_from_toml(&package_toml_path);
        }

        // No ownership metadata found (not an error)
        tracing::debug!(
            "No ownership metadata found for pack {} in {:?}",
            pack_id_str,
            pack_dir
        );
        Ok(Vec::new())
    }

    /// Load ownership declarations from ownership.json file.
    ///
    /// # Errors
    ///
    /// Returns error if file cannot be read or JSON is malformed.
    fn load_ownership_json(path: &std::path::Path) -> Result<Vec<OwnershipDeclaration>> {
        use std::io::Read;

        let mut file = std::fs::File::open(path).map_err(|e| Error::IoError(e))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| Error::IoError(e))?;

        // Parse JSON array of OwnershipDeclaration objects
        let declarations: Vec<OwnershipDeclaration> =
            serde_json::from_str(&contents).map_err(|e| Error::SerializationError(e))?;

        tracing::debug!(
            "Loaded {} ownership declarations from {:?}",
            declarations.len(),
            path
        );

        Ok(declarations)
    }

    /// Load ownership declarations from package.toml [ownership] section.
    ///
    /// # Errors
    ///
    /// Returns error if TOML is malformed or required fields are missing.
    fn load_ownership_from_toml(path: &std::path::Path) -> Result<Vec<OwnershipDeclaration>> {
        use crate::ownership::{MergeStrategy, OwnershipClass};
        use std::io::Read;

        let mut file = std::fs::File::open(path).map_err(|e| Error::IoError(e))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| Error::IoError(e))?;

        // Parse TOML
        let toml_value: toml::Value = toml::from_str(&contents)?;

        // Extract [ownership.declarations] array
        let ownership_section = toml_value.get("ownership").and_then(|v| v.as_table());

        let declarations_array = ownership_section
            .and_then(|table| table.get("declarations"))
            .and_then(|v| v.as_array());

        let declarations = if let Some(array) = declarations_array {
            // Parse each declaration from TOML table
            let mut result = Vec::new();
            for (idx, decl_value) in array.iter().enumerate() {
                let decl_table = decl_value
                    .as_table()
                    .ok_or_else(|| Error::ValidationFailed {
                        reason: format!(
                            "ownership.declarations[{}] is not a table in {:?}",
                            idx, path
                        ),
                    })?;

                // Parse target (required)
                let target = Self::parse_ownership_target(decl_table, path, idx)?;

                // Parse class (required)
                let class_str = decl_table
                    .get("class")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| Error::ValidationFailed {
                        reason: format!(
                            "ownership.declarations[{}].class is missing in {:?}",
                            idx, path
                        ),
                    })?;

                let class = match class_str {
                    "exclusive" => OwnershipClass::Exclusive,
                    "mergeable" => OwnershipClass::Mergeable,
                    "overlay" => OwnershipClass::Overlay,
                    "forbidden_overlap" => OwnershipClass::ForbiddenOverlap,
                    other => {
                        tracing::warn!(
                            "Unknown ownership class '{}' in {:?}, treating as forbidden_overlap",
                            other,
                            path
                        );
                        OwnershipClass::ForbiddenOverlap
                    }
                };

                // Parse merge_strategy (optional, only for mergeable)
                let merge_strategy = if class == OwnershipClass::Mergeable {
                    if let Some(strategy_str) =
                        decl_table.get("merge_strategy").and_then(|v| v.as_str())
                    {
                        Some(match strategy_str {
                            "concat" => MergeStrategy::Concat,
                            "last_writer_wins" => MergeStrategy::LastWriterWins,
                            "first_writer_wins" => MergeStrategy::FirstWriterWins,
                            "recursive" => MergeStrategy::Recursive,
                            "fail_on_conflict" => MergeStrategy::FailOnConflict,
                            custom_query => MergeStrategy::CustomSparql {
                                query: custom_query.to_string(),
                            },
                        })
                    } else {
                        tracing::warn!(
                            "mergeable ownership in {:?}[{}] missing merge_strategy, defaulting to concat",
                            path, idx
                        );
                        Some(MergeStrategy::Concat)
                    }
                } else {
                    None
                };

                // Build declaration
                let mut declaration = OwnershipDeclaration::new(
                    target,
                    class,
                    // Note: owner_pack is inferred from directory name
                    path.parent()
                        .and_then(|p| p.file_name())
                        .and_then(|n| n.to_str())
                        .unwrap_or("unknown")
                        .to_string(),
                );
                declaration.merge_strategy = merge_strategy;

                result.push(declaration);
            }

            tracing::debug!(
                "Loaded {} ownership declarations from package.toml {:?}",
                result.len(),
                path
            );

            result
        } else {
            // No ownership section found
            Vec::new()
        };

        Ok(declarations)
    }

    /// Parse ownership target from TOML table.
    ///
    /// # Errors
    ///
    /// Returns error if no valid target field is found.
    fn parse_ownership_target(
        table: &toml::value::Table, path: &std::path::Path, decl_idx: usize,
    ) -> Result<OwnershipTarget> {
        use std::path::PathBuf;

        // Try each target type
        if let Some(file_path) = table.get("file_path").and_then(|v| v.as_str()) {
            return Ok(OwnershipTarget::FilePath(PathBuf::from(file_path)));
        }

        if let Some(namespace) = table.get("rdf_namespace").and_then(|v| v.as_str()) {
            return Ok(OwnershipTarget::RdfNamespace(namespace.to_string()));
        }

        if let Some(field) = table.get("protocol_field").and_then(|v| v.as_str()) {
            return Ok(OwnershipTarget::ProtocolField(field.to_string()));
        }

        if let Some(variable) = table.get("template_variable").and_then(|v| v.as_str()) {
            return Ok(OwnershipTarget::TemplateVariable(variable.to_string()));
        }

        if let Some(dep) = table.get("dependency_package").and_then(|v| v.as_str()) {
            return Ok(OwnershipTarget::DependencyPackage(dep.to_string()));
        }

        if let Some(flag) = table.get("feature_flag").and_then(|v| v.as_str()) {
            return Ok(OwnershipTarget::FeatureFlag(flag.to_string()));
        }

        Err(Error::ValidationFailed {
            reason: format!(
                "ownership.declarations[{}] has no valid target field in {:?} \
                 (expected one of: file_path, rdf_namespace, protocol_field, \
                  template_variable, dependency_package, feature_flag)",
                decl_idx, path
            ),
        })
    }

    /// Check policy contradictions.
    ///
    /// Validates that policy packs don't have contradictory rules. Common contradictions:
    /// - NoDefaults policy forbids what Strict policy requires
    /// - Conflicting policy constraints on same target (e.g., forbid X vs require X)
    /// - Mutually exclusive policy rules (e.g., one allows experimental, another requires certified)
    fn check_policies(&self, packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
        use crate::atomic::{AtomicPackCategory, AtomicPackClass};

        let mut conflicts = Vec::new();

        // Filter to only policy packs
        let policy_packs: Vec<_> = packs
            .iter()
            .filter(|p| p.class.category() == AtomicPackCategory::Policy)
            .collect();

        if policy_packs.is_empty() {
            return Ok(conflicts);
        }

        // Check for direct policy pack conflicts
        // The most common contradiction: NoDefaults + Strict together
        let has_no_defaults = policy_packs
            .iter()
            .any(|p| p.class == AtomicPackClass::PolicyNoDefaults);
        let has_strict = policy_packs
            .iter()
            .any(|p| p.class == AtomicPackClass::PolicyStrict);

        if has_no_defaults && has_strict {
            // Find the actual pack IDs for the conflict report
            let no_defaults_pack = policy_packs
                .iter()
                .find(|p| p.class == AtomicPackClass::PolicyNoDefaults)
                .unwrap();
            let strict_pack = policy_packs
                .iter()
                .find(|p| p.class == AtomicPackClass::PolicyStrict)
                .unwrap();

            conflicts.push(Conflict::new(
                CompatibilityDimension::PolicyContradiction,
                vec![(*no_defaults_pack).clone(), (*strict_pack).clone()],
                "Policy contradiction: NoDefaults forbids inferred capabilities, but Strict requires explicit validation - these policies have incompatible assumptions about capability declaration".to_string(),
                ConflictSeverity::Error,
            ).with_resolution(ConflictResolution::UserResolve));
        }

        // Check for duplicate policy packs (same policy class twice)
        let mut policy_classes_seen: std::collections::HashMap<AtomicPackClass, Vec<AtomicPackId>> =
            std::collections::HashMap::new();

        for policy in &policy_packs {
            policy_classes_seen
                .entry(policy.class)
                .or_default()
                .push((*policy).clone());
        }

        for (policy_class, pack_ids) in &policy_classes_seen {
            if pack_ids.len() > 1 {
                // Same policy class applied multiple times = contradiction
                conflicts.push(Conflict::new(
                    CompatibilityDimension::PolicyContradiction,
                    pack_ids.clone(),
                    format!(
                        "Policy contradiction: duplicate policy class {:?} applied multiple times - policies should be applied once per composition",
                        policy_class
                    ),
                    ConflictSeverity::Warning,
                ).with_resolution(ConflictResolution::PreferFirst));
            }
        }

        // Load policy rules from pack metadata and check for deeper contradictions
        let loaded_policies = self.load_policy_rules(&policy_packs)?;
        conflicts.extend(Self::check_policy_rule_contradictions(&loaded_policies));

        Ok(conflicts)
    }

    /// Load policy rules from pack metadata files.
    ///
    /// Reads `policy.json` or `[policy]` section in `package.toml` for each
    /// policy pack. Returns a list of (pack_id, rules) tuples.
    fn load_policy_rules(
        &self, policy_packs: &[&AtomicPackId],
    ) -> Result<Vec<(AtomicPackId, Vec<String>)>> {
        let mut result = Vec::new();

        for policy_pack in policy_packs {
            let pack_dir = self.cache_dir.join(policy_pack.to_string());
            let rules = self.load_policy_rules_from_dir(&pack_dir);
            if !rules.is_empty() {
                result.push(((*policy_pack).clone(), rules));
            }
        }

        Ok(result)
    }

    /// Load policy rule names from a pack directory.
    fn load_policy_rules_from_dir(&self, pack_dir: &std::path::Path) -> Vec<String> {
        // Try policy.json first
        let policy_json = pack_dir.join("policy.json");
        if policy_json.exists() {
            if let Ok(rules) = Self::load_policy_rules_json(&policy_json) {
                return rules;
            }
        }

        // Fallback to package.toml [policy.rules]
        let package_toml = pack_dir.join("package.toml");
        if package_toml.exists() {
            if let Ok(rules) = Self::load_policy_rules_toml(&package_toml) {
                return rules;
            }
        }

        Vec::new()
    }

    /// Load policy rule names from policy.json.
    fn load_policy_rules_json(path: &std::path::Path) -> Result<Vec<String>> {
        use std::io::Read;

        let mut file = std::fs::File::open(path).map_err(|e| Error::IoError(e))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| Error::IoError(e))?;

        #[derive(serde::Deserialize)]
        struct PolicyJson {
            rules: Option<Vec<PolicyRuleEntry>>,
        }

        #[derive(serde::Deserialize)]
        struct PolicyRuleEntry {
            #[serde(default)]
            name: Option<String>,
            #[serde(rename = "type", default)]
            rule_type: Option<String>,
        }

        let policy: PolicyJson =
            serde_json::from_str(&contents).map_err(Error::SerializationError)?;

        let rules = policy
            .rules
            .unwrap_or_default()
            .into_iter()
            .map(|r| {
                r.name
                    .unwrap_or_else(|| r.rule_type.unwrap_or_else(|| "unknown".to_string()))
            })
            .collect();

        Ok(rules)
    }

    /// Load policy rule names from package.toml [policy.rules].
    fn load_policy_rules_toml(path: &std::path::Path) -> Result<Vec<String>> {
        use std::io::Read;

        let mut file = std::fs::File::open(path).map_err(|e| Error::IoError(e))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| Error::IoError(e))?;

        let toml_value: toml::Value = toml::from_str(&contents)?;

        let rules_array = toml_value
            .get("policy")
            .and_then(|v| v.get("rules"))
            .and_then(|v| v.as_array());

        let rules = if let Some(array) = rules_array {
            array
                .iter()
                .filter_map(|entry| {
                    entry
                        .get("name")
                        .or_else(|| entry.get("type"))
                        .and_then(|v| v.as_str())
                        .map(String::from)
                })
                .collect()
        } else {
            Vec::new()
        };

        Ok(rules)
    }

    /// Check loaded policy rules for contradictions.
    ///
    /// Detects conflicting trust tier requirements, incompatible runtime constraints,
    /// and mutually exclusive feature flags across policy packs.
    fn check_policy_rule_contradictions(
        loaded_policies: &[(AtomicPackId, Vec<String>)],
    ) -> Vec<Conflict> {
        let mut conflicts = Vec::new();

        // Collect trust tier requirements across all policy packs
        let mut trust_tier_requirements: Vec<(&AtomicPackId, &str)> = Vec::new();
        let mut runtime_constraints: Vec<(&AtomicPackId, &str)> = Vec::new();

        for (pack_id, rules) in loaded_policies {
            for rule in rules {
                let rule_lower = rule.to_lowercase();
                if rule_lower.contains("trust_tier")
                    || rule_lower.contains("trust-tier")
                    || rule_lower.contains("trusttier")
                {
                    trust_tier_requirements.push((pack_id, rule.as_str()));
                }
                if rule_lower.contains("runtime") || rule_lower.contains("approved_runtime") {
                    runtime_constraints.push((pack_id, rule.as_str()));
                }
            }
        }

        // Check for conflicting trust tier requirements
        // If one policy requires EnterpriseCertified and another allows Experimental, that's a conflict
        for (i, (pack1, rule1)) in trust_tier_requirements.iter().enumerate() {
            for (pack2, rule2) in trust_tier_requirements.iter().skip(i + 1) {
                if rule1 != rule2 {
                    conflicts.push(Conflict::new(
                        CompatibilityDimension::PolicyContradiction,
                        vec![(*pack1).clone(), (*pack2).clone()],
                        format!(
                            "Policy contradiction: conflicting trust tier requirements - '{}' vs '{}'",
                            rule1, rule2
                        ),
                        ConflictSeverity::Warning,
                    ).with_resolution(ConflictResolution::UserResolve)
                    .with_context("rule_type".to_string(), "trust_tier".to_string()));
                }
            }
        }

        // Check for conflicting runtime constraints
        for (i, (pack1, rule1)) in runtime_constraints.iter().enumerate() {
            for (pack2, rule2) in runtime_constraints.iter().skip(i + 1) {
                if rule1 != rule2 {
                    conflicts.push(
                        Conflict::new(
                            CompatibilityDimension::PolicyContradiction,
                            vec![(*pack1).clone(), (*pack2).clone()],
                            format!(
                            "Policy contradiction: incompatible runtime constraints - '{}' vs '{}'",
                            rule1, rule2
                        ),
                            ConflictSeverity::Warning,
                        )
                        .with_resolution(ConflictResolution::UserResolve)
                        .with_context("rule_type".to_string(), "runtime".to_string()),
                    );
                }
            }
        }

        conflicts
    }

    /// Check version range conflicts.
    fn check_versions(packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
        let mut conflicts = Vec::new();

        // Check for duplicate pack IDs with different versions
        let mut pack_map: HashMap<String, Vec<AtomicPackId>> = HashMap::new();

        for pack in packs {
            let name = pack
                .to_string()
                .split('-')
                .next()
                .unwrap_or("unknown")
                .to_string();
            pack_map.entry(name).or_default().push(pack.clone());
        }

        for (_name, pack_list) in pack_map {
            if pack_list.len() > 1 {
                conflicts.push(
                    Conflict::info(
                        CompatibilityDimension::VersionRange,
                        pack_list.clone(),
                        "Multiple versions of same pack detected".to_string(),
                    )
                    .with_resolution(ConflictResolution::PreferLast),
                );
            }
        }

        Ok(conflicts)
    }

    /// Check capability identity conflicts.
    ///
    /// Validates that capability packs don't have identity collisions. Common conflicts:
    /// - Multiple packs of same exact class (e.g., 2x SurfaceMcp) = ERROR
    /// - Multiple surface packs of same category (e.g., SurfaceMcp + SurfaceA2a) = WARNING
    /// - Multiple contract packs defining same contract = ERROR
    /// - Different capability categories = No conflict
    fn check_capabilities(packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
        use crate::atomic::{AtomicPackCategory, AtomicPackClass};

        let mut conflicts = Vec::new();

        // Filter to only capability packs (exclude core, policy, validator, receipt, consequence)
        let capability_packs: Vec<_> = packs
            .iter()
            .filter(|p| {
                matches!(
                    p.class.category(),
                    AtomicPackCategory::Surface
                        | AtomicPackCategory::Contract
                        | AtomicPackCategory::Projection
                        | AtomicPackCategory::Runtime
                )
            })
            .collect();

        if capability_packs.is_empty() {
            return Ok(conflicts);
        }

        // Rule 1: Check for duplicate exact classes (e.g., 2x SurfaceMcp) = ERROR
        let mut class_map: std::collections::HashMap<AtomicPackClass, Vec<AtomicPackId>> =
            std::collections::HashMap::new();

        for pack in &capability_packs {
            class_map
                .entry(pack.class)
                .or_default()
                .push((*pack).clone());
        }

        for (class, pack_ids) in &class_map {
            if pack_ids.len() > 1 {
                // Same exact class multiple times = hard error
                conflicts.push(Conflict::new(
                    CompatibilityDimension::CapabilityIdentity,
                    pack_ids.clone(),
                    format!(
                        "Capability identity conflict: duplicate class '{:?}' detected. Multiple packs of the same exact class cannot be installed together.",
                        class
                    ),
                    ConflictSeverity::Error,
                )
                .with_resolution(ConflictResolution::Fail)
                .with_context("class".to_string(), format!("{:?}", class))
                .with_context("count".to_string(), pack_ids.len().to_string()));
            }
        }

        // Rule 2: Check for same-category, different classes = WARNING
        // Surface category: SurfaceMcp + SurfaceA2a = warning (potential conflict)
        let mut category_packs: std::collections::HashMap<AtomicPackCategory, Vec<AtomicPackId>> =
            std::collections::HashMap::new();

        for pack in &capability_packs {
            category_packs
                .entry(pack.class.category())
                .or_default()
                .push((*pack).clone());
        }

        let category_has_distinct_classes = |packs: &[AtomicPackId]| {
            packs.len() > 1 && packs.iter().any(|p| p.class != packs[0].class)
        };

        // Check Surface category
        if let Some(surface_packs) = category_packs.get(&AtomicPackCategory::Surface) {
            if category_has_distinct_classes(surface_packs) {
                // Multiple surface packs = potential conflict
                conflicts.push(Conflict::new(
                    CompatibilityDimension::CapabilityIdentity,
                    surface_packs.clone(),
                    format!(
                        "Capability identity warning: multiple surface packs detected ({:?}). Different surface protocols (MCP, A2A) may be incompatible in the same composition.",
                        surface_packs.iter().map(|p| p.class).collect::<Vec<_>>()
                    ),
                    ConflictSeverity::Warning,
                )
                .with_resolution(ConflictResolution::UserResolve)
                .with_context("category".to_string(), "Surface".to_string())
                .with_context("classes".to_string(), format!("{:?}", surface_packs.iter().map(|p| p.class).collect::<Vec<_>>())));
            }
        }

        // Check Contract category
        if let Some(contract_packs) = category_packs.get(&AtomicPackCategory::Contract) {
            if category_has_distinct_classes(contract_packs) {
                // Multiple contract packs = error (mutually exclusive contract definitions)
                conflicts.push(Conflict::new(
                    CompatibilityDimension::CapabilityIdentity,
                    contract_packs.clone(),
                    format!(
                        "Capability identity conflict: multiple contract packs detected ({:?}). Different API contracts (OpenAPI, GraphQL) cannot coexist in the same composition.",
                        contract_packs.iter().map(|p| p.class).collect::<Vec<_>>()
                    ),
                    ConflictSeverity::Error,
                )
                .with_resolution(ConflictResolution::Fail)
                .with_context("category".to_string(), "Contract".to_string())
                .with_context("classes".to_string(), format!("{:?}", contract_packs.iter().map(|p| p.class).collect::<Vec<_>>())));
            }
        }

        // Rule 3: Check Projection category = WARNING (multiple language projections OK but unusual)
        if let Some(projection_packs) = category_packs.get(&AtomicPackCategory::Projection) {
            if category_has_distinct_classes(projection_packs) {
                conflicts.push(Conflict::new(
                    CompatibilityDimension::CapabilityIdentity,
                    projection_packs.clone(),
                    format!(
                        "Capability identity warning: multiple projection packs detected ({:?}). Multiple language projections are supported but may indicate configuration error.",
                        projection_packs.iter().map(|p| p.class).collect::<Vec<_>>()
                    ),
                    ConflictSeverity::Warning,
                )
                .with_resolution(ConflictResolution::UserResolve)
                .with_context("category".to_string(), "Projection".to_string())
                .with_context("classes".to_string(), format!("{:?}", projection_packs.iter().map(|p| p.class).collect::<Vec<_>>())));
            }
        }

        // Rule 4: Check Runtime category = ERROR (multiple runtimes are incompatible)
        if let Some(runtime_packs) = category_packs.get(&AtomicPackCategory::Runtime) {
            if category_has_distinct_classes(runtime_packs) {
                conflicts.push(Conflict::new(
                    CompatibilityDimension::CapabilityIdentity,
                    runtime_packs.clone(),
                    format!(
                        "Capability identity conflict: multiple runtime packs detected ({:?}). Different deployment runtimes (stdio, axum, actix, embedded, standalone) cannot be used simultaneously.",
                        runtime_packs.iter().map(|p| p.class).collect::<Vec<_>>()
                    ),
                    ConflictSeverity::Error,
                )
                .with_resolution(ConflictResolution::Fail)
                .with_context("category".to_string(), "Runtime".to_string())
                .with_context("classes".to_string(), format!("{:?}", runtime_packs.iter().map(|p| p.class).collect::<Vec<_>>())));
            }
        }

        // Rule 5: Different categories = No conflict (e.g., Surface + Projection is valid)
        // This is the happy path - no conflicts added

        Ok(conflicts)
    }

    /// Check receipt schema incompatibilities.
    ///
    /// Validates that receipt packs don't have incompatible schema requirements.
    /// Common incompatibilities:
    /// - Chained receipts require hash-linking, forbid standalone receipts
    /// - Enterprise-signed receipts require Ed25519 signatures, forbid unsigned
    /// - Conflicting receipt format requirements (e.g., one requires chaining, another forbids it)
    fn check_receipt_schemas(&self, packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
        use crate::atomic::{AtomicPackCategory, AtomicPackClass};

        let mut conflicts = Vec::new();

        // Filter to only receipt packs
        let receipt_packs: Vec<_> = packs
            .iter()
            .filter(|p| p.class.category() == AtomicPackCategory::Receipt)
            .collect();

        if receipt_packs.is_empty() {
            return Ok(conflicts);
        }

        // Check for incompatible receipt type combinations
        let has_chained = receipt_packs
            .iter()
            .any(|p| p.class == AtomicPackClass::ReceiptChained);
        let has_enterprise_signed = receipt_packs
            .iter()
            .any(|p| p.class == AtomicPackClass::ReceiptEnterpriseSigned);

        // Schema requirement definitions
        // Chained receipts: Require hash-linking, incompatible with standalone receipts
        // Enterprise-signed: Require Ed25519 signatures, incompatible with unsigned receipts
        if has_chained && has_enterprise_signed {
            // Find the actual pack IDs for the conflict report
            let chained_pack = receipt_packs
                .iter()
                .find(|p| p.class == AtomicPackClass::ReceiptChained)
                .unwrap();
            let enterprise_pack = receipt_packs
                .iter()
                .find(|p| p.class == AtomicPackClass::ReceiptEnterpriseSigned)
                .unwrap();

            conflicts.push(Conflict::new(
                CompatibilityDimension::ReceiptSchema,
                vec![(*chained_pack).clone(), (*enterprise_pack).clone()],
                "Receipt schema incompatibility: Chained receipts require hash-linking and cannot be combined with enterprise-signed receipts which require Ed25519 signatures. These are mutually exclusive receipt formats with different cryptographic requirements.".to_string(),
                ConflictSeverity::Error,
            ).with_resolution(ConflictResolution::Fail));
        }

        // Check for duplicate receipt packs (same receipt type twice)
        let mut receipt_classes_seen: std::collections::HashMap<
            AtomicPackClass,
            Vec<AtomicPackId>,
        > = std::collections::HashMap::new();

        for receipt in &receipt_packs {
            receipt_classes_seen
                .entry(receipt.class)
                .or_default()
                .push((*receipt).clone());
        }

        for (receipt_class, pack_ids) in &receipt_classes_seen {
            if pack_ids.len() > 1 {
                // Same receipt class applied multiple times = warning (not necessarily an error)
                conflicts.push(Conflict::new(
                    CompatibilityDimension::ReceiptSchema,
                    pack_ids.clone(),
                    format!(
                        "Receipt schema warning: duplicate receipt class {:?} applied multiple times - a single receipt mechanism should be used for consistency",
                        receipt_class
                    ),
                    ConflictSeverity::Warning,
                ).with_resolution(ConflictResolution::PreferFirst));
            }
        }

        // Load receipt schema metadata from pack definitions and check for deeper contradictions
        let loaded_schemas = self.load_receipt_schema_metadata(&receipt_packs)?;
        conflicts.extend(Self::check_receipt_schema_contradictions(&loaded_schemas));

        Ok(conflicts)
    }

    /// Load receipt schema metadata from pack metadata files.
    ///
    /// Reads `receipt.json` or `[receipt]` section in `package.toml` for each
    /// receipt pack. Returns schema metadata tuples.
    fn load_receipt_schema_metadata(
        &self, receipt_packs: &[&AtomicPackId],
    ) -> Result<Vec<(AtomicPackId, ReceiptSchemaMeta)>> {
        let mut result = Vec::new();

        for receipt_pack in receipt_packs {
            let pack_dir = self.cache_dir.join(receipt_pack.to_string());

            // Try receipt.json first
            let receipt_json = pack_dir.join("receipt.json");
            if receipt_json.exists() {
                if let Ok(meta) = Self::load_receipt_schema_json(&receipt_json) {
                    result.push(((*receipt_pack).clone(), meta));
                    continue;
                }
            }

            // Fallback to package.toml [receipt] section
            let package_toml = pack_dir.join("package.toml");
            if package_toml.exists() {
                if let Ok(meta) = Self::load_receipt_schema_toml(&package_toml) {
                    result.push(((*receipt_pack).clone(), meta));
                }
            }

            // Default metadata from pack class
            let default_meta = match receipt_pack.class {
                crate::atomic::AtomicPackClass::ReceiptChained => ReceiptSchemaMeta {
                    schema_version: "1".to_string(),
                    signature_algorithm: None,
                    hash_function: Some("sha256".to_string()),
                    chain_depth_max: None,
                },
                crate::atomic::AtomicPackClass::ReceiptEnterpriseSigned => ReceiptSchemaMeta {
                    schema_version: "1".to_string(),
                    signature_algorithm: Some("ed25519".to_string()),
                    hash_function: Some("sha256".to_string()),
                    chain_depth_max: None,
                },
                _ => ReceiptSchemaMeta::default(),
            };
            result.push(((*receipt_pack).clone(), default_meta));
        }

        Ok(result)
    }

    /// Load receipt schema metadata from receipt.json.
    fn load_receipt_schema_json(path: &std::path::Path) -> Result<ReceiptSchemaMeta> {
        use std::io::Read;

        let mut file = std::fs::File::open(path).map_err(|e| Error::IoError(e))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| Error::IoError(e))?;

        #[derive(serde::Deserialize)]
        struct ReceiptJson {
            #[serde(default)]
            schema_version: Option<String>,
            #[serde(default)]
            signature_algorithm: Option<String>,
            #[serde(default)]
            hash_function: Option<String>,
            #[serde(default)]
            chain_depth_max: Option<u32>,
        }

        let receipt: ReceiptJson =
            serde_json::from_str(&contents).map_err(Error::SerializationError)?;

        Ok(ReceiptSchemaMeta {
            schema_version: receipt.schema_version.unwrap_or_else(|| "1".to_string()),
            signature_algorithm: receipt.signature_algorithm,
            hash_function: receipt.hash_function,
            chain_depth_max: receipt.chain_depth_max,
        })
    }

    /// Load receipt schema metadata from package.toml [receipt] section.
    fn load_receipt_schema_toml(path: &std::path::Path) -> Result<ReceiptSchemaMeta> {
        use std::io::Read;

        let mut file = std::fs::File::open(path).map_err(|e| Error::IoError(e))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| Error::IoError(e))?;

        let toml_value: toml::Value = toml::from_str(&contents)?;

        let receipt_section = toml_value.get("receipt").and_then(|v| v.as_table());

        Ok(ReceiptSchemaMeta {
            schema_version: receipt_section
                .and_then(|t| t.get("schema_version"))
                .and_then(|v| v.as_str())
                .unwrap_or("1")
                .to_string(),
            signature_algorithm: receipt_section
                .and_then(|t| t.get("signature_algorithm"))
                .and_then(|v| v.as_str())
                .map(String::from),
            hash_function: receipt_section
                .and_then(|t| t.get("hash_function"))
                .and_then(|v| v.as_str())
                .map(String::from),
            chain_depth_max: receipt_section
                .and_then(|t| t.get("chain_depth_max"))
                .and_then(|v| v.as_integer())
                .map(|v| v as u32),
        })
    }

    /// Check loaded receipt schema metadata for contradictions.
    ///
    /// Detects:
    /// - Incompatible receipt format versions (e.g., v1 vs v2 schemas)
    /// - Conflicting signature algorithms (e.g., Ed25519 vs RSA)
    /// - Hash function mismatches (e.g., SHA-256 vs SHA-3)
    /// - Chain depth requirements (e.g., one requires depth 10, another forbids >5)
    fn check_receipt_schema_contradictions(
        loaded_schemas: &[(AtomicPackId, ReceiptSchemaMeta)],
    ) -> Vec<Conflict> {
        let mut conflicts = Vec::new();

        for (i, (pack1, meta1)) in loaded_schemas.iter().enumerate() {
            for (pack2, meta2) in loaded_schemas.iter().skip(i + 1) {
                // Check for incompatible receipt format versions
                if meta1.schema_version != meta2.schema_version {
                    conflicts.push(Conflict::new(
                        CompatibilityDimension::ReceiptSchema,
                        vec![(*pack1).clone(), (*pack2).clone()],
                        format!(
                            "Receipt schema incompatibility: incompatible format versions - pack '{}' uses v{} but pack '{}' uses v{}",
                            pack1, meta1.schema_version, pack2, meta2.schema_version
                        ),
                        ConflictSeverity::Warning,
                    ).with_resolution(ConflictResolution::UserResolve)
                    .with_context("issue".to_string(), "schema_version_mismatch".to_string()));
                }

                // Check for conflicting signature algorithms
                match (&meta1.signature_algorithm, &meta2.signature_algorithm) {
                    (Some(algo1), Some(algo2)) if algo1 != algo2 => {
                        conflicts.push(Conflict::new(
                            CompatibilityDimension::ReceiptSchema,
                            vec![(*pack1).clone(), (*pack2).clone()],
                            format!(
                                "Receipt schema incompatibility: conflicting signature algorithms - pack '{}' requires '{}' but pack '{}' requires '{}'",
                                pack1, algo1, pack2, algo2
                            ),
                            ConflictSeverity::Error,
                        ).with_resolution(ConflictResolution::Fail)
                        .with_context("issue".to_string(), "signature_algorithm_conflict".to_string()));
                    }
                    _ => {}
                }

                // Check for hash function mismatches
                match (&meta1.hash_function, &meta2.hash_function) {
                    (Some(hash1), Some(hash2)) if hash1 != hash2 => {
                        conflicts.push(Conflict::new(
                            CompatibilityDimension::ReceiptSchema,
                            vec![(*pack1).clone(), (*pack2).clone()],
                            format!(
                                "Receipt schema warning: hash function mismatch - pack '{}' uses '{}' but pack '{}' uses '{}'. Cross-verification may fail.",
                                pack1, hash1, pack2, hash2
                            ),
                            ConflictSeverity::Warning,
                        ).with_resolution(ConflictResolution::UserResolve)
                        .with_context("issue".to_string(), "hash_function_mismatch".to_string()));
                    }
                    _ => {}
                }

                // Check for chain depth requirement conflicts
                match (meta1.chain_depth_max, meta2.chain_depth_max) {
                    (Some(max1), Some(max2)) if max1 != max2 => {
                        conflicts.push(Conflict::new(
                            CompatibilityDimension::ReceiptSchema,
                            vec![(*pack1).clone(), (*pack2).clone()],
                            format!(
                                "Receipt schema warning: conflicting chain depth requirements - pack '{}' requires max depth {} but pack '{}' requires max depth {}",
                                pack1, max1, pack2, max2
                            ),
                            ConflictSeverity::Warning,
                        ).with_resolution(ConflictResolution::UserResolve)
                        .with_context("issue".to_string(), "chain_depth_conflict".to_string()));
                    }
                    _ => {}
                }
            }
        }

        conflicts
    }

    /// Check consequence migration conflicts.
    fn check_consequences(packs: &[AtomicPackId]) -> Result<Vec<Conflict>> {
        use crate::atomic::{AtomicPackCategory, AtomicPackClass};

        let mut conflicts = Vec::new();

        // Filter to only consequence packs
        let consequence_packs: Vec<_> = packs
            .iter()
            .filter(|p| p.class.category() == AtomicPackCategory::Consequence)
            .collect();

        if consequence_packs.is_empty() {
            return Ok(conflicts);
        }

        // Check for direct consequence pack conflicts
        // The most critical contradiction: SemverMigration vs BreakingChange
        // SemverMigration requires semantic versioning and forbids breaking changes without version bump
        // BreakingChange explicitly allows breaking changes, which violates semver principles
        let has_semver_migration = consequence_packs
            .iter()
            .any(|p| p.class == AtomicPackClass::ConsequenceSemverMigration);
        let has_breaking_change = consequence_packs
            .iter()
            .any(|p| p.class == AtomicPackClass::ConsequenceBreakingChange);

        if has_semver_migration && has_breaking_change {
            // Find the actual pack IDs for the conflict report
            let semver_pack = consequence_packs
                .iter()
                .find(|p| p.class == AtomicPackClass::ConsequenceSemverMigration)
                .unwrap();
            let breaking_pack = consequence_packs
                .iter()
                .find(|p| p.class == AtomicPackClass::ConsequenceBreakingChange)
                .unwrap();

            conflicts.push(Conflict::new(
                CompatibilityDimension::ConsequenceMigration,
                vec![(*semver_pack).clone(), (*breaking_pack).clone()],
                "Consequence migration incompatibility: SemverMigration requires semantic versioning with automatic version increments for breaking changes, but BreakingChange explicitly allows breaking changes without proper versioning - these migration strategies are fundamentally incompatible".to_string(),
                ConflictSeverity::Error,
            ).with_resolution(ConflictResolution::Fail));
        }

        // Check for duplicate consequence packs (same consequence class twice)
        let mut consequence_classes_seen: std::collections::HashMap<
            AtomicPackClass,
            Vec<AtomicPackId>,
        > = std::collections::HashMap::new();

        for consequence in &consequence_packs {
            consequence_classes_seen
                .entry(consequence.class)
                .or_default()
                .push((*consequence).clone());
        }

        for (consequence_class, pack_ids) in &consequence_classes_seen {
            if pack_ids.len() > 1 {
                // Same consequence class applied multiple times = potential conflict
                conflicts.push(Conflict::new(
                    CompatibilityDimension::ConsequenceMigration,
                    pack_ids.clone(),
                    format!(
                        "Consequence migration conflict: duplicate consequence class {:?} applied multiple times - migration strategies should be applied once per composition",
                        consequence_class
                    ),
                    ConflictSeverity::Warning,
                ).with_resolution(ConflictResolution::PreferFirst));
            }
        }

        Ok(conflicts)
    }
}

impl Default for CompatibilityChecker {
    fn default() -> Self {
        Self::with_cache_dir(
            dirs::cache_dir()
                .unwrap_or_else(|| std::path::PathBuf::from(".cache"))
                .join("ggen")
                .join("packs"),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_conflict_creation() {
        let pack1 = AtomicPackId::new(AtomicPackClass::CoreOntology, "pack1".to_string());
        let pack2 = AtomicPackId::new(AtomicPackClass::CoreHooks, "pack2".to_string());

        let conflict = Conflict::error(
            CompatibilityDimension::OntologyNamespace,
            vec![pack1.clone(), pack2.clone()],
            "Namespace conflict".to_string(),
        );

        assert!(conflict.is_error());
        assert!(!conflict.is_warning());
        assert_eq!(conflict.packs.len(), 2);
    }

    #[test]
    fn test_conflict_with_resolution() {
        let pack = AtomicPackId::new(AtomicPackClass::CoreOntology, "test".to_string());

        let conflict = Conflict::warning(
            CompatibilityDimension::EmittedFilePath,
            vec![pack],
            "File conflict".to_string(),
        )
        .with_resolution(ConflictResolution::PreferFirst);

        assert!(conflict.resolution.is_some());
        assert!(conflict.is_warning());
    }

    #[test]
    fn test_compatibility_report_success() {
        let packs = vec![
            AtomicPackId::new(
                crate::atomic::AtomicPackClass::CoreOntology,
                "pack1".to_string(),
            ),
            AtomicPackId::new(
                crate::atomic::AtomicPackClass::CoreOntology,
                "pack2".to_string(),
            ),
        ];

        let report = CompatibilityReport::success(packs);
        assert!(report.compatible);
        assert_eq!(report.conflicts.len(), 0);
        assert_eq!(report.errors().len(), 0);
    }

    #[test]
    fn test_compatibility_report_failure() {
        let pack1 = AtomicPackId::new(AtomicPackClass::CoreOntology, "pack1".to_string());
        let pack2 = AtomicPackId::new(AtomicPackClass::CoreHooks, "pack2".to_string());

        let conflict = Conflict::error(
            CompatibilityDimension::CapabilityIdentity,
            vec![pack1, pack2],
            "Duplicate capability".to_string(),
        );

        let report = CompatibilityReport::failure(vec![conflict], vec![]);
        assert!(!report.compatible);
        assert_eq!(report.errors().len(), 1);
    }

    #[test]
    fn test_check_ontology_namespaces() {
        let pack1 = AtomicPackId::new(AtomicPackClass::SurfaceMcp, "server".to_string());
        let pack2 = AtomicPackId::new(AtomicPackClass::SurfaceMcp, "client".to_string());

        let conflicts = CompatibilityChecker::check_ontology_namespaces(&[pack1, pack2]).unwrap();
        // Should find conflict (same "mcp" namespace)
        assert!(!conflicts.is_empty());
    }

    #[test]
    fn test_check_protocol_fields() {
        let decl1 = OwnershipDeclaration::exclusive(
            OwnershipTarget::ProtocolField("api_version".to_string()),
            "pack1".to_string(),
        );

        let decl2 = OwnershipDeclaration::exclusive(
            OwnershipTarget::ProtocolField("api_version".to_string()),
            "pack2".to_string(),
        );

        let conflicts = CompatibilityChecker::check_protocol_fields(&[decl1, decl2]).unwrap();
        assert!(!conflicts.is_empty());
        assert_eq!(
            conflicts[0].dimension,
            CompatibilityDimension::ProtocolField
        );
    }

    #[test]
    fn test_check_emitted_files() {
        let decl1 = OwnershipDeclaration::exclusive(
            OwnershipTarget::FilePath(PathBuf::from("/src/main.rs")),
            "pack1".to_string(),
        );

        let decl2 = OwnershipDeclaration::mergeable(
            OwnershipTarget::FilePath(PathBuf::from("/src/main.rs")),
            "pack2".to_string(),
            MergeStrategy::Concat,
        );

        let conflicts = CompatibilityChecker::check_emitted_files(&[decl1, decl2]).unwrap();
        // Should find conflict but with mergeable resolution
        assert!(!conflicts.is_empty());
    }

    #[test]
    fn test_conflicts_by_dimension() {
        let pack1 = AtomicPackId::new(AtomicPackClass::CoreOntology, "pack1".to_string());
        let pack2 = AtomicPackId::new(AtomicPackClass::CoreHooks, "pack2".to_string());

        let conflicts = vec![
            Conflict::error(
                CompatibilityDimension::OntologyNamespace,
                vec![pack1.clone(), pack2.clone()],
                "Namespace conflict".to_string(),
            ),
            Conflict::warning(
                CompatibilityDimension::EmittedFilePath,
                vec![pack1, pack2],
                "File conflict".to_string(),
            ),
        ];

        let report = CompatibilityReport::failure(conflicts, vec![]);

        assert_eq!(
            report
                .conflicts_by_dimension(CompatibilityDimension::OntologyNamespace)
                .len(),
            1
        );
        assert_eq!(
            report
                .conflicts_by_dimension(CompatibilityDimension::EmittedFilePath)
                .len(),
            1
        );
        assert_eq!(
            report
                .conflicts_by_dimension(CompatibilityDimension::ProtocolField)
                .len(),
            0
        );
    }

    #[test]
    fn test_count_by_severity() {
        let pack1 = AtomicPackId::new(AtomicPackClass::CoreOntology, "pack1".to_string());
        let pack2 = AtomicPackId::new(AtomicPackClass::CoreHooks, "pack2".to_string());

        let conflicts = vec![
            Conflict::error(
                CompatibilityDimension::OntologyNamespace,
                vec![pack1.clone(), pack2.clone()],
                "Error".to_string(),
            ),
            Conflict::warning(
                CompatibilityDimension::EmittedFilePath,
                vec![pack1.clone(), pack2.clone()],
                "Warning".to_string(),
            ),
            Conflict::info(
                CompatibilityDimension::VersionRange,
                vec![pack1, pack2],
                "Info".to_string(),
            ),
        ];

        let report = CompatibilityReport::failure(conflicts, vec![]);
        let counts = report.count_by_severity();

        assert_eq!(counts.get(&ConflictSeverity::Error), Some(&1));
        assert_eq!(counts.get(&ConflictSeverity::Warning), Some(&1));
        assert_eq!(counts.get(&ConflictSeverity::Info), Some(&1));
    }

    #[test]
    fn test_check_receipt_schemas_no_receipts() {
        let checker = CompatibilityChecker::default();
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "mcp".to_string()),
            AtomicPackId::new(AtomicPackClass::ProjectionRust, "rust".to_string()),
        ];

        let conflicts = checker.check_receipt_schemas(&packs).unwrap();
        assert!(conflicts.is_empty());
    }

    #[test]
    fn test_check_receipt_schemas_chained_only() {
        let checker = CompatibilityChecker::default();
        let packs = vec![AtomicPackId::new(
            AtomicPackClass::ReceiptChained,
            "chained-receipt".to_string(),
        )];

        let conflicts = checker.check_receipt_schemas(&packs).unwrap();
        assert!(conflicts.is_empty());
    }

    #[test]
    fn test_check_receipt_schemas_enterprise_only() {
        let checker = CompatibilityChecker::default();
        let packs = vec![AtomicPackId::new(
            AtomicPackClass::ReceiptEnterpriseSigned,
            "enterprise-receipt".to_string(),
        )];

        let conflicts = checker.check_receipt_schemas(&packs).unwrap();
        assert!(conflicts.is_empty());
    }

    #[test]
    fn test_check_receipt_schemas_chained_conflict() {
        let checker = CompatibilityChecker::default();
        let pack1 = AtomicPackId::new(AtomicPackClass::ReceiptChained, "chained-v1".to_string());
        let pack2 = AtomicPackId::new(
            AtomicPackClass::ReceiptEnterpriseSigned,
            "enterprise-signed".to_string(),
        );

        let conflicts = checker
            .check_receipt_schemas(&[pack1.clone(), pack2.clone()])
            .unwrap();

        assert_eq!(conflicts.len(), 1);
        assert_eq!(
            conflicts[0].dimension,
            CompatibilityDimension::ReceiptSchema
        );
        assert!(conflicts[0].is_error());
        assert_eq!(conflicts[0].packs.len(), 2);
        assert!(conflicts[0].packs.contains(&pack1));
        assert!(conflicts[0].packs.contains(&pack2));
        assert!(conflicts[0]
            .description
            .contains("mutually exclusive receipt formats"));
    }

    #[test]
    fn test_check_receipt_schemas_duplicate_chained() {
        let checker = CompatibilityChecker::default();
        let pack1 = AtomicPackId::new(AtomicPackClass::ReceiptChained, "chained-v1".to_string());
        let pack2 = AtomicPackId::new(AtomicPackClass::ReceiptChained, "chained-v2".to_string());

        let conflicts = checker
            .check_receipt_schemas(&[pack1.clone(), pack2.clone()])
            .unwrap();

        assert_eq!(conflicts.len(), 1);
        assert_eq!(
            conflicts[0].dimension,
            CompatibilityDimension::ReceiptSchema
        );
        assert!(conflicts[0].is_warning());
        assert!(conflicts[0].description.contains("duplicate receipt class"));
    }

    #[test]
    fn test_check_receipt_schemas_mixed_compatible() {
        let checker = CompatibilityChecker::default();
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::ReceiptChained, "chained".to_string()),
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "mcp".to_string()),
            AtomicPackId::new(AtomicPackClass::ProjectionRust, "rust".to_string()),
        ];

        let conflicts = checker.check_receipt_schemas(&packs).unwrap();
        assert!(conflicts.is_empty());
    }

    #[test]
    fn test_check_consequences_semver_vs_breaking_change() {
        let packs = vec![
            AtomicPackId::new(
                AtomicPackClass::ConsequenceSemverMigration,
                "migration1".to_string(),
            ),
            AtomicPackId::new(
                AtomicPackClass::ConsequenceBreakingChange,
                "breaking1".to_string(),
            ),
        ];

        let conflicts = CompatibilityChecker::check_consequences(&packs).unwrap();

        // Should find error-level conflict between SemverMigration and BreakingChange
        assert!(!conflicts.is_empty());
        assert_eq!(
            conflicts[0].dimension,
            CompatibilityDimension::ConsequenceMigration
        );
        assert_eq!(conflicts[0].severity, ConflictSeverity::Error);
        assert_eq!(conflicts[0].packs.len(), 2);
    }

    #[test]
    fn test_check_consequences_semver_migration_only() {
        let packs = vec![AtomicPackId::new(
            AtomicPackClass::ConsequenceSemverMigration,
            "migration1".to_string(),
        )];

        let conflicts = CompatibilityChecker::check_consequences(&packs).unwrap();

        // Should have no conflicts with only SemverMigration
        assert!(conflicts.is_empty());
    }

    #[test]
    fn test_check_consequences_breaking_change_only() {
        let packs = vec![AtomicPackId::new(
            AtomicPackClass::ConsequenceBreakingChange,
            "breaking1".to_string(),
        )];

        let conflicts = CompatibilityChecker::check_consequences(&packs).unwrap();

        // Should have no conflicts with only BreakingChange
        assert!(conflicts.is_empty());
    }

    #[test]
    fn test_check_consequences_duplicate_consequence_packs() {
        let packs = vec![
            AtomicPackId::new(
                AtomicPackClass::ConsequenceSemverMigration,
                "migration1".to_string(),
            ),
            AtomicPackId::new(
                AtomicPackClass::ConsequenceSemverMigration,
                "migration2".to_string(),
            ),
        ];

        let conflicts = CompatibilityChecker::check_consequences(&packs).unwrap();

        // Should find warning for duplicate SemverMigration packs
        assert!(!conflicts.is_empty());
        assert_eq!(
            conflicts[0].dimension,
            CompatibilityDimension::ConsequenceMigration
        );
        assert_eq!(conflicts[0].severity, ConflictSeverity::Warning);
    }

    #[test]
    fn test_check_consequences_no_consequence_packs() {
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::CoreOntology, "core".to_string()),
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "surface".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_consequences(&packs).unwrap();

        // Should have no conflicts when no consequence packs are present
        assert!(conflicts.is_empty());
    }

    // ===== Capability Identity Conflict Tests =====

    #[test]
    fn test_check_capabilities_no_conflicts() {
        use crate::atomic::AtomicPackClass;

        // Different categories = no conflict
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "server".to_string()),
            AtomicPackId::new(AtomicPackClass::ProjectionRust, "rust-proj".to_string()),
            AtomicPackId::new(AtomicPackClass::RuntimeStdio, "stdio-rt".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_capabilities(&packs).unwrap();
        assert!(
            conflicts.is_empty(),
            "Should have no conflicts for different categories"
        );
    }

    #[test]
    fn test_check_capabilities_duplicate_class_error() {
        use crate::atomic::AtomicPackClass;

        // Same class twice = ERROR
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "server1".to_string()),
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "server2".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_capabilities(&packs).unwrap();
        assert_eq!(conflicts.len(), 1, "Should detect duplicate class");
        assert_eq!(conflicts[0].severity, ConflictSeverity::Error);
        assert_eq!(
            conflicts[0].dimension,
            CompatibilityDimension::CapabilityIdentity
        );
        assert!(conflicts[0].description.contains("duplicate class"));
        assert!(conflicts[0].description.contains("SurfaceMcp"));
    }

    #[test]
    fn test_check_capabilities_multiple_surface_warning() {
        use crate::atomic::AtomicPackClass;

        // Multiple surface packs = WARNING
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "mcp-surface".to_string()),
            AtomicPackId::new(AtomicPackClass::SurfaceA2a, "a2a-surface".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_capabilities(&packs).unwrap();
        assert_eq!(conflicts.len(), 1, "Should detect multiple surface packs");
        assert_eq!(conflicts[0].severity, ConflictSeverity::Warning);
        assert!(conflicts[0].description.contains("multiple surface packs"));
        assert!(conflicts[0].description.contains("MCP"));
        assert!(conflicts[0].description.contains("A2A"));
    }

    #[test]
    fn test_check_capabilities_multiple_contract_error() {
        use crate::atomic::AtomicPackClass;

        // Multiple contract packs = ERROR
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::ContractOpenapi, "openapi".to_string()),
            AtomicPackId::new(AtomicPackClass::ContractGraphql, "graphql".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_capabilities(&packs).unwrap();
        assert_eq!(conflicts.len(), 1, "Should detect multiple contract packs");
        assert_eq!(conflicts[0].severity, ConflictSeverity::Error);
        assert!(conflicts[0].description.contains("multiple contract packs"));
        assert!(conflicts[0].description.contains("OpenAPI"));
        assert!(conflicts[0].description.contains("GraphQL"));
    }

    #[test]
    fn test_check_capabilities_multiple_projection_warning() {
        use crate::atomic::AtomicPackClass;

        // Multiple projection packs = WARNING
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::ProjectionRust, "rust".to_string()),
            AtomicPackId::new(AtomicPackClass::ProjectionTypescript, "ts".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_capabilities(&packs).unwrap();
        assert_eq!(
            conflicts.len(),
            1,
            "Should detect multiple projection packs"
        );
        assert_eq!(conflicts[0].severity, ConflictSeverity::Warning);
        assert!(conflicts[0]
            .description
            .contains("multiple projection packs"));
    }

    #[test]
    fn test_check_capabilities_multiple_runtime_error() {
        use crate::atomic::AtomicPackClass;

        // Multiple runtime packs = ERROR
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::RuntimeStdio, "stdio".to_string()),
            AtomicPackId::new(AtomicPackClass::RuntimeAxum, "axum".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_capabilities(&packs).unwrap();
        assert_eq!(conflicts.len(), 1, "Should detect multiple runtime packs");
        assert_eq!(conflicts[0].severity, ConflictSeverity::Error);
        assert!(conflicts[0].description.contains("multiple runtime packs"));
        assert!(conflicts[0].description.contains("stdio"));
        assert!(conflicts[0].description.contains("axum"));
    }

    #[test]
    fn test_check_capabilities_complex_composition() {
        use crate::atomic::AtomicPackClass;

        // Complex valid composition: one from each category
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "mcp".to_string()),
            AtomicPackId::new(AtomicPackClass::ContractOpenapi, "openapi".to_string()),
            AtomicPackId::new(AtomicPackClass::ProjectionRust, "rust".to_string()),
            AtomicPackId::new(AtomicPackClass::RuntimeStdio, "stdio".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_capabilities(&packs).unwrap();
        assert!(
            conflicts.is_empty(),
            "Valid composition should have no conflicts"
        );
    }

    #[test]
    fn test_check_capabilities_non_capability_packs_ignored() {
        use crate::atomic::AtomicPackClass;

        // Core, policy, validator packs should be ignored
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::CoreOntology, "ontology".to_string()),
            AtomicPackId::new(AtomicPackClass::PolicyStrict, "strict".to_string()),
            AtomicPackId::new(AtomicPackClass::ValidatorShacl, "shacl".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_capabilities(&packs).unwrap();
        assert!(
            conflicts.is_empty(),
            "Non-capability packs should be ignored"
        );
    }

    #[test]
    fn test_check_capabilities_conflict_context() {
        use crate::atomic::AtomicPackClass;

        // Verify conflict has proper context metadata
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "mcp1".to_string()),
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "mcp2".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_capabilities(&packs).unwrap();
        assert_eq!(conflicts.len(), 1);
        let conflict = &conflicts[0];

        // Check context fields
        assert!(conflict.context.contains_key("class"));
        assert!(conflict.context.contains_key("count"));
        assert_eq!(conflict.context.get("count").unwrap(), "2");

        // Check resolution strategy
        assert_eq!(conflict.resolution, Some(ConflictResolution::Fail));
    }

    #[test]
    fn test_check_capabilities_all_surface_conflict() {
        use crate::atomic::AtomicPackClass;

        // Test that we have both MCP and A2A surfaces (only 2 exist)
        let packs = vec![
            AtomicPackId::new(AtomicPackClass::SurfaceMcp, "mcp".to_string()),
            AtomicPackId::new(AtomicPackClass::SurfaceA2a, "a2a".to_string()),
        ];

        let conflicts = CompatibilityChecker::check_capabilities(&packs).unwrap();
        assert_eq!(conflicts.len(), 1);
        assert!(conflicts[0].description.contains("SurfaceMcp"));
        assert!(conflicts[0].description.contains("SurfaceA2a"));
    }
}
