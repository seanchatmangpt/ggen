# Milestone 2 Analysis Report: Code Quality & Typestates Refactoring

## Executive Summary
This report presents the findings and proposed architectural improvements for Milestone 2. Through static analysis of `crates/ggen-marketplace`, we have identified five code quality and typestate deficiencies—ranging from non-deterministic JSON serialization hashes to incorrect trust-tier comparisons and naive case-sensitive injection checks—and provided concrete, mock-free remediation strategies.

---

## 1. Non-Deterministic Hashmaps in Serializable Objects
### Observations
* **File**: `crates/ggen-marketplace/src/marketplace/composition_receipt.rs`
  * Line 179: `pub versions: HashMap<String, String>,`
  * Line 203: `pub ownership_map: HashMap<String, OwnershipRecord>,`
* **File**: `crates/ggen-marketplace/src/marketplace/compatibility.rs`
  * Line 159: `pub context: HashMap<String, String>,` inside serializable `Conflict` struct.
* **File**: `crates/ggen-marketplace/src/marketplace/policy.rs`
  * Line 171: `pub metadata: HashMap<String, String>,` inside serializable `PolicyRule` struct.
  * Line 334: `pub context: HashMap<String, String>,` inside serializable `EnforcementResult` / `PolicyViolation` struct.
* **File**: `crates/ggen-marketplace/src/marketplace/profile.rs`
  * Line 62: `pub metadata: HashMap<String, String>,` in `RuntimeConstraint`
  * Line 109: `pub metadata: HashMap<String, String>,` in `Profile`
  * Line 356: `pub metadata: HashMap<String, String>,` in `CustomProfileEntry`
  * Line 402: `pub profiles: HashMap<String, CustomProfileEntry>,` in `ProfileConfig`

### Analysis
Rust's `std::collections::HashMap` does not guarantee any deterministic iteration order across executions because it uses a randomized hashing function (SipHash 1-3) to prevent hash collision DoS attacks. When serializing structures containing `HashMap`s to JSON to compute cryptographic signatures or receipts (e.g., in `compute_receipt_id`), the keys may be serialized in different orders. This results in different JSON strings and, consequently, non-deterministic SHA-256 hashes, causing verification failures for logically identical receipts.

### Proposed Fix Strategy
Replace `HashMap` with `BTreeMap` from `std::collections` in all serializable structures. `BTreeMap` has a deterministic iteration order based on key ordering, ensuring stable JSON representation.

#### Before vs. After (Snippet: `composition_receipt.rs`)
```rust
// Before
use std::collections::{HashMap, HashSet};

pub struct CompositionReceipt {
    pub versions: HashMap<String, String>,
    pub ownership_map: HashMap<String, OwnershipRecord>,
}

// After
use std::collections::{BTreeMap, HashSet};

pub struct CompositionReceipt {
    pub versions: BTreeMap<String, String>,
    pub ownership_map: BTreeMap<String, OwnershipRecord>,
}
```

---

## 2. Flawed Trust Tier Priority and Requirement Satisfiability
### Observations
* **File**: `crates/ggen-marketplace/src/marketplace/trust.rs`
  * Lines 53-65: `priority()` maps `Quarantined` to `5`, `Experimental` to `6`, and `Blocked` to `7`.
  * Lines 67-71: `meets_requirement` checks `self.priority() <= required.priority()`.

### Analysis
1. Under the current `priority()` mapping, `Quarantined` (5) has a lower numerical value (higher priority/trust) than `Experimental` (6). Therefore, a `Quarantined` package meets the `Experimental` minimum requirement because `5 <= 6` evaluates to `true`. This allows compromised/suspended packages to bypass development validation gates.
2. If the minimum requirement is set to `Blocked` (7), a `Blocked` package (7) satisfies it since `7 <= 7` is `true`. However, a blocked package should never satisfy any requirements.

### Proposed Fix Strategy
1. Re-order the priority enum mapping so that `Experimental` (5) is more trusted/higher priority than `Quarantined` (6), placing `Quarantined` and `Blocked` at the bottom.
2. Add an explicit check in `meets_requirement` returning `false` immediately if `self` is `TrustTier::Blocked`.

#### Before vs. After (Snippet: `trust.rs`)
```rust
// Before
pub const fn priority(self) -> u8 {
    match self {
        Self::EnterpriseCertified => 1,
        Self::EnterpriseApproved => 2,
        Self::CommunityReviewed => 3,
        Self::ProductionReady => 4,
        Self::Quarantined => 5,
        Self::Experimental => 6,
        Self::Blocked => 7,
    }
}

pub fn meets_requirement(self, required: Self) -> bool {
    self.priority() <= required.priority()
}

// After
pub const fn priority(self) -> u8 {
    match self {
        Self::EnterpriseCertified => 1,
        Self::EnterpriseApproved => 2,
        Self::CommunityReviewed => 3,
        Self::ProductionReady => 4,
        Self::Experimental => 5,
        Self::Quarantined => 6,
        Self::Blocked => 7,
    }
}

pub fn meets_requirement(self, required: Self) -> bool {
    if matches!(self, Self::Blocked) {
        return false;
    }
    self.priority() <= required.priority()
}
```

---

## 3. Hardcoded Registry Classification in RDF Mapper
### Observations
* **File**: `crates/ggen-marketplace/src/marketplace/rdf_mapper.rs`
  * Line 633: Reconstructing `ReleaseInfo` uses `registry_class: default_registry_class()`.
  * `default_registry_class()` hardcodes to `RegistryClass::Public`.
  * `package_to_rdf` does not map the `registry_class` to RDF triples.

### Analysis
When round-tripping packages through the RDF store, their true transport classifications (such as `PrivateEnterprise` or `MirroredAirGapped`) are lost and replaced with `Public`. This triggers policy failures under strict enterprise profiles (e.g. `regulated-finance-profile`) that reject public packages.

### Proposed Fix Strategy
1. Add a `registry_class` helper in `Properties` under `ontology.rs` mapping to the property URI `registryClass`.
2. Serialize/deserialize the `RegistryClass` enum dynamically to/from JSON in the RDF mapper, storing it as a simple datatype literal. This preserves all variants and fields without complicating the RDF schema.

#### Proposed Snippets
* **In `ontology.rs`**:
```rust
pub fn registry_class() -> String {
    Self::uri("registryClass")
}
```

* **In `rdf_mapper.rs` (Serialization)**:
```rust
// Within package_to_rdf release loop:
let reg_class_json = serde_json::to_string(&release.registry_class)
    .map_err(|e| Error::RegistryError(format!("Registry class serialization failed: {}", e)))?;
self.insert_literal_triple(
    &version_uri,
    &Properties::registry_class(),
    &reg_class_json,
)?;
```

* **In `rdf_mapper.rs` (Deserialization)**:
```rust
// Query construction inside query_release_info:
SELECT ?releasedAt ?changelog ?checksum ?downloadUrl ?signature ?trustTier ?registryClass WHERE {
    ...
    OPTIONAL { <{}> <{}> ?registryClass }
}

// Extraction inside query_release_info:
let registry_class = Self::extract_optional_literal(&solution, "registryClass")
    .and_then(|s| serde_json::from_str::<crate::marketplace::trust::RegistryClass>(&s).ok())
    .unwrap_or_else(crate::marketplace::models::default_registry_class);
```

---

## 4. Case-Sensitive SPARQL Injection Check
### Observations
* **File**: `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs`
  * Lines 391-409: `detect_injection` matches uppercase strings `DROP`, `DELETE WHERE {`, etc.

### Analysis
The injection scanner only checks for exact uppercase matches. Suspicious queries using lowercase variations (e.g., `drop` or `delete where {`) easily bypass the scanner, creating a major security vulnerability in the RDF control plane.

### Proposed Fix Strategy
Convert the query to uppercase before checking it against the blocklist patterns.

#### Before vs. After (Snippet: `rdf_control.rs`)
```rust
// Before
fn detect_injection(&self, query: &str) -> bool {
    let suspicious_patterns = [
        "DROP",
        "DELETE WHERE {",
        "INSERT DATA {",
        "CLEAR GRAPH",
        "; DELETE",
    ];
    for pattern in &suspicious_patterns {
        if query.contains(pattern) {
            return true;
        }
    }
    false
}

// After
fn detect_injection(&self, query: &str) -> bool {
    let query_upper = query.to_uppercase();
    let suspicious_patterns = [
        "DROP",
        "DELETE WHERE {",
        "INSERT DATA {",
        "CLEAR GRAPH",
        "; DELETE",
    ];
    for pattern in &suspicious_patterns {
        if query_upper.contains(pattern) {
            return true;
        }
    }
    false
}
```

---

## 5. Metadata-Only README Validation Mock
### Observations
* **File**: `crates/ggen-marketplace/src/marketplace/validation.rs`
  * Lines 423-442: `ReadmeValidator` checks `!package.metadata.description.is_empty()`.

### Analysis
The `ReadmeValidator` does not perform any actual disk I/O to check for the physical presence of a README file. Instead, it accepts a description field check as confirmation that documentation exists. This is an "Oracle Gap" that fails to detect missing package documentation.

### Proposed Fix Strategy
Modify `ReadmeValidator` to verify physical README file existence. Construct paths referencing the expected package development directory (`marketplace/packages/{package_id}`) and the cache path (`~/.cache/ggen/packs/{package_id}/{version}`).

#### Proposed Snippet (`validation.rs`)
```rust
#[async_trait]
impl Validator for ReadmeValidator {
    async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
        let package_id = package.metadata.id.as_str();
        let version = package.latest_version.as_str();
        let cache_dir = crate::marketplace::metadata::get_pack_cache_dir(&package.metadata.id, version);
        
        let paths_to_check = [
            std::path::PathBuf::from(format!("marketplace/packages/{package_id}")),
            std::path::PathBuf::from(format!("../marketplace/packages/{package_id}")),
            std::path::PathBuf::from(format!("../../marketplace/packages/{package_id}")),
            cache_dir,
        ];
        
        let has_readme = paths_to_check.iter().any(|dir| {
            dir.join("README.md").exists() || dir.join("README").exists()
        });

        Ok(ValidationCheck {
            name: "Documentation".to_string(),
            passed: has_readme,
            severity: CheckSeverity::Major,
            message: if has_readme {
                "README file present".to_string()
            } else {
                "Missing README file in package directory".to_string()
            },
            weight: 20,
        })
    }
}
```
