# Analysis of Milestone 2 (Refactor Code Quality & Typestates)

## Summary of Findings
Milestone 2 aims to resolve quality, correctness, security, and deterministic verification issues across the `ggen-marketplace` codebase. This report identifies specific bugs in five target areas—serialization determinism, trust-tier logic, dynamic RDF mapping, SPARQL injection detection, and README validation—and details concrete, non-destructive recommendation fix strategies for each.

---

## Detailed Analysis & Recommendations

### 1. Replace `HashMap` with `BTreeMap` for Deterministic JSON Serialization
* **Problem**: Serialization of `HashMap` objects is non-deterministic because the iteration order of `HashMap` is randomized/arbitrary. When calculating cryptographic hash digests of serialized JSON structures (such as `CompositionReceipt`), this leads to non-reproducible receipt IDs.
* **Target Files & Structs**:
  1. `crates/ggen-marketplace/src/marketplace/composition_receipt.rs`:
     - Struct `CompositionReceipt` (Lines 161–213)
       - `versions: HashMap<String, String>` (Line 179)
       - `ownership_map: HashMap<String, OwnershipRecord>` (Line 203)
  2. `crates/ggen-marketplace/src/marketplace/compatibility.rs`:
     - Struct `Conflict` (Lines 140–160)
       - `context: HashMap<String, String>` (Line 159)
  3. `crates/ggen-marketplace/src/marketplace/profile.rs`:
     - Struct `CustomProfile` (Lines 58–63)
       - `metadata: HashMap<String, String>` (Line 62)
     - Struct `CustomProfileEntry` (Lines 105–110)
       - `metadata: HashMap<String, String>` (Line 109)
     - Struct `CustomProfileConfig` (Lines 352–403)
       - `metadata: HashMap<String, String>` (Line 356)
       - `profiles: HashMap<String, CustomProfileEntry>` (Line 402)
  4. `crates/ggen-marketplace/src/marketplace/policy.rs`:
     - Struct `Policy` (Lines 167–172)
       - `metadata: HashMap<String, String>` (Line 171)
     - Struct `PolicyEvaluation` (Lines 330–335)
       - `context: HashMap<String, String>` (Line 334)
* **Concrete Recommendation**:
  - Replace `std::collections::HashMap` usage with `std::collections::BTreeMap` in the fields of the aforementioned structs.
  - Update imports at the top of each file (e.g., `use std::collections::BTreeMap;`).
  - Replace constructor calls (e.g., `HashMap::new()` to `BTreeMap::new()`).
  - Update Serde annotations `#[serde(skip_serializing_if = "HashMap::is_empty")]` to `#[serde(skip_serializing_if = "BTreeMap::is_empty")]`.

---

### 2. Fix Comparison Ordering Logic in Trust Tiers
* **Problem**: 
  - `Blocked` packages should never satisfy any requirements. However, in the current priority implementation (`Blocked` has priority 7), if the minimum required tier is `Blocked`, a package with the `Blocked` tier satisfies it since `7 <= 7` is evaluated as `true`.
  - `Quarantined` packages under restriction are currently treated as less restrictive than `Experimental` packages because `Quarantined` has priority 5 and `Experimental` has priority 6, meaning `Quarantined` meets `Experimental` requirements (`5 <= 6` is `true`).
* **Target File & Lines**:
  - `crates/ggen-marketplace/src/marketplace/trust.rs` (Lines 53–71)
* **Concrete Recommendation**:
  - Modify `priority(self)` (Line 55) to swap `Experimental` and `Quarantined` priorities so that `Quarantined` has a higher priority number (making it more restrictive / lower trust than `Experimental`):
    ```rust
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
    ```
  - Modify `meets_requirement` to explicitly reject `Blocked` packages immediately:
    ```rust
    pub fn meets_requirement(self, required: Self) -> bool {
        if matches!(self, Self::Blocked) {
            return false;
        }
        self.priority() <= required.priority()
    }
    ```

---

### 3. Dynamic Registry Classification RDF Mapping
* **Problem**: The RDF Mapper hardcodes the registry classification property to `RegistryClass::Public` when reconstructing a package release from RDF triples in `query_release_info`, failing to preserve the true registry class (`Public`, `PrivateEnterprise`, or `MirroredAirGapped`).
* **Target File & Lines**:
  - `crates/ggen-marketplace/src/marketplace/rdf_mapper.rs` (Line 633)
* **Concrete Recommendation**:
  - In `package_to_rdf`, dynamically serialize all fields of the `RegistryClass` property for each release:
    - Add a `registryClass` node linked from the version node.
    - Write the variant name (`classType` as `"public"`, `"private_enterprise"`, or `"mirrored_air_gapped"`).
    - Write properties specific to each variant: `registryUrl` / `registryType` for `Public`; `registryUrl` / `requireSignature` / `allowUnlisted` for `PrivateEnterprise`; `primaryUrl` / `mirrorPath` / `syncIntervalSeconds` for `MirroredAirGapped`.
  - In `query_release_info`, query these properties using a SPARQL query on the `registryClass` node, parse the properties from the results, and reconstruct the correct `RegistryClass` enum instead of using `default_registry_class()`.

---

### 4. Case-Insensitive SPARQL Query Injection Check
* **Problem**: The SPARQL query injection detection pattern matching is case-sensitive, making it easy to bypass (e.g., using `drop graph` instead of `DROP GRAPH`).
* **Target File & Lines**:
  - `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs` (Lines 391–409)
* **Concrete Recommendation**:
  - Convert the query string to uppercase before matching it against the suspicious uppercase patterns:
    ```rust
    fn detect_injection(&self, query: &str) -> bool {
        let suspicious_patterns = [
            "DROP",
            "DELETE WHERE {",
            "INSERT DATA {",
            "CLEAR GRAPH",
            "; DELETE",
        ];

        let query_upper = query.to_uppercase();
        for pattern in &suspicious_patterns {
            if query_upper.contains(pattern) {
                warn!("Suspicious pattern detected in query: {pattern}");
                return true;
            }
        }
        false
    }
    ```

---

### 5. Check Actual README File Existence in ReadmeValidator
* **Problem**: `ReadmeValidator` performs a metadata-only check (`!package.metadata.description.is_empty()`) instead of verifying if a physical README file exists in the package directory.
* **Target File & Lines**:
  - `crates/ggen-marketplace/src/marketplace/validation.rs` (Lines 423–452)
* **Concrete Recommendation**:
  - Update `ReadmeValidator`'s `validate` method to search for file existence:
    - Attempt to resolve the package directory using:
      1. Dev path: `marketplace/packages/{package_id}`
      2. Cache path: `{cache_dir}/{package_id}/{version}` or `{cache_dir}/{package_id}` (reading `GGEN_PACK_CACHE_DIR` env or falling back to home dir `.cache/ggen/packs`).
    - Verify if any common README variant exists as a file (e.g., `README.md`, `README`, `readme.md`, `readme.txt`, `readme` case-insensitively).
    - Example helper:
      ```rust
      fn check_readme_in_dir(dir: &std::path::Path) -> bool {
          if !dir.is_dir() { return false; }
          let names = ["README.md", "README", "readme.md", "readme.txt", "readme"];
          names.iter().any(|name| dir.join(name).is_file())
      }
      ```
    - Mark the check as passed if the file is found.
