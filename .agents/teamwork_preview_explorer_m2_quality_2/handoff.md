# Handoff Report — explorer_5

## 1. Observation
- **Deterministic Hashing**: `crates/ggen-marketplace/src/marketplace/composition_receipt.rs` contains struct `CompositionReceipt` with non-deterministic serialization fields (Lines 179 & 203):
  ```rust
  pub versions: HashMap<String, String>,
  pub ownership_map: HashMap<String, OwnershipRecord>,
  ```
  And other files/structs contain non-deterministic `HashMap` fields that are serialized/deserialized:
  - `crates/ggen-marketplace/src/marketplace/compatibility.rs` (Line 159: `Conflict::context`)
  - `crates/ggen-marketplace/src/marketplace/profile.rs` (Lines 62, 109, 356, 402)
  - `crates/ggen-marketplace/src/marketplace/policy.rs` (Lines 171, 334)
- **Trust Ordering**: `crates/ggen-marketplace/src/marketplace/trust.rs` has priority logic (Lines 53–71):
  ```rust
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
  ```
- **RDF Classification Mapping**: `crates/ggen-marketplace/src/marketplace/rdf_mapper.rs` hardcodes the registry class in `query_release_info` (Line 633):
  ```rust
  registry_class: crate::marketplace::models::default_registry_class(),
  ```
- **SPARQL Injection**: `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs` performs a case-sensitive check on SPARQL queries (Lines 391–409):
  ```rust
  for pattern in &suspicious_patterns {
      if query.contains(pattern) {
  ```
- **README Validation**: `crates/ggen-marketplace/src/marketplace/validation.rs` performs a metadata check for documentation presence (Lines 428–429):
  ```rust
  // In a real implementation, this would check for actual README files
  let has_documentation = !package.metadata.description.is_empty();
  ```

## 2. Logic Chain
- **Deterministic Hashing**: 
  - Iteration of `HashMap` is randomized in Rust.
  - Serialization order of keys in JSON follows the iteration order.
  - Thus, serializing `HashMap` values produces different JSON strings, leading to different SHA-256 digests in `compute_receipt_id()`.
  - Replacing all serialized `HashMap` fields with `BTreeMap` guarantees deterministic alphabetical sorting of keys, fixing the issue.
- **Trust Ordering**:
  - `meets_requirement` checks `self.priority() <= required.priority()`.
  - If `required` is `Blocked` (priority 7), any package with priority <= 7 (which includes `Blocked` packages) meets the requirement.
  - Swapping priority values for `Experimental` and `Quarantined` ensures `Quarantined` packages have lower trust/priority value than `Experimental`.
  - Adding `if matches!(self, Self::Blocked) { return false; }` prevents blocked packages from satisfying any requirement.
- **RDF Classification Mapping**:
  - `default_registry_class()` hardcodes to `RegistryClass::Public`.
  - By writing the full properties of `RegistryClass` (including variant and variant-specific values) to the RDF store in `package_to_rdf`, and querying/parsing them dynamically in `query_release_info`, we preserve the true classification.
- **SPARQL Injection**:
  - `query.contains(pattern)` does not match lowercase keywords (e.g. `drop` instead of `DROP`).
  - Converting query to uppercase first ensures all matches are case-insensitive.
- **README Validation**:
  - Description is a metadata field. Its presence does not mean a README file exists.
  - Resolving the package directory (in dev packages or cache) and checking for files named `README.md`, `README`, etc., ensures physical file validation.

## 3. Caveats
- Checked and identified serializable structures using `HashMap` in `profile.rs` and `policy.rs`. These must also be replaced to prevent other deterministic hashing issues.
- The `test_validation` unit test uses a dummy `Package` with no physical directory. When checking for actual file existence, the test package might fail the check unless a mock directory exists. The test assertions only verify quality score > 0, so it will continue to pass even if README check fails.

## 4. Conclusion
The five Milestone 2 tasks are highly actionable and can be resolved through clean Rust code modifications. The recommended strategies will be passed to the implementer subagent.

## 5. Verification Method
- Execute `cargo check --all-targets` to confirm compilation.
  - *Observation*: Running `cargo check --all-targets` in the workspace baseline failed with exit code 101 due to a compilation issue in `benches/a2a_bench.rs` (unresolved import `ggen_core::a2a`). This is outside the `ggen-marketplace` crate but affects full workspace compilation checking.
- Execute `cargo test --all-targets` to run all project tests.
