# Review Report — Milestone 2 Refactoring Quality & Correctness

## 1. Verdict

**Verdict:** **PASS**

All requirements for Milestone 2 refactoring have been implemented correctly, robustly, and safely. The code compiles cleanly, and the test suite passes with zero failures.

---

## 2. Review Comments

### A. Correctness & Typestate Refactoring
* **Non-Deterministic Serialization (`HashMap` -> `BTreeMap`)**:
  * Replacing `HashMap` with `BTreeMap` in `CompositionReceipt` (`versions`, `ownership_map`), `Conflict` (`context`), `RuntimeConstraint` (`metadata`), `Profile` (`metadata`), `CustomProfileEntry` (`metadata`), and `ProfileConfig` (`profiles`) ensures sorted serialization of keys when generating JSON objects. This resolves the non-deterministic output hash issue caused by random iteration order in standard `HashMap`.
* **Trust Tier Ordering**:
  * Swapping the priority of `Quarantined` (now `6`) and `Experimental` (now `5`) ensures that quarantined packages (lower tier, higher numeric priority) do not satisfy experimental gates.
  * Adding an explicit `if matches!(self, Self::Blocked) { return false; }` guard in `meets_requirement` ensures blocked packages fail verification unconditionally, satisfying the security policy.
* **RDF Registry Classification Roundtrip**:
  * Added `registryClass` property mappings, including serialization of nested enum parameters (`registryUrl`, `registryType`, `requireSignature`, `allowUnlisted`, `primaryUrl`, `mirrorPath`, `syncIntervalSeconds`).
  * Deserialization in `rdf_mapper.rs` handles each enum variant (`Public`, `PrivateEnterprise`, `MirroredAirGapped`) dynamically, ensuring full round-trip preservation of registry configuration.
* **Case-Insensitive SPARQL Injection**:
  * `detect_injection` converts incoming queries to uppercase before pattern matching against `DROP`, `DELETE WHERE {`, `INSERT DATA {`, etc. This prevents bypasses using mixed-case injection payloads (e.g. `drop graph`).
* **Physical README Validation**:
  * The updated `ReadmeValidator::validate` scans candidate paths on disk (e.g., `marketplace/packages/{package_id}`, cache paths, etc.) case-insensitively for a file matching or starting with `readme` (e.g., `README.md`, `README`, `readme.txt`). This correctly validates physical files on disk instead of just checking metadata.

### B. Safety & Security
* **Zip Slip and Path Traversal Prevention**:
  * The extraction logic in `install.rs` (both ZIP and tar.gz) properly checks entry paths for `..` components.
  * It verifies that the destination path starts with the extraction destination target path, preventing files from being written outside the cache directory.
  * Symlinks and hardlinks are explicitly disallowed within the archive during extraction to prevent symlink traversal attacks.
* **Atomic Promotion**:
  * Extraction occurs in a temporary directory inside the cache. Once extraction finishes and validates, the target directory is deleted if it exists, and the temporary directory is atomically renamed. This guarantees that crash-recovery or interruptions do not leave half-extracted packages in the cache.

### C. Compatibility
* Roundtrip serialization was verified using specific test cases (`test_rdf_mapping_registry_class_roundtrip`), demonstrating complete schema conformance and compatibility.

---

## 3. Build/Test Verification Results

Verification was performed using `cargo test --package ggen-marketplace --target-dir /tmp/ggen_reviewer_1_target`. An isolated target directory was used to avoid concurrency locks with other compiler processes running on the host.

### Test Summary
All **276** tests passed successfully:
* `ggen-marketplace` Unit Tests: **261 passed**
* `m2_challenger_stress_tests.rs`: **5 passed**
* `m2_challenger_tests.rs`: **5 passed**
* `milestone2_challenger_tests.rs`: **4 passed**
* Doc Tests: **1 passed**

### Test Execution Output
```
test result: ok. 261 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.07s

     Running tests/m2_challenger_stress_tests.rs (/tmp/ggen_reviewer_1_target/debug/deps/m2_challenger_stress_tests-999fde9e80d95fe0)

running 5 tests
test test_stress_trust_tier_comparisons ... ok
test test_stress_sparql_injection_detection ... ok
test test_stress_readme_validator_scenarios ... ok
test test_stress_receipt_serialization_determinism ... ok
test test_stress_rdf_registry_class_roundtrip ... ok

test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.17s

     Running tests/m2_challenger_tests.rs (/tmp/ggen_reviewer_1_target/debug/deps/m2_challenger_tests-83df4e8993067b82)

running 5 tests
test test_challenger_trust_tier_comparisons ... ok
test test_challenger_receipt_serialization_determinism ... ok
test test_challenger_readme_validator_physical_presence ... ok
test test_challenger_sparql_injection_case_insensitivity ... ok
test test_challenger_rdf_registry_class_roundtrip ... ok

test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.02s

     Running tests/milestone2_challenger_tests.rs (/tmp/ggen_reviewer_1_target/debug/deps/milestone2_challenger_tests-8066814a54d3e45f)

running 4 tests
test test_challenger_trust_tier_priorities ... ok
test test_challenger_receipt_determinism ... ok
test test_challenger_readme_validator ... ok
test test_challenger_rdf_dynamic_mapping ... ok

test result: ok. 4 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.01s

   Doc-tests ggen_marketplace

running 1 test
test crates/ggen-marketplace/src/marketplace/metadata.rs - marketplace::metadata::load_pack_metadata (line 120) ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 33.90s
```
