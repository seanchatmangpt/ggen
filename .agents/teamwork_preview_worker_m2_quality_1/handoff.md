# Handoff Report — Milestone 2 Refactoring

## 1. Observation

During our static analysis of the `crates/ggen-marketplace` codebase, we directly observed the following files and structural patterns:

* **Non-Deterministic Serialization**:
  * `crates/ggen-marketplace/src/marketplace/composition_receipt.rs`
    * Line 179: `pub versions: HashMap<String, String>,`
    * Line 203: `pub ownership_map: HashMap<String, OwnershipRecord>,`
  * `crates/ggen-marketplace/src/marketplace/compatibility.rs`
    * Line 159: `pub context: HashMap<String, String>,` inside `Conflict` struct
  * `crates/ggen-marketplace/src/marketplace/profile.rs`
    * Line 62: `pub metadata: HashMap<String, String>,` in `RuntimeConstraint`
    * Line 109: `pub metadata: HashMap<String, String>,` in `Profile`
    * Line 356: `pub metadata: HashMap<String, String>,` in `CustomProfileEntry`
    * Line 402: `pub profiles: HashMap<String, CustomProfileEntry>,` in `ProfileConfig`
  * `crates/ggen-marketplace/src/marketplace/policy.rs`
    * Line 171: `pub metadata: HashMap<String, String>,` in `Policy`
    * Line 334: `pub context: HashMap<String, String>,` in `PolicyViolation`

* **Trust Tier Ordering**:
  * `crates/ggen-marketplace/src/marketplace/trust.rs`
    * Priority mapping: `Quarantined => 5`, `Experimental => 6`, `Blocked => 7`.
    * Requirement checking:
      ```rust
      pub fn meets_requirement(self, required: Self) -> bool {
          self.priority() <= required.priority()
      }
      ```

* **RDF Registry Classification**:
  * `crates/ggen-marketplace/src/marketplace/rdf_mapper.rs`
    * Line 633: `registry_class: crate::marketplace::models::default_registry_class(),` was hardcoded to `default_registry_class()`.
    * No registry class properties were serialized or queried dynamically in `package_to_rdf` or `query_release_info`.

* **Case-Sensitive Injection Check**:
  * `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs`
    * Line 391: `detect_injection` checked `query.contains(pattern)` using uppercase static patterns (`DROP`, `DELETE WHERE {`, etc.) without converting the input query string to a uniform case.

* **Metadata-Only Readme Validator**:
  * `crates/ggen-marketplace/src/marketplace/validation.rs`
    * Line 427: `ReadmeValidator::validate` checked only `!package.metadata.description.is_empty();` and did not perform disk checks for README files.

We executed the `cargo test --package ggen-marketplace` command and observed the following output:
```
test result: ok. 231 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.05s
   Doc-tests ggen_marketplace
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 3.60s
```

## 2. Logic Chain

* **HashMap -> BTreeMap**:
  * Rust `std::collections::HashMap` does not guarantee iteration order due to SipHash randomization. When serializing to JSON to generate cryptographic receipt IDs (via `compute_receipt_id`), the non-deterministic key ordering leads to unstable hashes for identical content. Replacing `HashMap` with `BTreeMap` guarantees sorted key order, producing a stable JSON representation and a deterministic cryptographic hash.

* **Trust Priority & Blocked Packages**:
  * The priority comparison checks `self.priority() <= required.priority()` (where lower numeric priority is more trustworthy). Having `Quarantined` (5) be lower than `Experimental` (6) allows quarantined packages to satisfy experimental gates. Swapping their priorities (`Experimental => 5`, `Quarantined => 6`) ensures quarantined packages do not satisfy experimental requirements. 
  * Explicitly checking `self == Blocked` and returning `false` guarantees that blocked packages never satisfy any requirements, even if the required tier is `Blocked`.

* **Dynamic Registry Classification RDF Mapping**:
  * Adding RDF properties for `registryClass`, `registryUrl`, `registryType`, `requireSignature`, `allowUnlisted`, `primaryUrl`, `mirrorPath`, and `syncIntervalSeconds` in `ontology.rs` and serializing/deserializing them dynamically in `rdf_mapper.rs` ensures that packages keep their true registry class and transport classification when round-tripped through the RDF store, preventing policy violations in strict enterprise profiles.

* **Case-Insensitive Injection Detection**:
  * Query injection strings can contain mixed-case or lowercase sequences (e.g. `drop graph`). Converting the query to uppercase using `.to_uppercase()` prior to comparing with the blocked uppercase patterns ensures all variations of SQL/SPARQL injection keywords are detected.

* **Physical README Validation**:
  * Changing `ReadmeValidator` to construct candidate dev and cache search paths, and scan the directories case-insensitively for any filename matching or beginning with `readme` (such as `README.md`, `README`, `readme.txt`) ensures that physical pack documentation exists on disk rather than merely validating the presence of metadata description.
  * Modifying tests to locally create mock package directories with a `README.md` file and clean them up afterward keeps the tests self-contained, isolated, and safe from parallel environment variable races.

## 3. Caveats

* Unused import warnings and mutable variable warnings in `install.rs` (which belongs to the same crate) were not touched as modifying `install.rs` is strictly outside the scope boundaries of Milestone 2.
* We removed the unused `HashMap` imports from the files we modified (`policy.rs` and `profile.rs`) to ensure zero clippy warnings are introduced in the files modified by this task.

## 4. Conclusion

All 5 refactoring roadmap tasks for Milestone 2 have been fully implemented in `crates/ggen-marketplace`. All unit tests, doc-tests, and new test assertions pass successfully and verify the correct behavior of:
1. Stable `BTreeMap` JSON serialization.
2. Correct trust tier priority enforcement.
3. Bidirectional round-trip RDF mapping of `RegistryClass` variants.
4. Case-insensitive SPARQL injection detection.
5. Physical presence checks for README files on disk.

## 5. Verification Method

To verify the changes, execute the following command:
```bash
cargo test --package ggen-marketplace
```

Verify that all 231 tests pass successfully. 

Inspect the following files to verify the implemented changes:
* `crates/ggen-marketplace/src/marketplace/composition_receipt.rs`
* `crates/ggen-marketplace/src/marketplace/compatibility.rs`
* `crates/ggen-marketplace/src/marketplace/profile.rs`
* `crates/ggen-marketplace/src/marketplace/policy.rs`
* `crates/ggen-marketplace/src/marketplace/trust.rs`
* `crates/ggen-marketplace/src/marketplace/rdf_mapper.rs`
* `crates/ggen-marketplace/src/marketplace/ontology.rs`
* `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs`
* `crates/ggen-marketplace/src/marketplace/validation.rs`
