## 2026-06-12T03:37:13Z

You are a worker agent. Your task is to implement the Milestone 2 code quality and typestates refactoring roadmap for the ggen marketplace package as analyzed by the explorers.

### Working Directory
/Users/sac/ggen/.agents/teamwork_preview_worker_m2_quality_1

### Objective
Implement the following refactoring tasks:
1. **HashMap -> BTreeMap for Deterministic JSON Serialization**:
   Replace `std::collections::HashMap` with `std::collections::BTreeMap` in all serializable structures to ensure deterministic JSON serialization (e.g. for cryptographic receipt hashing):
   - `crates/ggen-marketplace/src/marketplace/composition_receipt.rs` (in `CompositionReceipt` and related structures)
   - `crates/ggen-marketplace/src/marketplace/compatibility.rs` (in `Conflict` and related structures)
   - `crates/ggen-marketplace/src/marketplace/profile.rs` (in `RuntimeConstraint`, `Profile`, `CustomProfileEntry`, `ProfileConfig` or similar configurations)
   - `crates/ggen-marketplace/src/marketplace/policy.rs` (in `Policy`, `PolicyRule`, `PolicyEvaluation`, `EnforcementResult`, `PolicyViolation` or similar structs)
   Update all imports, constructors, and Serde attributes accordingly.

2. **Fix Comparison Ordering Logic in Trust Tiers**:
   - Update `crates/ggen-marketplace/src/marketplace/trust.rs`:
     - Swap priority values of `Experimental` and `Quarantined` in `priority()` so `Experimental` has a lower value (higher trust) than `Quarantined`.
     - In `meets_requirement()`, ensure `Blocked` packages never satisfy any requirements (return false immediately if the package's tier is `Blocked`).

3. **Dynamic Registry Classification RDF Mapping**:
   - Update the RDF Mapper in `crates/ggen-marketplace/src/marketplace/rdf_mapper.rs` and property URI mappings in `crates/ggen-marketplace/src/marketplace/ontology.rs` (if necessary).
   - In `package_to_rdf`, serialize the actual `RegistryClass` enum variants to RDF triples (public, private enterprise, mirrored air-gapped) and serialize their fields (such as `registry_url`, `require_signature`, `allow_unlisted`, `primary_url`, `mirror_path`, `sync_interval_seconds`) dynamically rather than hardcoding.
   - In `query_release_info`, query these properties using a SPARQL query, extract them from the solution, and reconstruct the correct `RegistryClass` enum variant instead of using `default_registry_class()`.

4. **Case-Insensitive SPARQL Injection Check**:
   - Update `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs`:
     - In `detect_injection`, convert the query to uppercase (or lowercase) before comparing with the suspicious patterns list to ensure mixed-case or lowercase injection attempts are detected.

5. **README File Check in ReadmeValidator**:
   - Update `crates/ggen-marketplace/src/marketplace/validation.rs`:
     - Modify `ReadmeValidator::validate` to verify the physical presence of a README file (e.g. `README.md`, `README`, case-insensitively) in the package's directory or the cache directory, rather than checking if description is non-empty.
     - Update the unit tests (e.g., `test_validation`) to dynamically create mock README files before validating and clean them up afterward.

### Scope Boundaries
Only implement the files listed above for Milestone 2. Do not touch unrelated files or milestones.

### Input Information
- Explorer reports:
  - `/Users/sac/ggen/.agents/teamwork_preview_explorer_m2_quality_1/analysis.md`
  - `/Users/sac/ggen/.agents/teamwork_preview_explorer_m2_quality_2/analysis.md`
  - `/Users/sac/ggen/.agents/teamwork_preview_explorer_m2_quality_3/analysis.md`
- Codebase root: `/Users/sac/ggen`

### Output Requirements
Write a detailed handoff report to `/Users/sac/ggen/.agents/teamwork_preview_worker_m2_quality_1/handoff.md` summarizing:
- What changes were made (files, structs, functions).
- The commands run to build and test the changes.
- The results of those builds and tests (must build and pass cleanly).

### Verification
You must verify your changes:
1. Run the build command (`cargo build --all-targets`) and the test command (`cargo test --all-targets`) for affected targets.
2. Verify that your tests cover the new cases (e.g. BTreeMap serialization determinism, trust priority checks, case-insensitive injection detection, actual file check for README, dynamic RDF registry class mapping).
3. Document build/test commands and results in your handoff report.

### MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
