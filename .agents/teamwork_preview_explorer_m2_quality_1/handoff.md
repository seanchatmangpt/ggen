# Handoff Report: explorer_4 - Milestone 2 Code Quality & Typestates Analysis

## 1. Observation
We observed the following exact definitions, structures, and behaviors within the `crates/ggen-marketplace` crate:

1. **HashMap Usages in Serializable Objects**:
   - `crates/ggen-marketplace/src/marketplace/composition_receipt.rs`
     - Line 179: `pub versions: HashMap<String, String>,`
     - Line 203: `pub ownership_map: HashMap<String, OwnershipRecord>,`
   - `crates/ggen-marketplace/src/marketplace/compatibility.rs`
     - Line 159: `pub context: HashMap<String, String>,`
   - `crates/ggen-marketplace/src/marketplace/policy.rs`
     - Line 171: `pub metadata: HashMap<String, String>,`
     - Line 334: `pub context: HashMap<String, String>,`
   - `crates/ggen-marketplace/src/marketplace/profile.rs`
     - Line 62: `pub metadata: HashMap<String, String>,`
     - Line 109: `pub metadata: HashMap<String, String>,`
     - Line 356: `pub metadata: HashMap<String, String>,`
     - Line 402: `pub profiles: HashMap<String, CustomProfileEntry>,`

2. **Trust Tier Comparison Logic**:
   - `crates/ggen-marketplace/src/marketplace/trust.rs`
     - Lines 53-65:
       ```rust
       Self::EnterpriseCertified => 1,
       Self::EnterpriseApproved => 2,
       Self::CommunityReviewed => 3,
       Self::ProductionReady => 4,
       Self::Quarantined => 5,
       Self::Experimental => 6,
       Self::Blocked => 7,
       ```
     - Lines 69-71:
       ```rust
       pub fn meets_requirement(self, required: Self) -> bool {
           self.priority() <= required.priority()
       }
       ```

3. **Hardcoded Registry Classification**:
   - `crates/ggen-marketplace/src/marketplace/rdf_mapper.rs`
     - Line 633:
       ```rust
       registry_class: crate::marketplace::models::default_registry_class(),
       ```
     - The function `default_registry_class()` in `crates/ggen-marketplace/src/marketplace/models.rs` (line 561) returns `RegistryClass::Public`.
     - In `package_to_rdf` (lines 49-284), `registry_class` is not mapped to the RDF store.

4. **Case-Sensitive SPARQL Injection Check**:
   - `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs`
     - Lines 391-409:
       ```rust
       fn detect_injection(&self, query: &str) -> bool {
           // Basic injection detection
           let suspicious_patterns = [
               "DROP",
               "DELETE WHERE {",
               "INSERT DATA {",
               "CLEAR GRAPH",
               "; DELETE",
           ];
           for pattern in &suspicious_patterns {
               if query.contains(pattern) {
                   warn!("Suspicious pattern detected in query: {pattern}");
                   return true;
               }
           }
           false
       }
       ```

5. **Metadata-Only README Validation Check**:
   - `crates/ggen-marketplace/src/marketplace/validation.rs`
     - Lines 427-442:
       ```rust
       async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
           // In a real implementation, this would check for actual README files
           let has_documentation = !package.metadata.description.is_empty();
       ```

---

## 2. Logic Chain
1. **HashMap to BTreeMap**:
   - Observations show multiple `HashMap`s used in structs that derive `Serialize` and `Deserialize` (e.g., `CompositionReceipt`, `Conflict`, `PolicyViolation`, etc.).
   - Since `HashMap` has randomized hash orderings, serialized JSON representation order varies.
   - Consequently, computing a SHA-256 hash over serialized JSON (like in `compute_receipt_id`) yields non-deterministic hashes, causing cryptographic chain verification or signature verification to fail randomly.
   - Replacing `HashMap` with `BTreeMap` guarantees lexicographically ordered serialization keys, resolving the non-determinism.

2. **Trust Tier Comparisons**:
   - Observations show `priority()` orders `Quarantined` (5) and `Experimental` (6).
   - The comparison check `self.priority() <= required.priority()` means a lower numerical priority satisfies the requirement.
   - Since `5 <= 6`, a `Quarantined` package (5) satisfies `Experimental` (6), bypassing validation gates.
   - Additionally, since `7 <= 7`, a `Blocked` package (7) satisfies `Blocked` (7).
   - Swapping the priority mapping of `Experimental` and `Quarantined` and refusing `Blocked` package checks immediately ensures `Blocked` packages never satisfy any requirements and `Quarantined` packages do not bypass `Experimental`.

3. **Registry Class Mapping**:
   - Observations show `rdf_mapper.rs` hardcodes the reconstructed `registry_class` to `Public` via `default_registry_class()`.
   - As a result, when round-tripped through RDF, all enterprise registries are classified as public, causing strict profiles to reject valid private packages.
   - Dynamically serializing `registry_class` to JSON and inserting/retrieving it in the RDF store preserves classification.

4. **Case-Insensitive SPARQL Check**:
   - Observations show that `detect_injection` matches against uppercase string patterns via `.contains()`.
   - Input queries containing lowercase tokens (e.g. `drop`) bypass this check.
   - Normalizing the query via `.to_uppercase()` before scanning ensures case-insensitivity.

5. **Physical README File Existence**:
   - Observations show that `ReadmeValidator` only checks that the manifest description is non-empty.
   - This metadata check allows packages without any actual documentation files to pass.
   - Implementing a real file existence check across candidate local paths (`marketplace/packages/{id}`) and cached folders confirms physical document presence.

---

## 3. Caveats
- No compilation/behavioral analysis was performed on external custom plugins or tools outside the workspace.
- We assumed that `RegistryClass` can be safely serialized to a datatype string JSON literal in the RDF store.

---

## 4. Conclusion
We conclude that the codebase has five major bugs/vulnerabilities under Milestone 2:
1. Non-deterministic receipt IDs due to randomized `HashMap` key orderings.
2. Validation gate bypasses for quarantined and blocked packages due to incorrect priority math.
3. Hardcoded public classification for private registries in the RDF mapping layer.
4. Case-sensitive SPARQL injection check bypasses.
5. Incomplete (fake) README validation check.

Implementing the proposed, mock-free improvements will restore cryptographic determinism, satisfy Forture 5 CISO validation rules, and secure the RDF query plane.

---

## 5. Verification Method
1. **Crate Test Command**:
   Run the marketplace unit tests to verify base behavior:
   ```bash
   cargo test -p ggen-marketplace
   ```
2. **Deterministic Hash Verification**:
   Inspect `composition_receipt.rs` and verify that the `versions` and `ownership_map` fields use `BTreeMap` instead of `HashMap`.
3. **Maturity/Trust Tests**:
   Inspect `trust.rs` and verify that:
   - `TrustTier::Blocked.meets_requirement(TrustTier::Blocked)` returns `false`.
   - `TrustTier::Quarantined.meets_requirement(TrustTier::Experimental)` returns `false`.
4. **SPARQL Injection Check**:
   Inspect `rdf_control.rs` and check that `detect_injection("drop table;")` returns `true`.
5. **README File Verification**:
   Verify that `ReadmeValidator` does not pass on a mock package structure lacking a physical `README.md` or `README` file on disk.
