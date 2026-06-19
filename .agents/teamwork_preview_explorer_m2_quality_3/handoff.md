# Handoff Report — explorer_6 (Milestone 2 Analysis)

## 1. Observation
- **Observation 1 (Non-deterministic hashing)**: In `crates/ggen-marketplace/src/marketplace/composition_receipt.rs`, the `CompositionReceipt` struct contains:
  ```rust
  179:     pub versions: HashMap<String, String>,
  ...
  203:     pub ownership_map: HashMap<String, OwnershipRecord>,
  ```
  In `crates/ggen-marketplace/src/marketplace/compatibility.rs`, the `Conflict` struct contains:
  ```rust
  159:     pub context: HashMap<String, String>,
  ```
- **Observation 2 (Trust tier priority bypass)**: In `crates/ggen-marketplace/src/marketplace/trust.rs`:
  ```rust
  55:     pub const fn priority(self) -> u8 {
  56:         match self {
  57:             Self::EnterpriseCertified => 1,
  58:             Self::EnterpriseApproved => 2,
  59:             Self::CommunityReviewed => 3,
  60:             Self::ProductionReady => 4,
  61:             Self::Quarantined => 5,
  62:             Self::Experimental => 6,
  63:             Self::Blocked => 7,
  64:         }
  65:     }
  ...
  69:     pub fn meets_requirement(self, required: Self) -> bool {
  70:         self.priority() <= required.priority()
  71:     }
  ```
- **Observation 3 (Registry class hardcoding)**: In `crates/ggen-marketplace/src/marketplace/rdf_mapper.rs` line 633, reconstruction from RDF hardcodes:
  ```rust
  633:             registry_class: crate::marketplace::models::default_registry_class(),
  ```
- **Observation 4 (Case-sensitive injection bypass)**: In `crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs` lines 401-406:
  ```rust
  401:         for pattern in &suspicious_patterns {
  402:             if query.contains(pattern) {
  403:                 warn!("Suspicious pattern detected in query: {pattern}");
  404:                 return true;
  405:             }
  406:         }
  ```
- **Observation 5 (Lazy README verification)**: In `crates/ggen-marketplace/src/marketplace/validation.rs` lines 428-430:
  ```rust
  428:         // In a real implementation, this would check for actual README files
  429:         let has_documentation = !package.metadata.description.is_empty();
  ```

## 2. Logic Chain
- **Logic Chain 1**: Since `HashMap` has randomized key iteration order, serialization of `versions` and `ownership_map` via `to_json()` is non-deterministic. Because `receipt_id` is computed directly over the JSON string of `CompositionReceipt`, this randomized serialization creates unstable receipt IDs. Changing these to `BTreeMap` guarantees sorted key serialization, producing deterministic JSON strings and digests.
- **Logic Chain 2**: `Quarantined` has priority 5, which is numerically less than `Experimental` (6). Under `self.priority() <= required.priority()`, `Quarantined` packages satisfy `Experimental` requirements, which is a logic error. Similarly, a `Blocked` package (7) satisfies a `Blocked` requirement (7) because `7 <= 7` is true, bypassing safety controls. Swapping the priority values of `Experimental` and `Quarantined` and adding an explicit early return for `Self::Blocked` fixes both vulnerabilities.
- **Logic Chain 3**: The hardcoded `default_registry_class()` inside `query_release_info` yields a `Public` registry class for all reconstructed package releases. Reading and writing dynamic properties (like `registryClass`, `registryUrl`, `requireSignature`, `allowUnlisted`, `mirrorPath`, and `syncIntervalSeconds`) allows reconstructing the exact variant of `RegistryClass` dynamically.
- **Logic Chain 4**: The `.contains()` check is case-sensitive and checks for uppercase strings (e.g. `"DROP"`). A query like `drop graph ...` uses lowercase characters, so `.contains("DROP")` returns `false` and bypasses the security control. Converting the query string to lowercase and comparing it with lowercase suspicious patterns prevents this bypass.
- **Logic Chain 5**: The description length check in `ReadmeValidator` does not verify the physical presence of a `README.md` file in the package's local directory or cache path. Changing this to resolve the directory path and verify file existence checks the actual package files.

## 3. Caveats
- This investigation is purely read-only and does not implement the fixes. 
- It is assumed that the packages in the local workspace directory are stored under `marketplace/packages/<package_id>` and packages in the cache are stored under `~/.cache/ggen/packs/<package_id>/<version>`.

## 4. Conclusion
Milestone 2 has 5 distinct logic bugs and security bypasses. All have been traced to their exact files and lines, and specific fix strategies have been proposed. No code has been modified in accordance with the read-only constraint.

## 5. Verification Method
Verify that the following commands compile and pass without regressions once the implementer applies the changes:
- Run all workspace tests: `cargo test`
- Build all targets: `cargo build --all-targets`
- Specific test verification path: `crates/ggen-marketplace/src/marketplace/trust.rs` tests (to verify the updated trust tier hierarchy) and `crates/ggen-marketplace/src/marketplace/validation.rs` tests (to verify the README existence validator).
