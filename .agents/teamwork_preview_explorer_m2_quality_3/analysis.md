# structured analysis and Recommendation Report — Milestone 2

This report details the architectural and implementation analysis for Milestone 2 (Refactor Code Quality & Typestates) of the `ggen` marketplace refactoring project. Each identified issue is analyzed with its corresponding file paths, lines, root causes, and recommended fix strategies.

---

## 1. Deterministic JSON Hashing via BTreeMap

### Root Cause Analysis
The composition receipt system computes `receipt_id` by hashing the JSON representation of the `CompositionReceipt` struct (computed in `compute_receipt_id()`). Currently, `CompositionReceipt` uses `std::collections::HashMap` for `versions` and `ownership_map`.
Because `HashMap` in Rust does not guarantee iteration order (using randomized state/SipHash keys by default to resist DoS attacks), the serialized JSON keys appear in a non-deterministic order. Consequently, two structurally identical receipts can serialize to different JSON strings and produce different, invalid `receipt_id` digests.

### Impacted Files & Lines
- **`crates/ggen-marketplace/src/marketplace/composition_receipt.rs`**
  - Line 16: `use std::collections::{HashMap, HashSet};`
  - Line 179: `pub versions: HashMap<String, String>,`
  - Line 203: `pub ownership_map: HashMap<String, OwnershipRecord>,`
  - Line 268: `versions: HashMap::new(),`
  - Line 276: `ownership_map: HashMap::new(),`
- **`crates/ggen-marketplace/src/marketplace/compatibility.rs`**
  - Line 7: `use std::collections::HashMap;`
  - Line 159: `pub context: HashMap<String, String>,` (under serializable `Conflict` struct)
  - Line 175: `context: HashMap::new(),`

### Recommended Fix Strategy
Replace `HashMap` with `BTreeMap` (which maintains keys in sorted order, ensuring deterministic serialization):
1. **`composition_receipt.rs`**:
   - Replace `HashMap` with `BTreeMap` in imports and struct fields.
   - Update `CompositionReceipt::new` to initialize `BTreeMap::new()`.
2. **`compatibility.rs`**:
   - Replace `HashMap` with `BTreeMap` in imports, `Conflict` struct fields, and its `new()` constructor.

---

## 2. Trust Tier Comparison Ordering logic

### Root Cause Analysis
- **`Quarantined` Bypassing `Experimental`**:
  In `TrustTier::priority()`, the tiers are mapped to numeric values:
  - `Quarantined => 5`
  - `Experimental => 6`
  - `Blocked => 7`
  
  The comparison logic is `self.priority() <= required.priority()`, where lower numbers mean "meets or exceeds". Therefore, a `Quarantined` package (5) satisfies an `Experimental` requirement (6) because `5 <= 6` evaluates to `true`. This allows quarantined packages to bypass the experimental trust gate.
- **`Blocked` Satisfying Requirements**:
  Currently, if a requirement is set to `Blocked` (7), a `Blocked` package (7) satisfies it (`7 <= 7` is `true`). Under CISO security policies, `Blocked` packages should never satisfy any requirements.

### Impacted Files & Lines
- **`crates/ggen-marketplace/src/marketplace/trust.rs`**
  - Lines 55-65: `priority` function priority mappings.
  - Lines 69-71: `meets_requirement` implementation.

### Recommended Fix Strategy
1. Swap the priorities of `Experimental` and `Quarantined` so that `Experimental` is more trusted (has a lower priority number) than `Quarantined`:
   ```rust
   Self::Experimental => 5,
   Self::Quarantined => 6,
   Self::Blocked => 7,
   ```
2. Modify `meets_requirement` to explicitly fail if `self` is `TrustTier::Blocked`:
   ```rust
   pub fn meets_requirement(self, required: Self) -> bool {
       if matches!(self, Self::Blocked) {
           return false;
       }
       self.priority() <= required.priority()
   }
   ```

---

## 3. Dynamic Registry Class Mapping in RDF

### Root Cause Analysis
During RDF-to-Package reconstruction in `rdf_mapper.rs` (`query_release_info` function), the `registry_class` field in the constructed `ReleaseInfo` is hardcoded to `default_registry_class()` (which returns a `Public` registry class). Moreover, `package_to_rdf` does not write the `RegistryClass` enum variants to RDF triples, preventing dynamic classification of registries (public vs private enterprise vs mirrored air-gapped).

### Impacted Files & Lines
- **`crates/ggen-marketplace/src/marketplace/rdf_mapper.rs`**
  - Line 633: `registry_class: crate::marketplace::models::default_registry_class(),`
  - Lines 201-278: Missing serialization logic for `registry_class` inside `package_to_rdf`.

### Recommended Fix Strategy
1. **Serialization (`package_to_rdf`)**:
   Under the release loop, write the registry class variants to the store:
   ```rust
   let (reg_class_type, reg_url) = match &release.registry_class {
       RegistryClass::Public { url, .. } => ("public", url),
       RegistryClass::PrivateEnterprise { url, .. } => ("private_enterprise", url),
       RegistryClass::MirroredAirGapped { primary_url, .. } => ("mirrored_air_gapped", primary_url),
   };
   self.insert_literal_triple(&version_uri, &format!("{}registryClass", Namespaces::GGEN), reg_class_type)?;
   self.insert_literal_triple(&version_uri, &format!("{}registryUrl", Namespaces::GGEN), reg_url)?;
   
   // Insert variant-specific properties (e.g. requireSignature, allowUnlisted, mirrorPath, syncIntervalSeconds)
   ```
2. **Reconstruction (`query_release_info`)**:
   - Update the SPARQL query to select the registry class properties: `?registryClass`, `?registryUrl`, etc.
   - Extract the variables and map them to their corresponding `RegistryClass` enum variants.

---

## 4. Case-Insensitive SPARQL Query Injection Check

### Root Cause Analysis
In `RdfControlPlane::detect_injection`, the checks use `query.contains(pattern)`. The defined `suspicious_patterns` (e.g., `"DROP"`, `"DELETE WHERE {"`) are all uppercase. Because `.contains()` is case-sensitive, an attacker can bypass the injection protection check completely by submitting lowercase or mixed-case queries (e.g., `drop graph <...>` or `delete where { ... }`).

### Impacted Files & Lines
- **`crates/ggen-marketplace/src/marketplace/rdf/rdf_control.rs`**
  - Lines 391-410: `detect_injection` function.

### Recommended Fix Strategy
Convert the query string to lowercase and match against lowercase patterns:
```rust
fn detect_injection(&self, query: &str) -> bool {
    let query_lower = query.to_lowercase();
    let suspicious_patterns = [
        "drop",
        "delete where {",
        "insert data {",
        "clear graph",
        "; delete",
    ];

    for pattern in &suspicious_patterns {
        if query_lower.contains(pattern) {
            warn!("Suspicious pattern detected in query: {pattern}");
            return true;
        }
    }
    false
}
```

---

## 5. ReadmeValidator File Existence Check

### Root Cause Analysis
The current `ReadmeValidator::validate` does not inspect the disk for documentation files. Instead, it checks if `package.metadata.description` is non-empty, which is a metadata check. This fails to verify if actual README files were included in the package.

### Impacted Files & Lines
- **`crates/ggen-marketplace/src/marketplace/validation.rs`**
  - Lines 423-442: `ReadmeValidator` implementation.
  - Lines 522-537: `test_validation` unit test.

### Recommended Fix Strategy
1. **Directory Resolution & Existence Check**:
   Resolve the package directory path dynamically (checking workspace directories, the current directory, and the environment/default cache directories like `~/.cache/ggen/packs/`) and check if any expected files exist:
   ```rust
   let has_readme = if let Some(dir) = resolved_package_dir {
       dir.join("README.md").exists()
           || dir.join("README").exists()
           || dir.join("readme.md").exists()
           || dir.join("README.txt").exists()
   } else {
       false
   };
   ```
2. **Unit Test Setup**:
   In `test_validation`, create the cache directory path dynamically and write a mock `README.md` file to it prior to calling `validator.validate(&package)`, cleaning it up afterward. This ensures the unit tests compile and run properly.
