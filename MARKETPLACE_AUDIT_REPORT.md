# ggen Pack & Marketplace Audit Report

## Executive Summary
This report presents a synthesized audit of the `ggen-marketplace` Rust core and the `marketplace/` catalog/validation subsystem. The audit identified critical logic bugs, security risks, catalog inconsistencies, and architectural gaps that compromise the security, correctness, and reliability of the marketplace platform.

Key findings include:
1. **Cache Verification Bug**: A fundamental logic mismatch in the cache manager causes digest verification of cached packages to always fail. The system hashes individual uncompressed files in a directory walk and compares it to the digest of the compressed archive (`zip`/`tar.gz`), forcing unnecessary purges and re-downloads of all packs.
2. **Non-Deterministic Receipts**: Cryptographic receipt IDs are non-deterministic across executions because they are derived from JSON serializations of standard `HashMap`s, which iterate in arbitrary, randomized order.
3. **Insecure Archive Extraction (Zip Slip)**: Pack extraction is performed directly into the cache directory without path-traversal validation, creating a Zip Slip vulnerability that permits malicious packages to overwrite files outside the cache directory.
4. **Bypasses and Incomplete Protections**: Naive case-sensitive SPARQL injection checks, hardcoded registry classification mappings, incomplete validation checks (such as the `ReadmeValidator` mock), and disabled test suites with empty `assert!(true)` placeholders undermine enterprise security and compliance.
5. **Registry Indexer Logic Failure**: Dotted key table lookups (e.g., `package.metadata`) in the Python registry indexer treat nested tables as flat keys, resulting in all registry items being incorrectly flagged as `production_ready: false`.

This report provides detailed technical analysis, code snippets, typestate refactoring patterns, and a prioritized roadmap for fixing these issues.

---

## Section 1: Audit of ggen-marketplace Rust Core

This section details the critical correctness and security defects discovered in the core Rust implementations within the `crates/ggen-marketplace` crate.

### 1. Cache Verification Bug
- **Affected Files**: `src/marketplace/cache.rs` and `src/marketplace/install.rs`
- **Description**: When a package is downloaded and installed, the cache entry's digest is calculated over the raw, compressed package archive bytes (e.g., `zip` or `tar.gz` data). However, when verifying the cache digest later, the cache manager walks the uncompressed directory, hashes each individual file, and compares the resulting cumulative hash against the stored archive hash.
- **Code Snippets**:
  In `src/marketplace/cache.rs` (lines 458-494):
  ```rust
  pub fn verify_digest(&self, pack: &CachedPack) -> Result<bool> {
      use sha2::{Digest, Sha256};

      let mut hasher = Sha256::new();
      let mut verified = true;

      // Walk the pack directory and hash all files
      if pack.cache_path.exists() {
          for entry in walkdir::WalkDir::new(&pack.cache_path)
              .into_iter()
              .filter_map(|e| e.ok())
          {
              if entry.file_type().is_file() {
                  if let Ok(contents) = fs::read(entry.path()) {
                      hasher.update(&contents);
                  } else {
                      verified = false;
                      break;
                  }
              }
          }
      }

      let calculated_digest = hex::encode(hasher.finalize());
      let matches = calculated_digest == pack.digest;
      ...
      Ok(verified && matches)
  }
  ```
  In `src/marketplace/install.rs` (lines 446-456):
  ```rust
  // Calculate final digest (calculates over compressed archive bytes)
  let digest = ChecksumCalculator::calculate(&pack_data);

  // Create cached pack entry
  let cached_pack = CachedPack::new(
      package_id.clone(),
      version.clone(),
      digest,
      pack_data.len() as u64,
      cache_path,
  );
  ```
- **Consequences**: Cache verification fails consistently on every cache hit. This forces the installer to delete the cache directory and re-download packages on every installation attempt, severely degrading performance. Additionally, since the `WalkDir` order depends on the filesystem's directory structure, the calculated directory digest is non-deterministic even if the files are identical.

### 2. Non-Deterministic Receipt IDs
- **Affected File**: `src/marketplace/composition_receipt.rs`
- **Description**: The `CompositionReceipt` structure uses standard `HashMap`s to store version and ownership metadata. When computing the cryptographic ID of the receipt (`compute_receipt_id`), the structure is serialized to JSON.
- **Code Snippets**:
  In `src/marketplace/composition_receipt.rs` (lines 178-180 and 202-204):
  ```rust
  pub versions: HashMap<String, String>,
  ...
  pub ownership_map: HashMap<String, OwnershipRecord>,
  ```
  In `src/marketplace/composition_receipt.rs` (lines 487-498):
  ```rust
  pub fn compute_receipt_id(&mut self) -> Result<()> {
      // Temporarily clear the receipt_id for hashing
      let _ = self.receipt_id.take();

      let json = self.to_json()?;
      let hash = sha2_digest(&json);

      self.receipt_id = Some(hash.clone());
      tracing::debug!("Computed receipt_id: {}", hash);

      Ok(())
  }
  ```
- **Consequences**: Rust’s default `HashMap` uses a randomized hash builder to prevent Denial-of-Service attacks. This causes key iteration order to vary across processes and execution runs. The generated JSON string has varying key sequences, yielding different SHA-256 hashes for functionally identical receipts, breaking the cryptographic chain of custody.

### 3. Insecure Archive Extraction & HTTP Connection Pooling
- **Affected File**: `src/marketplace/install.rs`
- **Description**: 
  1. The archive extraction helper methods `extract_tar_gz` and `extract_zip` unpack files directly to the destination cache path using third-party library extract functions without validating if the files contain path traversal elements (e.g. `..`).
  2. If the extraction process is interrupted, the directory is left in a partially extracted state, leading to subsequent build failures or stale cache loads.
  3. The `download_pack` function instantiates a new HTTP `Client` on every download request instead of reusing a pooled connection, causing excessive socket allocation overhead.
- **Code Snippets**:
  In `src/marketplace/install.rs` (lines 805-840):
  ```rust
  fn extract_tar_gz(&self, data: &[u8], dest: &Path) -> Result<()> {
      use tar::Archive;

      let decoder = GzDecoder::new(data);
      let mut archive = Archive::new(decoder);

      archive
          .unpack(dest)
          .map_err(|e| Error::InstallationFailed {
              reason: format!("Failed to extract tar.gz: {}", e),
          })?;

      Ok(())
  }

  fn extract_zip(&self, data: &[u8], dest: &Path) -> Result<()> {
      use zip::ZipArchive;

      let cursor = std::io::Cursor::new(data);
      let mut archive = ZipArchive::new(cursor).map_err(|e| Error::InstallationFailed {
          reason: format!("Failed to open ZIP archive: {}", e),
      })?;

      archive
          .extract(dest)
          .map_err(|e| Error::InstallationFailed {
              reason: format!("Failed to extract ZIP: {}", e),
          })?;

      Ok(())
  }
  ```
- **Consequences**: This exposes the installer to Zip Slip directory traversal attacks. A malicious package could write files outside the target directory and overwrite critical system or user files. The lack of connection reuse causes connection latency and resource exhaustion during batch downloads.

### 4. RDF Registry Class Hardcoding
- **Affected File**: `src/marketplace/rdf_mapper.rs`
- **Description**: During the reconstruction of a `ReleaseInfo` object from the RDF metadata graph, the mapping layer completely ignores the package's actual registry classification (e.g., `PrivateEnterprise`) and hardcodes it to the default registry class.
- **Code Snippet**:
  In `src/marketplace/rdf_mapper.rs` (lines 624-635):
  ```rust
  Ok(ReleaseInfo {
      version: version.clone(),
      released_at,
      changelog,
      checksum,
      signature,
      download_url,
      dependencies,
      trust_tier,
      registry_class: crate::marketplace::models::default_registry_class(),
  })
  ```
  `default_registry_class()` returns `RegistryClass::Public`.
- **Consequences**: When round-tripped through the RDF graph, private enterprise packages are incorrectly classified as public. Enterprise policy layers (such as the `regulated_finance_profile`) that strictly forbid the installation of public marketplace packages will erroneously block these packages.

### 5. Naive Case-Sensitive SPARQL Injection Checks
- **Affected File**: `src/marketplace/rdf/rdf_control.rs`
- **Description**: The SPARQL query sanitation logic uses a naive case-sensitive blocklist to detect query injection attacks, checking only for uppercase strings like `"DROP"` or `"DELETE WHERE {"`.
- **Code Snippet**:
  In `src/marketplace/rdf/rdf_control.rs` (lines 391-409):
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
- **Consequences**: An attacker can bypass the check by using lowercase commands (e.g., `drop graph <test>`). Conversely, normal package descriptions containing substrings like `"Dropbox"` or `"DROP"` will trigger false-positives and be blocked.

### 6. Dead Code in FMEA Mitigations & Unimplemented Custom Policies
- **Affected Files**: `src/marketplace/policy.rs` and `src/marketplace/rdf/fmea_mitigations.rs`
- **Description**:
  1. The `rdf/fmea_mitigations.rs` module contains recovery mechanisms (e.g., `mitigate_malformed_triple`, `mitigate_query_timeout`) for FMEA failures that are never actually invoked by the main control plane.
  2. The custom policy engine implementation in `policy.rs` contains stubs that immediately reject custom rules with a failure error.
- **Code Snippet**:
  In `src/marketplace/policy.rs` (lines 513-518):
  ```rust
  PolicyRule::CustomSparql { .. } | PolicyRule::CustomShell { .. } => {
      // Custom rules require execution context
      return Err(Error::ValidationFailed {
          reason: "Custom policy rules require execution context".to_string(),
      });
  }
  ```
- **Consequences**: Policy validation fails immediately if a package uses custom rules. More importantly, executing `CustomShell` rules represents a critical Remote Code Execution (RCE) vector if policy manifests can be supplied by untrusted packages.

### 7. Typestate Trust Tier Comparison Logic Issue
- **Affected File**: `src/marketplace/trust.rs`
- **Description**: The numeric priorities for trust tiers are set such that `Blocked` is `7` (the highest value). The function `meets_requirement` checks if the package's priority is less than or equal to the required priority (`self.priority() <= required.priority()`).
- **Code Snippet**:
  In `src/marketplace/trust.rs` (lines 53-71):
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

  pub fn meets_requirement(self, required: Self) -> bool {
      self.priority() <= required.priority()
  }
  ```
- **Consequences**: Because `Blocked` has priority `7`, any package tier (such as `Experimental` or `EnterpriseCertified`) will satisfy a policy requiring `Blocked` (since `6 <= 7` and `1 <= 7` evaluate to `true`). Additionally, `Quarantined` packages (priority `5`) will satisfy minimum requirements intended for `Experimental` (priority `6`) environments.

---

## Section 2: Audit of marketplace/ Catalog and Validation

This section details catalog errors, ontology issues, broken validation scripts, and configuration inconsistencies in the `marketplace/` directory.

### 1. Broken Documentation Validation Script
- **File Path**: `marketplace/validate-docs.sh`
- **Description**: The validation script checks for the existence of several markdown guides.
- **Code Snippet**:
  ```bash
  REQUIRED_DOCS=(
      "README.md"
      "USER_GUIDE.md"
      "PUBLISHING_GUIDE.md"
      "API.md"
      "DOCUMENTATION_INDEX.md"
  )
  ```
- **Consequences**: Running `validate-docs.sh` fails immediately because `USER_GUIDE.md`, `PUBLISHING_GUIDE.md`, `API.md`, and `DOCUMENTATION_INDEX.md` are missing from the repository. This breaks automated continuous integration checks.

### 2. Dotted Key Dictionary Lookup Bug in Registry Indexer
- **File Path**: `marketplace/scripts/generate_registry_index.py`
- **Description**: The script reads the parsed TOML manifest dictionary using a flat dotted-string key (e.g., `"package.metadata"`), whereas the TOML parser parses dotted tables as nested dictionaries (e.g., `data["package"]["metadata"]`).
- **Code Snippets**:
  In `generate_registry_index.py` (lines 72-75):
  ```python
  package = data.get("package", {})
  package_tags = data.get("package.tags", {})
  package_keywords = data.get("package.keywords", {})
  package_metadata = data.get("package.metadata", {})
  ```
  Which leads to (line 117):
  ```python
  "production_ready": package_metadata.get("production_ready", False) if isinstance(package_metadata, dict) else False,
  ```
- **Consequences**: `package_metadata` always resolves to `{}`. Consequently, all packages are written to the index with `production_ready = false` even if their manifests define them as `true`.

### 3. OWL Semantic Violations in ontology.ttl
- **File Path**: `marketplace/ontology.ttl`
- **Description**: 
  1. `market:license` is declared as an `owl:ObjectProperty` but specifies a literal range (`xsd:string`). An object property's range must be an owl class/individual; literal ranges require `owl:DatatypeProperty`.
  2. Datatype properties use `owl:sameAs` to assign literal numbers, which violates OWL axioms.
- **Code Snippets**:
  In `marketplace/ontology.ttl` (lines 83-86):
  ```turtle
  market:license a owl:ObjectProperty ;
    rdfs:label "License"@en ;
    rdfs:domain market:Package ;
    rdfs:range xsd:string .
  ```
  In `marketplace/ontology.ttl` (lines 475-488):
  ```turtle
  market:criticalGuardsThreshold a owl:DatatypeProperty ;
    rdfs:label "Critical Guards Threshold"@en ;
    rdfs:domain market:ScoringScheme ;
    rdfs:range xsd:decimal ;
    owl:sameAs "80"^^xsd:decimal ;
    rdfs:comment "Minimum score required if any critical guard fails" .
  ```
- **Consequences**: The ontology violates standard OWL semantic checks. Any standards-compliant Semantic Web tool or reasoner will reject the ontology as invalid.

### 4. Packs Referencing Non-Existent Packages & External Crates
- **Affected Files**: Pack manifests like `marketplace/packs/devops-automation.toml`, `mcp-rust.toml`, and `lsp-max.toml`
- **Description**: The pack configuration files reference packages that do not exist in the marketplace catalog, or confuse external Rust crates with marketplace packs.
- **Examples**:
  - `devops-automation.toml` references `cicd-pipeline-generator` and `docker-compose-template`, which are missing.
  - `mcp-rust.toml` and `lsp-max.toml` list external crates like `tokio`, `serde`, and `axum` in their `packages` array.
- **Consequences**: Resolving pack dependencies fails during deployment since the packages are not indexed in the registry.

### 5. Fake/Incomplete Validations & Ignored Test Suites
- **Affected Files**: `crates/ggen-marketplace/src/marketplace/validation.rs` and `marketplace/packages/agent-editor/tests/integration_test.rs`
- **Description**:
  1. The `ReadmeValidator` does not check the file system for a `README.md` file; it simply verifies that the description field in the manifest metadata is not empty.
  2. Integration tests in the `agent-editor` package are marked `#[ignore]` and contain dummy `assert!(true)` validations, despite claims of high test coverage.
- **Code Snippet**:
  In `src/marketplace/validation.rs` (lines 427-430):
  ```rust
  async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
      // In a real implementation, this would check for actual README files
      let has_documentation = !package.metadata.description.is_empty();
  ```
  In `agent-editor/tests/integration_test.rs` (lines 5-10):
  ```rust
  #[test]
  #[ignore]
  fn test_agent_initialization() {
      // Test agent creation
      assert!(true);
  }
  ```
- **Consequences**: This violates the `AGENTS.md` verification constitution's anti-cheating guidelines and hides coverage deficits behind ignored test suites.

### 6. Duplicate Entry in index.json
- **File Path**: `marketplace/index.json`
- **Description**: The index file contains two entries for `chatman-cli`.
- **Code Snippet**:
  In `marketplace/index.json` (lines 171-189):
  ```json
  {
    "id": "chatman-cli",
    "path": "packages/chatman-cli",
    ...
  },
  {
    "id": "chatman-cli",
    "path": "packages/chatman-cli-0.1.0",
    ...
  }
  ```
  The directory `packages/chatman-cli-0.1.0` does not exist in the filesystem.
- **Consequences**: This occurs because `rebuild-index.py` crawls package directories recursively using `.rglob("package.toml")` and regex-based TOML field parsing, importing legacy/stale versions of manifests.

### 7. Missing cargo make Tasks
- **File Path**: `Makefile.toml`
- **Description**: The `marketplace/README.md` instructs users to run `cargo make marketplace-validate`, `cargo make marketplace-report`, and `cargo make marketplace-validate-update`. However, none of these tasks exist in `Makefile.toml`.
- **Consequences**: Users following the readme instructions encounter command execution failures.

---

## Section 3: Typestate Patterns & Refactoring Recommendations

To resolve the core architectural defects, we recommend adopting type-safe compile-time states (typestates) and formal enum priorities.

### 1. Typestates for CompositionReceipt States
To prevent non-deterministic and unverified receipts from being serialized or chained, we propose modeling the receipt lifecycle via distinct types:
```rust
pub struct Draft;
pub struct Signed;
pub struct Verified;

pub struct CompositionReceipt<State = Draft> {
    pub receipt_id: Option<String>,
    pub parent_receipt_id: Option<String>,
    pub versions: std::collections::BTreeMap<String, String>, // Use BTreeMap for deterministic sorting
    pub ownership_map: std::collections::BTreeMap<String, OwnershipRecord>,
    // Other fields...
    _state: std::marker::PhantomData<State>,
}

impl CompositionReceipt<Draft> {
    pub fn sign(self, key: &ed25519_dalek::SigningKey) -> CompositionReceipt<Signed> {
        // Deterministically serialize BTreeMaps to ensure stable hashing
        // Perform signature generation
        CompositionReceipt {
            receipt_id: Some(calculated_deterministic_hash),
            _state: std::marker::PhantomData,
            // Copy other fields...
        }
    }
}

impl CompositionReceipt<Signed> {
    pub fn verify(self) -> Result<CompositionReceipt<Verified>, VerificationError> {
        // Validate cryptographic signatures
        Ok(CompositionReceipt {
            _state: std::marker::PhantomData,
            // Copy fields...
        })
    }
}
```
*Note*: Changing `HashMap` to `BTreeMap` ensures deterministic iteration and serialization order, preventing unstable receipt IDs.

### 2. Typestates for PackDownloader & Installer States
To prevent directory extraction of unverified packages or Zip Slip exploits, we should enforce verification boundaries at compile-time:
```rust
pub struct Unverified;
pub struct VerifiedDigest;
pub struct Extracted;
pub struct Validated;

pub struct PackInstaller<State = Unverified> {
    pack_data: Vec<u8>,
    package_id: PackageId,
    version: PackageVersion,
    extracted_path: Option<PathBuf>,
    _state: std::marker::PhantomData<State>,
}

impl PackInstaller<Unverified> {
    pub fn verify_checksum(self, expected_checksum: &str) -> Result<PackInstaller<VerifiedDigest>, ValidationError> {
        let calculated = ChecksumCalculator::calculate(&self.pack_data);
        if calculated == expected_checksum {
            Ok(PackInstaller { _state: std::marker::PhantomData, .. })
        } else {
            Err(ValidationError::ChecksumMismatch)
        }
    }
}

impl PackInstaller<VerifiedDigest> {
    pub fn extract(self, temp_base_dir: &Path) -> Result<PackInstaller<Extracted>, ExtractionError> {
        // 1. Unpack to a unique temporary directory
        // 2. Validate every file path to protect against Zip Slip traversal (no '..')
        Ok(PackInstaller {
            extracted_path: Some(temp_path),
            _state: std::marker::PhantomData,
            ..
        })
    }
}

impl PackInstaller<Extracted> {
    pub fn validate(self, policy: &Policy) -> Result<PackInstaller<Validated>, PolicyError> {
        // Run full ontology and metadata policy checks on extracted files
        Ok(PackInstaller { _state: std::marker::PhantomData, .. })
    }
}
```

### 3. Safe TrustTier Logic
Replace the numeric comparison priority with a strict enum ordering or a match-based validator that handles quarantined/blocked states explicitly:
```rust
impl TrustTier {
    pub fn satisfies(self, required: Self) -> bool {
        match (self, required) {
            (Self::Blocked, _) | (_, Self::Blocked) => false, // Blocked is never satisfied and satisfies nothing
            (Self::Quarantined, Self::Experimental) => false, // Quarantined cannot satisfy experimental
            (s, r) => s.priority() <= r.priority(),
        }
    }
}
```

---

## Section 4: Actionable Refactoring Roadmap

This table outlines prioritized remediation steps for each of the identified issues:

| Task / File | Severity | Estimated Effort | Concrete Action |
| :--- | :--- | :--- | :--- |
| **Cache Verification** / `cache.rs` | **Critical** | 4 hours | Update `verify_digest` to match the exact mechanism used to compute installer checksums (e.g. hash the compressed archive file, or verify directory structure against a deterministic manifest). |
| **Zip Slip & Temp Extr** / `install.rs` | **Critical** | 3 hours | Add path validation checks in `extract_zip` and `extract_tar_gz` preventing files from resolving outside of the target path. Extract to a temp directory first, then atomically rename to avoid partial states. |
| **Receipt Determinism** / `composition_receipt.rs` | **High** | 2 hours | Replace `HashMap` with `BTreeMap` for all serializable fields, enforcing a stable iteration order for cryptographic JSON signatures. |
| **SPARQL injection** / `rdf_control.rs` | **High** | 2 hours | Re-implement sanitation using a case-insensitive check and clean token-based SPARQL parsing instead of basic string matching. |
| **Dotted Key TOML Lookup** / `generate_registry_index.py` | **High** | 1 hour | Refactor flat dictionary lookup from `data.get("package.metadata", {})` to nested lookup `data.get("package", {}).get("metadata", {})`. |
| **Registry Class Mapping** / `rdf_mapper.rs` | **High** | 2 hours | Map the `registry_class` enum to the RDF model and restore it during RDF-to-ReleaseInfo deserialization rather than hardcoding to `Public`. |
| **Trust Tier comparison** / `trust.rs` | **High** | 1 hour | Re-order priority mapping or implement explicit match rules so `Blocked` cannot satisfy any requirements, and `Quarantined` does not bypass `Experimental` checks. |
| **Documentation Script** / `validate-docs.sh` | **Medium** | 1 hour | Create the missing documentation guides or update the required script list to reflect existing files. |
| **Ontology OWL Axioms** / `ontology.ttl` | **Medium** | 2 hours | Change `market:license` to `owl:DatatypeProperty`. Replace `owl:sameAs` with custom properties or remove it from numeric thresholds. |
| **Invalid Pack References** / Pack TOMLs | **Medium** | 1 hour | Clean up pack manifests by removing references to non-existent packages and external dependencies. |
| **Fake README Validation** / `validation.rs` | **Medium** | 2 hours | Replace the metadata description checker in `ReadmeValidator` with a genuine file existence check in the package's local directory. |
| **Ignored Unit Tests** / Package tests | **Medium** | 4 hours | Remove dummy `assert!(true)` tests and `#[ignore]` attributes, replacing them with functional integration tests. |
| **Duplicate Index entries** / `index.json` | **Low** | 1 hour | Remove the duplicate entry for `chatman-cli-0.1.0` and correct the indexing crawler to avoid directories containing version suffixes. |
| **Makefile Tasks** / `Makefile.toml` | **Low** | 1 hour | Define the missing tasks (`marketplace-validate`, `marketplace-report`, and `marketplace-validate-update`) in the cargo make file. |
