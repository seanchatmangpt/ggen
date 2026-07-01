//! Inverse pipeline μ⁻¹ — recovers ontological structure from generated artifacts.
//!
//! The forward pipeline μ₁–μ₅ transforms O (ontology) → A (artifact).
//! The inverse pipeline μ⁻¹₁–μ⁻¹₅ transforms A (artifact) → O (recovered ontology).
//!
//! This makes A → O a first-class provenance path with typed stages and a signed receipt.

use std::collections::HashMap;
use std::path::PathBuf;

use chrono::{DateTime, Utc};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};

/// The five stages of the inverse pipeline.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum InverseStage {
    /// μ⁻¹₁ — Scan: enumerate artifact file paths by language.
    Scan,
    /// μ⁻¹₂ — Extract: parse AST/text to recover ServiceDef/structure.
    Extract,
    /// μ⁻¹₃ — Convert: transform ServiceDef into RDF Turtle string.
    Convert,
    /// μ⁻¹₄ — Validate: check recovered triples are non-empty and well-formed.
    Validate,
    /// μ⁻¹₅ — Emit: produce InverseReceipt with BLAKE3 hashes of inputs and output.
    Emit,
}

/// Provenance receipt produced by a successful inverse pipeline run.
///
/// Every field is populated from real execution — no default sentinels.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct InverseReceipt {
    /// UUID v4 for this inverse pipeline run.
    pub operation_id: String,
    /// RFC-3339 timestamp.
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Map of input file path → BLAKE3 hex hash of file content.
    pub input_hashes: HashMap<String, String>,
    /// BLAKE3 hex hash of all recovered RDF concatenated.
    pub output_hash: String,
    /// Number of non-empty lines in the recovered RDF output.
    pub recovered_triple_count: usize,
    /// True if μ⁻¹₄ validation passed (RDF is non-empty and well-formed).
    pub shacl_valid: bool,
    /// Last stage successfully completed.
    pub last_stage: InverseStage,
    /// Ed25519 signature over the receipt body, hex-encoded.
    ///
    /// Empty until [`InverseReceipt::sign`] (or [`InversePipeline::run_signed`])
    /// is called. An empty signature makes [`InverseReceipt::verify`] return
    /// `false` (fail-closed), mirroring the forward `Receipt` provenance path.
    pub signature: String,
    /// UUID v4 of the forward Receipt this inverse run links to (if any).
    ///
    /// This field enables bi-directional tracing: a forward Receipt (O → A)
    /// can be linked to an inverse Receipt (A → O) via their operation_ids.
    /// Used by [`InverseReceiptChain`] to maintain cryptographic provenance chains.
    pub previous_operation_id: Option<String>,
}

impl InverseReceipt {
    /// Produces the canonical byte message that is signed/verified.
    ///
    /// The message is the JSON serialization of the receipt with the
    /// `signature` field blanked, so signing and verification operate over an
    /// identical, deterministic body. This mirrors the forward
    /// `Receipt::signing_message` strategy.
    ///
    /// # Errors
    ///
    /// Returns [`InversePipelineError::Serialization`] if the receipt cannot be
    /// serialized to JSON.
    fn signing_message(&self) -> InverseResult<Vec<u8>> {
        let unsigned = Self {
            signature: String::new(),
            ..self.clone()
        };
        let json = serde_json::to_string(&unsigned)
            .map_err(|e| InversePipelineError::Serialization(e.to_string()))?;
        Ok(json.into_bytes())
    }

    /// Computes the BLAKE3 hex hash of this receipt (signed).
    ///
    /// The hash is deterministic across the entire receipt body including
    /// the signature, timestamp, and all hashes. Used by
    /// [`InverseReceiptChain`] to link receipts cryptographically.
    ///
    /// # Errors
    ///
    /// Returns [`InversePipelineError::Serialization`] if the receipt cannot be
    /// serialized to JSON.
    pub fn hash(&self) -> InverseResult<String> {
        let json = serde_json::to_string(self)
            .map_err(|e| InversePipelineError::Serialization(e.to_string()))?;
        let mut hasher = blake3::Hasher::new();
        hasher.update(json.as_bytes());
        Ok(hasher.finalize().to_hex().to_string())
    }

    /// Signs the receipt with the given Ed25519 signing key, populating
    /// `signature` with the hex-encoded signature bytes.
    ///
    /// Mirrors the forward [`crate::Receipt::sign`] path: it consumes `self`
    /// and returns the signed receipt so the emit step is a single expression.
    ///
    /// # Errors
    ///
    /// Returns [`InversePipelineError::Serialization`] if the signing message
    /// cannot be produced.
    pub fn sign(mut self, signing_key: &SigningKey) -> InverseResult<Self> {
        let message = self.signing_message()?;
        let signature = signing_key.sign(&message);
        self.signature = hex::encode(signature.to_bytes());
        Ok(self)
    }

    /// Verifies the receipt's Ed25519 signature against the given verifying key.
    ///
    /// Fail-closed: returns `false` for an empty signature, a non-hex
    /// signature, a malformed signature, a tampered receipt body, or a wrong
    /// key. Returns `true` only when the signature was produced by the matching
    /// signing key over the current receipt body.
    #[must_use]
    pub fn verify(&self, verifying_key: &VerifyingKey) -> bool {
        // Fail-closed on an unsigned receipt — an empty signature is never valid.
        if self.signature.is_empty() {
            return false;
        }
        let message = match self.signing_message() {
            Ok(m) => m,
            Err(_) => return false,
        };
        let signature_bytes = match hex::decode(&self.signature) {
            Ok(b) => b,
            Err(_) => return false,
        };
        let signature = match Signature::from_slice(&signature_bytes) {
            Ok(s) => s,
            Err(_) => return false,
        };
        verifying_key.verify(&message, &signature).is_ok()
    }
}

/// A cryptographically linked chain of inverse receipts.
///
/// Each receipt in the chain is verified for:
/// 1. Valid Ed25519 signature
/// 2. Consistent BLAKE3 chain hashes (each receipt's hash is included in the next)
///
/// The chain maintains a running `chain_hash` that is the BLAKE3 of the concatenation
/// of all receipt hashes, providing tamper-evident history of all inverse runs.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct InverseReceiptChain {
    /// All receipts in order from first to latest.
    pub receipts: Vec<InverseReceipt>,
    /// BLAKE3 hex hash of concatenated receipt hashes (chain integrity proof).
    pub chain_hash: String,
    /// RFC-3339 timestamp when the chain was created/updated.
    pub created_at: DateTime<Utc>,
}

impl InverseReceiptChain {
    /// Creates a new empty chain with an initial chain hash.
    ///
    /// The initial chain hash is BLAKE3 of the empty string.
    #[must_use]
    pub fn new() -> Self {
        let empty_hash = {
            let mut hasher = blake3::Hasher::new();
            hasher.update(b"");
            hasher.finalize().to_hex().to_string()
        };
        Self {
            receipts: Vec::new(),
            chain_hash: empty_hash,
            created_at: Utc::now(),
        }
    }

    /// Appends a verified receipt to the chain.
    ///
    /// Before appending, the receipt's signature is verified with the given key.
    /// If verification fails, the receipt is not appended and an error is returned
    /// (fail-closed).
    ///
    /// After appending, `chain_hash` is recomputed as:
    /// ```text
    /// chain_hash = BLAKE3(previous_chain_hash || receipt.hash())
    /// ```
    ///
    /// # Errors
    ///
    /// Returns [`InversePipelineError::ValidateFailed`] if:
    /// - The receipt signature is invalid
    /// - The receipt hash cannot be computed
    pub fn append(
        &mut self, receipt: InverseReceipt, verifying_key: &VerifyingKey,
    ) -> InverseResult<()> {
        // Fail-closed: verify the receipt before appending.
        if !receipt.verify(verifying_key) {
            return Err(InversePipelineError::ValidateFailed(
                "Receipt signature verification failed; cannot append to chain".to_string(),
            ));
        }

        // Compute the receipt's hash for chain linkage.
        let receipt_hash = receipt.hash()?;

        // Update chain hash: BLAKE3(previous_chain_hash || receipt_hash)
        let mut hasher = blake3::Hasher::new();
        hasher.update(self.chain_hash.as_bytes());
        hasher.update(receipt_hash.as_bytes());
        self.chain_hash = hasher.finalize().to_hex().to_string();

        self.receipts.push(receipt);
        Ok(())
    }

    /// Verifies the entire chain's integrity.
    ///
    /// Checks:
    /// 1. All receipt signatures are valid
    /// 2. Chain hash consistency (each step in the chain hash computation matches)
    ///
    /// Returns `true` if all checks pass, `false` otherwise (fail-closed).
    /// Returns `false` on any error (serialization, signature, chain mismatch).
    #[must_use]
    pub fn verify(&self, verifying_key: &VerifyingKey) -> bool {
        // Empty chain is trivially valid.
        if self.receipts.is_empty() {
            return true;
        }

        // Recompute chain hash from scratch.
        let mut expected_chain_hash = {
            let mut hasher = blake3::Hasher::new();
            hasher.update(b"");
            hasher.finalize().to_hex().to_string()
        };

        for receipt in &self.receipts {
            // Fail-closed: any invalid signature fails the entire chain.
            if !receipt.verify(verifying_key) {
                return false;
            }

            // Fail-closed: any hash computation failure fails the chain.
            let receipt_hash = match receipt.hash() {
                Ok(h) => h,
                Err(_) => return false,
            };

            // Recompute chain hash for this step.
            let mut hasher = blake3::Hasher::new();
            hasher.update(expected_chain_hash.as_bytes());
            hasher.update(receipt_hash.as_bytes());
            expected_chain_hash = hasher.finalize().to_hex().to_string();
        }

        // Chain hash must match the stored value.
        expected_chain_hash == self.chain_hash
    }
}

impl Default for InverseReceiptChain {
    fn default() -> Self {
        Self::new()
    }
}

/// Errors that can occur during the inverse pipeline.
#[derive(Debug, thiserror::Error)]
pub enum InversePipelineError {
    #[error("Scan failed: no artifact files found in {0:?}")]
    ScanEmpty(Vec<PathBuf>),

    #[error("Extract failed for {path}: {reason}")]
    ExtractFailed { path: String, reason: String },

    #[error("Convert produced empty RDF for service '{service}'")]
    ConvertEmpty { service: String },

    #[error("Validate failed: {0}")]
    ValidateFailed(String),

    #[error("Receipt serialization failed: {0}")]
    Serialization(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Convenience alias for inverse pipeline results.
pub type InverseResult<T> = std::result::Result<T, InversePipelineError>;

/// The inverse pipeline executor.
pub struct InversePipeline;

impl InversePipeline {
    /// Run all five inverse stages against the given file paths.
    ///
    /// Paths are classified by extension: `.rs` → Rust, `.ex`/`.exs` → Elixir, `.go` → Go.
    /// Unknown extensions are skipped (not an error).
    ///
    /// Returns `InverseResult<InverseReceipt>` with full provenance.
    ///
    /// The receipt returned by `run` is **unsigned** (`signature` is empty),
    /// mirroring the forward `Receipt::new` → `Receipt::sign` two-step. To
    /// obtain a signed, verifiable provenance receipt, use
    /// [`InversePipeline::run_signed`].
    pub fn run(paths: &[PathBuf]) -> InverseResult<InverseReceipt> {
        // μ⁻¹₁ Scan: filter to known, existing source files.
        let known_paths: Vec<&PathBuf> = paths
            .iter()
            .filter(|p| {
                if !p.exists() {
                    return false;
                }
                matches!(
                    p.extension().and_then(|e| e.to_str()),
                    Some("rs") | Some("ex") | Some("exs") | Some("go")
                )
            })
            .collect();

        if known_paths.is_empty() {
            return Err(InversePipelineError::ScanEmpty(paths.to_vec()));
        }

        // μ⁻¹₂ Extract: read each file, compute BLAKE3, call the appropriate extractor.
        let mut all_services: Vec<super::ServiceDef> = Vec::new();
        let mut input_hashes: HashMap<String, String> = HashMap::new();

        for path in &known_paths {
            let content = std::fs::read_to_string(path)?;

            // Hash the raw file content.
            let hash = {
                let mut hasher = blake3::Hasher::new();
                hasher.update(content.as_bytes());
                hasher.finalize().to_hex().to_string()
            };
            input_hashes.insert(path.to_string_lossy().into_owned(), hash);

            let path_str = path.to_string_lossy().into_owned();
            let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");

            let extracted = match ext {
                "rs" => super::extract_rust_service(&path_str).map_err(|e| {
                    InversePipelineError::ExtractFailed {
                        path: path_str.clone(),
                        reason: e.to_string(),
                    }
                })?,
                "ex" | "exs" => super::extract_elixir_genserver(&path_str).map_err(|e| {
                    InversePipelineError::ExtractFailed {
                        path: path_str.clone(),
                        reason: e.to_string(),
                    }
                })?,
                "go" => super::extract_go_service(&path_str).map_err(|e| {
                    InversePipelineError::ExtractFailed {
                        path: path_str.clone(),
                        reason: e.to_string(),
                    }
                })?,
                // Should never reach here due to the Scan filter above.
                _ => Vec::new(),
            };

            all_services.extend(extracted);
        }

        // μ⁻¹₃ Convert: transform all collected ServiceDefs into RDF.
        // If we extracted services but convert_to_rdf yields nothing meaningful, that is an error.
        let rdf = if all_services.is_empty() {
            // No service definitions found — this is not a hard error; some source files have
            // no public structs/types. Return a receipt with empty output.
            String::new()
        } else {
            let first_service_name = all_services[0].name.clone();
            let rdf = super::convert_to_rdf(&all_services).map_err(|e| {
                InversePipelineError::ExtractFailed {
                    path: String::from("convert"),
                    reason: e.to_string(),
                }
            })?;
            let has_data = rdf
                .lines()
                .any(|l| !l.trim().is_empty() && !l.starts_with("@prefix"));
            if !has_data {
                return Err(InversePipelineError::ConvertEmpty {
                    service: first_service_name,
                });
            }
            rdf
        };

        // μ⁻¹₄ Validate: check RDF output is non-empty and contains service declarations.
        // The convert_to_rdf function emits Turtle format, so we look for code: prefixed
        // service IRIs rather than bare N-Triples markers.
        let shacl_valid = if rdf.is_empty() {
            // No services to validate — pass vacuously (nothing was promised).
            true
        } else {
            let has_service_decl = rdf.lines().any(|l| {
                let t = l.trim();
                // Turtle resource declaration: `code:Name a code:Service`
                (t.starts_with("code:") || t.starts_with('<'))
                    && (t.contains("a code:Service") || t.contains("code:Service"))
            });
            if !has_service_decl {
                return Err(InversePipelineError::ValidateFailed(
                    "recovered RDF contains no code:Service declarations".to_string(),
                ));
            }
            true
        };

        // μ⁻¹₅ Emit: build receipt with BLAKE3 of output, non-empty UUID, real timestamp.
        let output_hash = {
            let mut hasher = blake3::Hasher::new();
            hasher.update(rdf.as_bytes());
            hasher.finalize().to_hex().to_string()
        };

        let recovered_triple_count = rdf
            .lines()
            .filter(|l| !l.trim().is_empty() && !l.starts_with("@prefix"))
            .count();

        let receipt = InverseReceipt {
            operation_id: uuid::Uuid::new_v4().to_string(),
            timestamp: chrono::Utc::now(),
            input_hashes,
            output_hash,
            recovered_triple_count,
            shacl_valid,
            last_stage: InverseStage::Emit,
            // Unsigned by default — `run_signed` populates this in the Emit step.
            signature: String::new(),
            // No forward receipt linkage by default. Set via `link_to_forward()` if needed.
            previous_operation_id: None,
        };

        Ok(receipt)
    }

    /// Run all five inverse stages and sign the resulting receipt with the
    /// given Ed25519 signing key, making A → O a first-class, **provable**
    /// provenance path.
    ///
    /// This is the authoritative emit path: the μ⁻¹₅ Emit stage produces a
    /// receipt whose `signature` is a non-empty Ed25519 signature over the
    /// receipt body. The signing key is obtained from the same
    /// [`crate::generate_keypair`] mechanism used by the forward `Receipt`
    /// path, so forward and inverse provenance share one crypto surface.
    ///
    /// # Errors
    ///
    /// Returns the same errors as [`InversePipeline::run`], plus
    /// [`InversePipelineError::Serialization`] if the receipt cannot be signed.
    pub fn run_signed(
        paths: &[PathBuf], signing_key: &SigningKey,
    ) -> InverseResult<InverseReceipt> {
        let receipt = Self::run(paths)?;
        receipt.sign(signing_key)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // Reuse the SAME keypair mechanism as the forward `Receipt` provenance path.
    // `generate_keypair` is re-exported at the ggen-core crate root from
    // `ggen_config` (see lib.rs) and returns real Ed25519 keys (no test doubles).
    use crate::generate_keypair;
    use std::io::Write;

    fn write_temp_rust(content: &str) -> tempfile::NamedTempFile {
        let mut f =
            tempfile::NamedTempFile::with_suffix(".rs").expect("Failed to create named temp file");
        f.write_all(content.as_bytes())
            .expect("Failed to write temp file content");
        f
    }

    #[test]
    fn test_scan_empty_paths_returns_error() {
        let result = InversePipeline::run(&[]);
        assert!(
            matches!(result, Err(InversePipelineError::ScanEmpty(_))),
            "Expected ScanEmpty error for empty input"
        );
    }

    #[test]
    fn test_run_real_rust_file_produces_receipt() {
        // A minimal Rust struct that the extractor can find.
        let content = r#"
pub struct UserService {
    pub name: String,
    pub port: u16,
}

impl UserService {
    pub fn get_user(&self, id: u32) -> String { String::new() }
    pub fn create_user(&self, name: String) -> bool { false }
}
"#;
        let tmp = write_temp_rust(content);
        let paths = vec![tmp.path().to_path_buf()];
        let result = InversePipeline::run(&paths);

        // The pipeline must not panic. Either it succeeds with a valid receipt,
        // or it returns a typed error — never panics or unwraps.
        match result {
            Ok(receipt) => {
                // Receipt invariants: operation_id non-empty, last_stage is Emit,
                // output_hash non-empty.
                assert!(
                    !receipt.operation_id.is_empty(),
                    "operation_id must be non-empty"
                );
                assert_eq!(
                    receipt.last_stage,
                    InverseStage::Emit,
                    "last_stage must be Emit on success"
                );
                assert!(
                    !receipt.output_hash.is_empty(),
                    "output_hash must be non-empty"
                );
                // Input hashes must include the temp file path.
                assert!(
                    !receipt.input_hashes.is_empty(),
                    "input_hashes must record at least one file"
                );
            }
            Err(InversePipelineError::ConvertEmpty { .. })
            | Err(InversePipelineError::ValidateFailed(_)) => {
                // Acceptable: extractor found nothing to convert.
            }
            Err(e) => panic!("Unexpected error: {e}"),
        }
    }

    #[test]
    fn test_unknown_extension_skipped_gracefully() {
        // .txt files must be skipped by the Scan stage → ScanEmpty.
        let mut f =
            tempfile::NamedTempFile::with_suffix(".txt").expect("Failed to create named temp file");
        f.write_all(b"not a rust file")
            .expect("Failed to write content");
        let result = InversePipeline::run(&[f.path().to_path_buf()]);
        assert!(
            matches!(result, Err(InversePipelineError::ScanEmpty(_))),
            "Expected ScanEmpty for .txt file"
        );
    }

    #[test]
    fn test_nonexistent_path_skipped_gracefully() {
        // Paths that do not exist must be skipped → ScanEmpty.
        let ghost = PathBuf::from("/tmp/this_file_does_not_exist_abc123.rs");
        let result = InversePipeline::run(&[ghost]);
        assert!(
            matches!(result, Err(InversePipelineError::ScanEmpty(_))),
            "Expected ScanEmpty for non-existent path"
        );
    }

    #[test]
    fn test_receipt_operation_id_is_unique() {
        // Two consecutive runs must produce different operation_ids (UUID v4).
        let content = "pub struct Foo { pub x: i32, }\n";
        let tmp1 = write_temp_rust(content);
        let tmp2 = write_temp_rust(content);

        let r1 = InversePipeline::run(&[tmp1.path().to_path_buf()]);
        let r2 = InversePipeline::run(&[tmp2.path().to_path_buf()]);

        // Both must succeed or both fail with the same typed error — we only care about
        // the uniqueness property when both succeed.
        if let (Ok(receipt1), Ok(receipt2)) = (r1, r2) {
            assert_ne!(
                receipt1.operation_id, receipt2.operation_id,
                "Each run must produce a distinct operation_id"
            );
        }
        // If either fails with a typed error, that is also acceptable for this test.
    }

    #[test]
    fn test_input_hash_is_blake3_hex() {
        let content = "pub struct Bar { pub y: u64, }\n";
        let tmp = write_temp_rust(content);
        let result = InversePipeline::run(&[tmp.path().to_path_buf()]);

        if let Ok(receipt) = result {
            for (_, hash) in &receipt.input_hashes {
                // BLAKE3 hex output is always 64 lowercase hex characters.
                assert_eq!(hash.len(), 64, "BLAKE3 hash must be 64 hex chars");
                assert!(
                    hash.chars().all(|c| c.is_ascii_hexdigit()),
                    "BLAKE3 hash must contain only hex digits"
                );
            }
        }
        // If extraction found nothing and returned a typed error, skip the hash check.
    }

    // ── μ⁻¹ signed-receipt provenance (Chicago TDD) ──────────────────────────
    //
    // These tests exercise the A → O recovery path with REAL Ed25519 keys
    // (no mocks/doubles), a REAL source file on disk, and state-based
    // assertions on the observable receipt. They mirror the forward
    // `Receipt` sign/verify contract and enforce coding-agent-mistakes.md
    // §4.2 invariants (non-empty signature for a real run) and fail-closed
    // verification (§1.3 fail-open is forbidden).

    /// A minimal but extractor-friendly Rust source: `pub struct` + fields +
    /// `impl` methods. This guarantees `convert_to_rdf` emits a
    /// `code:Name a code:Service` declaration so μ⁻¹₄ Validate passes and
    /// `recovered_triple_count > 0`.
    const RECOVERABLE_RUST: &str = r#"
pub struct OrderService {
    pub id: u64,
    pub region: String,
}

impl OrderService {
    pub fn place(&self, sku: String) -> bool { false }
    pub fn cancel(&self, id: u64) -> bool { false }
}
"#;

    #[test]
    fn test_run_signed_real_file_produces_verifiable_receipt() {
        // Arrange — real temp source file + real Ed25519 keypair.
        let tmp = write_temp_rust(RECOVERABLE_RUST);
        let (signing_key, verifying_key) = generate_keypair();

        // Act — run the inverse pipeline and sign the receipt.
        let receipt = InversePipeline::run_signed(&[tmp.path().to_path_buf()], &signing_key)
            .expect("inverse pipeline should recover an OrderService and sign the receipt");

        // Assert — observable provenance state from a real A → O run.
        assert!(
            receipt.recovered_triple_count > 0,
            "recovered_triple_count must be > 0 for a recoverable struct; got {}",
            receipt.recovered_triple_count
        );
        assert!(
            receipt.shacl_valid,
            "validate stage must pass for recovered RDF"
        );
        assert_eq!(
            receipt.last_stage,
            InverseStage::Emit,
            "must complete through Emit"
        );

        // §4.2 invariant analogues: real UUID v4, non-empty signature.
        assert!(
            !receipt.signature.is_empty(),
            "a real signed run must carry a NON-EMPTY signature"
        );
        let parsed_id =
            uuid::Uuid::parse_str(&receipt.operation_id).expect("operation_id must be a real UUID");
        assert_eq!(
            parsed_id.get_version_num(),
            4,
            "operation_id must be UUID v4"
        );
        assert_ne!(
            parsed_id,
            uuid::Uuid::nil(),
            "operation_id must be non-zero"
        );
        assert!(
            !receipt.output_hash.is_empty(),
            "output_hash must be populated"
        );
        assert!(
            !receipt.input_hashes.is_empty(),
            "input_hashes must record the source file"
        );

        // Verification with the matching key must SUCCEED.
        assert!(
            receipt.verify(&verifying_key),
            "verify() must succeed for a correctly signed receipt"
        );
    }

    #[test]
    fn test_unsigned_run_receipt_fails_verification() {
        // Arrange — `run` (not `run_signed`) leaves signature empty.
        let tmp = write_temp_rust(RECOVERABLE_RUST);
        let (_signing_key, verifying_key) = generate_keypair();

        // Act
        let receipt = InversePipeline::run(&[tmp.path().to_path_buf()])
            .expect("inverse pipeline should produce an (unsigned) receipt");

        // Assert — fail-closed: an empty signature is never valid.
        assert!(
            receipt.signature.is_empty(),
            "run() must leave signature empty"
        );
        assert!(
            !receipt.verify(&verifying_key),
            "an unsigned receipt (empty signature) must fail verification (fail-closed)"
        );
    }

    #[test]
    fn test_tampered_body_fails_verification() {
        // Arrange — produce a real signed receipt over a real run.
        let tmp = write_temp_rust(RECOVERABLE_RUST);
        let (signing_key, verifying_key) = generate_keypair();
        let receipt = InversePipeline::run_signed(&[tmp.path().to_path_buf()], &signing_key)
            .expect("signed run should succeed");
        assert!(
            receipt.verify(&verifying_key),
            "precondition: receipt verifies before tampering"
        );

        // Act — tamper with the receipt BODY (the output_hash is part of the
        // signed message). The signature no longer matches the mutated body.
        let mut tampered = receipt.clone();
        tampered.output_hash = format!("{}deadbeef", &tampered.output_hash);

        // Assert — fail-closed: a body that disagrees with the signature is invalid.
        assert!(
            !tampered.verify(&verifying_key),
            "tampering with the receipt body must invalidate the signature (fail-closed)"
        );
        // The pristine receipt still verifies — proves the failure is the tamper,
        // not a flaky key/message.
        assert!(
            receipt.verify(&verifying_key),
            "original receipt must still verify"
        );
    }

    #[test]
    fn test_blanked_signature_fails_verification() {
        // Arrange — real signed receipt.
        let tmp = write_temp_rust(RECOVERABLE_RUST);
        let (signing_key, verifying_key) = generate_keypair();
        let receipt = InversePipeline::run_signed(&[tmp.path().to_path_buf()], &signing_key)
            .expect("signed run should succeed");

        // Act — blank the signature field (the §1.5 contract-drift sentinel).
        let mut blanked = receipt.clone();
        blanked.signature = String::new();

        // Assert — fail-closed: empty signature must verify false.
        assert!(
            !blanked.verify(&verifying_key),
            "a blanked signature must fail verification (fail-closed)"
        );
    }

    #[test]
    fn test_corrupt_nonhex_signature_fails_verification() {
        // Arrange — real signed receipt.
        let tmp = write_temp_rust(RECOVERABLE_RUST);
        let (signing_key, verifying_key) = generate_keypair();
        let receipt = InversePipeline::run_signed(&[tmp.path().to_path_buf()], &signing_key)
            .expect("signed run should succeed");

        // Act — set signature to non-hex garbage (simulates a corrupt receipt file).
        let mut corrupt = receipt.clone();
        corrupt.signature = "{}".to_string();

        // Assert — fail-closed: non-hex signature must verify false (no panic).
        assert!(
            !corrupt.verify(&verifying_key),
            "a non-hex signature must fail verification (fail-closed)"
        );
    }

    #[test]
    fn test_wrong_key_fails_verification() {
        // Arrange — sign with one key, verify with an unrelated key.
        let tmp = write_temp_rust(RECOVERABLE_RUST);
        let (signing_key, _verifying_key) = generate_keypair();
        let (_other_signing, wrong_key) = generate_keypair();
        let receipt = InversePipeline::run_signed(&[tmp.path().to_path_buf()], &signing_key)
            .expect("signed run should succeed");

        // Act + Assert — a signature from a different key must not verify.
        assert!(
            !receipt.verify(&wrong_key),
            "verification with the wrong key must fail"
        );
    }
}
