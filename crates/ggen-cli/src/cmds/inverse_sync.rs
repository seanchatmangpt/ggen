//! Inverse Sync Command — μ⁻¹ Pipeline CLI
//!
//! `ggen inverse-sync` recovers RDF ontologies from generated artifacts,
//! validates coherence, and emits a cryptographic ProvenanceEnvelope.
//!
//! Usage:
//!   ggen inverse-sync --source-dir <PATH> --ontology <PATH>
//!   ggen inverse-sync --source-dir <PATH> --ontology <PATH> --signing-key <PATH>
//!   ggen inverse-sync --source-dir <PATH> --ontology <PATH> --output-envelope <PATH>
//!
//! This command executes the full μ⁻¹ inverse pipeline:
//!   - μ⁻¹₁ Scan: enumerate artifact files by language
//!   - μ⁻¹₂ Extract: parse AST/text to recover service definitions
//!   - μ⁻¹₃ Convert: transform service definitions into RDF Turtle
//!   - μ⁻¹₄ Validate: verify recovered RDF is well-formed
//!   - μ⁻¹₅ Emit: produce signed InverseReceipt
//!
//! Then validates coherence (O ≅ A ≅ L) and emits ProvenanceEnvelope.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use ed25519_dalek::SigningKey;
use ggen_core::receipt::{generate_keypair, ProvenanceEnvelope};
use ggen_core::reverse_sync::inverse_pipeline::InversePipeline;
use ggen_graph::{CoherenceChecker, Pole, PoleState};
use serde::Serialize;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use uuid::Uuid;

// ============================================================================
// Output Types
// ============================================================================

/// Output from `ggen inverse-sync`
#[derive(Debug, Clone, Serialize)]
pub struct InverseSyncOutput {
    /// Overall status: "success" or "error"
    pub status: String,

    /// Number of source files scanned
    pub files_scanned: usize,

    /// Number of recovered triple count
    pub recovered_triples: usize,

    /// Inverse operation ID
    pub inverse_operation_id: String,

    /// Whether the coherence check passed (admitted)
    pub coherence_admitted: bool,

    /// Path to the output envelope
    pub envelope_path: String,

    /// Error message (if failed)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,

    /// Coherence drift details (if incoherent)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub coherence_drifts: Option<Vec<String>>,
}

// ============================================================================
// Domain Helpers (thin layer for complexity management)
// ============================================================================

/// Load and validate the signing key from disk.
fn load_signing_key(key_path: &PathBuf) -> std::result::Result<SigningKey, String> {
    let key_content =
        fs::read_to_string(key_path).map_err(|e| format!("Failed to read signing key: {}", e))?;
    let key_bytes = hex::decode(key_content.trim())
        .map_err(|e| format!("Failed to decode signing key hex: {}", e))?;
    let key_array: [u8; 32] = key_bytes
        .as_slice()
        .try_into()
        .map_err(|_| "Signing key must be exactly 32 bytes".to_string())?;
    Ok(SigningKey::from_bytes(&key_array))
}

/// Resolve the signing key path: explicit arg or default to .ggen/keys/signing.key
fn resolve_signing_key_path(signing_key: Option<String>) -> PathBuf {
    signing_key
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(".ggen/keys/signing.key"))
}

/// Resolve the output envelope path: explicit arg or default to .ggen/envelopes/latest.json
fn resolve_envelope_path(output_envelope: Option<String>) -> PathBuf {
    output_envelope
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(".ggen/envelopes/latest.json"))
}

/// Collect all artifact files from the source directory recursively.
fn collect_artifact_files(source_dir: &PathBuf) -> std::result::Result<Vec<PathBuf>, String> {
    if !source_dir.exists() {
        return Err(format!("Source directory not found: {}", source_dir.display()));
    }

    let mut files = Vec::new();

    for entry in fs::read_dir(source_dir)
        .map_err(|e| format!("Failed to read source directory: {}", e))?
    {
        let entry = entry.map_err(|e| format!("Failed to read directory entry: {}", e))?;
        let path = entry.path();

        if path.is_dir() {
            // Recursively collect from subdirectories.
            let sub_files = collect_artifact_files(&path)?;
            files.extend(sub_files);
        } else if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            // Include known source file types: .rs, .ex, .exs, .go
            if matches!(ext, "rs" | "ex" | "exs" | "go") {
                files.push(path);
            }
        }
    }

    Ok(files)
}

/// Load ontology from file and compute its BLAKE3 hash.
fn load_ontology_hash(ontology_path: &PathBuf) -> std::result::Result<(String, usize), String> {
    let content =
        fs::read_to_string(ontology_path).map_err(|e| format!("Failed to read ontology: {}", e))?;

    // Compute BLAKE3 hash of the ontology content.
    let mut hasher = blake3::Hasher::new();
    hasher.update(content.as_bytes());
    let hash = hasher.finalize().to_hex().to_string();

    // Count non-empty, non-prefix lines as approximate triple count.
    let triple_count = content
        .lines()
        .filter(|l| !l.trim().is_empty() && !l.starts_with("@prefix"))
        .count();

    Ok((hash, triple_count))
}

/// Execute the inverse sync pipeline and coherence check.
fn do_inverse_sync(
    source_dir: String, ontology: String, signing_key: Option<String>,
    output_envelope: Option<String>,
) -> std::result::Result<InverseSyncOutput, NounVerbError> {
    let source_dir = PathBuf::from(&source_dir);
    let ontology_path = PathBuf::from(&ontology);
    let signing_key_path = resolve_signing_key_path(signing_key);
    let envelope_path = resolve_envelope_path(output_envelope);

    // === Step 1: Collect artifact files ===
    let artifact_files = collect_artifact_files(&source_dir)
        .map_err(|e| NounVerbError::execution_error(format!("Failed to collect artifacts: {}", e)))?;

    if artifact_files.is_empty() {
        return Ok(InverseSyncOutput {
            status: "error".to_string(),
            files_scanned: 0,
            recovered_triples: 0,
            inverse_operation_id: String::new(),
            coherence_admitted: false,
            envelope_path: envelope_path.to_string_lossy().to_string(),
            error: Some("No artifact files found in source directory".to_string()),
            coherence_drifts: None,
        });
    }

    // === Step 2: Load signing key ===
    let signing_key_obj = load_signing_key(&signing_key_path).map_err(|e| {
        NounVerbError::execution_error(format!(
            "Failed to load signing key from {}: {}",
            signing_key_path.display(),
            e
        ))
    })?;

    // === Step 3: Run inverse pipeline μ⁻¹₁–μ⁻¹₅ ===
    let inverse_receipt = InversePipeline::run_signed(&artifact_files, &signing_key_obj)
        .map_err(|e| {
            NounVerbError::execution_error(format!("Inverse pipeline failed: {}", e))
        })?;

    // === Step 4: Load expected ontology and compute hash ===
    let (ontology_hash, ontology_triple_count) = load_ontology_hash(&ontology_path)
        .map_err(|e| NounVerbError::execution_error(e))?;

    // === Step 5: Emit OCEL pack lifecycle events and compute L pole hash ===
    // For this task, we simulate OCEL events from the inverse receipt metadata.
    let ocel_events = vec![
        format!(
            r#"{{"activity":"inverse-scan","timestamp":"{}","files":{}}}"#,
            chrono::Utc::now().to_rfc3339(),
            artifact_files.len()
        ),
        format!(
            r#"{{"activity":"inverse-extract","timestamp":"{}","triples":{}}}"#,
            chrono::Utc::now().to_rfc3339(),
            inverse_receipt.recovered_triple_count
        ),
        format!(
            r#"{{"activity":"inverse-validate","timestamp":"{}","valid":{}}}"#,
            chrono::Utc::now().to_rfc3339(),
            inverse_receipt.shacl_valid
        ),
    ];

    let ocel_event_refs: Vec<&str> = ocel_events.iter().map(|s| s.as_str()).collect();
    let l_pole = CoherenceChecker::fingerprint_event_log(&ocel_event_refs);

    // === Step 6: Compute A pole hash from recovered files ===
    let artifact_strings: Vec<String> = artifact_files
        .iter()
        .map(|p| p.to_string_lossy().into_owned())
        .collect();
    let artifact_data: Vec<(&str, u64)> = artifact_strings
        .iter()
        .zip(artifact_files.iter())
        .map(|(s, p)| {
            let size = fs::metadata(p).map(|m| m.len()).unwrap_or(0);
            (s.as_str(), size)
        })
        .collect();
    let a_pole = CoherenceChecker::fingerprint_artifacts(&artifact_data);

    // === Step 7: Create O pole from expected ontology hash ===
    let o_pole = PoleState {
        pole: Pole::Ontology,
        hash: ontology_hash.clone(),
        item_count: ontology_triple_count,
        timestamp: chrono::Utc::now(),
    };

    // === Step 8: Check coherence with expectations ===
    let mut expectations = HashMap::new();
    expectations.insert(Pole::Ontology, ontology_hash);

    let a_pole_hash = a_pole.hash.clone();
    let coherence_report = CoherenceChecker::check_with_expectations(&[o_pole, a_pole, l_pole], &expectations);
    let coherence_admitted = coherence_report.admitted;

    let coherence_drifts = if !coherence_report.drifts.is_empty() {
        Some(
            coherence_report
                .drifts
                .iter()
                .map(|d| format!("{:?}: {}", d.kind, d.detail))
                .collect(),
        )
    } else {
        None
    };

    // === Step 9: Create ProvenanceEnvelope ===
    // Note: CoherenceReport from ggen_graph is used for the semantic check,
    // but ProvenanceEnvelope expects a different structure. We convert the ggen_graph
    // CoherenceReport to the envelope's CoherenceReport.
    let mut envelope = ProvenanceEnvelope::from_inverse(inverse_receipt.clone());

    // Build the envelope's CoherenceReport from the ggen_graph coherence check
    let envelope_coherence_report = {
        use ggen_core::receipt::provenance_envelope::CoherenceReport as EnvelopeCoherenceReport;
        EnvelopeCoherenceReport::new(
            Uuid::new_v4().to_string(),
            a_pole_hash,
            inverse_receipt.output_hash.clone(),
            coherence_admitted,
            if coherence_admitted { None } else { coherence_drifts.as_ref().map(|d: &Vec<String>| d.join("; ")) },
        )
    };

    envelope = envelope.add_coherence(envelope_coherence_report);

    // === Step 10: Serialize envelope to JSON ===
    let envelope_json = envelope.to_json().map_err(|e| {
        NounVerbError::execution_error(format!("Failed to serialize envelope: {}", e))
    })?;

    // === Step 11: Write envelope to disk ===
    if let Some(parent) = envelope_path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            NounVerbError::execution_error(format!(
                "Failed to create envelope directory: {}",
                e
            ))
        })?;
    }

    fs::write(&envelope_path, &envelope_json).map_err(|e| {
        NounVerbError::execution_error(format!(
            "Failed to write envelope to {}: {}",
            envelope_path.display(),
            e
        ))
    })?;

    // === Step 12: Return result ===
    let status = if coherence_admitted && inverse_receipt.shacl_valid {
        "success".to_string()
    } else {
        "incoherent".to_string()
    };

    Ok(InverseSyncOutput {
        status,
        files_scanned: artifact_files.len(),
        recovered_triples: inverse_receipt.recovered_triple_count,
        inverse_operation_id: inverse_receipt.operation_id,
        coherence_admitted,
        envelope_path: envelope_path.to_string_lossy().to_string(),
        error: if !coherence_admitted { Some("Coherence check failed".to_string()) } else { None },
        coherence_drifts,
    })
}

// ============================================================================
// Verbs (thin wrappers; complexity ≤ 5 per Poka-Yoke gate FM-1.1)
// ============================================================================

/// Recover RDF ontology from generated artifacts via the μ⁻¹ inverse pipeline.
///
/// Scans artifact files (Rust, Elixir, Go), extracts service definitions,
/// converts to RDF Turtle, validates coherence against an expected ontology,
/// and emits a signed ProvenanceEnvelope.
///
/// Required arguments:
///   --source-dir <PATH>      Directory containing artifact files to recover from
///   --ontology <PATH>        Path to expected RDF ontology (for coherence check)
///
/// Optional arguments:
///   --signing-key <PATH>     Ed25519 signing key (default: .ggen/keys/signing.key)
///   --output-envelope <PATH> Where to save the envelope (default: .ggen/envelopes/latest.json)
///
/// Exit codes:
///   0  Success — coherence check passed, envelope written
///   1  Failure — coherence check failed or pipeline error
#[verb]
pub fn inverse_sync(
    source_dir: String, ontology: String, signing_key: Option<String>,
    output_envelope: Option<String>,
) -> Result<InverseSyncOutput> {
    do_inverse_sync(source_dir, ontology, signing_key, output_envelope)
}
