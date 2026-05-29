//! Pack Receipt Generation
//!
//! This module provides cryptographic receipt generation for pack operations.

use ed25519_dalek::{SecretKey, SigningKey, VerifyingKey};
use std::fs;
use std::path::PathBuf;

/// Error type for pack receipt operations
#[derive(Debug)]
pub enum PackReceiptError {
    Runtime(String),
}

impl std::fmt::Display for PackReceiptError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PackReceiptError::Runtime(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for PackReceiptError {}

/// Result type for pack receipt operations
pub type Result<T> = std::result::Result<T, PackReceiptError>;

/// The witnessed closure of a pack installation.
///
/// This is the O* of `A = μ(O*)` for a pack install: every input capable of
/// determining what was installed, plus the durable artifacts that resulted.
/// `generate_pack_install_receipt` binds ALL of these into the signed receipt so
/// the receipt is a faithful proof object, not a decorative `hash(pack_id)`.
pub struct PackInstallClosure<'a> {
    /// Pack identifier (e.g. `acme/base`).
    pub pack_id: &'a str,
    /// Resolved pack version recorded in the lockfile.
    pub pack_version: &'a str,
    /// Non-empty SHA-256 (hex) digest pinned in `.ggen/packs.lock`
    /// (`integrity` without the `sha256-` prefix). Binds the pack closure.
    pub pack_digest: &'a str,
    /// Packages declared by the pack and recorded as installed.
    pub packages_installed: &'a [String],
    /// Absolute paths of durable artifacts produced by the install
    /// (e.g. the install dir and the lockfile). Their contents are hashed
    /// into `output_hashes`.
    pub artifact_paths: &'a [PathBuf],
}

/// Generates a cryptographic receipt for a SUCCESSFUL pack installation.
///
/// Fail-closed by contract: this function is only invoked after the install has
/// succeeded and a non-empty lockfile digest exists. A FAILED install must NOT
/// call this function — emitting a receipt for work that did not happen is
/// fail-open contract drift. The caller (`pack add`) enforces this gate.
///
/// Closure binding (input_hashes / output_hashes):
/// - `input_hashes` binds the actuator identity, the pack identity+version, the
///   pack digest, and each declared package — the full pack closure, not just
///   `hash(pack_id)`.
/// - `output_hashes` binds the real installed artifacts by hashing their on-disk
///   contents (install dir manifest, lockfile), not the status string.
///
/// Returns an error (and writes nothing) if the digest is empty, mirroring the
/// lockfile invariant: no digest means nothing was durably pinned, so there is
/// nothing lawful to witness.
pub fn generate_pack_install_receipt(closure: &PackInstallClosure<'_>) -> Result<PathBuf> {
    use ggen_core::receipt::{hash_data, Receipt};

    // Refuse to witness an install that pinned no digest — that is not a lawful,
    // completed install (lockfile invariant 4.1).
    if closure.pack_digest.trim().is_empty() {
        return Err(PackReceiptError::Runtime(format!(
            "Refusing to emit receipt for '{}': empty pack digest (no durable install to witness)",
            closure.pack_id
        )));
    }

    // Create .ggen/receipts and .ggen/keys directories
    let receipts_dir = PathBuf::from(".ggen/receipts");
    let keys_dir = PathBuf::from(".ggen/keys");

    fs::create_dir_all(&receipts_dir).map_err(|e| {
        PackReceiptError::Runtime(format!("Failed to create receipts directory: {}", e))
    })?;

    fs::create_dir_all(&keys_dir).map_err(|e| {
        PackReceiptError::Runtime(format!("Failed to create keys directory: {}", e))
    })?;

    // Load or generate Ed25519 keypair
    let private_key_path = keys_dir.join("private.pem");
    let public_key_path = keys_dir.join("public.pem");

    let (signing_key, _verifying_key) = if private_key_path.exists() {
        // Load existing keypair
        load_or_generate_keypair(&private_key_path, &public_key_path)?
    } else {
        // Generate new keypair
        let (signing_key, verifying_key) = ggen_core::receipt::generate_keypair();

        // Save keys
        save_keypair(
            &signing_key,
            &verifying_key,
            &private_key_path,
            &public_key_path,
        )?;

        (signing_key, verifying_key)
    };

    // Create operation ID with timestamp
    let timestamp = chrono::Utc::now();
    let operation_id = format!(
        "pack-install-{}-{}",
        closure.pack_id,
        timestamp.format("%Y%m%d-%H%M%S")
    );

    // input_hashes: bind the FULL pack closure (O*).
    let mut input_hashes: Vec<String> = Vec::new();
    input_hashes.push(format!(
        "actuator:ggen-pack-add@{}",
        env!("CARGO_PKG_VERSION")
    ));
    input_hashes.push(format!(
        "pack:{}@{}:{}",
        closure.pack_id, closure.pack_version, closure.pack_digest
    ));
    for package in closure.packages_installed {
        input_hashes.push(format!("package:{}", package));
    }

    // output_hashes: bind the REAL installed artifacts by hashing their on-disk
    // contents. A path that cannot be read is recorded as MISSING (honest gap),
    // never silently dropped.
    let mut output_hashes: Vec<String> = Vec::new();
    for path in closure.artifact_paths {
        let display = path.display();
        match read_artifact_bytes(path) {
            Some(bytes) => output_hashes.push(format!("{}:{}", display, hash_data(&bytes))),
            None => output_hashes.push(format!("{}:MISSING", display)),
        }
    }
    // Guarantee a non-empty witnessed output even if no artifact paths were
    // supplied: bind the pinned digest as the output of record.
    if output_hashes.is_empty() {
        output_hashes.push(format!("pack-digest:sha256-{}", closure.pack_digest));
    }

    // Create and sign receipt
    let receipt = Receipt::new(operation_id, input_hashes, output_hashes, None)
        .sign(&signing_key)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to sign receipt: {}", e)))?;

    // Save receipt to file
    let receipt_filename = format!(
        "pack-{}-{}.json",
        closure.pack_id,
        timestamp.format("%Y%m%d-%H%M%S")
    );
    let receipt_path = receipts_dir.join(&receipt_filename);

    let receipt_json = serde_json::to_string_pretty(&receipt)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to serialize receipt: {}", e)))?;

    fs::write(&receipt_path, receipt_json)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to write receipt: {}", e)))?;

    Ok(receipt_path)
}

/// Read an artifact's bytes for hashing.
///
/// For a directory, hashes a deterministic manifest of its entries' names and
/// sizes (sorted) so the install directory contributes real, stable evidence to
/// `output_hashes`. For a file, returns its raw bytes. Returns `None` if the
/// path cannot be read.
fn read_artifact_bytes(path: &std::path::Path) -> Option<Vec<u8>> {
    let meta = fs::metadata(path).ok()?;
    if meta.is_dir() {
        let mut entries: Vec<String> = fs::read_dir(path)
            .ok()?
            .filter_map(|e| e.ok())
            .map(|e| {
                let name = e.file_name().to_string_lossy().into_owned();
                let len = e.metadata().map(|m| m.len()).unwrap_or(0);
                format!("{}:{}", name, len)
            })
            .collect();
        entries.sort();
        Some(entries.join("\n").into_bytes())
    } else {
        fs::read(path).ok()
    }
}

/// Loads existing keypair or generates a new one
fn load_or_generate_keypair(
    private_key_path: &PathBuf, _public_key_path: &PathBuf,
) -> Result<(SigningKey, VerifyingKey)> {
    use ed25519_dalek::SECRET_KEY_LENGTH;

    // Read private key
    let private_key_bytes = fs::read(private_key_path)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to read private key: {}", e)))?;

    if private_key_bytes.len() != SECRET_KEY_LENGTH {
        return Err(PackReceiptError::Runtime(
            "Invalid private key length".to_string(),
        ));
    }

    let secret_key = SecretKey::try_from(private_key_bytes.as_slice())
        .map_err(|_| PackReceiptError::Runtime("Invalid private key".to_string()))?;

    let signing_key: SigningKey = secret_key.into();
    let verifying_key = signing_key.verifying_key();

    Ok((signing_key, verifying_key))
}

/// Saves Ed25519 keypair to PEM files
fn save_keypair(
    signing_key: &SigningKey, verifying_key: &VerifyingKey, private_key_path: &PathBuf,
    public_key_path: &PathBuf,
) -> Result<()> {
    // Save private key (hex-encoded for simplicity)
    let private_key_hex = hex::encode(signing_key.to_bytes());
    fs::write(private_key_path, private_key_hex)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to write private key: {}", e)))?;

    // Save public key (hex-encoded for simplicity)
    let public_key_hex = hex::encode(verifying_key.to_bytes());
    fs::write(public_key_path, public_key_hex)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to write public key: {}", e)))?;

    Ok(())
}
