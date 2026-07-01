//! Authoritative pack-install provenance receipt emission.
//!
//! This is the single, root-parameterized implementation of pack-install
//! receipt signing. It supersedes the cwd-implicit copy that previously lived in
//! `ggen-cli` (`crates/ggen-cli/src/cmds/packs_receipt.rs`), which now delegates
//! here. Consolidating the path means there is exactly one place that decides
//! what a lawful pack receipt looks like — no drift between the CLI surface and
//! the agent surface.
//!
//! Fail-closed by contract: a receipt is emitted only for an install that pinned
//! a non-empty digest (lockfile invariant 4.1). An empty digest means nothing
//! durable was installed, so there is nothing lawful to witness and emission is
//! refused.

use ed25519_dalek::{SecretKey, SigningKey, VerifyingKey};
use std::fs;
use std::path::{Path, PathBuf};

/// Error type for pack receipt operations.
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

/// Result type for pack receipt operations.
pub type Result<T> = std::result::Result<T, PackReceiptError>;

/// The witnessed closure of a pack installation.
///
/// This is the `O*` of `A = μ(O*)` for a pack install: every input capable of
/// determining what was installed, plus the durable artifacts that resulted.
/// [`emit_install_receipt`] binds ALL of these into the signed receipt so the
/// receipt is a faithful proof object, not a decorative `hash(pack_id)`.
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
    /// Absolute paths of durable artifacts produced by the install (e.g. the
    /// install dir and the lockfile). Their contents are hashed into
    /// `output_hashes`.
    pub artifact_paths: &'a [PathBuf],
}

/// Generates a cryptographic receipt for a SUCCESSFUL pack installation, rooted
/// at `root` (the project directory whose `.ggen/` holds receipts and keys).
///
/// Fail-closed by contract: only invoked after an install succeeded and a
/// non-empty lockfile digest exists. A FAILED install must NOT call this —
/// emitting a receipt for work that did not happen is fail-open contract drift.
///
/// Closure binding:
/// - `input_hashes` binds the actuator identity, the pack identity+version, the
///   pack digest, and each declared package — the full pack closure.
/// - `output_hashes` binds the real installed artifacts by hashing their on-disk
///   contents (install dir manifest, lockfile), not the status string.
///
/// Returns an error (and writes nothing) if the digest is empty, mirroring the
/// lockfile invariant: no digest means nothing was durably pinned, so there is
/// nothing lawful to witness.
pub fn emit_install_receipt(root: &Path, closure: &PackInstallClosure<'_>) -> Result<PathBuf> {
    use crate::receipt::{hash_data, Receipt};

    // Refuse to witness an install that pinned no digest — that is not a lawful,
    // completed install (lockfile invariant 4.1).
    if closure.pack_digest.trim().is_empty() {
        return Err(PackReceiptError::Runtime(format!(
            "Refusing to emit receipt for '{}': empty pack digest (no durable install to witness)",
            closure.pack_id
        )));
    }

    let receipts_dir = root.join(".ggen").join("receipts");
    let keys_dir = root.join(".ggen").join("keys");

    fs::create_dir_all(&receipts_dir).map_err(|e| {
        PackReceiptError::Runtime(format!("Failed to create receipts directory: {}", e))
    })?;
    fs::create_dir_all(&keys_dir).map_err(|e| {
        PackReceiptError::Runtime(format!("Failed to create keys directory: {}", e))
    })?;

    // Load or generate the Ed25519 keypair under the project root.
    let private_key_path = keys_dir.join("private.pem");
    let public_key_path = keys_dir.join("public.pem");

    let (signing_key, _verifying_key) = if private_key_path.exists() {
        load_keypair(&private_key_path)?
    } else {
        let (signing_key, verifying_key) = crate::receipt::generate_keypair();
        save_keypair(
            &signing_key,
            &verifying_key,
            &private_key_path,
            &public_key_path,
        )?;
        (signing_key, verifying_key)
    };

    let timestamp = chrono::Utc::now();
    let operation_id = format!(
        "pack-install-{}-{}",
        closure.pack_id,
        timestamp.format("%Y%m%d-%H%M%S")
    );

    // input_hashes: bind the FULL pack closure (O*).
    let mut input_hashes: Vec<String> = Vec::new();
    input_hashes.push(format!(
        "actuator:ggen-pack-install@{}",
        env!("CARGO_PKG_VERSION")
    ));
    input_hashes.push(format!(
        "pack:{}@{}:{}",
        closure.pack_id, closure.pack_version, closure.pack_digest
    ));
    for package in closure.packages_installed {
        input_hashes.push(format!("package:{}", package));
    }

    // output_hashes: bind the REAL installed artifacts by hashing on-disk
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

    let receipt = Receipt::new(operation_id, input_hashes, output_hashes, None)
        .sign(&signing_key)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to sign receipt: {}", e)))?;

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
/// sizes (sorted) so the install directory contributes real, stable evidence.
/// For a file, returns its raw bytes. Returns `None` if the path cannot be read.
fn read_artifact_bytes(path: &Path) -> Option<Vec<u8>> {
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

/// Loads an existing Ed25519 keypair from the hex-encoded private key file.
fn load_keypair(private_key_path: &Path) -> Result<(SigningKey, VerifyingKey)> {
    use ed25519_dalek::SECRET_KEY_LENGTH;

    let private_key_raw = fs::read(private_key_path)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to read private key: {}", e)))?;
    let private_key_hex = std::str::from_utf8(&private_key_raw)
        .map_err(|_| PackReceiptError::Runtime("Private key file is not valid UTF-8".to_string()))?
        .trim();
    let private_key_bytes = hex::decode(private_key_hex).map_err(|e| {
        PackReceiptError::Runtime(format!("Failed to hex-decode private key: {}", e))
    })?;

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

/// Saves an Ed25519 keypair as hex-encoded files.
fn save_keypair(
    signing_key: &SigningKey, verifying_key: &VerifyingKey, private_key_path: &Path,
    public_key_path: &Path,
) -> Result<()> {
    let private_key_hex = hex::encode(signing_key.to_bytes());
    fs::write(private_key_path, private_key_hex)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to write private key: {}", e)))?;

    let public_key_hex = hex::encode(verifying_key.to_bytes());
    fs::write(public_key_path, public_key_hex)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to write public key: {}", e)))?;

    Ok(())
}

/// Verifies a provenance receipt at `receipt_path` against the public key stored
/// under `root/.ggen/keys/public.pem`.
///
/// Returns `Ok((is_valid, operation_id, reason))`. This is fail-closed: a
/// missing key, an unreadable/garbled receipt, or an empty signature all yield
/// `is_valid == false` with a reason — never a spurious `true`.
pub fn verify_install_receipt(
    root: &Path, receipt_path: &Path,
) -> (bool, Option<String>, Option<String>) {
    use crate::receipt::Receipt;

    let receipt_bytes = match fs::read(receipt_path) {
        Ok(b) => b,
        Err(e) => return (false, None, Some(format!("cannot read receipt: {}", e))),
    };

    let receipt: Receipt = match serde_json::from_slice(&receipt_bytes) {
        Ok(r) => r,
        Err(e) => return (false, None, Some(format!("malformed receipt: {}", e))),
    };
    let operation_id = Some(receipt.operation_id.clone());

    if receipt.signature.trim().is_empty() {
        return (false, operation_id, Some("empty signature".to_string()));
    }

    let public_key_path = root.join(".ggen").join("keys").join("public.pem");
    let verifying_key = match load_verifying_key(&public_key_path) {
        Ok(k) => k,
        Err(e) => return (false, operation_id, Some(e)),
    };

    match receipt.verify(&verifying_key) {
        Ok(()) => (true, operation_id, None),
        Err(e) => (
            false,
            operation_id,
            Some(format!("signature invalid: {}", e)),
        ),
    }
}

/// Loads the hex-encoded verifying key written by [`save_keypair`].
fn load_verifying_key(public_key_path: &Path) -> std::result::Result<VerifyingKey, String> {
    let raw = fs::read(public_key_path).map_err(|e| {
        format!(
            "cannot read public key {}: {}",
            public_key_path.display(),
            e
        )
    })?;
    let hex_str = std::str::from_utf8(&raw)
        .map_err(|_| "public key file is not valid UTF-8".to_string())?
        .trim();
    let bytes = hex::decode(hex_str).map_err(|e| format!("cannot hex-decode public key: {}", e))?;
    let arr: [u8; 32] = bytes
        .as_slice()
        .try_into()
        .map_err(|_| "public key is not 32 bytes".to_string())?;
    VerifyingKey::from_bytes(&arr).map_err(|e| format!("invalid public key: {}", e))
}
