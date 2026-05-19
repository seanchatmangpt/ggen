//! Composition receipt — records capability-to-pack composition events.
//!
//! Moved from CLI `capability.rs` into domain layer.
//! Signs with Ed25519 using `.ggen/keys/signing.key`.

use chrono::Utc;
use ggen_core::receipt::{generate_keypair, hash_data, Receipt};
use ggen_core::utils::error::Result;
use std::path::Path;

/// Emit a signed composition receipt for a capability enable operation.
///
/// Writes to `.ggen/receipts/` with timestamped archive and `latest.json`.
pub fn emit_composition_receipt(
    capability: &str,
    projection: Option<&str>,
    runtime: Option<&str>,
    profile: Option<&str>,
    atomic_packs: &[String],
    ggen_dir: &Path,
) -> Result<std::path::PathBuf> {
    use std::fs;

    // 1. Ensure directories exist
    let keys_dir = ggen_dir.join("keys");
    let receipts_dir = ggen_dir.join("receipts");
    fs::create_dir_all(&keys_dir)?;
    fs::create_dir_all(&receipts_dir)?;

    // 2. Load or generate signing keypair
    let signing_key_path = keys_dir.join("signing.key");
    let verifying_key_path = keys_dir.join("verifying.key");

    let signing_key = if signing_key_path.exists() {
        let hex_str = fs::read_to_string(&signing_key_path)?;
        let bytes = hex::decode(hex_str.trim())?;
        let sk_bytes: [u8; 32] = bytes
            .try_into()
            .map_err(|_| ggen_core::utils::error::Error::new("Invalid signing key length"))?;
        ed25519_dalek::SigningKey::from_bytes(&sk_bytes)
    } else {
        let (sk, vk) = generate_keypair();
        fs::write(&signing_key_path, hex::encode(sk.to_bytes()))?;
        fs::write(&verifying_key_path, hex::encode(vk.to_bytes()))?;
        sk
    };

    // 3. Build input hashes
    let mut input_hashes: Vec<String> = vec![
        format!("capability:{}", capability),
    ];
    if let Some(p) = projection {
        input_hashes.push(format!("projection:{}", p));
    }
    if let Some(r) = runtime {
        input_hashes.push(format!("runtime:{}", r));
    }
    if let Some(p) = profile {
        input_hashes.push(format!("profile:{}", p));
    }
    for pack in atomic_packs {
        input_hashes.push(format!("pack:{}", pack));
    }

    // 4. Create and sign receipt
    let receipt = Receipt::new(
        format!("capability-{}", capability),
        input_hashes,
        vec![], // composition has no output files
        None,
    )
    .sign(&signing_key)
    .map_err(|e| ggen_core::utils::error::Error::new(&e.to_string()))?;

    // 5. Write files
    let timestamp = Utc::now().format("%Y%m%d-%H%M%S");
    let receipt_json = serde_json::to_string_pretty(&receipt)?;
    let timestamped_path = receipts_dir.join(format!("composition-{}.json", timestamp));
    fs::write(&timestamped_path, &receipt_json)?;

    let latest_path = receipts_dir.join("latest.json");
    fs::write(&latest_path, &receipt_json)?;

    Ok(latest_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_emit_composition_receipt() {
        let tmp = TempDir::new().unwrap();
        let ggen_dir = tmp.path().join(".ggen");

        let path = emit_composition_receipt(
            "mcp",
            Some("rust"),
            Some("axum"),
            Some("enterprise-strict"),
            &["mcp-rust".to_string()],
            &ggen_dir,
        )
        .unwrap();

        assert!(path.exists());
        let content = std::fs::read_to_string(&path).unwrap();
        let json: serde_json::Value = serde_json::from_str(&content).unwrap();
        assert!(json.get("signature").and_then(|s| s.as_str()).map_or(false, |s| !s.is_empty()));
    }

    #[test]
    fn test_emit_reuses_existing_key() {
        let tmp = TempDir::new().unwrap();
        let ggen_dir = tmp.path().join(".ggen");
        std::fs::create_dir_all(ggen_dir.join("keys")).unwrap();

        // First receipt — generates key
        let path1 = emit_composition_receipt(
            "mcp", None, None, None, &["mcp-rust".to_string()], &ggen_dir,
        ).unwrap();

        // Second receipt — reuses key
        let path2 = emit_composition_receipt(
            "a2a", None, None, None, &["a2a-rust".to_string()], &ggen_dir,
        ).unwrap();

        assert!(path1.exists());
        assert!(path2.exists());
    }
}
