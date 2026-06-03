use crate::models::{FileEntry, Receipt};
use anyhow::Result;
use std::path::PathBuf;
use uuid::Uuid;

/// Deterministic aggregate hash over the scanned file set.
///
/// Computes BLAKE3 of each `(path, content-hash)` pair in sorted order. Independent of
/// scan order, so two scans of the same tree produce the same `aggregate_hash` — the
/// stable scan identity a downstream pack receipt binds to.
#[must_use]
pub fn aggregate_hash(files: &[FileEntry]) -> String {
    let mut pairs: Vec<(&str, &str)> = files
        .iter()
        .map(|f| (f.path.as_str(), f.hash.as_str()))
        .collect();
    pairs.sort_unstable();
    let mut hasher = blake3::Hasher::new();
    for (path, hash) in pairs {
        hasher.update(path.as_bytes());
        hasher.update(&[0]);
        hasher.update(hash.as_bytes());
        hasher.update(&[0]);
    }
    hasher.finalize().to_hex().to_string()
}

/// Emit the scan receipt.
///
/// Writes a JSON `scan-receipt.json` (consumed by `ggen lsp emit_pack --from_scan`)
/// carrying the deterministic `aggregate_hash`, plus a TOML copy under `receipts/`
/// for `verify-no-deletion` compatibility.
pub fn generate_receipt(
    files: &[FileEntry], out: &std::path::Path, scan_roots: Vec<String>,
    output_artifacts: Vec<String>,
) -> Result<Receipt> {
    let id = Uuid::new_v4().to_string();
    let total_bytes: u64 = files.iter().map(|f| f.size_bytes).sum();
    let receipt = crate::models::Receipt {
        id,
        timestamp: chrono::Utc::now().to_rfc3339(),
        scan_roots,
        file_count: files.len(),
        total_bytes,
        aggregate_hash: aggregate_hash(files),
        commands_run: vec![],
        output_artifacts,
        files: files.to_vec(),
    };

    std::fs::create_dir_all(out)?;
    // JSON scan receipt — the cross-tool surface ggen reads.
    std::fs::write(
        out.join("scan-receipt.json"),
        serde_json::to_string_pretty(&receipt)?,
    )?;
    // TOML copy retained for verify-no-deletion.
    std::fs::create_dir_all(out.join("receipts"))?;
    let receipt_path = out.join(format!("receipts/scan_{}.toml", receipt.id));
    std::fs::write(&receipt_path, toml::to_string(&receipt)?)?;
    Ok(receipt)
}

pub fn verify_no_deletion(before_path: &PathBuf, after_path: &PathBuf) -> Result<()> {
    let before_content = std::fs::read_to_string(before_path)?;
    let after_content = std::fs::read_to_string(after_path)?;

    let before: Receipt = toml::from_str(&before_content)?;
    let after: Receipt = toml::from_str(&after_content)?;

    let mut missing = Vec::new();
    for bf in before.files {
        if !after.files.iter().any(|af| af.path == bf.path) {
            missing.push(bf.path);
        }
    }

    if missing.is_empty() {
        println!("Verification passed. No files deleted.");
    } else {
        println!("REFUSAL: Files missing in after receipt:");
        for p in missing {
            println!("- {}", p);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{FileEntry, Language};
    use std::time::SystemTime;

    fn fe(path: &str, hash: &str) -> FileEntry {
        FileEntry {
            path: path.to_string(),
            language: Language::from_extension("rs"),
            size_bytes: 1,
            hash: hash.to_string(),
            modified_time: SystemTime::UNIX_EPOCH,
            git_root: None,
            is_test: false,
            is_binary: false,
        }
    }

    #[test]
    fn aggregate_hash_is_order_independent_and_content_sensitive() {
        let a = vec![fe("a.rs", "h1"), fe("b.rs", "h2")];
        let b = vec![fe("b.rs", "h2"), fe("a.rs", "h1")];
        assert_eq!(
            aggregate_hash(&a),
            aggregate_hash(&b),
            "independent of scan order"
        );
        assert!(
            !aggregate_hash(&a).is_empty(),
            "non-empty for a non-empty scan"
        );

        let c = vec![fe("a.rs", "CHANGED"), fe("b.rs", "h2")];
        assert_ne!(
            aggregate_hash(&a),
            aggregate_hash(&c),
            "sensitive to content changes"
        );
    }
}
