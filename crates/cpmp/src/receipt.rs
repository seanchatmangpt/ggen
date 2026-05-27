use std::path::PathBuf;
use anyhow::Result;
use crate::models::{FileEntry, Receipt};
use uuid::Uuid;

pub fn generate_receipt(files: &[FileEntry], out: &std::path::Path) -> Result<Receipt> {
    let id = Uuid::new_v4().to_string();
    let total_bytes: u64 = files.iter().map(|f| f.size_bytes).sum();
    let receipt = crate::models::Receipt {
        id,
        timestamp: chrono::Utc::now().to_rfc3339(),
        scan_roots: vec![],
        file_count: files.len(),
        total_bytes,
        aggregate_hash: String::new(),
        commands_run: vec![],
        output_artifacts: vec![],
        files: files.to_vec(),
    };
    
    std::fs::create_dir_all(out.join("receipts"))?;
    let receipt_path = out.join(format!("receipts/scan_{}.toml", receipt.id));
    let toml = toml::to_string(&receipt)?;
    std::fs::write(&receipt_path, toml)?;
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
