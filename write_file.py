import os

receipt_rs = """use std::path::PathBuf;
use anyhow::Result;
use crate::models::{FileEntry, Receipt};
use uuid::Uuid;
use chrono::Utc;
use blake3::Hasher;

pub fn generate_receipt(
    files: &[FileEntry],
    root_paths: Vec<String>,
    directory_count: i64,
    command_run: String,
    output_artifacts: Vec<String>,
    warnings: Vec<String>,
    refusals: Vec<String>,
    out: &PathBuf
) -> Result<Receipt> {
    let id = Uuid::new_v4().to_string();
    let timestamp = Utc::now().to_rfc3339();
    let total_bytes = files.iter().map(|f| f.size).sum();
    
    let mut hasher = Hasher::new();
    for f in files {
        hasher.update(f.hash.as_bytes());
    }
    let aggregate_hash = hasher.finalize().to_string();

    let receipt = Receipt {
        id,
        timestamp,
        root_paths,
        file_count: files.len() as i64,
        directory_count,
        total_bytes,
        hash_algorithm: "blake3".to_string(),
        aggregate_hash,
        catalog_version: "1.0".to_string(),
        command_run,
        output_artifacts,
        warnings,
        refusals,
        files: files.to_vec(),
    };
    
    let receipt_path = out.join(format!("receipt_{}.toml", receipt.id));
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
    let mut unchanged = Vec::new();
    let mut modified = Vec::new();
    let mut added = Vec::new();

    for bf in &before.files {
        if let Some(af) = after.files.iter().find(|a| a.path == bf.path) {
            if af.hash == bf.hash {
                unchanged.push(bf.path.clone());
            } else {
                modified.push(bf.path.clone());
            }
        } else {
            missing.push(bf.path.clone());
        }
    }

    for af in &after.files {
        if !before.files.iter().any(|b| b.path == af.path) {
            added.push(af.path.clone());
        }
    }
    
    println!("UNCHANGED: {}", unchanged.len());
    println!("ADDED: {}", added.len());
    println!("MODIFIED: {}", modified.len());
    println!("MISSING: {}", missing.len());

    if missing.is_empty() {
        println!("Verification passed. No files deleted.");
    } else {
        println!("REFUSAL: Files missing in after receipt:");
        for p in &missing {
            println!("- {}", p);
        }
    }
    
    Ok(())
}
"""

with open('/Users/sac/capability-map/src/receipt.rs', 'w') as f:
    f.write(receipt_rs)
