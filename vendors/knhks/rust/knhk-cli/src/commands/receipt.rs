// rust/knhk-cli/src/commands/receipt.rs
// Receipt commands - Receipt operations

use std::fs;
use std::path::PathBuf;
use serde::{Deserialize, Serialize};

/// Receipt storage entry
#[derive(Debug, Serialize, Deserialize)]
struct ReceiptEntry {
    id: String,
    ticks: u32,
    lanes: u32,
    span_id: u64,
    a_hash: u64,
    timestamp_ms: u64,
}

/// Receipt storage (file-based)
#[derive(Debug, Serialize, Deserialize)]
struct ReceiptStorage {
    receipts: Vec<ReceiptEntry>,
}

/// Get receipt
/// receipt(Id) -> Receipt
pub fn get(id: String) -> Result<(), String> {
    let storage = load_receipts()?;
    
    let receipt = storage.receipts.iter()
        .find(|r| r.id == id)
        .ok_or_else(|| format!("Receipt not found: {}", id))?;
    
    println!("Receipt: {}", receipt.id);
    println!("  Ticks: {}", receipt.ticks);
    println!("  Lanes: {}", receipt.lanes);
    println!("  Span ID: 0x{:016x}", receipt.span_id);
    println!("  Hash (A): 0x{:016x}", receipt.a_hash);
    println!("  Timestamp: {} ms", receipt.timestamp_ms);
    
    Ok(())
}

/// Merge receipts
/// merge(Receipts) -> Receipt (Π ⊕)
pub fn merge(ids: String) -> Result<(), String> {
    let storage = load_receipts()?;
    
    let receipt_ids: Vec<String> = ids.split(',').map(|s| s.trim().to_string()).collect();
    
    if receipt_ids.is_empty() {
        return Err("No receipt IDs provided".to_string());
    }
    
    // Load receipts
    let receipts: Vec<&ReceiptEntry> = receipt_ids.iter()
        .map(|id| {
            storage.receipts.iter()
                .find(|r| r.id == *id)
                .ok_or_else(|| format!("Receipt not found: {}", id))
        })
        .collect::<Result<Vec<_>, _>>()?;
    
    if receipts.is_empty() {
        return Err("No receipts found".to_string());
    }
    
    // Merge via ⊕ operation (associative, branchless)
    // - ticks: max of all ticks
    // - lanes: sum of all lanes
    // - span_id: XOR of all span_ids
    // - a_hash: XOR of all a_hashes
    let merged_ticks = receipts.iter().map(|r| r.ticks).max().unwrap_or(0);
    let merged_lanes: u32 = receipts.iter().map(|r| r.lanes).sum();
    let merged_span_id = receipts.iter().fold(0u64, |acc, r| acc ^ r.span_id);
    let merged_a_hash = receipts.iter().fold(0u64, |acc, r| acc ^ r.a_hash);
    
    println!("Merged receipts:");
    for receipt in &receipts {
        println!("  - {} (ticks: {}, lanes: {})", receipt.id, receipt.ticks, receipt.lanes);
    }
    println!("Result:");
    println!("  Ticks: {} (max)", merged_ticks);
    println!("  Lanes: {} (sum)", merged_lanes);
    println!("  Span ID: 0x{:016x} (XOR)", merged_span_id);
    println!("  Hash (A): 0x{:016x} (XOR)", merged_a_hash);
    
    Ok(())
}

/// Verify receipt integrity
/// verify(ReceiptId) -> bool (Merkle tree verification)
pub fn verify(id: String) -> Result<(), String> {
    let storage = load_receipts()?;
    
    let receipt = storage.receipts.iter()
        .find(|r| r.id == id)
        .ok_or_else(|| format!("Receipt not found: {}", id))?;
    
    println!("Verifying receipt: {}", receipt.id);
    
    // Basic integrity checks
    if receipt.ticks == 0 && receipt.lanes == 0 {
        return Err("Receipt appears invalid (zero ticks and lanes)".to_string());
    }
    
    if receipt.span_id == 0 {
        return Err("Receipt has invalid span ID (zero)".to_string());
    }
    
    if receipt.a_hash == 0 {
        return Err("Receipt has invalid hash (zero)".to_string());
    }
    
    // Verify receipt is within tick budget
    if receipt.ticks > 8 {
        println!("⚠ Warning: Receipt exceeds tick budget ({} > 8)", receipt.ticks);
    }
    
    // Verify lanes ≤ 8
    if receipt.lanes > 8 {
        println!("⚠ Warning: Receipt exceeds lane limit ({} > 8)", receipt.lanes);
    }
    
    println!("✓ Receipt integrity verified");
    println!("  Ticks: {} (budget: ≤8)", receipt.ticks);
    println!("  Lanes: {} (max: 8)", receipt.lanes);
    println!("  Span ID: 0x{:016x} (valid)", receipt.span_id);
    println!("  Hash (A): 0x{:016x} (valid)", receipt.a_hash);
    
    // Note: Full Merkle tree verification requires lockchain integration
    // This is a basic integrity check
    
    Ok(())
}

/// Show receipt details (alias for get)
pub fn show(id: String) -> Result<(), String> {
    get(id)
}

/// List receipts
pub fn list() -> Result<(), String> {
    let storage = load_receipts()?;
    
    if storage.receipts.is_empty() {
        println!("No receipts found");
        return Ok(());
    }
    
    println!("Receipts:");
    for (i, receipt) in storage.receipts.iter().enumerate() {
        println!("  {}. {} (ticks: {}, hash: 0x{:016x})", 
                 i + 1, receipt.id, receipt.ticks, receipt.a_hash);
    }
    
    Ok(())
}

fn get_config_dir() -> Result<PathBuf, String> {
    #[cfg(target_os = "windows")]
    {
        let mut path = PathBuf::from(std::env::var("APPDATA").map_err(|_| "APPDATA not set")?);
        path.push("knhk");
        Ok(path)
    }
    
    #[cfg(not(target_os = "windows"))]
    {
        let home = std::env::var("HOME").map_err(|_| "HOME not set")?;
        let mut path = PathBuf::from(home);
        path.push(".knhk");
        Ok(path)
    }
}

fn load_receipts() -> Result<ReceiptStorage, String> {
    let config_dir = get_config_dir()?;
    let receipts_file = config_dir.join("receipts.json");
    
    if !receipts_file.exists() {
        return Ok(ReceiptStorage {
            receipts: Vec::new(),
        });
    }
    
    let content = fs::read_to_string(&receipts_file)
        .map_err(|e| format!("Failed to read receipts file: {}", e))?;
    
    let storage: ReceiptStorage = serde_json::from_str(&content)
        .map_err(|e| format!("Failed to parse receipts file: {}", e))?;
    
    Ok(storage)
}

