// rust/knhk-cli/src/commands/cover.rs
// Cover commands - Cover definition over O

use std::fs;
use std::path::PathBuf;
use serde::{Deserialize, Serialize};

/// Cover storage entry
#[derive(Debug, Serialize, Deserialize)]
struct CoverEntry {
    id: String,
    select: String,
    shard: String,
}

/// Cover storage
#[derive(Debug, Serialize, Deserialize)]
struct CoverStorage {
    covers: Vec<CoverEntry>,
}

/// Define cover over O
/// cover(#{select := SelectSpec, shard := ShardSpec})
pub fn define(select: String, shard: String) -> Result<(), String> {
    println!("Defining cover over O...");
    println!("  Select: {}", select);
    println!("  Shard: {}", shard);
    
    // Validate shard spec (max_run_len ≤ 8)
    if shard.contains("max_run_len") {
        // Extract max_run_len value (simplified parsing)
        if let Some(len_str) = shard.split("max_run_len").nth(1) {
            if let Some(len_val) = len_str.split_whitespace().find(|s| s.parse::<u64>().is_ok()) {
                let len = len_val.parse::<u64>()
                    .map_err(|e| format!("Invalid shard length '{}': {}", len_val, e))?;
                
                // Validate guard: len ≤ 8 (Chatman Constant)
                if len > 8 {
                    return Err(format!("Shard max_run_len {} exceeds Chatman Constant (8)", len));
                }
            }
        }
    }
    
    // Load existing covers
    let mut storage = load_covers()?;
    
    // Create new cover entry
    let cover_id = format!("cover_{}", storage.covers.len() + 1);
    storage.covers.push(CoverEntry {
        id: cover_id.clone(),
        select: select.clone(),
        shard: shard.clone(),
    });
    
    // Save covers
    save_covers(&storage)?;
    
    println!("✓ Cover defined (id: {})", cover_id);
    
    Ok(())
}

/// List covers
pub fn list() -> Result<(), String> {
    let storage = load_covers()?;
    
    if storage.covers.is_empty() {
        println!("No covers defined");
        return Ok(());
    }
    
    println!("Defined covers:");
    for (i, cover) in storage.covers.iter().enumerate() {
        println!("  {}. {} (id: {})", i + 1, cover.select, cover.id);
        println!("     Shard: {}", cover.shard);
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

fn load_covers() -> Result<CoverStorage, String> {
    let config_dir = get_config_dir()?;
    let covers_file = config_dir.join("covers.json");
    
    if !covers_file.exists() {
        return Ok(CoverStorage {
            covers: Vec::new(),
        });
    }
    
    let content = fs::read_to_string(&covers_file)
        .map_err(|e| format!("Failed to read covers file: {}", e))?;
    
    let storage: CoverStorage = serde_json::from_str(&content)
        .map_err(|e| format!("Failed to parse covers file: {}", e))?;
    
    Ok(storage)
}

fn save_covers(storage: &CoverStorage) -> Result<(), String> {
    let config_dir = get_config_dir()?;
    fs::create_dir_all(&config_dir)
        .map_err(|e| format!("Failed to create config directory: {}", e))?;
    
    let covers_file = config_dir.join("covers.json");
    let content = serde_json::to_string_pretty(storage)
        .map_err(|e| format!("Failed to serialize covers: {}", e))?;
    
    fs::write(&covers_file, content)
        .map_err(|e| format!("Failed to write covers file: {}", e))?;
    
    Ok(())
}

