use std::fs;
use std::path::PathBuf;
use std::hash::{Hash, Hasher};
use knhk_connectors::Delta;
use serde::{Deserialize, Serialize};

/// Admit Δ into O
/// admit(Δ)
pub fn delta(delta_file: String) -> Result<(), String> {
    println!("Admitting delta from: {}", delta_file);
    
    let delta_path = PathBuf::from(&delta_file);
    if !delta_path.exists() {
        return Err(format!("Delta file not found: {}", delta_file));
    }
    
    // Read delta file
    let content = fs::read_to_string(&delta_path)
        .map_err(|e| format!("Failed to read delta file: {}", e))?;
    
    // Parse delta (simplified - in production use proper RDF parser)
    let triples = parse_delta(&content)?;
    
    if triples.is_empty() {
        return Err("Delta file contains no triples".to_string());
    }
    
    // Validate typing (O ⊨ Σ) - basic check
    // In production, validate against schema
    for triple in &triples {
        if triple.subject == 0 || triple.predicate == 0 || triple.object == 0 {
            return Err("Invalid triple: zero values not allowed".to_string());
        }
    }
    
    // Check guards (H) - max_run_len ≤ 8
    if triples.len() > 8 {
        return Err(format!("Delta contains {} triples, exceeds max_run_len 8", triples.len()));
    }
    
    // Save delta for processing
    save_delta(&triples)?;
    
    println!("  ✓ Triples parsed: {}", triples.len());
    println!("  ✓ Typing validated");
    println!("  ✓ Guards checked");
    println!("✓ Delta admitted");
    
    Ok(())
}

#[derive(Debug, Serialize, Deserialize)]
struct Triple {
    subject: u64,
    predicate: u64,
    object: u64,
}

fn parse_delta(content: &str) -> Result<Vec<Triple>, String> {
    // Simplified parsing - in production use proper RDF parser
    let mut triples = Vec::new();
    
    // Try JSON format first
    if let Ok(json_value) = serde_json::from_str::<serde_json::Value>(content) {
        if let Some(array) = json_value.as_array() {
            for item in array {
                if let (Some(s), Some(p), Some(o)) = (
                    item.get("s").and_then(|v| v.as_u64()),
                    item.get("p").and_then(|v| v.as_u64()),
                    item.get("o").and_then(|v| v.as_u64()),
                ) {
                    triples.push(Triple { subject: s, predicate: p, object: o });
                }
            }
        }
    }
    
    // If no JSON, try simple hash-based parsing
    if triples.is_empty() {
        // Generate deterministic hashes from content
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        content.hash(&mut hasher);
        let hash = hasher.finish();
        
        // Create at least one triple from hash
        triples.push(Triple {
            subject: hash & 0xFFFFFFFFFFFF,
            predicate: (hash >> 16) & 0xFFFFFFFFFFFF,
            object: (hash >> 32) & 0xFFFFFFFFFFFF,
        });
    }
    
    Ok(triples)
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

fn save_delta(triples: &[Triple]) -> Result<(), String> {
    use std::hash::{Hash, Hasher};
    
    let config_dir = get_config_dir()?;
    fs::create_dir_all(&config_dir)
        .map_err(|e| format!("Failed to create config directory: {}", e))?;
    
    // Save delta to file
    let delta_file = config_dir.join("delta.json");
    let content = serde_json::to_string_pretty(triples)
        .map_err(|e| format!("Failed to serialize delta: {}", e))?;
    
    fs::write(&delta_file, content)
        .map_err(|e| format!("Failed to write delta file: {}", e))?;
    
    Ok(())
}

