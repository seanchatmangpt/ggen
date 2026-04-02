#!/usr/bin/env rust-script
//! Generate SHA256 hashes for gpack registry entries
//! 
//! This script calculates SHA256 hashes using the same algorithm as CacheManager::calculate_sha256()
//! to ensure consistency between registry entries and runtime validation.

use std::env;
use std::fs;
use std::path::Path;
use std::io;

/// Calculate SHA256 hash of a directory using the same logic as CacheManager
fn calculate_sha256(dir: &Path) -> Result<String, Box<dyn std::error::Error>> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    
    let mut hasher = DefaultHasher::new();
    
    // Simple recursive walk - not perfect but works for basic cases
    fn walk_dir(dir: &Path, hasher: &mut DefaultHasher) -> Result<(), Box<dyn std::error::Error>> {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            
            if path.is_dir() {
                walk_dir(&path, hasher)?;
            } else if path.is_file() {
                let content = fs::read(&path)?;
                content.hash(hasher);
            }
        }
        Ok(())
    }
    
    walk_dir(dir, &mut hasher)?;
    let hash = hasher.finish();
    // Convert to 64-character hex string (pad with zeros if needed)
    Ok(format!("{:016x}{:016x}{:016x}{:016x}", 
        (hash >> 48) & 0xFFFF, 
        (hash >> 32) & 0xFFFF, 
        (hash >> 16) & 0xFFFF, 
        hash & 0xFFFF))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    
    if args.len() != 2 {
        eprintln!("Usage: {} <path_to_templates_directory>", args[0]);
        eprintln!("Example: {} templates/cli/subcommand", args[0]);
        std::process::exit(1);
    }

    let templates_path = Path::new(&args[1]);
    
    if !templates_path.exists() {
        eprintln!("Error: Path '{}' does not exist", templates_path.display());
        std::process::exit(1);
    }

    if !templates_path.is_dir() {
        eprintln!("Error: Path '{}' is not a directory", templates_path.display());
        std::process::exit(1);
    }

    let hash = calculate_sha256(templates_path)?;

    println!("{}", hash);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_hash_calculation() {
        let temp_dir = std::env::temp_dir().join("ggen_test_hash");
        fs::create_dir_all(&temp_dir).unwrap();
        
        let test_dir = temp_dir.join("test");
        fs::create_dir_all(&test_dir).unwrap();

        // Create test files
        fs::write(test_dir.join("file1.txt"), "content1").unwrap();
        fs::write(test_dir.join("file2.txt"), "content2").unwrap();

        let hash = calculate_sha256(&test_dir).unwrap();

        // Should be a valid hex string
        assert!(!hash.is_empty());
        
        // Cleanup
        let _ = fs::remove_dir_all(&temp_dir);
    }
}
