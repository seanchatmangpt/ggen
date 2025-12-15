// rust/knhk-cli/src/commands/boot.rs
// Boot commands - Initialize Σ and Q

use std::fs;
use std::path::PathBuf;
use std::io::Write;

/// Initialize Σ and Q
/// boot(#{sigma => SigmaTTL, q => QTTL})
pub fn init(sigma: String, q: String) -> Result<(), String> {
    println!("Initializing system with Σ and Q...");
    
    // Get config directory
    let config_dir = get_config_dir()?;
    fs::create_dir_all(&config_dir)
        .map_err(|e| format!("Failed to create config directory: {}", e))?;
    
    // Load schema file
    let sigma_path = PathBuf::from(&sigma);
    if !sigma_path.exists() {
        return Err(format!("Schema file not found: {}", sigma));
    }
    let sigma_content = fs::read_to_string(&sigma_path)
        .map_err(|e| format!("Failed to read schema file: {}", e))?;
    
    // Load invariants file
    let q_path = PathBuf::from(&q);
    if !q_path.exists() {
        return Err(format!("Invariants file not found: {}", q));
    }
    let q_content = fs::read_to_string(&q_path)
        .map_err(|e| format!("Failed to read invariants file: {}", e))?;
    
    // Validate schema format (basic check - must be non-empty)
    if sigma_content.trim().is_empty() {
        return Err("Schema file is empty".to_string());
    }
    
    // Validate invariants format (basic check - must be non-empty)
    if q_content.trim().is_empty() {
        return Err("Invariants file is empty".to_string());
    }
    
    // Store schema and invariants in config directory
    let sigma_config = config_dir.join("sigma.ttl");
    let q_config = config_dir.join("q.sparql");
    
    fs::write(&sigma_config, &sigma_content)
        .map_err(|e| format!("Failed to write schema config: {}", e))?;
    fs::write(&q_config, &q_content)
        .map_err(|e| format!("Failed to write invariants config: {}", e))?;
    
    // Write initialization marker
    let init_marker = config_dir.join(".initialized");
    fs::write(&init_marker, "initialized")
        .map_err(|e| format!("Failed to write init marker: {}", e))?;
    
    println!("  ✓ Schema loaded: {}", sigma);
    println!("  ✓ Invariants loaded: {}", q);
    println!("  ✓ Config directory: {}", config_dir.display());
    println!("✓ System initialized");
    
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

