// rust/knhk-cli/src/commands/context.rs
// Context commands - Context management operations

use std::fs;
use std::path::PathBuf;
use serde::{Deserialize, Serialize};

/// Context storage entry
#[derive(Debug, Serialize, Deserialize)]
struct ContextEntry {
    id: String,
    name: String,
    schema_iri: String,
    active: bool,
    created_at_ms: u64,
}

/// Context storage
#[derive(Debug, Serialize, Deserialize)]
struct ContextStorage {
    contexts: Vec<ContextEntry>,
    current_context: Option<String>,
}

/// List all contexts
pub fn list() -> Result<(), String> {
    let storage = load_contexts()?;
    
    if storage.contexts.is_empty() {
        println!("No contexts defined");
        return Ok(());
    }
    
    println!("Contexts:");
    for (i, ctx) in storage.contexts.iter().enumerate() {
        let marker = if ctx.id == storage.current_context.as_ref().unwrap_or(&"".to_string()) {
            "*"
        } else {
            " "
        };
        println!("  {}{}. {} (id: {})", marker, i + 1, ctx.name, ctx.id);
        println!("     Schema: {}", ctx.schema_iri);
    }
    
    Ok(())
}

/// Show current context
pub fn current() -> Result<(), String> {
    let storage = load_contexts()?;
    
    if let Some(ref current_id) = storage.current_context {
        if let Some(ctx) = storage.contexts.iter().find(|c| c.id == *current_id) {
            println!("Current context: {}", ctx.name);
            println!("  ID: {}", ctx.id);
            println!("  Schema: {}", ctx.schema_iri);
        } else {
            println!("Current context ID '{}' not found", current_id);
        }
    } else {
        println!("No current context set");
    }
    
    Ok(())
}

/// Create new context
pub fn create(id: String, name: String, schema_iri: String) -> Result<(), String> {
    let mut storage = load_contexts()?;
    
    // Check if context with same ID already exists
    if storage.contexts.iter().any(|c| c.id == id) {
        return Err(format!("Context '{}' already exists", id));
    }
    
    // Create context entry
    let context_entry = ContextEntry {
        id: id.clone(),
        name: name.clone(),
        schema_iri: schema_iri.clone(),
        active: false,
        created_at_ms: get_current_timestamp_ms(),
    };
    
    storage.contexts.push(context_entry);
    save_contexts(&storage)?;
    
    println!("✓ Context created: {} (id: {})", name, id);
    println!("  Schema: {}", schema_iri);
    
    Ok(())
}

/// Switch to different context
pub fn use_context(id: String) -> Result<(), String> {
    let mut storage = load_contexts()?;
    
    // Verify context exists
    if !storage.contexts.iter().any(|c| c.id == id) {
        return Err(format!("Context '{}' not found", id));
    }
    
    // Deactivate all contexts
    for ctx in &mut storage.contexts {
        ctx.active = false;
    }
    
    // Activate selected context
    if let Some(ctx) = storage.contexts.iter_mut().find(|c| c.id == id) {
        ctx.active = true;
        storage.current_context = Some(id.clone());
    }
    
    save_contexts(&storage)?;
    
    println!("✓ Switched to context: {}", id);
    
    Ok(())
}

fn get_current_timestamp_ms() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
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

fn load_contexts() -> Result<ContextStorage, String> {
    let config_dir = get_config_dir()?;
    let contexts_file = config_dir.join("contexts.json");
    
    if !contexts_file.exists() {
        return Ok(ContextStorage {
            contexts: Vec::new(),
            current_context: None,
        });
    }
    
    let content = fs::read_to_string(&contexts_file)
        .map_err(|e| format!("Failed to read contexts file: {}", e))?;
    
    let storage: ContextStorage = serde_json::from_str(&content)
        .map_err(|e| format!("Failed to parse contexts file: {}", e))?;
    
    Ok(storage)
}

fn save_contexts(storage: &ContextStorage) -> Result<(), String> {
    let config_dir = get_config_dir()?;
    fs::create_dir_all(&config_dir)
        .map_err(|e| format!("Failed to create config directory: {}", e))?;
    
    let contexts_file = config_dir.join("contexts.json");
    let content = serde_json::to_string_pretty(storage)
        .map_err(|e| format!("Failed to serialize contexts: {}", e))?;
    
    fs::write(&contexts_file, content)
        .map_err(|e| format!("Failed to write contexts file: {}", e))?;
    
    Ok(())
}

