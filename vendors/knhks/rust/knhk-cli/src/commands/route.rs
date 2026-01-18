// rust/knhk-cli/src/commands/route.rs
// Route commands - Action routing

use std::fs;
use std::path::PathBuf;
use serde::{Deserialize, Serialize};

/// Route storage entry
#[derive(Debug, Serialize, Deserialize)]
struct RouteEntry {
    id: String,
    name: String,
    kind: String,
    target: String,
    encode: Option<String>,
}

/// Route storage
#[derive(Debug, Serialize, Deserialize)]
struct RouteStorage {
    routes: Vec<RouteEntry>,
}

/// Install route
/// route(#{name, kind, target, encode})
pub fn install(name: String, kind: String, target: String) -> Result<(), String> {
    println!("Installing route: {}", name);
    println!("  Kind: {}", kind);
    println!("  Target: {}", target);
    
    // Validate route kind (webhook, kafka, grpc, lockchain)
    let valid_kinds = ["webhook", "kafka", "grpc", "lockchain"];
    if !valid_kinds.iter().any(|&k| k == kind) {
        return Err(format!("Invalid route kind: {}. Must be one of: {:?}", kind, valid_kinds));
    }
    
    // Validate target format based on kind
    match kind.as_str() {
        "webhook" => {
            if !target.starts_with("http://") && !target.starts_with("https://") {
                return Err(format!("Webhook target must be HTTP(S) URL: {}", target));
            }
        }
        "kafka" => {
            if !target.starts_with("kafka://") {
                return Err(format!("Kafka target must start with kafka://: {}", target));
            }
        }
        "grpc" => {
            if !target.starts_with("grpc://") {
                return Err(format!("gRPC target must start with grpc://: {}", target));
            }
        }
        "lockchain" => {
            // Lockchain target is just a path
            if target.is_empty() {
                return Err("Lockchain target cannot be empty".to_string());
            }
        }
        _ => {}
    }
    
    // Load existing routes
    let mut storage = load_routes()?;
    
    // Check if route with same name exists
    if storage.routes.iter().any(|r| r.name == name) {
        return Err(format!("Route with name '{}' already exists", name));
    }
    
    // Create new route entry
    let route_id = format!("route_{}", storage.routes.len() + 1);
    storage.routes.push(RouteEntry {
        id: route_id.clone(),
        name: name.clone(),
        kind: kind.clone(),
        target: target.clone(),
        encode: Some("json".to_string()), // Default encoding
    });
    
    // Save routes
    save_routes(&storage)?;
    
    println!("✓ Route installed (id: {})", route_id);
    
    Ok(())
}

/// List routes
pub fn list() -> Result<(), String> {
    let storage = load_routes()?;
    
    if storage.routes.is_empty() {
        println!("No routes installed");
        return Ok(());
    }
    
    println!("Installed routes:");
    for (i, route) in storage.routes.iter().enumerate() {
        println!("  {}. {} (id: {})", i + 1, route.name, route.id);
        println!("     Kind: {}", route.kind);
        println!("     Target: {}", route.target);
        if let Some(ref encode) = route.encode {
            println!("     Encode: {}", encode);
        }
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

fn load_routes() -> Result<RouteStorage, String> {
    let config_dir = get_config_dir()?;
    let routes_file = config_dir.join("routes.json");
    
    if !routes_file.exists() {
        return Ok(RouteStorage {
            routes: Vec::new(),
        });
    }
    
    let content = fs::read_to_string(&routes_file)
        .map_err(|e| format!("Failed to read routes file: {}", e))?;
    
    let storage: RouteStorage = serde_json::from_str(&content)
        .map_err(|e| format!("Failed to parse routes file: {}", e))?;
    
    Ok(storage)
}

fn save_routes(storage: &RouteStorage) -> Result<(), String> {
    let config_dir = get_config_dir()?;
    fs::create_dir_all(&config_dir)
        .map_err(|e| format!("Failed to create config directory: {}", e))?;
    
    let routes_file = config_dir.join("routes.json");
    let content = serde_json::to_string_pretty(storage)
        .map_err(|e| format!("Failed to serialize routes: {}", e))?;
    
    fs::write(&routes_file, content)
        .map_err(|e| format!("Failed to write routes file: {}", e))?;
    
    Ok(())
}

