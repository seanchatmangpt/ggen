use anyhow::Result;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

use rgen::mock_registry::MockGitHubRegistry;

/// Demo function that simulates the complete marketplace workflow
pub fn run_marketplace_demo() -> Result<()> {
    println!("🚀 Rgen Marketplace Demo");
    println!("========================");
    
    // Setup: Create a temporary project directory
    let project_dir = TempDir::new()?;
    let project_path = project_dir.path();
    println!("📁 Created project directory: {}", project_path.display());
    
    // Setup: Create mock GitHub registry (simulating registry.rgen.dev)
    let mock_registry = MockGitHubRegistry::new()?;
    println!("🌐 Created mock GitHub registry with index.json");
    
    // Step 1: Search for rpacks
    println!("\n🔍 Step 1: Searching for 'rust' rpacks...");
    let search_results = simulate_search(&mock_registry, "rust")?;
    for result in &search_results {
        println!("   Found: {}", result);
    }
    
    // Step 2: Add an rpack
    println!("\n📦 Step 2: Adding rpack...");
    let rpack_id = "io.rgen.rust.cli-subcommand";
    let rpack_version = "0.2.0";
    
    // Create mock rpack repository (simulating GitHub repo)
    let rpack_dir = mock_registry.create_mock_rpack(rpack_id, rpack_version)?;
    println!("   Created mock rpack repository: {}", rpack_dir.display());
    
    // Simulate downloading and caching the rpack
    let cache_dir = project_path.join(".rgen").join("rpacks");
    fs::create_dir_all(&cache_dir)?;
    
    let cached_rpack_dir = cache_dir.join(rpack_id).join(rpack_version);
    fs::create_dir_all(&cached_rpack_dir)?;
    copy_dir_all(&rpack_dir, &cached_rpack_dir)?;
    println!("   Cached rpack to: {}", cached_rpack_dir.display());
    
    // Step 3: Create/update lockfile
    println!("\n🔒 Step 3: Creating lockfile...");
    let lockfile_path = project_path.join("rgen.lock");
    let lockfile_content = format!(
        r#"[[pack]]
id = "{}"
version = "{}"
sha256 = "mock_sha256_hash"
source = "https://github.com/mock/rpack-rust-cli.git#def456ghi789"
"#,
        rpack_id, rpack_version
    );
    fs::write(&lockfile_path, lockfile_content)?;
    println!("   Created lockfile: {}", lockfile_path.display());
    
    // Step 4: List installed rpacks
    println!("\n📋 Step 4: Listing installed rpacks...");
    let installed_packs = simulate_list_packs(&lockfile_path)?;
    for pack in &installed_packs {
        println!("   Installed: {}@{}", pack.id, pack.version);
    }
    
    // Step 5: Show available templates
    println!("\n📄 Step 5: Available templates...");
    let template_path = cached_rpack_dir.join("templates").join("main.tmpl");
    if template_path.exists() {
        let template_content = fs::read_to_string(&template_path)?;
        let lines: Vec<&str> = template_content.lines().take(10).collect();
        println!("   Template: {}:main.tmpl", rpack_id);
        for line in lines {
            println!("     {}", line);
        }
        println!("     ...");
    }
    
    // Step 6: Simulate template generation
    println!("\n⚡ Step 6: Simulating template generation...");
    println!("   Command: rgen gen {}:main.tmpl name=MyApp", rpack_id);
    println!("   Would generate: output/myapp.rs");
    
    // Step 7: Update rpacks
    println!("\n🔄 Step 7: Checking for updates...");
    println!("   {}@{} is up to date", rpack_id, rpack_version);
    
    // Step 8: Remove rpack
    println!("\n🗑️  Step 8: Removing rpack...");
    simulate_remove_pack(&lockfile_path, rpack_id)?;
    let remaining_packs = simulate_list_packs(&lockfile_path)?;
    println!("   Remaining rpacks: {}", remaining_packs.len());
    
    println!("\n✅ Marketplace demo completed successfully!");
    println!("   Project directory: {}", project_path.display());
    println!("   Registry index: {}", mock_registry.index_path().display());
    
    Ok(())
}

/// Simulate searching for rpacks in the registry
fn simulate_search(registry: &MockGitHubRegistry, query: &str) -> Result<Vec<String>> {
    let index_content = registry.index_content()?;
    let data: serde_json::Value = serde_json::from_str(&index_content)?;
    
    let mut results = Vec::new();
    if let Some(rpacks) = data["rpacks"].as_array() {
        for pack in rpacks {
            let id = pack["id"].as_str().unwrap_or("");
            let name = pack["name"].as_str().unwrap_or("");
            let description = pack["description"].as_str().unwrap_or("");
            let tags = pack["tags"].as_array()
                .map(|tags| tags.iter()
                    .filter_map(|tag| tag.as_str())
                    .collect::<Vec<_>>()
                    .join(", "))
                .unwrap_or_default();
            
            // Simple search logic
            if id.to_lowercase().contains(&query.to_lowercase()) ||
               name.to_lowercase().contains(&query.to_lowercase()) ||
               description.to_lowercase().contains(&query.to_lowercase()) ||
               tags.to_lowercase().contains(&query.to_lowercase()) {
                results.push(id.to_string());
            }
        }
    }
    
    Ok(results)
}

/// Simulate listing installed rpacks from lockfile
fn simulate_list_packs(lockfile_path: &PathBuf) -> Result<Vec<InstalledPack>> {
    if !lockfile_path.exists() {
        return Ok(Vec::new());
    }
    
    let content = fs::read_to_string(lockfile_path)?;
    let mut packs = Vec::new();
    
    // Simple TOML parsing for demo
    let mut current_id = String::new();
    let mut current_version = String::new();
    
    for line in content.lines() {
        if line.starts_with("id = ") {
            current_id = line.trim_start_matches("id = \"").trim_end_matches("\"").to_string();
        } else if line.starts_with("version = ") {
            current_version = line.trim_start_matches("version = \"").trim_end_matches("\"").to_string();
            if !current_id.is_empty() {
                packs.push(InstalledPack {
                    id: current_id.clone(),
                    version: current_version.clone(),
                });
                current_id.clear();
                current_version.clear();
            }
        }
    }
    
    Ok(packs)
}

/// Simulate removing a pack from lockfile
fn simulate_remove_pack(lockfile_path: &PathBuf, pack_id: &str) -> Result<()> {
    if !lockfile_path.exists() {
        return Ok(());
    }
    
    let content = fs::read_to_string(lockfile_path)?;
    let mut lines: Vec<&str> = content.lines().collect();
    let mut new_lines = Vec::new();
    let mut skip_pack = false;
    
    for line in lines {
        if line.starts_with("[[pack]]") {
            skip_pack = false;
        } else if line.starts_with(&format!("id = \"{}\"", pack_id)) {
            skip_pack = true;
            continue;
        }
        
        if !skip_pack {
            new_lines.push(line);
        }
    }
    
    let new_content = new_lines.join("\n");
    fs::write(lockfile_path, new_content)?;
    
    Ok(())
}

/// Helper struct for installed pack info
#[derive(Debug, PartialEq)]
struct InstalledPack {
    id: String,
    version: String,
}

/// Helper function to copy directory recursively
fn copy_dir_all(src: &PathBuf, dst: &PathBuf) -> Result<()> {
    fs::create_dir_all(dst)?;
    
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        
        if ty.is_dir() {
            copy_dir_all(&entry.path(), &dst.join(entry.file_name()))?;
        } else {
            fs::copy(entry.path(), dst.join(entry.file_name()))?;
        }
    }
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marketplace_demo() {
        run_marketplace_demo().unwrap();
    }
}
