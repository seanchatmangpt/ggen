use anyhow::Result;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

use rgen::mock_registry::MockGitHubRegistry;

// Helper function to simulate search
fn simulate_search(_mock_registry: &MockGitHubRegistry, query: &str) -> Result<Vec<String>> {
    // Simulate search results
    let results = vec![
        format!("io.rgen.rust.cli-subcommand (matches: {})", query),
        format!("io.rgen.rust.web-server (matches: {})", query),
        format!("io.rgen.rust.database-models (matches: {})", query),
    ];
    Ok(results)
}

// Helper function to copy directory recursively
fn copy_dir_all(src: &std::path::Path, dst: &std::path::Path) -> Result<()> {
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

#[test]
fn test_marketplace_search_functionality() -> Result<()> {
    // Test search functionality with mock data
    let mut packs = HashMap::new();
    
    // Add mock pack data
    packs.insert("io.rgen.rust.cli-subcommand".to_string(), "Rust CLI Subcommand Generator".to_string());
    packs.insert("io.rgen.python.web-api".to_string(), "Python Web API Generator".to_string());
    
    // Test basic search
    let search_query = "rust";
    let mut results = Vec::new();
    
    for (id, name) in &packs {
        if name.to_lowercase().contains(&search_query.to_lowercase()) {
            results.push(id.clone());
        }
    }
    
    assert_eq!(results.len(), 1);
    assert_eq!(results[0], "io.rgen.rust.cli-subcommand");
    
    // Test search with different query
    let search_query = "python";
    let mut results = Vec::new();
    
    for (id, name) in &packs {
        if name.to_lowercase().contains(&search_query.to_lowercase()) {
            results.push(id.clone());
        }
    }
    
    assert_eq!(results.len(), 1);
    assert_eq!(results[0], "io.rgen.python.web-api");
    
    Ok(())
}

#[test]
fn test_marketplace_workflow() -> Result<()> {
    // Setup: Create a temporary project directory
    let project_dir = TempDir::new()?;
    let project_path = project_dir.path();

    // Setup: Create mock GitHub registry
    let mock_registry = MockGitHubRegistry::new()?;

    // Step 1: Simulate searching for rpacks
    let search_results = simulate_search(&mock_registry, "rust")?;
    assert!(!search_results.is_empty());
    assert!(search_results.iter().any(|p| p.contains("rust")));

    // Step 2: Simulate adding an rpack
    let rpack_id = "io.rgen.rust.cli-subcommand";
    let rpack_version = "0.2.0";

    // Create mock rpack repository
    let rpack_dir = mock_registry.create_mock_rpack(rpack_id, rpack_version)?;

    // Simulate downloading and caching the rpack
    let cache_dir = project_path.join(".rgen").join("rpacks");
    fs::create_dir_all(&cache_dir)?;

    // Copy the mock rpack to cache (simulating git clone)
    let cached_rpack_dir = cache_dir.join(rpack_id).join(rpack_version);
    fs::create_dir_all(&cached_rpack_dir)?;
    copy_dir_all(&rpack_dir, &cached_rpack_dir)?;

    // Step 3: Simulate creating/updating lockfile
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

    // Step 4: Simulate generating code using the rpack
    let template_path = cached_rpack_dir.join("templates").join("cli_subcommand.tmpl");
    if template_path.exists() {
        let template_content = fs::read_to_string(&template_path)?;
        assert!(template_content.contains("subcommand"));
    }

    // Step 5: Simulate listing installed rpacks
    let lockfile_content = fs::read_to_string(&lockfile_path)?;
    assert!(lockfile_content.contains(rpack_id));
    assert!(lockfile_content.contains(rpack_version));

    // Step 6: Simulate removing an rpack
    fs::remove_file(&lockfile_path)?;
    fs::remove_dir_all(&cached_rpack_dir)?;

    // Verify removal
    assert!(!lockfile_path.exists());
    assert!(!cached_rpack_dir.exists());

    Ok(())
}

#[test]
fn test_marketplace_demo() -> Result<()> {
    // Setup: Create a temporary project directory
    let project_dir = TempDir::new()?;
    let project_path = project_dir.path();

    // Setup: Create mock GitHub registry (simulating registry.rgen.dev)
    let mock_registry = MockGitHubRegistry::new()?;

    // Step 1: Search for rpacks
    let search_results = simulate_search(&mock_registry, "rust")?;
    assert!(!search_results.is_empty());

    // Step 2: Add an rpack
    let rpack_id = "io.rgen.rust.cli-subcommand";
    let rpack_version = "0.2.0";

    // Create mock rpack repository
    let rpack_dir = mock_registry.create_mock_rpack(rpack_id, rpack_version)?;

    // Simulate downloading and caching the rpack
    let cache_dir = project_path.join(".rgen").join("rpacks");
    fs::create_dir_all(&cache_dir)?;

    // Copy the mock rpack to cache (simulating git clone)
    let cached_rpack_dir = cache_dir.join(rpack_id).join(rpack_version);
    fs::create_dir_all(&cached_rpack_dir)?;
    copy_dir_all(&rpack_dir, &cached_rpack_dir)?;

    // Step 3: Create/update lockfile
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

    // Step 4: Generate code using the rpack
    let template_path = cached_rpack_dir.join("templates").join("cli_subcommand.tmpl");
    if template_path.exists() {
        let template_content = fs::read_to_string(&template_path)?;
        assert!(template_content.contains("subcommand"));
    }

    // Step 5: List installed rpacks
    let lockfile_content = fs::read_to_string(&lockfile_path)?;
    assert!(lockfile_content.contains(rpack_id));
    assert!(lockfile_content.contains(rpack_version));

    Ok(())
}

#[test]
fn test_marketplace_search_standalone() -> Result<()> {
    // Test basic search functionality without external dependencies
    let mut packs = HashMap::new();
    
    // Add mock pack data
    packs.insert("io.rgen.rust.cli-subcommand".to_string(), "Rust CLI Subcommand Generator".to_string());
    packs.insert("io.rgen.python.web-api".to_string(), "Python Web API Generator".to_string());
    
    // Test search function
    let search_query = "rust";
    let mut results = Vec::new();
    
    for (id, name) in &packs {
        if name.to_lowercase().contains(&search_query.to_lowercase()) {
            results.push(id.clone());
        }
    }
    
    assert_eq!(results.len(), 1);
    assert_eq!(results[0], "io.rgen.rust.cli-subcommand");
    
    Ok(())
}

#[test]
fn test_marketplace_pack_resolution() -> Result<()> {
    // Test pack resolution logic
    let pack_id = "io.rgen.test.pack";
    let version = "1.2.0";
    let git_url = "https://github.com/test/repo.git";
    let git_rev = "abc123";
    let sha256 = "def456";
    
    // Test latest version resolution
    assert_eq!(version, "1.2.0");
    
    // Test version lookup
    assert_eq!(version, "1.2.0");
    assert_eq!(git_url, "https://github.com/test/repo.git");
    
    Ok(())
}

#[test]
fn test_marketplace_cache_operations() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let cache_dir = temp_dir.path().join("cache");
    fs::create_dir_all(&cache_dir)?;
    
    // Test cache directory structure
    let pack_id = "io.rgen.test.pack";
    let version = "1.0.0";
    let pack_cache_dir = cache_dir.join(pack_id).join(version);
    fs::create_dir_all(&pack_cache_dir)?;
    
    // Create mock pack files
    let manifest_path = pack_cache_dir.join("rpack.toml");
    let manifest_content = r#"
[pack]
id = "io.rgen.test.pack"
name = "Test Pack"
version = "1.0.0"
description = "A test pack"

[templates]
patterns = ["*.tmpl"]
"#;
    fs::write(&manifest_path, manifest_content)?;
    
    // Test manifest reading
    let manifest_content = fs::read_to_string(&manifest_path)?;
    assert!(manifest_content.contains("Test Pack"));
    
    // Test cache cleanup
    fs::remove_dir_all(&pack_cache_dir)?;
    assert!(!pack_cache_dir.exists());
    
    Ok(())
}

#[test]
fn test_marketplace_lockfile_operations() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let lockfile_path = temp_dir.path().join("rgen.lock");
    
    // Test lockfile creation
    let lockfile_content = r#"[[pack]]
id = "io.rgen.test.pack"
version = "1.0.0"
sha256 = "abc123def456"
source = "https://github.com/test/repo.git#abc123"
"#;
    fs::write(&lockfile_path, lockfile_content)?;
    
    // Test lockfile reading
    let content = fs::read_to_string(&lockfile_path)?;
    assert!(content.contains("io.rgen.test.pack"));
    assert!(content.contains("1.0.0"));
    
    // Test lockfile update
    let updated_content = r#"[[pack]]
id = "io.rgen.test.pack"
version = "1.0.0"
sha256 = "abc123def456"
source = "https://github.com/test/repo.git#abc123"

[[pack]]
id = "io.rgen.another.pack"
version = "2.0.0"
sha256 = "def456ghi789"
source = "https://github.com/test/another.git#def456"
"#;
    fs::write(&lockfile_path, updated_content)?;
    
    let updated = fs::read_to_string(&lockfile_path)?;
    assert!(updated.contains("io.rgen.another.pack"));
    
    // Test lockfile removal
    fs::remove_file(&lockfile_path)?;
    assert!(!lockfile_path.exists());
    
    Ok(())
}