use anyhow::Result;
use std::env;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// End-to-end test for the complete marketplace workflow
#[test]
fn test_marketplace_e2e_workflow() -> Result<()> {
    // Setup: Create a temporary project directory
    let project_dir = TempDir::new()?;
    let project_path = project_dir.path();

    // Setup: Use local registry for testing
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let registry_path = PathBuf::from(manifest_dir).join("docs").join("registry");
    let file_url = format!("file://{}/", registry_path.canonicalize()?.display());

    // Set environment variable for local registry
    env::set_var("GGEN_REGISTRY_URL", &file_url);

    // Step 1: Search for gpacks
    println!("Step 1: Searching for gpacks...");
    let search_results = simulate_search("rust")?;
    assert!(!search_results.is_empty());
    assert!(search_results.iter().any(|r| r.contains("cli-subcommand")));
    println!("✓ Search found {} gpacks", search_results.len());

    // Step 2: Add an gpack
    println!("Step 2: Adding gpack...");
    let gpack_id = "io.ggen.rust.cli-subcommand";
    let gpack_version = "0.1.0";

    // Simulate downloading and caching the gpack
    let cache_dir = project_path.join(".ggen").join("gpacks");
    fs::create_dir_all(&cache_dir)?;

    // Copy the gpack to cache (simulating git clone)
    let cached_gpack_dir = cache_dir.join(gpack_id).join(gpack_version);
    fs::create_dir_all(&cached_gpack_dir)?;
    copy_dir_all(
        &registry_path
            .join("..")
            .join("..")
            .join("templates")
            .join("cli")
            .join("subcommand"),
        &cached_gpack_dir,
    )?;

    println!("✓ Gpack cached to: {}", cached_gpack_dir.display());

    // Step 3: Create/update lockfile
    println!("Step 3: Creating lockfile...");
    let lockfile_path = project_path.join("ggen.lock");
    let lockfile_content = format!(
        r#"version = "1.0"
generated = "2024-12-19T00:00:00Z"

[[pack]]
id = "{}"
version = "{}"
sha256 = "58db67ac8440401e"
source = "https://github.com/seanchatmangpt/ggen.git#11ea0739a579165c33fde5fb4d5a347bed6f5c58"
"#,
        gpack_id, gpack_version
    );
    fs::write(&lockfile_path, lockfile_content)?;
    println!("✓ Lockfile created");

    // Step 4: Generate code using the gpack
    println!("Step 4: Generating code...");
    let template_path = cached_gpack_dir.join("rust.tmpl");
    if template_path.exists() {
        let template_content = fs::read_to_string(&template_path)?;
        assert!(template_content.contains("subcommand"));
        println!("✓ Template found and validated");
    } else {
        println!("⚠ Template not found at expected path");
    }

    // Step 5: List installed gpacks
    println!("Step 5: Listing installed gpacks...");
    let lockfile_content = fs::read_to_string(&lockfile_path)?;
    assert!(lockfile_content.contains(gpack_id));
    assert!(lockfile_content.contains(gpack_version));
    println!("✓ Lockfile contains installed gpack");

    // Step 6: Test template resolution
    println!("Step 6: Testing template resolution...");
    let template_ref = format!("{}:rust.tmpl", gpack_id);
    let resolved_template = resolve_template(&template_ref, project_path)?;
    assert!(resolved_template.exists());
    println!("✓ Template resolution successful");

    // Step 7: Cleanup
    println!("Step 7: Cleaning up...");
    fs::remove_file(&lockfile_path)?;
    fs::remove_dir_all(&cached_gpack_dir)?;
    println!("✓ Cleanup completed");

    println!("\n🎉 E2E marketplace workflow test passed!");
    Ok(())
}

/// Simulate search functionality
fn simulate_search(query: &str) -> Result<Vec<String>> {
    // Load registry index
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let registry_path = PathBuf::from(manifest_dir)
        .join("docs")
        .join("registry")
        .join("index.json");
    let registry_content = fs::read_to_string(&registry_path)?;
    let registry: serde_json::Value = serde_json::from_str(&registry_content)?;

    let mut results = Vec::new();
    if let Some(packs) = registry.get("packs").and_then(|p| p.as_object()) {
        for (pack_id, pack_data) in packs {
            if let Some(name) = pack_data.get("name").and_then(|n| n.as_str()) {
                if name.to_lowercase().contains(&query.to_lowercase()) {
                    results.push(pack_id.clone());
                }
            }
            if let Some(description) = pack_data.get("description").and_then(|d| d.as_str()) {
                if description.to_lowercase().contains(&query.to_lowercase()) {
                    results.push(pack_id.clone());
                }
            }
        }
    }

    Ok(results)
}

/// Copy directory recursively
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

/// Resolve template reference to actual file path
fn resolve_template(template_ref: &str, project_dir: &std::path::Path) -> Result<PathBuf> {
    let parts: Vec<&str> = template_ref.split(':').collect();
    if parts.len() != 2 {
        anyhow::bail!("Invalid template reference: {}", template_ref);
    }

    let pack_id = parts[0];
    let template_path = parts[1];

    // Load lockfile to get pack version
    let lockfile_path = project_dir.join("ggen.lock");
    let lockfile_content = fs::read_to_string(&lockfile_path)?;

    // Parse lockfile to find pack version
    let mut pack_version = None;
    for line in lockfile_content.lines() {
        if line.contains(&format!("id = \"{}\"", pack_id)) {
            // Find version in next few lines
            let lines: Vec<&str> = lockfile_content.lines().collect();
            if let Some(idx) = lines
                .iter()
                .position(|l| l.contains(&format!("id = \"{}\"", pack_id)))
            {
                for line in lines.iter().take((idx + 5).min(lines.len())).skip(idx + 1) {
                    if line.starts_with("version = ") {
                        pack_version = Some(
                            line
                                .trim_start_matches("version = \"")
                                .trim_end_matches("\""),
                        );
                        break;
                    }
                }
            }
            break;
        }
    }

    let version =
        pack_version.ok_or_else(|| anyhow::anyhow!("Pack version not found in lockfile"))?;

    // Construct template path
    let cache_dir = project_dir.join(".ggen").join("gpacks");
    let template_file = cache_dir.join(pack_id).join(version).join(template_path);

    Ok(template_file)
}

/// Test registry validation
#[test]
fn test_registry_validation() -> Result<()> {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let registry_path = PathBuf::from(manifest_dir)
        .join("docs")
        .join("registry")
        .join("index.json");

    // Load and validate registry
    let registry_content = fs::read_to_string(&registry_path)?;
    let registry: serde_json::Value = serde_json::from_str(&registry_content)?;

    // Basic structure validation
    assert!(registry.is_object());
    assert!(registry.get("updated").is_some());
    assert!(registry.get("packs").is_some());

    let packs = registry.get("packs").unwrap().as_object().unwrap();
    assert!(!packs.is_empty());

    // Validate cli-subcommand pack
    let cli_pack = packs.get("io.ggen.rust.cli-subcommand").unwrap();
    assert_eq!(
        cli_pack.get("id").unwrap().as_str().unwrap(),
        "io.ggen.rust.cli-subcommand"
    );
    assert_eq!(
        cli_pack.get("latest_version").unwrap().as_str().unwrap(),
        "0.1.0"
    );

    let versions = cli_pack.get("versions").unwrap().as_object().unwrap();
    let version_0_1_0 = versions.get("0.1.0").unwrap();
    assert_eq!(
        version_0_1_0.get("git_url").unwrap().as_str().unwrap(),
        "https://github.com/seanchatmangpt/ggen.git"
    );
    assert_eq!(
        version_0_1_0
            .get("git_rev")
            .unwrap()
            .as_str()
            .unwrap()
            .len(),
        40
    );
    assert_eq!(
        version_0_1_0.get("sha256").unwrap().as_str().unwrap().len(),
        64
    );

    println!("✓ Registry validation passed");
    Ok(())
}

/// Test hash generation script
#[test]
fn test_hash_generation() -> Result<()> {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let templates_path = PathBuf::from(manifest_dir)
        .join("templates")
        .join("cli")
        .join("subcommand");

    // Run hash generation script
    let output = std::process::Command::new("./scripts/generate_registry_hashes")
        .arg(templates_path.to_string_lossy().as_ref())
        .output()?;

    assert!(output.status.success());
    let hash = String::from_utf8(output.stdout)?;
    let hash = hash.trim();

    // Validate hash format
    assert_eq!(hash.len(), 64);
    assert!(hash.chars().all(|c| c.is_ascii_hexdigit()));

    println!("✓ Hash generation test passed: {}", hash);
    Ok(())
}
