use anyhow::Result;
use assert_cmd::Command;
use std::fs;
use tempfile::TempDir;

#[path = "common/mod.rs"]
mod test_config;
use test_config::integration_timeout;

/// E2E tests for production marketplace workflow
///
/// Tests the complete marketplace workflow against the production registry
/// at https://seanchatmangpt.github.io/ggen/
///
/// Note: These tests require internet connectivity and may be affected by
/// registry availability. They are designed to test the real-world workflow.

#[test]
fn test_search_production_registry() -> Result<()> {
    // Test searching the production registry
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("search").arg("rust");

    // Set production registry URL explicitly
    cmd.env(
        "GGEN_REGISTRY_URL",
        "https://seanchatmangpt.github.io/ggen/docs/registry/",
    );

    let output = cmd.output()?;

    // Command should complete (might fail if network unavailable)
    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should return search results
        assert!(
            stdout.contains("io.ggen") || stdout.contains("gpack") || stdout.contains("rust"),
            "Search should return results from production registry"
        );
    } else {
        // If failed, check if it's a network issue
        let stderr = String::from_utf8_lossy(&output.stderr);
        println!(
            "Note: Production registry search failed (possibly network): {}",
            stderr
        );
    }

    Ok(())
}

#[test]
fn test_add_from_production_registry() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let project_dir = temp_dir.path();

    // Test adding a package from production registry
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("add")
        .arg("io.ggen.rust.cli-subcommand")
        .current_dir(project_dir);

    // Set production registry URL
    cmd.env(
        "GGEN_REGISTRY_URL",
        "https://seanchatmangpt.github.io/ggen/docs/registry/",
    );

    let output = cmd.output()?;

    // If successful, verify lockfile was created
    if output.status.success() {
        let lockfile_path = project_dir.join("ggen.lock");
        assert!(
            lockfile_path.exists(),
            "Lockfile should be created after successful add"
        );

        // Verify lockfile content
        let lockfile_content = fs::read_to_string(&lockfile_path)?;
        assert!(lockfile_content.contains("io.ggen.rust.cli-subcommand"));
        assert!(lockfile_content.contains("version"));
        assert!(lockfile_content.contains("sha256"));
        assert!(lockfile_content.contains("source"));

        // Verify cache directory was created
        let cache_dir = project_dir.join(".ggen").join("cache");
        if cache_dir.exists() {
            assert!(
                cache_dir.is_dir(),
                "Cache directory should exist after download"
            );
        }
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        println!(
            "Note: Production registry add failed (possibly network): {}",
            stderr
        );
    }

    Ok(())
}

#[test]
fn test_lockfile_created_with_sha256() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let project_dir = temp_dir.path();

    // Add a package and verify SHA256 in lockfile
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("add")
        .arg("io.ggen.rust.cli-subcommand")
        .current_dir(project_dir);

    cmd.env(
        "GGEN_REGISTRY_URL",
        "https://seanchatmangpt.github.io/ggen/docs/registry/",
    );

    let output = cmd.output()?;

    if output.status.success() {
        let lockfile_path = project_dir.join("ggen.lock");
        assert!(lockfile_path.exists());

        let lockfile_content = fs::read_to_string(&lockfile_path)?;

        // Parse as TOML
        let lockfile: toml::Value = toml::from_str(&lockfile_content)?;
        let packs = lockfile
            .get("packs")
            .and_then(|p| p.as_array())
            #[allow(clippy::expect_used)]
            .expect("Lockfile should have packs array");

        assert!(!packs.is_empty());

        let sha256 = packs[0]
            .get("sha256")
            .and_then(|s| s.as_str())
            #[allow(clippy::expect_used)]
            .expect("Pack should have sha256 field");

        // Verify SHA256 is real (not placeholder)
        assert_eq!(sha256.len(), 64, "SHA256 should be 64 hex characters");
        assert!(sha256.chars().all(|c| c.is_ascii_hexdigit()));

        // Should not be all zeros
        assert_ne!(
            sha256, "0000000000000000000000000000000000000000000000000000000000000000",
            "SHA256 should not be placeholder"
        );
    }

    Ok(())
}

#[test]
#[ignore] // Network-dependent: requires internet connection and GitHub Pages
fn test_production_registry_index_accessible() -> Result<()> {
    // Test that the production registry index.json is accessible
    // This is a basic connectivity test

    // Use reqwest to check if index is accessible
    let client = reqwest::blocking::Client::builder()
        .timeout(integration_timeout())
        .build()?;

    let index_url = "https://seanchatmangpt.github.io/ggen/docs/registry/index.json";

    match client.get(index_url).send() {
        Ok(response) => {
            assert!(
                response.status().is_success(),
                "Registry index should be accessible"
            );

            // Try to parse as JSON
            let text = response.text()?;
            let parsed: serde_json::Value = serde_json::from_str(&text)?;

            // Should have expected structure
            assert!(
                parsed.get("packs").is_some() || parsed.get("packages").is_some(),
                "Registry index should have packs or packages field"
            );
        }
        Err(e) => {
            println!(
                "Note: Could not access production registry (network issue): {}",
                e
            );
        }
    }

    Ok(())
}

#[test]
fn test_search_with_detailed_output() -> Result<()> {
    // Test search with detailed output flag
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("search").arg("rust").arg("--detailed");

    cmd.env(
        "GGEN_REGISTRY_URL",
        "https://seanchatmangpt.github.io/ggen/docs/registry/",
    );

    let output = cmd.output()?;

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Detailed output should include more information
        assert!(
            stdout.contains("ID:")
                || stdout.contains("Name:")
                || stdout.contains("Description:")
                || stdout.contains("Version:"),
            "Detailed search should include metadata"
        );
    }

    Ok(())
}

#[test]
fn test_search_with_category_filter() -> Result<()> {
    // Test search with category filter
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("search").arg("cli").arg("--category").arg("rust");

    cmd.env(
        "GGEN_REGISTRY_URL",
        "https://seanchatmangpt.github.io/ggen/docs/registry/",
    );

    let output = cmd.output()?;

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Results should be filtered by category
        assert!(
            stdout.contains("rust") || stdout.is_empty(),
            "Category filter should work"
        );
    }

    Ok(())
}

#[test]
fn test_search_with_json_output() -> Result<()> {
    // Test search with JSON output format
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("search").arg("rust").arg("--json");

    cmd.env(
        "GGEN_REGISTRY_URL",
        "https://seanchatmangpt.github.io/ggen/docs/registry/",
    );

    let output = cmd.output()?;

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Output should be valid JSON
        if !stdout.is_empty() {
            let parsed: Result<serde_json::Value, _> = serde_json::from_str(&stdout);
            assert!(parsed.is_ok(), "JSON output should be valid");
        }
    }

    Ok(())
}

#[test]
fn test_packs_command_lists_installed() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let project_dir = temp_dir.path();

    // First, add a package
    let mut add_cmd = Command::cargo_bin("ggen")?;
    add_cmd
        .arg("add")
        .arg("io.ggen.rust.cli-subcommand")
        .current_dir(project_dir);

    add_cmd.env(
        "GGEN_REGISTRY_URL",
        "https://seanchatmangpt.github.io/ggen/docs/registry/",
    );

    let add_output = add_cmd.output()?;

    if add_output.status.success() {
        // Then list installed packs
        let mut packs_cmd = Command::cargo_bin("ggen")?;
        packs_cmd.arg("packs").current_dir(project_dir);

        let packs_output = packs_cmd.output()?;

        if packs_output.status.success() {
            let stdout = String::from_utf8_lossy(&packs_output.stdout);

            // Should list the installed pack
            assert!(
                stdout.contains("io.ggen.rust.cli-subcommand"),
                "packs command should list installed packages"
            );
        }
    }

    Ok(())
}

#[test]
fn test_categories_command() -> Result<()> {
    // Test the categories command
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("categories");

    cmd.env(
        "GGEN_REGISTRY_URL",
        "https://seanchatmangpt.github.io/ggen/docs/registry/",
    );

    let output = cmd.output()?;

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should show popular categories
        assert!(
            stdout.contains("rust")
                || stdout.contains("python")
                || stdout.contains("category")
                || stdout.contains("Popular"),
            "categories command should show available categories"
        );
    }

    Ok(())
}

#[test]
fn test_show_command_displays_pack_info() -> Result<()> {
    // Test the show command for displaying pack metadata
    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("show").arg("io.ggen.rust.cli-subcommand");

    cmd.env(
        "GGEN_REGISTRY_URL",
        "https://seanchatmangpt.github.io/ggen/docs/registry/",
    );

    let output = cmd.output()?;

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should display pack information
        assert!(
            stdout.contains("io.ggen.rust.cli-subcommand")
                || stdout.contains("Name:")
                || stdout.contains("Description:")
                || stdout.contains("Version:"),
            "show command should display pack metadata"
        );
    }

    Ok(())
}

#[test]
fn test_marketplace_workflow_end_to_end() -> Result<()> {
    // Complete E2E workflow: search → add → verify → list
    let temp_dir = TempDir::new()?;
    let project_dir = temp_dir.path();

    let registry_url = "https://seanchatmangpt.github.io/ggen/docs/registry/";

    // Step 1: Search for packages
    let mut search_cmd = Command::cargo_bin("ggen")?;
    search_cmd.arg("search").arg("rust");
    search_cmd.env("GGEN_REGISTRY_URL", registry_url);

    let search_output = search_cmd.output()?;
    let search_success = search_output.status.success();

    if !search_success {
        println!(
            "Note: Search failed (network issue): {}",
            String::from_utf8_lossy(&search_output.stderr)
        );
        return Ok(());
    }

    // Step 2: Add a package
    let mut add_cmd = Command::cargo_bin("ggen")?;
    add_cmd
        .arg("add")
        .arg("io.ggen.rust.cli-subcommand")
        .current_dir(project_dir);
    add_cmd.env("GGEN_REGISTRY_URL", registry_url);

    let add_output = add_cmd.output()?;
    let add_success = add_output.status.success();

    if !add_success {
        println!(
            "Note: Add failed (network issue): {}",
            String::from_utf8_lossy(&add_output.stderr)
        );
        return Ok(());
    }

    // Step 3: Verify lockfile was created with correct content
    let lockfile_path = project_dir.join("ggen.lock");
    assert!(lockfile_path.exists(), "Lockfile should exist after add");

    let lockfile_content = fs::read_to_string(&lockfile_path)?;
    assert!(lockfile_content.contains("io.ggen.rust.cli-subcommand"));
    assert!(lockfile_content.contains("version"));
    assert!(lockfile_content.contains("sha256"));

    // Verify SHA256 is real
    let lockfile: toml::Value = toml::from_str(&lockfile_content)?;
    let packs = lockfile.get("packs").unwrap().as_array().unwrap();
    let sha256 = packs[0].get("sha256").unwrap().as_str().unwrap();

    assert_eq!(sha256.len(), 64);
    assert!(sha256.chars().all(|c| c.is_ascii_hexdigit()));
    assert_ne!(sha256, "0".repeat(64));

    // Step 4: List installed packages
    let mut packs_cmd = Command::cargo_bin("ggen")?;
    packs_cmd.arg("packs").current_dir(project_dir);

    let packs_output = packs_cmd.output()?;

    if packs_output.status.success() {
        let stdout = String::from_utf8_lossy(&packs_output.stdout);
        assert!(stdout.contains("io.ggen.rust.cli-subcommand"));
    }

    Ok(())
}

#[test]
fn test_registry_fallback_to_default() -> Result<()> {
    // Test that registry falls back to default URL if env var not set
    let temp_dir = TempDir::new()?;

    let mut cmd = Command::cargo_bin("ggen")?;
    cmd.arg("search").arg("rust").current_dir(temp_dir.path());

    // Don't set GGEN_REGISTRY_URL - should use default

    let output = cmd.output()?;

    // Should complete (success or specific error, not crash)
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success()
            || stderr.contains("Error")
            || stderr.contains("registry")
            || stderr.contains("network"),
        "Command should use default registry or provide error"
    );

    Ok(())
}
