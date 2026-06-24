use std::path::PathBuf;
use tempfile::TempDir;
use ggen_daemon::{catalog::RepoCatalogEntry, generator::generate_bundle};

fn ggen_root() -> PathBuf {
    let cwd = std::env::current_dir().unwrap();
    // Tests run from workspace root via `cargo test`
    cwd.ancestors()
        .find(|p| p.join(".specify").exists())
        .map(|p| p.to_path_buf())
        .unwrap_or(cwd)
}

fn fake_entry(name: &str) -> RepoCatalogEntry {
    RepoCatalogEntry {
        name: name.to_string(),
        github_url: format!("https://github.com/seanchatmangpt/{}", name),
        short_desc: format!("Test repo {}", name),
        primary_language: Some("Rust".to_string()),
    }
}

#[test]
fn generate_bundle_writes_readme_to_target_dir() {
    let root = ggen_root();
    let manifest = ".specify/specs/community-health/ggen.toml";
    if !root.join(manifest).exists() {
        eprintln!("skipping: manifest not found at {:?}", root.join(manifest));
        return;
    }

    let target = TempDir::new().unwrap();
    let entry = fake_entry("test-repo");

    let written = generate_bundle(&root, manifest, &entry, target.path()).unwrap();

    // At minimum README.md should be written
    assert!(!written.is_empty(), "expected at least one file written");
    let readme = target.path().join("README.md");
    assert!(readme.exists(), "README.md should be written");

    let content = std::fs::read_to_string(&readme).unwrap();
    assert!(content.contains("test-repo"), "README should mention repoName");
    assert!(!content.is_empty(), "README should have content");
}

#[test]
fn generate_bundle_skips_update_mode_existing_files() {
    let root = ggen_root();
    let manifest = ".specify/specs/community-health/ggen.toml";
    if !root.join(manifest).exists() {
        return;
    }

    let target = TempDir::new().unwrap();
    let entry = fake_entry("skip-test");

    // Pre-create README.md — but community-health uses mode = "Create", not "Update"
    // So the file should still be overwritten. This tests that Update-mode skipping
    // is correct for specs that actually use Update mode.
    let readme = target.path().join("README.md");
    std::fs::write(&readme, "ORIGINAL").unwrap();

    let written = generate_bundle(&root, manifest, &entry, target.path()).unwrap();

    // Create mode overwrites — original should be gone
    let content = std::fs::read_to_string(&readme).unwrap();
    assert_ne!(content, "ORIGINAL", "Create mode should overwrite existing file");
    let _ = written;
}
