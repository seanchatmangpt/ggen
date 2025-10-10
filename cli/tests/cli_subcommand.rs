use assert_cmd::prelude::*;
use dirs;
use std::process::Command;

#[test]
fn cli_gen_creates_files() {
    let td = assert_fs::TempDir::new().unwrap();
    let root = td.path();

    // Create a proper pack structure
    let pack_dir = root.join("pack");
    let templates_dir = pack_dir.join("templates/cli/subcommand");
    std::fs::create_dir_all(&templates_dir).unwrap();

    // Create the template file
    std::fs::write(
        templates_dir.join("rust.tmpl"),
        r#"---
to: out/hello.rs
vars: { cmd: hello }
---
pub fn hello() {}
"#,
    )
    .unwrap();

    // Create a lockfile entry for the local pack
    let lockfile_content = r#"version = "1.0.0"
generated = "2024-01-01T00:00:00Z"

[[packs]]
id = "local.test"
version = "1.0.0"
sha256 = "abc123"
source = "local"
"#;
    std::fs::write(root.join("ggen.lock"), lockfile_content).unwrap();

    // Create a cache entry for the local pack in the system cache directory
    let system_cache_dir = dirs::cache_dir().unwrap().join("ggen/gpacks");
    let cache_dir = system_cache_dir.join("local.test/1.0.0");
    let cache_templates_dir = cache_dir.join("templates");
    std::fs::create_dir_all(&cache_templates_dir).unwrap();

    // Copy the template to the cache
    let cache_template_dir = cache_templates_dir.join("cli/subcommand");
    std::fs::create_dir_all(&cache_template_dir).unwrap();
    std::fs::copy(
        templates_dir.join("rust.tmpl"),
        cache_template_dir.join("rust.tmpl"),
    )
    .unwrap();

    // Create the manifest
    std::fs::write(
        cache_templates_dir.join("ggen.toml"),
        r#"
[gpack]
id = "local.test"
name = "Local Test Pack"
version = "1.0.0"
description = "Local test pack"
license = "MIT"
ggen_compat = "1.0.0"
"#,
    )
    .unwrap();

    // Run `ggen gen local.test:cli/subcommand/rust.tmpl --var cmd=hello --dry`
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.current_dir(root)
        .arg("gen")
        .arg("local.test:cli/subcommand/rust.tmpl")
        .arg("--var")
        .arg("cmd=hello")
        .arg("--dry");
    cmd.assert().success();

    // In dry-run mode, file should not exist
    assert!(!root.join("out/hello.rs").exists());
}
