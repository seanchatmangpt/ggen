//! Integration tests for CLI generator with workspace pattern
//!
//! Tests the full pipeline:
//! 1. Parse RDF/TTL file
//! 2. Generate workspace structure (crates/{cli-crate}, crates/{domain-crate})
//! 3. Generate CLI layer with clap-noun-verb v3.3.0
//! 4. Generate domain layer
//! 5. Verify workspace structure
//! 6. Verify CLI-to-domain contract

use anyhow::Result;
use assert_fs::prelude::*;
use assert_fs::TempDir;
use std::fs;
use std::path::Path;

#[test]
fn test_generate_workspace_structure() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let output_dir = temp_dir.path().join("generated-cli");
    
    // Create template directory with new templates
    let template_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&template_dir)?;
    
    // Copy workspace template
    let workspace_templates_dir = Path::new("templates/cli/workspace");
    if workspace_templates_dir.exists() {
        // Test with actual templates
        let generator = ggen_ai::rdf::CliGenerator::new(Path::new("templates").to_path_buf());
        
        // Create a minimal TTL file for testing
        let ttl_content = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix cli: <http://ggen.dev/schema/cli#> .
@prefix cnv: <http://ggen.dev/schema/clap-noun-verb#> .

<#test-project> a cli:CliProject ;
    cli:name "test-cli" ;
    cli:version "0.1.0" ;
    cli:description "Test CLI project" ;
    cli:edition "2021" ;
    cli:license "MIT" ;
    cli:hasCliCrate "test-cli" ;
    cli:hasDomainCrate "test-core" ;
    cli:hasWorkspaceResolver "2" .

<#user> a cnv:Noun ;
    cnv:name "user" ;
    cnv:description "User management" ;
    cnv:hasVerb <#create-user> .

<#create-user> a cnv:Verb ;
    cnv:name "create" ;
    cnv:description "Create a new user" ;
    cnv:domainFunction "test_core::user::create" .
"#;
        
        let ttl_file = temp_dir.path().join("test.ttl");
        fs::write(&ttl_file, ttl_content)?;
        
        // Generate CLI project
        generator.generate_from_ttl(&ttl_file, &output_dir)?;
        
        // Verify workspace structure exists
        assert!(output_dir.join("Cargo.toml").exists(), "Workspace Cargo.toml should exist");
        assert!(output_dir.join("crates").exists(), "crates/ directory should exist");
        assert!(output_dir.join("crates/test-cli").exists(), "CLI crate directory should exist");
        assert!(output_dir.join("crates/test-core").exists(), "Domain crate directory should exist");
        
        // Verify workspace Cargo.toml has correct members
        let workspace_cargo = fs::read_to_string(output_dir.join("Cargo.toml"))?;
        assert!(workspace_cargo.contains("crates/test-cli"), "Should include CLI crate in members");
        assert!(workspace_cargo.contains("crates/test-core"), "Should include domain crate in members");
        assert!(workspace_cargo.contains("clap-noun-verb = \"3.3.0\""), "Should use clap-noun-verb v3.3.0");
        
        // Verify CLI crate structure
        let cli_crate_dir = output_dir.join("crates/test-cli");
        assert!(cli_crate_dir.join("Cargo.toml").exists(), "CLI crate Cargo.toml should exist");
        assert!(cli_crate_dir.join("src/main.rs").exists(), "CLI main.rs should exist");
        assert!(cli_crate_dir.join("src/lib.rs").exists(), "CLI lib.rs should exist");
        assert!(cli_crate_dir.join("src/runtime.rs").exists(), "CLI runtime.rs should exist");
        assert!(cli_crate_dir.join("src/cmds").exists(), "CLI cmds/ directory should exist");
        
        // Verify domain crate structure
        let domain_crate_dir = output_dir.join("crates/test-core");
        assert!(domain_crate_dir.join("Cargo.toml").exists(), "Domain crate Cargo.toml should exist");
        assert!(domain_crate_dir.join("src/lib.rs").exists(), "Domain lib.rs should exist");
        assert!(domain_crate_dir.join("src/user").exists(), "Domain user module should exist");
        
        Ok(())
    } else {
        // Skip test if templates don't exist (e.g., in CI without templates)
        eprintln!("Warning: templates/cli/workspace not found, skipping test");
        Ok(())
    }
}

#[test]
fn test_cli_to_domain_contract() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let output_dir = temp_dir.path().join("generated-cli");
    
    let workspace_templates_dir = Path::new("templates/cli/workspace");
    if workspace_templates_dir.exists() {
        let generator = ggen_ai::rdf::CliGenerator::new(Path::new("templates").to_path_buf());
        
        let ttl_content = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix cli: <http://ggen.dev/schema/cli#> .
@prefix cnv: <http://ggen.dev/schema/clap-noun-verb#> .

<#test-project> a cli:CliProject ;
    cli:name "test-cli" ;
    cli:version "0.1.0" ;
    cli:description "Test CLI project" ;
    cli:edition "2021" ;
    cli:license "MIT" ;
    cli:hasCliCrate "test-cli" ;
    cli:hasDomainCrate "test-core" .

<#user> a cnv:Noun ;
    cnv:name "user" ;
    cnv:description "User management" ;
    cnv:hasVerb <#create-user> .

<#create-user> a cnv:Verb ;
    cnv:name "create" ;
    cnv:description "Create a new user" ;
    cnv:domainFunction "test_core::user::create" .
"#;
        
        let ttl_file = temp_dir.path().join("test.ttl");
        fs::write(&ttl_file, ttl_content)?;
        
        generator.generate_from_ttl(&ttl_file, &output_dir)?;
        
        // Verify CLI crate references domain crate
        let cli_cargo = fs::read_to_string(output_dir.join("crates/test-cli/Cargo.toml"))?;
        assert!(cli_cargo.contains("test-core"), "CLI crate should depend on domain crate");
        
        // Verify CLI verb calls domain function
        let verb_file = output_dir.join("crates/test-cli/src/cmds/user/create.rs");
        if verb_file.exists() {
            let verb_content = fs::read_to_string(&verb_file)?;
            assert!(verb_content.contains("test_core"), "Verb should reference domain crate");
            assert!(verb_content.contains("user::create"), "Verb should reference domain function");
        }
        
        // Verify domain crate has no CLI dependencies
        let domain_cargo = fs::read_to_string(output_dir.join("crates/test-core/Cargo.toml"))?;
        assert!(!domain_cargo.contains("clap"), "Domain crate should not depend on clap");
        assert!(!domain_cargo.contains("clap-noun-verb"), "Domain crate should not depend on clap-noun-verb");
        
        Ok(())
    } else {
        eprintln!("Warning: templates/cli/workspace not found, skipping test");
        Ok(())
    }
}

