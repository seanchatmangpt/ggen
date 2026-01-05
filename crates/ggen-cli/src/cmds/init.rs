//! Init Command - Initialize a new ggen project
//!
//! `ggen init` scaffolds a minimal, working ggen project with hardcoded files.
//! Creates directory structure and seed files that can be customized.

#![allow(clippy::unused_unit)] // clap-noun-verb macro generates this

use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::fs;
use std::path::Path;

// ============================================================================
// Output Types
// ============================================================================

/// Output for the `ggen init` command
#[derive(Debug, Clone, Serialize)]
pub struct InitOutput {
    /// Overall status: "success" or "partial" or "error"
    pub status: String,

    /// Project directory initialized
    pub project_dir: String,

    /// Files created (new files)
    pub files_created: Vec<String>,

    /// Files overwritten (replaced existing files)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub files_overwritten: Option<Vec<String>>,

    /// Directories created
    pub directories_created: Vec<String>,

    /// Error message (if failed)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,

    /// Warning message (if partial success)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub warning: Option<String>,

    /// Next steps
    pub next_steps: Vec<String>,
}

// ============================================================================
// Hardcoded File Contents
// ============================================================================

const GGEN_TOML: &str = r#"[project]
name = "my-ggen-project"
version = "0.1.0"
description = "A ggen project initialized with default templates"
authors = ["ggen init"]
license = "MIT"

[generation]
ontology_dir = "schema/"
templates_dir = "templates/"
output_dir = "src/generated/"
incremental = true
overwrite = false

[sync]
enabled = true
on_change = "manual"
validate_after = true
conflict_mode = "fail"

[rdf]
formats = ["turtle"]
default_format = "turtle"
strict_validation = false

[templates]
enable_caching = true
auto_reload = true

[output]
formatting = "default"
line_length = 100
indent = 2
"#;

const DOMAIN_TTL: &str = r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <https://example.com/> .

# Example Person class
ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "Represents a person in the system" .

# Example properties for Person
ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ;
    rdfs:label "name" ;
    rdfs:comment "The name of the person" .

ex:email a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ;
    rdfs:label "email" ;
    rdfs:comment "The email address of the person" .

ex:age a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ;
    rdfs:label "age" ;
    rdfs:comment "The age of the person" .
"#;

const MAKEFILE: &str = r#".PHONY: help setup build clean

help:
	@echo "ggen project - Available targets:"
	@echo "  make setup   - Run startup.sh to initialize project"
	@echo "  make build   - Generate code from ontology (ggen sync)"
	@echo "  make clean   - Remove generated artifacts"
	@echo ""
	@echo "See scripts/startup.sh for custom initialization steps"

setup:
	@bash scripts/startup.sh

build:
	ggen sync

clean:
	rm -rf src/generated/
	rm -rf .ggen/
"#;

const STARTUP_SH: &str = r#"#!/bin/bash
# Startup script for ggen project initialization
# Run this to prepare your environment for development

set -e

echo "🚀 Initializing ggen project..."

# Create directories if they don't exist
mkdir -p schema
mkdir -p templates
mkdir -p src/generated
mkdir -p scripts

echo "✅ Project structure created"

# Display next steps
echo ""
echo "📋 Next steps:"
echo "  1. Edit schema/domain.ttl to define your domain model"
echo "  2. Create Tera templates in templates/ for your target languages"
echo "  3. Run 'ggen sync' to generate code from your ontology"
echo "  4. Run 'make build' to regenerate after changes"
echo ""
echo "📚 Learn more:"
echo "  - ggen Documentation: https://docs.ggen.io"
echo "  - RDF/Turtle Syntax: https://www.w3.org/TR/turtle/"
echo "  - Tera Template Language: https://keats.github.io/tera/"
"#;

// ============================================================================
// The Init Command
// ============================================================================

/// Initialize a new ggen project with default structure and scripts.
///
/// Creates a minimal, working ggen project scaffold with:
/// - ggen.toml configuration
/// - schema/domain.ttl (RDF ontology with example)
/// - Makefile (setup, build, clean targets)
/// - scripts/startup.sh (project initialization script)
/// - templates/ (empty, ready for custom Tera templates)
/// - src/generated/ (output directory)
///
/// ## Usage
///
/// ```bash
/// # Initialize in current directory
/// ggen init
///
/// # Initialize in specific directory
/// ggen init --path my-project
/// ```
///
/// ## Flags
///
/// --path PATH               Project directory (default: current directory)
/// --force                   Overwrite existing files
///
/// ## Output
///
/// Returns JSON with created files and next steps.
///
/// ## Next Steps
///
/// After initialization:
/// 1. Run `make setup` to prepare your environment
/// 2. Edit schema/domain.ttl with your domain model
/// 3. Create Tera templates in templates/ for your target languages
/// 4. Run `make build` to generate code from your ontology
///
#[allow(clippy::unused_unit)]
#[verb("init", "root")]
pub fn init(path: Option<String>, force: Option<bool>) -> clap_noun_verb::Result<InitOutput> {
    // Thin CLI layer: parse arguments and delegate to helper
    let project_dir = path.unwrap_or_else(|| ".".to_string());
    let force = force.unwrap_or(false);

    // Delegate to initialization logic
    perform_init(&project_dir, force)
}

/// Helper function that performs the actual initialization.
///
/// This function contains all the file I/O and directory creation logic,
/// keeping the verb function thin and following the CLI architecture pattern.
///
/// Strategy:
/// 1. Check if ggen artifacts already exist (not just any directory content)
/// 2. If they do and --force is not used, error
/// 3. Track which files are created vs overwritten
/// 4. Report both clearly to user
fn perform_init(
    project_dir: &str,
    force: bool,
) -> clap_noun_verb::Result<InitOutput> {
    // Convert to Path for easier manipulation
    let base_path = Path::new(project_dir);

    // List of ggen-specific artifacts to check for
    let ggen_artifacts = vec!["ggen.toml", "Makefile", "schema/domain.ttl", "scripts/startup.sh"];

    // Check if ggen has already been initialized in this directory
    let has_ggen_artifacts = ggen_artifacts.iter().any(|artifact| {
        let path = base_path.join(artifact);
        path.exists()
    });

    if has_ggen_artifacts && !force {
        return Ok(InitOutput {
            status: "error".to_string(),
            project_dir: project_dir.to_string(),
            files_created: vec![],
            files_overwritten: None,
            directories_created: vec![],
            error: Some(
                "ggen project already initialized here. Use --force to reinitialize.".to_string(),
            ),
            warning: None,
            next_steps: vec!["Run 'make build' to regenerate code".to_string()],
        });
    }

    // Ensure base directory exists
    if let Err(e) = fs::create_dir_all(base_path) {
        return Ok(InitOutput {
            status: "error".to_string(),
            project_dir: project_dir.to_string(),
            files_created: vec![],
            files_overwritten: None,
            directories_created: vec![],
            error: Some(format!("Failed to create project directory: {}", e)),
            warning: None,
            next_steps: vec![],
        });
    }

    let mut files_created = vec![];
    let mut files_overwritten = vec![];
    let mut directories_created = vec![];
    let mut errors = vec![];

    // Create directories
    let dirs = vec!["schema", "templates", "src/generated", "scripts"];
    for dir in &dirs {
        let dir_path = base_path.join(dir);
        match fs::create_dir_all(&dir_path) {
            Ok(_) => directories_created.push(dir.to_string()),
            Err(e) => errors.push(format!("Failed to create {}: {}", dir, e)),
        }
    }

    // Helper closure to write file and track if it was created or overwritten
    let mut write_file = |path: &Path, content: &str, filename: &str| {
        let exists = path.exists();
        match fs::write(path, content) {
            Ok(_) => {
                if exists {
                    files_overwritten.push(filename.to_string());
                } else {
                    files_created.push(filename.to_string());
                }
            }
            Err(e) => errors.push(format!("Failed to write {}: {}", filename, e)),
        }
    };

    // Create ggen.toml
    let toml_path = base_path.join("ggen.toml");
    write_file(&toml_path, GGEN_TOML, "ggen.toml");

    // Create schema/domain.ttl
    let schema_path = base_path.join("schema").join("domain.ttl");
    write_file(&schema_path, DOMAIN_TTL, "schema/domain.ttl");

    // Create Makefile
    let makefile_path = base_path.join("Makefile");
    write_file(&makefile_path, MAKEFILE, "Makefile");

    // Create scripts/startup.sh
    let startup_sh_path = base_path.join("scripts").join("startup.sh");
    let exists_before_startup = startup_sh_path.exists();
    write_file(&startup_sh_path, STARTUP_SH, "scripts/startup.sh");
    // Make the script executable on Unix systems (only if we just created it)
    if !exists_before_startup {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let _ = fs::set_permissions(&startup_sh_path, std::fs::Permissions::from_mode(0o755));
        }
    }

    // Create .gitignore (only if it doesn't exist - preserve user's gitignore)
    let gitignore_path = base_path.join(".gitignore");
    if !gitignore_path.exists() {
        let gitignore_content = "# ggen outputs\nsrc/generated/\n.ggen/\n";
        write_file(&gitignore_path, gitignore_content, ".gitignore");
    }

    // Create README.md (only if it doesn't exist - preserve user's README)
    let readme_path = base_path.join("README.md");
    let readme_content = r#"# My ggen Project

Generated by `ggen init`.

## Getting Started

1. **Initialize project**: Run `make setup` to prepare your environment
2. **Define schema**: Edit `schema/domain.ttl` with your domain model
3. **Create templates**: Add Tera templates in `templates/` for your target languages
4. **Generate code**: Run `make build` to generate code from your ontology

## Project Structure

```
.
├── Makefile                     # Build targets (setup, build, clean)
├── ggen.toml                    # ggen project configuration
├── README.md                    # This file
├── schema/
│   └── domain.ttl               # RDF ontology (Turtle format)
├── templates/                   # Tera templates for code generation
│   └── (create your templates here)
├── scripts/
│   └── startup.sh               # Project initialization script
└── src/
    └── generated/               # Generated code output
```

## Makefile Targets

```bash
make help       # Show available targets
make setup      # Run scripts/startup.sh to initialize project
make build      # Generate code from ontology (ggen sync)
make clean      # Remove generated artifacts
```

## Manual Commands

```bash
# Generate code from ontology
ggen sync

# Dry-run: preview changes without writing
ggen sync --dry-run

# Watch mode: regenerate on file changes
ggen sync --watch

# Validate without generating
ggen sync --validate-only
```

## Adding Templates

Create Tera templates in the `templates/` directory. Each template can generate code in your target language.

Example template structure:
```
templates/
├── rust.tera          # Rust code generator
├── typescript.tera    # TypeScript code generator
└── python.tera        # Python code generator
```

See `ggen.toml` to configure which templates to run and where they output.

## Learn More

- [ggen Documentation](https://docs.ggen.io)
- [RDF/Turtle Syntax](https://www.w3.org/TR/turtle/)
- [Tera Template Language](https://keats.github.io/tera/)
- [Custom Startup Scripts](scripts/startup.sh)
"#;
    // Create README.md (only if it doesn't exist - preserve user's README)
    if !readme_path.exists() {
        write_file(&readme_path, readme_content, "README.md");
    }

    // Determine final status and warnings
    let status = if errors.is_empty() {
        "success".to_string()
    } else {
        "partial".to_string()
    };

    let error = if errors.is_empty() {
        None
    } else {
        Some(errors.join("; "))
    };

    let warning = if !files_overwritten.is_empty() {
        Some(format!(
            "Overwrote {} file(s). Run --force to reinitialize.",
            files_overwritten.len()
        ))
    } else {
        None
    };

    let files_overwritten_opt = if files_overwritten.is_empty() {
        None
    } else {
        Some(files_overwritten)
    };

    Ok(InitOutput {
        status,
        project_dir: project_dir.to_string(),
        files_created,
        files_overwritten: files_overwritten_opt,
        directories_created,
        error,
        warning,
        next_steps: vec![
            "Run 'make setup' to initialize your project".to_string(),
            "Edit schema/domain.ttl to define your domain model".to_string(),
            "Create Tera templates in templates/ for your target languages".to_string(),
            "Run 'make build' to generate code from your ontology".to_string(),
        ],
    })
}
