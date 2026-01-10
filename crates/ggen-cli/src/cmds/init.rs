//! Init Command - Initialize a new ggen project
//!
//! `ggen init` scaffolds a minimal, working ggen project with hardcoded files.
//! Creates directory structure and seed files that can be customized.
//!
//! ## BIG BANG 80/20 Screening Gate
//!
//! Before initialization, users must confirm they have:
//! 1. **Real user data** (CSV/JSON, not promised)
//! 2. **Found an existing standard ontology** (schema.org, FOAF, Dublin Core, SKOS)
//! 3. **Clear problem articulation** (one sentence, no 100-page docs)
//! 4. **Market signal** (email list, beta users, commitment - not enthusiasm)
//! 5. **Speed validation plan** (can validate with 10 users in 48 hours)
//!
//! This prevents Seth-like patterns: custom ontologies, 3-month research cycles,
//! zero market validation.

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

    /// Files preserved (user files not touched)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub files_preserved: Option<Vec<String>>,

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

# BIG BANG 80/20: Specification Closure First
# Before using ggen, confirm:
# 1. Do you have real user data (CSV/JSON)? Not promised—actual files.
# 2. Can you find one existing standard ontology (schema.org, FOAF, Dublin Core, SKOS)?
#    Should take 5 minutes. If it takes 3 months, you're building custom (wrong path).
# 3. Can you explain your problem in one sentence? No 100-page documents.
# 4. Has anyone (not a friend, not a co-founder) committed to this?
#    Email, contract, payment—proof, not enthusiasm.
# 5. Can you validate with 10 real users in 48 hours?
#
# If you answered NO to any of these, stop. Talk to Sean before proceeding.

[ontology]
# REQUIRED: Use standard ontologies only (BIG BANG 80/20 gate)
# Approved: schema.org, FOAF, Dublin Core, SKOS, Big Five
# Replace with your chosen standard ontology below.
# Remove this entire ontology section if using only custom data without ontology scaffolding.
standard_only = true

# Example: Using schema.org for e-commerce domain
# [[ontology.pack]]
# name = "schema-org"
# version = "^3.13.0"
# namespace = "https://schema.org/"

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

const DOMAIN_TTL: &str = r#"# ggen v6: Schema.org Example Ontology
# Uses schema.org (standard ontology) instead of custom namespace.
#
# In BIG BANG 80/20 mode, replace this with:
# 1. Your actual user data (CSV → RDF conversion)
# 2. A chosen standard ontology (schema.org, FOAF, Dublin Core, SKOS)
# 3. Only custom triples if schema.org + standard combo doesn't fit
#
# Why? Because Seth built 3-month custom ontology for what schema.org does in 5 minutes.

@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix schema: <https://schema.org/> .

# Example: Using schema.org Person and properties
# This is a real, standard vocabulary used by Google, Microsoft, Yahoo, Yandex
# See: https://schema.org/Person

schema:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "Schema.org Person type - standard vocabulary for person data" .

# Properties from schema.org
schema:name a rdf:Property ;
    rdfs:domain schema:Person ;
    rdfs:label "name" ;
    rdfs:comment "The name of the person (from schema.org)" .

schema:email a rdf:Property ;
    rdfs:domain schema:Person ;
    rdfs:label "email" ;
    rdfs:comment "The email address (from schema.org)" .

schema:age a rdf:Property ;
    rdfs:domain schema:Person ;
    rdfs:label "age" ;
    rdfs:comment "The age of the person (from schema.org)" .

# NEXT STEPS:
# 1. Load your actual CSV/JSON user data
# 2. Validate with 10 real users (not friends)
# 3. Only extend schema.org if needed (stay standard-first)
# 4. Run: ggen sync --validate-only
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
# Implements BIG BANG 80/20 screening gate before project can proceed
#
# Purpose: Prevent Seth-like patterns (custom ontologies, 3-month research, zero validation)
# by enforcing execution discipline: real data, standard ontologies, quick validation.

set -e

echo "🚀 ggen v6: BIG BANG 80/20 Screening Gate"
echo ""
echo "Before initializing, you must answer 5 questions about execution readiness."
echo "If you answer NO to any, stop and talk to Sean."
echo ""

# Screening Question 1: User Data
echo "❓ Question 1/5: Do you have real user data (CSV/JSON)?"
echo "   (Not promised. Actual files. If building a feature, do you have beta users' data?)"
echo "   Answer (yes/no):"
read -r q1
if [[ "$q1" != "yes" ]]; then
    echo "❌ STOP. You need real data to validate with. Build MVP first, use ggen after."
    exit 1
fi

# Screening Question 2: Standard Ontology
echo ""
echo "❓ Question 2/5: Can you find ONE existing standard ontology for your domain?"
echo "   (schema.org, FOAF, Dublin Core, SKOS - should take 5 min, not 3 months)"
echo "   Answer (yes/no):"
read -r q2
if [[ "$q2" != "yes" ]]; then
    echo "❌ STOP. You're about to build a custom ontology (Seth's mistake)."
    echo "   5 min: Find schema.org. 3 months: Build custom. Which path?"
    exit 1
fi

# Screening Question 3: Problem Articulation
echo ""
echo "❓ Question 3/5: Can you explain your problem in ONE sentence?"
echo "   (No 100-page documents. Just the core job-to-be-done.)"
echo "   Say it out loud, then answer (yes/no):"
read -r q3
if [[ "$q3" != "yes" ]]; then
    echo "❌ STOP. You don't have clarity. Write it down. One sentence. Try again."
    exit 1
fi

# Screening Question 4: Market Signal
echo ""
echo "❓ Question 4/5: Has anyone (not friends, not co-founders) committed to this?"
echo "   (Email list, signed beta contract, payment - PROOF, not enthusiasm)"
echo "   Answer (yes/no):"
read -r q4
if [[ "$q4" != "yes" ]]; then
    echo "⚠️  WARNING: Zero external validation. You're building in a vacuum."
    echo "   Proceed? (yes/no):"
    read -r q4_confirm
    if [[ "$q4_confirm" != "yes" ]]; then
        exit 1
    fi
fi

# Screening Question 5: Validation Speed
echo ""
echo "❓ Question 5/5: Can you validate with 10 real users in 48 hours?"
echo "   Answer (yes/no):"
read -r q5
if [[ "$q5" != "yes" ]]; then
    echo "⚠️  WARNING: You don't have a validation plan. How will you know if it works?"
    echo "   Proceed? (yes/no):"
    read -r q5_confirm
    if [[ "$q5_confirm" != "yes" ]]; then
        exit 1
    fi
fi

echo ""
echo "✅ Screening complete. You passed the litmus test."
echo ""

# Create directories if they don't exist
mkdir -p schema
mkdir -p templates
mkdir -p src/generated
mkdir -p scripts
mkdir -p data

echo "📁 Project structure created:"
echo "   schema/           - Your ontology files (use standard bases, not custom)"
echo "   data/             - Your real user data (CSV/JSON)"
echo "   templates/        - Tera templates for code generation"
echo "   src/generated/    - Generated code output"
echo "   scripts/          - Custom scripts"

echo ""
echo "📋 Next steps (in order):"
echo "  1. Add your actual user data to data/ (CSV or JSON)"
echo "  2. Edit schema/domain.ttl with standard ontology (schema.org, FOAF, Dublin Core, SKOS)"
echo "  3. Create Tera templates in templates/ for your target language"
echo "  4. Run 'ggen sync --validate-only' to test without writing"
echo "  5. Run 'ggen sync' to generate code"
echo "  6. Validate with your 10 real users (not friends)"
echo ""
echo "⚡ Speed targets:"
echo "   - Data upload: 1 hour"
echo "   - Ontology selection: 1 hour (use standard, don't build custom)"
echo "   - Template creation: 2-4 hours"
echo "   - First user validation: 24 hours"
echo ""
echo "📚 Resources:"
echo "  - schema.org: https://schema.org/ (Google, Microsoft, Yahoo - trusted)"
echo "  - FOAF: http://xmlns.com/foaf/spec/ (Social networks)"
echo "  - Dublin Core: http://dublincore.org/ (Metadata)"
echo "  - SKOS: https://www.w3.org/2004/02/skos/ (Controlled vocabularies)"
echo "  - ggen Docs: https://docs.ggen.io"
echo "  - RDF/Turtle: https://www.w3.org/TR/turtle/"
echo "  - Tera: https://keats.github.io/tera/"
echo ""
echo "💡 Remember: Seth's problem was building a custom 100-page ontology instead of"
echo "   using schema.org in 5 minutes. Stay disciplined. Use standards first."
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
            files_preserved: None,
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
            files_preserved: None,
            directories_created: vec![],
            error: Some(format!("Failed to create project directory: {}", e)),
            warning: None,
            next_steps: vec![],
        });
    }

    // Validate write permissions early - try creating a temp file
    let temp_test = base_path.join(".ggen_write_test");
    match fs::write(&temp_test, "") {
        Ok(_) => {
            let _ = fs::remove_file(&temp_test); // Clean up test file
        }
        Err(e) => {
            return Ok(InitOutput {
                status: "error".to_string(),
                project_dir: project_dir.to_string(),
                files_created: vec![],
                files_overwritten: None,
                files_preserved: None,
                directories_created: vec![],
                error: Some(format!(
                    "No write permission in project directory: {}",
                    e
                )),
                warning: None,
                next_steps: vec!["Check directory permissions or try a different location"
                    .to_string()],
            });
        }
    }

    let mut files_created = vec![];
    let mut files_overwritten = vec![];
    let mut files_preserved = vec![];
    let mut directories_created = vec![];
    let mut errors = vec![];

    // Create directories - only track as "created" if they didn't exist
    let dirs = vec!["schema", "templates", "src/generated", "scripts"];
    for dir in &dirs {
        let dir_path = base_path.join(dir);
        let existed = dir_path.exists();
        match fs::create_dir_all(&dir_path) {
            Ok(_) => {
                if !existed {
                    directories_created.push(dir.to_string());
                }
            }
            Err(e) => {
                errors.push(format!("Failed to create directory {}: {}", dir, e));
            }
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
    write_file(&startup_sh_path, STARTUP_SH, "scripts/startup.sh");

    // Create .gitignore (only if it doesn't exist - preserve user's gitignore)
    let gitignore_path = base_path.join(".gitignore");
    if !gitignore_path.exists() {
        let gitignore_content = "# ggen outputs\nsrc/generated/\n.ggen/\n";
        write_file(&gitignore_path, gitignore_content, ".gitignore");
    } else {
        files_preserved.push(".gitignore".to_string());
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
    } else {
        files_preserved.push("README.md".to_string());
    }

    // Drop the closure so we can borrow errors again
    drop(write_file);

    // Always ensure startup.sh is executable on Unix systems
    // (important for both new files AND overwrites)
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        if let Err(e) = fs::set_permissions(
            &startup_sh_path,
            std::fs::Permissions::from_mode(0o755),
        ) {
            errors.push(format!(
                "Warning: Could not set execute permissions on startup.sh: {}",
                e
            ));
        }
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

    // Build warning message with details about what happened
    let warning = if !files_overwritten.is_empty() || !files_preserved.is_empty() {
        let mut msgs = vec![];
        if !files_overwritten.is_empty() {
            msgs.push(format!("Overwrote {} file(s)", files_overwritten.len()));
        }
        if !files_preserved.is_empty() {
            msgs.push(format!("Preserved {} user file(s)", files_preserved.len()));
        }
        Some(msgs.join("; ") + ".")
    } else {
        None
    };

    let files_overwritten_opt = if files_overwritten.is_empty() {
        None
    } else {
        Some(files_overwritten)
    };

    let files_preserved_opt = if files_preserved.is_empty() {
        None
    } else {
        Some(files_preserved)
    };

    Ok(InitOutput {
        status,
        project_dir: project_dir.to_string(),
        files_created,
        files_overwritten: files_overwritten_opt,
        files_preserved: files_preserved_opt,
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
