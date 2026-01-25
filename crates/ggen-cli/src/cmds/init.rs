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
//!
//! ## Atomic Initialization
//!
//! Uses FileTransaction for atomic file operations with automatic rollback on failure.
//! Either all files are created successfully, or no changes are made.

#![allow(clippy::unused_unit)] // clap-noun-verb macro generates this

use clap_noun_verb_macros::verb;
use ggen_core::codegen::FileTransaction;
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

    /// Transaction details (if atomic operation succeeded)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub transaction: Option<TransactionInfo>,

    /// Git hooks installation result
    #[serde(skip_serializing_if = "Option::is_none")]
    pub git_hooks: Option<super::git_hooks::HooksInstallOutput>,
}

/// Transaction information for atomic init operation
#[derive(Debug, Clone, Serialize)]
pub struct TransactionInfo {
    /// Total files affected by transaction
    pub total_files: usize,

    /// Number of backups created
    pub backups_created: usize,

    /// Whether transaction was committed successfully
    pub committed: bool,
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
# REQUIRED: Path to your RDF ontology file (Turtle format)
# Approved: schema.org, FOAF, Dublin Core, SKOS, Big Five
# Replace with your chosen standard ontology below.
source = "schema/domain.ttl"
# Use standard ontologies only (BIG BANG 80/20 gate)
standard_only = true

# Example: Using schema.org for e-commerce domain
# [[ontology.pack]]
# name = "schema-org"
# version = "^3.13.0"
# namespace = "https://schema.org/"

[generation]
output_dir = "src/generated/"

# Define at least one generation rule (required)
# This example rule generates Rust structs from the Person ontology
[[generation.rules]]
name = "example-rule"
# SPARQL SELECT query to extract ontology concepts
query = { inline = """
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX schema: <https://schema.org/>

SELECT ?class ?label ?comment
WHERE {
  ?class a rdfs:Class ;
         rdfs:label ?label .
  OPTIONAL { ?class rdfs:comment ?comment . }
}
LIMIT 10
""" }
# Template file to render (relative to templates directory)
template = { file = "templates/example.txt.tera" }
# Output file path (relative to output_dir)
output_file = "ontology-summary.txt"
# How to handle existing output files
mode = "Overwrite"

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

const EXAMPLE_TEMPLATE: &str = r#"# Example Tera Template for ggen
# This template is called by the generation.rules in ggen.toml
# It receives data from the SPARQL query and renders output

# Ontology Classes from schema.org
{% for row in results %}
## Class: {{ row.label | default(value="Unknown") }}

{% if row.comment %}
{{ row.comment }}
{% else %}
(No description provided)
{% endif %}

**Identifier**: `{{ row.class }}`

---
{% endfor %}

*Generated by ggen v6*
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
/// --skip-hooks              Skip git hooks installation
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
pub fn init(
    path: Option<String>,
    force: Option<bool>,
    skip_hooks: Option<bool>,
) -> clap_noun_verb::Result<InitOutput> {
    // Thin CLI layer: parse arguments and delegate to helper
    let project_dir = path.unwrap_or_else(|| ".".to_string());
    let force = force.unwrap_or(false);
    let skip_hooks = skip_hooks.unwrap_or(false);

    // Delegate to initialization logic
    perform_init(&project_dir, force, skip_hooks)
}

/// Helper function that performs the actual initialization.
///
/// This function contains all the file I/O and directory creation logic,
/// keeping the verb function thin and following the CLI architecture pattern.
///
/// ## Atomic Initialization Strategy
///
/// 1. Pre-flight checks (directory exists, artifacts present, permissions)
/// 2. Create FileTransaction for atomic file operations
/// 3. Create directories (tracked separately, not part of transaction)
/// 4. Write all files via transaction (automatic backup of existing files)
/// 5. Set permissions on startup.sh
/// 6. Commit transaction (point of no return)
/// 7. Build InitOutput from TransactionReceipt
///
/// Any error before commit triggers automatic rollback via Drop trait.
fn perform_init(
    project_dir: &str,
    force: bool,
    skip_hooks: bool,
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
            transaction: None,
            git_hooks: None,
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
            transaction: None,
            git_hooks: None,
        });
    }

    // Pre-flight validation: Check disk space and basic environment
    let preflight = ggen_core::validation::PreFlightValidator::for_init(base_path);
    if let Err(e) = preflight.validate(None) {
        return Ok(InitOutput {
            status: "error".to_string(),
            project_dir: project_dir.to_string(),
            files_created: vec![],
            files_overwritten: None,
            files_preserved: None,
            directories_created: vec![],
            error: Some(format!("{}", e)),
            warning: None,
            next_steps: vec![
                "Ensure you have at least 100MB of free disk space".to_string(),
                "Verify write permissions to the target directory".to_string(),
            ],
            transaction: None,
            git_hooks: None,
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
                transaction: None,
                git_hooks: None,
            });
        }
    }

    // Create FileTransaction for atomic file operations
    let mut tx = FileTransaction::new().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to initialize file transaction: {}", e))
    })?;

    let mut directories_created = vec![];
    let mut files_preserved = vec![];

    // Create directories - tracked separately (not part of transaction)
    // These won't be automatically rolled back, but that's acceptable for empty directories
    let dirs = vec!["schema", "templates", "src/generated", "scripts"];
    for dir in &dirs {
        let dir_path = base_path.join(dir);
        let existed = dir_path.exists();
        fs::create_dir_all(&dir_path).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to create directory {}: {}", dir, e))
        })?;
        if !existed {
            directories_created.push(dir.to_string());
        }
    }

    // Check which conditional files to preserve
    let gitignore_path = base_path.join(".gitignore");
    let gitignore_exists = gitignore_path.exists();
    if gitignore_exists {
        files_preserved.push(".gitignore".to_string());
    }

    let readme_path = base_path.join("README.md");
    let readme_exists = readme_path.exists();
    if readme_exists {
        files_preserved.push("README.md".to_string());
    }

    // Write all files via transaction
    // Any error here will trigger automatic rollback via Drop

    // Create ggen.toml
    let toml_path = base_path.join("ggen.toml");
    tx.write_file(&toml_path, GGEN_TOML).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write ggen.toml: {}", e))
    })?;

    // Create schema/domain.ttl
    let schema_path = base_path.join("schema").join("domain.ttl");
    tx.write_file(&schema_path, DOMAIN_TTL).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write schema/domain.ttl: {}", e))
    })?;

    // Create Makefile
    let makefile_path = base_path.join("Makefile");
    tx.write_file(&makefile_path, MAKEFILE).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write Makefile: {}", e))
    })?;

    // Create example template (templates/example.txt.tera)
    let template_path = base_path.join("templates").join("example.txt.tera");
    tx.write_file(&template_path, EXAMPLE_TEMPLATE).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write templates/example.txt.tera: {}", e))
    })?;

    // Create scripts/startup.sh
    let startup_sh_path = base_path.join("scripts").join("startup.sh");
    tx.write_file(&startup_sh_path, STARTUP_SH).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write scripts/startup.sh: {}", e))
    })?;

    // Create .gitignore (only if it doesn't exist - preserve user's gitignore)
    if !gitignore_exists {
        let gitignore_content = "# ggen outputs\nsrc/generated/\n.ggen/\n";
        tx.write_file(&gitignore_path, gitignore_content).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to write .gitignore: {}", e))
        })?;
    }

    // Create README.md (only if it doesn't exist - preserve user's README)
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
    if !readme_exists {
        tx.write_file(&readme_path, readme_content).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to write README.md: {}", e))
        })?;
    }

    // Set executable permissions on startup.sh before commit
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(
            &startup_sh_path,
            std::fs::Permissions::from_mode(0o755),
        ).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to set execute permissions on startup.sh: {}", e))
        })?;
    }

    // Commit transaction - this is the point of no return
    // After this, all changes are permanent and rollback is disabled
    let receipt = tx.commit().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to commit file transaction: {}", e))
    })?;

    // Install git hooks after successful file creation
    let git_hooks_result = super::git_hooks::install_git_hooks(base_path, skip_hooks)
        .ok(); // Convert to Option, don't fail init if hooks fail

    // Build InitOutput from TransactionReceipt
    let files_created: Vec<String> = receipt
        .files_created
        .iter()
        .filter_map(|p| {
            // Convert absolute paths to relative paths for display
            p.strip_prefix(base_path)
                .ok()
                .map(|rel| rel.display().to_string())
        })
        .collect();

    let files_modified: Vec<String> = receipt
        .files_modified
        .iter()
        .filter_map(|p| {
            p.strip_prefix(base_path)
                .ok()
                .map(|rel| rel.display().to_string())
        })
        .collect();

    let files_overwritten_opt = if files_modified.is_empty() {
        None
    } else {
        Some(files_modified.clone())
    };

    let files_preserved_opt = if files_preserved.is_empty() {
        None
    } else {
        Some(files_preserved.clone())
    };

    // Build warning message with details about what happened
    let warning = if !files_modified.is_empty() || !files_preserved.is_empty() {
        let mut msgs = vec![];
        if !files_modified.is_empty() {
            msgs.push(format!("Overwrote {} file(s)", files_modified.len()));
        }
        if !files_preserved.is_empty() {
            msgs.push(format!("Preserved {} user file(s)", files_preserved.len()));
        }
        Some(msgs.join("; ") + ".")
    } else {
        None
    };

    Ok(InitOutput {
        status: "success".to_string(),
        project_dir: project_dir.to_string(),
        files_created,
        files_overwritten: files_overwritten_opt,
        files_preserved: files_preserved_opt,
        directories_created,
        error: None,
        warning,
        next_steps: vec![
            "Run 'make setup' to initialize your project".to_string(),
            "Edit schema/domain.ttl to define your domain model".to_string(),
            "Create Tera templates in templates/ for your target languages".to_string(),
            "Run 'make build' to generate code from your ontology".to_string(),
        ],
        transaction: Some(TransactionInfo {
            total_files: receipt.total_files(),
            backups_created: receipt.backups.len(),
            committed: true,
        }),
        git_hooks: git_hooks_result,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use tempfile::tempdir;

    #[test]
    fn test_atomic_init_success() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let project_path = temp_dir.path().to_str().expect("Invalid path");

        let result = perform_init(project_path, false, true)
            .expect("Init should succeed");

        // Verify success status
        assert_eq!(result.status, "success");
        assert!(result.error.is_none());

        // Verify transaction info
        assert!(result.transaction.is_some());
        let tx_info = result.transaction.unwrap();
        assert!(tx_info.total_files > 0, "Should have created files");
        assert!(tx_info.committed, "Transaction should be committed");

        // Verify files were created
        assert!(!result.files_created.is_empty(), "Should have created files");
        assert!(result.directories_created.len() >= 4, "Should have created at least 4 directories");

        // Verify actual files exist on disk
        let base = PathBuf::from(project_path);
        assert!(base.join("ggen.toml").exists(), "ggen.toml should exist");
        assert!(base.join("schema/domain.ttl").exists(), "domain.ttl should exist");
        assert!(base.join("Makefile").exists(), "Makefile should exist");
        assert!(base.join("scripts/startup.sh").exists(), "startup.sh should exist");
        assert!(base.join("templates/example.txt.tera").exists(), "example.txt.tera should exist");
    }

    #[test]
    fn test_init_preserves_existing_files() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let project_path = temp_dir.path().to_str().expect("Invalid path");
        let base = PathBuf::from(project_path);

        // Create existing .gitignore and README.md with custom content
        fs::create_dir_all(&base).expect("Failed to create base dir");
        let gitignore_content = "# Custom gitignore\n*.log\n";
        let readme_content = "# Custom README\n\nMy project\n";
        fs::write(base.join(".gitignore"), gitignore_content)
            .expect("Failed to write .gitignore");
        fs::write(base.join("README.md"), readme_content)
            .expect("Failed to write README.md");

        let result = perform_init(project_path, false, true)
            .expect("Init should succeed");

        // Verify files were preserved
        assert!(result.files_preserved.is_some(), "Should have preserved files");
        let preserved = result.files_preserved.unwrap();
        assert!(preserved.contains(&".gitignore".to_string()), "Should preserve .gitignore");
        assert!(preserved.contains(&"README.md".to_string()), "Should preserve README.md");

        // Verify original content is intact
        let gitignore_after = fs::read_to_string(base.join(".gitignore"))
            .expect("Failed to read .gitignore");
        let readme_after = fs::read_to_string(base.join("README.md"))
            .expect("Failed to read README.md");
        assert_eq!(gitignore_after, gitignore_content, ".gitignore should be unchanged");
        assert_eq!(readme_after, readme_content, "README.md should be unchanged");
    }

    #[test]
    fn test_init_force_overwrites_files() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let project_path = temp_dir.path().to_str().expect("Invalid path");
        let base = PathBuf::from(project_path);

        // First init
        perform_init(project_path, false, true)
            .expect("First init should succeed");

        // Modify a file
        let toml_path = base.join("ggen.toml");
        let original_content = fs::read_to_string(&toml_path)
            .expect("Failed to read ggen.toml");
        fs::write(&toml_path, "# Modified\n").expect("Failed to modify ggen.toml");

        // Second init without force should fail
        let result = perform_init(project_path, false, true)
            .expect("Should return result");
        assert_eq!(result.status, "error");
        assert!(result.error.is_some());
        assert!(result.error.unwrap().contains("already initialized"));

        // Second init with force should succeed
        let result = perform_init(project_path, true, true)
            .expect("Force init should succeed");
        assert_eq!(result.status, "success");
        assert!(result.files_overwritten.is_some(), "Should have overwritten files");

        // Verify original content was restored
        let restored_content = fs::read_to_string(&toml_path)
            .expect("Failed to read ggen.toml");
        assert_eq!(restored_content, original_content, "Content should be restored");
    }

    #[test]
    fn test_transaction_receipt_tracking() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let project_path = temp_dir.path().to_str().expect("Invalid path");

        let result = perform_init(project_path, false, true)
            .expect("Init should succeed");

        // Verify transaction info is present and accurate
        assert!(result.transaction.is_some());
        let tx_info = result.transaction.unwrap();

        // Total files should match created files count
        assert_eq!(
            tx_info.total_files,
            result.files_created.len(),
            "Transaction total_files should match files_created count"
        );

        // No backups should be created on fresh init
        assert_eq!(tx_info.backups_created, 0, "No backups on fresh init");

        // Transaction should be committed
        assert!(tx_info.committed, "Transaction should be committed");
    }

    #[test]
    fn test_transaction_creates_backups_on_overwrite() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let project_path = temp_dir.path().to_str().expect("Invalid path");

        // First init
        perform_init(project_path, false, true)
            .expect("First init should succeed");

        // Force re-init
        let result = perform_init(project_path, true, true)
            .expect("Force init should succeed");

        // Verify backups were created
        assert!(result.transaction.is_some());
        let tx_info = result.transaction.unwrap();
        assert!(tx_info.backups_created > 0, "Should have created backups on overwrite");
    }

    #[test]
    fn test_init_creates_all_required_directories() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let project_path = temp_dir.path().to_str().expect("Invalid path");
        let base = PathBuf::from(project_path);

        let result = perform_init(project_path, false, true)
            .expect("Init should succeed");

        // Verify all required directories exist
        assert!(base.join("schema").is_dir(), "schema/ should exist");
        assert!(base.join("templates").is_dir(), "templates/ should exist");
        assert!(base.join("src/generated").is_dir(), "src/generated/ should exist");
        assert!(base.join("scripts").is_dir(), "scripts/ should exist");

        // Verify directories_created list
        let dirs = &result.directories_created;
        assert!(dirs.contains(&"schema".to_string()), "Should report schema/ created");
        assert!(dirs.contains(&"templates".to_string()), "Should report templates/ created");
        assert!(dirs.contains(&"src/generated".to_string()), "Should report src/generated/ created");
        assert!(dirs.contains(&"scripts".to_string()), "Should report scripts/ created");
    }

    #[test]
    #[cfg(unix)]
    fn test_startup_sh_is_executable() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let project_path = temp_dir.path().to_str().expect("Invalid path");
        let base = PathBuf::from(project_path);

        perform_init(project_path, false, true)
            .expect("Init should succeed");

        // Verify startup.sh has executable permissions
        let startup_path = base.join("scripts/startup.sh");
        let metadata = fs::metadata(&startup_path)
            .expect("Failed to get startup.sh metadata");

        use std::os::unix::fs::PermissionsExt;
        let mode = metadata.permissions().mode();
        assert!(mode & 0o111 != 0, "startup.sh should be executable");
    }

    #[test]
    fn test_init_output_structure() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let project_path = temp_dir.path().to_str().expect("Invalid path");

        let result = perform_init(project_path, false, true)
            .expect("Init should succeed");

        // Verify InitOutput structure
        assert_eq!(result.status, "success");
        assert_eq!(result.project_dir, project_path);
        assert!(!result.files_created.is_empty());
        assert!(result.error.is_none());
        assert!(!result.next_steps.is_empty());
        assert!(result.transaction.is_some());

        // Verify serialization works (for JSON output)
        let json = serde_json::to_string(&result)
            .expect("Should serialize to JSON");
        assert!(json.contains("\"status\":\"success\""));
        assert!(json.contains("\"transaction\""));
    }
}
