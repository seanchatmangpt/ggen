# Git Hooks & Ontology Evolution Workflow

**ggen v2.6.0** | **Ontology-Driven Development** | **Automated Code Evolution**

---

## Overview

ggen integrates **Git hooks** with **RDF ontology tracking** to create a **continuous regeneration workflow**. When you modify your domain model (RDF/Turtle files), Git hooks automatically detect changes and can trigger code regeneration, validation, and quality checks.

### Key Concepts

- **Git Hooks**: Custom scripts that run at specific Git lifecycle events (pre-commit, post-commit, pre-push)
- **Ontology Files**: RDF/Turtle (.ttl) files defining your domain model as semantic graphs
- **Code Generation**: Automatic creation of Rust/TypeScript/SQL code from ontology definitions
- **Schema Evolution**: Tracking how your domain model changes over time

### Benefits

✅ **Automated Validation** - Catch ontology errors before commit
✅ **Continuous Regeneration** - Code stays in sync with domain model
✅ **Version Control** - Track ontology evolution alongside code
✅ **Type Safety** - Schema changes propagate to all generated code
✅ **Zero Manual Sync** - No risk of frontend/backend drift

---

## Table of Contents

1. [Hook Types](#hook-types)
2. [Ontology Generation](#ontology-generation)
3. [Workflow Examples](#workflow-examples)
4. [Best Practices](#best-practices)
5. [RDF Schema Format](#rdf-schema-format)
6. [SPARQL Queries](#sparql-queries)
7. [Troubleshooting](#troubleshooting)

---

## Hook Types

### Pre-Commit Hook

**File**: `crates/ggen-utils/src/bin/git_hook_pre_commit.rs`

**Triggers**: Before Git commit (blocks bad commits)

**Purpose**: Fast validation (2-5 seconds) to prevent broken code from entering version control

**Checks Performed**:

1. **No `unwrap()` in production code** - Enforces proper error handling
2. **No `unimplemented!()` placeholders** (main branch only)
3. **No TODO/FUTURE comments** (main branch only)
4. **No `expect()` in production code** (except CLI layer)
5. **Code formatting** - Runs `cargo make fmt --check`
6. **Clippy warnings** - Runs `cargo make lint` on staged packages

**Branch Behavior**:
- **Main branch**: Strict rules (no TODO/unimplemented allowed)
- **Feature branches**: Relaxed rules (TODO/unimplemented allowed for WIP)

**Installation**:

```bash
# Build and install hooks
cargo run --bin git_hook_installer --package ggen-utils

# Hooks are copied to .git/hooks/ as native binaries (no bash wrappers)
```

**Example Output**:

```
🔍 Running pre-commit validation...
🔒 Main branch detected - enforcing strict rules (no TODO/FUTURE/unimplemented!)
   Checking for unwrap() calls in production code...
  ✅ No unwrap() in production code
   Checking for unimplemented!() placeholders...
  ✅ No unimplemented!() placeholders
   Checking for FUTURE/TODO comments...
  ✅ No FUTURE/TODO comments
   Checking for expect() calls in production code...
  ✅ No expect() in production code (CLI exempt)
   Checking Rust formatting...
  ✅ Code is formatted
   Running clippy on staged packages...
  ✅ Clippy checks passed
✅ Pre-commit validation passed
```

---

### Pre-Push Hook

**File**: `crates/ggen-utils/src/bin/git_hook_pre_push.rs`

**Triggers**: Before `git push` (blocks broken pushes)

**Purpose**: Comprehensive validation (5-10 minutes) to ensure production readiness

**Checks Performed**:

1. **Full test suite** - All tests must pass
2. **Build verification** - `cargo build --release`
3. **Ontology validation** - SHACL constraints and SPARQL queries
4. **Security audit** - `cargo audit` for dependency vulnerabilities
5. **Documentation build** - `cargo doc --no-deps`

**Timeout Management**:

The pre-push hook uses `cargo make check-pre-push` which has a **10-minute timeout** configured in `Makefile.toml`:

```toml
[tasks.check-pre-push]
description = "Fast pre-push check (10min timeout)"
workspace = false
run_task = { name = ["test-fast", "lint", "build"] }
```

This prevents infinite hangs while allowing comprehensive validation.

---

### Post-Commit Hook (Ontology Tracking)

**Purpose**: Track ontology changes and optionally regenerate code

**Implementation**: Uses `ggen hook monitor` to watch `.ttl` files

**Workflow**:

```bash
# Setup automatic regeneration on ontology changes
ggen hook create \
  --event on-ontology-change \
  --script ./scripts/regen-on-commit.sh \
  --name "auto-regenerate"

# Monitor ontology files
ggen hook monitor --graph domain.ttl &
```

**Example Script** (`scripts/regen-on-commit.sh`):

```bash
#!/bin/bash
set -e

ONTOLOGY_FILE=$1
echo "📝 Ontology changed: $ONTOLOGY_FILE"

# Backup current code
BACKUP_DIR=".backups/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"
tar -czf "$BACKUP_DIR/pre-regen.tar.gz" src/

# Regenerate Rust models
echo "🔄 Regenerating Rust models..."
ggen template generate-rdf \
  --ontology "$ONTOLOGY_FILE" \
  --template rust-models \
  --output-dir src/models

# Regenerate TypeScript types
echo "🔄 Regenerating TypeScript types..."
ggen template generate-rdf \
  --ontology "$ONTOLOGY_FILE" \
  --template typescript-models \
  --output-dir frontend/src/types

# Regenerate SQL schema
echo "🔄 Regenerating database schema..."
ggen template generate-rdf \
  --ontology "$ONTOLOGY_FILE" \
  --template sql-schema \
  --output-dir migrations

# Verify regeneration with tests
if cargo test; then
  echo "✅ Regeneration successful!"
  git add src/ frontend/ migrations/
  git commit -m "chore: Regenerate code from ontology changes"
else
  echo "❌ Tests failed! Restoring backup..."
  tar -xzf "$BACKUP_DIR/pre-regen.tar.gz"
  exit 1
fi
```

---

## Ontology Generation

### How Git Hooks Integrate with ggen

```
┌─────────────────────────────────────────────────────────────┐
│                    Git Lifecycle Events                     │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
         ┌─────────────────────────────────────┐
         │       Pre-Commit Hook (Fast)        │
         │  ✓ Validate ontology syntax         │
         │  ✓ Check code quality               │
         │  ✓ Run formatters                   │
         └─────────────────────────────────────┘
                           │
                           ▼ (commit allowed)
         ┌─────────────────────────────────────┐
         │    Post-Commit Hook (Optional)      │
         │  ✓ Detect .ttl file changes         │
         │  ✓ Trigger code regeneration        │
         │  ✓ Update generated artifacts       │
         └─────────────────────────────────────┘
                           │
                           ▼
         ┌─────────────────────────────────────┐
         │    Pre-Push Hook (Comprehensive)    │
         │  ✓ Full test suite                  │
         │  ✓ Build verification               │
         │  ✓ SHACL/SPARQL validation          │
         │  ✓ Security audit                   │
         └─────────────────────────────────────┘
                           │
                           ▼ (push allowed)
         ┌─────────────────────────────────────┐
         │         Remote Repository           │
         │  ✓ CI/CD triggered                  │
         │  ✓ Documentation deployed           │
         └─────────────────────────────────────┘
```

### Ontology File Locations

**Standard Locations**:
- **Domain models**: `domain.ttl`, `ontology/schema.ttl`
- **Template metadata**: `template.ttl` (per template directory)
- **Generated RDF**: `.ggen/ontology/` (tracked by Git)

**File Watching**:

ggen hooks monitor these patterns:
- `**/*.ttl` - Turtle RDF files
- `**/*.rdf` - RDF/XML files
- `**/*.owl` - OWL ontology files

---

## Workflow Examples

### Workflow 1: Initial Commit with Ontology

**Scenario**: First commit of a new project with domain model

```bash
# 1. Create domain ontology
cat > domain.ttl << 'EOF'
@prefix : <http://example.org/blog#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

:User a owl:Class ;
  rdfs:label "User" ;
  rdfs:comment "A registered user" .

:email a owl:DatatypeProperty ;
  rdfs:domain :User ;
  rdfs:range xsd:string .
EOF

# 2. Generate initial code
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output-dir src/models

# 3. Commit (pre-commit hook validates)
git add domain.ttl src/
git commit -m "feat: Add initial User domain model"
# → Pre-commit hook runs:
#    ✓ Validates domain.ttl syntax
#    ✓ Checks generated code formatting
#    ✓ Runs clippy on new code
```

---

### Workflow 2: Adding New Code (Schema Evolution)

**Scenario**: Add a new `Post` entity to existing ontology

```bash
# 1. Edit ontology
cat >> domain.ttl << 'EOF'

:Post a owl:Class ;
  rdfs:label "Post" ;
  rdfs:comment "A blog post" .

:title a owl:DatatypeProperty ;
  rdfs:domain :Post ;
  rdfs:range xsd:string .

:hasAuthor a owl:ObjectProperty ;
  rdfs:domain :Post ;
  rdfs:range :User .
EOF

# 2. Regenerate code (automated if hook setup)
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output-dir src/models
# → Generates new src/models/post.rs
# → Updates src/models/user.rs (adds hasPosts relationship)

# 3. Run tests to verify
cargo test
# → Tests catch any breaking changes

# 4. Commit changes
git add domain.ttl src/models/
git commit -m "feat: Add Post entity with User relationship"
# → Pre-commit hook validates:
#    ✓ Updated ontology is valid
#    ✓ New code compiles
#    ✓ No unwrap/TODO violations
```

---

### Workflow 3: Refactoring (Breaking Changes)

**Scenario**: Rename `email` → `emailAddress` (breaking change)

```bash
# 1. Update ontology
sed -i 's/:email/:emailAddress/g' domain.ttl

# 2. Regenerate code
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output-dir src/models
# → Field renamed in all generated structs

# 3. Fix dependent code (compiler errors guide you)
# Update API handlers, database queries, etc.

# 4. Update database migration
ggen template generate-rdf \
  --ontology domain.ttl \
  --template sql-schema \
  --output-dir migrations
# → Generates ALTER TABLE migration

# 5. Run full test suite
cargo test
# → Ensures nothing breaks

# 6. Commit with clear message
git add domain.ttl src/ migrations/
git commit -m "refactor!: Rename User.email to User.emailAddress

BREAKING CHANGE: User.email field renamed to User.emailAddress.
Update API clients to use new field name."
# → Pre-commit hook ensures quality
# → Pre-push hook runs full validation
```

---

## Best Practices

### 1. Ontology Versioning

**Track ontology versions explicitly**:

```turtle
@prefix : <http://example.org/blog#> .

: a owl:Ontology ;
  owl:versionInfo "2.1.0" ;
  rdfs:label "Blog Platform Ontology v2.1.0" ;
  rdfs:comment "Added comment upvoting feature" .
```

**Semantic Versioning**:
- **Major** (3.0.0): Breaking changes (rename fields, remove classes)
- **Minor** (2.1.0): Backward-compatible additions (new classes/properties)
- **Patch** (2.0.1): Documentation/comment updates only

---

### 2. Schema Evolution Strategy

**Use additive changes when possible**:

```turtle
# ✅ Good: Add optional property
:phoneNumber a owl:DatatypeProperty ;
  rdfs:domain :User ;
  rdfs:range xsd:string .

# ❌ Bad: Remove required property
# :email a owl:DatatypeProperty .  # Commented out = breaking change
```

**Deprecation pattern**:

```turtle
# Mark old property as deprecated
:email a owl:DatatypeProperty ;
  owl:deprecated true ;
  rdfs:comment "DEPRECATED: Use emailAddress instead. Will be removed in v3.0" .

# Add new property
:emailAddress a owl:DatatypeProperty ;
  rdfs:domain :User ;
  rdfs:range xsd:string .
```

---

### 3. Conflict Resolution

**When ontology and code diverge**:

```bash
# 1. Check which is source of truth
git log --oneline -- domain.ttl src/models/

# 2. If ontology is newer, regenerate code
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output-dir src/models \
  --force  # Overwrite manual changes

# 3. If code has manual changes you want to keep:
# → Extract changes to separate files (not generated)
# → Update ontology to reflect manual additions
# → Regenerate to verify consistency
```

**Prevention**:
- **Never edit generated files** - Mark with `// AUTO-GENERATED - DO NOT EDIT`
- **Keep manual code separate** - Use `src/models/` (generated) + `src/custom/` (manual)
- **Document extensions** - Add `custom_models.ttl` for non-generated entities

---

### 4. Git Hook Best Practices

**Make hooks fast** (pre-commit < 5 seconds):

```rust
// Only check staged files, not entire workspace
fn get_staged_rust_files() -> Vec<PathBuf> {
    Command::new("git")
        .arg("diff")
        .arg("--cached")
        .arg("--name-only")
        .arg("--diff-filter=d")
        .output()
        // Filter for *.rs files only
}
```

**Use progressive validation**:
- **Pre-commit**: Fast checks (syntax, formatting)
- **Pre-push**: Comprehensive checks (tests, build, security)
- **CI/CD**: Full validation (integration tests, performance)

**Provide clear feedback**:

```rust
if count > 0 {
    eprintln!("❌ ERROR: Cannot commit {} unwrap() calls", count);
    eprintln!("   Replace with proper Result<T,E> error handling");
    eprintln!("   Use ? operator or match statements instead");
    return ExitCode::FAILURE;
}
```

---

## RDF Schema Format

### Template Metadata (`.ggen/template.ttl`)

Every ggen template includes RDF metadata:

```turtle
@prefix ggen: <http://ggen.dev/ontology#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<http://ggen.dev/templates/rust-cli> a ggen:Template ;
  ggen:templateName "rust-cli" ;
  ggen:version "2.5.0" ;
  ggen:description "Production-ready Rust CLI with clap noun-verb" ;
  ggen:stability "stable" ;
  ggen:category "cli" ;
  ggen:tags "rust", "cli", "clap" ;
  ggen:testCoverage 92.5 ;

  ggen:hasVariable [
    ggen:variableName "project_name" ;
    ggen:variableType "string" ;
    ggen:isRequired true ;
    ggen:description "Name of the CLI project"
  ] ;

  ggen:generatesFile "Cargo.toml" ;
  ggen:generatesFile "src/main.rs" ;
  ggen:generatesFile "src/cmds/mod.rs" .
```

**SHACL Validation** (automatic):

```turtle
ggen:TemplateShape a sh:NodeShape ;
  sh:targetClass ggen:Template ;
  sh:property [
    sh:path ggen:templateName ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string
  ] ;
  sh:property [
    sh:path ggen:version ;
    sh:pattern "^\\d+\\.\\d+\\.\\d+$"  # Semantic versioning
  ] .
```

---

## SPARQL Queries

### Query Ontology Changes

**List all classes**:

```bash
ggen graph query domain.ttl --sparql "
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

  SELECT ?class ?label ?comment WHERE {
    ?class a owl:Class ;
           rdfs:label ?label .
    OPTIONAL { ?class rdfs:comment ?comment }
  }
  ORDER BY ?label
"
```

**Find properties for a class**:

```bash
ggen graph query domain.ttl --sparql "
  PREFIX : <http://example.org/blog#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

  SELECT ?property ?label ?range WHERE {
    ?property rdfs:domain :User ;
              rdfs:label ?label ;
              rdfs:range ?range .
  }
"
```

**Validate ontology completeness**:

```bash
ggen graph query domain.ttl --sparql "
  PREFIX owl: <http://www.w3.org/2002/07/owl#>

  # Find classes without comments
  SELECT ?class WHERE {
    ?class a owl:Class .
    FILTER NOT EXISTS { ?class rdfs:comment ?comment }
  }
"
```

---

## Troubleshooting

### Hook Not Running

**Problem**: Pre-commit hook doesn't execute

**Solution**:

```bash
# 1. Check hook exists
ls -la .git/hooks/pre-commit

# 2. Verify executable bit
chmod +x .git/hooks/pre-commit

# 3. Reinstall hooks
cargo run --bin git_hook_installer --package ggen-utils

# 4. Test manually
.git/hooks/pre-commit
```

---

### Ontology Validation Fails

**Problem**: Ontology has syntax errors

**Solution**:

```bash
# 1. Load ontology to see errors
ggen graph load domain.ttl
# → Shows line numbers of syntax errors

# 2. Validate with external tools
riot --validate domain.ttl

# 3. Check common issues:
# - Missing prefixes
# - Unclosed literals
# - Invalid URIs
# - Missing trailing dots
```

---

### Code Generation Fails

**Problem**: Regeneration produces broken code

**Solution**:

```bash
# 1. Check ontology is valid
ggen graph load domain.ttl

# 2. Regenerate with verbose output
RUST_LOG=debug ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output-dir src/models

# 3. Check for unsupported types
# Some XSD types may not map cleanly to Rust

# 4. Review generated code
git diff src/models/
```

---

### Merge Conflicts in Ontology

**Problem**: Git merge conflict in `.ttl` files

**Solution**:

```bash
# 1. Use RDF-aware merge tool
# (Standard Git merge works but may break syntax)

# 2. Manual resolution:
# - Keep both branches' classes/properties
# - Resolve duplicate IRIs
# - Validate after merge

# 3. Validate merged ontology
ggen graph load domain.ttl

# 4. Regenerate code from merged ontology
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output-dir src/models
```

---

## Summary

### Ontology-Driven Git Workflow

1. **Define domain model** in RDF/Turtle files
2. **Generate code** from ontology (Rust, TypeScript, SQL)
3. **Git hooks validate** ontology and code quality
4. **Commit changes** - pre-commit hook prevents errors
5. **Push changes** - pre-push hook ensures production readiness
6. **Evolve schema** - regenerate code automatically
7. **Track versions** - ontology versions align with releases

### Key Files

- **Hooks**: `crates/ggen-utils/src/bin/git_hook_*.rs`
- **Installer**: `crates/ggen-utils/src/bin/git_hook_installer.rs`
- **Ontology**: `domain.ttl`, `template.ttl`
- **Validation**: `crates/ggen-domain/src/rdf/validation.rs`
- **Makefile**: `Makefile.toml` (defines `check-pre-push` task)

### Resources

- [Hooks System Guide](./src/guides/hooks.md)
- [Ontology-Driven Tutorial](./src/tutorials/ontology-driven-workflow.md)
- [RDF Marketplace Patterns](./MARKETPLACE_RDF_PATTERNS.md)
- [Template Validation](./src/concepts/rdf-shacl-sparql.md)

---

**Questions?** See [GitHub Issues](https://github.com/ggen-project/ggen/issues) or [Documentation](./README.md)
