# Simple Project Scaffolding

Learn to generate complete Rust projects from specifications: Cargo.toml, directory structure, CI/CD, and more.

**Status**: ✅ Complete
**Difficulty**: ⭐ Beginner
**Time**: 20-30 minutes
**Focus**: Project generation from RDF specifications

---

## 1. Overview

This example shows how to use ggen to generate an entire Rust project structure from RDF specifications, eliminating boilerplate creation. You'll learn:

- Project metadata specification in RDF
- Generating Cargo.toml from ontology
- Creating src/ structure automatically
- Generating CI/CD configuration
- Scaffolding project directories

**What this teaches**: Specification-driven project generation replaces manual scaffolding. One specification → complete project structure.

---

## 2. Prerequisites

- ggen CLI (5.0.0+)
- Basic understanding of Rust projects
- Basic YAML knowledge
- Node.js 18+ (for validation)

---

## 3. Quick Start

```bash
cd examples/simple-project

# Validate setup
./validate.mjs

# View specification
cat ontology/projects.ttl

# View templates
cat templates/cargo-toml.tera

# Review golden files
cat golden/generated/Cargo.toml
```

---

## 4. Architecture

### Project Generation Pipeline

```
RDF Ontology (project spec)
    ↓
SPARQL Queries (extract structure)
    ↓
Tera Templates (render files)
    ↓
Generated Project
    ├── Cargo.toml
    ├── src/lib.rs
    ├── src/main.rs
    ├── tests/
    ├── .github/workflows/ci.yml
    └── .gitignore
```

### Specification Components

1. **RustProject**: Top-level project definition
2. **Dependencies**: Cargo dependencies
3. **Directory**: Project structure
4. **TestPattern**: Testing approach
5. **GitignoreEntry**: Git exclusions

---

## 5. File Structure

```
simple-project/
├── ggen.toml              # 10 generation rules
├── ontology/
│   └── projects.ttl       # RDF project spec
├── templates/             # 8 Tera templates
│   ├── cargo-toml.tera
│   ├── lib-rs.tera
│   ├── main-rs.tera
│   ├── project-guide.tera
│   ├── dependencies.tera
│   ├── structure.tera
│   ├── testing.tera
│   ├── getting-started.tera
│   ├── ci-yaml.tera
│   └── gitignore.tera
├── golden/                # Expected outputs
│   └── generated/
├── validate.mjs           # Validation script
└── README.md              # This file
```

---

## 6. Step-by-Step Tutorial

### Step 1: Examine Ontology (5 min)

```bash
cat ontology/projects.ttl
```

You'll see:
- `RustProject` class with properties: name, version, description
- `Dependency` class for project dependencies
- `Directory` class for folder structure
- SHACL validation shapes

### Step 2: Review Templates (5 min)

```bash
cat templates/cargo-toml.tera
```

Key elements:
- YAML frontmatter: `to: "generated/Cargo.toml"`
- Variable substitution: `{{ project_name }}`
- Tera filters: `{{ project_name | snake_case }}`

### Step 3: Run Validation (3 min)

```bash
./validate.mjs
```

Checks:
- ✓ Required files present
- ✓ README sections complete
- ✓ SHACL validation shapes present
- ✓ Generation rules defined

### Step 4: Inspect Golden Files (3 min)

```bash
cat golden/generated/Cargo.toml
cat golden/generated/lib.rs
cat golden/generated/main.rs
```

These are deterministic reference outputs.

### Step 5: Understand SPARQL Queries (4 min)

From ggen.toml:

```sparql
PREFIX proj: <https://ggen.io/ontology/projects#>
SELECT ?projectName ?version ?description
WHERE {
  ?project a proj:RustProject ;
    proj:name ?projectName ;
    proj:version ?version ;
    proj:description ?description .
}
```

This extracts project metadata from RDF.

---

## 7. Configuration Reference

### ggen.toml Structure

```toml
[project]
name = "simple-project"

[ontology]
source = "ontology/projects.ttl"

[[generation.rules]]
name = "generate-cargo-toml"
sparql = "..."
template = { file = "templates/cargo-toml.tera" }
output_file = "generated/Cargo.toml"
```

### Template Patterns

**Cargo.toml Generation**:
```tera
[package]
name = "{{ project_name }}"
version = "{{ version }}"
```

**Conditional Sections**:
```tera
{% if has_main %}
[[bin]]
name = "main"
path = "src/main.rs"
{% endif %}
```

### SPARQL Patterns

**Select Project Metadata**:
```sparql
SELECT ?projectName ?version ?description
WHERE {
  ?project a proj:RustProject ;
    proj:name ?projectName ;
    proj:version ?version .
}
```

---

## 8. Troubleshooting

### Generation Fails
- Check: `ggen.toml` has valid SPARQL syntax
- Check: ontology.ttl has matching instances
- Check: Output directories writable

### Golden Files Don't Match
- Verify: Template variables match SPARQL SELECT clause
- Verify: Filters are valid (snake_case, pascal_case, etc.)
- Regenerate: `ggen sync --force`

### Validation Script Fails
- Check: All required files present
- Check: README has 9 sections
- Check: Ontology has SHACL shapes

---

## 9. Next Steps

### Learn More
- Move to `ai-template-creation/` for AI-assisted generation
- Explore `complete-project-generation/` for complex scaffolding
- Read `openapi/` for API-driven generation

### Practice
1. Add a new dependency to ontology.ttl
2. Update templates to include it
3. Regenerate and verify golden files match

### Apply
Use this pattern to scaffold your own projects:
1. Create ontology for your domain
2. Write Tera templates
3. Run `ggen sync` for instant scaffolding

---

## Summary

Project scaffolding from specifications is **fast, reproducible, and auditable**. This example demonstrates:
- RDF ontologies for project structure
- SPARQL extraction of specifications
- Tera template generation
- Golden file validation
- Deterministic project creation

**Key Learning**: Specification-driven development replaces manual work.

---

**Status**: GREEN ✓
**Quality Gates**: All passed
**Reproducibility**: 100% deterministic
