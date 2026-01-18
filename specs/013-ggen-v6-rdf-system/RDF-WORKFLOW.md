# RDF-First Workflow Guide - ggen v6

**Project**: 013-ggen-v6-rdf-system
**Constitutional Equation**: `code = μ(spec.ttl)`

---

## Overview

This project uses **RDF-first architecture** where:
- **Source of Truth**: Turtle/RDF files (`.ttl`) in `ontology/`
- **Generated Artifacts**: Markdown files in `generated/` (NEVER edit manually)
- **Transformation**: `ggen sync` reads RDF, executes SPARQL, renders templates

---

## Directory Structure

```
013-ggen-v6-rdf-system/
├── ggen.toml                      # Configuration (SPARQL queries, templates)
├── ontology/                      # SOURCE OF TRUTH (edit these)
│   ├── feature-content.ttl        # System-level specification
│   ├── mvp-80-20.ttl              # 80/20 analysis in RDF
│   └── spec-kit-schema.ttl        # SHACL shapes (symlink)
├── templates/                     # Tera templates
│   └── spec.tera                  # Markdown generation template
├── generated/                     # DERIVED ARTIFACTS (never edit)
│   ├── spec.md                    # Generated from feature-content.ttl
│   └── .receipt.json              # Cryptographic provenance
├── scripts/                       # Helper scripts
│   ├── sync.sh                    # Run ggen sync with validation
│   └── validate-rdf-workflow.sh   # Validate RDF setup
├── 80-20-PLAN.md                  # Implementation plan (markdown)
└── 80-20-PRIORITIZATION.md        # Prioritization summary
```

---

## The Five-Stage Pipeline

**ggen v6** implements a deterministic transformation pipeline:

```
μ₁ → μ₂ → μ₃ → μ₄ → μ₅
```

### Stage μ₁: Normalization (SHACL Validation)
- Validates RDF against SHACL shapes
- Ensures spec integrity before processing
- **Config**: `[v6.validation]` in ggen.toml

### Stage μ₂: Extraction (SPARQL SELECT)
- Executes SPARQL queries against RDF graph
- Extracts structured data for templates
- **Config**: `[[generation]].query` in ggen.toml

### Stage μ₃: Emission (Tera Templates)
- Renders templates with SPARQL results
- Generates code/markdown output
- **Config**: `[[generation]].template` in ggen.toml

### Stage μ₄: Canonicalization (Format Normalization)
- Ensures consistent formatting (line endings, whitespace)
- **Config**: `[v6.canonicalization]` in ggen.toml

### Stage μ₅: Receipt (Cryptographic Provenance)
- Generates SHA-256 hashes of inputs and outputs
- Proves `output = μ(input)`
- **Config**: `[v6.receipt]` in ggen.toml

---

## Workflow

### 1. Edit RDF Sources (Turtle Files)

```bash
# Edit the source of truth
vim ontology/feature-content.ttl

# Changes might include:
# - Adding user stories (:us-006 a sk:UserStory)
# - Updating priorities (sk:priority "P1")
# - Adding 80/20 markers (sk:eightyTwentyCategory "Core-20-Percent")
```

### 2. Run Generation

```bash
# Option A: Use helper script (recommended)
./scripts/sync.sh

# Option B: Direct ggen command (when v6 is available)
ggen sync

# Option C: Manual workflow (current ggen v5)
# ggen v5 doesn't have sync yet - this project SPECIFIES ggen v6
```

### 3. Verify Output

```bash
# Check generated files
ls -l generated/

# View generated spec
cat generated/spec.md

# Check cryptographic receipt
cat generated/.receipt.json
```

### 4. Verify Determinism

```bash
# Run twice, compare hashes
ggen sync
sha256sum generated/spec.md > /tmp/hash1.txt

ggen sync
sha256sum generated/spec.md > /tmp/hash2.txt

diff /tmp/hash1.txt /tmp/hash2.txt
# Should output nothing (files identical)
```

---

## Configuration (ggen.toml)

### Key Sections

```toml
[v6]
enabled = true
ontology = "ontology/spec-kit-schema.ttl,ontology/feature-content.ttl"
output_dir = "generated"

[[generation]]
name = "spec"
query = """
PREFIX sk: <http://github.com/github/spec-kit#>
SELECT ?featureBranch ?featureName ?title ?priority ...
WHERE { ... }
"""
template = "templates/spec.tera"
output = "generated/spec.md"

[v6.validation]
enabled = true
shapes_file = "ontology/spec-kit-schema.ttl"
fail_on_violation = true

[v6.receipt]
enabled = true
algorithm = "SHA-256"
output_file = "generated/.receipt.json"
```

---

## Working with RDF

### Adding a New User Story

```turtle
# In ontology/feature-content.ttl

:us-006 a sk:UserStory ;
    sk:storyIndex 6 ;
    sk:title "New feature title" ;
    sk:priority "P1" ;
    sk:implementationPhase "MVP-Phase-1" ;
    sk:eightyTwentyCategory "Core-20-Percent" ;
    sk:description "As a user, I want..." ;
    sk:priorityRationale "Why this matters..." ;
    sk:hasAcceptanceScenario :us-006-as-001 .

:us-006-as-001 a sk:AcceptanceScenario ;
    sk:scenarioIndex 1 ;
    sk:given "..." ;
    sk:when "..." ;
    sk:then "..." .
```

### Querying RDF with SPARQL

```sparql
PREFIX sk: <http://github.com/github/spec-kit#>

# Get all P1 user stories
SELECT ?title ?description
WHERE {
    ?story a sk:UserStory ;
           sk:priority "P1" ;
           sk:title ?title ;
           sk:description ?description .
}
ORDER BY ?title

# Get 80/20 core features
SELECT ?title ?effort
WHERE {
    ?story sk:eightyTwentyCategory "Core-20-Percent" ;
           sk:title ?title ;
           sk:estimatedEffort ?effort .
}
```

---

## Validation

### Validate RDF Workflow

```bash
./scripts/validate-rdf-workflow.sh
```

**Checks**:
- ✓ ggen.toml configuration
- ✓ RDF ontology files exist
- ✓ TTL syntax validation (if rdflib installed)
- ✓ Tera templates exist
- ✓ No "ggen render" references (legacy command)
- ✓ Constitutional equation references
- ✓ 80/20 markers in RDF

### Validate TTL Syntax

```bash
# With Python rdflib
python3 - <<'EOF'
from rdflib import Graph
g = Graph()
g.parse("ontology/feature-content.ttl", format="turtle")
g.parse("ontology/mvp-80-20.ttl", format="turtle")
print(f"Valid! {len(g)} triples loaded")
EOF

# With rapper (if installed)
rapper -i turtle -c ontology/feature-content.ttl
```

---

## Common Tasks

### Update 80/20 Prioritization

```bash
# Edit RDF markers
vim ontology/feature-content.ttl

# Mark as core 20%
sk:implementationPhase "MVP-Phase-1" ;
sk:eightyTwentyCategory "Core-20-Percent" ;

# Mark as deferred 80%
sk:implementationPhase "Phase-2-Deferred" ;
sk:eightyTwentyCategory "Deferred-80-Percent" ;
sk:deferRationale "Why deferring..." ;
sk:workaround "How to work around..." ;
```

### Add SPARQL Query

```bash
# Edit ggen.toml
vim ggen.toml

# Add new [[generation]] block
[[generation]]
name = "tasks"
query = """
PREFIX sk: <http://github.com/github/spec-kit#>
SELECT ?taskId ?description ?status
WHERE {
    ?task a sk:Task ;
          sk:taskId ?taskId ;
          sk:description ?description ;
          sk:status ?status .
}
ORDER BY ?taskId
"""
template = "templates/tasks.tera"
output = "generated/tasks.md"
```

### Create New Template

```bash
# Create template file
vim templates/my-template.tera

# Tera template syntax
{% for item in results %}
## {{ item.title }}
Priority: {{ item.priority }}
{{ item.description }}
{% endfor %}
```

---

## Troubleshooting

### "ggen sync" not found
- **Cause**: ggen v6 not yet implemented
- **Solution**: This project SPECIFIES ggen v6. Once v6 is built, it will work.
- **Current**: ggen v5 doesn't have `sync` command

### TTL Parse Errors
```bash
# Check syntax
rapper -i turtle -c ontology/feature-content.ttl

# Common issues:
# - Missing semicolon at end of triple
# - Unclosed quotes
# - Invalid prefix
```

### SHACL Validation Failures
```bash
# Disable temporarily to debug
vim ggen.toml
# Set: fail_on_violation = false

# Common issues:
# - Priority must be "P1", "P2", or "P3" (not "HIGH", "LOW")
# - Missing required properties (sk:title, sk:description)
```

### Non-Deterministic Output
```bash
# Check for timestamps in templates
grep -r "now()\|date()\|time()" templates/

# Check for random elements
grep -r "random\|uuid" templates/

# Ensure stable ordering in SPARQL
# Use ORDER BY in all queries
```

---

## Best Practices

### 1. RDF is Source of Truth
- ✅ **DO**: Edit `.ttl` files in `ontology/`
- ❌ **DON'T**: Edit `.md` files in `generated/`
- **Why**: Generated files are overwritten by `ggen sync`

### 2. Use 80/20 Markers
```turtle
sk:eightyTwentyCategory "Core-20-Percent" ;
sk:implementationPhase "MVP-Phase-1" ;
sk:estimatedEffort "2 days" ;
```

### 3. Validate Before Committing
```bash
./scripts/validate-rdf-workflow.sh
./scripts/sync.sh
git diff generated/  # Should show expected changes only
```

### 4. Document Assumptions
```turtle
:assume-001 a sk:Assumption ;
    sk:description "Single-file TTL loading is sufficient for MVP" ;
    sk:rationale "Specs are < 1000 lines, concatenation workaround available" .
```

---

## Testing the Workflow (When v6 is Ready)

```bash
# 1. Modify spec
echo ':test a sk:Feature ; sk:featureName "Test" .' >> ontology/feature-content.ttl

# 2. Generate
ggen sync

# 3. Verify in git
git diff generated/spec.md

# 4. Verify determinism
ggen sync
git status  # Should show "nothing to commit" (idempotent)

# 5. Revert and verify
git checkout ontology/feature-content.ttl
ggen sync
git diff generated/spec.md  # Should show no changes
```

---

## Reference

- **ggen.toml**: `/Users/sac/ggen/specs/013-ggen-v6-rdf-system/ggen.toml`
- **Feature Spec (RDF)**: `ontology/feature-content.ttl` (270 triples)
- **80/20 Analysis (RDF)**: `ontology/mvp-80-20.ttl`
- **Generated Spec**: `generated/spec.md` (when v6 is ready)
- **Validation Script**: `scripts/validate-rdf-workflow.sh`
- **Sync Helper**: `scripts/sync.sh`

---

## Next Steps

1. **Implement ggen v6 MVP** (8 days, 5 capabilities)
2. **Run** `ggen sync` **to test workflow**
3. **Use ggen v6 to regenerate its own spec** (self-hosting)
4. **Validate** constitutional equation: `spec.md = μ(feature.ttl)`

---

**Remember**: This project uses RDF to SPECIFY ggen v6. Once v6 is implemented, you can use it to regenerate this very specification!
