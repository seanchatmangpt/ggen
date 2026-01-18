# ggen v6 RDF-First Code Generation System (Pure 3T Specification)

## Overview

This specification demonstrates **purity of implementation** using the **3T methodology** (TOML, Tera, Turtle). The specification about ggen v6's RDF-first architecture is itself generated FROM RDF ontology.

**Meta-Circular Property**: We use spec-kit to specify the tool that implements spec-kit.

## The Constitutional Equation

```
code = μ(spec.ttl)
```

Where:
- `spec.ttl` = RDF triples in `ontology/*.ttl` files (semantic substrate)
- `μ` = deterministic projection pipeline (μ₁→μ₂→μ₃→μ₄→μ₅)
- `code` = generated markdown/code files
- `μ∘μ = μ` = idempotence (running twice produces zero changes)

**Key Insight**: All artifacts in `generated/` are deterministic projections of semantic substrate. Edit the ontology, not the generated files.

## Directory Structure (3T Architecture)

```
013-ggen-v6-rdf-system/
├── README.md                           # ← You are here (3T documentation)
├── ggen.toml                           # TOML: Pipeline configuration
├── ontology/                           # TURTLE: Semantic substrate (source of truth)
│   ├── feature-content.ttl             # System specification (270 triples)
│   ├── mvp-80-20.ttl                   # 80/20 analysis (160 triples)
│   └── spec-kit-schema.ttl             # SHACL shapes (symlink)
├── templates/                          # TERA: Rendering templates
│   └── spec.tera                       # Markdown generation template
├── generated/                          # Generated artifacts (gitignored)
│   ├── spec.md                         # Generated specification
│   └── .receipt.json                   # Cryptographic provenance
├── scripts/                            # Helper scripts
│   ├── sync.sh                         # Run ggen sync with validation
│   └── validate-rdf-workflow.sh        # Validate 3T compliance
├── 80-20-PLAN.md                       # Implementation roadmap
├── 80-20-PRIORITIZATION.md             # Prioritization summary
├── RDF-WORKFLOW.md                     # Workflow guide
└── RDF-COMPLIANCE-REPORT.md            # Compliance audit
```

## The 3T Methodology

**3T** = **TOML + Tera + Turtle**

1. **TOML** (`ggen.toml`): Configuration-as-code
   - SPARQL queries for extraction (μ₂)
   - Template mappings
   - Pipeline configuration
   - Validation rules

2. **Tera** (`templates/*.tera`): Templates for rendering (μ₃)
   - Transform RDF bindings into code/markdown
   - Conditional logic, loops, filters
   - Multiple output formats from same ontology

3. **Turtle** (`ontology/*.ttl`): Semantic substrate
   - RDF triples encode all knowledge
   - SHACL shapes enforce constraints
   - SPARQL-queryable, machine-readable
   - Single source of truth

## How to Generate Artifacts

### Prerequisites

- ggen v6 CLI (with v6 pipeline support)
- RDF/SPARQL support (Oxigraph)
- Tera template engine

**Note**: ggen v6 is not yet implemented. This project SPECIFIES ggen v6.

### Generation Workflow

```bash
cd /Users/sac/ggen/specs/013-ggen-v6-rdf-system

# Option 1: Use helper script (recommended)
./scripts/sync.sh

# Option 2: Direct command (when v6 is available)
ggen sync

# Verify idempotence (no file changes on second run)
ggen sync
git status  # Should show "nothing to commit"

# Verify cryptographic provenance
cat generated/.receipt.json
```

### What Happens During `ggen sync`

The five-stage pipeline (μ₁ through μ₅) executes:

#### 1. μ₁ - Normalization (SHACL Validation)
- Validates RDF against `spec-kit-schema.ttl` SHACL shapes
- Catches missing required fields, invalid priorities, wrong ID patterns
- **Config**: `[v6.validation]` in ggen.toml
- Fails fast with actionable error messages

#### 2. μ₂ - Extraction (SPARQL SELECT)
- Executes SPARQL queries against RDF graph
- Extracts user stories, requirements, success criteria
- **Config**: `[[generation]].query` in ggen.toml
- Bindings passed to templates as variables

#### 3. μ₃ - Emission (Tera Templates)
- Templates transform RDF bindings into markdown/code
- Complex grouping logic (nested scenarios, 80/20 markers)
- **Config**: `[[generation]].template` in ggen.toml
- Output written to `generated/*.md`

#### 4. μ₄ - Canonicalization (Deterministic Formatting)
- Normalize line endings (LF)
- Trim trailing whitespace
- Ensure final newline
- **Config**: `[v6.canonicalization]` in ggen.toml
- Guarantees bit-for-bit reproducibility

#### 5. μ₅ - Receipt Generation (Cryptographic Provenance)
- SHA-256 hashes of input ontology files
- SHA-256 hashes of output markdown files
- Timestamp and pipeline configuration hash
- **Config**: `[v6.receipt]` in ggen.toml
- Proves: `hash(spec.md) = hash(μ(ontology))`

## Constitutional Invariants

This specification enforces ggen v6's constitutional laws:

1. **Idempotence** (μ∘μ = μ): Running `ggen sync` twice produces zero file changes
2. **Determinism**: Same ontology generates bit-for-bit identical output across platforms
3. **Provenance**: Receipt cryptographically proves output derived from ontology
4. **No-Edit Law**: Generated files in `generated/` are never hand-edited
5. **Substrate Primacy**: Only ontology (.ttl files) is version-controlled as source of truth

## Modifying the Specification

**CRITICAL**: To modify the specification, edit the ONTOLOGY, not the generated markdown.

### Example: Add a New User Story

1. Edit `ontology/feature-content.ttl`:
   ```turtle
   :us-006 a sk:UserStory ;
       sk:storyIndex 6 ;
       sk:title "Developer validates RDF specs with SHACL" ;
       sk:priority "P1" ;
       sk:implementationPhase "MVP-Phase-1" ;
       sk:eightyTwentyCategory "Core-20-Percent" ;
       sk:estimatedEffort "1 day" ;
       sk:description "As a developer, I want to validate my RDF specifications against SHACL shapes..." ;
       sk:hasAcceptanceScenario :us-006-as-001 .

   :us-006-as-001 a sk:AcceptanceScenario ;
       sk:scenarioIndex 1 ;
       sk:given "a TTL file with invalid priority value" ;
       sk:when "developer runs ggen sync" ;
       sk:then "SHACL validation fails with clear error message" .
   ```

2. Link to feature:
   ```turtle
   :ggen-v6-system
       sk:hasUserStory :us-001, :us-002, :us-003, :us-004, :us-005, :us-006 .
   ```

3. Regenerate:
   ```bash
   ggen sync
   ```

4. Verify in `generated/spec.md`

### Example: Update 80/20 Prioritization

1. Edit `ontology/feature-content.ttl`:
   ```turtle
   # Move from deferred to core
   :us-004 sk:eightyTwentyCategory "Core-20-Percent" ;
           sk:implementationPhase "MVP-Phase-1" ;
           sk:estimatedEffort "1 day" .
   ```

2. Regenerate and verify:
   ```bash
   ggen sync
   grep -A 5 "us-004" generated/spec.md
   ```

## SHACL Validation Examples

The specification uses SHACL shapes to enforce quality:

### Valid User Story (Passes Validation)
```turtle
:us-006 a sk:UserStory ;
    sk:priority "P1" ;                # ✓ Must be "P1", "P2", or "P3"
    sk:title "Valid title" ;          # ✓ At least 5 characters
    sk:description "As a developer..." .  # ✓ At least 20 characters
```

### Invalid User Story (Fails Validation)
```turtle
:us-bad a sk:UserStory ;
    sk:priority "HIGH" ;              # ✗ Must be P1/P2/P3
    sk:title "Bad" ;                  # ✗ < 5 characters
    sk:description "Short" .          # ✗ < 20 characters
```

Running `ggen sync` on invalid ontology produces:
```
Error during μ₁ (Normalization):
  File: ontology/feature-content.ttl:120
  Violation: priority must be one of: P1, P2, P3
  Value: "HIGH"
```

## 80/20 Analysis in RDF

This specification uses the **80/20 principle** encoded in RDF:

### Core 20% (8 developer-days, 80% value)
```turtle
:us-001 sk:eightyTwentyCategory "Core-20-Percent" ;
        sk:implementationPhase "MVP-Phase-1" ;
        sk:estimatedEffort "4 days" .
```

**5 Core Capabilities**:
1. CAP-001: Parse TTL → RDF Graph (2 days)
2. CAP-002: Execute SPARQL SELECT (2 days)
3. CAP-003: Render Tera Template (1 day)
4. CAP-004: Deterministic Output (1 day)
5. CAP-005: ggen sync Command (2 days)

### Deferred 80% (32+ developer-days, 20% value)
```turtle
:us-004 sk:eightyTwentyCategory "Deferred-80-Percent" ;
        sk:implementationPhase "Phase-2-Deferred" ;
        sk:deferRationale "Determinism ensures same output; idempotence is git noise reduction" ;
        sk:workaround "Always regenerate all files initially" .
```

**8 Deferred Features**:
- SHACL validation
- Cryptographic receipts
- Idempotence optimization
- Multi-file loading
- Advanced SPARQL (CONSTRUCT)
- Template inheritance
- Error recovery
- Performance optimization

See `80-20-PLAN.md` for complete analysis.

## Testing (Testcontainers E2E)

### Running Tests

```bash
# Run testcontainers e2e test (validates 3T workflow)
cd /Users/sac/ggen/vendors/spec-kit
pytest tests/integration/test_3t_e2e.py -v -s

# Or run all tests
pytest tests/integration/ -v
```

### What the Test Validates

The e2e test validates the complete 3T workflow:

1. **3T Files Present**: TOML (ggen.toml), Tera (templates/), Turtle (ontology/)
2. **ggen sync Execution**: Command runs without errors
3. **Idempotence** (μ∘μ = μ): Running twice produces zero file changes
4. **Determinism**: Same input always produces identical output
5. **Cryptographic Provenance**: Receipt proves spec.md = μ(ontology)
6. **Constitutional Equation**: hash(output) = hash(μ(input))

See `tests/integration/test_3t_e2e.py` for implementation.

## Comparison: Traditional vs 3T Specification

| Aspect | Traditional (spec.md) | 3T (This Spec) |
|--------|----------------------|----------------|
| Source Format | Markdown prose | RDF triples (430 total) |
| Editing | Direct markdown editing | Edit ontology, regenerate |
| Validation | Manual review | SHACL shapes at generation-time |
| Consistency | Requires discipline | Enforced by structure |
| Traceability | Manual linking | RDF relationships |
| Provenance | None | Cryptographic receipt (SHA-256) |
| Idempotence | N/A | Guaranteed by μ (μ∘μ = μ) |
| Versioning | Markdown diffs | Ontology diffs (semantic) |
| 80/20 Analysis | Manual prioritization | RDF markers (queryable) |

## Benefits of 3T Ontology-Driven Specifications

1. **Machine-Readable**: Specifications can be queried with SPARQL, analyzed programmatically
2. **Structural Validation**: SHACL catches errors before generation (μ₁ stage)
3. **Traceability**: RDF relationships link requirements → user stories → entities
4. **Consistency**: Templates ensure uniform formatting across all outputs
5. **Provenance**: Cryptographic proof of generation (SHA-256 receipts)
6. **Reusability**: Same ontology generates multiple views (markdown, JSON, HTML, code)
7. **80/20 Analysis**: Prioritization encoded in RDF, queryable
8. **Zero Drift**: spec.md = μ(ontology) → impossible for spec and code to diverge

## Files That Should Be Version-Controlled

**Version Control (3T Files Only)**:
- ✅ `ggen.toml` - TOML pipeline configuration
- ✅ `ontology/feature-content.ttl` - TURTLE semantic substrate
- ✅ `ontology/mvp-80-20.ttl` - TURTLE 80/20 analysis
- ✅ `templates/spec.tera` - TERA rendering templates
- ✅ `README.md` - 3T documentation (this file)
- ✅ `.gitignore` - Ignore generated artifacts
- ✅ `scripts/*.sh` - Helper scripts

**Never Version Control (Generated Artifacts)**:
- ❌ `generated/*.md` - All generated markdown
- ❌ `generated/.receipt.json` - Cryptographic receipt

Add to `.gitignore`:
```gitignore
# Generated artifacts (regenerated from ontology via ggen sync)
generated/
```

## Project Status

**Current**: ggen v5 (does not have `sync` command)
**Specifying**: ggen v6 (RDF-first code generation with 3T methodology)

This project is **eating its own dog food** - using spec-kit to specify ggen v6, which will implement the very methodology used to specify it.

**Next Steps**:
1. Implement ggen v6 MVP (8 days, 5 core capabilities)
2. Run `ggen sync` to validate this specification
3. Use ggen v6 to regenerate its own spec (self-hosting)
4. Prove constitutional equation: code = μ(spec.ttl)

## Quick Reference

- **Total RDF Triples**: 430 (270 in feature-content.ttl, 160 in mvp-80-20.ttl)
- **User Stories**: 5 (system-level)
- **Core Capabilities**: 5 (20% effort, 80% value)
- **Deferred Features**: 8 (80% effort, 20% value)
- **Configuration**: `/Users/sac/ggen/specs/013-ggen-v6-rdf-system/ggen.toml`
- **Validation**: `./scripts/validate-rdf-workflow.sh`
- **Sync**: `./scripts/sync.sh`

## Further Reading

- `RDF-WORKFLOW.md` - Complete workflow documentation
- `80-20-PLAN.md` - 8-day implementation roadmap
- `80-20-PRIORITIZATION.md` - Detailed prioritization analysis
- `RDF-COMPLIANCE-REPORT.md` - 3T compliance audit
- `specs/001-v6-3t-implementation/README.md` - Original 3T specification

---

**Remember**: This is a **3T specification** (TOML + Tera + Turtle). Edit the ontology, run `ggen sync`, verify the output. Never edit generated files directly.
