# Spec-Kit 3T: RDF-Based Specification System

## Overview

This is a **pure 3T transformation** of GitHub's Spec-Kit from markdown templates to an ontology-driven specification system. The system demonstrates **meta-circularity**: a specification methodology ABOUT ontology-first development that IS ITSELF ontology-driven.

## The Constitutional Equation

```
spec.md = μ(ontology)
```

Where:
- **ontology** = RDF triples in `.ttl` files (semantic substrate)
- **μ** = deterministic five-stage pipeline (μ₁→μ₂→μ₃→μ₄→μ₅)
- **spec.md** = generated specification document

**Key Insight**: The specification is NOT hand-written markdown. It's a deterministic projection of semantic substrate through templates.

## Directory Structure

```
vendors/spec-kit-3t/
├── README.md                           # ← You are here
├── ggen.toml                           # Pipeline configuration (247 lines)
├── ontology/
│   └── spec-kit-schema.ttl             # RDF vocabulary + SHACL shapes (600+ lines)
├── examples/
│   └── 001-photo-albums-content.ttl    # Example feature as RDF triples (180 lines)
├── templates/
│   ├── spec-header.tera                # Document metadata template
│   ├── user-stories.tera               # User stories with nested scenarios
│   ├── requirements.tera               # Functional requirements + entities
│   ├── success-criteria.tera           # Measurable outcomes
│   └── assumptions.tera                # Documented assumptions
├── generated/                          # Generated artifacts (gitignored)
│   ├── spec-header.md
│   ├── user-stories.md
│   ├── requirements.md
│   ├── success-criteria.md
│   ├── assumptions.md
│   └── .receipt.json                   # Cryptographic provenance
└── contracts/                          # API contracts (OpenAPI, GraphQL)
```

## How to Generate Specifications

### Prerequisites

- ggen v6 CLI (with v6 pipeline support)
- RDF/SPARQL support (Oxigraph)
- Tera template engine
- SHACL validation

### Generation Workflow

```bash
cd /Users/sac/ggen/vendors/spec-kit-3t

# Generate specification from ontology
ggen sync

# Verify idempotence (no file changes on second run)
ggen sync

# Verify cryptographic provenance
ggen verify
```

### What Happens During `ggen sync`

The five-stage pipeline executes:

1. **μ₁ - Normalization**: SHACL validation against `spec-kit-schema.ttl`
   - Validates feature branch pattern (`^[0-9]{3}-[a-z0-9-]+$`)
   - Validates priority values (`P1`, `P2`, `P3`)
   - Validates requirement ID patterns (`^FR-[0-9]{3}$`)
   - Ensures user stories have ≥1 acceptance scenario
   - Fails fast with actionable error messages

2. **μ₂ - Extraction**: SPARQL SELECT queries extract RDF bindings
   - 5 queries (spec-header, user-stories, requirements, success-criteria, assumptions)
   - Extract user stories with nested acceptance scenarios
   - Bindings passed to templates as variables

3. **μ₃ - Emission**: Tera templates render bindings into markdown
   - Templates transform RDF into human-readable format
   - Complex grouping logic (nested scenarios using `set_global`)
   - Output written to `generated/*.md`

4. **μ₄ - Canonicalization**: Deterministic formatting
   - Normalize line endings (LF)
   - Trim trailing whitespace
   - Ensure final newline
   - Guarantees bit-for-bit reproducibility

5. **μ₅ - Receipt Generation**: Cryptographic provenance
   - SHA-256 hashes of input ontology files
   - SHA-256 hashes of output markdown files
   - Timestamp and pipeline configuration hash
   - Proves: `hash(spec.md) = hash(μ(ontology))`

## Constitutional Invariants

This specification enforces Spec-Kit's constitutional laws:

1. **Idempotence** (μ∘μ = μ): Running `ggen sync` twice produces zero file changes
2. **Determinism**: Same ontology generates bit-for-bit identical markdown across platforms
3. **Provenance**: Receipt cryptographically proves `spec.md = μ(ontology)`
4. **No-Edit Law**: Generated files in `generated/` are never hand-edited
5. **Substrate Primacy**: Only ontology (`.ttl` files) is version-controlled as source of truth

## Ontology Schema

The `spec-kit-schema.ttl` defines the RDF vocabulary:

### Core Classes

- `sk:Feature` - Complete feature specification
- `sk:UserStory` - Prioritized user journey (P1/P2/P3)
- `sk:AcceptanceScenario` - Given-When-Then acceptance criterion
- `sk:FunctionalRequirement` - System capability (FR-XXX pattern)
- `sk:SuccessCriterion` - Measurable outcome (SC-XXX pattern)
- `sk:Entity` - Key domain entity
- `sk:EdgeCase` - Boundary condition
- `sk:Dependency`, `sk:Assumption` - Supporting documentation
- `sk:ImplementationPlan`, `sk:Task` - Technical planning
- `sk:Clarification` - Structured question with options

### Key Properties

**Datatype Properties**:
- `sk:featureBranch`, `sk:featureName`, `sk:created`, `sk:status`
- `sk:storyIndex`, `sk:title`, `sk:priority`, `sk:description`
- `sk:given`, `sk:when`, `sk:then` (GWT pattern)
- `sk:requirementId`, `sk:criterionId`, `sk:taskId`
- `sk:measurable`, `sk:metric`, `sk:target`

**Object Properties**:
- `sk:hasUserStory`, `sk:hasAcceptanceScenario`
- `sk:hasFunctionalRequirement`, `sk:hasSuccessCriterion`
- `sk:hasEntity`, `sk:hasEdgeCase`
- `sk:hasImplementationPlan`, `sk:hasTask`

### SHACL Validation

10 SHACL shapes enforce structural constraints:

```turtle
sk:UserStoryShape a shacl:NodeShape ;
    shacl:targetClass sk:UserStory ;
    shacl:property [
        shacl:path sk:priority ;
        shacl:in ("P1" "P2" "P3") ;  # Only P1, P2, or P3 allowed
    ] ;
    shacl:property [
        shacl:path sk:hasAcceptanceScenario ;
        shacl:minCount 1 ;  # At least one scenario required
    ] .
```

## Example: Photo Albums Feature

The `examples/001-photo-albums-content.ttl` file demonstrates a complete feature:

```turtle
:photo-albums a sk:Feature ;
    sk:featureBranch "001-photo-albums" ;
    sk:featureName "Photo Album Organization Application" ;
    sk:created "2025-12-19"^^xsd:date ;
    sk:status "Draft" ;
    sk:hasUserStory :us-001, :us-002, :us-003 .

:us-001 a sk:UserStory ;
    sk:storyIndex 1 ;
    sk:title "Create and Manage Albums" ;
    sk:priority "P1" ;
    sk:priorityRationale "This is the foundational capability..." ;
    sk:independentTest "Can be fully tested by creating an album..." ;
    sk:hasAcceptanceScenario :us-001-as-001, :us-001-as-002 .

:us-001-as-001 a sk:AcceptanceScenario ;
    sk:scenarioIndex 1 ;
    sk:given "the application is open on the main album view" ;
    sk:when "the user clicks 'Create Album' and enters a name" ;
    sk:then "a new empty album appears in the album list" .
```

## Modifying Specifications

**CRITICAL**: To modify specifications, edit the ONTOLOGY, not the generated markdown.

### Example: Add a New User Story

1. Edit `examples/001-photo-albums-content.ttl`:
   ```turtle
   :us-004 a sk:UserStory ;
       sk:storyIndex 4 ;
       sk:title "Delete Photos from Albums" ;
       sk:priority "P2" ;
       sk:priorityRationale "Photo deletion improves user control..." ;
       sk:independentTest "Can be tested by adding 5 photos..." ;
       sk:hasAcceptanceScenario :us-004-as-001 .

   :us-004-as-001 a sk:AcceptanceScenario ;
       sk:scenarioIndex 1 ;
       sk:given "an album contains 3 photos" ;
       sk:when "the user selects a photo and presses Delete" ;
       sk:then "the photo is removed from the album" .
   ```

2. Link to feature:
   ```turtle
   :photo-albums
       sk:hasUserStory :us-001, :us-002, :us-003, :us-004 .  # Add :us-004
   ```

3. Regenerate:
   ```bash
   ggen sync
   ```

4. Verify in `generated/user-stories.md`

## Comparison: Traditional vs 3T Spec-Kit

| Aspect | Traditional (Markdown) | 3T (RDF-Based) |
|--------|------------------------|----------------|
| Source Format | Markdown prose | RDF triples |
| Editing | Direct markdown editing | Edit ontology, regenerate |
| Validation | Manual review | SHACL shapes at compile-time |
| Consistency | Requires discipline | Enforced by structure |
| Traceability | Manual linking | RDF relationships |
| Provenance | None | Cryptographic receipt |
| Idempotence | N/A | Guaranteed by μ |
| Versioning | Markdown diffs | Ontology diffs |

## Benefits of RDF-Based Specifications

1. **Machine-Readable**: Specifications can be queried, analyzed, validated programmatically
2. **Structural Validation**: SHACL catches errors before generation (FR-001 vs REQ-1)
3. **Traceability**: RDF relationships link requirements → user stories → success criteria
4. **Consistency**: Templates ensure uniform formatting
5. **Provenance**: Cryptographic proof of generation
6. **Reusability**: Same ontology can generate multiple views (markdown, JSON, HTML)
7. **Queryability**: SPARQL enables complex queries (e.g., "all P1 stories with ≥3 scenarios")

## Files to Version Control

**Version Control (3T Files Only)**:
- ✅ `ggen.toml` - Pipeline configuration
- ✅ `ontology/spec-kit-schema.ttl` - Vocabulary definition
- ✅ `examples/*.ttl` - Feature content as RDF
- ✅ `templates/*.tera` - Rendering templates
- ✅ `README.md` - Documentation (this file)
- ✅ `.gitignore` - Ignore generated artifacts

**Never Version Control (Generated Artifacts)**:
- ❌ `generated/*.md` - All generated markdown
- ❌ `generated/.receipt.json` - Cryptographic receipt

Add to `.gitignore`:
```gitignore
# Generated artifacts (regenerated from ontology)
generated/
```

## Migration from Markdown Spec-Kit

See [MIGRATION.md](./MIGRATION.md) for detailed steps to convert existing markdown specifications to RDF.

**Quick Migration Process**:
1. Analyze markdown spec structure (user stories, requirements, etc.)
2. Create RDF instances for each entity
3. Define relationships (`hasUserStory`, `hasAcceptanceScenario`)
4. Run SHACL validation to catch structural errors
5. Generate with `ggen sync` and compare to original
6. Iterate until content-equivalent

## Troubleshooting

### Problem: `ggen sync` fails with SHACL violation
**Solution**: Check error message for file/line number, fix ontology triple

Example error:
```
Error during μ₁ (Normalization):
  File: examples/001-photo-albums-content.ttl:45
  Violation: priority must be P1, P2, or P3
  Value: "HIGH"
```

Fix:
```turtle
sk:priority "P1" ;  # Not "HIGH"
```

### Problem: Generated spec.md is missing content
**Solution**: Verify SPARQL query returns results (test queries manually), check template logic

### Problem: File changes detected on second `ggen sync`
**Solution**: Idempotence violation - check μ₄ canonicalization, ensure no timestamps in templates

### Problem: Want to add new section to specification
**Solution**:
1. Add new class to `spec-kit-schema.ttl` (e.g., `sk:TestPlan`)
2. Add instances to content `.ttl` files
3. Create new template `templates/test-plan.tera`
4. Add new `[[generation]]` rule to `ggen.toml`
5. Run `ggen sync`

## Integration with Spec-Kit Workflow

The 3T transformation preserves the Spec-Kit workflow:

**Original Commands** (markdown-based):
- `/speckit.constitution` → Creates `constitution.md`
- `/speckit.specify` → Creates `spec.md`
- `/speckit.plan` → Creates `plan.md`
- `/speckit.tasks` → Creates `tasks.md`
- `/speckit.implement` → Executes tasks

**3T Equivalent** (RDF-based):
- `/speckit.constitution` → Creates `constitution.ttl` (RDF triples)
- `/speckit.specify` → Creates `feature-content.ttl` + runs `ggen sync` → generates `spec.md`
- `/speckit.plan` → Creates `plan.ttl` + runs `ggen sync` → generates `plan.md`
- `/speckit.tasks` → Creates `tasks.ttl` + runs `ggen sync` → generates `tasks.md`
- `/speckit.implement` → Reads generated task markdown (unchanged)

**Key Difference**: Instead of AI directly writing markdown, AI writes RDF triples which are then projected through templates.

## Advanced Features

### SPARQL Queries for Analysis

Query all P1 user stories:
```sparql
PREFIX sk: <http://github.com/github/spec-kit#>

SELECT ?title ?description
WHERE {
    ?story a sk:UserStory ;
           sk:priority "P1" ;
           sk:title ?title ;
           sk:description ?description .
}
ORDER BY ?title
```

Find user stories with ≥4 acceptance scenarios:
```sparql
PREFIX sk: <http://github.com/github/spec-kit#>

SELECT ?title (COUNT(?scenario) AS ?scenarioCount)
WHERE {
    ?story a sk:UserStory ;
           sk:title ?title ;
           sk:hasAcceptanceScenario ?scenario .
}
GROUP BY ?title
HAVING (COUNT(?scenario) >= 4)
```

### Multi-View Generation

The same ontology can generate multiple views:

**View 1: Full Specification** (current)
- SPARQL: Extract all content
- Template: `spec-full.tera`
- Output: `spec.md`

**View 2: Executive Summary**
- SPARQL: Extract only P1 stories + top-level requirements
- Template: `spec-summary.tera`
- Output: `summary.md`

**View 3: JSON API**
- SPARQL: Extract all content
- Template: `spec.json.tera`
- Output: `spec.json`

**View 4: HTML Website**
- SPARQL: Extract all content
- Template: `spec.html.tera`
- Output: `index.html`

## Meta-Circular Property

This specification system is **self-demonstrating**:

- **Describes**: Spec-Kit methodology using RDF ontologies
- **Uses**: The exact same RDF ontology approach it describes
- **Proves**: Specifications CAN be ontology-driven, not just code

The specification becomes a **proof-of-concept** for the paradigm it proposes.

## Next Steps

1. Review generated specification in `generated/*.md`
2. Validate against original Spec-Kit markdown templates
3. Test idempotence (run `ggen sync` twice, verify no changes)
4. Verify cryptographic receipt with `ggen verify`
5. Use this as template for converting other Spec-Kit features to 3T

## References

- GitHub Spec-Kit: https://github.com/github/spec-kit
- ggen v6 Thesis System: `/Users/sac/ggen/docs/ggen-v6-thesis/`
- ggen v6 Ontology: `/Users/sac/ggen/v6/ontology/ggen-v6.ttl`
- Original v6 3T Specification: `/Users/sac/ggen/specs/001-v6-3t-implementation/`

---

**The future of software specifications is ontology-first. This transformation proves it's possible.**
