# v6 Specification: Pure 3T Implementation

## Overview

This specification demonstrates **purity of implementation** - the specification itself is written using the exact 3T (TOML, Tera, Turtle) methodology it describes. This creates a **meta-circular property**: the specification about v6's ontology-first approach is generated FROM ontology.

## The Constitutional Equation

```
spec.md = μ(ontology)
```

Where:
- `ontology` = RDF triples in `ontology/*.ttl` files (semantic substrate)
- `μ` = deterministic projection pipeline (μ₁→μ₂→μ₃→μ₄→μ₅)
- `spec.md` = generated specification document

**Key Insight**: The specification is NOT hand-written markdown. It's a deterministic projection of semantic substrate through templates.

## Directory Structure

```
specs/001-v6-3t-implementation/
├── README.md                           # ← You are here
├── ggen.toml                           # Pipeline configuration
├── ontology/
│   ├── spec-schema.ttl                 # Vocabulary definition (485 lines)
│   └── v6-spec-content.ttl             # All content as RDF (887 lines)
├── templates/
│   ├── spec-main.tera                  # Document header
│   ├── user-stories.tera               # User stories with nested scenarios
│   ├── requirements.tera               # Functional requirements
│   ├── success-criteria.tera           # Success criteria
│   ├── entities.tera                   # Key entities
│   ├── edge-cases.tera                 # Edge cases
│   ├── dependencies.tera               # Dependencies
│   ├── assumptions.tera                # Assumptions
│   ├── non-goals.tera                  # Non-goals
│   └── risks.tera                      # Risks with mitigations
└── generated/                          # Generated artifacts (gitignored)
    ├── spec-header.md                  # Generated specification header
    ├── user-stories.md                 # Generated user stories
    ├── requirements.md                 # Generated requirements
    ├── success-criteria.md             # Generated success criteria
    ├── entities.md                     # Generated entities
    ├── edge-cases.md                   # Generated edge cases
    ├── dependencies.md                 # Generated dependencies
    ├── assumptions.md                  # Generated assumptions
    ├── non-goals.md                    # Generated non-goals
    ├── risks.md                        # Generated risks
    └── .receipt.json                   # Cryptographic provenance
```

## How to Generate the Specification

### Prerequisites

- ggen v6 CLI (with v6 pipeline support)
- RDF/SPARQL support (Oxigraph)
- Tera template engine

### Generation Workflow

```bash
cd /Users/sac/ggen/specs/001-v6-3t-implementation

# Generate specification from ontology
ggen sync

# Verify idempotence (no file changes on second run)
ggen sync

# Verify cryptographic provenance
ggen verify
```

### What Happens During `ggen sync`

The five-stage pipeline (μ₁ through μ₅) executes:

1. **μ₁ - Normalization**: CONSTRUCT queries rewrite ontology + SHACL validation
   - Validates against `spec-schema.ttl` SHACL shapes
   - Catches missing required fields, invalid ID patterns, wrong priorities
   - Fails fast with actionable error messages

2. **μ₂ - Extraction**: SELECT queries extract RDF bindings
   - 10 SPARQL queries (one per generation rule)
   - Extract user stories, requirements, success criteria, etc.
   - Bindings passed to templates as variables

3. **μ₃ - Emission**: Tera templates render bindings into markdown
   - Templates transform RDF into human-readable format
   - Complex grouping logic (e.g., nested acceptance scenarios)
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

This specification enforces v6's constitutional laws:

1. **Idempotence** (μ∘μ = μ): Running `ggen sync` twice produces zero file changes
2. **Determinism**: Same ontology generates bit-for-bit identical markdown across platforms
3. **Provenance**: Receipt cryptographically proves spec.md derived from ontology
4. **No-Edit Law**: Generated files in `generated/` are never hand-edited
5. **Substrate Primacy**: Only ontology (.ttl files) is version-controlled as source of truth

## Modifying the Specification

**CRITICAL**: To modify the specification, edit the ONTOLOGY, not the generated markdown.

### Example: Add a New Functional Requirement

1. Edit `ontology/v6-spec-content.ttl`:
   ```turtle
   :fr-021 a spec:FunctionalRequirement ;
       spec:requirementId "FR-021" ;
       spec:category "New Category" ;
       spec:description "System MUST implement new feature..." .
   ```

2. Link to specification:
   ```turtle
   :v6-spec
       spec:hasRequirement :fr-001, :fr-002, ..., :fr-021 .  # Add :fr-021
   ```

3. Regenerate:
   ```bash
   ggen sync
   ```

4. Verify in `generated/requirements.md`

### Example: Add a New User Story

1. Add story and scenarios in `ontology/v6-spec-content.ttl`
2. Update `spec:hasUserStory` relationship
3. Run `ggen sync`
4. New story appears in `generated/user-stories.md`

## SHACL Validation Examples

The specification uses SHACL shapes to enforce quality:

### Valid Requirement (Passes Validation)
```turtle
:fr-021 a spec:FunctionalRequirement ;
    spec:requirementId "FR-021" ;         # ✓ Matches pattern ^FR-[0-9]{3}$
    spec:description "System MUST..." .   # ✓ At least 10 characters
```

### Invalid Requirement (Fails Validation)
```turtle
:fr-bad a spec:FunctionalRequirement ;
    spec:requirementId "REQ-1" ;          # ✗ Wrong pattern
    spec:description "Short" .             # ✗ < 10 characters
```

Running `ggen sync` on invalid ontology produces:
```
Error during μ₁ (Normalization):
  File: ontology/v6-spec-content.ttl:450
  Violation: requirementId must match pattern ^FR-[0-9]{3}$
  Value: "REQ-1"
```

## Meta-Circular Property

This specification is **self-demonstrating**:

- **Describes**: v6's 3T methodology (TOML, Tera, Turtle)
- **Uses**: The exact same 3T methodology it describes
- **Proves**: Specifications can be ontology-driven, not just code

The specification becomes a **proof-of-concept** for the paradigm it proposes.

## Comparison: Traditional vs 3T Specification

| Aspect | Traditional (spec.md) | 3T (This Spec) |
|--------|----------------------|----------------|
| Source Format | Markdown prose | RDF triples |
| Editing | Direct markdown editing | Edit ontology, regenerate |
| Validation | Manual review | SHACL shapes at compile-time |
| Consistency | Requires discipline | Enforced by structure |
| Traceability | Manual linking | RDF relationships |
| Provenance | None | Cryptographic receipt |
| Idempotence | N/A | Guaranteed by μ |
| Versioning | Markdown diffs | Ontology diffs |

## Benefits of Ontology-Driven Specifications

1. **Machine-Readable**: Specifications can be queried, analyzed, validated programmatically
2. **Structural Validation**: SHACL catches errors before generation
3. **Traceability**: RDF relationships link requirements → user stories → success criteria
4. **Consistency**: Templates ensure uniform formatting
5. **Provenance**: Cryptographic proof of generation
6. **Reusability**: Same ontology can generate multiple views (markdown, JSON, HTML)

## Files That Should Be Version-Controlled

**Version Control (3T Files Only)**:
- ✅ `ggen.toml` - Pipeline configuration
- ✅ `ontology/spec-schema.ttl` - Vocabulary definition
- ✅ `ontology/v6-spec-content.ttl` - Content as RDF
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

## Troubleshooting

### Problem: `ggen sync` fails with SHACL violation
**Solution**: Check the error message for file/line number, fix the ontology triple

### Problem: Generated spec.md is missing content
**Solution**: Verify SPARQL query returns results (test queries manually), check template logic

### Problem: File changes detected on second `ggen sync`
**Solution**: Idempotence violation - check μ₄ canonicalization, ensure no timestamps in templates

### Problem: Want to add new section to specification
**Solution**:
1. Add new class to `spec-schema.ttl` (e.g., `spec:TestPlan`)
2. Add instances to `v6-spec-content.ttl`
3. Create new template `templates/test-plan.tera`
4. Add new `[[generation]]` rule to `ggen.toml`
5. Run `ggen sync`

## Next Steps

1. Review generated specification in `generated/*.md`
2. Validate against original requirements
3. Test idempotence (run `ggen sync` twice, verify no changes)
4. Verify cryptographic receipt with `ggen verify`
5. Use this as template for future ontology-driven specifications

## References

- v6 Thesis Generation System: `/Users/sac/ggen/docs/ggen-v6-thesis/`
- v6 Ontology Schema: `/Users/sac/ggen/v6/ontology/ggen-v6.ttl`
- Original Specification: `spec.md` (deprecated, kept for reference)

---

**The future of software specifications is ontology-first. This specification proves it's possible.**
