<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Creating ggen Examples: Dos and Don'ts](#creating-ggen-examples-dos-and-donts)
  - [Overview](#overview)
  - [Directory Structure](#directory-structure)
    - [DO: Use a consistent, predictable layout](#do-use-a-consistent-predictable-layout)
    - [DON'T: Mix source and generated files](#dont-mix-source-and-generated-files)
  - [Ontology Design](#ontology-design)
    - [DO: Separate vocabulary from instance data](#do-separate-vocabulary-from-instance-data)
    - [DON'T: Hard-code domain data in templates](#dont-hard-code-domain-data-in-templates)
  - [Manifest Configuration (ggen.toml)](#manifest-configuration-ggentoml)
    - [DO: Document each rule with comment blocks](#do-document-each-rule-with-comment-blocks)
    - [DON'T: Leave rules undocumented](#dont-leave-rules-undocumented)
  - [SPARQL Queries](#sparql-queries)
    - [DO: Use ORDER BY for template grouping](#do-use-order-by-for-template-grouping)
    - [DO: Use OPTIONAL for nullable fields](#do-use-optional-for-nullable-fields)
    - [DON'T: Assume all fields exist](#dont-assume-all-fields-exist)
  - [Template Patterns](#template-patterns)
    - [DO: Include YAML frontmatter with documentation](#do-include-yaml-frontmatter-with-documentation)
    - [DO: Use `?`-prefixed variable access with defaults](#do-use--prefixed-variable-access-with-defaults)
    - [DON'T: Access variables without defaults](#dont-access-variables-without-defaults)
    - [DO: Use state variables for grouping](#do-use-state-variables-for-grouping)
    - [DON'T: Forget to close grouped structures](#dont-forget-to-close-grouped-structures)
  - [Generated File Headers](#generated-file-headers)
    - [DO: Mark files as auto-generated](#do-mark-files-as-auto-generated)
    - [DON'T: Leave generated files unmarked](#dont-leave-generated-files-unmarked)
  - [Validation and Testing](#validation-and-testing)
    - [DO: Provide golden files for comparison](#do-provide-golden-files-for-comparison)
    - [DO: Include verification scripts](#do-include-verification-scripts)
    - [DON'T: Ship examples without validation](#dont-ship-examples-without-validation)
  - [Documentation Tiers](#documentation-tiers)
    - [DO: Provide three levels of documentation](#do-provide-three-levels-of-documentation)
    - [DO: Include prerequisites clearly](#do-include-prerequisites-clearly)
    - [DON'T: Assume knowledge without stating it](#dont-assume-knowledge-without-stating-it)
  - [Naming Conventions](#naming-conventions)
    - [DO: Use consistent naming across layers](#do-use-consistent-naming-across-layers)
    - [DON'T: Mix naming conventions](#dont-mix-naming-conventions)
  - [Multi-Output Generation](#multi-output-generation)
    - [DO: Generate multiple synchronized outputs from one source](#do-generate-multiple-synchronized-outputs-from-one-source)
    - [DON'T: Create disconnected generation rules](#dont-create-disconnected-generation-rules)
  - [Git Hygiene](#git-hygiene)
    - [DO: Gitignore generated output](#do-gitignore-generated-output)
    - [DON'T: Commit generated files to version control](#dont-commit-generated-files-to-version-control)
  - [Example Checklist](#example-checklist)
  - [Quick Reference](#quick-reference)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Creating ggen Examples: Dos and Don'ts

A practical guide to creating high-quality ggen examples that demonstrate the RDF-first specification approach.

## Overview

This guide documents the patterns and anti-patterns for creating ggen examples, based on the reference implementation in `examples/openapi/`. Follow these guidelines to create examples that are deterministic, maintainable, and educational.

---

## Directory Structure

### DO: Use a consistent, predictable layout

```
examples/your-example/
├── ggen.toml                 # Generation manifest (required)
├── ontology/                 # RDF source files (required)
│   ├── schema.ttl           # Vocabulary definitions
│   └── instance.ttl         # Instance data
├── templates/                # Tera templates (required)
│   └── *.tera               # One template per output type
├── golden/                   # Expected outputs (recommended)
│   └── lib/                 # Mirrors generated structure
├── lib/                      # Generated output (gitignored)
├── README.md                 # Overview and quick start
├── .gitignore               # Exclude generated files
├── verify.sh                # Validation script (recommended)
└── CONFIGURATION_EXPLAINED.md  # Detailed rule documentation
```

### DON'T: Mix source and generated files

```
# BAD: No separation between source and output
examples/bad-example/
├── ggen.toml
├── ontology.ttl
├── template.tera
├── output.rs          # Generated file in root
└── helpers.rs         # Hand-written file mixed in
```

---

## Ontology Design

### DO: Separate vocabulary from instance data

**Vocabulary schema** (`ontology/api-schema.ttl`):
```turtle
@prefix api: <https://ggen.io/ontology/api#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Define classes
api:Entity a rdfs:Class ;
    rdfs:label "Entity" ;
    rdfs:comment "A domain entity with properties" .

# Define properties
api:name a rdf:Property ;
    rdfs:domain api:Entity ;
    rdfs:range xsd:string .
```

**Instance data** (`ontology/blog-api.ttl`):
```turtle
@prefix blog: <https://ggen.io/examples/blog#> .
@prefix api: <https://ggen.io/ontology/api#> .

blog:User a api:Entity ;
    api:name "User" ;
    rdfs:comment "Blog user account" ;
    api:hasProperty blog:User_email .
```

### DON'T: Hard-code domain data in templates

```tera
{# BAD: Domain data embedded in template #}
{% set entities = ["User", "Post", "Comment"] %}
{% for entity in entities %}
export const {{ entity }}Schema = z.object({});
{% endfor %}
```

---

## Manifest Configuration (ggen.toml)

### DO: Document each rule with comment blocks

```toml
# =============================================================================
# RULE 1: Entity Schemas
# Purpose: Generates validation schemas for all domain entities
# Output: lib/schemas/entities.mjs
# Query: Extracts entities and properties with validation constraints
# =============================================================================
[[generation.rules]]
name = "entity-schemas"
query = { inline = """
PREFIX api: <https://ggen.io/ontology/api#>

SELECT ?entityName ?propertyName ?propertyType ?required
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:hasProperty ?property .
  ?property api:name ?propertyName ;
            api:type ?propertyType .
  OPTIONAL { ?property api:required ?required }
}
ORDER BY ?entityName ?propertyName
""" }
template = { file = "templates/entity-schemas.tera" }
output_file = "schemas/entities.mjs"
mode = "Overwrite"
```

### DON'T: Leave rules undocumented

```toml
# BAD: No context, hard to understand
[[generation.rules]]
name = "schemas"
query = { inline = "SELECT ?x ?y WHERE { ?x a api:Entity ; api:name ?y }" }
template = { file = "templates/schemas.tera" }
output_file = "out.js"
```

---

## SPARQL Queries

### DO: Use ORDER BY for template grouping

```sparql
# GOOD: Sorted results enable entity grouping in templates
SELECT ?entityName ?propertyName ?propertyType
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:hasProperty ?property .
  ?property api:name ?propertyName ;
            api:type ?propertyType .
}
ORDER BY ?entityName ?propertyName
```

### DO: Use OPTIONAL for nullable fields

```sparql
# GOOD: Handles missing optional data gracefully
SELECT ?entityName ?minLength ?maxLength ?format
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName .
  OPTIONAL { ?entity api:minLength ?minLength }
  OPTIONAL { ?entity api:maxLength ?maxLength }
  OPTIONAL { ?entity api:format ?format }
}
```

### DON'T: Assume all fields exist

```sparql
# BAD: Will fail if any entity lacks minLength
SELECT ?entityName ?minLength
WHERE {
  ?entity a api:Entity ;
          api:name ?entityName ;
          api:minLength ?minLength .  # Not optional!
}
```

---

## Template Patterns

### DO: Include YAML frontmatter with documentation

```tera
---
to: lib/schemas/entities.mjs
description: Generates Zod validation schemas from RDF ontology
vars:
  entityName: string - Name of the entity
  propertyName: string - Property identifier
  propertyType: string - Data type (string, integer, boolean)
metadata:
  category: schemas
  output_type: validation
---
```

### DO: Use `?`-prefixed variable access with defaults

```tera
{# GOOD: Safe variable access with fallbacks #}
{%- for row in sparql_results -%}
{%- set entityName = row["?entityName"] | default(value="") -%}
{%- set required = row["?required"] | default(value="false") -%}
{%- set minLength = row["?minLength"] | default(value="") -%}

{% if minLength != "" %}.min({{ minLength }}){% endif %}
{%- endfor %}
```

### DON'T: Access variables without defaults

```tera
{# BAD: Will fail on missing optional fields #}
{%- for row in sparql_results -%}
{%- set minLength = row["?minLength"] -%}  {# No default! #}
.min({{ minLength }})  {# Renders as .min() when undefined #}
{%- endfor %}
```

### DO: Use state variables for grouping

```tera
{# GOOD: Track entity boundaries for proper grouping #}
{%- set current_entity = "" -%}
{%- for row in sparql_results -%}
{%- set entityName = row["?entityName"] | default(value="") -%}

{%- if entityName != current_entity -%}
{%- if current_entity != "" %}
});  {# Close previous entity #}
{% endif -%}
{%- set_global current_entity = entityName %}
export const {{ entityName | lower }}Schema = z.object({
{%- endif %}
  {{ propertyName }}: z.string(),
{%- endfor %}
});
```

### DON'T: Forget to close grouped structures

```tera
{# BAD: Missing closing brace for last entity #}
{%- for row in sparql_results -%}
{%- if entityName != current_entity -%}
export const {{ entityName }}Schema = z.object({
{%- set_global current_entity = entityName %}
{%- endif %}
  {{ propertyName }}: z.string(),
{%- endfor %}
{# Missing final }); #}
```

---

## Generated File Headers

### DO: Mark files as auto-generated

```javascript
/**
 * @fileoverview Generated schemas from RDF ontology
 * DO NOT EDIT - auto-generated by ggen sync
 * @module generated/schemas
 */
```

```yaml
# Generated by ggen sync - DO NOT EDIT
# Source: ontology/blog-api.ttl
# Template: templates/openapi-schemas.tera
openapi: "3.0.3"
```

### DON'T: Leave generated files unmarked

```javascript
// BAD: No indication this is generated
export const userSchema = z.object({
  id: z.string(),
});
```

---

## Validation and Testing

### DO: Provide golden files for comparison

```
golden/lib/
├── schemas/
│   └── entities.mjs    # Expected output
├── types/
│   └── entities.mjs    # Expected output
└── openapi/
    └── openapi.yaml    # Expected output
```

### DO: Include verification scripts

```bash
#!/bin/bash
# verify.sh - Compare generated output with golden files

set -e

echo "Comparing lib/ with golden/lib/..."

diff -r lib/ golden/lib/ && echo "All files match!" || {
    echo "Differences found. Run: diff -r lib/ golden/lib/"
    exit 1
}
```

### DON'T: Ship examples without validation

```
# BAD: No way to verify correctness
examples/untested-example/
├── ggen.toml
├── ontology/
└── templates/
# No golden/, no verify.sh
```

---

## Documentation Tiers

### DO: Provide three levels of documentation

| Document | Audience | Content |
|----------|----------|---------|
| `README.md` | Everyone | Quick start, project structure, prerequisites |
| `BEGINNER_GUIDE.md` | Newcomers | Step-by-step walkthrough, concepts explained |
| `CONFIGURATION_EXPLAINED.md` | Developers | Each rule documented with query explanation |

### DO: Include prerequisites clearly

```markdown
## Prerequisites

### Required Knowledge
- Basic RDF/Turtle syntax ([RDF Primer](https://www.w3.org/TR/rdf11-primer/))
- SPARQL basics ([SPARQL Query Language](https://www.w3.org/TR/sparql11-query/))
- Target language familiarity (JavaScript, Rust, etc.)

### Required Tools
- `ggen` CLI installed and in PATH
- Node.js 18+ (for JavaScript examples)
```

### DON'T: Assume knowledge without stating it

```markdown
## Quick Start

Run `ggen sync` and it will generate the schemas.

<!-- BAD: No context, no prerequisites, no explanation -->
```

---

## Naming Conventions

### DO: Use consistent naming across layers

| Layer | Convention | Example |
|-------|------------|---------|
| Ontology prefixes | lowercase | `blog:`, `api:` |
| Entity URIs | PascalCase | `blog:User`, `blog:Post` |
| Property URIs | Entity_property | `blog:User_email` |
| Template files | kebab-case.tera | `zod-schemas.tera` |
| Output files | kebab-case.ext | `entities.mjs` |
| Generated exports | camelCase | `userSchema`, `isUser` |

### DON'T: Mix naming conventions

```
# BAD: Inconsistent naming
blog:user_entity       # Should be blog:User
blog:UserEMAIL         # Should be blog:User_email
ZOD_SCHEMAS.tera       # Should be zod-schemas.tera
ENTITIES.MJS           # Should be entities.mjs
```

---

## Multi-Output Generation

### DO: Generate multiple synchronized outputs from one source

```
Single ontology (blog-api.ttl)
    ↓
┌─────────────────────────────────────────┐
│ ggen sync                               │
├─────────────────────────────────────────┤
│ → lib/openapi/openapi.yaml   (OpenAPI)  │
│ → lib/schemas/entities.mjs   (Zod)      │
│ → lib/types/entities.mjs     (JSDoc)    │
│ → lib/guards/entities.mjs    (Guards)   │
└─────────────────────────────────────────┘
All outputs are 100% consistent
```

### DON'T: Create disconnected generation rules

```toml
# BAD: Rules don't share ontology, outputs can diverge
[[generation.rules]]
name = "openapi"
query = { file = "queries/openapi.sparql" }  # Different query

[[generation.rules]]
name = "zod"
query = { file = "queries/zod.sparql" }      # Different query - may diverge!
```

---

## Git Hygiene

### DO: Gitignore generated output

```gitignore
# .gitignore
lib/              # Generated output
output/           # Alternative output location
node_modules/     # Dependencies
.ggen/            # Cache directory
```

### DON'T: Commit generated files to version control

```
# BAD: Generated files in git history
git add lib/schemas/entities.mjs  # This is generated!
git commit -m "Add schemas"       # Will cause merge conflicts
```

---

## Example Checklist

Before publishing a ggen example, verify:

- [ ] **Ontology**: TTL files in `ontology/` with schema and instance data separated
- [ ] **Manifest**: `ggen.toml` with documented rules and clear output paths
- [ ] **Templates**: `.tera` files with YAML frontmatter and safe variable access
- [ ] **Golden files**: Expected outputs in `golden/` for comparison
- [ ] **Verification**: `verify.sh` or `validate.mjs` script that compares outputs
- [ ] **README**: Quick start, prerequisites, project structure
- [ ] **Gitignore**: Generated `lib/` directory excluded from version control
- [ ] **Headers**: All generated files marked with "DO NOT EDIT" comment
- [ ] **Naming**: Consistent conventions across all layers
- [ ] **Multi-output**: Single ontology generates multiple synchronized formats

---

## Quick Reference

| Aspect | Do | Don't |
|--------|-----|-------|
| **Source of truth** | TTL ontology files | Hard-coded template data |
| **Directory layout** | ontology/, templates/, lib/ | Flat structure |
| **SPARQL variables** | `row["?varName"]` with `default()` | Direct access without `?` |
| **Grouping** | ORDER BY + state variables | Nested loops in template |
| **Optional fields** | OPTIONAL clause + default filter | Required patterns only |
| **Generated files** | Gitignored, marked as generated | Committed to repo |
| **Validation** | Golden files + verify script | No verification |
| **Documentation** | Three tiers (README, guide, config) | Single README |

---

## See Also

- `examples/openapi/` - Reference implementation demonstrating all patterns
- `examples/openapi/BEGINNER_GUIDE.md` - Step-by-step tutorial
- `examples/openapi/CONFIGURATION_EXPLAINED.md` - Detailed rule documentation
- `CLAUDE.md` - Project constitution and RDF-first principles
