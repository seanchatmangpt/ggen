# Documentation as Code Pattern

This demonstrates the **proper way** to manage documentation in ggen:

## The 3T Pattern

### 1. **Turtle (RDF)** - Data Layer
File: `documentation.ttl`

Defines documentation as RDF classes and properties:
- `doc:Guide` - Learning-oriented documentation
- `doc:Reference` - Quick-lookup documentation
- `doc:Gap` - Documentation gaps to fill

Properties:
- `doc:title` - Document title
- `doc:description` - What it covers
- `doc:pages` - Number of pages
- `doc:effort` - S/M/L/XL effort
- `doc:priority` - P0/P1/P2/P3 priority (for gaps)
- `doc:category` - Learning/Reference/Example/Feature
- `doc:outputFile` - Output filename
- `doc:template` - Which template to use

### 2. **ggen.toml** - Orchestration Layer
File: `ggen.toml`

Defines:
- **Inference Rules (SPARQL CONSTRUCT)**:
  - `guide_hierarchy`: Materialize all guide properties
  - `reference_hierarchy`: Materialize all reference properties
  - `gap_priority`: Materialize gap groupings

- **Generation Rules (SPARQL SELECT → Tera)**:
  - `guides`: Generate guides manifest
  - `references`: Generate references manifest
  - `gaps`: Generate gap analysis
  - `todos`: Generate actionable todos

### 3. **Tera** - Template Layer
Files in `templates/`:
- `generate-guides.tera` - Render guide table + metadata
- `generate-gap-analysis.tera` - Organize gaps by priority + category
- `generate-todos.tera` - Create actionable checklist

## How It Works

```bash
# 1. Define documentation structure in RDF
# documentation.ttl contains all docs + gaps

# 2. Configure generation in ggen.toml
# - Inference rules materialize structure
# - Generation rules query the graph
# - Templates render to markdown

# 3. Generate everything
ggen sync

# Output:
# GENERATED_GUIDES_MANIFEST.txt (markdown table)
# GENERATED_REFERENCES_MANIFEST.txt (markdown table)
# GENERATED_GAP_ANALYSIS.md (organized gap list)
# GENERATED_TODOS.md (actionable checklist)
```

## The Right Way vs The Wrong Way

### ❌ WRONG: Write markdown prose
```markdown
# Documentation Index

This is a guide about ggen. It teaches SPARQL patterns...
[20 pages of prose explaining everything]
```

### ✅ RIGHT: Define data + generate docs
```turtle
# Define in RDF
doc:guide-sparql a doc:Guide ;
  doc:title "SPARQL Guide" ;
  doc:pages 20 .
```

```jinja2
{# Generate with Tera #}
| **{{ row.title }}** | {{ row.pages }} pages |
```

```bash
# Generate
ggen sync
```

## Key Benefits

1. **Single Source of Truth**: RDF is authoritative
2. **Queryable**: Use SPARQL to organize/filter
3. **Maintainable**: Update .ttl, re-generate all markdown
4. **Versionable**: Track changes to ontology, not prose
5. **Automatable**: CI/CD can regenerate docs on every change
6. **Consistent**: Same template structure for all docs
7. **Composable**: Mix and match inference rules

## What You Should Have Done

Instead of writing this in prose markdown:
```
# Gap Analysis

We identified 50+ documentation gaps...
CRITICAL GAPS:
1. SHACL Validation
2. Troubleshooting Guide
...
```

You should have:

1. **Defined gaps in RDF** (documentation.ttl)
```turtle
doc:gap-shacl a doc:Gap ;
  doc:title "Complete SHACL Validation" ;
  doc:priority "P0" ;
  doc:category "Feature" .
```

2. **Queried with SPARQL** (ggen.toml)
```sparql
SELECT ?title ?priority ?category WHERE {
  ?gap doc:title ?title ;
    doc:priority ?priority ;
    doc:category ?category .
}
```

3. **Generated markdown** (generate-gap-analysis.tera)
```jinja2
### {{ priority }}
| {{ gap.title }} | {{ gap.priority }} | {{ gap.category }} |
```

4. **Let ggen sync create it**
```bash
ggen sync
```

## The Meta Insight

**ggen is for generating code from RDF.**
**So you should use ggen to generate documentation from RDF too!**

This is "eating your own dog food" - using the tool itself to solve the problem it was designed to solve.

---

**Files:**
- `documentation.ttl` - RDF ontology of all documentation
- `ggen.toml` - Configuration + SPARQL queries
- `templates/*.tera` - Markdown generation templates
- `README.md` - This file

**To generate documentation:**
```bash
cd /home/user/ggen
ggen sync --config .specify/specs/999-docs-as-code/ggen.toml
```

**Output in:** `docs/GENERATED_*.md`
