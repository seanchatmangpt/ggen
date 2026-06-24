# Getting Started with ggen Ontology Embedding

Welcome to ggen v26.5.28! This guide will help you use embedded ontologies to generate code with zero network dependency.

## What is ggen?

ggen is a specification-driven code generation tool that transforms RDF ontologies into typed source code. **The key innovation in v26.5.28 is ontology embedding**: 12 W3C standard ontologies are compiled into the binary, meaning you can generate code completely offline.

## Installation

### From Cargo

```bash
cargo install ggen-cli
```

### From Source

```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo install --path crates/ggen-cli
```

### Verify Installation

```bash
ggen --version
# Output: ggen 26.5.28
```

## Your First Generation (5 minutes)

### Step 1: Create a Project Directory

```bash
mkdir my-ggen-project
cd my-ggen-project
```

### Step 2: Initialize a Project

```bash
ggen init
```

This creates:
- `ggen.toml` — Project configuration
- `schema/domain.ttl` — Your RDF ontology (example)
- `templates/` — Directory for Tera templates
- `.ggen/` — Build artifacts and cache

### Step 3: Check Available Embedded Ontologies

```bash
ggen ontology list --embedded
```

Output shows all 12 core ontologies available offline:
- RDF (http://www.w3.org/1999/02/22-rdf-syntax-ns#)
- RDFS (http://www.w3.org/2000/01/rdf-schema#)
- OWL (http://www.w3.org/2002/07/owl#)
- DC (Dublin Core Terms)
- DCAT (Data Catalog)
- FOAF (Friend of a Friend)
- VCARD (vCard)
- SKOS (Simple Knowledge Organization System)
- PROV (PROV-O)
- And 3 more...

### Step 4: Use an Embedded Ontology

Edit `ggen.toml`:

```toml
[pipeline]
ontology_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

[[pipelines.steps]]
sparql = """
SELECT ?resource
WHERE {
  ?resource rdf:type rdf:Property .
}
"""

[output]
file = "properties.json"
format = "json"
```

### Step 5: Generate Code

```bash
ggen sync --dry-run
```

This shows what will be generated without writing files.

```bash
ggen sync
```

This generates your code. Check the generated artifacts in your project.

### Step 6: Verify Determinism

Run the same command again:

```bash
ggen sync
```

You'll see the exact same output hash (deterministic generation). This proves the code generation is reproducible.

## Common Workflows

### Workflow 1: Offline Code Generation

Perfect for environments without internet access.

```bash
# All core ontologies work without network
ggen ontology status http://www.w3.org/2002/07/owl#
# Output: EMBEDDED (available offline)

# Generate code entirely offline
ggen sync --offline
```

### Workflow 2: Mix Embedded + Marketplace Packages

Start with embedded ontologies, add domain-specific packages.

```bash
# Install a domain-specific package (financial services)
ggen ontology install financial/banking@1.0.0

# Use it in your pipeline
ggen sync
```

### Workflow 3: Multi-Domain Code Generation

Generate code from multiple ontologies in one run.

```bash
# List what's available
ggen ontology search financial

# Create a lock file (reproducible dependencies)
ggen ontology lock

# Generate code
ggen sync
```

## Hello World Examples

### Example 1: Generate JSON Schema from RDF

**File: `ggen.toml`**
```toml
[pipeline]
ontology_uri = "http://www.w3.org/2000/01/rdf-schema#"

[[pipelines.steps]]
sparql = """
SELECT ?class ?label
WHERE {
  ?class rdf:type rdfs:Class .
  ?class rdfs:label ?label .
}
ORDER BY ?label
"""

[output]
file = "schema.json"
format = "json"
template = "json_schema.tera"
```

**File: `templates/json_schema.tera`**
```jinja2
{
  "definitions": [
    {% for row in results %}
    {
      "name": "{{ row.class }}",
      "label": "{{ row.label }}"
    }{% if not loop.last %},{% endif %}
    {% endfor %}
  ]
}
```

Run:
```bash
ggen sync
cat schema.json
```

### Example 2: Generate TypeScript Types from OWL

**File: `ggen.toml`**
```toml
[pipeline]
ontology_uri = "http://www.w3.org/2002/07/owl#"

[[pipelines.steps]]
sparql = """
SELECT ?class ?property ?type
WHERE {
  ?class a owl:Class .
  ?class a ?property .
  ?property a owl:ObjectProperty | owl:DatatypeProperty .
}
"""

[output]
file = "types.ts"
format = "typescript"
template = "typescript_types.tera"
```

**File: `templates/typescript_types.tera`**
```typescript
{% for class in results | group_by(attribute="class") %}
export interface {{ class.key | title }} {
  {% for prop in class.value %}
  {{ prop.property }}: {{ prop.type }};
  {% endfor %}
}
{% endfor %}
```

## Understanding ggen.toml

The configuration file drives code generation:

```toml
[pipeline]
# Required: RDF ontology to load (embedded or marketplace)
ontology_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

# Optional: Base ontology to mix (second ontology in same run)
ontology_base = "http://www.w3.org/2000/01/rdf-schema#"

[[pipelines.steps]]
# SPARQL query to extract data from the ontology
sparql = """
SELECT ?resource ?label
WHERE {
  ?resource a rdf:Property .
  ?resource rdfs:label ?label .
}
ORDER BY ?label
"""

# Optional: Template to transform the SPARQL results
template = "output.tera"

[output]
# Output file path (relative to project root)
file = "generated.json"

# Output format (json, yaml, xml, turtle, typescript, python, go, etc.)
format = "json"

[validation]
# Enable strict validation (reject profiles that violate constraints)
strict_mode = true
```

## Troubleshooting

### "Ontology not found" Error

```bash
$ ggen sync
Error: Ontology http://example.com/custom.ttl not found
```

**Solution**: Check if the ontology is embedded or installed.

```bash
ggen ontology list --embedded
ggen ontology status http://example.com/custom.ttl
```

If not found, install from marketplace:
```bash
ggen ontology install example/custom@1.0.0
```

Or load from file:
```bash
# In ggen.toml, use file:// URI
ontology_uri = "file:///home/user/my-ontology.ttl"
ggen sync
```

### "SPARQL Query Error" Error

```bash
$ ggen sync
Error: SPARQL parse error: unexpected token
```

**Solution**: Validate your SPARQL syntax.

```bash
# Use ggen's SPARQL validator
ggen validate-sparql "SELECT ?x WHERE { ?x a rdf:Property . }"
```

Most common issues:
- Missing period at end of query
- Misspelled RDF properties
- Using undefined prefixes

### "Permission Denied" Error

```bash
Error: Permission denied: .ggen/cache
```

**Solution**: Fix cache directory permissions.

```bash
chmod -R u+w .ggen/cache
ggen sync
```

## Performance Tips

### Tip 1: Use `--offline` for Embedded Ontologies

Embedding ontologies is fast (<1 microsecond lookup), but saving the flag prevents any network attempts:

```bash
ggen sync --offline
```

### Tip 2: Create Lock Files for Reproducibility

Lock files pin package versions and prevent upgrades:

```bash
ggen ontology lock
ggen sync
```

Lock files ensure the same code generation in CI/CD.

### Tip 3: Cache Marketplace Packages

Once installed, packages are cached locally:

```bash
ggen ontology install financial/banking@1.0.0
# First run: downloads from marketplace (~500 ms)

ggen sync
# Subsequent runs: uses cache (<10 ms)
```

### Tip 4: Batch Multiple Pipelines

If you have multiple `[[pipelines.steps]]` in `ggen.toml`, they run sequentially. Combine where possible:

```toml
[[pipelines.steps]]
sparql = """
SELECT ?resource ?label ?comment
WHERE {
  ?resource a rdf:Property .
  ?resource rdfs:label ?label .
  OPTIONAL { ?resource rdfs:comment ?comment . }
}
"""
```

## Next Steps

1. **[Read the User Guide](./USAGE_GUIDE.md)** — Comprehensive workflows and advanced usage
2. **[Check the API Reference](./API_REFERENCE.md)** — Full API documentation for custom integrations
3. **[Browse Examples](../examples/)** — 40+ ready-to-run projects
4. **[Run the Tests](../crates/ggen-core/tests/)** — See integration tests for complex patterns

## Key Concepts

### Embedded Ontologies

W3C standard ontologies (RDF, RDFS, OWL, etc.) are compiled into the binary. They're always available, even without internet.

**Advantages:**
- Zero network latency
- Works offline
- Reproducible (no version changes)
- 12 KB binary size overhead

### Marketplace Packages

Domain-specific ontologies you install separately. Perfect for financial, healthcare, manufacturing domains.

**Advantages:**
- Specialized vocabularies
- Kept up-to-date
- Versioned and signed
- Optional (only install what you need)

### Deterministic Generation

Every `ggen sync` produces identical output for the same inputs. Verified with SHA-256 hashes.

**Benefits:**
- Reproducible CI/CD
- Cryptographic receipts prove what was generated
- Prevents accidental divergence

## Getting Help

- **Stuck?** Check [TROUBLESHOOTING.md](./TROUBLESHOOTING.md)
- **Questions?** See [FAQ.md](./FAQ.md)
- **Want to contribute?** [See CONTRIBUTING.md](../CONTRIBUTING.md)
- **Report a bug?** [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)

## What's Next in ggen?

- **Phase 7**: Private registry support (self-hosted ontology marketplace)
- **Phase 8**: GUI marketplace browser
- **Phase 9**: Custom ontology registration
- **Phase 10**: Performance optimizations (parallel pipeline stages)

---

**Happy generating!** 🎉

For more details, see:
- [USAGE_GUIDE.md](./USAGE_GUIDE.md) — Complete workflows
- [API_REFERENCE.md](./API_REFERENCE.md) — Rust API documentation
- [TROUBLESHOOTING.md](./TROUBLESHOOTING.md) — Common issues and solutions
