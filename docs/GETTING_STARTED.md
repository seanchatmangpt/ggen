# Getting Started with ggen Ontology Embedding

Welcome to ggen! This guide will help you use embedded ontologies to generate code with zero network dependency.

> Command syntax below is verified live against the current CLI (`ggen --help`, post
> `2026-ggen-core-replacement` migration, PR #255). `ggen sync` requires the `run` subcommand
> (`ggen sync run`, not bare `ggen sync`); there is no `--offline` flag on `sync run`; there is
> no `ggen validate-sparql` command. The ontology/marketplace conceptual content below (embedded
> ontologies, package installation) was not re-verified end-to-end in this pass — see
> `docs/reference/ggen_sync_manual.md` and `ggen ontology --help` for the current authoritative
> surface if something here doesn't match.

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

This creates 7 files/dirs (confirmed live via `ggen init`'s own JSON output,
`"total_files": 7`):
- `ggen.toml` — Project configuration
- `schema/domain.ttl` — Your RDF ontology (example)
- `templates/example.txt.tera` — Directory + example Tera template
- `Makefile`, `.gitignore`, `README.md`, `scripts/startup.sh` — supporting project scaffolding

Note: `ggen init` does **not** create a `.ggen/` directory — that only appears as a line inside
the generated `.gitignore`. `.ggen/` (and `.ggen-v2/`) get created later, on first `ggen sync run`.

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

Edit `ggen.toml` (this is the real "declarative-rules" schema `ggen init` scaffolds and
`ggen sync run` parses — confirmed against `crates/ggen-cli/src/cmds/init.rs`'s `GGEN_TOML`
const; the `[pipeline]`/`ontology_uri`/`[[pipelines.steps]]`/`[output].file` shape shown in an
earlier version of this doc does not exist anywhere in the codebase):

```toml
[project]
name = "my-ggen-project"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"
standard_only = true

[generation]
output_dir = "."

[[generation.rules]]
name = "rdf-properties"
query = { inline = """
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?resource
WHERE {
  ?resource rdf:type rdf:Property .
}
""" }
template = { file = "templates/example.txt.tera" }
output_file = "properties.json"
mode = "Overwrite"
```

### Step 5: Generate Code

```bash
ggen sync run --dry-run
```

This shows what will be generated without writing files.

```bash
ggen sync run
```

This generates your code. Check the generated artifacts in your project.

### Step 6: Verify Determinism

Run the same command again:

```bash
ggen sync run
```

You'll see the exact same output hash (deterministic generation). This proves the code generation is reproducible.

## Common Workflows

### Workflow 1: Offline Code Generation

Perfect for environments without internet access.

```bash
# All core ontologies work without network
ggen ontology status http://www.w3.org/2002/07/owl#
# Output: EMBEDDED (available offline)

# Generate code (embedded ontologies resolve locally; no --offline flag exists on `sync run`)
ggen sync run
```

### Workflow 2: Mix Embedded + Marketplace Packages

Start with embedded ontologies, add domain-specific packages.

```bash
# Install a domain-specific package (financial services)
ggen ontology install financial/banking@1.0.0

# Use it in your pipeline
ggen sync run
```

### Workflow 3: Multi-Domain Code Generation

Generate code from multiple ontologies in one run.

```bash
# List what's available
ggen ontology search financial

# Create a lock file (reproducible dependencies)
ggen ontology lock

# Generate code
ggen sync run
```

## Hello World Examples

### Example 1: Generate JSON Schema from RDF

**File: `ggen.toml`**
```toml
[project]
name = "json-schema-example"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"
standard_only = true

[generation]
output_dir = "."

[[generation.rules]]
name = "json-schema"
query = { inline = """
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?class ?label
WHERE {
  ?class rdf:type rdfs:Class .
  ?class rdfs:label ?label .
}
ORDER BY ?label
""" }
template = { file = "templates/json_schema.tera" }
output_file = "schema.json"
mode = "Overwrite"
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
ggen sync run
cat schema.json
```

### Example 2: Generate TypeScript Types from OWL

**File: `ggen.toml`**
```toml
[project]
name = "typescript-types-example"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"
standard_only = true

[generation]
output_dir = "."

[[generation.rules]]
name = "typescript-types"
query = { inline = """
PREFIX owl: <http://www.w3.org/2002/07/owl#>
SELECT ?class ?property ?type
WHERE {
  ?class a owl:Class .
  ?class a ?property .
  ?property a owl:ObjectProperty | owl:DatatypeProperty .
}
""" }
template = { file = "templates/typescript_types.tera" }
output_file = "types.ts"
mode = "Overwrite"
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

**ggen.toml has two independently-parsed, incompatible schemas** (see
`.claude/rules/architecture.md`'s "ggen.toml has two schemas" section for the full mechanism).
What `ggen init` scaffolds, and what this guide's examples use, is the "declarative-rules"
schema (`ggen_config::manifest::GgenManifest`) — chosen automatically whenever the file has a
non-empty `[[generation.rules]]` array. The fields below are verified against
`crates/ggen-cli/src/cmds/init.rs`'s `GGEN_TOML` scaffold const:

```toml
[project]
name = "my-ggen-project"
version = "0.1.0"

[ontology]
# Path to your RDF ontology file (Turtle format)
source = "schema/domain.ttl"
# Restrict to standard ontologies (schema.org, FOAF, Dublin Core, SKOS, etc.)
standard_only = true

[generation]
output_dir = "."

# One or more generation rules; each pairs a SPARQL query with a template
[[generation.rules]]
name = "example-rule"
# Inline SPARQL, or { file = "path/to/query.sparql" }
query = { inline = """
SELECT ?resource ?label
WHERE {
  ?resource a rdf:Property .
  ?resource rdfs:label ?label .
}
ORDER BY ?label
""" }
# Template file, relative to the project root
template = { file = "templates/output.tera" }
# Output file path, relative to output_dir
output_file = "generated.json"
# Create | Overwrite — Create silently skips files that already exist
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
```

There is no `[pipeline]`/`ontology_uri`/`[[pipelines.steps]]`/bare-`sparql`-string shape, and no
`[validation].strict_mode` field, in either real schema — an earlier version of this doc invented
one that the codebase has never implemented.

## Troubleshooting

### "Ontology not found" Error

```bash
$ ggen sync run
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
ggen sync run
```

### "SPARQL Query Error" Error

```bash
$ ggen sync run
Error: SPARQL parse error: unexpected token
```

**Solution**: Validate your SPARQL syntax. There is no standalone `ggen validate-sparql`
command — `ggen graph validate` validates a Turtle ontology graph (not a raw SPARQL string), and
`ggen sync run --dry-run` will surface SPARQL parse errors from your `ggen.toml` pipeline steps
without writing files:

```bash
ggen sync run --dry-run
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
ggen sync run
```

## Performance Tips

### Tip 1: Embedded Ontologies Resolve Without Network Calls

Embedded ontologies are compiled into the binary and looked up locally — no flag is needed to
avoid network access for them (there is no `--offline` flag on `ggen sync run`):

```bash
ggen sync run
```

### Tip 2: Create Lock Files for Reproducibility

Lock files pin package versions and prevent upgrades:

```bash
ggen ontology lock
ggen sync run
```

Lock files ensure the same code generation in CI/CD.

### Tip 3: Cache Marketplace Packages

Once installed, packages are cached locally:

```bash
ggen ontology install financial/banking@1.0.0
# First run: downloads from marketplace (~500 ms)

ggen sync run
# Subsequent runs: uses cache (<10 ms)
```

### Tip 4: Batch Multiple Generation Rules

If you have multiple `[[generation.rules]]` entries in `ggen.toml`, they run sequentially.
Combine queries where possible rather than adding more rules than you need:

```toml
[[generation.rules]]
name = "combined-rule"
query = { inline = """
SELECT ?resource ?label ?comment
WHERE {
  ?resource a rdf:Property .
  ?resource rdfs:label ?label .
  OPTIONAL { ?resource rdfs:comment ?comment . }
}
""" }
template = { file = "templates/output.tera" }
output_file = "generated.json"
mode = "Overwrite"
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

Every `ggen sync run` produces identical output for the same inputs. Verified with SHA-256 hashes.

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
