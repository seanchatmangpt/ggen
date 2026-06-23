# ggen Usage Guide: Complete Workflows

This guide covers advanced ggen workflows for production use.

## Table of Contents

- [Using Embedded Ontologies Only](#using-embedded-ontologies-only)
- [Installing Marketplace Packages](#installing-marketplace-packages)
- [Version Management & Lock Files](#version-management--lock-files)
- [Multi-Domain Code Generation](#multi-domain-code-generation)
- [Performance Optimization](#performance-optimization)
- [Real-World Examples](#real-world-examples)
- [Continuous Integration](#continuous-integration)
- [Troubleshooting Advanced Issues](#troubleshooting-advanced-issues)

## Using Embedded Ontologies Only

### Offline-First Development

If your team works offline (airplane mode, restricted networks, or air-gapped environments), embedded ontologies are your answer.

**Step 1: Verify All Ontologies Are Embedded**

```bash
ggen ontology list --embedded
```

Output:
```
EMBEDDED ONTOLOGIES (12 total, 448 KB)

Name     | Namespace                                  | Size    | Status
---------|--------------------------------------------|---------|---------
rdf      | http://www.w3.org/1999/02/22-rdf-syntax-ns# | 2.1 KB  | EMBEDDED
rdfs     | http://www.w3.org/2000/01/rdf-schema#      | 1.8 KB  | EMBEDDED
owl      | http://www.w3.org/2002/07/owl#             | 4.2 KB  | EMBEDDED
dc       | http://purl.org/dc/terms/                  | 3.5 KB  | EMBEDDED
dcat     | http://www.w3.org/ns/dcat#                 | 5.1 KB  | EMBEDDED
foaf     | http://xmlns.com/foaf/0.1/                 | 2.3 KB  | EMBEDDED
vcard    | http://www.w3.org/2006/vcard/ns#           | 6.7 KB  | EMBEDDED
skos     | http://www.w3.org/2004/02/skos/core#       | 7.2 KB  | EMBEDDED
prov     | http://www.w3.org/ns/prov#                 | 8.1 KB  | EMBEDDED
xml      | http://www.w3.org/2001/XMLSchema#          | 15 KB   | EMBEDDED
xsd      | http://www.w3.org/2001/XMLSchema#          | 14 KB   | EMBEDDED
doap     | http://usefulinc.com/ns/doap#              | 4.1 KB  | EMBEDDED
```

**Step 2: Configure for Offline Mode**

**File: `ggen.toml`**
```toml
[pipeline]
ontology_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

[[pipelines.steps]]
sparql = """
SELECT ?resource ?label
WHERE {
  ?resource a rdf:Property .
  OPTIONAL { ?resource rdfs:label ?label . }
}
ORDER BY ?resource
"""

[output]
file = "properties.json"
format = "json"

[network]
# Force offline mode (fail hard if network is required)
allow_network = false
```

**Step 3: Generate Code Offline**

```bash
ggen sync
```

No network calls will be made. If you try to use an ontology that's not embedded, you'll get a clear error:

```
Error: Ontology http://example.com/custom.ttl not found
Available embedded ontologies:
  - http://www.w3.org/1999/02/22-rdf-syntax-ns#
  - http://www.w3.org/2000/01/rdf-schema#
  ... (10 more)

To use other ontologies, either:
1. Embed them in core bundle (compile from source)
2. Install from marketplace: ggen ontology install <package>
3. Load from file: Use file:// URI instead
```

### When to Use Embedded Ontologies

| Scenario | Recommendation |
|----------|----------------|
| Offline environment | ✅ Embedded only |
| Air-gapped network | ✅ Embedded only |
| Personal development | ✅ Embedded only (fastest) |
| CI/CD with no internet | ✅ Embedded only + lock files |
| Domain-specific projects | ❌ Mix embedded + packages |

## Installing Marketplace Packages

### Finding Packages

Search the marketplace for domain-specific ontologies:

```bash
ggen ontology search financial
```

Output:
```
MARKETPLACE PACKAGES (financial domain)

Package              | Version | Downloads | Status
---------------------|---------|-----------|--------
financial/banking    | 1.2.1   | 1,234     | STABLE
financial/insurance  | 1.1.0   | 856       | STABLE
financial/accounting | 0.9.2   | 423       | BETA
financial/investing  | 1.0.0   | 567       | STABLE
```

### Installing a Package

```bash
# Install the latest version
ggen ontology install financial/banking

# Install a specific version
ggen ontology install financial/banking@1.2.1

# Install multiple packages at once
ggen ontology install financial/banking financial/insurance healthcare/hl7
```

### Verifying Installation

```bash
ggen ontology status financial/banking@1.2.1
```

Output:
```
Package: financial/banking@1.2.1
Status: INSTALLED
Location: ~/.ggen/packages/financial/banking/1.2.1/
Size: 1.2 MB
Checksum: sha256:a3f5c7e1d9b2e4f6a8c1d3e5f7a9b1c3
Installed: 2026-06-23T10:45:32Z
Cache hits: 142
```

### Using Installed Packages

**File: `ggen.toml`**
```toml
[pipeline]
# Use marketplace package
ontology_uri = "marketplace:financial/banking@1.2.1"

[[pipelines.steps]]
sparql = """
SELECT ?account ?balance ?currency
WHERE {
  ?account a banking:BankAccount .
  ?account banking:balance ?balance .
  ?account banking:currency ?currency .
}
ORDER BY ?account
"""

[output]
file = "accounts.json"
format = "json"
template = "accounts.tera"
```

**File: `templates/accounts.tera`**
```jinja2
{
  "accounts": [
    {% for row in results %}
    {
      "account_id": "{{ row.account }}",
      "balance": {{ row.balance }},
      "currency": "{{ row.currency }}"
    }{% if not loop.last %},{% endif %}
    {% endfor %}
  ]
}
```

### Uninstalling Packages

```bash
# Remove a specific package version
ggen ontology uninstall financial/banking@1.2.1

# Remove all versions of a package
ggen ontology uninstall financial/banking --all

# Clean up all cached packages
ggen ontology cache clean
```

## Version Management & Lock Files

### Why Lock Files Matter

When you run `ggen sync` without a lock file, ggen uses:
1. Embedded ontologies (fixed version in binary)
2. Latest marketplace packages

This means CI/CD might generate different code when a new package version is released.

**Lock files solve this by pinning versions.**

### Creating Lock Files

```bash
# Create a lock file for your current configuration
ggen ontology lock
```

This creates `ggen.lock`:

```json
{
  "created_at": "2026-06-23T12:34:56Z",
  "ggen_version": "26.5.28",
  "packages": [
    {
      "package": "embedded:rdf",
      "version": "v26.5.28-embedded",
      "checksum": "sha256:abc123...",
      "size_bytes": 2048
    },
    {
      "package": "financial/banking",
      "version": "1.2.1",
      "checksum": "sha256:def456...",
      "size_bytes": 1245678,
      "installed_at": "2026-06-23T10:45:32Z"
    }
  ],
  "output_hash": "sha256:xyz789...",
  "pipeline_hash": "sha256:pqr012..."
}
```

### Using Lock Files in CI/CD

Commit `ggen.lock` to version control:

```bash
git add ggen.lock
git commit -m "chore: lock ontology versions for reproducible builds"
git push
```

In CI/CD, use the lock file:

```bash
# GitHub Actions
- name: Generate code
  run: ggen sync --locked

# GitLab CI
script:
  - ggen sync --locked

# Jenkins
sh 'ggen sync --locked'
```

If a package version is missing, it fails:

```
Error: Package financial/banking@1.2.1 not installed
Lock file requires version 1.2.1 but only 1.2.0 is available

To update, run:
  ggen ontology lock --update
```

### Updating Lock Files

When you want to upgrade packages:

```bash
# Update all packages to latest versions
ggen ontology lock --update

# Update a specific package
ggen ontology lock --update financial/banking

# Review changes before committing
git diff ggen.lock
```

### Lock File Verification

Verify integrity of locked packages:

```bash
ggen ontology verify --locked
```

Output:
```
Verifying lock file integrity...

Package                    | Status   | Message
---------------------------|----------|------------------
embedded:rdf               | ✅ OK    | Checksum valid
embedded:rdfs              | ✅ OK    | Checksum valid
financial/banking@1.2.1    | ✅ OK    | Checksum valid
healthcare/hl7@3.1.2       | ❌ FAIL  | Checksum mismatch (package may be corrupted)

Result: 2 packages verified, 1 failed
```

## Multi-Domain Code Generation

### Combining Multiple Ontologies

Generate code from financial + healthcare ontologies in one run:

**File: `ggen.toml`**
```toml
[pipeline]
# Primary ontology
ontology_uri = "marketplace:financial/banking@1.2.1"

# Secondary ontologies (mixed in same run)
ontology_mixins = [
  "marketplace:healthcare/hl7@3.1.2",
  "embedded:dcat"
]

[[pipelines.steps]]
sparql = """
SELECT ?entity ?type ?name
WHERE {
  {
    ?entity a banking:BankAccount .
    BIND('BankAccount' AS ?type)
  } UNION {
    ?entity a hl7:Patient .
    BIND('Patient' AS ?type)
  } UNION {
    ?entity a dcat:Dataset .
    BIND('Dataset' AS ?type)
  }
  ?entity rdf:label ?name .
}
ORDER BY ?type ?name
"""

[output]
file = "entities.json"
format = "json"
template = "entities.tera"
```

### Sequential Pipelines

Generate multiple artifacts in sequence:

**File: `ggen.toml`**
```toml
[pipeline]
ontology_uri = "marketplace:financial/banking@1.2.1"

# First generation: Accounts
[[pipelines.steps]]
name = "accounts"
sparql = """
SELECT ?account ?balance
WHERE {
  ?account a banking:BankAccount .
  ?account banking:balance ?balance .
}
"""
output_file = "accounts.json"
template = "accounts.tera"

# Second generation: Transactions
[[pipelines.steps]]
name = "transactions"
sparql = """
SELECT ?transaction ?amount ?date ?account
WHERE {
  ?transaction a banking:Transaction .
  ?transaction banking:amount ?amount .
  ?transaction banking:date ?date .
  ?transaction banking:relatedAccount ?account .
}
"""
output_file = "transactions.json"
template = "transactions.tera"

# Third generation: Reports
[[pipelines.steps]]
name = "reports"
sparql = """
SELECT ?account ?total_balance
WHERE {
  ?account a banking:BankAccount .
  {
    SELECT ?account (SUM(?amount) AS ?total_balance)
    WHERE {
      ?transaction banking:amount ?amount .
      ?transaction banking:relatedAccount ?account .
    }
    GROUP BY ?account
  }
}
"""
output_file = "reports.json"
template = "reports.tera"
```

Run one at a time or all together:

```bash
# Generate everything
ggen sync

# Generate only "accounts"
ggen sync --step accounts

# Generate "accounts" and "transactions"
ggen sync --steps accounts,transactions
```

## Performance Optimization

### Benchmark Your Pipeline

Check how long generation takes:

```bash
ggen sync --audit
```

Output:
```
μ₁ (Load Ontology):       45 ms
μ₂ (Extract SPARQL):      128 ms
μ₃ (Render Templates):    234 ms
μ₄ (Canonicalize):        12 ms
μ₅ (Generate Receipt):    8 ms
────────────────────────────────
Total:                     427 ms

Performance SLOs:
✅ Load: 45 ms < 5s target
✅ Extract: 128 ms < 5s target
✅ Render: 234 ms < 5s target
✅ Canonicalize: 12 ms < 100ms target
✅ Receipt: 8 ms < 100ms target
────────────────────────────────
✅ ALL SLOs MET
```

### Optimization Tips

#### Tip 1: Prefer Embedded Ontologies

Embedded lookups are microseconds fast:

```toml
# Fast (embedded)
ontology_uri = "embedded:rdf"

# Slow (marketplace, first load)
ontology_uri = "marketplace:financial/banking"
```

The first marketplace load downloads the package. Subsequent runs use cache.

#### Tip 2: Cache Marketplace Packages Locally

```bash
# First run: downloads package
ggen sync  # ~500 ms

# Second run: uses cache
ggen sync  # ~50 ms
```

Cache location: `~/.ggen/packages/`

#### Tip 3: Reduce SPARQL Complexity

Simpler queries are faster:

```sparql
# ❌ SLOW: Multiple optional patterns
SELECT ?resource ?label ?comment ?seeAlso
WHERE {
  ?resource a rdf:Property .
  OPTIONAL { ?resource rdfs:label ?label . }
  OPTIONAL { ?resource rdfs:comment ?comment . }
  OPTIONAL { ?resource rdfs:seeAlso ?seeAlso . }
  OPTIONAL { ?resource rdf:value ?value . }
  OPTIONAL { ?resource rdfs:domain ?domain . }
}

# ✅ FAST: Only required patterns
SELECT ?resource ?label
WHERE {
  ?resource a rdf:Property .
  ?resource rdfs:label ?label .
}
```

#### Tip 4: Batch Multiple Steps

Combine sequential pipelines into one SPARQL query:

```toml
# ❌ SLOW: Three separate queries (128 + 128 + 128 = 384 ms)
[[pipelines.steps]]
name = "accounts"
sparql = "SELECT ?account WHERE { ?account a banking:BankAccount . }"

[[pipelines.steps]]
name = "transactions"
sparql = "SELECT ?transaction WHERE { ?transaction a banking:Transaction . }"

[[pipelines.steps]]
name = "customers"
sparql = "SELECT ?customer WHERE { ?customer a banking:Customer . }"

# ✅ FAST: One combined query (128 ms)
[[pipelines.steps]]
name = "all"
sparql = """
SELECT ?account ?transaction ?customer
WHERE {
  OPTIONAL { ?account a banking:BankAccount . }
  OPTIONAL { ?transaction a banking:Transaction . }
  OPTIONAL { ?customer a banking:Customer . }
}
"""
```

## Real-World Examples

### Example 1: Financial Services Data Model

**Project**: Generate REST API models for a banking system

**File: `ggen.toml`**
```toml
[pipeline]
ontology_uri = "marketplace:financial/banking@1.2.1"
ontology_mixins = ["embedded:owl", "embedded:skos"]

[[pipelines.steps]]
sparql = """
SELECT ?class ?label ?description
WHERE {
  ?class a owl:Class .
  ?class rdfs:label ?label .
  OPTIONAL { ?class rdfs:comment ?description . }
  FILTER(STRSTARTS(STR(?class), STR(banking:)))
}
ORDER BY ?label
"""
template = "rest_model.tera"
output_file = "src/models/generated.rs"

[[pipelines.steps]]
sparql = """
SELECT ?prop ?domain ?range
WHERE {
  ?prop a owl:ObjectProperty .
  ?prop rdfs:domain ?domain .
  ?prop rdfs:range ?range .
}
"""
template = "rest_relationships.tera"
output_file = "src/models/relationships.rs"
```

### Example 2: Healthcare FHIR Code Generation

**Project**: Generate TypeScript interfaces for FHIR bundles

**File: `ggen.toml`**
```toml
[pipeline]
ontology_uri = "marketplace:healthcare/fhir@4.0.1"

[[pipelines.steps]]
sparql = """
PREFIX fhir: <http://hl7.org/fhir/>
SELECT ?resource ?name ?description
WHERE {
  ?resource a fhir:StructureDefinition .
  ?resource fhir:name ?name .
  OPTIONAL { ?resource fhir:description ?description . }
}
"""
template = "typescript_interfaces.tera"
output_file = "src/fhir/resources.ts"
```

### Example 3: Manufacturing Knowledge Graph

**Project**: Generate documentation from equipment ontology

**File: `ggen.toml`**
```toml
[pipeline]
ontology_uri = "marketplace:manufacturing/equipment@2.0.0"
ontology_mixins = ["embedded:skos", "embedded:prov"]

[[pipelines.steps]]
sparql = """
PREFIX eq: <http://example.com/equipment#>
SELECT ?equipment ?name ?specs ?manufacturer
WHERE {
  ?equipment a eq:EquipmentType .
  ?equipment rdfs:label ?name .
  ?equipment eq:specifications ?specs .
  ?equipment eq:manufacturer ?manufacturer .
}
ORDER BY ?name
"""
template = "equipment_markdown.tera"
output_file = "docs/equipment-catalog.md"
```

## Continuous Integration

### GitHub Actions

**File: `.github/workflows/generate-code.yml`**
```yaml
name: Code Generation

on: [push, pull_request]

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      # Cache ggen cache between runs
      - name: Cache ggen packages
        uses: actions/cache@v3
        with:
          path: ~/.ggen/packages/
          key: ggen-packages-${{ hashFiles('ggen.lock') }}
      
      # Generate code using lock file (reproducible)
      - name: Install ggen
        run: cargo install ggen-cli
      
      - name: Generate code
        run: ggen sync --locked
      
      # Fail if generated code changed (ensures code is up-to-date)
      - name: Check for changes
        run: |
          if git diff --quiet; then
            echo "✅ Generated code is up-to-date"
          else
            echo "❌ Generated code changed. Regenerate and commit."
            git diff
            exit 1
          fi
      
      # Run tests on generated code
      - name: Run tests
        run: cargo test --all
```

### GitLab CI

**File: `.gitlab-ci.yml`**
```yaml
generate_code:
  image: rust:latest
  cache:
    paths:
      - ~/.ggen/packages/
    key: $CI_COMMIT_REF_SLUG
  script:
    - cargo install ggen-cli
    - ggen sync --locked
    - git diff --exit-code || (echo "Generated code changed"; exit 1)
  only:
    - merge_requests
```

## Troubleshooting Advanced Issues

### Issue 1: "Lock File Mismatch"

```
Error: Lock file mismatch
Expected: financial/banking@1.2.1 (sha256:abc123)
Found: financial/banking@1.2.0 (sha256:def456)
```

**Causes:**
1. Lock file expects version 1.2.1 but only 1.2.0 is installed
2. Package was corrupted or re-downloaded

**Solution:**
```bash
# Option 1: Reinstall the exact version
ggen ontology install financial/banking@1.2.1 --force

# Option 2: Update lock file to current versions
ggen ontology lock --update
```

### Issue 2: "SPARQL Timeout"

```
Error: SPARQL query timeout (>30s)
Query: SELECT ?x WHERE { ... }
```

**Causes:**
1. Ontology is very large (millions of triples)
2. Query has inefficient patterns
3. System is overloaded

**Solution:**
```bash
# Check ontology size
ggen ontology info financial/banking@1.2.1
# Output: 50M triples (large!)

# Optimize SPARQL: add LIMIT
SELECT ?x WHERE { ?x a rdf:Property . } LIMIT 1000

# Or use offline mode (faster)
ggen sync --offline
```

### Issue 3: "Out of Memory"

```
Error: Out of memory
Ontology: financial/banking@1.2.1 (1.2 GB)
```

**Causes:**
1. Ontology is very large
2. System doesn't have enough RAM
3. Multiple ontologies loaded simultaneously

**Solution:**
```bash
# Use only embedded ontologies (smaller)
ontology_uri = "embedded:rdf"

# Or process in smaller batches
# Reduce SPARQL result size
SELECT ?x WHERE { ?x a rdf:Property . } LIMIT 100

# Increase system RAM or use a larger instance
```

---

## Summary

| Task | Command | Note |
|------|---------|------|
| List embedded ontologies | `ggen ontology list --embedded` | Zero network |
| Search packages | `ggen ontology search <domain>` | Requires internet |
| Install package | `ggen ontology install <package>@<version>` | Cached locally |
| Create lock file | `ggen ontology lock` | For reproducibility |
| Generate code | `ggen sync --locked` | Uses lock file |
| Benchmark | `ggen sync --audit` | Shows performance |
| Verify | `ggen ontology verify --locked` | Checksum validation |

For more help, see [TROUBLESHOOTING.md](./TROUBLESHOOTING.md)
