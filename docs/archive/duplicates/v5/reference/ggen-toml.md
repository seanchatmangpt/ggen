<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen.toml Reference](#ggentoml-reference)
  - [Complete Example](#complete-example)
  - [Section: `[project]`](#section-project)
    - [`name` (required)](#name-required)
    - [`version` (required)](#version-required)
    - [`description` (optional)](#description-optional)
    - [`author` (optional)](#author-optional)
    - [`license` (optional)](#license-optional)
  - [Section: `[rdf]`](#section-rdf)
    - [`ontology_file` (required)](#ontology_file-required)
    - [`schema_file` (optional)](#schema_file-optional)
    - [`base_uri` (required)](#base_uri-required)
    - [`prefixes` (optional)](#prefixes-optional)
  - [Section: `[templates]`](#section-templates)
    - [`source_dir` (required)](#source_dir-required)
    - [`output_dir` (required)](#output_dir-required)
    - [`backup_enabled` (optional, default: `false`)](#backup_enabled-optional-default-false)
  - [Section: `[sparql]`](#section-sparql)
    - [`timeout` (optional, default: `30`)](#timeout-optional-default-30)
    - [`max_results` (optional, default: `10000`)](#max_results-optional-default-10000)
    - [`cache_enabled` (optional, default: `true`)](#cache_enabled-optional-default-true)
  - [Section: `[logging]`](#section-logging)
    - [`level` (optional, default: `"info"`)](#level-optional-default-info)
    - [`format` (optional, default: `"text"`)](#format-optional-default-text)
    - [`output` (optional, default: `"stdout"`)](#output-optional-default-stdout)
  - [Section: `[security]`](#section-security)
    - [`validate_paths` (optional, default: `true`)](#validate_paths-optional-default-true)
    - [`path_traversal_protection` (optional, default: `true`)](#path_traversal_protection-optional-default-true)
  - [Real-World Examples](#real-world-examples)
    - [Example 1: Thesis Generation](#example-1-thesis-generation)
    - [Example 2: ASTRO State Machine](#example-2-astro-state-machine)
    - [Example 3: Minimal Configuration](#example-3-minimal-configuration)
  - [Configuration Validation](#configuration-validation)
    - [Common Errors](#common-errors)
      - [Error: Missing required field](#error-missing-required-field)
      - [Error: Invalid ontology file path](#error-invalid-ontology-file-path)
      - [Error: Invalid base URI](#error-invalid-base-uri)
  - [Best Practices](#best-practices)
    - [1. Separate Schema and Content](#1-separate-schema-and-content)
    - [2. Use Meaningful Project Names](#2-use-meaningful-project-names)
    - [3. Enable Backups During Development](#3-enable-backups-during-development)
    - [4. Set Appropriate Timeouts](#4-set-appropriate-timeouts)
    - [5. Use Prefixes for Readability](#5-use-prefixes-for-readability)
  - [Environment Variables](#environment-variables)
  - [Version History](#version-history)
  - [Related Documentation](#related-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen.toml Reference

The `ggen.toml` file is the project manifest for ggen code generation. It defines:
- Project metadata
- RDF ontology sources
- Template configuration
- SPARQL settings
- Security and validation rules

## Complete Example

```toml
[project]
name = "my-project"
version = "1.0.0"
description = "My code generation project"
author = "Your Name"
license = "MIT"

[rdf]
ontology_file = "ontology/content.ttl"
schema_file = "ontology/schema.ttl"
base_uri = "http://example.com/myapp#"
prefixes = { myapp = "http://example.com/myapp#", rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#" }

[templates]
source_dir = "templates"
output_dir = "output"
backup_enabled = true

[sparql]
timeout = 30
max_results = 10000
cache_enabled = true

[logging]
level = "info"
format = "text"
output = "stdout"

[security]
validate_paths = true
path_traversal_protection = true
```

---

## Section: `[project]`

Project-level metadata.

### `name` (required)

Project name (string).

```toml
name = "thesis-generator"
```

**Constraints:**
- Must be valid filesystem name
- Lowercase, hyphens allowed
- No spaces or special characters

### `version` (required)

Project version following semantic versioning.

```toml
version = "1.0.0"
```

**Format**: `MAJOR.MINOR.PATCH`

### `description` (optional)

Human-readable project description.

```toml
description = "Generate PhD thesis from RDF ontology"
```

### `author` (optional)

Author name(s).

```toml
author = "Alice Smith"
```

### `license` (optional)

License identifier (SPDX format).

```toml
license = "MIT"
# Or: "Apache-2.0", "GPL-3.0", "BSD-3-Clause", etc.
```

---

## Section: `[rdf]`

RDF ontology configuration.

### `ontology_file` (required)

Path to main RDF ontology file (content).

```toml
ontology_file = "ontology/my-content.ttl"
```

**Supported formats:**
- `.ttl` - Turtle (recommended)
- `.n3` - Notation3
- `.rdf` - RDF/XML
- `.jsonld` - JSON-LD
- `.trig` - TriG (named graphs)

**Path resolution:**
- Relative to `ggen.toml` location
- Can use absolute paths

### `schema_file` (optional)

Path to RDF schema/vocabulary file.

```toml
schema_file = "ontology/schema.ttl"
```

**Why separate?**
- **Schema**: Vocabulary definitions (classes, properties)
- **Content**: Actual data instances

**Example:**
```turtle
# schema.ttl
:Book a rdfs:Class .
:title a rdf:Property .

# content.ttl
:MyBook a :Book ;
  :title "Introduction to RDF" .
```

### `base_uri` (required)

Base IRI for relative URIs.

```toml
base_uri = "http://example.com/myapp#"
```

**Effect:**
```turtle
# In your .ttl file, if base_uri is set:
:User a rdfs:Class .
# Expands to:
<http://example.com/myapp#User> a rdfs:Class .
```

### `prefixes` (optional)

Custom namespace prefixes (inline table).

```toml
prefixes = {
  myapp = "http://example.com/myapp#",
  rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  rdfs = "http://www.w3.org/2000/01/rdf-schema#"
}
```

**Enables shorter SPARQL queries:**
```sparql
PREFIX myapp: <http://example.com/myapp#>
SELECT ?x WHERE { ?x a myapp:User }
```

**Common prefixes:**
- `rdf`: `http://www.w3.org/1999/02/22-rdf-syntax-ns#`
- `rdfs`: `http://www.w3.org/2000/01/rdf-schema#`
- `xsd`: `http://www.w3.org/2001/XMLSchema#`
- `owl`: `http://www.w3.org/2002/07/owl#`

---

## Section: `[templates]`

Template engine configuration.

### `source_dir` (required)

Directory containing Tera templates.

```toml
source_dir = "templates"
```

**Structure example:**
```
templates/
├── main.tera
├── chapter.tera
├── theorem.tera
└── utils/
    └── helpers.tera
```

**File extension**: `.tera` (Tera template files)

### `output_dir` (required)

Directory for generated output files.

```toml
output_dir = "output"
```

**Behavior:**
- Created if doesn't exist
- Existing files overwritten (unless `backup_enabled = true`)
- Relative to `ggen.toml` location

### `backup_enabled` (optional, default: `false`)

Enable backups before overwriting files.

```toml
backup_enabled = true
```

**Effect:**
- Before overwriting `output/file.txt`, creates `output/file.txt.backup`
- Prevents accidental data loss during development

---

## Section: `[sparql]`

SPARQL query engine configuration.

### `timeout` (optional, default: `30`)

Query timeout in seconds.

```toml
timeout = 30
```

**Why?**
- Prevents infinite loops in complex queries
- Useful for large ontologies (1000+ triples)

**Example:**
```toml
timeout = 5   # Fast queries only
timeout = 120 # Allow complex reasoning
```

### `max_results` (optional, default: `10000`)

Maximum number of results per query.

```toml
max_results = 10000
```

**Prevents:**
- Memory exhaustion from unbounded queries
- Accidentally generating 1M+ code files

### `cache_enabled` (optional, default: `true`)

Enable SPARQL query result caching.

```toml
cache_enabled = true
```

**Effect:**
- Repeated identical queries return cached results
- Speeds up incremental generation
- Disable for debugging or frequently changing ontologies

---

## Section: `[logging]`

Logging configuration.

### `level` (optional, default: `"info"`)

Logging level.

```toml
level = "debug"
```

**Options:**
- `"error"` - Errors only
- `"warn"` - Errors + warnings
- `"info"` - Errors + warnings + info (recommended)
- `"debug"` - All messages (verbose)
- `"trace"` - Maximum verbosity (very noisy)

### `format` (optional, default: `"text"`)

Log output format.

```toml
format = "json"
```

**Options:**
- `"text"` - Human-readable (default)
- `"json"` - Machine-parseable (for log aggregation)

**Example outputs:**
```
# text format
2025-12-18 10:30:00 INFO Loaded ontology: 142 triples

# json format
{"timestamp":"2025-12-18T10:30:00Z","level":"INFO","message":"Loaded ontology: 142 triples"}
```

### `output` (optional, default: `"stdout"`)

Where to write logs.

```toml
output = "logs/ggen.log"
```

**Options:**
- `"stdout"` - Standard output (console)
- `"stderr"` - Standard error
- `"/path/to/file.log"` - Write to file

---

## Section: `[security]`

Security and validation settings.

### `validate_paths` (optional, default: `true`)

Validate all file paths before access.

```toml
validate_paths = true
```

**Prevents:**
- Invalid paths (non-existent directories)
- Symlink attacks
- Permission issues

**Disable for:** Debugging only

### `path_traversal_protection` (optional, default: `true`)

Block path traversal attempts (`../../../etc/passwd`).

```toml
path_traversal_protection = true
```

**Effect:**
- Reject paths containing `..`
- Enforce staying within project directory
- Essential for autonomous agent usage

**NEVER disable in production.**

---

## Real-World Examples

### Example 1: Thesis Generation

```toml
[project]
name = "phd-thesis"
version = "1.0.0"
description = "PhD Thesis Generator"

[rdf]
ontology_file = "ontology/content.ttl"
schema_file = "ontology/thesis-schema.ttl"
base_uri = "https://ggen.io/thesis/"
prefixes = { thesis = "https://ggen.io/thesis/", rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#" }

[templates]
source_dir = "templates"
output_dir = "output"
backup_enabled = true

[sparql]
timeout = 30
max_results = 10000

[logging]
level = "info"
```

**Source**: `specs/012-grand-unified-kgc-thesis/ggen.toml`

### Example 2: ASTRO State Machine

```toml
[project]
name = "astro-state-machine"
version = "2.1.0"
description = "Distributed order processing state machine"

[rdf]
ontology_file = "ontology/order-processing.ttl"
schema_file = "ontology/state-machine-schema.ttl"
base_uri = "https://example.com/astro#"

[templates]
source_dir = "templates"
output_dir = "src/generated"

[sparql]
timeout = 60  # Complex state graph queries
max_results = 50000  # Large state machine (47 states, 128 transitions)

[logging]
level = "debug"  # Verbose during development
```

### Example 3: Minimal Configuration

```toml
[project]
name = "simple-gen"
version = "0.1.0"

[rdf]
ontology_file = "data.ttl"
base_uri = "http://example.com/#"

[templates]
source_dir = "templates"
output_dir = "out"
```

**Minimalist approach**: Only required fields, all defaults.

---

## Configuration Validation

### Common Errors

#### Error: Missing required field

```toml
[project]
# Missing 'name' field
version = "1.0.0"
```

**Fix:**
```toml
[project]
name = "my-project"  # Add required field
version = "1.0.0"
```

#### Error: Invalid ontology file path

```toml
[rdf]
ontology_file = "does-not-exist.ttl"
```

**Fix:**
```bash
# Verify file exists
ls ontology/content.ttl

# Update path
[rdf]
ontology_file = "ontology/content.ttl"
```

#### Error: Invalid base URI

```toml
[rdf]
base_uri = "not a valid uri"  # Missing scheme
```

**Fix:**
```toml
[rdf]
base_uri = "http://example.com/myapp#"  # Valid HTTP IRI
```

---

## Best Practices

### 1. Separate Schema and Content

```toml
[rdf]
schema_file = "ontology/schema.ttl"    # Vocabulary (rarely changes)
ontology_file = "ontology/content.ttl"  # Data (changes frequently)
```

**Why?**
- Reuse schema across multiple projects
- Easier version control
- Clear separation of concerns

### 2. Use Meaningful Project Names

```toml
# ❌ Bad
name = "proj1"

# ✅ Good
name = "user-service-codegen"
```

### 3. Enable Backups During Development

```toml
[templates]
backup_enabled = true  # Prevent accidental overwrites
```

**Disable in CI/CD** (not needed for fresh checkouts).

### 4. Set Appropriate Timeouts

```toml
[sparql]
timeout = 5   # Small ontologies (<100 triples)
timeout = 30  # Medium (100-1000 triples)
timeout = 120 # Large (1000+ triples) or complex reasoning
```

### 5. Use Prefixes for Readability

```toml
[rdf]
prefixes = {
  app = "http://example.com/myapp#",
  rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  rdfs = "http://www.w3.org/2000/01/rdf-schema#",
  xsd = "http://www.w3.org/2001/XMLSchema#"
}
```

**Effect in SPARQL:**
```sparql
# Without prefixes (ugly)
SELECT ?x WHERE {
  ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
     <http://example.com/myapp#User>
}

# With prefixes (clean)
SELECT ?x WHERE { ?x a app:User }
```

---

## Environment Variables

ggen supports environment variable expansion:

```toml
[rdf]
ontology_file = "${ONTOLOGY_PATH}/content.ttl"

[templates]
output_dir = "${OUTPUT_DIR}"
```

**Usage:**
```bash
export ONTOLOGY_PATH=/data/ontologies
export OUTPUT_DIR=/tmp/generated
ggen sync
```

**Default values:**
```toml
output_dir = "${OUTPUT_DIR:-output}"  # Default to "output" if not set
```

---

## Version History

| Version | Changes |
|---------|---------|
| 5.0.0 | Initial `ggen.toml` format |
| 5.1.0 | Added `[security]` section |
| 5.2.0 | Added environment variable support |

---

## Related Documentation

- **[CLI Commands](cli.md)** - How ggen uses this config
- **[RDF Ontology Structure](rdf-ontology-structure.md)** - Ontology file format
- **[Templates](tera-templates.md)** - Template syntax reference
- **[Getting Started](../getting-started/README.md)** - Create your first `ggen.toml`

---

**Format**: TOML (Tom's Obvious, Minimal Language)
**Location**: Project root directory
**Required**: Yes (ggen won't run without it)
**Validation**: Run `ggen validate` to check configuration
