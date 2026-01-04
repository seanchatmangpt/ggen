# ggen Configuration Reference (Complete & Compressed)

> Complete ggen.toml schema on 4 pages. Copy-paste ready.

## Minimal Config

```toml
[project]
name = "my-project"

[generation]
ontology_dir = "ontology/"
templates_dir = "templates/"
output_dir = "src/generated/"
```

---

## Complete Schema

### `[project]`

| Key | Type | Default | Example |
|-----|------|---------|---------|
| name | string | (required) | "my-api" |
| version | string | "0.1.0" | "1.0.0" |
| description | string | "" | "REST API from RDF" |
| authors | [string] | [] | ["Alice <a@example.com>"] |
| repository | string | "" | "https://github.com/user/repo" |

### `[generation]`

| Key | Type | Default | Purpose |
|-----|------|---------|---------|
| ontology_dir | string | (required) | Load .ttl files from here |
| templates_dir | string | (required) | Load .tera templates from here |
| output_dir | string | (required) | Write generated files here |
| protected_paths | [glob] | [] | Never overwrite (poka-yoke) |
| regenerate_paths | [glob] | [] | Safe to regenerate |
| cache_dir | string | none | Enable caching (optional) |
| timeout_ms | u64 | 30000 | Max execution time |
| dry_run | bool | false | Don't write files |

### `[generation.poka_yoke]`

| Key | Type | Default | Effect |
|-----|------|---------|--------|
| warning_headers | bool | true | Add "DO NOT EDIT" comments |
| gitignore_generated | bool | true | Add to .gitignore |
| gitattributes_generated | bool | true | Mark as linguist-generated |

### `[[inference.rules]]` (Array)

```toml
[[inference.rules]]
name = "inheritance_closure"
description = "Materialize class hierarchies"
query = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
CONSTRUCT { ?sub :allSuperclasses ?super . }
WHERE { ?sub rdfs:subClassOf+ ?super . }
"""

# All rules listed in order
rule_order = ["inheritance_closure", "property_expansion", ...]
```

### `[inference]`

| Key | Type | Default |
|-----|------|---------|
| enabled | bool | true |
| rule_order | [string] | execution order |

### `[[generation.rules]]` (Per-Template Rules)

```toml
[[generation.rules]]
name = "rust_structs"
description = "Generate Rust struct definitions"
ontology = "ontology/domain.ttl"
query = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?class ?label ?comment
WHERE { ?class a rdfs:Class ; rdfs:label ?label .
  OPTIONAL { ?class rdfs:comment ?comment . } }
"""
template = "rust-struct.tera"
output_file = "models.rs"
```

| Key | Type | Required |
|-----|------|----------|
| name | string | yes |
| description | string | no |
| ontology | string | yes (or inline) |
| query | string | yes (or inline) |
| template | string | yes (or inline) |
| output_file | string | yes |

### `[marketplace]`

| Key | Type | Default | Purpose |
|-----|------|---------|---------|
| enabled | bool | false | Enable package marketplace |
| registry | string | "official" | registry.ggen.io or custom |
| fmea_validation | bool | false | Validate packages |
| require_fmea | bool | false | Require [fmea] section |
| critical_threshold | u32 | 300 | RPN threshold for acceptance |

### `[codeowners]`

| Key | Type | Default |
|-----|------|---------|
| enabled | bool | false |
| source_dirs | [string] | ["ontology"] |
| base_dirs | [string] | ["src/generated", "src/domain"] |
| output_path | string | ".github/CODEOWNERS" |

### `[lifecycle]`

```toml
[lifecycle.phases.pre_generation]
run = "cargo fmt --check"
on_error = "fail"  # fail|warn|ignore

[lifecycle.phases.post_generation]
run = "cargo test"
on_error = "fail"
```

| Phase | When | Variables |
|-------|------|-----------|
| pre_generation | Before sync | $ONTOLOGY, $TEMPLATES |
| post_generation | After files written | $OUTPUT_DIR, $FILES_GENERATED |
| validation | Before commit | $TEST_RESULTS |

### `[security]`

| Key | Type | Purpose |
|-----|------|---------|
| sign_packages | bool | GPG sign marketplace packages |
| verify_signatures | bool | Verify before installing |
| allowed_domains | [string] | Trusted registries |

### `[performance]`

| Key | Type | Default |
|-----|------|---------|
| parallel_templates | bool | true |
| max_workers | u32 | num_cpus |
| query_timeout_ms | u64 | 10000 |
| result_cache_size | u32 | 10000 |

---

## Minimal Template Config

```toml
[[generation.rules]]
name = "example"
template = "my.tera"
output_file = "output.rs"

[inference]
enabled = true
[[inference.rules]]
name = "base_rule"
query = """SELECT ?x WHERE { ?x a rdfs:Class . }"""
```

---

## Environment Variable Expansion

```toml
ontology_dir = "${ONTOLOGY_PATH}"     # Load from $ONTOLOGY_PATH
output_dir = "target/${BUILD_TYPE}"   # target/debug or target/release
cache_dir = "${XDG_CACHE_HOME}/ggen"  # Linux cache dir

# Fallback syntax
output_dir = "${OUTPUT_DIR:-src/generated/}"
```

---

## CLI Overrides

```bash
ggen sync \
  --config custom.toml \                    # Use different file
  --ontology-override "ontology/v2/" \      # Override dirs
  --template-override "templates/v2/" \
  --output-override "out/" \
  --rule inheritance_closure \              # Run specific rule only
  --timeout 60000 \                         # 60 second timeout
  --dry-run \                               # Don't write files
  --validate-only \                         # Just validate, no generation
  --format json \                           # Machine-readable output
  --audit audit.json \                      # Write audit trail
  --watch \                                 # Live-reload mode
  --verbose                                 # Debug output
```

---

## Real-World Examples

### REST API

```toml
[project]
name = "api"

[generation]
ontology_dir = "ontology/"
templates_dir = "templates/"
output_dir = "src/generated/"
protected_paths = ["src/domain/**/*"]
regenerate_paths = ["src/generated/**/*"]

[generation.poka_yoke]
warning_headers = true
gitignore_generated = true

[[inference.rules]]
name = "hierarchy"
query = "SELECT ?sub :allSuperclasses ?super WHERE { ?sub rdfs:subClassOf+ ?super }"

[[generation.rules]]
name = "rust_api"
template = "rust-actix.tera"
output_file = "api.rs"

[lifecycle.phases.post_generation]
run = "cargo test"
```

### Multi-Crate Workspace

```toml
[project]
name = "workspace-generator"

[generation]
ontology_dir = "ontology/"
templates_dir = "templates/"
output_dir = "generated/"

[[generation.rules]]
name = "core"
template = "core.tera"
output_file = "crates/core/src/generated.rs"

[[generation.rules]]
name = "api"
template = "api.tera"
output_file = "crates/api/src/generated.rs"

[[generation.rules]]
name = "db"
template = "db.tera"
output_file = "crates/db/src/generated.rs"
```

### Marketplace Package

```toml
[project]
name = "jwt-auth-template"
version = "1.0.0"

[marketplace]
enabled = true
registry = "official"

[marketplace.fmea]
severity = 3
occurrence = 2
detection = 2
rpn = 12  # 3*2*2
mitigations = ["Unit tests for auth logic", "Integration tests with mock JWT"]
```

---

## Decision Matrix: When to Use What

| Need | Config |
|------|--------|
| Prevent overwrites | `protected_paths = ["src/domain/**/*"]` |
| Materialize ontology | `[[inference.rules]]` with CONSTRUCT |
| Generate code | `[[generation.rules]]` with template |
| Run tests after | `[lifecycle.phases.post_generation]` |
| Multi-project sync | Multiple `[[generation.rules]]` |
| Marketplace packages | `[marketplace]` enabled |
| CI/CD automation | `--validate-only --format json` |
| Development loop | `ggen sync --watch` |

---

## Schema Validation (SHACL)

ggen validates `ggen.toml` against internal SHACL schema:
- All required keys present
- Types match (string, bool, array, etc.)
- Paths exist (ontology_dir, templates_dir)
- SPARQL queries valid syntax
- Template files exist

Error: Run with `--verbose` to see validation details.

