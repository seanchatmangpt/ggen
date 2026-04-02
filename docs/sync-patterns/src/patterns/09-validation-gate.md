<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [9. VALIDATION GATE **](#9-validation-gate-)
  - [Context](#context)
  - [Connections](#connections)
  - [Implementation](#implementation)
    - [Invocation](#invocation)
    - [Behavior](#behavior)
    - [Validation Layers](#validation-layers)
    - [Output](#output)
    - [Error Output](#error-output)
  - [Validation Checks Summary](#validation-checks-summary)
  - [Integration with CI/CD](#integration-with-cicd)
  - [The Deeper Pattern](#the-deeper-pattern)
  - [Relationship to DRY RUN](#relationship-to-dry-run)
  - [When This Pattern Breaks](#when-this-pattern-breaks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 9. VALIDATION GATE **

*Check the map before starting the journey.*

---

## Context

You have written a `ggen.toml` manifest per **[MANIFEST AS TRUTH](02-manifest-as-truth.md)**. The manifest references ontology files, query files, and template files. It declares inference rules and generation rules. Any of these could have errors:

- A typo in a file path
- Invalid SPARQL syntax
- A missing template
- A malformed TOML structure

You want to know if the configuration is valid *before* running the full pipeline. You don't want to wait for inference and generation just to discover a typo in line 1.

---

❖ ❖ ❖

**Errors discovered late are expensive. Errors discovered early are cheap.**

The forces:
- Configuration errors should fail fast
- The pipeline should not waste time on doomed executions
- Error messages should point to the source of the problem
- Validation should be quick (much faster than full generation)

Without a validation gate:
- Errors surface mid-pipeline (after work is done)
- Error messages may be obscure (deep in template rendering)
- Developers iterate slowly (run full pipeline to find each error)

**Therefore:**

**Provide a validation-only mode that checks all configuration without executing generation. Validate file existence, syntax, and semantic correctness. Fail fast with clear error messages pointing to the problem source.**

The validation should check:
- Manifest syntax and required fields
- Referenced files exist
- Ontology files parse correctly
- SPARQL queries parse correctly
- Template files parse correctly
- Semantic rules (e.g., CONSTRUCT queries have CONSTRUCT keyword)

---

❖ ❖ ❖

## Connections

This pattern modifies **[THE SINGLE COMMAND](01-single-command.md)** via a flag.

- **[DRY RUN](08-dry-run.md)** goes further—it generates but doesn't write
- **[ERROR SIGNALS](12-error-signals.md)** reports validation failures
- **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** is what gets validated

---

## Implementation

### Invocation

```bash
ggen sync --validate-only
```

### Behavior

Validation stops before execution:

```
Normal execution:         Validate-only:
──────────────────       ──────────────────
Load manifest    ✓       Load manifest    ✓
Validate refs    ✓       Validate refs    ✓
Load ontology    ✓       Check ontology   ✓ (parse, don't store)
Run inference    ✓       Check queries    ✓ (parse, don't execute)
Run generation   ✓       Check templates  ✓ (parse, don't render)
Write files      ✓       Stop             ✓
Return result    ✓       Return result    ✓
```

The key difference: **parse but don't execute**.

### Validation Layers

**Layer 1: Manifest Structure**

```rust
// Check TOML syntax
let manifest_content = std::fs::read_to_string("ggen.toml")?;
let manifest: GgenManifest = toml::from_str(&manifest_content)?;

// Check required fields
if manifest.project.name.is_empty() {
    return Err(Error::new("project.name is required"));
}
```

**Layer 2: File References**

```rust
// Check ontology source exists
let source_path = base_path.join(&manifest.ontology.source);
if !source_path.exists() {
    return Err(Error::new(&format!(
        "Ontology source not found: {}",
        source_path.display()
    )));
}

// Check imports exist
for import in &manifest.ontology.imports {
    let import_path = base_path.join(import);
    if !import_path.exists() {
        return Err(Error::new(&format!(
            "Ontology import not found: {}",
            import_path.display()
        )));
    }
}
```

**Layer 3: Syntax Validation**

```rust
// Parse ontology (don't store)
let content = std::fs::read_to_string(&source_path)?;
Graph::parse_turtle(&content)?;  // Parse-only mode

// Parse SPARQL queries
for rule in &manifest.inference.rules {
    sparql::parse_construct(&rule.construct)?;
}

for rule in &manifest.generation.rules {
    let query = load_query_source(&rule.query)?;
    sparql::parse_select(&query)?;
}

// Parse templates
for rule in &manifest.generation.rules {
    let template = load_template_source(&rule.template)?;
    tera::Tera::one_off(&template, &tera::Context::new(), false)?;
}
```

**Layer 4: Semantic Validation**

```rust
// Check CONSTRUCT queries use CONSTRUCT
for rule in &manifest.inference.rules {
    if !rule.construct.trim().to_uppercase().starts_with("CONSTRUCT") {
        return Err(Error::new(&format!(
            "Inference rule '{}' must use CONSTRUCT query",
            rule.name
        )));
    }
}

// Warn about non-deterministic queries
for rule in &manifest.inference.rules {
    if !rule.construct.to_uppercase().contains("ORDER BY") {
        warn!("Inference rule '{}' lacks ORDER BY - may be non-deterministic", rule.name);
    }
}
```

### Output

```
$ ggen sync --validate-only

Validating ggen.toml...

  ✓ Manifest syntax valid
  ✓ Project metadata complete
  ✓ Ontology source: ontology/domain.ttl
  ✓ Ontology import: ontology/common.ttl
  ✓ Inference rule: infer-required-fields
  ✓ Inference rule: infer-nullable-fields
  ✓ Generation rule: structs
  ✓ Generation rule: enums
  ✓ Generation rule: mod-file

Validation passed (9 checks in 23ms)
```

### Error Output

```
$ ggen sync --validate-only

Validating ggen.toml...

  ✓ Manifest syntax valid
  ✓ Project metadata complete
  ✗ Ontology source not found: ontology/domian.ttl
                                        ^^^^^^
    Did you mean: ontology/domain.ttl?

error[E0001]: Validation failed
  1 error, 0 warnings
```

---

## Validation Checks Summary

| Check | Layer | Failure Mode |
|-------|-------|--------------|
| TOML syntax | 1 | Parse error with line number |
| Required fields | 1 | Missing field error |
| File existence | 2 | File not found with suggestion |
| Turtle syntax | 3 | Parse error with position |
| SPARQL syntax | 3 | Parse error with position |
| Tera syntax | 3 | Parse error with position |
| Query type | 4 | Wrong query type (SELECT vs CONSTRUCT) |
| Determinism | 4 | Warning for missing ORDER BY |

---

## Integration with CI/CD

Validation is fast—suitable for every commit:

```yaml
- name: Validate ggen configuration
  run: ggen sync --validate-only
```

This catches configuration errors before expensive operations (tests, builds).

---

## The Deeper Pattern

VALIDATION GATE is about **fail-fast**.

The later an error is discovered, the more work has been wasted. Validation inverts this: check everything that can be checked statically, before any dynamic work begins.

This is a form of **shift-left**: moving error detection earlier in the pipeline.

```
                  Error Discovery Cost
                         ↑
                         │
                   ██████│██████████████████ Late (during generation)
              ████│██████│
         ████│████│      │
    ████│████│    │      │
████│████│    │      │      │
    │    │    │      │      │
────┴────┴────┴──────┴──────┴─────→ Pipeline Stage
 Validate  Load  Infer  Generate  Write
```

VALIDATION GATE keeps errors in the leftmost, cheapest zone.

---

## Relationship to DRY RUN

| Mode | What runs | Cost |
|------|-----------|------|
| `--validate-only` | Parsing only | Low (~10ms) |
| `--dry-run` | Full generation | Medium (~100ms) |
| Normal | Generation + writing | High (~100ms + I/O) |

Use `--validate-only` for quick syntax checks.
Use `--dry-run` for previewing what will be generated.
Use normal mode when ready to generate.

---

## When This Pattern Breaks

VALIDATION GATE struggles when:

- Validation is expensive (complex semantic checks)
- Some errors only manifest at runtime (dynamic template logic)
- The boundary between validation and execution is unclear

ggen manages this by:

- Keeping validation checks lightweight
- Accepting that some errors surface later (template variables)
- Clearly separating parsing from execution

The pattern remains: check what you can check early, and check it fast.
