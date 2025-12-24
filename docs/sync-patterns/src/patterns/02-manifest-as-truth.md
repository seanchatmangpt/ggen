# 2. MANIFEST AS TRUTH **

*A map is only useful if it reflects the territory.*

---

## Context

You have embraced **[THE SINGLE COMMAND](01-single-command.md)**. Now the question arises: how does the command know what to do?

Commands with many flags can express configuration, but they become unwieldy. Environment variables scatter configuration across the system. Code-based configuration requires modifying and recompiling.

You need a way to declare intent that is:
- Readable by humans
- Parseable by machines
- Versionable with code
- Portable across environments

---

❖ ❖ ❖

**When configuration is scattered or implicit, the system becomes unpredictable. When configuration is centralized and explicit, the system becomes trustworthy.**

The forces at play:
- Configuration should live with the code it generates
- Configuration should be readable without running the system
- Configuration should be validatable before execution
- Configuration should be the single source of truth

Multiple configuration sources create:
- Precedence confusion (which overrides which?)
- Debugging difficulty (where did this value come from?)
- Environmental drift (works on my machine)
- Documentation burden (documenting what, exactly?)

**Therefore:**

**Place all synchronization configuration in a single manifest file, located at the root of the project. Make this file the exclusive source of truth for what gets generated.**

The manifest should:
- Declare the ontology sources
- Define inference rules
- Specify generation rules
- Set validation requirements
- Configure output paths

The command-line may override some behaviors (like `--dry-run`), but it never specifies *what* gets generated—only *how* the generation proceeds.

---

❖ ❖ ❖

## Connections

This pattern receives its power from **[THE SINGLE COMMAND](01-single-command.md)**, which reads it.

- **[ONTOLOGY LOADING](04-ontology-loading.md)** is configured by the `[ontology]` section
- **[INFERENCE ENRICHMENT](05-inference-enrichment.md)** is configured by the `[inference]` section
- **[GENERATION RULES](06-generation-rules.md)** is configured by the `[generation]` section
- **[VALIDATION GATE](09-validation-gate.md)** is configured by the `[validation]` section
- **[DETERMINISTIC OUTPUT](13-deterministic-output.md)** is supported by ordered data structures within the manifest

---

## Implementation

In ggen, the manifest is `ggen.toml`:

```toml
[project]
name = "my-service"
version = "1.0.0"
description = "Domain types for my service"

[ontology]
source = "ontology/domain.ttl"
imports = ["ontology/common.ttl", "ontology/validation.ttl"]
base_iri = "https://example.org/my-service#"

[ontology.prefixes]
schema = "https://schema.org/"
domain = "https://example.org/domain#"

[inference]
max_reasoning_timeout_ms = 5000

[[inference.rules]]
name = "infer-required-fields"
description = "Mark fields as required based on cardinality"
order = 1
construct = """
PREFIX domain: <https://example.org/domain#>
CONSTRUCT {
  ?field domain:isRequired true .
}
WHERE {
  ?field domain:minCardinality ?min .
  FILTER(?min > 0)
}
"""

[[generation.rules]]
name = "structs"
query = { file = "queries/structs.sparql" }
template = { file = "templates/struct.tera" }
output_file = "{{ name | snake_case }}.rs"
mode = "Overwrite"

[generation]
output_dir = "src/generated"
require_audit_trail = true
max_sparql_timeout_ms = 5000

[validation]
shacl = ["ontology/shapes.ttl"]
validate_syntax = true
no_unsafe = true
```

### The Sections

| Section | Purpose |
|---------|---------|
| `[project]` | Identity and metadata |
| `[ontology]` | Source of domain truth |
| `[inference]` | Rules that derive new facts |
| `[generation]` | Rules that produce code |
| `[validation]` | Rules that verify correctness |

### Location Convention

The manifest lives at the project root:

```
my-project/
├── ggen.toml           ← THE manifest
├── ontology/
│   └── domain.ttl
├── queries/
│   └── structs.sparql
├── templates/
│   └── struct.tera
└── src/
    └── generated/      ← output goes here
```

### Manifest Discovery

When you run `ggen sync`, the system looks for `ggen.toml` in the current directory. You can override this:

```bash
ggen sync --manifest path/to/ggen.toml
```

But the manifest—wherever it is—remains the source of truth.

### Query and Template Sources

The manifest supports two forms for queries and templates:

**File-based** (preferred for complex content):
```toml
query = { file = "queries/structs.sparql" }
template = { file = "templates/struct.tera" }
```

**Inline** (for simple cases):
```toml
query = { inline = "SELECT ?name WHERE { ?s rdfs:label ?name }" }
template = { inline = "pub struct {{ name }} {}" }
```

Both are declared in the manifest. The manifest is truth.

---

## Validation

Before the pipeline runs, the manifest is validated:

1. **Schema validation** — Required fields present, types correct
2. **Reference validation** — Files referenced actually exist
3. **Semantic validation** — Rules make sense (e.g., CONSTRUCT queries have CONSTRUCT keyword)

If validation fails, the pipeline does not run. This is **[VALIDATION GATE](09-validation-gate.md)** applied to the manifest itself.

---

## The Deeper Pattern

MANIFEST AS TRUTH is about **declarative configuration**.

Imperative configuration says: "Do this, then this, then this."
Declarative configuration says: "This is what I want. Make it so."

The manifest declares intent. The system figures out execution. This separation is what makes the system trustworthy:

- You can read the manifest and know what *should* happen
- You can diff manifests and know what *changed*
- You can validate manifests and know what *will* happen

The manifest is not just configuration. It is a **contract** between you and the system.

---

## When This Pattern Breaks

MANIFEST AS TRUTH struggles when:

- Configuration needs to be dynamic (computed at runtime)
- Different environments need radically different configurations
- The manifest becomes so large it's hard to navigate

ggen addresses these partially:

- Template variables can compute some values
- Multiple manifest files can exist (pointed to by `--manifest`)
- Section organization keeps the manifest navigable

But fundamentally, if your configuration needs to be *computed*, a static manifest may not suffice. In such cases, consider generating the manifest as part of a higher-level process—making the manifest itself a generated artifact from a yet-higher truth.

The pattern remains: there is always a manifest. The manifest is always truth. What varies is what generates the manifest.
