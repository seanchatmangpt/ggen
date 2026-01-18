# 6. GENERATION RULES **

*From knowledge, code. From queries, templates. From rules, files.*

---

## Context

You have a graph loaded via **[ONTOLOGY LOADING](04-ontology-loading.md)** and enriched via **[INFERENCE ENRICHMENT](05-inference-enrichment.md)**. The graph contains facts about your domain: entities, fields, relationships, constraints.

Now you must transform this knowledge into code. But the transformation is not simple: different parts of the graph become different kinds of code. Entities become structs. Fields become properties. Relationships become references.

You need a way to specify: "This part of the graph becomes this kind of code."

---

❖ ❖ ❖

**When transformation logic is scattered in code, it is hard to understand, test, and maintain. When transformation is declared as rules, it becomes visible, verifiable, and changeable.**

The forces:
- Each type of generated code requires different graph queries
- Each query result must be shaped into code via templates
- Multiple rules produce multiple files
- Rules must be independent yet composable
- The relationship between graph and code must be traceable

Embedding transformation in code leads to:
- Black-box generation (what produces what?)
- Tight coupling (changing one thing breaks others)
- Untestable transformations (can't run queries in isolation)

**Therefore:**

**Express each code generation as a rule with three parts: a SPARQL SELECT query to extract data, a Tera template to shape the output, and an output pattern to name the file. Each rule is independent; together they produce the complete codebase.**

The rule should specify:
- A name (for identification and filtering)
- A query (SELECT) that returns rows of variables
- A template that uses those variables
- An output file pattern (which may include variables)
- A generation mode (create, overwrite, or merge)

---

❖ ❖ ❖

## Connections

This pattern queries the graph produced by **[ONTOLOGY LOADING](04-ontology-loading.md)** and enriched by **[INFERENCE ENRICHMENT](05-inference-enrichment.md)**.

- **[TEMPLATE RENDERING](07-template-rendering.md)** explains how templates work
- **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** configures the rules
- **[RULE SELECTION](16-rule-selection.md)** allows running specific rules
- **[DETERMINISTIC OUTPUT](13-deterministic-output.md)** requires consistent query ordering
- **[AUDIT TRAIL](14-audit-trail.md)** records rule execution

---

## Implementation

### Manifest Configuration

The `[generation]` section defines rules:

```toml
[generation]
output_dir = "src/generated"
require_audit_trail = true
max_sparql_timeout_ms = 5000

[[generation.rules]]
name = "structs"
query = { file = "queries/structs.sparql" }
template = { file = "templates/struct.tera" }
output_file = "{{ name | snake_case }}.rs"
mode = "Overwrite"
skip_empty = true

[[generation.rules]]
name = "enums"
query = { inline = """
    PREFIX domain: <https://example.org/domain#>
    SELECT ?name ?values
    WHERE {
        ?enum a domain:Enum ;
              rdfs:label ?name ;
              domain:hasValues ?values .
    }
    ORDER BY ?name
""" }
template = { file = "templates/enum.tera" }
output_file = "{{ name | snake_case }}.rs"
mode = "Create"

[[generation.rules]]
name = "mod-file"
query = { file = "queries/all-modules.sparql" }
template = { inline = "{% for m in sparql_results %}pub mod {{ m.name }};\n{% endfor %}" }
output_file = "mod.rs"
mode = "Overwrite"
```

### Rule Anatomy

| Field | Purpose |
|-------|---------|
| `name` | Unique identifier for the rule |
| `query` | SPARQL SELECT query (file or inline) |
| `template` | Tera template (file or inline) |
| `output_file` | Output path pattern with variables |
| `mode` | `Create`, `Overwrite`, or `Merge` |
| `skip_empty` | If true, skip when query returns no results |

### Execution Flow

```
┌──────────────────────────────────────────────────────────────────┐
│ For each generation rule:                                         │
│                                                                    │
│   1. Load query from QuerySource (file or inline)                 │
│              ↓                                                     │
│   2. Execute SELECT against enriched graph                        │
│              ↓                                                     │
│   3. Get result rows: [{var1: val1, var2: val2}, ...]            │
│              ↓                                                     │
│   4. For each row:                                                │
│      a. Build template context from row variables                 │
│      b. Render template → generated code                          │
│      c. Expand output_file pattern → file path                    │
│      d. Apply mode (create/overwrite/merge)                       │
│      e. Write file                                                │
│                                                                    │
└──────────────────────────────────────────────────────────────────┘
```

### Query Results to Template Context

SPARQL query results become template variables:

```sparql
SELECT ?name ?type ?is_required
WHERE {
    ?field rdfs:label ?name ;
           domain:type ?type .
    OPTIONAL { ?field domain:isRequired ?is_required }
}
```

Each result row becomes a template context:

```json
{
  "name": "user_id",
  "type": "String",
  "is_required": "true"
}
```

The template accesses these as variables:

```jinja2
pub struct {{ name | pascal_case }} {
    {% if is_required == "true" %}
    pub {{ name }}: {{ type }},
    {% else %}
    pub {{ name }}: Option<{{ type }}>,
    {% endif %}
}
```

### Output File Patterns

The `output_file` is itself a template:

```toml
output_file = "{{ name | snake_case }}.rs"
```

For a query result where `name = "UserProfile"`:
- Template expansion: `user_profile.rs`
- Full path: `src/generated/user_profile.rs`

### Generation Modes

| Mode | Behavior |
|------|----------|
| `Create` | Only write if file doesn't exist |
| `Overwrite` | Always write, replacing existing |
| `Merge` | Preserve marked sections (future) |

`Create` is safe—it never overwrites human edits. `Overwrite` is deterministic—same input always produces same output.

---

## Multiple Files Per Rule

A single rule can generate multiple files if the query returns multiple rows:

```sparql
SELECT ?entity_name ...
WHERE {
    ?entity a domain:Entity ;
            rdfs:label ?entity_name .
}
```

If this returns 5 entities, the rule generates 5 files (one per entity).

---

## The All-Results Variable

For rules that need to see all results at once (like `mod.rs`), the full query results are available as `sparql_results`:

```jinja2
// mod.rs - auto-generated
{% for entity in sparql_results %}
pub mod {{ entity.name | snake_case }};
{% endfor %}
```

This enables:
- Aggregation templates
- Index files
- Summary documents

---

## The Deeper Pattern

GENERATION RULES embody **the separation of concerns**:

- **What to extract** → SPARQL query
- **How to shape it** → Tera template
- **Where to put it** → Output pattern

Each concern is expressed in its own language, optimized for that concern:
- SPARQL excels at graph traversal
- Tera excels at text generation
- File patterns excel at naming

This separation makes each part:
- Independently testable (run the query alone)
- Independently changeable (modify template without touching query)
- Independently understandable (read what each part does)

---

## Ordering and Determinism

For **[DETERMINISTIC OUTPUT](13-deterministic-output.md)**, queries should include `ORDER BY`:

```sparql
SELECT ?name ?type
WHERE { ... }
ORDER BY ?name
```

Without ordering, the same graph may produce results in different orders, leading to different file contents (even if semantically equivalent).

The manifest uses ordered arrays for rule declarations, but the order of file generation within a rule depends on query results.

---

## When This Pattern Breaks

GENERATION RULES struggles when:

- Generated code needs complex logic (templates become unwieldy)
- Multiple queries must be joined (SPARQL can join, but it's complex)
- Output depends on previous outputs (rules are independent)

ggen addresses these partially:

- Tera supports macros and includes for template organization
- SPARQL supports complex joins and subqueries
- Inference rules can pre-compute complex derivations

For very complex generation, consider:
- Breaking into smaller, simpler rules
- Moving complexity to inference (derive first, generate simply)
- Post-processing generated code

The pattern remains: each rule declares a transformation from graph to code, independently and completely.
