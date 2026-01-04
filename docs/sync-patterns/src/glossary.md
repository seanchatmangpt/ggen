# Glossary

> *"To share a language is to share a world."*

---

## Core Terms

### ggen sync
The single command in ggen v5 that performs knowledge-driven code generation. It synchronizes domain knowledge (ontologies) with code (generated files).

### Manifest
The `ggen.toml` file that declares all configuration for a generation run. It is the **[MANIFEST AS TRUTH](patterns/02-manifest-as-truth.md)**.

### Ontology
An RDF graph expressed in Turtle (.ttl) format that describes domain knowledge: entities, properties, relationships, and constraints.

### Pipeline
The sequence of stages that transforms ontology → inference → generation → files. See **[PIPELINE STATE](patterns/15-pipeline-state.md)**.

### Pattern
A recurring solution to a recurring problem, expressed as a relationship between context, forces, and resolution.

---

## Knowledge Terms

### Triple
The fundamental unit of RDF: subject-predicate-object. For example: `domain:User rdf:type rdfs:Class`.

### Graph
A collection of triples that can be queried. In ggen, the ontology forms a graph that grows as inference adds derived facts.

### SPARQL
The query language for RDF graphs. ggen uses CONSTRUCT for inference and SELECT for generation.

### CONSTRUCT Query
A SPARQL query that produces new triples. Used in **[INFERENCE ENRICHMENT](patterns/05-inference-enrichment.md)** to derive facts.

### SELECT Query
A SPARQL query that produces rows of variable bindings. Used in **[GENERATION RULES](patterns/06-generation-rules.md)** to extract data for templates.

### Materialization
The process of adding derived triples (from CONSTRUCT queries) back into the graph, making them queryable by subsequent rules.

### Tera
The template engine used by ggen. It uses Jinja2-style syntax (`{{ variable }}`, `{% if %}`, `{% for %}`).

---

## Execution Terms

### Inference Rule
A named CONSTRUCT query that derives new facts from existing ones. Configured in `[inference.rules]`.

### Generation Rule
A named combination of SELECT query + template + output pattern that produces code files. Configured in `[generation.rules]`.

### Rule Order
The explicit ordering of inference rules via the `order` field. Lower numbers execute first.

### Query Source
Where a query comes from: `{ file = "..." }` for file-based or `{ inline = "..." }` for embedded.

### Template Source
Where a template comes from: `{ file = "..." }` for file-based or `{ inline = "..." }` for embedded.

### Output Pattern
A template string for the generated file path, e.g., `{{ name | snake_case }}.rs`.

### Generation Mode
How file writing behaves: `Create` (only if absent), `Overwrite` (always), or `Merge` (preserve sections).

---

## Safety Terms

### Dry Run
Executing the pipeline without writing files. Invoked via `--dry-run`. See **[DRY RUN](patterns/08-dry-run.md)**.

### Validation Gate
Checking configuration validity without executing generation. Invoked via `--validate-only`. See **[VALIDATION GATE](patterns/09-validation-gate.md)**.

### Force
Overriding file protections to write regardless of mode. Invoked via `--force`. See **[FORCE OVERWRITE](patterns/10-force-overwrite.md)**.

### Timeout
The maximum allowed execution time. Configurable per-stage and globally. See **[TIMEOUT PROTECTION](patterns/11-timeout-protection.md)**.

### Exit Code
The numeric status returned by ggen sync. 0 = success, 1-6 = specific failure categories. See **[ERROR SIGNALS](patterns/12-error-signals.md)**.

---

## Integrity Terms

### Determinism
The property of producing identical outputs from identical inputs. See **[DETERMINISTIC OUTPUT](patterns/13-deterministic-output.md)**.

### Content Hash
A SHA256 hash of a file's contents, used to verify identity and detect changes.

### Query Hash
A SHA256 hash of a query's text, used to verify the query hasn't changed.

### Audit Trail
A JSON file (`audit.json`) recording all execution details. See **[AUDIT TRAIL](patterns/14-audit-trail.md)**.

### Provenance
The record of where something came from and how it was produced.

---

## Architecture Terms

### CLI Layer
Layer 3 of the architecture. Handles argument parsing and output formatting. Thin and stateless.

### Integration Layer
Layer 2 of the architecture. Handles orchestration and async execution. Contains `SyncExecutor`.

### Domain Layer
Layer 1 of the architecture. Contains pure generation logic. Contains `GenerationPipeline`.

### SyncOptions
The configuration struct passed from CLI to Integration layer. Contains all flags and settings.

### SyncResult
The result struct passed from Integration layer back to CLI. Contains execution summary.

### PipelineState
The internal state accumulated during pipeline execution. See **[PIPELINE STATE](patterns/15-pipeline-state.md)**.

---

## File Terms

### ggen.toml
The manifest file. See **[MANIFEST AS TRUTH](patterns/02-manifest-as-truth.md)**.

### *.ttl
Turtle files containing RDF ontologies.

### *.sparql
SPARQL query files (optional; queries can also be inline).

### *.tera
Tera template files (optional; templates can also be inline).

### audit.json
The audit trail file. See **[AUDIT TRAIL](patterns/14-audit-trail.md)**.

### src/generated/
Default output directory for generated files.

---

## Pattern Terms

### Confidence Stars
Indicators of pattern establishment:
- `*` — Emerging pattern, still evolving
- `**` — Established pattern, proven in practice
- `***` — Fundamental pattern, essential to the language

### Forces
The tensions or pressures that create a problem within a context.

### Therefore
The solution that resolves the forces. Always stated as an imperative.

### Connections
Links to related patterns that support or extend a pattern.

---

## Workflow Terms

### Full Sync
Running `ggen sync` without restrictions. All rules execute.

### Selective Sync
Running `ggen sync --rule X`. Only named rules execute. See **[RULE SELECTION](patterns/16-rule-selection.md)**.

### Preview
Using `--dry-run` to see what would happen without doing it.

### Validate
Using `--validate-only` to check configuration without generating.

### Regenerate
Running `ggen sync --force` to overwrite all protected files.

---

## Abbreviations

| Abbreviation | Meaning |
|--------------|---------|
| RDF | Resource Description Framework |
| TTL | Turtle (Terse RDF Triple Language) |
| SPARQL | SPARQL Protocol and RDF Query Language |
| IRI | Internationalized Resource Identifier |
| CLI | Command-Line Interface |
| CI/CD | Continuous Integration / Continuous Deployment |
| SLO | Service Level Objective |
| BTreeMap | Balanced Tree Map (ordered data structure) |

---

## See Also

- **[How to Use This Pattern Language](how-to-use.md)** — Guidance on reading and applying patterns
- **[Pattern Map](pattern-map.md)** — Visual relationships between patterns
- **[Introduction](README.md)** — The philosophy behind the pattern language
