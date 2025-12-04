# ggen v4.0.0 CLI Reference

Complete reference for all 47 verbs across 9 CLI modules.

**Version**: 4.0.0
**Framework**: clap-noun-verb v5.3.0
**Last Updated**: 2025-12-03

---

## Table of Contents

1. [Global Flags](#global-flags)
2. [AI Module](#ai-module) (6 verbs)
3. [Template Module](#template-module) (7 verbs)
4. [Graph Module](#graph-module) (5 verbs)
5. [Ontology Module](#ontology-module) (4 verbs)
6. [Project Module](#project-module) (4 verbs)
7. [Paper Module](#paper-module) (3 verbs)
8. [CI Module](#ci-module) (4 verbs)
9. [Workflow Module](#workflow-module) (2 verbs)
10. [Utils Module](#utils-module-fmea) (5 verbs)

---

## Global Flags

### `ggen --version` / `-V`
Show ggen version information.

```bash
$ ggen --version
ggen 4.0.0
```

### `ggen --graph`
Export complete command graph for AI agent workflow planning.

**Output Format**: JSON with all nouns, verbs, and metadata.

```bash
$ ggen --graph
{
  "version": "5.3.0",
  "total_verbs": 47,
  "nouns": {
    "ai": { "verbs": [...] },
    "template": { "verbs": [...] },
    ...
  }
}
```

### `ggen --capabilities <noun> <verb>`
Show metadata for a specific verb (arguments, return type, JSON support).

```bash
$ ggen --capabilities template generate
{
  "noun": "template",
  "verb": "generate",
  "description": "Generate code from template with variable substitution",
  "arguments": [
    { "name": "template", "type": "Option<String>", "optional": true },
    { "name": "output", "type": "Option<String>", "optional": true },
    { "name": "force", "type": "bool", "optional": true }
  ],
  "return_type": "GenerateOutput",
  "supports_json_output": true
}
```

### `ggen --introspect <noun> <verb>`
Show detailed type information for a verb (human-readable format).

```bash
$ ggen --introspect template generate
Verb: template::generate
Description: Generate code from template with variable substitution
Return Type: GenerateOutput
JSON Output: true

Arguments:
  - template (Option<String>): Path to template file
    Optional: yes
  - output (Option<String>): Output file path (default: generated.rs)
    Default: generated.rs
    Optional: yes
  - force (bool): Overwrite existing output file
    Default: false
    Optional: yes
```

---

## AI Module

Code generation from natural language using ggen-ai domain functions.

### `ggen ai generate-ontology`
Generate RDF ontology from natural language prompt.

```bash
ggen ai generate-ontology --prompt "create user and order models" [--output ontology.ttl]
```

**Arguments**:
- `--prompt <PROMPT>` (required): Natural language description of ontology
- `--output <PATH>` (optional): Output file path (default: `ontology.ttl`)

**Returns**: GenerateOutput with ontology_path and triple_count

**Example**:
```bash
ggen ai generate-ontology \
  --prompt "E-commerce: Product, Order, Review with relationships" \
  --output domain.ttl
```

### `ggen ai analyze-model`
Analyze existing code/models and extract structure.

```bash
ggen ai analyze-model --input model.rs [--language rust] [--output analysis.json]
```

**Arguments**:
- `--input <PATH>` (required): Source file to analyze
- `--language <LANG>` (optional): Language hint (default: auto-detect)
- `--output <PATH>` (optional): Output analysis file

### `ggen ai generate-schema`
Generate JSON Schema from RDF ontology.

```bash
ggen ai generate-schema --ontology domain.ttl [--output schema.json]
```

### `ggen ai validate-model`
Validate model against semantic rules.

```bash
ggen ai validate-model --ontology domain.ttl [--strict]
```

### `ggen ai export-types`
Export type definitions for a model.

```bash
ggen ai export-types --ontology domain.ttl [--language rust|typescript|python]
```

### `ggen ai infer-relationships`
Infer implicit relationships in ontology.

```bash
ggen ai infer-relationships --ontology domain.ttl [--output inferred.ttl]
```

---

## Template Module

Code generation using templates and variable substitution.

### `ggen template generate`
Generate code from template with variable substitution.

```bash
ggen template generate [--template path] [--output output.rs] [--force]
```

**Arguments**:
- `--template <PATH>` (optional): Path to template file
- `--output <PATH>` (optional): Output file path (default: `generated.rs`)
- `--force` (optional): Overwrite existing output file

**Example**:
```bash
ggen template generate --template src/template.tmpl --output src/generated.rs --force
```

### `ggen template validate`
Validate template syntax and variables.

```bash
ggen template validate --template template.tmpl
```

### `ggen template list`
List all available templates.

```bash
ggen template list [--category category]
```

### `ggen template preview`
Preview template output without writing.

```bash
ggen template preview --template template.tmpl [--variables vars.json]
```

### `ggen template export`
Export template in different format.

```bash
ggen template export --template template.tmpl --format liquid|jinja2|tera
```

### `ggen template import`
Import template from external source.

```bash
ggen template import --source url [--name template-name]
```

### `ggen template generate-rdf`
Generate from ontology using RDF-based templates.

```bash
ggen template generate-rdf --ontology domain.ttl --template rust-models
```

---

## Graph Module

RDF graph management and SPARQL querying.

### `ggen graph load`
Load RDF graph from file or URL.

```bash
ggen graph load --source file.ttl [--format turtle|jsonld|rdfxml]
```

**Arguments**:
- `--source <PATH|URL>` (required): File path or URL to load
- `--format <FORMAT>` (optional): RDF format (default: `turtle`)

**Example**:
```bash
ggen graph load --source https://example.com/ontology.ttl --format turtle
```

### `ggen graph query`
Execute SPARQL query on loaded graph.

```bash
ggen graph query --graph ontology.ttl --query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" [--format json]
```

**Arguments**:
- `--graph <PATH>` (required): Graph file to query
- `--query <SPARQL>` (required): SPARQL query string
- `--format <FORMAT>` (optional): Output format (default: `turtle`)

### `ggen graph export`
Export graph in different RDF format.

```bash
ggen graph export --source ontology.ttl --format jsonld [--output output.jsonld]
```

### `ggen graph validate`
Validate RDF graph structure.

```bash
ggen graph validate --source ontology.ttl [--strict]
```

### `ggen graph merge`
Merge multiple RDF graphs.

```bash
ggen graph merge --graphs graph1.ttl graph2.ttl [--output merged.ttl]
```

---

## Ontology Module

Ontology management and extraction.

### `ggen ontology extract`
Extract ontology from code/documentation.

```bash
ggen ontology extract --source code.rs [--language rust] [--output ontology.ttl]
```

### `ggen ontology generate`
Generate ontology from specification.

```bash
ggen ontology generate --spec spec.yaml [--output ontology.ttl]
```

### `ggen ontology validate`
Validate ontology structure and semantics.

```bash
ggen ontology validate --ontology domain.ttl [--strict]
```

### `ggen ontology init`
Initialize new ontology project.

```bash
ggen ontology init --name myapp [--template starter|advanced|minimal]
```

---

## Project Module

Project scaffolding and management.

### `ggen project new`
Create new project from template.

```bash
ggen project new --name myproject --template rust|typescript|python [--output dir]
```

**Arguments**:
- `--name <NAME>` (required): Project name
- `--template <TEMPLATE>` (required): Project template
- `--output <PATH>` (optional): Output directory

### `ggen project generate`
Generate project structure from ontology.

```bash
ggen project generate --ontology domain.ttl --template rust-backend
```

### `ggen project plan`
Plan code generation strategy.

```bash
ggen project plan --ontology domain.ttl [--output plan.json]
```

### `ggen project validate`
Validate project structure.

```bash
ggen project validate [--output report.json]
```

---

## Paper Module

Academic paper and documentation generation.

### `ggen paper export`
Export documentation as academic paper.

```bash
ggen paper export --source docs/ [--format pdf|html|markdown] [--output paper.pdf]
```

### `ggen paper compile`
Compile paper from LaTeX/Markdown sources.

```bash
ggen paper compile --source paper.md [--output paper.pdf]
```

### `ggen paper init-bibliography`
Initialize bibliography from ontology.

```bash
ggen paper init-bibliography --ontology domain.ttl [--format bibtex|csl]
```

---

## CI Module

CI/CD workflow generation and management.

### `ggen ci workflow`
Generate GitHub Actions workflow configuration.

```bash
ggen ci workflow [--name workflow-name] [--output .github/workflows/build.yml]
```

**Arguments**:
- `--name <NAME>` (optional): Workflow name (default: `build`)
- `--output <PATH>` (optional): Output workflow file

### `ggen ci action`
Generate reusable CI action.

```bash
ggen ci action --name action-name [--language rust|typescript]
```

### `ggen ci pipeline`
Generate complete CI/CD pipeline.

```bash
ggen ci pipeline --template standard|advanced [--output .github/workflows/]
```

### `ggen ci validate`
Validate CI configuration.

```bash
ggen ci validate --workflow .github/workflows/build.yml
```

---

## Workflow Module

Workflow orchestration and automation.

### `ggen workflow generate`
Generate workflow specification.

```bash
ggen workflow generate --ontology domain.ttl [--output workflow.yaml]
```

### `ggen workflow execute`
Execute workflow definition.

```bash
ggen workflow execute --workflow workflow.yaml [--dry-run]
```

---

## Utils Module (FMEA)

Failure Mode and Effects Analysis (FMEA) reporting and system health.

### `ggen utils fmea report`
Generate FMEA report with failure mode analysis.

```bash
ggen utils fmea report [--format text|json] [--risk CRITICAL|HIGH|MEDIUM|LOW] [--top 20]
```

**Arguments**:
- `--format <FORMAT>` (optional): Output format (default: `text`)
- `--risk <LEVEL>` (optional): Filter by risk level
- `--top <N>` (optional): Show top N failure modes (default: 20)

**Example**:
```bash
ggen utils fmea report --format json --risk HIGH | jq .
```

### `ggen utils fmea pareto`
Generate Pareto analysis (80/20 rule) for failure modes.

```bash
ggen utils fmea pareto
```

**Output**: Visual bar chart showing RPN distribution with 80% threshold line.

### `ggen utils fmea list`
List failure modes with filters.

```bash
ggen utils fmea list [--category category] [--sort rpn|severity|id]
```

### `ggen utils fmea show`
Show specific failure mode details.

```bash
ggen utils fmea show --mode-id FM_001 [--events]
```

### `ggen utils fmea export`
Export FMEA data to JSON.

```bash
ggen utils fmea export [--output fmea-report.json]
```

### `ggen utils doctor`
System health check.

```bash
ggen utils doctor [--detailed]
```

### `ggen utils env`
Show environment information.

```bash
ggen utils env [--json]
```

---

## Common Options

### Output Format
Most commands support JSON output for machine parsing:

```bash
ggen template generate --template hello.tmpl --output-format json
```

### Verbose Output
Enable detailed logging:

```bash
ggen --verbose template generate --template hello.tmpl
```

### Config File
Specify custom configuration:

```bash
ggen --config custom.toml template generate --template hello.tmpl
```

---

## Exit Codes

- `0`: Success
- `1`: Generic error
- `2`: Argument parsing error
- `3`: File not found
- `4`: Permission denied
- `5`: Invalid format
- `6`: Network error
- `127`: Command not found

---

## Introspection Examples

### Discover all available verbs
```bash
ggen --graph | jq '.nouns | keys'
# Output: ["ai", "ci", "graph", "ontology", "paper", "project", "template", "workflow"]
```

### Get all verbs for a noun
```bash
ggen --graph | jq '.nouns.template.verbs[].name'
# Output: generate, validate, list, preview, export, import, generate-rdf
```

### Find verbs that support JSON output
```bash
ggen --graph | jq '[.nouns | to_entries[].value.verbs[] | select(.supports_json_output) | .name]'
```

### Check required arguments for a verb
```bash
ggen --capabilities ai generate-ontology | jq '.arguments[] | select(.optional == false)'
```

---

## Performance Tips

1. **Use `--format json` for parsing**: Machine-readable output is faster to parse
2. **Cache graph files**: Use `ggen graph load` once and query multiple times
3. **Batch operations**: Process multiple items in single command when possible
4. **Validate early**: Run `validate` commands to catch errors before generation

---

## See Also

- [Installation Guide](../how-to-guides/installation.md)
- [Getting Started Tutorial](../tutorials/getting-started.md)
- [Architecture Explanation](../explanations/architecture.md)
- [Migration Guide from v3.4.0](../how-to-guides/migrate-v3-to-v4.md)

---

**Generated for ggen v4.0.0**
