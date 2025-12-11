# Complete CLI Command Reference

**ggen** - RDF-based code generation toolkit

**Version**: 3.4.1+

---

## Table of Contents

- [Template Commands](#template-commands)
- [Graph Commands](#graph-commands)
- [Ontology Commands](#ontology-commands)
- [Project Commands](#project-commands)
- [AI Commands](#ai-commands)
- [Utility Commands](#utility-commands)
- [Global Options](#global-options)

---

## Template Commands

Manage and generate from templates.

### `ggen template list`

List all available templates.

**Usage**:
```bash
ggen template list
```

**Output**:
```json
{
  "templates": [
    {
      "name": "hello.tmpl",
      "description": "Basic hello world template",
      "version": "1.0.0"
    },
    ...
  ]
}
```

**Examples**:
```bash
# List all 22 built-in templates
ggen template list

# Parse output with jq
ggen template list | jq '.templates[].name'
```

---

### `ggen template show`

Show template metadata and variables.

**Alias**: `ggen template get`

**Usage**:
```bash
ggen template show --template <TEMPLATE_NAME>
```

**Options**:
- `--template <NAME>` - Template file name or path (required)

**Examples**:
```bash
# Show hello template details
ggen template show --template hello.tmpl

# Show custom template
ggen template show --template ~/my-templates/custom.tmpl
```

**Output**:
```yaml
name: hello.tmpl
description: Basic hello world template
version: 1.0.0
variables:
  - name: target_name
    description: Name of the target
    required: true
    default: "World"
```

---

### `ggen template new`

Create a new template file.

**Usage**:
```bash
ggen template new --name <NAME> [OPTIONS]
```

**Options**:
- `--name <NAME>` - Template name (required)
- `--output <PATH>` - Output directory (default: `./templates/`)
- `--description <TEXT>` - Template description
- `--version <VERSION>` - Template version (default: `1.0.0`)

**Examples**:
```bash
# Create basic template
ggen template new --name my-template

# Create with description and custom output
ggen template new \
  --name javascript-model \
  --description "JavaScript model generator" \
  --output ./custom-templates/

# Create versioned template
ggen template new \
  --name api-v2 \
  --version 2.0.0 \
  --description "REST API generator v2"
```

---

### `ggen template lint`

Validate template syntax and structure.

**Usage**:
```bash
ggen template lint --template <TEMPLATE_FILE>
```

**Options**:
- `--template <FILE>` - Template file to validate (required)

**Examples**:
```bash
# Lint template
ggen template lint --template my-template.tmpl

# Lint all templates in directory
for f in templates/*.tmpl; do
  echo "Linting $f..."
  ggen template lint --template "$f"
done
```

**Checks**:
- Valid YAML frontmatter
- Required fields present (name, description, version)
- Template syntax validity
- Variable definitions

---

### `ggen template generate`

Generate from template (basic version).

**Usage**:
```bash
ggen template generate --template <TEMPLATE> [OPTIONS]
```

**Options**:
- `--template <FILE>` - Template file (required)
- `--vars <KEY=VALUE>` - Template variables (repeatable)
- `--output <PATH>` - Output file path
- `--dry-run` - Preview without writing files

**Examples**:
```bash
# Generate with variables
ggen template generate \
  --template rust-struct.tmpl \
  --vars name=User \
  --vars fields=id,name,email \
  --output src/models/user.rs

# Dry run
ggen template generate \
  --template api.tmpl \
  --vars service=auth \
  --dry-run
```

---

### `ggen template generate-tree`

Generate file tree from template.

**Usage**:
```bash
ggen template generate-tree --template <TEMPLATE> [OPTIONS]
```

**Options**:
- `--template <FILE>` - Template file (required)
- `--output <DIR>` - Output directory (default: `.`)

**Examples**:
```bash
# Generate project structure
ggen template generate-tree --template project-scaffold.tmpl

# Generate to specific directory
ggen template generate-tree \
  --template fullstack-app.tmpl \
  --output ./my-app
```

---

## Graph Commands

Work with RDF graphs for querying and visualization.

### `ggen graph load`

Load RDF data into the graph store.

**Usage**:
```bash
ggen graph load --file <RDF_FILE>
```

**Options**:
- `--file <PATH>` - RDF file to load (required)
- `--format <FORMAT>` - RDF format (auto-detected from extension)
  - Supported: `turtle`, `rdf-xml`, `n-triples`, `n-quads`

**Examples**:
```bash
# Load Turtle file
ggen graph load --file product-ontology.ttl

# Load RDF/XML
ggen graph load --file schema.rdf

# Load N-Triples
ggen graph load --file data.nt
```

**Output**:
```
✓ Loaded 1,234 triples from product-ontology.ttl
```

---

### `ggen graph query`

Query graph with SPARQL.

**Usage**:
```bash
ggen graph query --sparql_query <QUERY>
```

**Options**:
- `--sparql_query <QUERY>` - SPARQL query string (required)
- `--output <FILE>` - Save results to file
- `--format <FORMAT>` - Output format (`json`, `csv`, `tsv`)

**Examples**:
```bash
# Basic SELECT query
ggen graph query --sparql_query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

# Query with prefixes
ggen graph query --sparql_query '
PREFIX schema: <https://schema.org/>
SELECT ?name ?price
WHERE {
  ?product a schema:Product ;
           schema:name ?name ;
           schema:price ?price .
}
'

# Save results to file
ggen graph query \
  --sparql_query "SELECT * WHERE { ?s ?p ?o }" \
  --output results.json
```

**Output** (JSON):
```json
[
  {
    "s": "http://example.org/product001",
    "p": "http://schema.org/name",
    "o": "Laptop"
  },
  ...
]
```

**See Also**: [Query RDF with SPARQL Guide](../../how-to/generation/query-rdf-sparql.md)

---

### `ggen graph export`

Export graph to file.

**Usage**:
```bash
ggen graph export --output <FILE> [OPTIONS]
```

**Options**:
- `--output <FILE>` - Output file path (required)
- `--format <FORMAT>` - RDF format (`turtle`, `rdf-xml`, `n-triples`, `json-ld`)

**Examples**:
```bash
# Export to Turtle
ggen graph export --output graph.ttl --format turtle

# Export to JSON-LD
ggen graph export --output graph.jsonld --format json-ld

# Export to N-Triples
ggen graph export --output graph.nt --format n-triples
```

---

### `ggen graph visualize`

Visualize graph structure.

**Usage**:
```bash
ggen graph visualize --output <FILE> [OPTIONS]
```

**Options**:
- `--output <FILE>` - Output image file (required)
- `--format <FORMAT>` - Image format (`svg`, `png`, `pdf`)
- `--max-nodes <N>` - Limit nodes displayed (default: 100)

**Examples**:
```bash
# Generate SVG visualization
ggen graph visualize --output graph.svg

# Generate PNG with node limit
ggen graph visualize \
  --output graph.png \
  --format png \
  --max-nodes 50
```

---

## Ontology Commands

Extract and work with ontology schemas.

### `ggen ontology extract`

Extract ontology schema from RDF/OWL file.

**Usage**:
```bash
ggen ontology extract --ontology_file <FILE> [OPTIONS]
```

**Options**:
- `--ontology_file <FILE>` - RDF/OWL ontology file (required)
- `--output <FILE>` - Output JSON file (default: `schema.json`)
- `--namespace <URI>` - Filter by namespace

**Examples**:
```bash
# Extract schema to default output
ggen ontology extract --ontology_file schema.ttl

# Extract with custom output
ggen ontology extract \
  --ontology_file ecommerce.rdf \
  --output ecommerce-schema.json

# Extract specific namespace
ggen ontology extract \
  --ontology_file schema.ttl \
  --namespace "http://example.org#"
```

**Output** (`schema.json`):
```json
{
  "classes": [
    {
      "name": "Product",
      "label": "Product",
      "comment": "A product in the catalog",
      "properties": [
        {
          "name": "name",
          "range": "string",
          "required": true
        },
        ...
      ]
    }
  ]
}
```

**See Also**: [Quick Start Tutorial](../../getting-started/quick-start.md)

---

### `ggen ontology init`

Initialize ontology project with examples.

**Usage**:
```bash
ggen ontology init [NAME] [OPTIONS]
```

**Options**:
- `[NAME]` - Project name (default: current directory name)
- `--template <TEMPLATE>` - Base template (`schema.org`, `foaf`, `custom`)
- `--output <DIR>` - Output directory (default: `.`)

**Examples**:
```bash
# Initialize in current directory
ggen ontology init

# Initialize new project
ggen ontology init my-ontology

# Initialize with Schema.org template
ggen ontology init ecommerce-api --template schema.org

# Initialize with FOAF template
ggen ontology init social-network --template foaf
```

**Creates**:
```
my-ontology/
├── ontology/
│   ├── schema.ttl          # Main ontology
│   └── examples.ttl        # Example instances
├── templates/
│   └── default.tmpl        # Generation template
├── config.yaml             # Project configuration
└── README.md
```

---

### `ggen ontology generate`

Generate code from ontology schema.

**Usage**:
```bash
ggen ontology generate <SCHEMA_FILE> [OPTIONS]
```

**Options**:
- `<SCHEMA_FILE>` - Extracted schema JSON (required)
- `--language <LANG>` - Target language (`typescript`, `python`, `rust`, `javascript`)
- `--output <DIR>` - Output directory (required)
- `--zod` - Generate Zod schemas (JavaScript/TypeScript)
- `--utilities` - Generate utility functions

**Examples**:
```bash
# Generate JavaScript with Zod
ggen ontology generate schema.json \
  --language javascript \
  --zod \
  --output src/models/

# Generate TypeScript interfaces
ggen ontology generate schema.json \
  --language typescript \
  --output src/types/

# Generate with utilities
ggen ontology generate schema.json \
  --language typescript \
  --zod \
  --utilities \
  --output src/generated/
```

**See Also**: [Generate JavaScript + Zod Guide](../../how-to/generation/generate-javascript-zod.md)

---

### `ggen ontology validate`

Validate ontology schema quality.

**Usage**:
```bash
ggen ontology validate <SCHEMA_FILE> [OPTIONS]
```

**Options**:
- `<SCHEMA_FILE>` - Schema JSON to validate (required)
- `--strict` - Enable strict validation rules

**Examples**:
```bash
# Basic validation
ggen ontology validate schema.json

# Strict validation
ggen ontology validate schema.json --strict
```

**Checks**:
- Missing property domains/ranges
- Undefined class references
- Cardinality constraints
- Namespace consistency
- Required field presence

---

## Project Commands

Scaffold and manage code generation projects.

### `ggen project init`

Initialize project with conventions.

**Usage**:
```bash
ggen project init [OPTIONS]
```

**Options**:
- `--name <NAME>` - Project name (default: current directory)
- `--preset <PRESET>` - Convention preset (`clap-noun-verb`, `custom`)
- `--path <DIR>` - Project directory (default: `.`)

**Examples**:
```bash
# Initialize in current directory
ggen project init

# Initialize with name and preset
ggen project init \
  --name my-project \
  --preset clap-noun-verb

# Initialize in specific directory
ggen project init \
  --path ./workspace \
  --name my-workspace
```

**Creates**:
```
my-project/
├── .ggen/
│   └── config.yaml         # Project configuration
├── templates/              # Custom templates
├── schemas/                # Ontology schemas
└── generated/              # Generated code (gitignored)
```

---

### `ggen project new`

Create new project from scratch.

**Usage**:
```bash
ggen project new <NAME> [OPTIONS]
```

**Options**:
- `<NAME>` - Project name (required)
- `--type <TYPE>` - Project type (`rust-cli`, `rust-web`, `nextjs`, `python`)
- `--framework <FRAMEWORK>` - Framework choice (varies by type)
- `--output <DIR>` - Output directory (default: `./<NAME>`)

**Examples**:
```bash
# Create Rust CLI project
ggen project new my-cli --type rust-cli

# Create Next.js app
ggen project new my-app \
  --type nextjs \
  --framework app-router

# Create with custom output
ggen project new my-service \
  --type rust-web \
  --output ./workspace
```

---

### `ggen project gen`

Generate code from template with RDF integration.

**Usage**:
```bash
ggen project gen --template_ref <TEMPLATE> [OPTIONS]
```

**Options**:
- `--template_ref <REF>` - Template reference (required)
  - Format: `file:path/to/template.tmpl` or `pack:template-name`
- `--vars <KEY=VALUE>` - Template variables (repeatable)
- `--output <FILE>` - Output file path
- `--dry-run` - Preview without writing

**Examples**:
```bash
# Generate from file template
ggen project gen \
  --template_ref file:templates/model.tmpl \
  --vars name=User \
  --output src/models/user.js

# Generate from pack template
ggen project gen \
  --template_ref pack:javascript-zod \
  --vars schema_file=schema.json

# Dry run with multiple variables
ggen project gen \
  --template_ref file:service.tmpl \
  --vars service=auth \
  --vars port=8080 \
  --vars db=postgres \
  --dry-run
```

---

### `ggen project watch`

Watch for changes and auto-regenerate.

**Usage**:
```bash
ggen project watch [OPTIONS]
```

**Options**:
- `--path <DIR>` - Directory to watch (default: `.`)
- `--debounce <MS>` - Debounce delay in milliseconds (default: `300`)

**Examples**:
```bash
# Watch current directory
ggen project watch

# Watch specific directory
ggen project watch --path ./my-project

# Custom debounce
ggen project watch \
  --path ./src \
  --debounce 500
```

---

### `ggen project plan`

Generate project plan from template.

**Usage**:
```bash
ggen project plan --template <TEMPLATE> [OPTIONS]
```

**Options**:
- `--template <FILE>` - Template file (required)
- `--output <FILE>` - Plan output file (default: `plan.json`)
- `--format <FORMAT>` - Output format (`json`, `yaml`, `toml`)
- `--var <KEY=VALUE>` - Plan variables (repeatable)

**Examples**:
```bash
# Create JSON plan
ggen project plan \
  --template service.tmpl \
  --output plan.json

# Create YAML plan with variables
ggen project plan \
  --template app.tmpl \
  --var name=myapp \
  --var author=me \
  --format yaml \
  --output plan.yaml
```

---

### `ggen project apply`

Apply generation plan.

**Usage**:
```bash
ggen project apply <PLAN_FILE> [OPTIONS]
```

**Options**:
- `<PLAN_FILE>` - Plan file (JSON/YAML/TOML) (required)
- `--yes` - Auto-confirm without prompting
- `--dry-run` - Preview changes

**Examples**:
```bash
# Apply with confirmation
ggen project apply plan.json

# Auto-confirm
ggen project apply plan.yaml --yes

# Dry run
ggen project apply plan.toml --dry-run
```

---

### `ggen project generate`

Generate using zero-config conventions.

**Usage**:
```bash
ggen project generate [TEMPLATE] [OPTIONS]
```

**Options**:
- `[TEMPLATE]` - Specific template name (optional)
- `--path <DIR>` - Project path (default: `.`)
- `--output <DIR>` - Output directory (default: `./generated`)
- `--force` - Overwrite existing files

**Examples**:
```bash
# Generate all templates
ggen project generate

# Generate specific template
ggen project generate my-template

# Force overwrite
ggen project generate --force

# Custom paths
ggen project generate \
  --path ./my-project \
  --output ./src/generated \
  my-template
```

---

## AI Commands

AI-assisted code generation and analysis.

### `ggen ai chat`

Interactive AI chat session.

**Usage**:
```bash
ggen ai chat [PROMPT] [OPTIONS]
```

**Options**:
- `[PROMPT]` - Initial prompt (optional)
- `--interactive` - Enter interactive mode
- `--model <MODEL>` - AI model to use

**Examples**:
```bash
# Single question
ggen ai chat "Explain RDF triples"

# Interactive session
ggen ai chat --interactive

# With specific model
ggen ai chat "Generate Rust struct" --model gpt-4
```

**Note**: Requires AI provider configuration (API key in environment).

---

### `ggen ai generate`

Generate code with AI assistance.

**Usage**:
```bash
ggen ai generate <PROMPT> [OPTIONS]
```

**Options**:
- `<PROMPT>` - Generation prompt (required)
- `--model <MODEL>` - AI model (`gpt-4`, `claude-3-sonnet`)
- `--output <FILE>` - Save output to file

**Examples**:
```bash
# Generate code
ggen ai generate "Create a REST API server in Rust"

# With specific model
ggen ai generate \
  "Generate Zod schema for e-commerce product" \
  --model claude-3-sonnet

# Save to file
ggen ai generate \
  "Generate TypeScript types" \
  --output src/types/generated.ts
```

---

### `ggen ai analyze`

Analyze code and provide insights.

**Usage**:
```bash
ggen ai analyze [CODE] [OPTIONS]
```

**Options**:
- `[CODE]` - Code snippet to analyze (optional if using `--file`)
- `--file <PATH>` - Analyze file instead of snippet
- `--model <MODEL>` - AI model to use

**Examples**:
```bash
# Analyze code snippet
ggen ai analyze 'fn main() { println!("hello"); }'

# Analyze from file
ggen ai analyze --file src/main.rs

# With specific model
ggen ai analyze --file app.js --model claude-3-sonnet
```

---

## Utility Commands

Additional utilities and helpers.

### `ggen utils env`

Manage environment variables.

**Usage**:
```bash
ggen utils env [OPTIONS]
```

**Options**:
- `--list` - List all variables
- `--get <KEY>` - Get variable value
- `--set <KEY=VALUE>` - Set variable value

**Examples**:
```bash
# List all
ggen utils env --list

# Get value
ggen utils env --get API_KEY

# Set value
ggen utils env --set API_KEY=sk-...
```

---

### `ggen workflow report`

Generate workflow reports.

**Usage**:
```bash
ggen workflow report --workflow-file <FILE> [OPTIONS]
```

**Options**:
- `--workflow-file <FILE>` - Workflow JSON file (required)
- `--format <FORMAT>` - Output format (`html`, `markdown`, `json`)
- `--output <FILE>` - Output file

**Examples**:
```bash
# Generate HTML report
ggen workflow report \
  --workflow-file workflow.json \
  --format html \
  --output report.html

# Generate Markdown report
ggen workflow report \
  --workflow-file workflow.json \
  --format markdown
```

---

## Global Options

Available on all commands:

**`-h, --help`** - Print help information

**`-V, --version`** - Print version information

**Examples**:
```bash
# Get version
ggen --version

# Get help
ggen --help

# Get command-specific help
ggen template --help
ggen graph query --help
```

---

## Environment Variables

**`GGEN_API_KEY`** - AI provider API key

**`GGEN_MODEL`** - Default AI model

**`GGEN_CONFIG_DIR`** - Config directory (default: `~/.ggen`)

**`GGEN_TEMPLATE_DIR`** - Custom template directory

**Examples**:
```bash
# Set API key
export GGEN_API_KEY=sk-your-key-here

# Set default model
export GGEN_MODEL=claude-3-sonnet

# Custom config directory
export GGEN_CONFIG_DIR=~/my-ggen-config
```

---

## Exit Codes

- `0` - Success
- `1` - General error
- `2` - Invalid arguments
- `3` - File not found
- `4` - Permission denied
- `5` - Validation failed

---

## See Also

- [Quick Start Tutorial](../../getting-started/quick-start.md)
- [Generate JavaScript + Zod](../../how-to/generation/generate-javascript-zod.md)
- [Query RDF with SPARQL](../../how-to/generation/query-rdf-sparql.md)
- [Ontology-Driven Development](../../explanations/fundamentals/ontology-driven-development.md)
- [RDF for Programmers](../../explanations/fundamentals/rdf-for-programmers.md)

---

**Last Updated**: 2025-12-10
**ggen Version**: 3.4.1+
