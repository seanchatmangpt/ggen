# Chapter 5: The Noun-Verb CLI Design

## Philosophy: Commands as Sentences

Most CLI tools organize around **verbs**: `create`, `delete`, `update`, `list`.

GGen organizes around **nouns**: `project`, `market`, `template`, `graph`.

Why? Because the **entities** in your domain are more stable than the **operations** you perform on them.

Compare:

**Verb-first CLI:**
```bash
create project my-app
delete project my-app
update template api
list templates
install template crud
```

**Noun-first CLI (GGen):**
```bash
ggen project create my-app
ggen project delete my-app
ggen template update api
ggen template list
ggen market install crud
```

The noun-first approach creates a natural hierarchy:
- All **project** operations are under `ggen project`
- All **market** operations are under `ggen market`
- All **template** operations are under `ggen template`

This makes the CLI **discoverable** and **intuitive**. You think "I want to do something with a project" and type `ggen project <TAB>` to see options.

## The Core Nouns

GGen has four primary nouns:

### 1. `project`

A **project** is your codebase—a directory with a knowledge graph and generated code.

**Operations:**
- `ggen project init` - Create a new project
- `ggen project generate` - Generate code from knowledge
- `ggen project validate` - Validate knowledge graph
- `ggen project query` - Run SPARQL queries
- `ggen project sync` - Sync knowledge with code

**Philosophy:** A project is where knowledge lives and code is projected.

### 2. `market`

The **market** is a repository of reusable templates, patterns, and ontologies.

**Operations:**
- `ggen market search` - Find templates
- `ggen market install` - Install a template
- `ggen market publish` - Publish your template
- `ggen market update` - Update installed templates

**Philosophy:** Don't reinvent patterns—reuse community templates.

### 3. `template`

A **template** transforms knowledge (RDF) into code.

**Operations:**
- `ggen template create` - Create a new template
- `ggen template test` - Test a template
- `ggen template list` - List available templates
- `ggen template edit` - Edit a template

**Philosophy:** Templates are the bridge between knowledge and code.

### 4. `graph`

The **graph** is your domain knowledge in RDF/OWL format.

**Operations:**
- `ggen graph add` - Add entities or relationships
- `ggen graph query` - Run SPARQL queries
- `ggen graph visualize` - Generate graph visualizations
- `ggen graph validate` - Validate with SHACL
- `ggen graph merge` - Merge multiple graphs

**Philosophy:** The graph is your source of truth.

## Command Structure

All GGen commands follow this pattern:

```
ggen <noun> <verb> [arguments] [flags]
```

**Examples:**

```bash
# Project commands
ggen project init my-ecommerce
ggen project generate --template api-types
ggen project validate --strict

# Market commands
ggen market search crud
ggen market install @ggen/crud-rest-api
ggen market publish ./my-template

# Template commands
ggen template create my-template
ggen template test my-template --input test-graph.ttl

# Graph commands
ggen graph add entity User
ggen graph query "SELECT ?class WHERE { ?class a owl:Class }"
ggen graph visualize --output graph.svg
```

## Flags and Options

GGen uses consistent flag conventions:

**Global flags (work with all commands):**
- `--verbose` or `-v` - Detailed output
- `--quiet` or `-q` - Minimal output
- `--dry-run` - Show what would happen without doing it
- `--help` or `-h` - Show help

**Common flags:**
- `--output` or `-o` - Specify output file/directory
- `--format` or `-f` - Specify output format (json, yaml, turtle, etc.)
- `--template` or `-t` - Specify template to use
- `--force` - Override safety checks
- `--watch` - Watch for changes and regenerate

**Examples:**

```bash
# Verbose output
ggen project generate --verbose

# Dry run (see what would be generated)
ggen project generate --dry-run

# Custom output directory
ggen project generate --output ./generated

# Watch mode (regenerate on knowledge changes)
ggen project generate --watch

# Force overwrite existing files
ggen project generate --force
```

## The Daily Workflow

A typical day with GGen follows this pattern:

### Morning: Set up or sync

```bash
# Start a new project
ggen project init my-app --template ecommerce

# Or sync existing project
cd my-app
ggen project sync
```

### During Development: Modify knowledge

```bash
# Add an entity
ggen graph add entity Review --properties rating:integer,comment:string

# Add a relationship
ggen graph add relationship Review belongsTo Product

# Validate the graph
ggen graph validate
```

### Generate Code

```bash
# Generate all configured outputs
ggen project generate

# Or generate specific targets
ggen project generate --template db-schema
ggen project generate --template ts-types
ggen project generate --template crud-api
```

### Query and Explore

```bash
# Query the graph
ggen graph query "SELECT ?entity WHERE { ?entity a owl:Class }"

# Visualize relationships
ggen graph visualize --output docs/domain-model.svg

# Find all indexed properties
ggen graph query "SELECT ?prop WHERE { ?prop :indexed true }"
```

### Install Patterns

```bash
# Search the market
ggen market search authentication

# Install a pattern
ggen market install @ggen/oauth2-pattern

# Apply it to your graph
ggen graph merge --source market/oauth2-pattern.ttl
```

### Iterate

```bash
# Make changes to graph
nano knowledge/domain.ttl

# Regenerate (watch mode auto-regenerates)
ggen project generate

# Run tests
npm test

# Commit knowledge and generated code
git add knowledge/ generated/
git commit -m "Add Review entity with validation"
```

## Configuration

GGen projects use a config file: `ggen.config.json` or `ggen.config.yaml`.

**Example `ggen.config.yaml`:**

```yaml
project:
  name: my-ecommerce
  version: 1.0.0

knowledge:
  sources:
    - knowledge/domain.ttl
    - knowledge/patterns.ttl
  namespaces:
    app: http://myapp.com/ontology#

generation:
  targets:
    - name: db-schema
      template: "@ggen/postgresql-schema"
      output: migrations/

    - name: ts-types
      template: "@ggen/typescript-types"
      output: src/types/

    - name: crud-api
      template: "@ggen/nestjs-crud"
      output: src/api/

    - name: graphql-schema
      template: "@ggen/graphql-schema"
      output: src/schema.graphql

    - name: api-docs
      template: "@ggen/openapi-docs"
      output: docs/api.yaml

validation:
  shapes: knowledge/shapes.ttl
  strict: true

market:
  registry: https://market.ggen.io
  cache: ~/.ggen/cache
```

This config defines:
- Where knowledge is stored (`knowledge/`)
- What to generate (`db-schema`, `ts-types`, etc.)
- Which templates to use (`@ggen/postgresql-schema`)
- Where to output generated code
- Validation rules

## Intelligent Defaults

GGen uses convention over configuration:

**Default directory structure:**
```
my-project/
├── ggen.config.yaml       # Configuration
├── knowledge/             # Knowledge graphs
│   ├── domain.ttl         # Main domain model
│   ├── patterns.ttl       # Reusable patterns
│   └── shapes.ttl         # SHACL validation
├── templates/             # Custom templates
│   └── my-template.hbs
├── generated/             # Generated code (git-ignored)
│   ├── types/
│   ├── api/
│   └── schema/
└── src/                   # Hand-written code
    └── custom/
```

**Default namespaces:**
- `app:` → `http://yourproject.com/ontology#`
- `owl:` → `http://www.w3.org/2002/07/owl#`
- `rdfs:` → `http://www.w3.org/2000/01/rdf-schema#`

**Default generation targets:**
If you don't specify targets, GGen generates based on detected project type:
- Node.js project → TypeScript types
- Python project → Pydantic models
- Database detected → SQL migrations

## Advanced Commands

### Audit and Diff

```bash
# Show what's changed since last generation
ggen project diff

# Show impact of a knowledge change
ggen graph impact --entity User

# Audit generated code for drift
ggen project audit
```

### Migration

```bash
# Generate migration from knowledge changes
ggen project migrate --from v1.0 --to v2.0

# Preview migration
ggen project migrate --dry-run

# Rollback migration
ggen project rollback --to v1.0
```

### Multi-Language

```bash
# Generate for multiple languages
ggen project generate --lang typescript,python,rust

# Language-specific output
ggen project generate --lang python --output python-client/
```

## Shell Completion

GGen supports tab completion for all shells:

```bash
# Bash
ggen completion bash > /etc/bash_completion.d/ggen

# Zsh
ggen completion zsh > ~/.zsh/completion/_ggen

# Fish
ggen completion fish > ~/.config/fish/completions/ggen.fish
```

Now you can type `ggen project <TAB>` and see all available verbs.

## Help System

Every noun and verb has built-in help:

```bash
# Top-level help
ggen --help

# Noun-level help
ggen project --help

# Verb-level help
ggen project generate --help

# Examples for a command
ggen project generate --examples
```

## The Design Philosophy

The noun-verb structure reflects KGC principles:

1. **Nouns are domain concepts** (project, market, template, graph)
2. **Verbs are operations** (create, generate, validate, query)
3. **Entities are stable**, operations vary
4. **Discoverability** through hierarchy
5. **Consistency** through conventions

This makes GGen **learnable**—you don't memorize commands, you explore the structure.

In the next chapters, we'll deep-dive into each noun:
- **Chapter 6:** The `project` noun
- **Chapter 7:** The `market` noun
- **Chapter 8:** Generation workflows
