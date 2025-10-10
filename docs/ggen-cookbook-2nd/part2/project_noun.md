<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Chapter 6: The `project` Noun](#chapter-6-the-project-noun)
  - [What is a GGen Project?](#what-is-a-ggen-project)
  - [Creating a Project](#creating-a-project)
    - [Basic Initialization](#basic-initialization)
    - [Template-Based Initialization](#template-based-initialization)
    - [Interactive Initialization](#interactive-initialization)
  - [Project Structure](#project-structure)
    - [Knowledge Directory](#knowledge-directory)
    - [Configuration](#configuration)
    - [Templates Directory](#templates-directory)
  - [Core Commands](#core-commands)
    - [`ggen project generate`](#ggen-project-generate)
    - [`ggen project validate`](#ggen-project-validate)
    - [`ggen project query`](#ggen-project-query)
    - [`ggen project sync`](#ggen-project-sync)
    - [`ggen project diff`](#ggen-project-diff)
    - [`ggen project audit`](#ggen-project-audit)
  - [Workflow Examples](#workflow-examples)
    - [Daily Development](#daily-development)
    - [Refactoring](#refactoring)
    - [Team Collaboration](#team-collaboration)
  - [Advanced Features](#advanced-features)
    - [Incremental Generation](#incremental-generation)
    - [Multi-Language Output](#multi-language-output)
    - [Migration Generation](#migration-generation)
    - [Custom Hooks](#custom-hooks)
  - [Best Practices](#best-practices)
  - [Troubleshooting](#troubleshooting)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Chapter 6: The `project` Noun

## What is a GGen Project?

A **project** in GGen is a directory that contains:

1. **Knowledge graphs** (RDF/OWL files) defining your domain
2. **Configuration** (`ggen.config.yaml`) specifying generation targets
3. **Templates** (optional custom templates)
4. **Generated code** (output from templates)
5. **Custom code** (hand-written business logic)

Think of it as a workspace where knowledge and code coexist, with GGen managing the transformation between them.

## Creating a Project

### Basic Initialization

```bash
ggen project init my-app
```

This creates:
```
my-app/
├── ggen.config.yaml
├── knowledge/
│   ├── domain.ttl
│   └── shapes.ttl
├── templates/
└── .gitignore
```

**What happens:**
1. Creates directory structure
2. Initializes empty knowledge graph
3. Generates default config
4. Sets up `.gitignore` to exclude generated code

### Template-Based Initialization

Start with a pre-built template:

```bash
# E-commerce starter
ggen project init my-store --template ecommerce

# SaaS multi-tenant starter
ggen project init my-saas --template saas-multitenant

# REST API starter
ggen project init my-api --template rest-api

# GraphQL API starter
ggen project init my-graphql --template graphql-api
```

Templates include:
- Pre-defined domain entities (User, Product, Order, etc.)
- Common relationships
- Validation rules (SHACL shapes)
- Generation targets for typical tech stacks

### Interactive Initialization

```bash
ggen project init --interactive
```

Walks you through:
1. Project name and description
2. Tech stack selection (Node.js, Python, Rust, etc.)
3. Database choice (PostgreSQL, MySQL, MongoDB, etc.)
4. API style (REST, GraphQL, gRPC)
5. Authentication strategy (JWT, OAuth2, etc.)
6. Initial entities to create

Output: Customized project with appropriate templates and config.

## Project Structure

### Knowledge Directory

**`knowledge/domain.ttl`** - Your domain model

```turtle
@prefix : <http://myapp.com/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

# Entities
:User a owl:Class ;
  rdfs:label "User" ;
  :hasProperty :userName ;
  :hasProperty :userEmail .

:Product a owl:Class ;
  rdfs:label "Product" ;
  :hasProperty :productSKU ;
  :hasProperty :productPrice .

# Relationships
:userOrders a owl:ObjectProperty ;
  rdfs:domain :User ;
  rdfs:range :Order .
```

**`knowledge/shapes.ttl`** - Validation rules (SHACL)

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .

:UserShape a sh:NodeShape ;
  sh:targetClass :User ;
  sh:property [
    sh:path :email ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    sh:minCount 1 ;
    sh:maxCount 1
  ] .
```

**`knowledge/patterns.ttl`** - Reusable patterns

```turtle
# CRUD pattern
:CRUDEntity a owl:Class ;
  :supportsCRUD true ;
  :hasEndpoint :listEndpoint ;
  :hasEndpoint :getEndpoint ;
  :hasEndpoint :createEndpoint ;
  :hasEndpoint :updateEndpoint ;
  :hasEndpoint :deleteEndpoint .
```

### Configuration

**`ggen.config.yaml`**

```yaml
project:
  name: my-app
  version: 0.1.0
  description: "My knowledge-first application"

knowledge:
  sources:
    - knowledge/domain.ttl
    - knowledge/patterns.ttl
  validation:
    shapes: knowledge/shapes.ttl
    strict: true
  namespaces:
    app: "http://myapp.com/ontology#"

generation:
  outputDir: generated/
  cleanBeforeGenerate: true

  targets:
    # Database schema
    - name: db-schema
      template: "@ggen/postgresql-schema"
      output: migrations/
      enabled: true
      config:
        idType: uuid
        timestamps: true
        softDelete: false

    # TypeScript types
    - name: ts-types
      template: "@ggen/typescript-types"
      output: src/types/
      enabled: true
      config:
        exportStyle: named
        includeComments: true
        strictNullChecks: true

    # NestJS CRUD controllers
    - name: crud-api
      template: "@ggen/nestjs-crud"
      output: src/api/
      enabled: true
      config:
        authGuard: JwtAuthGuard
        paginationDefault: 20

    # GraphQL schema
    - name: graphql
      template: "@ggen/graphql-schema"
      output: src/schema.graphql
      enabled: false

    # OpenAPI documentation
    - name: api-docs
      template: "@ggen/openapi-docs"
      output: docs/api.yaml
      enabled: true

    # Database seed data
    - name: seed-data
      template: "@ggen/seed-generator"
      output: seeds/
      enabled: true
      config:
        recordsPerEntity: 10

templates:
  local: templates/
  market: ~/.ggen/market/

hooks:
  preGenerate:
    - "npm run lint:fix"
  postGenerate:
    - "npm run format"
    - "npm run test:types"
```

### Templates Directory

**Custom templates** for project-specific generation:

```
templates/
├── my-custom-api.hbs        # Custom API template
├── my-validation.hbs        # Custom validation
└── helpers/                 # Template helpers
    └── formatters.js
```

**Example custom template:**

```handlebars
{{!-- templates/my-custom-api.hbs --}}
/**
 * Auto-generated API for {{className}}
 * @generated {{generatedAt}}
 */
import { Controller, Get, Post, Body } from '@nestjs/common';
import { {{className}}Service } from './{{fileName}}.service';

@Controller('{{apiPath}}')
export class {{className}}Controller {
  constructor(private readonly service: {{className}}Service) {}

  @Get()
  async findAll() {
    return this.service.findAll();
  }

  @Post()
  async create(@Body() data: Create{{className}}DTO) {
    return this.service.create(data);
  }
}
```

## Core Commands

### `ggen project generate`

Generate code from knowledge graphs.

**Basic usage:**
```bash
# Generate all enabled targets
ggen project generate

# Generate specific target
ggen project generate --target db-schema

# Generate multiple targets
ggen project generate --target ts-types,crud-api

# Dry run (preview without writing)
ggen project generate --dry-run
```

**Advanced options:**
```bash
# Watch mode (regenerate on file changes)
ggen project generate --watch

# Force overwrite (skip confirmation)
ggen project generate --force

# Custom output directory
ggen project generate --output ./build

# Verbose logging
ggen project generate --verbose

# Generate only changed entities
ggen project generate --incremental
```

**Examples:**

```bash
# Generate only database migrations
ggen project generate --target db-schema

# Generate TypeScript types and watch for changes
ggen project generate --target ts-types --watch

# Dry run to preview all changes
ggen project generate --dry-run --verbose
```

### `ggen project validate`

Validate knowledge graphs against SHACL shapes.

```bash
# Validate all graphs
ggen project validate

# Validate specific graph
ggen project validate knowledge/domain.ttl

# Strict mode (fail on warnings)
ggen project validate --strict

# Output format
ggen project validate --format json
```

**Example output:**

```
Validating knowledge graphs...

✓ knowledge/domain.ttl (15 entities, 23 properties)
✗ knowledge/shapes.ttl (2 violations)

Violations:
  [1] :User → :email
      Constraint: sh:pattern "^[a-zA-Z0-9._%+-]+@..."
      Value: "invalid-email"
      Severity: sh:Violation

  [2] :Product → :price
      Constraint: sh:minInclusive 0
      Value: -10
      Severity: sh:Violation

Summary: 2 violations, 0 warnings
```

### `ggen project query`

Run SPARQL queries against your knowledge graph.

```bash
# Run a query
ggen project query "SELECT ?class WHERE { ?class a owl:Class }"

# Query from file
ggen project query --file queries/all-entities.sparql

# Output format
ggen project query --format json "SELECT * WHERE { ?s ?p ?o }"

# Save results
ggen project query --output results.json "SELECT ?entity WHERE { ?entity :supportsCRUD true }"
```

**Example queries:**

```bash
# List all entities
ggen project query "SELECT ?entity ?label WHERE {
  ?entity a owl:Class .
  ?entity rdfs:label ?label
}"

# Find all required properties
ggen project query "SELECT ?entity ?prop WHERE {
  ?entity :hasProperty ?prop .
  ?prop :required true
}"

# Find relationships between entities
ggen project query "SELECT ?domain ?relation ?range WHERE {
  ?relation a owl:ObjectProperty .
  ?relation rdfs:domain ?domain .
  ?relation rdfs:range ?range
}"

# Analyze graph complexity
ggen project query "SELECT (COUNT(?prop) as ?count) ?entity WHERE {
  ?entity :hasProperty ?prop
} GROUP BY ?entity ORDER BY DESC(?count)"
```

### `ggen project sync`

Synchronize knowledge with generated code.

```bash
# Sync all
ggen project sync

# Sync specific target
ggen project sync --target db-schema

# Detect drift (code changed manually)
ggen project sync --detect-drift

# Reverse sync (infer knowledge from code)
ggen project sync --reverse
```

**Use cases:**

1. **After git pull:** Ensure generated code matches knowledge
2. **After manual edits:** Detect and fix drift
3. **Onboarding:** Sync existing codebase to knowledge

### `ggen project diff`

Show differences between knowledge and generated code.

```bash
# Show all diffs
ggen project diff

# Diff specific target
ggen project diff --target ts-types

# Diff format
ggen project diff --format unified
```

**Example output:**

```diff
Comparing knowledge/domain.ttl with generated/types/user.ts

+ Entity: User
+ Property: phoneNumber (xsd:string, optional)

Generated code changes:
  src/types/user.ts:
    @@ -3,6 +3,7 @@
     export interface User {
       id: string;
       email: string;
    +  phoneNumber?: string;
       createdAt: string;
     }
```

### `ggen project audit`

Audit generated code for consistency and drift.

```bash
# Full audit
ggen project audit

# Audit specific target
ggen project audit --target crud-api

# Report format
ggen project audit --format html --output audit-report.html
```

**Audit checks:**

1. **Consistency:** Does code match knowledge?
2. **Drift:** Was generated code manually edited?
3. **Coverage:** Are all entities generated?
4. **Staleness:** Are there unused generated files?
5. **Version:** Is code generated with current templates?

## Workflow Examples

### Daily Development

**Morning: Start work**
```bash
cd my-app
ggen project sync       # Sync after git pull
ggen project validate   # Ensure graph is valid
```

**Add a feature**
```bash
# Edit knowledge graph
nano knowledge/domain.ttl

# Validate changes
ggen project validate

# Generate code
ggen project generate --watch  # Auto-regenerate on save
```

**Check impact**
```bash
# See what changed
ggen project diff

# Run tests
npm test
```

**Commit**
```bash
git add knowledge/ generated/
git commit -m "Add Review entity"
```

### Refactoring

**Before:**
```bash
# Save current state
ggen project snapshot --name before-refactor

# Audit current state
ggen project audit > audit-before.txt
```

**Refactor knowledge:**
```bash
# Make changes to knowledge/domain.ttl
# (rename entity, change relationships, etc.)

# Validate
ggen project validate --strict

# Generate
ggen project generate
```

**After:**
```bash
# Compare snapshots
ggen project diff --from before-refactor

# Audit again
ggen project audit > audit-after.txt

# Diff audits
diff audit-before.txt audit-after.txt
```

### Team Collaboration

**Developer A: Add entity**
```bash
# Add new entity to knowledge/domain.ttl
ggen project validate
ggen project generate --target db-schema
git commit -am "Add Review entity"
git push
```

**Developer B: Pull changes**
```bash
git pull
ggen project sync      # Regenerate from updated knowledge
npm run migrate        # Apply new database migration
npm test               # Ensure tests pass
```

## Advanced Features

### Incremental Generation

Only regenerate changed entities:

```bash
# Generate only entities modified since last generation
ggen project generate --incremental

# Generate entities changed in last commit
ggen project generate --since HEAD~1
```

### Multi-Language Output

Generate for multiple target languages:

```bash
ggen project generate --lang typescript,python

# Output:
#   generated/typescript/
#   generated/python/
```

### Migration Generation

Generate migrations from knowledge changes:

```bash
# Show migration preview
ggen project migrate --dry-run

# Generate migration
ggen project migrate --name add_reviews

# Output:
#   migrations/003_add_reviews.sql (generated SQL)
#   migrations/003_add_reviews.rollback.sql (rollback)
```

### Custom Hooks

Run commands before/after generation:

```yaml
# ggen.config.yaml
hooks:
  preGenerate:
    - "npm run lint:knowledge"
  postGenerate:
    - "npm run format"
    - "npm run test:generated"
    - "npm run build"
```

## Best Practices

**1. Version control knowledge, not generated code**

`.gitignore`:
```
generated/
*.generated.ts
migrations/*.sql  # except initial schema
```

Commit knowledge graphs; regenerate code on pull.

**2. Separate custom from generated**

```
src/
├── generated/       # Generated code (don't edit)
│   ├── types/
│   └── api/
└── custom/          # Hand-written code
    ├── services/
    └── utils/
```

Extend generated code; don't modify it.

**3. Use validation shapes**

Always define SHACL shapes for constraints. Catch errors before generation.

**4. Audit regularly**

```bash
# Weekly audit
ggen project audit --format html --output weekly-audit.html
```

**5. Template versioning**

Lock template versions in config:

```yaml
generation:
  targets:
    - template: "@ggen/typescript-types@1.2.3"  # Lock version
```

## Troubleshooting

**Problem:** Generated code doesn't match knowledge

**Solution:**
```bash
ggen project sync --force       # Force regeneration
ggen project audit              # Check for drift
```

**Problem:** Validation fails

**Solution:**
```bash
ggen project validate --verbose  # See detailed errors
# Fix knowledge graph
ggen project validate --strict   # Re-validate
```

**Problem:** Generation is slow

**Solution:**
```bash
ggen project generate --incremental  # Only generate changes
ggen project generate --target X     # Generate specific target
```

## Summary

The `project` noun is the heart of GGen. It's where:
- Knowledge is defined
- Code is generated
- Validation occurs
- Synchronization happens

Master these commands:
- `init` - Create projects
- `generate` - Project code
- `validate` - Ensure correctness
- `query` - Explore knowledge
- `sync` - Maintain consistency
- `diff` - Track changes
- `audit` - Verify integrity

Next: The `market` noun—how to find and share reusable patterns.
