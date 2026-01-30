<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-To: Common Tasks](#how-to-common-tasks)
  - [How to: Generate a REST API](#how-to-generate-a-rest-api)
    - [Task: Create a new REST API from scratch](#task-create-a-new-rest-api-from-scratch)
  - [How to: Generate Database Migrations](#how-to-generate-database-migrations)
    - [Task: Create SQL migrations from ontology](#task-create-sql-migrations-from-ontology)
  - [How to: Generate Multi-Language Code](#how-to-generate-multi-language-code)
    - [Task: Generate TypeScript and Python from same ontology](#task-generate-typescript-and-python-from-same-ontology)
  - [How to: Update Existing Generation](#how-to-update-existing-generation)
    - [Task: Modify generated code by editing ontology](#task-modify-generated-code-by-editing-ontology)
  - [How to: Debug Generation Issues](#how-to-debug-generation-issues)
    - [Task: Diagnose why generation failed](#task-diagnose-why-generation-failed)
  - [How to: Cache and Performance](#how-to-cache-and-performance)
    - [Task: Speed up generation for large ontologies](#task-speed-up-generation-for-large-ontologies)
  - [How to: Watch Mode (Auto-Regenerate)](#how-to-watch-mode-auto-regenerate)
    - [Task: Automatically generate when files change](#task-automatically-generate-when-files-change)
  - [How to: Verify Determinism](#how-to-verify-determinism)
    - [Task: Confirm reproducible generation](#task-confirm-reproducible-generation)
  - [How to: CI/CD Integration](#how-to-cicd-integration)
    - [Task: Use ggen in your CI/CD pipeline](#task-use-ggen-in-your-cicd-pipeline)
  - [How to: Version Control Best Practices](#how-to-version-control-best-practices)
    - [Task: Manage ontologies and generated code in git](#task-manage-ontologies-and-generated-code-in-git)
  - [How to: Share Ontologies as Packages](#how-to-share-ontologies-as-packages)
    - [Task: Publish ontology to marketplace](#task-publish-ontology-to-marketplace)
  - [How to: Troubleshoot Common Issues](#how-to-troubleshoot-common-issues)
    - [Issue: "SPARQL query failed"](#issue-sparql-query-failed)
    - [Issue: "Template variable undefined"](#issue-template-variable-undefined)
    - [Issue: "Generation timed out"](#issue-generation-timed-out)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-To: Common Tasks

**Step-by-step guides for everyday ggen workflows.**

## How to: Generate a REST API

### Task: Create a new REST API from scratch

```bash
# 1. Create ontology
cat > .specify/specs/002-products/products.ttl <<'EOF'
@prefix : <https://api.example.org/products/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:Product a :ResourceType ;
  :fields ( :id :name :price :stock ) .

:ProductAPI a :APIService ;
  :baseUrl "/api/products" ;
  :endpoints ( :ListProducts :CreateProduct :GetProduct ) .

:ListProducts a :Endpoint ;
  :method :GET ;
  :path "/products" ;
  :responseType :ProductList .

:CreateProduct a :Endpoint ;
  :method :POST ;
  :path "/products" ;
  :requestBody :ProductInput ;
  :responseType :Product .

:GetProduct a :Endpoint ;
  :method :GET ;
  :path "/products/{id}" ;
  :responseType :Product .
EOF

# 2. Create template
cat > templates/rust/api.rs.tera <<'EOF'
pub mod products {
{% for endpoint in endpoints %}
    pub async fn {{ endpoint.name }}() {}
{% endfor %}
}
EOF

# 3. Generate
ggen sync

# 4. View generated code
cat src/api.rs
```

## How to: Generate Database Migrations

### Task: Create SQL migrations from ontology

```bash
# 1. Update ontology with database info
cat >> .specify/specs/001-users/users.ttl <<'EOF'
:UsersTable a :DatabaseTable ;
  :tableName "users" ;
  :columns ( :id :name :email :created_at ) .

:id a :Column ;
  :columnName "id" ;
  :type xsd:string ;
  :constraint :PrimaryKey .

:name a :Column ;
  :type xsd:string ;
  :nullable false .
EOF

# 2. Create migration template
cat > templates/sql/migration.sql.tera <<'EOF'
-- Migration generated from ontology
-- Do not edit manually

{% for table in tables %}
CREATE TABLE IF NOT EXISTS {{ table.tableName }} (
{% for column in table.columns %}
    {{ column.columnName }} {{ column.sqlType }}{{ column.constraints }},
{% endfor %}
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
{% endfor %}
EOF

# 3. Generate
ggen sync

# 4. Apply migrations
psql -U postgres -d mydb -f migrations/*.sql
```

## How to: Generate Multi-Language Code

### Task: Generate TypeScript and Python from same ontology

```bash
# 1. Add templates for multiple languages
cat > templates/typescript/types.ts.tera <<'EOF'
{% for resource in resources %}
export interface {{ resource.name }} {
{% for field in resource.fields %}
  {{ field.name }}: {{ field.typeScriptType }};
{% endfor %}
}
{% endfor %}
EOF

cat > templates/python/models.py.tera <<'EOF'
{% for resource in resources %}
@dataclass
class {{ resource.name }}:
{% for field in resource.fields %}
    {{ field.name }}: {{ field.pythonType }}
{% endfor %}
{% endfor %}
EOF

# 2. Update ggen.toml
cat >> ggen.toml <<'EOF'
templates = [
    "templates/rust/models.rs.tera",
    "templates/typescript/types.ts.tera",
    "templates/python/models.py.tera",
]
EOF

# 3. Generate all languages
ggen sync

# 4. Check all outputs
ls -la src/models.*
```

## How to: Update Existing Generation

### Task: Modify generated code by editing ontology

```bash
# 1. View current ontology
cat .specify/specs/001-users/users.ttl

# 2. Edit ontology (add new field)
# Add to :User:
# :phone a :Field ;
#   :type xsd:string ;
#   :required false .

# 3. Preview changes
ggen sync --dry_run true

# 4. Apply changes
ggen sync

# 5. Verify (phone field added)
grep -A 2 "phone:" src/models.rs
```

## How to: Debug Generation Issues

### Task: Diagnose why generation failed

```bash
# 1. Enable debug logging
export GGEN_LOG_LEVEL=debug

# 2. Run with verbose output
ggen sync --verbose

# 3. Check for SPARQL errors
# Look for: "SPARQL query failed"
# Check ontology syntax if errors found

# 4. Validate ontology
ggen sync --validate_only true

# 5. Check template syntax
# Tera will show template errors
# Common issue: missing filters or undefined variables

# 6. View raw SPARQL queries
cat .ggen/logs/*.json | jq '.sparql_queries'
```

## How to: Cache and Performance

### Task: Speed up generation for large ontologies

```bash
# 1. Enable caching
ggen sync --cache true

# 2. First run (generates cache)
time ggen sync
# Result: ~15 seconds

# 3. Second run (uses cache)
time ggen sync
# Result: ~2-3 seconds (6x faster!)

# 4. Clear cache if needed
rm -rf .ggen/cache/*
ggen sync  # Regenerates cache
```

## How to: Watch Mode (Auto-Regenerate)

### Task: Automatically generate when files change

```bash
# 1. Enable watch mode
ggen sync --watch true

# 2. Open another terminal
# In first terminal: ggen watches for changes

# 3. Edit ontology in second terminal
echo "# New comment" >> .specify/specs/001-users/users.ttl

# 4. First terminal automatically regenerates
# Watch: Files changed, regenerating...
# Watch: Generation complete

# 5. Stop watching
# Press Ctrl+C in watch terminal
```

## How to: Verify Determinism

### Task: Confirm reproducible generation

```bash
# 1. Generate code
ggen sync

# 2. Save receipt
cp .ggen/receipts/latest.json receipt-1.json

# 3. Delete generated files
rm -rf src/*.rs

# 4. Regenerate
ggen sync

# 5. Save new receipt
cp .ggen/receipts/latest.json receipt-2.json

# 6. Compare (should be identical)
diff receipt-1.json receipt-2.json
# Output: (empty - they're identical!)

# 7. Compare file hashes
jq '.files[] | .path, .hash' receipt-1.json > hashes-1.txt
jq '.files[] | .path, .hash' receipt-2.json > hashes-2.txt
diff hashes-1.txt hashes-2.txt
# Output: (empty - hashes match!)

echo "✓ Generation is deterministic"
```

## How to: CI/CD Integration

### Task: Use ggen in your CI/CD pipeline

```bash
# GitHub Actions example
# .github/workflows/generate.yml

name: Generate Code from Ontology

on:
  push:
    paths:
      - '.specify/specs/**/*.ttl'
      - 'templates/**'
    branches: [main, develop]

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: dtolnay/rust-toolchain@stable

      - name: Install ggen
        run: cargo install ggen

      - name: Generate code
        run: ggen sync --audit true

      - name: Commit changes
        run: |
          git config user.email "github-actions@example.com"
          git config user.name "GitHub Actions"
          git add -A
          git commit -m "Generated code from ontology"
          git push
```

## How to: Version Control Best Practices

### Task: Manage ontologies and generated code in git

```bash
# 1. In .gitignore (don't commit generated code):
cat >> .gitignore <<'EOF'
# Generated code
/src/*.rs
/src/*.ts
/src/*.py

# ggen cache
/.ggen/cache/
/.ggen/audit/

# Dependencies
/target/
/node_modules/
EOF

# 2. DO commit these:
git add .specify/specs/**/*.ttl  # Ontologies (source of truth)
git add templates/               # Templates
git add ggen.toml               # Configuration
git add .ggen/receipts/latest.json  # Proof of generation

# 3. Generate on checkout
cat > .git/hooks/post-checkout <<'EOF'
#!/bin/bash
if git diff --name-only HEAD@{1} HEAD | grep -E '\.specify.*\.ttl|templates/|ggen\.toml'; then
  echo "Specifications changed, regenerating..."
  ggen sync
fi
EOF

chmod +x .git/hooks/post-checkout
```

## How to: Share Ontologies as Packages

### Task: Publish ontology to marketplace

```bash
# 1. Create manifest
cat > .specify/manifest.toml <<'EOF'
[package]
name = "user-api"
version = "1.0.0"
author = "Your Name"
description = "User management ontology"

[dependencies]
# Other ontologies this depends on
# base-api = "0.1.0"
EOF

# 2. Publish
ggen publish --registry https://registry.ggen.io

# 3. Use in another project
ggen add user-api

# 4. Reference in ontology
@prefix userapi: <https://registry.ggen.io/user-api/> .
userapi:BaseUser a :Resource ;
  ...
```

## How to: Troubleshoot Common Issues

### Issue: "SPARQL query failed"

```bash
# Solution: Check RDF triples are correct
ggen sync --validate_only true

# Debug: List all triples
ggen inspect-ontology .specify/specs/**/*.ttl
```

### Issue: "Template variable undefined"

```bash
# Solution: Check variable name matches SPARQL query result
# In template: {{ user.name }}
# Ensure SPARQL query returns: ?name

# Debug: View SPARQL context
GGEN_LOG_LEVEL=debug ggen sync 2>&1 | grep -A 5 "template context"
```

### Issue: "Generation timed out"

```bash
# Solution: Reduce ontology complexity
# Split large .ttl files into smaller ones
# Simplify SPARQL queries

# Quick validation only (no generation)
ggen sync --validate_only true
```

---

**More how-to guides coming soon for:**
- GraphQL generation
- Microservices architecture
- Document generation
- Configuration files
