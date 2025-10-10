<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 004: NOUN-VERB CLI ***](#pattern-004-noun-verb-cli-)
  - [Context](#context)
  - [Problem](#problem)
  - [Forces](#forces)
  - [Solution](#solution)
  - [Result](#result)
  - [Command Structure](#command-structure)
    - [**Graph Operations**](#graph-operations)
    - [**Template Operations**](#template-operations)
    - [**Project Operations**](#project-operations)
    - [**Marketplace Operations**](#marketplace-operations)
    - [**Audit Operations**](#audit-operations)
    - [**CI/CD Operations**](#cicd-operations)
  - [Example: Complete Workflow](#example-complete-workflow)
  - [Command Discovery](#command-discovery)
    - [**Help System**](#help-system)
    - [**Tab Completion**](#tab-completion)
    - [**Command Validation**](#command-validation)
  - [Advanced Usage](#advanced-usage)
    - [**Chaining Operations**](#chaining-operations)
    - [**Scripting Integration**](#scripting-integration)
    - [**Configuration Integration**](#configuration-integration)
  - [Error Handling](#error-handling)
    - [**Semantic Errors**](#semantic-errors)
    - [**Template Errors**](#template-errors)
    - [**Project Errors**](#project-errors)
  - [Anti-Patterns](#anti-patterns)
  - [Related Patterns](#related-patterns)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern 004: NOUN-VERB CLI ***

## Context

Building on **Pattern 001: KNOWLEDGE-FIRST PROJECTION** and **Pattern 003: GRAPH-TEMPLATE BINDING**, we have semantic graphs as the source of truth and templates that explicitly declare their data needs. However, users need an intuitive way to interact with these semantic concepts through the command line. Traditional CLI designs don't align well with semantic operations.

## Problem

**How do you design a command-line interface that naturally maps to semantic operations on knowledge graphs while remaining intuitive for users?**

Traditional CLI approaches face:
- Commands don't reflect semantic concepts
- Difficult to discover available operations
- No clear mapping between CLI operations and graph entities
- Inconsistent command patterns across different domains
- Hard to extend with new semantic operations

## Forces

- Users think in terms of semantic concepts (graphs, templates, projects)
- CLI should be intuitive and discoverable
- Commands should map naturally to semantic operations
- Interface should be consistent across different domains
- New operations should be easy to add
- Commands should work well with shell completion
- Interface should support both interactive and scripted usage

## Solution

**Therefore, use a noun-verb command structure that directly maps to semantic concepts and operations.**

The CLI follows the pattern: `ggen <noun> <verb> [options]`

Where:
- **Nouns** represent semantic concepts (graph, template, project, market, audit, ci)
- **Verbs** represent operations on those concepts (load, query, apply, gen, search, install)

## Result

You achieve:

- **Semantic Clarity**: Commands directly reflect semantic concepts
- **Intuitive Discovery**: Users can explore available operations naturally
- **Consistent Patterns**: Same verb means the same thing across different nouns
- **Easy Extension**: New concepts and operations follow established patterns
- **Shell Integration**: Commands work well with tab completion and scripting
- **Domain Mapping**: Clear mapping between CLI operations and graph entities

This pattern enables → **Pattern 005: MULTI-LANGUAGE PROJECTION** (semantic operations)
This pattern enables → **Pattern 006: LOCKFILE VERSIONING** (dependency management)
This pattern enables → **Pattern 007: SNAPSHOT TESTING** (version control integration)

## Command Structure

### **Graph Operations**
```bash
# Load semantic data into graph
ggen graph load domain-model.ttl

# Query the graph
ggen graph query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"

# Export graph data
ggen graph export --format json > graph.json

# Validate graph against SHACL shapes
ggen graph validate --shapes shapes.ttl

# Show graph statistics
ggen graph stats
```

### **Template Operations**
```bash
# Apply template to generate code
ggen template apply user-model.tmpl --output src/models/user.rs

# List available templates
ggen template list

# Show template information
ggen template show user-model.tmpl

# Validate template syntax
ggen template validate user-model.tmpl
```

### **Project Operations**
```bash
# Generate complete project
ggen project gen --template web-api --output my-project

# Plan generation (dry run)
ggen project plan --template web-api

# Show differences
ggen project diff --template web-api

# Apply changes
ggen project apply --template web-api
```

### **Marketplace Operations**
```bash
# Search for templates
ggen market search "rust api"

# Install template
ggen market install io.ggen.rust.api

# Show template details
ggen market show io.ggen.rust.api

# Update template
ggen market update io.ggen.rust.api

# Remove template
ggen market remove io.ggen.rust.api
```

### **Audit Operations**
```bash
# Generate hazard report
ggen audit hazard

# Analyze performance
ggen audit performance

# Check security vulnerabilities
ggen audit security

# Assess project risk
ggen audit risk
```

### **CI/CD Operations**
```bash
# Deploy to GitHub Pages
ggen ci pages

# Manage GitHub Actions workflows
ggen ci workflow

# Trigger workflows
ggen ci trigger

# Build artifacts
ggen ci build
```

## Example: Complete Workflow

```bash
# 1. Load domain model
ggen graph load domain-model.ttl

# 2. Verify graph contents
ggen graph query "PREFIX : <http://example.org/> SELECT ?class WHERE { ?class a rdfs:Class }"

# 3. Search for relevant templates
ggen market search "rust struct"

# 4. Install template
ggen market install io.ggen.rust.struct

# 5. Apply template
ggen template apply io.ggen.rust.struct --output src/models.rs

# 6. Validate generated code
ggen audit security src/models.rs

# 7. Generate complete project
ggen project gen --template web-api --output my-api

# 8. Plan changes
ggen project plan --template web-api

# 9. Apply changes
ggen project apply --template web-api
```

## Command Discovery

### **Help System**
```bash
# General help
ggen --help

# Noun-specific help
ggen graph --help
ggen template --help
ggen project --help

# Verb-specific help
ggen graph load --help
ggen template apply --help
ggen project gen --help
```

### **Tab Completion**
```bash
# Complete nouns
ggen <TAB>
# Shows: graph, template, project, market, audit, ci

# Complete verbs for specific noun
ggen graph <TAB>
# Shows: load, query, export, validate, stats

# Complete options
ggen graph load <TAB>
# Shows: --format, --output, --verbose
```

### **Command Validation**
```bash
# Invalid noun
ggen invalid --help
# Error: Unknown command 'invalid'. Did you mean 'graph'?

# Invalid verb
ggen graph invalid --help
# Error: Unknown command 'invalid'. Available commands: load, query, export, validate, stats
```

## Advanced Usage

### **Chaining Operations**
```bash
# Chain graph operations
ggen graph load model.ttl && ggen graph query "SELECT ?s WHERE { ?s a rdfs:Class }" | ggen template apply struct.tmpl
```

### **Scripting Integration**
```bash
#!/bin/bash
# Generate API from domain model

set -e

# Load domain model
ggen graph load "$1"

# Generate models
ggen template apply models.tmpl --output src/models.rs

# Generate handlers
ggen template apply handlers.tmpl --output src/handlers.rs

# Generate tests
ggen template apply tests.tmpl --output tests/api_tests.rs

# Validate generated code
ggen audit security src/
```

### **Configuration Integration**
```bash
# Use configuration file
ggen --config ggen.toml project gen

# Override configuration
ggen --config ggen.toml --set port=3000 project gen
```

## Error Handling

### **Semantic Errors**
```bash
# Missing graph data
ggen template apply struct.tmpl
# Error: No graph data loaded. Use 'ggen graph load <file>' first.

# Invalid query
ggen graph query "INVALID SPARQL"
# Error: Invalid SPARQL query: syntax error at line 1
```

### **Template Errors**
```bash
# Missing template
ggen template apply missing.tmpl
# Error: Template 'missing.tmpl' not found

# Template validation error
ggen template apply invalid.tmpl
# Error: Template validation failed: missing required field 'name'
```

### **Project Errors**
```bash
# Conflicting files
ggen project apply --template web-api
# Error: Conflicting files detected. Use --force to overwrite.
```

## Anti-Patterns

❌ **Verb-Noun Structure**: `ggen apply template` (confusing)
❌ **Monolithic Commands**: `ggen generate-everything` (not composable)
❌ **Inconsistent Patterns**: Different verbs for similar operations
❌ **No Discovery**: Commands that can't be discovered through help
❌ **Hardcoded Operations**: Commands that don't work with different domains

## Related Patterns

- **Pattern 001: KNOWLEDGE-FIRST PROJECTION** - Provides semantic foundation
- **Pattern 003: GRAPH-TEMPLATE BINDING** - Enables semantic operations
- **Pattern 005: MULTI-LANGUAGE PROJECTION** - Semantic operations across languages
- **Pattern 006: LOCKFILE VERSIONING** - Dependency management operations
- **Pattern 007: SNAPSHOT TESTING** - Version control operations