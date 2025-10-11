<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Quickstart](#quickstart)
  - [AI-Powered Generation (NEW)](#ai-powered-generation-new)
    - [1. Generate Templates with AI](#1-generate-templates-with-ai)
    - [2. Generate SPARQL Queries with AI](#2-generate-sparql-queries-with-ai)
    - [3. Generate RDF Graphs with AI](#3-generate-rdf-graphs-with-ai)
    - [4. Generate Complete Projects with AI](#4-generate-complete-projects-with-ai)
  - [Marketplace Workflow (Recommended)](#marketplace-workflow-recommended)
    - [1. Search for Templates](#1-search-for-templates)
    - [2. Install an Gpack](#2-install-an-gpack)
    - [3. Generate Code](#3-generate-code)
    - [4. Verify Installation](#4-verify-installation)
  - [Local Templates (Advanced)](#local-templates-advanced)
    - [1. Initialize Project Structure](#1-initialize-project-structure)
    - [2. Create Template](#2-create-template)
    - [3. Generate](#3-generate)
  - [Marketplace vs Local Templates](#marketplace-vs-local-templates)
  - [Troubleshooting](#troubleshooting)
    - [First-time Setup Issues](#first-time-setup-issues)
    - [Template Not Found](#template-not-found)
    - [Generation Errors](#generation-errors)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Quickstart

## AI-Powered Generation (NEW)

**ggen-ai v1.0.0** introduces intelligent code generation using advanced LLMs with multi-provider support.

### 1. Generate Templates with AI

```bash
# Generate a REST API controller
ggen ai generate "A Rust REST API controller for user management" \
  --language rust --framework axum --output user_controller.tmpl

# Generate a Python CLI tool
ggen ai generate "Python CLI tool for data processing" \
  --language python --output data_tool.py.tmpl
```

### 2. Generate SPARQL Queries with AI

```bash
# Generate queries from natural language intent
ggen ai sparql "Find all users with admin role" \
  --graph data.ttl --output admin_query.sparql

# Generate complex queries
ggen ai sparql "Get all properties and relationships for resources" \
  --graph ontology.ttl --output properties.sparql
```

### 3. Generate RDF Graphs with AI

```bash
# Generate ontologies from domain descriptions
ggen ai graph "Person management system with roles and permissions" \
  --output person.ttl

# Generate domain-specific graphs
ggen ai graph "E-commerce product catalog with categories" \
  --output catalog.ttl
```

### 4. Generate Complete Projects with AI

```bash
# Generate entire project structures
ggen ai project "E-commerce API with authentication" \
  --name shop-api --language rust --framework axum \
  --tests --docs --output generated-shop-api/

# Generate web applications
ggen ai project "Python web application with database" \
  --name webapp --language python --framework fastapi \
  --output webapp/
```

## Marketplace Workflow (Recommended)

The fastest way to get started with ggen is using marketplace gpacks - pre-built, tested template collections.

### 1. Search for Templates

```bash
# Search for CLI subcommand templates
ggen search rust cli

# Output:
# ID                                    LATEST     KIND       TAGS
# io.ggen.rust.cli-subcommand           0.2.1      template   rust, cli, clap
# io.ggen.rust.api-endpoint             0.1.5      template   rust, api, axum
```

### 2. Install an Gpack

```bash
# Install the latest version
ggen add io.ggen.rust.cli-subcommand

# Or install specific version
ggen add io.ggen.rust.cli-subcommand@0.2.0
```

### 3. Generate Code

```bash
# Use the installed gpack template
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello description="Print a greeting"
```

### 4. Verify Installation

```bash
# List installed gpacks
ggen packs

# Show template details
ggen show io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl
```

Result:

```
src/cmds/hello.rs
```

## Local Templates (Advanced)

For custom templates or when you need full control over the generation process:

### 1. Initialize Project Structure

```bash
mkdir -p templates/cli/subcommand
```

### 2. Create Template

Create `templates/cli/subcommand/rust.tmpl`:

```yaml
---
to: src/cmds/{{ slug }}.rs
vars: { cmd: hello, summary: "Print a greeting", seed: cosmos }
rdf:
  - "graphs/cli.ttl"              # Local RDF file
shape:
  - "graphs/shapes/cli.shacl.ttl" # Local SHACL shapes
sparql:
  vars:
    - name: slug
      query: |
        PREFIX cli: <urn:ggen:cli#>
        SELECT ?slug WHERE { ?c a cli:Command ; cli:slug ?slug } LIMIT 1
determinism: { seed: "{{ seed }}" }
---
pub fn {{ slug }}(name:&str){ println!("hello {}", name); }
```

### 3. Generate

```bash
ggen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

## Marketplace vs Local Templates

| Feature | Marketplace Gpacks | Local Templates |
|---------|-------------------|-----------------|
| **Setup Time** | Instant | Requires creation |
| **Quality** | Community tested | Custom quality |
| **Updates** | Automatic | Manual |
| **Dependencies** | Managed | Manual |
| **Versioning** | Semantic | Ad-hoc |
| **Best For** | Common patterns | Custom needs |

## Troubleshooting

### First-time Setup Issues

```bash
# If ggen command not found after installation
which ggen
# Should show path to ggen binary

# If marketplace search fails
ggen search --help
# Check network connectivity and registry access
```

### Template Not Found

```bash
# Check if gpack is installed
ggen packs

# Reinstall if missing
ggen add io.ggen.rust.cli-subcommand

# Verify template path
ggen show io.ggen.rust.cli-subcommand
```

### Generation Errors

```bash
# Use dry run to preview
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello --dry

# Check variable requirements
ggen show io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl
```
