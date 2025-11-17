# Marketplace Quick Start

Get started with ggen templates in 15 minutes.

## What You'll Learn

In this tutorial, you'll:
- Find and install a pre-built template
- Use the template to generate code for your domain
- Understand the marketplace ecosystem
- (Bonus) Publish your own template

## Prerequisites

- ggen installed (`ggen --version` works)
- Basic familiarity with your target language (Rust, TypeScript, Python, etc.)
- Time: 15-20 minutes

## Success Looks Like

After this tutorial, you can:
- ✅ Search for templates
- ✅ Install a template into your project
- ✅ Generate code using the template
- ✅ Customize the generated code for your domain

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Finding Templates](#finding-templates)
3. [Installing Templates](#installing-templates)
4. [Using Templates](#using-templates)
5. [Publishing Your Template](#publishing-your-template)
6. [Troubleshooting](#troubleshooting)

---

## Quick Start

### Step 1: Search for a Template

```bash
# Search for REST API templates
ggen marketplace search "rest api"

# Output shows matching templates:
# ✓ rest-api-template (v1.0.0)
#   REST API server template in Rust, TypeScript, Python
#   Production ready | 4.2/5 rating | 523 installs
#
# ✓ advanced-rust-api-8020 (v2.1.3)
#   Advanced Rust REST API with auth, logging, tracing
#   Production ready | 4.5/5 rating | 1203 installs
```

### Step 2: Install a Template

```bash
# Install the template
ggen marketplace install rest-api-template

# Output:
# Installing rest-api-template (v1.0.0)...
# ✓ Downloaded (SHA256: abc123...)
# ✓ Resolved dependencies
# ✓ Installed to ~/.ggen/templates/rest-api-template
#
# Next: ggen marketplace list-templates
```

### Step 3: See What's Installed

```bash
# List installed templates
ggen marketplace list

# Output:
# Installed Templates:
# ├─ rest-api-template (v1.0.0)
# │  REST API server template
# │  Location: ~/.ggen/templates/rest-api-template
# │  Status: Ready to use ✓
# └─ ...
```

### Step 4: Use the Template

```bash
# Create your domain ontology (simple example)
cat > domain.ttl << 'EOF'
@prefix ex: <http://example.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User
  rdfs:label "User" ;
  rdfs:comment "A system user" ;
  ex:hasProperty [
    rdfs:label "id" ;
    ex:type ex:Integer
  ] ;
  ex:hasProperty [
    rdfs:label "email" ;
    ex:type ex:String
  ] .

ex:Post
  rdfs:label "Post" ;
  rdfs:comment "A blog post" ;
  ex:hasProperty [
    rdfs:label "id" ;
    ex:type ex:Integer
  ] ;
  ex:hasProperty [
    rdfs:label "title" ;
    ex:type ex:String
  ] .
EOF

# Generate code using the template
ggen generate \
  --template rest-api-template \
  --domain domain.ttl \
  --language rust \
  --output ./generated

# Output:
# Generating code from template: rest-api-template
# Domain ontology: domain.ttl
# Language: Rust
# ✓ Generated User API (src/models/user.rs)
# ✓ Generated Post API (src/models/post.rs)
# ✓ Generated REST handlers (src/handlers/)
# ✓ Generated tests (tests/)
#
# Generated files:
# ./generated/
# ├── src/
# │   ├── models/
# │   │   ├── user.rs
# │   │   └── post.rs
# │   ├── handlers/
# │   │   ├── user_handlers.rs
# │   │   └── post_handlers.rs
# │   ├── main.rs
# │   └── lib.rs
# ├── tests/
# │   ├── user_tests.rs
# │   └── post_tests.rs
# ├── Cargo.toml
# └── README.md
```

### Step 5: Use Generated Code

```bash
# Navigate to generated project
cd generated

# Run tests
cargo test

# Output:
# running 12 tests
# test user::tests::test_create_user ... ok
# test user::tests::test_get_user ... ok
# test post::tests::test_create_post ... ok
# ...
# test result: ok. 12 passed
```

✅ **Success!** You've now:
- Discovered a template
- Installed it
- Generated code from your domain
- Verified the generated code works

---

## Finding Templates

### Browse All Templates

```bash
# List all available templates (50+ available)
ggen marketplace search ""

# Or specifically filter
ggen marketplace search "*" --filter language=rust
ggen marketplace search "*" --filter language=typescript
ggen marketplace search "*" --filter category=api
ggen marketplace search "*" --filter status=production
```

### Search by Category

**Popular Categories:**
- `api` - REST/GraphQL APIs
- `data-model` - Type-safe data models
- `microservices` - Distributed systems
- `web` - Web applications
- `cli` - Command-line tools
- `database` - Database schemas
- `auth` - Authentication systems
- `academic` - Paper/research templates

```bash
# Search by category
ggen marketplace search "" --category api
ggen marketplace search "" --category microservices
```

### Search by Technology

```bash
# Language-specific templates
ggen marketplace search "rust" --language rust
ggen marketplace search "typescript" --language typescript
ggen marketplace search "python" --language python

# Framework-specific
ggen marketplace search "actix"        # Rust web framework
ggen marketplace search "express"      # Node.js web framework
ggen marketplace search "fastapi"      # Python web framework
```

### Featured Templates

**Most Popular (for production use):**

1. **rest-api-template** (v1.0.0)
   - Simple, well-documented REST API
   - Languages: Rust, TypeScript, Python
   - Best for: Learning, standard APIs

2. **advanced-rust-api-8020** (v2.1.3)
   - Production-grade Rust API
   - Features: Auth, logging, tracing, error handling
   - Best for: Production deployments

3. **graphql-api-rust** (v1.5.2)
   - GraphQL server template
   - Features: Subscriptions, authentication
   - Best for: GraphQL-first architectures

4. **microservices-architecture** (v2.0.0)
   - Multi-service templates
   - Features: Service discovery, event bus
   - Best for: Distributed systems

5. **multi-tenant-saas** (v1.2.1)
   - SaaS application template
   - Features: Tenancy, billing, auth
   - Best for: SaaS platforms

### Check Template Details

```bash
# Get detailed information about a template
ggen marketplace info rest-api-template

# Output:
# rest-api-template v1.0.0
# ─────────────────────────────────────
# Author: Sean Chatman
# License: MIT
# Repository: https://github.com/seanchatmangpt/ggen-templates
#
# Description:
#   A simple, production-ready REST API template
#   supporting Rust, TypeScript, and Python
#
# Supported Languages: Rust, TypeScript, Python, Go
# Supported Databases: PostgreSQL, MySQL, MongoDB
# Features:
#   - CRUD operations
#   - Error handling
#   - Logging
#   - Testing utilities
#
# Maturity: Production Ready (4.2/5 stars)
# Downloads: 523 (this week)
# Updated: 2024-11-10
#
# Installation:
#   ggen marketplace install rest-api-template
#
# Documentation: https://marketplace.ggen.dev/rest-api-template
```

### Filter by Maturity

Production-ready only:

```bash
# Only show production-ready templates
ggen marketplace search "" --min-maturity production

# Maturity levels:
# - prototype: Early experimental stage
# - alpha: Testing in progress
# - beta: Feature complete, bugs possible
# - stable: Production ready, mature
# - production: Battle-tested in production

ggen marketplace search "" --min-maturity beta    # Beta or better
ggen marketplace search "" --min-maturity stable  # Stable or production
```

---

## Installing Templates

### Basic Installation

```bash
# Install a single template
ggen marketplace install rest-api-template

# Specific version
ggen marketplace install rest-api-template@1.0.0

# Latest version
ggen marketplace install rest-api-template@latest
```

### View Installation Details

```bash
# What was installed?
ggen marketplace list

# Output:
# Installed Templates:
# ├─ rest-api-template (v1.0.0)
# │  Status: Ready ✓
# │  Location: ~/.ggen/templates/rest-api-template
# │  Size: 245 KB
# │  Installed: 2024-11-17
# │  Used: 3 times
# └─ graphql-api-rust (v1.5.2)
```

### Install with Dependencies

Templates can depend on other templates:

```bash
# Dependencies are automatically resolved and installed
ggen marketplace install advanced-microservices

# Output will show:
# Installing advanced-microservices (v2.0.0)...
# ✓ Resolving dependencies...
# ✓ Installing: base-api-template (v1.0.0)
# ✓ Installing: auth-template (v1.2.0)
# ✓ Installing: advanced-microservices (v2.0.0)
# ✓ All dependencies installed
```

### Install a Bundle

Install multiple related templates at once:

```bash
# List available bundles
ggen marketplace list-bundles

# Output:
# Web Development Bundle
#   - rest-api-template
#   - react-frontend-template
#   - database-migrations-template
#
# Install entire bundle
ggen marketplace install-bundle web-development

# All 3 templates installed together!
```

---

## Using Templates

### Generate Code from Template

```bash
# Basic usage
ggen generate \
  --template rest-api-template \
  --domain domain.ttl \
  --language rust \
  --output ./my-api

# Options:
# --template NAME       Which template to use
# --domain FILE        Your RDF ontology
# --language LANG      Output language (rust/typescript/python/go)
# --output DIR         Where to generate code
# --config FILE        Template configuration file
```

### Customize Generation

Templates accept configuration:

```bash
# Create a configuration file
cat > template-config.toml << 'EOF'
[api]
include_auth = true           # Add authentication
include_logging = true        # Add logging
include_testing = true        # Include tests
validation = "strict"         # Type validation level

[server]
port = 8080
host = "0.0.0.0"
timeout_seconds = 30

[database]
type = "postgresql"           # postgres/mysql/mongodb
migrations = true
EOF

# Use the config
ggen generate \
  --template rest-api-template \
  --domain domain.ttl \
  --language rust \
  --config template-config.toml \
  --output ./my-api
```

### Template Parameters

Check what parameters a template accepts:

```bash
# See template documentation
ggen marketplace info rest-api-template

# Look for "Configuration Options" section
# Or read the template's README.md
cat ~/.ggen/templates/rest-api-template/README.md
```

### Common Configurations

**Minimal API (lean, fast):**
```toml
[api]
include_auth = false
include_logging = false
include_testing = false
```

**Production API (full-featured):**
```toml
[api]
include_auth = true
include_logging = true
include_testing = true
include_monitoring = true
include_tracing = true

[server]
timeout_seconds = 60
keep_alive_seconds = 30

[database]
connection_pooling = true
max_connections = 100
migrations = true
```

---

## Publishing Your Template

### Create a Template

**Step 1: Create Directory Structure**

```bash
# Create your template
mkdir my-template
cd my-template

# Create template structure
mkdir -p src/templates src/ontology

# Create source files
touch README.md package.toml Makefile.toml
```

**Step 2: Add Template Files**

```bash
# src/templates/ - Your Jinja2 template files
cat > src/templates/main.rs.jinja2 << 'EOF'
{% for entity in entities %}
pub struct {{ entity.name | capitalize }} {
    {% for field in entity.fields %}
    pub {{ field.name }}: {{ field.type }},
    {% endfor %}
}
{% endfor %}
EOF

# src/ontology/ - Example ontology
cat > src/ontology/example.ttl << 'EOF'
@prefix ex: <http://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User
  rdfs:label "User" ;
  ex:hasProperty [ rdfs:label "id" ; ex:type ex:Integer ] .
EOF
```

**Step 3: Create Metadata (package.toml)**

```toml
[package]
name = "my-rest-api-template"
version = "1.0.0"
description = "My custom REST API template"
author = "Your Name <your.email@example.com>"
license = "MIT"
repository = "https://github.com/yourname/my-rest-api-template"

[metadata]
category = "api"
tags = ["rest", "api", "rust"]
min_ggen_version = "3.0.0"
production_ready = false  # Set to true once tested

[dependencies]
# Templates can depend on other templates
# base-api-template = "^1.0.0"
```

**Step 4: Add Documentation (README.md)**

```markdown
# My REST API Template

A REST API template for ggen.

## Usage

```bash
ggen generate \
  --template my-rest-api-template \
  --domain domain.ttl \
  --language rust
```

## Configuration

Options in template-config.toml:
- `api.include_auth` - Enable authentication
- `api.include_logging` - Enable logging
- etc.

## Entities

This template generates CRUD operations for these entities:
- User
- Post
```

### Validate Your Template

```bash
# Check your template is valid
ggen marketplace validate --package my-rest-api-template

# Output shows:
# ✓ package.toml valid
# ✓ README.md found
# ✓ Templates found (3 files)
# ✓ Example ontology found
# ✓ All dependencies available
#
# Validation Score: 92/100
# Status: Ready for beta testing
```

### Register Your Template

**Option 1: Local Testing**

```bash
# Test locally before publishing
ggen marketplace install ./my-template --local

# Use locally while testing
ggen generate --template my-rest-api-template ...

# Make changes, test, repeat
```

**Option 2: Publish to Marketplace**

```bash
# Once tested and validated:

# Step 1: Create GitHub repo
# https://github.com/new
# Repository name: ggen-template-my-rest-api
# Add your template code

# Step 2: Push to GitHub
cd my-template
git init
git add .
git commit -m "Add my REST API template"
git branch -M main
git remote add origin https://github.com/yourname/ggen-template-my-rest-api.git
git push -u origin main

# Step 3: Publish via ggen
ggen marketplace publish \
  --name my-rest-api-template \
  --version 1.0.0 \
  --github-url https://github.com/yourname/ggen-template-my-rest-api

# Output:
# Publishing template...
# ✓ Pushed to marketplace registry
# ✓ Added to https://marketplace.ggen.dev
# ✓ Available for install in 5 minutes
```

### Update Your Template

```bash
# Update version in package.toml
# version = "1.1.0"

# Test changes
ggen marketplace validate --package my-rest-api-template

# Publish update
ggen marketplace publish \
  --name my-rest-api-template \
  --version 1.1.0 \
  --github-url https://github.com/yourname/ggen-template-my-rest-api
```

---

## Troubleshooting

### Problem: "Template not found"

```bash
# Error:
# Error: Template 'rest-api-template' not found

# Solution:
# 1. Is it installed?
ggen marketplace list

# 2. Install it
ggen marketplace install rest-api-template

# 3. Check spelling
ggen marketplace search "rest api"  # See correct name
```

### Problem: "Generate command fails"

```bash
# Error:
# Error: Failed to generate from template

# Solutions:
# 1. Check template is installed
ggen marketplace list

# 2. Check domain ontology is valid
# Try a simple example first
cat > simple.ttl << 'EOF'
@prefix ex: <http://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User rdfs:label "User" .
EOF

ggen generate \
  --template rest-api-template \
  --domain simple.ttl \
  --language rust \
  --output ./test

# 3. Check template supports your language
ggen marketplace info rest-api-template | grep "Languages"

# 4. Check template version compatibility
ggen marketplace info rest-api-template | grep "Requires ggen"
```

### Problem: "Dependency resolution failed"

```bash
# Error:
# Error: Could not resolve dependencies

# Solution:
# 1. Check your internet connection
ping api.github.com

# 2. Try installing template directly
ggen marketplace install rest-api-template

# 3. Check if template dependencies exist
ggen marketplace info rest-api-template | grep -A 5 "Dependencies"

# 4. Report issue on GitHub
# https://github.com/seanchatmangpt/ggen/issues
```

### Problem: "Installation permission denied"

```bash
# Error:
# Error: Permission denied writing to ~/.ggen/templates

# Solutions:
# Option 1: Fix directory permissions
sudo chown -R $USER ~/.ggen

# Option 2: Install to different location
mkdir ~/my-templates
ggen marketplace install rest-api-template --path ~/my-templates
export GGEN_TEMPLATE_PATH=~/my-templates

# Option 3: Use Docker (no local install issues)
docker run -it seanchatmangpt/ggen marketplace install rest-api-template
```

---

## Next Steps

**Now that you understand templates:**

1. **[Configure LLM](../how-to-guides/configure-llm.md)** - Use AI to generate ontologies
2. **[Create Your Own Template](../how-to-guides/create-templates.md)** - Build reusable templates for your team
3. **[Deploy to Production](../how-to-guides/deploy-production.md)** - Use generated code in real projects
4. **[Browse Official Templates](https://marketplace.ggen.dev)** - See all available templates

---

## Summary

| Task | Command |
|------|---------|
| Find templates | `ggen marketplace search "rest api"` |
| Install template | `ggen marketplace install rest-api-template` |
| List installed | `ggen marketplace list` |
| Get info | `ggen marketplace info rest-api-template` |
| Generate code | `ggen generate --template rest-api-template --domain domain.ttl --language rust` |
| Validate custom | `ggen marketplace validate --package my-template` |
| Publish template | `ggen marketplace publish --name my-template --version 1.0.0` |

---

**Questions?** See the full [Marketplace Documentation](../explanations/marketplace.md) or file an [issue on GitHub](https://github.com/seanchatmangpt/ggen/issues).
