# ggen CLI Patterns Research

**Research Date:** 2025-10-11
**Status:** Comprehensive Analysis
**Sources:** Validation scripts, MCP examples, existing documentation, CLI help outputs

## Executive Summary

This research documents ggen's complete CLI structure, command relationships, template patterns, and integration capabilities. ggen is a **deterministic, language-agnostic code generation framework** that treats software artifacts as projections of knowledge graphs (RDF/SPARQL).

**Key Findings:**
- **11 primary command groups** with 40+ subcommands
- **YAML frontmatter** as the universal template format
- **RDF/SPARQL integration** for semantic code generation
- **MCP protocol** for AI assistant integration (Claude, Cline)
- **Ultrathink swarm intelligence** for autonomous development
- **Deterministic generation** via seed control

---

## 1. Command Taxonomy

### 1.1 Core Command Structure

```bash
ggen [OPTIONS] <COMMAND>

Options:
  -c, --config <FILE>          Configuration file
      --manifest-path <PATH>   Path to ggen.toml manifest
  -d, --debug <DEBUG>          Enable debug mode
  -l, --log-level <LOG_LEVEL>  Set log verbosity
```

### 1.2 Command Groups

#### A. **AI Commands** (`ggen ai`)
AI-powered template generation and analysis using Ollama/genai.

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `generate` | Generate templates from descriptions | `--description`, `--mock`, `--output` | `ggen ai generate --description "REST API" --output api.tmpl` |
| `validate` | Validate template structure | `--template` | `ggen ai validate --template user.tmpl` |
| `from-source` | Generate template from source | `--source-file`, `--output`, `--mock` | `ggen ai from-source --source-file config.rs --output template.tmpl` |
| `project` | Generate complete projects | `--description`, `--name`, `--language` | `ggen ai project --description "CLI tool" --name mycli` |
| `frontmatter` | Generate frontmatter YAML | `--description`, `--yaml`, `--rdf`, `--sparql` | `ggen ai frontmatter --description "User model" --yaml` |
| `sparql` | Generate SPARQL queries | `--description`, `--domain` | `ggen ai sparql --description "Get all users"` |
| `graph` | Generate RDF graphs | `--description`, `--output` | `ggen ai graph --description "E-commerce ontology"` |
| `models` | List available AI models | | `ggen ai models` |
| `demo` | Run AI demo | | `ggen ai demo` |
| `server` | Start MCP server | | `ggen ai server` |

**Key Pattern:** AI commands use `--mock` flag for testing without real AI models.

#### B. **Project Commands** (`ggen project`)
Project scaffolding and generation.

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `gen` | Generate from template | `--template`, `--var`, `--out` | `ggen project gen user.tmpl --var name=User --out src/` |
| `init` | Initialize new project | `--name`, `--template` | `ggen project init --name myproject` |
| `status` | Show project status | | `ggen project status` |

**Key Pattern:** Uses `--var key=value` for template variable substitution.

#### C. **Template Commands** (`ggen template`)
Template management and operations.

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `list` | List templates | `--category`, `--language` | `ggen template list --language rust` |
| `validate` | Validate template | `--template` | `ggen template validate user.tmpl` |
| `render` | Render template | `--template`, `--vars` | `ggen template render api.tmpl --vars '{"name":"User"}'` |
| `create` | Create new template | `--name`, `--type` | `ggen template create --name mytemplate` |

**Key Pattern:** Templates use YAML frontmatter with `to:` and `vars:` fields.

#### D. **Market Commands** (`ggen market`)
Marketplace operations for gpacks (ggen packages).

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `search` | Search marketplace | `--query`, `--category`, `--limit` | `ggen market search "rust cli" --limit 5` |
| `add` | Install package | `--id` | `ggen market add "io.ggen.rust.cli-subcommand"` |
| `list` | List installed | | `ggen market list` |
| `publish` | Publish package | `--project`, `--metadata` | `ggen market publish --project ./mypack` |
| `info` | Show package details | `--id` | `ggen market info "io.ggen.auth.user"` |

**Key Pattern:** Packages use reverse-DNS naming (io.ggen.category.name).

#### E. **Graph Commands** (`ggen graph`)
RDF graph operations (semantic layer).

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `load` | Load RDF file | `--file`, `--format` | `ggen graph load ontology.ttl --format turtle` |
| `query` | Execute SPARQL | `--query` | `ggen graph query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"` |
| `add-triple` | Add RDF triple | `--subject`, `--predicate`, `--object` | `ggen graph add-triple --subject ex:User --predicate rdf:type --object owl:Class` |
| `validate` | Validate graph | `--graph` | `ggen graph validate` |
| `export` | Export graph | `--format`, `--output` | `ggen graph export --format turtle --output out.ttl` |

**Key Pattern:** Uses RDF for semantic relationships, SPARQL for queries.

#### F. **Ultrathink Commands** (`ggen ultrathink`)
Swarm intelligence for autonomous development.

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `start` | Initialize swarm | `--topology`, `--max-agents` | `ggen ultrathink start --topology mesh` |
| `task` | Submit task | `--description`, `--task-type`, `--priority` | `ggen ultrathink task --description "Implement auth" --task-type code-generation --priority high` |
| `status` | Show swarm status | | `ggen ultrathink status` |
| `sync` | Sync with WIP | | `ggen ultrathink sync` |
| `intelligence` | Show metrics | | `ggen ultrathink intelligence` |
| `demo` | Demo capabilities | | `ggen ultrathink demo` |

**Key Pattern:** Tasks return UUIDs, status shows swarm metrics.

#### G. **Autonomous Commands** (`ggen autonomous`)
Autonomous graph evolution and regeneration.

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `status` | System status | | `ggen autonomous status` |
| `evolve` | Evolve graph | `--iterations` | `ggen autonomous evolve --iterations 10` |
| `regenerate` | Regenerate code | `--target` | `ggen autonomous regenerate --target src/` |

**Key Pattern:** Autonomous systems learn from feedback.

#### H. **Hook Commands** (`ggen hook`)
Knowledge hooks for autonomic regeneration.

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `register` | Register hook | `--name`, `--trigger` | `ggen hook register --name on-edit --trigger file-change` |
| `execute` | Execute hook | `--hook-id` | `ggen hook execute --hook-id abc123` |
| `list` | List hooks | | `ggen hook list` |

**Key Pattern:** Hooks trigger on events (file changes, errors, etc.).

#### I. **Swarm Commands** (`ggen swarm`)
Ultrathink swarm orchestration.

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `init` | Initialize swarm | `--topology` | `ggen swarm init --topology hierarchical` |
| `agent` | Manage agents | `--action`, `--type` | `ggen swarm agent --action spawn --type coder` |
| `orchestrate` | Orchestrate task | `--task` | `ggen swarm orchestrate --task "Build API"` |

**Key Pattern:** Supports hierarchical, mesh, ring, star topologies.

#### J. **CI/CD Commands** (`ggen ci`)
CI/CD operations and GitHub integration.

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `github` | GitHub operations | `--action`, `--repo` | `ggen ci github --action analyze --repo owner/repo` |
| `validate` | Validate CI config | `--config` | `ggen ci validate --config .github/workflows/ci.yml` |
| `generate` | Generate CI workflow | `--language`, `--output` | `ggen ci generate --language rust --output .github/workflows/ci.yml` |

**Key Pattern:** Integrates with GitHub Actions.

#### K. **Audit Commands** (`ggen audit`)
Security and performance auditing.

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `security` | Security scan | `--target` | `ggen audit security --target src/` |
| `performance` | Performance analysis | `--target` | `ggen audit performance --target src/` |
| `dependencies` | Dependency audit | | `ggen audit dependencies` |

**Key Pattern:** Provides actionable security/performance reports.

#### L. **Shell Commands** (`ggen shell`)
Shell integration and completion.

| Command | Purpose | Key Flags | Example |
|---------|---------|-----------|---------|
| `completion` | Generate completions | `--shell` | `ggen shell completion --shell bash > completion.bash` |
| `init` | Initialize shell | | `ggen shell init` |

**Key Pattern:** Supports bash, zsh, fish, powershell.

---

## 2. Template Patterns

### 2.1 Frontmatter Structure

**Universal Format: YAML Frontmatter**

```yaml
---
to: "output/path/{{variable}}.rs"
vars:
  variable: "default_value"
  another_var: "value"
rdf: |
  @prefix ex: <http://example.org/> .
  ex:Entity a owl:Class .
sparql: "SELECT ?name WHERE { ?entity ex:name ?name }"
determinism: true
seed: 42
---
Template content goes here
Use {{variable}} for substitution
```

### 2.2 Frontmatter Fields

| Field | Type | Required | Purpose | Example |
|-------|------|----------|---------|---------|
| `to` | string | Yes | Output path with variable interpolation | `"src/{{name}}.rs"` |
| `vars` | object/array | Yes | Template variables | `{name: "User"}` or `[{name: "string"}]` |
| `rdf` | string | No | RDF ontology in Turtle format | See ontology examples |
| `sparql` | string | No | SPARQL query for data | `"SELECT ?x WHERE { ?x a :Class }"` |
| `determinism` | boolean | No | Enable deterministic output | `true` |
| `seed` | integer | No | Random seed for determinism | `42` |
| `inject_marker` | string | No | Marker for idempotent injection | `"// [INJECTED]"` |
| `skip_if_exists` | boolean | No | Skip if output exists | `true` |

### 2.3 Template Examples

#### A. Simple Rust Model

```yaml
---
to: "src/models/{{name}}.rs"
vars:
  name: "user"
  author: "ggen"
---
//! {{name | capitalize}} model
//! Generated by ggen
//! Author: {{author}}

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{name | capitalize}} {
    pub id: u64,
    pub name: String,
}
```

#### B. RDF-Driven Template

```yaml
---
to: "src/models/{{entity}}.rs"
vars:
  entity: "User"
rdf: |
  @prefix ex: <http://example.org/> .
  @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
  @prefix owl: <http://www.w3.org/2002/07/owl#> .

  ex:{{entity}} a owl:Class ;
      rdfs:label "{{entity}}" ;
      ex:hasField "id" ;
      ex:hasField "email" ;
      ex:hasField "name" .
sparql: "SELECT ?field WHERE { ex:{{entity}} ex:hasField ?field }"
determinism: true
seed: 42
---
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{entity}} {
    {{#each fields}}
    pub {{this}}: String,
    {{/each}}
}
```

#### C. Multi-Language Template

```yaml
---
to: "{{language}}/models/{{name}}.{{extension}}"
vars:
  name: "User"
  language: "rust"
  extension: "rs"
  fields:
    - "id"
    - "email"
    - "name"
---
{{#if (eq language "rust")}}
use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct {{name}} {
    {{#each fields}}
    pub {{this}}: String,
    {{/each}}
}
{{else if (eq language "python")}}
from dataclasses import dataclass

@dataclass
class {{name}}:
    {{#each fields}}
    {{this}}: str
    {{/each}}
{{else if (eq language "typescript")}}
export interface {{name}} {
  {{#each fields}}
  {{this}}: string;
  {{/each}}
}
{{/if}}
```

### 2.4 Variable Types

| Type | Syntax | Example | Use Case |
|------|--------|---------|----------|
| String | `{{name}}` | `{{name}}` | Simple substitution |
| Array | `{{#each items}}` | `{{#each fields}}{{this}}{{/each}}` | Iteration |
| Object | `{{object.key}}` | `{{user.email}}` | Nested access |
| Conditional | `{{#if condition}}` | `{{#if has_auth}}` | Conditional blocks |
| Filter | `{{name \| filter}}` | `{{name \| capitalize}}` | Value transformation |

### 2.5 Built-in Filters

- `capitalize` - Capitalize first letter
- `uppercase` - Convert to uppercase
- `lowercase` - Convert to lowercase
- `snake_case` - Convert to snake_case
- `camel_case` - Convert to camelCase
- `pascal_case` - Convert to PascalCase
- `kebab_case` - Convert to kebab-case

---

## 3. Workflow Patterns

### 3.1 Basic Generation Workflow

```bash
# 1. Create or find template
ggen template create --name user-model

# 2. Generate code
ggen project gen user-model.tmpl --var name=User --out src/models/

# 3. Validate output
ggen template validate src/models/user.rs
```

### 3.2 AI-Assisted Workflow

```bash
# 1. Generate template from description
ggen ai generate --description "REST API for user management" --output api.tmpl

# 2. Validate template
ggen ai validate --template api.tmpl

# 3. Generate project
ggen project gen api.tmpl --var resource=users --out src/
```

### 3.3 RDF-Driven Workflow

```bash
# 1. Load ontology
ggen graph load ontology.ttl --format turtle

# 2. Query entities
ggen graph query "SELECT ?entity WHERE { ?entity a owl:Class }"

# 3. Generate code from graph
ggen project gen rdf-model.tmpl --var entity=User --out src/models/

# 4. Export updated graph
ggen graph export --format turtle --output ontology-updated.ttl
```

### 3.4 Marketplace Workflow

```bash
# 1. Search marketplace
ggen market search "rust authentication" --limit 5

# 2. View package details
ggen market info "io.ggen.auth.user"

# 3. Install package
ggen market add "io.ggen.auth.user"

# 4. Use installed template
ggen project gen ~/.ggen/gpacks/io.ggen.auth.user/auth.tmpl --out src/
```

### 3.5 Ultrathink Swarm Workflow

```bash
# 1. Initialize swarm
ggen ultrathink start --topology mesh

# 2. Submit task
TASK_ID=$(ggen ultrathink task \
  --description "Implement user authentication with JWT" \
  --task-type code-generation \
  --priority high | grep "Task ID:" | cut -d' ' -f3)

# 3. Monitor status
ggen ultrathink status

# 4. Check intelligence metrics
ggen ultrathink intelligence
```

### 3.6 Idempotent Injection Workflow

```bash
# 1. Create injection template
cat > route-injection.tmpl << 'EOF'
---
to: "src/router.rs"
inject_marker: "// [ROUTE-INJECTION]"
vars:
  route_path: "/api/users"
  handler: "user_handlers::list"
---
.route("{{route_path}}", get({{handler}})) // [ROUTE-INJECTION]
EOF

# 2. Inject (safe, idempotent)
ggen project gen route-injection.tmpl --var route_path="/api/users" --var handler="user_handlers::list"

# 3. Re-run (skips duplicate)
ggen project gen route-injection.tmpl --var route_path="/api/users" --var handler="user_handlers::list"
# Output: Skipped (duplicate detected)
```

### 3.7 Complete Project Workflow

```bash
# 1. Generate complete project structure
ggen ai project \
  --description "REST API for e-commerce with authentication" \
  --name "shop-api" \
  --language rust \
  --framework actix-web \
  --tests \
  --docs \
  --ci \
  --output ./shop-api

# 2. Validate project
cd shop-api
ggen project status

# 3. Build and test
cargo build
cargo test

# 4. Publish to marketplace
ggen market publish --project . --category web --keywords "api,rest,ecommerce"
```

---

## 4. Integration Patterns

### 4.1 MCP Integration (Claude Desktop / Cline)

#### A. Configuration

**Claude Desktop** (`~/Library/Application Support/Claude/claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "ggen": {
      "command": "ggen",
      "args": ["mcp", "start"],
      "type": "stdio",
      "env": {
        "GGEN_HOME": "${HOME}/.ggen/templates",
        "RUST_LOG": "ggen=info"
      }
    }
  }
}
```

**Cline VSCode** (`.vscode/settings.json`):

```json
{
  "mcp.servers": {
    "ggen": {
      "command": "ggen",
      "args": ["mcp", "start"],
      "type": "stdio",
      "autoStart": true,
      "logFile": "${workspaceFolder}/.vscode/ggen-mcp.log"
    }
  }
}
```

#### B. MCP Tools Available

**40+ MCP tools** exposed via `ggen mcp start`:

**Template Operations:**
- `ggen_template_list` - List available templates
- `ggen_template_render` - Render template with vars
- `ggen_gen` - Generate from template
- `ggen_gen_with_vars` - Generate with variables
- `ggen_gen_batch` - Batch generation (parallel)
- `ggen_inject_idempotent` - Idempotent code injection

**Graph Operations:**
- `ggen_graph_load` - Load RDF file
- `ggen_graph_query` - Execute SPARQL query
- `ggen_graph_add_triple` - Add RDF triple
- `ggen_graph_validate` - Validate graph
- `ggen_graph_export` - Export graph

**Marketplace:**
- `ggen_market_search` - Search marketplace
- `ggen_market_install` - Install package
- `ggen_market_publish` - Publish package
- `ggen_market_info` - Get package info

**Project:**
- `ggen_project_init` - Initialize project
- `ggen_project_gen` - Generate from template
- `ggen_project_status` - Show project status

**Hook System:**
- `ggen_hook_register` - Register knowledge hook
- `ggen_hook_execute` - Execute hook
- `ggen_hook_list` - List hooks

#### C. Conversation Patterns

**Pattern 1: Direct Generation**
```
User: "Create a Rust auth module with JWT"

Claude:
[Uses ggen_market_search with query="rust jwt auth"]
Found: io.ggen.rust.auth-jwt

[Uses ggen_gen_with_vars]
Generated: src/auth.rs
```

**Pattern 2: RDF-Driven Generation**
```
User: "Generate REST API from my ontology.ttl"

Claude:
[Uses ggen_graph_load with file="ontology.ttl"]
Loaded 127 triples

[Uses ggen_graph_query to find entities]
Found: Product, Order, Customer

[Uses ggen_gen_batch to generate all models]
Generated: src/models/product.rs, order.rs, customer.rs
```

**Pattern 3: Idempotent Injection**
```
User: "Add a new route to my router without duplicates"

Claude:
[Uses ggen_inject_idempotent]
Injected: src/router.rs (line 42)
Duplicate check: Passed
```

### 4.2 AI Integration (genai)

#### A. Configuration

```bash
# Environment variables
export GENAI_MODEL="ollama:qwen3-coder:30b"
export GENAI_API_KEY="optional-api-key"
export OLLAMA_HOST="http://localhost:11434"
```

#### B. Supported Providers

- **Ollama** (default) - `ollama:model-name`
- **OpenAI** - `openai:gpt-4`
- **Anthropic** - `anthropic:claude-3-sonnet`
- **Google Gemini** - `gemini:gemini-pro`

#### C. AI Command Patterns

```bash
# Use specific model
ggen ai generate --model "ollama:qwen3-coder:30b" --description "API endpoint"

# Mock mode (no AI calls)
ggen ai generate --mock --description "User model"

# Validate with AI
ggen ai validate --template user.tmpl --explain

# Generate from source
ggen ai from-source --source-file user.rs --output user.tmpl --analyze
```

### 4.3 Ultrathink Integration

#### A. Swarm Topologies

| Topology | Use Case | Characteristics |
|----------|----------|-----------------|
| Mesh | Complex tasks, high collaboration | All agents interconnected |
| Hierarchical | Structured workflows | Leader-follower pattern |
| Ring | Pipeline processing | Sequential agent chain |
| Star | Centralized coordination | Hub-and-spoke model |

#### B. Task Types

- `code-generation` - Generate code
- `refactoring` - Refactor existing code
- `testing` - Generate tests
- `documentation` - Generate docs
- `analysis` - Analyze code/architecture

#### C. Priority Levels

- `low` - Background tasks
- `medium` - Normal priority
- `high` - Important tasks
- `critical` - Urgent tasks

### 4.4 Hook System Integration

#### A. Hook Types

| Hook Type | Trigger | Purpose |
|-----------|---------|---------|
| `pre-gen` | Before generation | Validation, preparation |
| `post-gen` | After generation | Formatting, testing |
| `on-error` | Error occurred | Recovery, logging |
| `on-edit` | File changed | Auto-regeneration |
| `on-commit` | Git commit | CI/CD integration |

#### B. Hook Configuration

```yaml
# .ggen/hooks.yaml
hooks:
  - name: "auto-format"
    type: "post-gen"
    command: "rustfmt --edition 2021"
    patterns: ["*.rs"]

  - name: "validate-rdf"
    type: "pre-gen"
    command: "rapper -i turtle -c"
    patterns: ["*.ttl"]

  - name: "run-tests"
    type: "post-gen"
    command: "cargo test"
    patterns: ["src/**/*.rs"]
```

---

## 5. Error Handling Patterns

### 5.1 Common Errors

#### A. Template Errors

**Missing Variable:**
```bash
$ ggen project gen user.tmpl --out src/
Error: Missing required variable 'name'
Solution: Add --var name=User
```

**Invalid Frontmatter:**
```bash
$ ggen ai validate --template broken.tmpl
Error: Invalid YAML frontmatter at line 3
  Missing 'to' field
Solution: Add 'to: "output/path"' to frontmatter
```

**File Exists:**
```bash
$ ggen project gen user.tmpl --var name=User --out src/
Error: File already exists: src/user.rs
Solution: Use --force or add skip_if_exists: true to frontmatter
```

#### B. Graph Errors

**Invalid RDF:**
```bash
$ ggen graph load broken.ttl
Error: Parse error at line 15: Invalid triple
Solution: Validate RDF syntax with rapper or another RDF validator
```

**SPARQL Syntax Error:**
```bash
$ ggen graph query "SELECT WHERE { ?s ?p ?o }"
Error: SPARQL syntax error: Missing variable after SELECT
Solution: Add variable: "SELECT ?s WHERE { ?s ?p ?o }"
```

#### C. Marketplace Errors

**Package Not Found:**
```bash
$ ggen market add "io.ggen.nonexistent"
Error: Package not found: io.ggen.nonexistent
Solution: Search marketplace: ggen market search "keyword"
```

**Authentication Failed:**
```bash
$ ggen market publish --project ./mypack
Error: Authentication required
Solution: Run: ggen market login
```

#### D. AI Errors

**Model Not Available:**
```bash
$ ggen ai generate --model "ollama:missing-model"
Error: Model not found: missing-model
Solution: Pull model: ollama pull qwen3-coder:30b
```

**API Key Missing:**
```bash
$ ggen ai generate --model "openai:gpt-4"
Error: OPENAI_API_KEY not set
Solution: export OPENAI_API_KEY="your-key"
```

### 5.2 Validation Patterns

#### A. Template Validation

```bash
# Validate structure
ggen ai validate --template user.tmpl

# Expected output:
Template Validation Report
========================
File: user.tmpl
Valid: true
Issues Found: 0

Frontmatter:
  ✓ 'to' field present
  ✓ 'vars' field present
  ✓ Valid YAML syntax

Template Body:
  ✓ No undefined variables
  ✓ Valid template syntax
```

#### B. Graph Validation

```bash
# Validate RDF graph
ggen graph validate

# Expected output:
Graph Validation Report
======================
Triples: 127
Valid: true
Issues: 0

Validation checks:
  ✓ All triples valid
  ✓ No dangling references
  ✓ Namespace prefixes defined
```

#### C. Project Validation

```bash
# Validate project structure
ggen project status

# Expected output:
Project Status
==============
Name: shop-api
Valid: true
Complete: 95%

Structure:
  ✓ Cargo.toml present
  ✓ src/ directory exists
  ✓ tests/ directory exists
  ⚠ Missing: docs/
```

### 5.3 Debug Patterns

```bash
# Enable debug mode
ggen --debug true ai generate --description "API"

# Set log level
ggen --log-level debug project gen user.tmpl

# Verbose output
export RUST_LOG=ggen=debug
ggen project gen user.tmpl
```

---

## 6. Best Practices

### 6.1 Template Design

1. **Use descriptive variable names**
   - ✅ `{{user_name}}`
   - ❌ `{{n}}`

2. **Enable determinism for reproducibility**
   ```yaml
   determinism: true
   seed: 42
   ```

3. **Include RDF for semantic relationships**
   ```yaml
   rdf: |
     @prefix ex: <http://example.org/> .
     ex:User a owl:Class .
   ```

4. **Use filters for consistency**
   ```yaml
   {{name | pascal_case}}  # Ensures PascalCase
   ```

5. **Add documentation in templates**
   ```yaml
   ---
   to: "src/{{name}}.rs"
   # This template generates Rust structs with Serde
   ---
   ```

### 6.2 Workflow Optimization

1. **Use batch generation for multiple files**
   ```bash
   ggen gen batch --templates "*.tmpl" --parallel
   ```

2. **Cache marketplace searches**
   ```bash
   ggen market search "rust" --cache
   ```

3. **Use marketplace packages instead of custom templates**
   ```bash
   ggen market add "io.ggen.rust.api" # vs. creating from scratch
   ```

4. **Leverage ultrathink for complex tasks**
   ```bash
   ggen ultrathink task --description "Complete CRUD API"
   ```

### 6.3 Integration Best Practices

1. **Use MCP for AI assistants** - Let Claude/Cline call ggen tools
2. **Use hooks for automation** - Auto-format, auto-test on generation
3. **Use RDF for complex relationships** - Multi-entity relationships
4. **Use deterministic seeds** - For reproducible builds

---

## 7. Frontmatter Generation Patterns

### 7.1 JSON to YAML Conversion

```rust
use serde_json::json;
use serde_yaml;

// Generate frontmatter as JSON
let frontmatter = json!({
    "to": "src/{{name}}.rs",
    "vars": {
        "name": "User",
        "fields": ["id", "email"]
    },
    "rdf": "@prefix ex: <http://example.org/> .",
    "determinism": true
});

// Convert to YAML
let yaml = serde_yaml::to_string(&frontmatter)?;
```

### 7.2 AI-Generated Frontmatter

```bash
# Generate frontmatter using AI
ggen ai frontmatter \
  --description "User management system with authentication" \
  --yaml \
  --rdf \
  --sparql \
  --output user-system.tmpl
```

Output:
```yaml
---
to: src/models/{{name}}.rs
vars:
  name: user
  fields:
    - id
    - email
    - password_hash
    - created_at
rdf: |
  @prefix ex: <http://example.org/> .
  ex:User a owl:Class ;
    ex:hasField "id" ;
    ex:hasField "email" ;
    ex:hasAuthentication true .
sparql: |
  SELECT ?field WHERE {
    ex:User ex:hasField ?field
  }
determinism: true
seed: 42
---
```

### 7.3 Template Types

#### User Management
```yaml
to: src/models/user.rs
vars:
  - name: string
  - email: string
  - role: string
rdf: |
  @prefix ex: <http://example.org/> .
  ex:User a owl:Class .
determinism: true
```

#### API Controller
```yaml
to: src/controllers/{{resource}}_controller.rs
vars:
  - resource: string
  - actions: array
rdf: |
  @prefix ex: <http://example.org/> .
  ex:Controller a owl:Class .
determinism: true
```

#### SPARQL Query
```yaml
to: queries/{{query_name}}.sparql
vars:
  - query_name: string
  - domain: string
sparql: |
  SELECT ?entity WHERE {
    ?entity a ex:{{domain | capitalize}}
  }
determinism: true
```

---

## 8. Example Usage Scenarios

### 8.1 Scenario: Generate Multi-Language Models

**Goal:** Generate User model in Rust, Python, TypeScript

```bash
# 1. Create RDF model
cat > user-ontology.ttl << 'EOF'
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User a owl:Class ;
  rdfs:label "User" ;
  ex:hasField "id" ;
  ex:hasField "email" ;
  ex:hasField "name" .
EOF

# 2. Load into graph
ggen graph load user-ontology.ttl

# 3. Generate for each language
ggen project gen rust-model.tmpl --var entity=User --out rust/src/models/
ggen project gen python-model.tmpl --var entity=User --out python/models/
ggen project gen ts-model.tmpl --var entity=User --out typescript/types/
```

### 8.2 Scenario: Publish Template Package

**Goal:** Create and publish a reusable template package

```bash
# 1. Create package structure
mkdir -p mypack/{templates,docs,examples}

# 2. Create template
cat > mypack/templates/api-endpoint.tmpl << 'EOF'
---
to: "src/routes/{{resource}}.rs"
vars:
  resource: "user"
---
use axum::{Router, routing::*};

pub fn {{resource}}_routes() -> Router {
    Router::new()
        .route("/{{resource}}", get(list_{{resource}}s))
        .route("/{{resource}}/:id", get(get_{{resource}}))
}
EOF

# 3. Create metadata
cat > mypack/ggen.toml << 'EOF'
[package]
name = "io.mycompany.rust.api-endpoint"
version = "1.0.0"
description = "Axum API endpoint template"
author = "Your Name"
license = "MIT"
keywords = ["rust", "axum", "api"]
categories = ["templates", "web"]
EOF

# 4. Publish
ggen market publish --project mypack
```

### 8.3 Scenario: Autonomous Development

**Goal:** Let Ultrathink generate complete feature

```bash
# 1. Initialize swarm
ggen ultrathink start --topology mesh --max-agents 5

# 2. Submit complex task
ggen ultrathink task \
  --description "Implement user authentication system with:
    - JWT tokens
    - Password hashing
    - Login/logout endpoints
    - Session management
    - Rate limiting" \
  --task-type code-generation \
  --priority high

# 3. Monitor progress
watch -n 5 'ggen ultrathink status'

# 4. Check results
ggen ultrathink intelligence
```

---

## 9. Advanced Integration Examples

### 9.1 MCP Client (Rust)

```rust
use rmcp::{Client, StdioTransport};
use serde_json::json;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let transport = StdioTransport::new("ggen", vec!["mcp", "start"]);
    let mut client = Client::new(transport).await?;

    // Search marketplace
    let results = client.call_tool(
        "ggen_market_search",
        json!({"query": "rust api", "limit": 5})
    ).await?;

    // Generate code
    let gen_result = client.call_tool(
        "ggen_gen_with_vars",
        json!({
            "template": "api.tmpl",
            "vars": {"resource": "users"},
            "output": "src/routes/users.rs"
        })
    ).await?;

    Ok(())
}
```

### 9.2 MCP Client (Python)

```python
import asyncio
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def main():
    server_params = StdioServerParameters(
        command="ggen",
        args=["mcp", "start"]
    )

    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()

            # Generate code
            result = await session.call_tool(
                "ggen_gen_with_vars",
                {
                    "template": "python-dataclass.tmpl",
                    "vars": {
                        "class_name": "Product",
                        "fields": {"id": "int", "name": "str"}
                    },
                    "output": "models/product.py"
                }
            )
            print(f"Generated: {result}")

asyncio.run(main())
```

### 9.3 HTTP API (curl)

```bash
# Start HTTP server
ggen mcp start --transport http --port 8080 &

# Call tools via HTTP
curl -X POST http://localhost:8080/tools/call \
  -H "Content-Type: application/json" \
  -d '{
    "name": "ggen_market_search",
    "arguments": {
      "query": "rust authentication",
      "limit": 5
    }
  }' | jq

# Batch operations
curl -X POST http://localhost:8080/tools/batch \
  -H "Content-Type: application/json" \
  -d '{
    "calls": [
      {
        "name": "ggen_graph_load",
        "arguments": {"file": "ontology.ttl"}
      },
      {
        "name": "ggen_graph_query",
        "arguments": {
          "query": "SELECT ?entity WHERE { ?entity a owl:Class }"
        }
      },
      {
        "name": "ggen_gen_batch",
        "arguments": {
          "template": "model.tmpl",
          "vars": {"entities": "{{sparql_results}}"}
        }
      }
    ]
  }' | jq
```

---

## 10. Summary & Recommendations

### Key Takeaways

1. **ggen is a deterministic, graph-aware code generator**
   - RDF/SPARQL for semantic relationships
   - YAML frontmatter for universal templates
   - Deterministic output via seed control

2. **11 command groups cover complete development lifecycle**
   - AI generation, project scaffolding, marketplace
   - Graph operations, hooks, swarm intelligence
   - CI/CD, auditing, shell integration

3. **MCP integration enables AI assistant workflows**
   - 40+ tools for Claude Desktop / Cline
   - stdio, HTTP, SSE transports supported
   - Batch operations for efficiency

4. **Ultrathink swarm intelligence for autonomous dev**
   - Mesh, hierarchical, ring, star topologies
   - Task orchestration with priorities
   - Self-evolving code generation

5. **Template patterns are consistent and flexible**
   - YAML frontmatter with `to:` and `vars:`
   - RDF/SPARQL for complex relationships
   - Filters for value transformation

### Recommended Examples to Create

Based on this research, the following examples would best demonstrate ggen capabilities:

1. **Basic Template Example** - Simple Rust struct with frontmatter
2. **RDF-Driven Example** - Multi-entity models from ontology
3. **Marketplace Example** - Search, install, use package
4. **MCP Integration Example** - Claude Desktop conversation
5. **Ultrathink Example** - Complex feature generation
6. **Idempotent Injection Example** - Safe code injection
7. **Multi-Language Example** - Same model in Rust/Python/TS
8. **Complete Project Example** - Full-stack app generation
9. **Hook System Example** - Auto-format/test on generation
10. **Batch Generation Example** - Parallel multi-file generation

---

**End of Research Document**

**Next Steps:**
1. Create example projects based on these patterns
2. Add integration tests for each workflow
3. Document error scenarios with solutions
4. Create video tutorials for key workflows
