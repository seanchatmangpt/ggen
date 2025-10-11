<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Claude Code Integration Guide - ggen-mcp](#claude-code-integration-guide---ggen-mcp)
  - [🚀 Quick Start (2 Minutes)](#-quick-start-2-minutes)
  - [📋 Prerequisites](#-prerequisites)
  - [🔧 Manual Connection (5 Minutes)](#-manual-connection-5-minutes)
    - [Step 1: Build ggen-mcp](#step-1-build-ggen-mcp)
    - [Step 2: Add to Claude Code](#step-2-add-to-claude-code)
    - [Step 3: Test Connection](#step-3-test-connection)
  - [🛠️ Available MCP Tools (27 Total)](#-available-mcp-tools-27-total)
    - [AI-Powered Code Generation (7 tools)](#ai-powered-code-generation-7-tools)
      - [`ai_generate_template`](#ai_generate_template)
      - [`ai_generate_sparql`](#ai_generate_sparql)
      - [`ai_generate_ontology`](#ai_generate_ontology)
      - [`ai_generate_project`](#ai_generate_project)
      - [`ai_extend_graph`](#ai_extend_graph)
      - [`ai_validate_and_improve`](#ai_validate_and_improve)
      - [`ai_list_providers`](#ai_list_providers)
    - [Project Management (4 tools)](#project-management-4-tools)
      - [`project_gen`](#project_gen)
      - [`project_plan`](#project_plan)
      - [`project_apply`](#project_apply)
      - [`project_diff`](#project_diff)
    - [Marketplace (8 tools)](#marketplace-8-tools)
      - [`market_list`](#market_list)
      - [`market_search`](#market_search)
      - [`market_install`](#market_install)
      - [`market_recommend`](#market_recommend)
      - [`market_info`](#market_info)
      - [`market_offline_search`](#market_offline_search)
      - [`market_cache_status`](#market_cache_status)
      - [`market_sync`](#market_sync)
    - [Graph Operations (3 tools)](#graph-operations-3-tools)
      - [`graph_query`](#graph_query)
      - [`graph_load`](#graph_load)
      - [`graph_export`](#graph_export)
    - [Template Tools (2 tools)](#template-tools-2-tools)
      - [`template_create`](#template_create)
      - [`template_validate`](#template_validate)
    - [Hooks (1 tool)](#hooks-1-tool)
      - [`hook_register`](#hook_register)
  - [💡 Best Practices](#-best-practices)
    - [1. Start with Simple Queries](#1-start-with-simple-queries)
    - [2. Provide Context](#2-provide-context)
    - [3. Iterate Incrementally](#3-iterate-incrementally)
    - [4. Leverage AI Generation](#4-leverage-ai-generation)
  - [🔍 Troubleshooting](#-troubleshooting)
    - [Connection Issues](#connection-issues)
    - [Compilation Errors](#compilation-errors)
    - [MCP Server Not Responding](#mcp-server-not-responding)
    - [Missing Tools](#missing-tools)
  - [🚀 Advanced Usage](#-advanced-usage)
    - [Combine Multiple Tools](#combine-multiple-tools)
    - [Workflow Automation](#workflow-automation)
    - [Knowledge Graph Development](#knowledge-graph-development)
  - [📊 Performance](#-performance)
  - [🔐 Security](#-security)
  - [📈 Monitoring](#-monitoring)
  - [🎯 80/20 Success Metrics](#-8020-success-metrics)
  - [📚 Additional Resources](#-additional-resources)
  - [🤝 Contributing](#-contributing)
  - [📝 License](#-license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Claude Code Integration Guide - ggen-mcp

Complete guide for connecting ggen-mcp to Claude Code using the 80/20 principle: maximum value with minimum friction.

## 🚀 Quick Start (2 Minutes)

```bash
# 1. Run the connection script
./ggen-mcp/scripts/connect-claude-code.sh

# 2. Start using in Claude Code
# "Use ggen to generate a Rust REST API template"
```

## 📋 Prerequisites

- ✅ Rust 1.70+ installed
- ✅ Claude Code CLI configured
- ✅ ggen repository cloned

## 🔧 Manual Connection (5 Minutes)

If the automated script doesn't work:

### Step 1: Build ggen-mcp

```bash
cd /path/to/ggen
cargo build --package ggen-mcp --release
```

### Step 2: Add to Claude Code

```bash
# Add ggen-mcp to Claude Code MCP servers
claude mcp add ggen /path/to/ggen/target/release/ggen-mcp

# Verify connection
claude mcp list
```

### Step 3: Test Connection

Open Claude Code and try:
```
Use ggen to list available marketplace templates
```

## 🛠️ Available MCP Tools (27 Total)

### AI-Powered Code Generation (7 tools)

#### `ai_generate_template`
Generate templates from natural language descriptions.

**Example:**
```
Use ggen to generate a template for a TypeScript React component with hooks
```

**Parameters:**
- `description` (string): Natural language description
- `examples` (array): Optional example patterns
- `language` (string): Target language

#### `ai_generate_sparql`
Generate SPARQL queries from natural language intent.

**Example:**
```
Use ggen to create a SPARQL query that finds all people over 30
```

**Parameters:**
- `intent` (string): Natural language query intent
- `graph_context` (string): Optional graph schema context
- `prefixes` (object): Optional namespace prefixes

#### `ai_generate_ontology`
Generate RDF ontologies from domain descriptions.

**Example:**
```
Use ggen to create an ontology for an e-commerce domain with products, orders, and customers
```

**Parameters:**
- `domain` (string): Domain description
- `requirements` (array): Specific requirements
- `format` (string): Output format (turtle, rdf/xml)

#### `ai_generate_project`
Generate complete project structures.

**Example:**
```
Use ggen to generate a full-stack TypeScript project with React frontend and Express backend
```

**Parameters:**
- `description` (string): Project description
- `language` (string): Primary language
- `framework` (string): Framework/stack
- `features` (array): Required features

#### `ai_extend_graph`
Extend existing knowledge graphs with new information.

**Example:**
```
Use ggen to extend the knowledge graph with employee salary information
```

**Parameters:**
- `graph_file` (string): Path to existing graph
- `requirements` (string): What to add
- `context` (string): Optional context

#### `ai_validate_and_improve`
Validate and suggest improvements for code/templates.

**Example:**
```
Use ggen to validate and improve this template: [paste template]
```

**Parameters:**
- `content` (string): Content to validate
- `content_type` (string): Type (template, code, sparql)
- `criteria` (array): Validation criteria

#### `ai_list_providers`
List available AI providers and their capabilities.

**Example:**
```
Use ggen to list all available AI providers
```

### Project Management (4 tools)

#### `project_gen`
Generate files from templates with variables.

**Example:**
```
Use ggen to generate files from the rest-api template with name=UserService
```

**Parameters:**
- `template` (string): Template name or path
- `variables` (object): Template variables
- `output_dir` (string): Output directory

#### `project_plan`
Create execution plan without applying changes.

**Example:**
```
Use ggen to create a plan for generating a React app
```

**Parameters:**
- `template` (string): Template to use
- `variables` (object): Variables
- `output_dir` (string): Target directory

#### `project_apply`
Apply a previously created execution plan.

**Example:**
```
Use ggen to apply the execution plan from plan.json
```

**Parameters:**
- `plan_file` (string): Path to plan file

#### `project_diff`
Show differences between template output and existing files.

**Example:**
```
Use ggen to show diff for rest-api template in ./src
```

**Parameters:**
- `template` (string): Template name
- `target_dir` (string): Directory to compare

### Marketplace (8 tools)

#### `market_list`
List all available marketplace templates.

**Example:**
```
Use ggen to list all marketplace templates
```

**Parameters:**
- `category` (string): Optional category filter
- `limit` (number): Max results (default: 50)

#### `market_search`
Search marketplace by query.

**Example:**
```
Use ggen to search marketplace for React TypeScript templates
```

**Parameters:**
- `query` (string): Search terms
- `filters` (object): Optional filters

#### `market_install`
Install a template from marketplace.

**Example:**
```
Use ggen to install the react-typescript-component template
```

**Parameters:**
- `package_name` (string): Package to install
- `version` (string): Optional version

#### `market_recommend`
Get personalized package recommendations.

**Example:**
```
Use ggen to recommend templates based on my installed packages
```

**Parameters:**
- `context` (object): User context and preferences
- `installed` (array): Currently installed packages

#### `market_info`
Get detailed package information.

**Example:**
```
Use ggen to get info about the rest-api-express package
```

**Parameters:**
- `package_name` (string): Package name

#### `market_offline_search`
Search using cached offline data.

**Example:**
```
Use ggen to search cached marketplace for Python templates
```

**Parameters:**
- `query` (string): Search query
- `use_cache_only` (boolean): Only use cache

#### `market_cache_status`
Get marketplace cache statistics.

**Example:**
```
Use ggen to check marketplace cache status
```

#### `market_sync`
Synchronize local cache with remote marketplace.

**Example:**
```
Use ggen to sync marketplace cache
```

**Parameters:**
- `force` (boolean): Force full sync

### Graph Operations (3 tools)

#### `graph_query`
Execute SPARQL queries against RDF graphs.

**Example:**
```
Use ggen to query the knowledge graph: SELECT ?name WHERE { ?person foaf:name ?name }
```

**Parameters:**
- `query` (string): SPARQL query
- `graph_file` (string): Optional graph file
- `format` (string): Output format

#### `graph_load`
Load RDF data into graph store.

**Example:**
```
Use ggen to load RDF data from ontology.ttl
```

**Parameters:**
- `file_path` (string): RDF file path
- `format` (string): RDF format (turtle, rdf/xml)

#### `graph_export`
Export RDF graph to file.

**Example:**
```
Use ggen to export the graph to output.ttl in turtle format
```

**Parameters:**
- `output_file` (string): Output path
- `format` (string): Export format

### Template Tools (2 tools)

#### `template_create`
Create a new template.

**Example:**
```
Use ggen to create a new template for Express routes
```

**Parameters:**
- `name` (string): Template name
- `content` (string): Template content
- `metadata` (object): Template metadata

#### `template_validate`
Validate template syntax and structure.

**Example:**
```
Use ggen to validate the template at ./templates/api.hbs
```

**Parameters:**
- `template_path` (string): Path to template

### Hooks (1 tool)

#### `hook_register`
Register lifecycle hooks for automation.

**Example:**
```
Use ggen to register a post-generate hook that runs npm install
```

**Parameters:**
- `hook_type` (string): Hook type (pre-gen, post-gen, etc.)
- `command` (string): Command to execute

## 💡 Best Practices

### 1. Start with Simple Queries
```
✅ Good: "Use ggen to list marketplace templates"
❌ Complex: "Use ggen to generate a microservices architecture with 12 services..."
```

### 2. Provide Context
```
✅ Good: "Use ggen to generate a REST API template for Node.js with Express"
❌ Vague: "Use ggen to make an API"
```

### 3. Iterate Incrementally
```
Step 1: "Use ggen to search for React templates"
Step 2: "Use ggen to install react-typescript-app"
Step 3: "Use ggen to generate from react-typescript-app with name=MyApp"
```

### 4. Leverage AI Generation
```
✅ "Use ggen AI to generate a template for GraphQL resolvers"
✅ "Use ggen AI to create a SPARQL query for finding related entities"
✅ "Use ggen AI to extend the ontology with payment processing concepts"
```

## 🔍 Troubleshooting

### Connection Issues

**Problem:** `claude mcp list` doesn't show ggen

**Solution:**
```bash
# Re-add the server
claude mcp remove ggen
claude mcp add ggen $(pwd)/target/release/ggen-mcp

# Restart Claude Code
```

### Compilation Errors

**Problem:** Build fails with dependency errors

**Solution:**
```bash
# Clean and rebuild
cargo clean
cargo build --package ggen-mcp --release

# If still failing, update dependencies
cargo update
```

### MCP Server Not Responding

**Problem:** Tools return errors or timeout

**Solution:**
```bash
# Check server logs
GGEN_MCP_LOG=debug cargo run --package ggen-mcp

# Rebuild with fresh binary
cargo build --package ggen-mcp --release --force
```

### Missing Tools

**Problem:** Some tools don't appear in Claude Code

**Solution:**
```bash
# Verify server reports all tools
./target/release/ggen-mcp --list-tools

# Check Claude Code tool registry
claude mcp tools ggen
```

## 🚀 Advanced Usage

### Combine Multiple Tools

```
1. Use ggen to search marketplace for React templates
2. Use ggen to install react-typescript-component
3. Use ggen to generate Button component from template
4. Use ggen AI to validate and improve the generated code
```

### Workflow Automation

```
# Create a workflow with hooks
1. Use ggen to register a post-generate hook: npm install
2. Use ggen to register a post-generate hook: npm run format
3. Use ggen to generate project from template
   (hooks run automatically)
```

### Knowledge Graph Development

```
# Iterative graph development
1. Use ggen AI to generate initial ontology for domain
2. Use ggen to load ontology into graph store
3. Use ggen AI to extend graph with additional entities
4. Use ggen to query graph for validation
5. Use ggen to export final graph
```

## 📊 Performance

- **Cold start:** ~500ms (first tool call)
- **Subsequent calls:** ~50-200ms
- **AI generation:** 2-10s (depends on provider)
- **Graph operations:** 100-500ms (depends on graph size)
- **Marketplace operations:** 50-100ms (cached), 1-2s (remote)

## 🔐 Security

- **API Keys:** ggen-mcp never logs or stores API keys
- **File Access:** Limited to workspace directory
- **Network:** Only connects to configured AI providers
- **Sandboxing:** Runs in isolated process with limited permissions

## 📈 Monitoring

Enable detailed logging:

```bash
# Set log level
export GGEN_MCP_LOG=debug

# Run with logging
./target/release/ggen-mcp
```

Log levels:
- `error` - Only errors
- `warn` - Warnings and errors
- `info` - Normal operations (default)
- `debug` - Detailed debugging
- `trace` - Very verbose

## 🎯 80/20 Success Metrics

**20% of tools deliver 80% of value:**

1. **`ai_generate_template`** - Most versatile AI generation
2. **`market_search`** - Discover existing templates
3. **`project_gen`** - Core file generation
4. **`ai_validate_and_improve`** - Quality assurance
5. **`graph_query`** - Knowledge graph queries

Start with these 5 tools for maximum productivity!

## 📚 Additional Resources

- [MCP Server Architecture](./MCP_SERVER.md)
- [Complete Tool Reference](./MCP_USAGE_GUIDE.md)
- [Template Development](../examples/README.md)
- [Graph Operations](./GRAPH_OPERATIONS.md)

## 🤝 Contributing

Found issues or improvements?

1. Open issue: https://github.com/seanchatmangpt/ggen/issues
2. Submit PR with fix
3. Update documentation

## 📝 License

MIT License - See LICENSE file for details

---

**Built with ❤️ using Rust, RDF, SPARQL, and the Model Context Protocol**
