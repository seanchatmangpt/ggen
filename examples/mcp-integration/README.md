# GGen MCP Integration Guide

Complete guide for integrating ggen-mcp with Claude Desktop and Cline to enable AI-assisted project generation, template management, and knowledge graph operations.

## Table of Contents

- [What is MCP?](#what-is-mcp)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Available Tools](#available-tools)
- [Example Conversations](#example-conversations)
- [Advanced Features](#advanced-features)
- [Troubleshooting](#troubleshooting)

## What is MCP?

**Model Context Protocol (MCP)** is an open protocol that enables AI models to securely interact with external tools and data sources. GGen's MCP server exposes 25+ tools that allow Claude to:

- Generate projects from templates
- Search and install marketplace packages
- Query and manipulate RDF knowledge graphs
- Create and validate templates
- Use AI-powered code generation
- Register lifecycle hooks

## Prerequisites

### Required Software

1. **Rust** (1.70+) - for building ggen-mcp
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. **GGen CLI** - installed and configured
   ```bash
   cargo install --path /path/to/ggen
   ```

3. **One of the following MCP clients:**
   - **Claude Desktop** (macOS/Windows) - [Download](https://claude.ai/download)
   - **Cline** (VS Code Extension) - [Install from VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=saoudrizwan.claude-dev)

### Environment Setup

Set your AI provider API key (required for AI tools):
```bash
export ANTHROPIC_API_KEY="your-api-key-here"
# or
export OPENAI_API_KEY="your-api-key-here"
```

## Installation

### Automated Installation

Run the included installation script:

```bash
cd examples/mcp-integration
./install-ggen-mcp.sh
```

This will:
1. Build ggen-mcp in release mode
2. Add ggen-mcp to your Claude Desktop or Cline configuration
3. Test the connection
4. List available tools

### Manual Installation

#### For Claude Desktop

1. **Build ggen-mcp:**
   ```bash
   cd /path/to/ggen/ggen-mcp
   cargo build --release
   ```

2. **Add to Claude Desktop config:**

   **macOS:** `~/Library/Application Support/Claude/claude_desktop_config.json`
   **Windows:** `%APPDATA%\Claude\claude_desktop_config.json`

   ```json
   {
     "mcpServers": {
       "ggen": {
         "command": "/path/to/ggen/target/release/ggen-mcp",
         "args": [],
         "env": {
           "ANTHROPIC_API_KEY": "your-api-key-here",
           "GGEN_MCP_LOG": "info"
         }
       }
     }
   }
   ```

3. **Restart Claude Desktop**

#### For Cline (VS Code)

1. **Build ggen-mcp** (same as above)

2. **Configure in Cline settings:**
   - Open VS Code Settings (Cmd/Ctrl + ,)
   - Search for "Cline: MCP Servers"
   - Add new server configuration:
     ```json
     {
       "ggen": {
         "command": "/path/to/ggen/target/release/ggen-mcp",
         "args": [],
         "env": {
           "ANTHROPIC_API_KEY": "your-api-key"
         }
       }
     }
     ```

3. **Reload VS Code**

### Verification

Test the MCP connection:

```bash
./test-mcp-tools.sh
```

Or in Claude Desktop, ask:
```
List the available ggen MCP tools
```

You should see 25+ tools listed.

## Available Tools

GGen MCP provides 25+ specialized tools organized into 6 categories:

### 1. Project Tools (4 tools)

Generate and manage projects from templates:

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| `project_gen` | Generate files from template | `template`, `vars`, `dry_run`, `force` |
| `project_plan` | Create execution plan | `template`, `vars` |
| `project_apply` | Apply execution plan | `plan` |
| `project_diff` | Show template differences | `template`, `vars` |

### 2. Marketplace Tools (8 tools)

Discover and install templates:

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| `market_list` | List marketplace templates | `category`, `tag` |
| `market_search` | Search by query | `query`, `category`, `limit` |
| `market_install` | Install package | `package`, `version` |
| `market_info` | Get package details | `package_id` |
| `market_recommend` | Get recommendations | `based_on`, `category` |
| `market_offline_search` | Search cached data | `query`, `category` |
| `market_cache_status` | Check cache status | - |
| `market_sync` | Sync marketplace | `category`, `force` |

### 3. Graph Tools (3 tools)

RDF knowledge graph operations:

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| `graph_query` | Execute SPARQL query | `sparql`, `graph` |
| `graph_load` | Load RDF data | `file`, `graph`, `format` |
| `graph_export` | Export graph | `output`, `graph`, `format` |

### 4. Template Tools (2 tools)

Create and validate templates:

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| `template_create` | Create new template | `name`, `template_type` |
| `template_validate` | Validate template | `template` |

### 5. Hook Tools (1 tool)

Lifecycle automation:

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| `hook_register` | Register lifecycle hook | `event`, `command` |

### 6. AI Tools (7 tools)

AI-powered code generation:

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| `ai_generate_template` | Generate template from NL | `description`, `template_type`, `provider` |
| `ai_generate_sparql` | Generate SPARQL query | `intent`, `schema`, `provider` |
| `ai_generate_ontology` | Generate RDF ontology | `domain`, `requirements`, `provider` |
| `ai_generate_project` | Generate complete project | `description`, `requirements`, `provider` |
| `ai_extend_graph` | Extend knowledge graph | `graph`, `context`, `provider` |
| `ai_validate_and_improve` | Improve code/templates | `content`, `content_type`, `provider` |
| `ai_list_providers` | List AI providers | - |

## Example Conversations

See the `example-conversations/` directory for detailed examples:

1. **[01-basic-generation.md](example-conversations/01-basic-generation.md)** - Generate projects from templates
2. **[02-template-rendering.md](example-conversations/02-template-rendering.md)** - Work with templates
3. **[03-graph-query.md](example-conversations/03-graph-query.md)** - Query knowledge graphs
4. **[04-marketplace.md](example-conversations/04-marketplace.md)** - Search and install packages
5. **[05-hooks.md](example-conversations/05-hooks.md)** - Automate with hooks

### Quick Example

In Claude Desktop, try:

```
Search the ggen marketplace for React templates
```

Claude will use `market_search`:
```json
{
  "query": "React",
  "category": "web",
  "limit": 10
}
```

Then install one:
```
Install the react-typescript-starter package
```

Claude uses `market_install`:
```json
{
  "package": "react-typescript-starter",
  "version": "latest"
}
```

Finally, generate your project:
```
Generate a new React TypeScript project called "my-app" with
dark theme enabled
```

Claude uses `project_gen`:
```json
{
  "template": "react-typescript-starter",
  "vars": {
    "project_name": "my-app",
    "enable_dark_theme": true
  }
}
```

## Advanced Features

### 1. AI-Powered Generation

Use AI tools to generate projects from natural language:

```
Use AI to generate a REST API template for a task management system
with user authentication, CRUD operations, and PostgreSQL database
```

Claude will use `ai_generate_project` to create a complete project structure.

### 2. Knowledge Graph Integration

Query your project's knowledge graph:

```
Query the graph to find all templates that depend on React 18+
```

Claude generates and executes a SPARQL query:
```sparql
PREFIX ggen: <http://ggen.ai/ontology#>
PREFIX npm: <http://npm.org/>

SELECT ?template WHERE {
  ?template ggen:hasDependency ?dep .
  ?dep npm:name "react" .
  ?dep npm:version ?version .
  FILTER(regex(?version, "^18\\."))
}
```

### 3. Multi-Step Workflows

Claude can chain multiple tools:

```
Find the most popular Express.js templates, show me the top one's details,
then generate a project from it with TypeScript enabled
```

This uses:
1. `market_search` - Find Express templates
2. `market_info` - Get details
3. `project_gen` - Generate project

### 4. Validation and Planning

Preview changes before applying:

```
Create an execution plan for the express-api-boilerplate template
with my custom variables, but don't apply it yet
```

Uses `project_plan` to show what will be created without making changes.

## Integration Patterns

### Pattern 1: Template Discovery → Installation → Generation

```typescript
// User: "Set up a Next.js project with Tailwind CSS"

// 1. Search marketplace
market_search({ query: "nextjs tailwind" })

// 2. Install template
market_install({ package: "nextjs-tailwind-starter" })

// 3. Generate project
project_gen({
  template: "nextjs-tailwind-starter",
  vars: { project_name: "my-site" }
})
```

### Pattern 2: AI Generation → Validation → Application

```typescript
// User: "Create a custom authentication module"

// 1. Generate with AI
ai_generate_template({
  description: "User authentication with JWT and bcrypt",
  template_type: "module"
})

// 2. Validate generated template
template_validate({ template: "auth-module" })

// 3. Apply to project
project_gen({ template: "auth-module" })
```

### Pattern 3: Graph Query → Analysis → Extension

```typescript
// User: "Find templates with security issues and fix them"

// 1. Query for vulnerable dependencies
graph_query({
  sparql: "SELECT ?template WHERE { ?template ggen:hasVulnerability true }"
})

// 2. Use AI to improve
ai_validate_and_improve({
  content: "template_content",
  content_type: "template"
})

// 3. Update graph
ai_extend_graph({
  graph: "main",
  context: "Fixed security vulnerabilities"
})
```

## Troubleshooting

### Issue: MCP server not appearing in Claude Desktop

**Solution:**
1. Check config file location and JSON syntax
2. Verify binary path is absolute
3. Check logs: `~/Library/Logs/Claude/mcp-server-ggen.log` (macOS)
4. Restart Claude Desktop completely

### Issue: Tools fail with "Command not found"

**Solution:**
- Ensure `ggen` CLI is in PATH:
  ```bash
  which ggen
  export PATH="$HOME/.cargo/bin:$PATH"
  ```

### Issue: AI tools fail

**Solution:**
- Set API key in environment:
  ```json
  {
    "env": {
      "ANTHROPIC_API_KEY": "sk-ant-..."
    }
  }
  ```
- Check provider is available: `ai_list_providers`

### Issue: Permission denied errors

**Solution:**
```bash
chmod +x /path/to/ggen-mcp
```

### Issue: Graph operations fail

**Solution:**
- Initialize graph store: `ggen graph init`
- Check graph exists: `ggen graph list`
- Verify RDF file format

## Logging and Debugging

Enable debug logging:

```json
{
  "env": {
    "GGEN_MCP_LOG": "debug",
    "RUST_BACKTRACE": "1"
  }
}
```

View logs:
- **macOS:** `~/Library/Logs/Claude/mcp-server-ggen.log`
- **Windows:** `%APPDATA%\Claude\logs\mcp-server-ggen.log`

Test individual tools:
```bash
./test-mcp-tools.sh --tool market_search --params '{"query":"react"}'
```

## Best Practices

1. **Use dry_run first:** Preview project generation with `dry_run: true`
2. **Cache marketplace data:** Run `market_sync` for offline work
3. **Validate templates:** Always validate with `template_validate` before using
4. **Version control:** Use `project_plan` + `project_apply` for tracked changes
5. **AI provider selection:** Choose provider based on task complexity
6. **Graph maintenance:** Regularly export graphs with `graph_export`

## Further Resources

- **GGen Documentation:** `/docs/`
- **MCP Specification:** https://spec.modelcontextprotocol.io
- **Template Authoring Guide:** `/docs/templates.md`
- **Graph Query Reference:** `/docs/graph-queries.md`
- **AI Provider Comparison:** `/docs/ai-providers.md`

## Contributing

Found an issue or want to improve MCP integration? See:
- Report bugs: https://github.com/yourusername/ggen/issues
- Contribute: `/CONTRIBUTING.md`
- MCP server code: `/ggen-mcp/`

## License

MIT License - see LICENSE file for details
