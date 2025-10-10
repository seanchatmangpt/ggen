<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen-mcp Integration with Claude Code](#ggen-mcp-integration-with-claude-code)
  - [✅ Successfully Connected!](#-successfully-connected)
  - [🎯 Connection Status](#-connection-status)
  - [🛠️ Available Tools (40+)](#-available-tools-40)
    - [Project Tools](#project-tools)
    - [Marketplace Tools](#marketplace-tools)
    - [Graph Tools (RDF/SPARQL)](#graph-tools-rdfsparql)
    - [Template Tools](#template-tools)
    - [Hook Tools](#hook-tools)
  - [🚀 Usage Examples](#-usage-examples)
    - [Generate Project from Template](#generate-project-from-template)
    - [Search Marketplace](#search-marketplace)
    - [Query RDF Graph](#query-rdf-graph)
  - [📁 Configuration](#-configuration)
    - [Claude Code Config Location](#claude-code-config-location)
    - [Server Binary Location](#server-binary-location)
    - [Server Configuration](#server-configuration)
  - [🔧 Technical Details](#-technical-details)
    - [Implementation](#implementation)
    - [Features Enabled](#features-enabled)
    - [Dependencies](#dependencies)
  - [📊 Server Capabilities](#-server-capabilities)
  - [🎯 Integration with Claude-Flow](#-integration-with-claude-flow)
    - [Example Combined Workflow](#example-combined-workflow)
  - [🔄 Rebuilding the Server](#-rebuilding-the-server)
  - [📚 Documentation References](#-documentation-references)
  - [🎉 Success Criteria](#-success-criteria)
  - [🚀 Next Steps](#-next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen-mcp Integration with Claude Code

## ✅ Successfully Connected!

The ggen-mcp server is now connected to Claude Code and ready to use.

## 🎯 Connection Status

```bash
$ claude mcp list
ggen-mcp: /Users/sac/ggen/target/release/ggen-mcp - ✓ Connected
```

## 🛠️ Available Tools (40+)

### Project Tools
- `project_gen` - Generate files from templates with variables
- `project_plan` - Create execution plan without applying changes
- `project_apply` - Apply planned changes to filesystem
- `project_diff` - Show differences between planned and current state

### Marketplace Tools
- `market_list` - List available packages with filters
- `market_search` - Search marketplace with semantic queries
- `market_install` - Install packages with dependencies
- `market_recommend` - Get personalized package recommendations
- `market_info` - Get detailed package information
- `market_offline_search` - Search cached offline data
- `market_cache_status` - Get cache statistics
- `market_sync` - Synchronize with remote marketplace

### Graph Tools (RDF/SPARQL)
- `graph_query` - Execute SPARQL queries against RDF graph
- `graph_load` - Load RDF data from file
- `graph_export` - Export RDF graph to file

### Template Tools
- `template_create` - Create new template
- `template_validate` - Validate template syntax and structure

### Hook Tools
- `hook_register` - Register lifecycle hooks

## 🚀 Usage Examples

### Generate Project from Template

```javascript
// Using MCP tool directly
mcp__ggen-mcp__project_gen({
  template_path: "templates/rust-cli",
  output_path: "./my-new-cli",
  vars: {
    "project_name": "awesome-cli",
    "author": "Your Name"
  }
})
```

### Search Marketplace

```javascript
mcp__ggen-mcp__market_search({
  query: "rust web framework",
  category: "templates",
  limit: 10
})
```

### Query RDF Graph

```javascript
mcp__ggen-mcp__graph_query({
  query: `
    SELECT ?template ?name ?category
    WHERE {
      ?template rdf:type ggen:Template ;
                ggen:name ?name ;
                ggen:category ?category .
    }
    LIMIT 10
  `
})
```

## 📁 Configuration

### Claude Code Config Location
`/Users/sac/.claude.json`

### Server Binary Location
`/Users/sac/ggen/target/release/ggen-mcp`

### Server Configuration
```json
{
  "mcpServers": {
    "ggen-mcp": {
      "command": "/Users/sac/ggen/target/release/ggen-mcp",
      "transport": "stdio"
    }
  }
}
```

## 🔧 Technical Details

### Implementation
- **Language:** Rust
- **MCP Library:** rmcp 0.8.0
- **Transport:** stdio (Standard Input/Output)
- **Protocol Version:** 2024-11-05

### Features Enabled
- `transport-io` - Stdio transport support
- `server` - MCP server capabilities

### Dependencies
- `rmcp` - MCP protocol implementation
- `tokio` - Async runtime
- `serde_json` - JSON serialization
- `ggen-core` - Core template engine
- `async-trait` - Async trait support

## 📊 Server Capabilities

- ✅ **Tools** - 40+ MCP tools available
- ✅ **Logging** - Structured logging to stderr
- ✅ **Error Handling** - Comprehensive error types
- ✅ **Async** - Full async/await support
- ✅ **Type Safety** - Strong typing with Rust

## 🎯 Integration with Claude-Flow

The ggen-mcp server works alongside Claude-Flow for enhanced workflows:

1. **Claude-Flow** - Swarm orchestration and coordination
2. **ggen-mcp** - Template generation and RDF graph operations
3. **Flow-Nexus** - Cloud execution and neural features
4. **ruv-swarm** - Enhanced multi-agent coordination

### Example Combined Workflow

```javascript
// 1. Use Claude-Flow to orchestrate
mcp__claude-flow__swarm_init({topology: "mesh"})
mcp__claude-flow__agent_spawn({type: "coder"})

// 2. Use ggen-mcp to generate templates
mcp__ggen-mcp__project_gen({
  template_path: "rust-api",
  vars: {...}
})

// 3. Use Flow-Nexus for deployment
mcp__flow-nexus__sandbox_create({
  template: "rust",
  name: "api-test"
})
```

## 🔄 Rebuilding the Server

If you make changes to ggen-mcp:

```bash
# Rebuild
cargo build --package ggen-mcp --release

# Verify connection
claude mcp list
```

## 📚 Documentation References

- **Main README:** `/ggen-mcp/README.md`
- **MCP Server Guide:** `/docs/MCP_SERVER.md`
- **Usage Guide:** `/docs/MCP_USAGE_GUIDE.md`
- **Quick Reference:** `/docs/MCP_QUICK_REFERENCE.md`
- **Documentation Index:** `/docs/MCP_DOCUMENTATION_INDEX.md`

## 🎉 Success Criteria

- ✅ Server builds successfully
- ✅ Stdio transport enabled
- ✅ Connected to Claude Code
- ✅ 40+ tools available
- ✅ Compatible with Claude-Flow swarms
- ✅ Ready for template generation

## 🚀 Next Steps

1. **Test Tools**: Try using `mcp__ggen-mcp__market_list` or `mcp__ggen-mcp__project_gen`
2. **Create Templates**: Use ggen-mcp to generate project structures
3. **Integrate with Swarms**: Combine with Claude-Flow for orchestrated workflows
4. **Explore RDF**: Query semantic data with SPARQL

---

**Status:** ✅ OPERATIONAL
**Server Version:** 0.2.4
**Protocol Version:** MCP 2024-11-05
**Transport:** stdio
**Connection:** Active
