# ggen MCP Quick Reference

Fast reference for using ggen with the Model Context Protocol.

## 🚀 Quick Start

### Install ggen
```bash
brew install seanchatmangpt/tap/ggen
# or: cargo install ggen
```

### Configure Claude Desktop
```json
{
  "mcpServers": {
    "ggen": {
      "command": "ggen",
      "args": ["mcp", "start"],
      "type": "stdio"
    }
  }
}
```

Location: `~/Library/Application Support/Claude/claude_desktop_config.json` (macOS)

### Test
In Claude: "List available ggen templates"

---

## 🛠️ Top 10 Tools

| Tool | Purpose | Example |
|------|---------|---------|
| `ggen_template_list` | List templates | `{}` |
| `ggen_gen_with_vars` | Generate code | `{template: "x.tmpl", vars: {...}}` |
| `ggen_market_search` | Search gpacks | `{query: "rust cli"}` |
| `ggen_graph_query` | SPARQL query | `{query: "SELECT ..."}` |
| `ggen_inject_idempotent` | Add to file | `{template: "x.tmpl", target: "file.rs"}` |
| `ggen_gen_batch` | Multi-file gen | `{templates: [...], vars: {...}}` |
| `ggen_graph_load` | Load RDF | `{file: "data.ttl"}` |
| `ggen_market_add` | Install gpack | `{gpack: "io.ggen.rust.cli"}` |
| `ggen_template_show` | View template | `{template: "x.tmpl"}` |
| `ggen_gen_dry_run` | Preview output | `{template: "x.tmpl"}` |

---

## 📝 Common Workflows

### Workflow 1: Find & Use Template
```javascript
1. ggen_market_search({ query: "python flask" })
2. ggen_market_add({ gpack: "io.ggen.python.flask-api" })
3. ggen_gen_with_vars({ template: "...", vars: {...} })
```

### Workflow 2: RDF → Code
```javascript
1. ggen_graph_load({ file: "schema.ttl" })
2. ggen_graph_query({ query: "SELECT ?entity ..." })
3. ggen_gen_batch({ templates: [...], vars: {results} })
```

### Workflow 3: Inject Without Duplicates
```javascript
1. ggen_gen_dry_run({ template: "route.tmpl" })
2. ggen_inject_idempotent({ template: "route.tmpl", target: "router.rs" })
```

---

## 🔧 Configuration

### Environment Variables
```bash
GGEN_HOME=~/.ggen/templates        # Template directory
GGEN_REGISTRY_URL=https://...      # Marketplace URL
RUST_LOG=ggen=debug                # Logging level
GGEN_MAX_GRAPH_SIZE=100            # Max RDF size (MB)
```

### Transport Options

**Stdio (default):**
```bash
ggen mcp start
```

**HTTP:**
```bash
ggen mcp start --transport http --port 8080
```

**SSE:**
```bash
ggen mcp start --transport sse --port 3000
```

---

## 🐛 Troubleshooting

### Server won't start
```bash
# Check version
ggen --version

# Test manually
echo '{"jsonrpc":"2.0","method":"initialize"}' | ggen mcp start
```

### Templates not found
```bash
# Verify location
ls ~/.ggen/templates/

# Set explicitly
export GGEN_HOME=/custom/path
```

### SPARQL queries fail
```bash
# Check graph loaded
ggen_graph_stats({})

# Validate query
ggen_graph_validate_query({ query: "..." })
```

---

## 📚 Full Documentation

- **[MCP Server Guide](./MCP_SERVER.md)** - Complete setup & tools
- **[Usage Guide](./MCP_USAGE_GUIDE.md)** - Workflows & examples
- **[Code Examples](../examples/mcp/)** - Working code samples
- **[ggen Cookbook](./COOKBOOK-CONVO.md)** - Template recipes

---

## 🎯 Key Concepts

### Deterministic Generation
```yaml
---
determinism: 42  # Same seed = same output
---
```

### RDF-Backed Templates
```yaml
---
rdf_inline:
  - "@prefix ex: <http://example.org/> ."
sparql:
  get_entities: "SELECT ?e WHERE { ?e a owl:Class }"
---
{{ sparql(query="get_entities") }}
```

### Idempotent Injection
```yaml
---
inject:
  mode: "after"
  pattern: "// Routes"
  skip_if: "{{route_name}}"  # Won't duplicate
---
```

---

## 📊 Tool Categories

- **Template Mgmt** (8 tools): list, show, lint, create, edit, delete
- **Generation** (7 tools): gen, gen_with_vars, inject, batch, dry_run
- **RDF/SPARQL** (9 tools): load, query, add_triple, export, stats
- **Marketplace** (8 tools): search, add, remove, update, info
- **Project** (4 tools): init, scaffold, config, status
- **GitHub** (3 tools): pages_status, workflow_status, trigger
- **Utility** (3 tools): validate, completion, hazard

**Total: 42+ tools**

---

## ⚡ Performance Tips

1. **Use batch operations** for multiple files
2. **Enable caching** for SPARQL queries
3. **Compile templates** for faster rendering
4. **Set determinism** for reproducibility
5. **Use parallel=true** in batch generation

---

## 🔒 Security

```bash
# Sandbox templates
ggen mcp start --sandbox-templates

# Rate limiting
ggen mcp start --rate-limit 100/min

# Audit logging
ggen mcp start --audit-log /var/log/ggen.json
```

---

## 🆘 Getting Help

```bash
# CLI help
ggen mcp --help

# Tool schemas
ggen mcp tools --list
ggen mcp tools --describe ggen_gen

# Debug mode
RUST_LOG=debug ggen mcp start
```

---

**Built with ❤️ using Rust, RDF, SPARQL, and MCP**

[GitHub](https://github.com/seanchatmangpt/ggen) • [Docs](https://seanchatmangpt.github.io/ggen/) • [Discussions](https://github.com/seanchatmangpt/ggen/discussions)
