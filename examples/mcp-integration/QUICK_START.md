# GGen MCP Quick Start Guide

Get up and running with ggen-mcp in Claude Desktop or Cline in under 5 minutes!

## 🚀 5-Minute Setup

### Step 1: Build ggen-mcp

```bash
cd /path/to/ggen/ggen-mcp
cargo build --release
```

### Step 2: Install for Claude Desktop

**macOS:**
```bash
# Edit config
code ~/Library/Application\ Support/Claude/claude_desktop_config.json

# Add this:
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

# Restart Claude Desktop
```

**Windows:**
```bash
# Edit config
notepad %APPDATA%\Claude\claude_desktop_config.json

# Add the same configuration as above
# Restart Claude Desktop
```

### Step 3: Verify Installation

In Claude Desktop, ask:
```
List the available ggen MCP tools
```

You should see 25+ tools!

## 🎯 First Steps

### 1. Search for Templates
```
Search the ggen marketplace for React templates
```

### 2. Generate a Project
```
Generate a React TypeScript project called "my-app" using the
react-typescript-starter template with dark theme enabled
```

### 3. Query Knowledge Graph
```
Show me all templates that use React 18 or higher
```

### 4. Use AI Generation
```
Use AI to generate a FastAPI microservice template for a task
management system with user authentication
```

## 📚 What's Included

**25+ MCP Tools:**
- ✅ 4 Project tools (gen, plan, apply, diff)
- ✅ 8 Marketplace tools (search, install, info, etc.)
- ✅ 3 Graph tools (query, load, export)
- ✅ 2 Template tools (create, validate)
- ✅ 1 Hook tool (register)
- ✅ 7 AI tools (generate_*, validate, list)

## 🔥 Most Useful Tools

1. **project_gen** - Generate projects from templates
   ```
   Generate a new project from the fastapi-auth-db template with
   project_name=api and database_type=postgres
   ```

2. **market_search** - Find templates
   ```
   Search for Express.js API templates with authentication
   ```

3. **ai_generate_project** - AI-powered generation
   ```
   Use AI to create a complete REST API for a blog platform
   ```

4. **graph_query** - Query knowledge graphs
   ```
   Find all templates compatible with PostgreSQL
   ```

5. **hook_register** - Automate workflows
   ```
   Set up a hook to automatically format code and run tests
   after project generation
   ```

## 📖 Example Conversations

See detailed examples in `example-conversations/`:

1. **[01-basic-generation.md](example-conversations/01-basic-generation.md)**
   - Project generation basics
   - Preview and diff modes
   - Template variables

2. **[02-template-rendering.md](example-conversations/02-template-rendering.md)**
   - Creating custom templates
   - AI-powered template generation
   - Template validation

3. **[03-graph-query.md](example-conversations/03-graph-query.md)**
   - SPARQL queries
   - AI-generated queries
   - Graph data management

4. **[04-marketplace.md](example-conversations/04-marketplace.md)**
   - Browsing and searching
   - Package installation
   - Recommendations

5. **[05-hooks.md](example-conversations/05-hooks.md)**
   - Lifecycle automation
   - Conditional hooks
   - Multi-step workflows

## 🛠️ Troubleshooting

### Issue: Tools not appearing

**Solution:**
```bash
# Check if binary exists
ls -l /path/to/ggen/target/release/ggen-mcp

# Test manually
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | \
  /path/to/ggen/target/release/ggen-mcp

# Check logs
tail -f ~/Library/Logs/Claude/mcp-server-ggen.log
```

### Issue: API key errors

**Solution:**
- Set `ANTHROPIC_API_KEY` or `OPENAI_API_KEY` in the config
- Restart Claude Desktop after config changes

### Issue: Permission denied

**Solution:**
```bash
chmod +x /path/to/ggen/target/release/ggen-mcp
```

## 🔗 Links

- **Full Documentation:** [README.md](README.md)
- **Installation Script:** [install-ggen-mcp.sh](install-ggen-mcp.sh)
- **Test Script:** [test-mcp-tools.sh](test-mcp-tools.sh)
- **MCP Specification:** https://spec.modelcontextprotocol.io

## 💡 Pro Tips

1. **Use dry_run first:** Always preview with `dry_run: true` before generating
2. **Cache marketplace data:** Run `market_sync` for offline work
3. **Leverage AI tools:** Let AI generate SPARQL queries and templates
4. **Set up hooks:** Automate formatting, testing, and deployment
5. **Export graphs:** Share knowledge graphs with your team

## 🎉 You're Ready!

Start a conversation in Claude Desktop and say:

```
Search the ggen marketplace for templates I can use to build a web app
```

Happy generating! 🚀
