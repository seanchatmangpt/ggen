# ggen MCP Documentation Index

Complete guide to the ggen Model Context Protocol (MCP) server documentation.

## 📁 Documentation Structure

```
ggen/
├── docs/
│   ├── MCP_SERVER.md                    # Main MCP server documentation
│   ├── MCP_USAGE_GUIDE.md               # Detailed usage workflows
│   ├── MCP_QUICK_REFERENCE.md           # Quick reference guide
│   └── MCP_DOCUMENTATION_INDEX.md       # This file
└── examples/
    └── mcp/
        ├── README.md                    # Examples overview
        ├── claude-desktop/
        │   ├── config.json             # Claude Desktop config
        │   └── example-conversation.md # Sample conversations
        ├── cline-vscode/
        │   └── (VSCode integration examples)
        ├── rust-client/
        │   ├── Cargo.toml
        │   └── src/main.rs             # Rust rmcp client
        ├── python-client/
        │   ├── requirements.txt
        │   └── basic_usage.py          # Python MCP SDK
        └── http-api/
            └── curl/
                └── basic.sh            # curl examples
```

## 📚 Documentation Files

### [MCP_SERVER.md](./MCP_SERVER.md)
**Main MCP server documentation** - Start here!

**Contents:**
- What is the ggen MCP server
- Quick start installation
- 40+ available tools (organized by category)
- Configuration (stdio, SSE, HTTP transports)
- Integration examples (Claude Desktop, Cline, etc.)
- Environment variables
- Troubleshooting guide

**Use when:** Setting up ggen MCP for the first time

### [MCP_USAGE_GUIDE.md](./MCP_USAGE_GUIDE.md)
**Comprehensive usage guide** - Deep dive into workflows

**Contents:**
- Transport options deep dive
- Tool categories and workflows
- Real-world conversation examples
- Error handling patterns
- Performance tuning
- Advanced usage patterns
- Security best practices

**Use when:** Building complex workflows or troubleshooting issues

### [MCP_QUICK_REFERENCE.md](./MCP_QUICK_REFERENCE.md)
**Quick reference** - Fast lookup

**Contents:**
- Quick start commands
- Top 10 most-used tools
- Common workflows (3 steps)
- Environment variables
- Troubleshooting checklist
- Performance tips

**Use when:** You need a quick reminder or reference

## 🎯 Examples

### [examples/mcp/README.md](../examples/mcp/README.md)
**Examples overview** - Working code samples

**Contents:**
- Directory structure
- Claude Desktop integration
- Cline VSCode setup
- Rust client (rmcp)
- Python client (MCP SDK)
- HTTP API (curl)
- SSE client
- Advanced batch examples

**Use when:** Looking for copy-paste ready code

### Specific Examples

| Example | File | Purpose |
|---------|------|---------|
| **Claude Desktop** | `claude-desktop/config.json` | MCP config for Claude Desktop |
| **Conversations** | `claude-desktop/example-conversation.md` | Sample AI conversations |
| **Rust Client** | `rust-client/src/main.rs` | Using rmcp library |
| **Python Client** | `python-client/basic_usage.py` | Using MCP Python SDK |
| **HTTP API** | `http-api/curl/basic.sh` | curl commands for HTTP transport |

## 🚀 Quick Navigation

### I want to...

**...set up ggen MCP for the first time**
→ Read [MCP_SERVER.md](./MCP_SERVER.md#quick-start)

**...use it with Claude Desktop**
→ Copy [examples/mcp/claude-desktop/config.json](../examples/mcp/claude-desktop/config.json)

**...see example conversations**
→ Read [example-conversation.md](../examples/mcp/claude-desktop/example-conversation.md)

**...write a Rust client**
→ See [rust-client/src/main.rs](../examples/mcp/rust-client/src/main.rs)

**...write a Python client**
→ See [python-client/basic_usage.py](../examples/mcp/python-client/basic_usage.py)

**...use HTTP/REST API**
→ Run [http-api/curl/basic.sh](../examples/mcp/http-api/curl/basic.sh)

**...understand all 40+ tools**
→ Read [MCP_SERVER.md#available-tools](./MCP_SERVER.md#available-tools)

**...build complex workflows**
→ Read [MCP_USAGE_GUIDE.md#tool-categories-and-workflows](./MCP_USAGE_GUIDE.md#tool-categories-and-workflows)

**...troubleshoot issues**
→ Check [MCP_SERVER.md#troubleshooting](./MCP_SERVER.md#troubleshooting)

**...quick reference**
→ Read [MCP_QUICK_REFERENCE.md](./MCP_QUICK_REFERENCE.md)

## 📖 Reading Order

### For Beginners
1. [MCP_SERVER.md](./MCP_SERVER.md) (read "What is" + "Quick Start")
2. [examples/mcp/claude-desktop/config.json](../examples/mcp/claude-desktop/config.json) (copy config)
3. [example-conversation.md](../examples/mcp/claude-desktop/example-conversation.md) (see examples)
4. [MCP_QUICK_REFERENCE.md](./MCP_QUICK_REFERENCE.md) (bookmark for later)

### For Developers
1. [MCP_SERVER.md#available-tools](./MCP_SERVER.md#available-tools) (scan tools)
2. [examples/mcp/README.md](../examples/mcp/README.md) (choose your language)
3. [rust-client/src/main.rs](../examples/mcp/rust-client/src/main.rs) OR [python-client/basic_usage.py](../examples/mcp/python-client/basic_usage.py)
4. [MCP_USAGE_GUIDE.md](./MCP_USAGE_GUIDE.md) (deep dive)

### For Production Deployments
1. [MCP_SERVER.md#transport-options](./MCP_SERVER.md#transport-options)
2. [MCP_USAGE_GUIDE.md#security-best-practices](./MCP_USAGE_GUIDE.md#security-best-practices)
3. [MCP_USAGE_GUIDE.md#performance-tuning](./MCP_USAGE_GUIDE.md#performance-tuning)
4. [http-api/curl/basic.sh](../examples/mcp/http-api/curl/basic.sh) (for HTTP deployment)

## 🔍 Search Guide

### Search by Topic

| Topic | Location |
|-------|----------|
| **Installation** | MCP_SERVER.md#quick-start |
| **All 40+ tools** | MCP_SERVER.md#available-tools |
| **Configuration** | MCP_SERVER.md#configuration |
| **Transport (stdio/HTTP/SSE)** | MCP_SERVER.md#transport-options |
| **Workflows** | MCP_USAGE_GUIDE.md#tool-categories-and-workflows |
| **Error handling** | MCP_USAGE_GUIDE.md#error-handling |
| **Performance** | MCP_USAGE_GUIDE.md#performance-tuning |
| **Security** | MCP_USAGE_GUIDE.md#security-best-practices |
| **Claude Desktop** | examples/mcp/claude-desktop/ |
| **Rust client** | examples/mcp/rust-client/ |
| **Python client** | examples/mcp/python-client/ |
| **HTTP/REST** | examples/mcp/http-api/ |

### Search by Tool Category

| Category | Tool Count | Documentation |
|----------|-----------|---------------|
| Template Management | 8 | MCP_SERVER.md#template-management |
| Project Generation | 7 | MCP_SERVER.md#project-generation |
| RDF Graph Operations | 9 | MCP_SERVER.md#rdf-graph-operations |
| Marketplace (gpacks) | 8 | MCP_SERVER.md#marketplace-gpacks |
| Project Scaffolding | 4 | MCP_SERVER.md#project-scaffolding |
| GitHub Integration | 3 | MCP_SERVER.md#github-integration |
| Utility Tools | 3 | MCP_SERVER.md#utility-tools |

## 🎓 Learning Path

### Beginner (30 minutes)
- ✅ Read MCP_SERVER.md intro
- ✅ Copy Claude Desktop config
- ✅ Try 3 example conversations
- ✅ Bookmark quick reference

### Intermediate (2 hours)
- ✅ Read full MCP_SERVER.md
- ✅ Try all transport types
- ✅ Build Rust or Python client
- ✅ Explore all tool categories

### Advanced (1 day)
- ✅ Read MCP_USAGE_GUIDE.md
- ✅ Implement batch workflows
- ✅ Set up production deployment
- ✅ Tune performance

## 📝 Key Concepts

### What is MCP?
The **Model Context Protocol** allows AI assistants (like Claude) to interact with external tools. The ggen MCP server exposes ggen's code generation capabilities as MCP tools.

### What is ggen?
A **deterministic, graph-aware code generation framework** that treats code as projections of RDF knowledge graphs. Generate reproducible, multi-language code from semantic ontologies.

### Why ggen + MCP?
- 🎯 **40+ tools** for template-based generation
- 🌐 **RDF/SPARQL integration** for semantic code
- 📦 **Marketplace** of reusable templates
- 🔄 **Idempotent injection** into existing files
- 🎲 **Deterministic output** with fixed seeds

## 🆘 Getting Help

### In-Documentation
1. Check [Troubleshooting](./MCP_SERVER.md#troubleshooting)
2. Read [Error Handling](./MCP_USAGE_GUIDE.md#error-handling)
3. See [Quick Reference](./MCP_QUICK_REFERENCE.md)

### External Resources
- **GitHub Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- **MCP Spec**: https://modelcontextprotocol.io
- **ggen Docs**: https://seanchatmangpt.github.io/ggen/

### Debug Commands
```bash
# Test server
echo '{"jsonrpc":"2.0","method":"initialize"}' | ggen mcp start

# Verbose logging
RUST_LOG=debug ggen mcp start

# List tools
ggen mcp tools --list

# Describe tool
ggen mcp tools --describe ggen_gen_with_vars
```

## 📊 Documentation Statistics

| Metric | Count |
|--------|-------|
| **Documentation Files** | 4 |
| **Example Files** | 8+ |
| **Tools Documented** | 42+ |
| **Code Examples** | 20+ |
| **Workflows** | 15+ |
| **Transport Types** | 3 |
| **Client Languages** | 4 (Rust, Python, JavaScript, Shell) |

## 🔗 Related Documentation

- [ggen README.md](../README.md) - Project overview
- [ggen Cookbook](./COOKBOOK-CONVO.md) - Template recipes
- [GitHub API Integration](./GITHUB_API_RUST_INTEGRATION.md) - GitHub features
- [CLAUDE.md](../CLAUDE.md) - Development guidelines

---

**Last Updated:** 2024-10-10
**Version:** 1.0.0
**Status:** Complete ✅

**Built with ❤️ using Rust, RDF, SPARQL, and the Model Context Protocol**
