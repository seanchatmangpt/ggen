# ggen-mcp

[![MCP](https://img.shields.io/badge/MCP-Compatible-blue)](https://modelcontextprotocol.io)
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org/)

**Model Context Protocol (MCP) server for ggen** - Expose ggen's graph-aware code generation to AI assistants.

## 🚀 Quick Start

```bash
# Install ggen (includes MCP server)
brew install seanchatmangpt/tap/ggen

# Start MCP server
ggen mcp start
```

## 📚 Documentation

For complete MCP server documentation, see:

- **[MCP Server Guide](../docs/MCP_SERVER.md)** - Complete setup & tools (40+)
- **[Usage Guide](../docs/MCP_USAGE_GUIDE.md)** - Workflows and examples  
- **[Quick Reference](../docs/MCP_QUICK_REFERENCE.md)** - Fast lookup
- **[Code Examples](../examples/mcp/)** - Working samples
- **[Documentation Index](../docs/MCP_DOCUMENTATION_INDEX.md)** - Navigation

## 🛠️ Features

- **40+ MCP Tools** for code generation
- **3 Transport Options**: stdio, HTTP, SSE
- **RDF/SPARQL Integration** for semantic queries
- **Marketplace Access** to template packages (gpacks)
- **Deterministic Generation** with reproducible outputs

## ⚠️ Implementation Status

This crate defines the intended structure for the ggen MCP server. The full implementation is pending.

Current documentation provides:
- Complete API specification for all 40+ tools
- Integration examples (Claude Desktop, Rust, Python, etc.)
- Transport configuration (stdio, HTTP, SSE)
- Security and performance best practices

See `docs/` and `examples/mcp/` for comprehensive guides.

---

**Built with ❤️ using Rust, RDF, SPARQL, and the Model Context Protocol**
