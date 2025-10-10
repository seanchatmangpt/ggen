# 🏪 Marketplace

The ggen marketplace provides a curated ecosystem of reusable code generation packs (gpacks) served via GitHub Pages with automated validation and deployment. Discover, install, and use high-quality templates from the community.

## 📚 About

The ggen marketplace provides a curated ecosystem of reusable code generation packs (gpacks) served via GitHub Pages with automated validation and deployment. Discover, install, and use high-quality templates from the community.

### Key Statistics

| Metric | Value |
|--------|-------|
| Available Gpacks | 1 |
| Open Source | 100% |
| License | MIT |

## 🔍 Registry API

Access the marketplace registry programmatically:

- **Registry Index (JSON)**: [registry/index.json](registry/index.json)
- **Source Repository**: [seanchatmangpt/ggen](https://github.com/seanchatmangpt/ggen)

```bash
# Registry URL: https://seanchatmangpt.github.io/ggen/registry/
# API Endpoint: https://seanchatmangpt.github.io/ggen/registry/index.json
```

## 🚀 Quick Start

Get started with the ggen marketplace:

```bash
# Search for gpacks
ggen search rust cli

# Install an gpack
ggen add io.ggen.rust.cli-subcommand

# Use installed gpack
ggen gen io.ggen.rust.cli-subcommand:rust.tmpl cmd=test
```

## 📦 Available Gpacks

Currently available gpacks in the marketplace:

```
io.ggen.rust.cli-subcommand
├── Generate clap subcommands for Rust CLI applications
├── Version: 0.1.0
├── License: MIT
└── Tags: rust, cli, clap, subcommand
```

## 🔧 Configuration

Configure the marketplace registry URL:

```bash
# Use GitHub Pages marketplace (default)
export GGEN_REGISTRY_URL="https://seanchatmangpt.github.io/ggen/registry/"

# Use local registry for development/testing
export GGEN_REGISTRY_URL="file:///path/to/local/registry/"

# Use custom registry
export GGEN_REGISTRY_URL="https://your-registry.com/"
```

## 📖 Documentation

Learn more about using and contributing to the marketplace:

- [📖 Marketplace Guide](guides/marketplace.md)
- [🏠 Project README](README.md)

---

Built with ❤️ by the ggen community | [GitHub](https://github.com/seanchatmangpt/ggen)
