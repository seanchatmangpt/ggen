# Getting Started with ggen

Welcome to **ggen** - the RDF-based code generation toolkit that transforms knowledge graphs into production-ready code.

## 🚀 New to ggen?

Start here to go from zero to your first generated code in 10 minutes:

1. **[Quick Start Tutorial](quick-start.md)** ⭐ *Start here!*
   - Install ggen
   - Generate JavaScript with Zod schemas from an ontology
   - Query RDF data with SPARQL
   - Time: 10 minutes

2. **[Your First Generation](first-generation.md)**
   - Detailed walkthrough of your first code generation
   - Understanding what ggen does behind the scenes
   - Time: 15 minutes

## 📚 Next Steps

Once you've completed the Quick Start:

### Learn Core Workflows
- **[Tutorials](../tutorials/core/)** - Step-by-step learning paths for essential workflows
  - Generate code from RDF ontologies
  - Create custom templates
  - Build full-stack projects

### Solve Specific Problems
- **[How-to Guides](../how-to/)** - Solutions to common tasks
  - Generate JavaScript + Zod schemas from Schema.org
  - Create CLI tools from ontologies
  - Query RDF data with SPARQL

### Understand Concepts
- **[Explanations](../explanations/)** - Deep dives into ggen's approach
  - What is Ontology-Driven Development?
  - How RDF powers code generation
  - Template system architecture

### Look Up Commands
- **[Reference](../reference/)** - Complete command documentation
  - All CLI commands and flags
  - Template syntax reference
  - API documentation

## 💡 Common Use Cases

**"I want to generate Zod schemas from my domain model"**
→ Start with [Quick Start](quick-start.md), then [Generate JavaScript + Zod](../how-to/generation/generate-javascript-zod.md)

**"I need to scaffold a new project with best practices"**
→ See [Project Commands](../reference/commands/complete-cli-reference.md#project-commands)

**"I want to query my ontology for specific data"**
→ Follow [Query RDF with SPARQL](../how-to/generation/query-rdf-sparql.md)

**"I'm building a CLI tool and want consistency"**
→ Check out [Create CLI Tool from Ontology](../how-to/generation/create-cli-tool.md)

## 🆘 Need Help?

- **Installation Issues**: See [Installation Guide](#installation)
- **Errors**: Check [Troubleshooting](troubleshooting.md)
- **Questions**: [GitHub Discussions](https://github.com/seanchatmangpt/ggen/discussions)
- **Bugs**: [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)

---

## Installation

### Homebrew (macOS/Linux)
```bash
brew install seanchatmangpt/ggen/ggen
```

### Cargo (Any Platform)
```bash
cargo install ggen-cli-lib
```

### From Source
```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo install --path crates/ggen-cli
```

### Verify Installation
```bash
ggen --version
```

**Ready to start?** → [Quick Start Tutorial](quick-start.md)
