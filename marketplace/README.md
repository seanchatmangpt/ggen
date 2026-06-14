# ggen Marketplace

**Production-ready package registry for code generation templates, utilities, and AI integrations**

## 🚀 Quick Start

```bash
# Search for packages
ggen marketplace search "rust api"

# Install a package
ggen marketplace install "advanced-rust-api-8020"

# List installed packages
ggen marketplace list

# Search for package details
ggen marketplace search "advanced-rust-api-8020"
```

## 📖 Documentation

- **[Validation Guide](#validation)** - Package production readiness validation
- **[Package Registry Index](index.json)** - Local database of all 76+ marketplace packages
- **[Ontology Definition](ontology.ttl)** - RDF ontology definition for packages

## ✅ Validation

The marketplace includes a comprehensive validation system to ensure package production readiness.

### Quick Validation

```bash
# Validate all packages
cargo make marketplace-validate

# Generate validation reports
cargo make marketplace-report

# Validate and update production flags
cargo make marketplace-validate-update
```

### Validation Criteria

**Required Checks (Critical - 60% weight)**:
- ✅ `package.toml` - Complete metadata with version, description, license
- ✅ `README.md` - Documentation with examples (100+ characters)
- ✅ Source code - At least `src/main.rs` or `src/lib.rs` exists (or `templates/` for template-only packages)
- ✅ License file - `LICENSE`, `LICENSE-MIT`, or `LICENSE-APACHE` present

**Quality Checks (Bonus - 40% weight)**:
- ✅ RDF ontology - `rdf/ontology.ttl` with 200+ lines (if applicable)
- ✅ SPARQL queries - `sparql/*.rq` files (if applicable)
- ✅ Examples - Runnable examples in `examples/` directory
- ✅ Tests - Test files in `tests/` directory
- ✅ Documentation - Additional docs in `docs/` directory

### Scoring

- **95%+**: Production ready ✅
- **80-94%**: Needs improvement ⚠️
- **<80%**: Not ready ❌

### Validation Reports

Reports are generated in:
- **Markdown**: `marketplace/VALIDATION_REPORT.md`
- **JSON**: `marketplace/validation_results.json`

### Updating Production Flags

The validation system can automatically update `production_ready` flags in:
- `package.toml` files (under `[package.metadata]`)
- `marketplace/registry/index.json`

Use `cargo make marketplace-validate-update` to validate all packages and update flags based on validation results.

## 🌐 Live Marketplace

- **Registry URL**: `https://seanchatmangpt.github.io/ggen/marketplace/registry/index.json`
- **Repository**: `https://github.com/seanchatmangpt/ggen`
- **Documentation**: `https://seanchatmangpt.github.io/ggen/`

## 📦 What You Can Find

### Templates
- **REST APIs**: Production-ready API servers (Axum, Actix, Rocket)
- **CLI Applications**: Command-line tools with clap
- **Microservices**: Complete service architectures
- **GraphQL**: GraphQL servers with subscriptions

### Utilities
- **Code Generators**: AI-powered code generation
- **Testing Tools**: Test automation and fixtures
- **DevOps**: Deployment and CI/CD configurations

### AI Integrations
- **LLM Providers**: OpenAI, Anthropic, Ollama integrations
- **Prompt Templates**: Reusable AI prompts
- **RAG Systems**: Retrieval-augmented generation

## 🎯 Key Features

### For Users
- **Fast Search**: Find packages by name, tags, or keywords
- **Safe Installation**: Dry-run and verification options
- **Dependency Management**: Automatic dependency resolution
- **Version Control**: Install specific versions

### For Publishers
- **Simple Publishing**: CLI-based package submission
- **Automatic Deployment**: CI/CD via GitHub Pages
- **Semantic Versioning**: Built-in version management
- **Community Reach**: Discoverable by all ggen users

## 📊 Popular Packages

```bash
# Production REST API with authentication
ggen marketplace install "advanced-rust-api-8020"

# Comprehensive Rust showcase
ggen marketplace install "comprehensive-rust-showcase"

# AI-powered microservice
ggen marketplace install "ai-microservice"
```

## 🏗️ Package Structure

Every package includes:
- **README.md**: Documentation and examples
- **make.toml**: Lifecycle management
- **src/**: Source code
- **templates/**: Code generation templates (optional)
- **data/**: SPARQL/RDF specs (optional)
- **tests/**: Test suite

## 🔐 Security & Trust

- All packages reviewed before publication
- Source code visible in GitHub repository
- SHA256 checksums for verification
- License compliance checking
- No telemetry or tracking

## 🤝 Contributing

We welcome package contributions! See:
- **[CONTRIBUTING.md](../CONTRIBUTING.md)** for code standards
- **[CODE_OF_CONDUCT.md](../CODE_OF_CONDUCT.md)** for community guidelines

## 📞 Support

- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
- **Documentation**: https://seanchatmangpt.github.io/ggen/

## 🎓 Learning Resources

1. **Start**: Explore the [Registry Index](index.json) to see available packages
2. **Standard**: Adhere to the quality checklist in the [Validation Guide](#validation)
3. **Master**: Dive into the core [Ontology TTL](ontology.ttl) schema

---

**Ready to get started?** Run `ggen marketplace search` to explore available packages!
