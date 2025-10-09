# Changelog

All notable changes to this project will be documented in this file.

## [0.1.0] - 2025-01-XX

### Added
- Initial release of rgen CLI tool
- Language-agnostic code generation from RDF ontologies
- Template system with YAML frontmatter support
- Marketplace ecosystem for reusable rpacks (search, add, remove, update, packs)
- Deterministic code generation with manifest hashing
- Multi-language template support (Rust, Python, Bash, TypeScript, Go)
- CLI commands: gen, list, show, lint, validate, graph, hazard, completion
- RDF/SPARQL integration for semantic code generation
- Template validation and linting
- Shell completion generation (bash, zsh, fish)
- Comprehensive documentation and examples

### Features
- **Marketplace**: Search, install, and manage reusable code generation packs
- **Templates**: YAML frontmatter with RDF, SPARQL, and determinism support
- **Generation**: Deterministic, reproducible code projection from ontologies
- **Validation**: Template and RDF graph validation with SHACL
- **Performance**: Meets all SLOs (build ≤15s, incremental ≤2s, generation ≤3s)

### Documentation
- Installation guide
- Quick start tutorial
- Template development guide
- Marketplace usage instructions
- RDF/SPARQL integration guide
- CLI reference
- Troubleshooting guide

[0.1.0]: https://github.com/seanchatmangpt/rgen/releases/tag/v0.1.0