# 📚 ggen Documentation Index

Complete knowledge base for ggen - the ontology-driven code generator.

## 🚀 Quick Start

| Document | Purpose | Time |
|----------|---------|------|
| [README.md](../README.md) | Project overview and 5-minute quickstart | 10 min |
| [Poka-Yoke Quick Reference](./poka-yoke-quick-reference.md) | Mistake-proofing and safe code generation | 15 min |
| [Bug Reporting Guide](../BUG_REPORTING_GUIDE.md) | How to report issues effectively | 5 min |

## 📖 Core Documentation

### Advanced Capabilities (80/20 - Most Valuable)
- [SPARQL Inference Guide](./SPARQL_INFERENCE_GUIDE.md) - **CRITICAL**: Master CONSTRUCT queries for intelligent code generation (5 patterns + real examples)
- [Graph Querying API](./GRAPH_QUERYING_API.md) - Programmatic RDF access, SPARQL SELECT/CONSTRUCT/ASK, integration patterns
- [AI/AGI Integration Guide](./AI_AGI_INTEGRATION_GUIDE.md) - Build agentic systems that learn from generated code, 4-layer architecture

### Design & Architecture
- [GGEN-V5-DESIGN.md](../GGEN-V5-DESIGN.md) - v5.0 architecture and philosophy
- [GGEN-V5-STRATEGY.md](../GGEN-V5-STRATEGY.md) - v5.0 strategy and implementation roadmap

### Performance & Quality
- [PERFORMANCE.md](../PERFORMANCE.md) - Performance characteristics and benchmarks
- [TESTING.md](../TESTING.md) - Testing strategy and conventions

### Deployment
- [DEPLOYMENT_STATUS.md](../DEPLOYMENT_STATUS.md) - Deployment status and checklist
- [DOCKER.md](../DOCKER.md) - Docker deployment guide

## 🏗️ Project Structure

```
ggen/
├── .specify/              # RDF-first specifications
│   ├── ontology/          # Ontology schemas
│   ├── memory/            # Project constitution and memory
│   ├── specs/             # Feature specifications (RDF source)
│   └── templates/         # Tera templates for generation
│
├── crates/                # Rust workspace crates
│   ├── ggen-cli/          # Command-line interface
│   ├── ggen-core/         # Core code generation engine
│   ├── ggen-domain/       # Domain models and poka-yoke
│   ├── ggen-config/       # Configuration parsing
│   ├── ggen-e2e/          # End-to-end testing framework
│   └── [other crates]
│
├── docs/                  # Documentation files
├── specs/                 # Feature specifications (markdown + source)
└── templates/             # Code generation templates
```

## 🎯 By Feature

### Poka-Yoke (Mistake-Proofing)
- [Poka-Yoke Quick Reference](./poka-yoke-quick-reference.md)
- See: [Spec 006 - FMEA Marketplace](../specs/006-marketplace-fmea-poka-yoke/)

### End-to-End Testing
- See: [Spec 011 - E2E Testcontainers](../specs/011-e2e-testcontainers/)
- Implementation: `crates/ggen-e2e/`

### Code Generation
- See: [Spec 008 - N3 Code Generation](../specs/008-n3-code-gen/)
- Core implementation: `crates/ggen-core/`

### RDF & Specifications
- See: [Spec 001 - TTL Validation CLI](../specs/001-ttl-validation-cli/)
- See: [Spec 005 - TTL SHACL Validation](../specs/005-ttl-shacl-validation/)

## 🔗 External Resources

- **GitHub Repository**: https://github.com/seanchatmangpt/ggen
- **Crates.io**: https://crates.io/crates/ggen
- **Docs.rs**: https://docs.rs/ggen

## 📝 Contributing

Before contributing, review:
- [README.md](../README.md) for project philosophy
- [TESTING.md](../TESTING.md) for test conventions
- [PERFORMANCE.md](../PERFORMANCE.md) for performance guidelines

## 🔄 Version Information

**Current Version**: v5.0.2+

Key changes from v4:
- **Single command**: `ggen sync` (no `ggen generate`, `ggen validate`)
- **Configuration**: All settings in `ggen.toml` (no CLI flags)
- **Poka-Yoke**: Built-in mistake-proofing for safe code generation
- **End-to-End Testing**: Full testcontainers integration

---

**Last Updated**: December 23, 2025
