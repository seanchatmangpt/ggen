# Complete Project Generation

Master end-to-end project scaffolding: multi-crate workspaces, build automation, CI/CD, Docker, documentation.

**Status**: ✅ Complete
**Difficulty**: ⭐⭐ Intermediate
**Time**: 30-45 minutes
**Focus**: Complete production-ready project generation

---

## 1. Overview

Generate production-ready multi-crate Rust workspaces with:

- Multi-crate workspace structure (core, api, cli)
- Shared library dependencies
- Build automation (Makefile)
- CI/CD pipeline (GitHub Actions)
- Docker containerization
- Complete documentation
- Deployment strategy

**What this teaches**: Enterprise-scale project generation from specifications. Specification-driven architecture replaces manual setup.

---

## 2. Prerequisites

- ggen CLI (5.0.0+)
- Understanding of Rust workspaces
- Basic Docker knowledge
- Node.js 18+

---

## 3. Quick Start

```bash
cd examples/complete-project-generation

# Validate
./validate.mjs

# Review specification
cat ontology/workspace.ttl

# View generation config
cat ggen.toml

# Inspect golden files
cat golden/generated/Cargo.toml
```

---

## 4. Architecture

### Multi-Crate Workspace

```
workspace/
├── Cargo.toml (workspace root)
├── core/
│   ├── Cargo.toml (shared library)
│   └── src/lib.rs
├── api/
│   ├── Cargo.toml (API service)
│   └── src/main.rs
└── cli/
    ├── Cargo.toml (CLI tool)
    └── src/main.rs
```

### Generation Pipeline

```
RDF Workspace Spec
        ↓
SPARQL Extraction
        ↓
Tera Template Rendering
        ↓
├── Cargo.toml (workspace + crates)
├── Makefile (build automation)
├── CI/CD configuration
├── Docker setup
└── Documentation
```

### Design Principles

1. **Monorepo Structure**: Single repository, multiple crates
2. **Shared Core**: Common library used by all services
3. **Independent Services**: API and CLI can deploy separately
4. **Automation**: Makefile handles all build tasks
5. **Infrastructure as Code**: Dockerfile, CI/CD, and docs generated

---

## 5. File Structure

```
complete-project-generation/
├── ggen.toml                # 10 generation rules
├── ontology/
│   └── workspace.ttl        # RDF workspace spec
├── templates/               # 8 Tera templates
│   ├── workspace-toml.tera
│   ├── crate-toml.tera
│   ├── makefile.tera
│   ├── ci-workflow.tera
│   ├── architecture.tera
│   ├── build-guide.tera
│   ├── deployment.tera
│   ├── docker-compose.tera
│   └── docs-index.tera
├── golden/                  # Expected outputs
│   └── generated/
├── validate.mjs            # Validation script
└── README.md               # This file
```

---

## 6. Step-by-Step Tutorial

### Step 1: Understand Workspace Specification (5 min)

```bash
cat ontology/workspace.ttl
```

Key entities:
- `Workspace`: Top-level definition
- `Crate`: Individual packages (core, api, cli)
- `Service`: Deployable services
- Properties: name, version, description, members

### Step 2: Review Generation Rules (5 min)

```bash
head -50 ggen.toml
```

Rules include:
1. Workspace-level Cargo.toml
2. Per-crate Cargo.toml generation
3. Makefile creation
4. CI/CD workflow generation
5. Documentation generation

### Step 3: Examine Workspace Toml Template (5 min)

```bash
cat templates/workspace-toml.tera
```

Generates:
```toml
[workspace]
members = ["core", "api", "cli"]
```

### Step 4: Review Build Automation (5 min)

```bash
cat templates/makefile.tera
```

Provides targets:
- `make build` - Release build
- `make test` - Run all tests
- `make lint` - Clippy and format checks
- `make clean` - Remove artifacts

### Step 5: Understand CI/CD Pipeline (5 min)

```bash
cat templates/ci-workflow.tera
```

GitHub Actions workflow:
- Checkout code
- Install Rust toolchain
- Build release
- Run tests
- Run linting

### Step 6: Deploy with Docker (5 min)

```bash
cat templates/docker-compose.tera
```

Services:
- API service with port mapping
- Environment configuration
- Volume management

---

## 7. Configuration Reference

### Workspace Configuration

```toml
[workspace]
members = ["core", "api", "cli"]

[workspace.metadata]
name = "myworkspace"
```

### Crate Configuration

Per-crate Cargo.toml:
```toml
[package]
name = "core"
version = "0.1.0"
edition = "2021"
```

### SPARQL Query for Workspace

```sparql
PREFIX ws: <https://ggen.io/ontology/workspace#>
SELECT ?workspaceName ?members
WHERE {
  ?ws a ws:Workspace ;
    ws:name ?workspaceName ;
    ws:members ?members .
}
```

### Template Variables

```tera
workspace_name: string      # Workspace identifier
members: array              # List of crate names
crate_name: string         # Individual crate name
version: string            # Version identifier
description: string        # Human-readable description
```

---

## 8. Troubleshooting

### Crate Generation Fails
- Check: All crates in workspace defined in ontology
- Check: SPARQL queries return crate data
- Check: Template variables match SELECT clause

### Build Errors
- Check: All crates compile independently
- Check: Dependencies are consistent across crates
- Check: Path dependencies use relative paths

### Docker Fails
- Check: Rust toolchain available
- Check: Build happens in Docker context
- Check: Ports not already in use

### CI/CD Issues
- Check: YAML is valid (templates/ci-workflow.tera)
- Check: GitHub Actions permissions granted
- Check: Secrets configured if needed

---

## 9. Next Steps

### Advanced Topics
- Multi-workspace deployments
- Cross-crate dependency management
- Shared build configuration
- Platform-specific builds

### Practice Exercises
1. Add a fourth crate to the workspace
2. Update Makefile with new targets
3. Modify CI/CD to add security scanning
4. Create additional Docker services

### Real-World Application
- Generate microservice architecture
- Scale to 10+ crates
- Implement custom build steps
- Integrate with monitoring/logging

### Related Examples
- See `simple-project/` for single-crate projects
- See `openapi/` for API specification generation
- See `ai-template-creation/` for AI-assisted scaffolding

---

## Summary

Complete project generation demonstrates:
- Multi-crate workspace orchestration
- Specification-driven architecture
- Automated build and deploy
- Infrastructure as code (Makefile, Docker, CI/CD)
- Production-ready scaffolding

**Key Learning**: From RDF specification to deployment-ready project in one command.

---

**Status**: GREEN ✓
**Quality Gates**: All passed
**Reproducibility**: 100% deterministic
