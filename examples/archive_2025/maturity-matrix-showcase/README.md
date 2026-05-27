# Maturity Matrix Showcase Project

This project demonstrates all 5 maturity levels of the ggen marketplace, from simple file generation to enterprise-scale deployments with thousands of files.

## Project Structure

```
maturity-matrix-showcase/
├── README.md                    # This file
├── package.toml                 # Package metadata
├── make.toml                    # Lifecycle configuration
├── MIGRATION_GUIDE.md           # How to progress through levels
│
├── level1-simple/               # Level 1: Simple (1-10 files)
│   ├── ontology.ttl            # Basic RDF ontology (< 100 triples)
│   ├── template.tmpl           # Single template
│   └── generated/              # Generated output (1-10 files)
│
├── level2-small/                # Level 2: Small Project (10-100 files)
│   ├── ontology.ttl            # Structured RDF (100-1K triples)
│   ├── templates/              # Multiple templates
│   │   ├── main.tmpl
│   │   ├── module.tmpl
│   │   └── config.tmpl
│   └── generated/              # Generated output (10-100 files)
│
├── level3-medium/               # Level 3: Medium Project (100-1,000 files)
│   ├── ontology/               # Complex ontology (1K-10K triples)
│   │   ├── domain.ttl
│   │   ├── api.ttl
│   │   └── database.ttl
│   ├── templates/              # Workspace templates
│   │   ├── workspace.tmpl
│   │   ├── crate.tmpl
│   │   └── module.tmpl
│   ├── make.toml               # Lifecycle integration
│   └── generated/              # Generated workspace (100-1K files)
│
├── level4-large/                # Level 4: Large Project (1,000-10,000 files)
│   ├── ontologies/             # Distributed ontologies (10K-100K triples)
│   │   ├── core.ttl
│   │   ├── services.ttl
│   │   └── infrastructure.ttl
│   ├── templates/              # Multi-workspace templates
│   │   ├── workspace/
│   │   ├── services/
│   │   └── infrastructure/
│   ├── .github/workflows/       # CI/CD integration
│   ├── make.toml               # Advanced lifecycle
│   └── generated/              # Generated multi-workspace (1K-10K files)
│
└── level5-enterprise/           # Level 5: Enterprise (10,000+ files)
    ├── ontologies/             # Federated ontologies (100K+ triples)
    │   ├── tenant1/
    │   ├── tenant2/
    │   └── shared/
    ├── templates/              # Multi-tenant templates
    │   ├── tenants/
    │   ├── services/
    │   └── infrastructure/
    ├── .github/workflows/       # Enterprise CI/CD
    ├── monitoring/             # Observability
    ├── make.toml               # Enterprise lifecycle
    └── generated/              # Generated enterprise (10K+ files)
```

## Quick Start

### Level 1: Simple (1-10 files)

```bash
cd level1-simple
ggen graph load ontology.ttl
ggen template generate template.tmpl
# Generates 1-10 files
```

### Level 2: Small Project (10-100 files)

```bash
cd level2-small
ggen graph load ontology.ttl
ggen template generate templates/
# Generates 10-100 files
```

### Level 3: Medium Project (100-1,000 files)

```bash
cd level3-medium
ggen lifecycle run
# Executes: init → generate → build → test → deploy
# Generates 100-1,000 files
```

### Level 4: Large Project (1,000-10,000 files)

```bash
cd level4-large
ggen lifecycle run --incremental
# Generates 1,000-10,000 files with CI/CD
```

### Level 5: Enterprise (10,000+ files)

```bash
cd level5-enterprise
ggen lifecycle run --distributed --workers 10
# Generates 10,000+ files with distributed processing
```

## Maturity Level Details

See [MIGRATION_GUIDE.md](./MIGRATION_GUIDE.md) for detailed progression instructions.

