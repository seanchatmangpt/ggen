<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Marketplace Ecosystem](#marketplace-ecosystem)
  - [Overview](#overview)
  - [Gpack Structure](#gpack-structure)
  - [Gpack Manifest](#gpack-manifest)
  - [Using Gpacks](#using-gpacks)
    - [Search](#search)
    - [Install](#install)
    - [Use](#use)
  - [Version Management](#version-management)
  - [Publishing Gpacks](#publishing-gpacks)
  - [Best Practices](#best-practices)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Marketplace Ecosystem

The ggen marketplace provides a curated ecosystem of reusable code generation templates (gpacks).

## Overview

The marketplace is hosted at [seanchatmangpt.github.io/ggen](https://seanchatmangpt.github.io/ggen/) and provides:

- **Transparency**: All templates are open source
- **Reliability**: GitHub Pages with CDN-backed infrastructure
- **Community**: Easy contribution via pull requests
- **Free**: No additional hosting costs
- **Automated**: CI/CD validates and publishes updates

## Gpack Structure

Gpacks are versioned template collections:

```
<gpack-id>/
├── ggen.toml          # Gpack manifest
├── templates/          # Template files
│   └── rust-models/
│       └── models.rs.tmpl
├── macros/            # Reusable fragments
└── tests/             # Golden tests
```

## Gpack Manifest

```toml
[gpack]
id = "io.ggen.rust.models"
name = "Rust Models"
version = "0.2.1"
description = "Generate Rust structs from RDF"
license = "MIT"
ggen_compat = ">=0.2 <0.4"

[dependencies]
"io.ggen.macros.std" = "^0.2"

[templates]
entrypoints = ["rust-models/models.rs.tmpl"]
```

## Using Gpacks

### Search

```bash
ggen marketplace search "rust api"
```

### Install

```bash
ggen marketplace install io.ggen.rust.models
```

### Use

```bash
ggen template generate-rdf \
  --ontology domain.ttl \
  --template io.ggen.rust.models \
  --output src/
```

## Version Management

Gpacks use semantic versioning and lockfiles:

```toml
# ggen.lock
[gpacks]
"io.ggen.rust.models" = "0.2.1"
```

**Benefits:**
- Reproducible builds
- Version pinning
- Dependency resolution

## Publishing Gpacks

1. Create gpack structure
2. Write `ggen.toml` manifest
3. Add templates and tests
4. Submit pull request to registry

## Best Practices

1. **Version pinning**: Use specific versions in production
2. **Lockfiles**: Commit `ggen.lock` to version control
3. **Validation**: Test gpacks before publishing
4. **Documentation**: Document template usage and variables

## See Also

- [Marketplace Workflow Tutorial](../tutorials/marketplace-workflow.md)
- [CLI Reference](../reference/cli.md#marketplace-commands)
- [Create Templates Guide](../how-to-guides/create-templates.md)

