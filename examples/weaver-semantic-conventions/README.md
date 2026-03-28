# weaver-semantic-conventions

Defines OTel semantic conventions for ggen observability in RDF, then generates a Weaver registry YAML, Rust telemetry constants, and a live-check shell script — all from a single `ggen sync`.

## What it generates

| File | Description |
|------|-------------|
| `generated/semconv-registry.yaml` | OTel Weaver registry with 3 groups (ggen.cli, ggen.codegen, ggen.sync) and their attributes |
| `generated/telemetry_constants.rs` | Rust `pub const` definitions for every semantic convention attribute |
| `generated/live-check.sh` | Shell script that invokes `weaver registry check` against the generated registry |

## Prerequisites

- [ggen](https://github.com/seanchatmangpt/ggen) — install and add `~/.local/bin` to `PATH`
- [weaver](https://github.com/open-telemetry/weaver) — only needed to run `live-check.sh`

## Quick start

```bash
cd examples/weaver-semantic-conventions
~/.local/bin/ggen sync
```

## Expected output tree

```
examples/weaver-semantic-conventions/
  ontology/
    ggen-semconv.ttl          ← RDF source (edit this, not the generated files)
  templates/
    semconv-registry.yaml.tera
    telemetry-constants.rs.tera
    live-check.sh.tera
  generated/
    semconv-registry.yaml     ← Weaver registry (generated)
    telemetry_constants.rs    ← Rust constants (generated)
    live-check.sh             ← Validation script (generated)
  ggen.toml
  README.md
```

## How it works

The ontology (`ontology/ggen-semconv.ttl`) declares three `semconv:Group` resources (`ggen.cli`, `ggen.codegen`, `ggen.sync`) and their associated `semconv:Attribute` nodes. `ggen sync` runs three SPARQL SELECT queries against the loaded RDF graph and renders each result set through a Tera template, writing the three output files in one pass.

To add a new attribute, extend the TTL and re-run `ggen sync`.
