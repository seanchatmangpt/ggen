# ggen v6 - 80/20 Prioritization Summary

**Date**: 2025-12-20
**Feature**: 013-ggen-v6-rdf-system
**Goal**: Identify the 20% of features that deliver 80% of value

---

## Executive Summary

This document maps the **system-level user stories** to the **80/20 implementation phases** defined in `ontology/mvp-80-20.ttl`.

**The Core Insight**: Proving the constitutional equation `code = μ(spec.ttl)` works requires only 5 capabilities (8 days). Everything else is optimization.

---

## CORE 20% (Must Have - MVP Phase 1)

**Total Effort**: 8 developer-days
**Value Delivered**: 80% (proves RDF-first methodology works)

### User Stories in MVP Phase 1

| Story | Title | Priority | Capabilities | Effort |
|-------|-------|----------|--------------|--------|
| **US-001** | Machine-Readable RDF Specifications | P1 | CAP-001 (TTL Parsing)<br>CAP-002 (SPARQL) | 4 days |
| **US-002** | Deterministic Pipeline | P1 | CAP-003 (Tera)<br>CAP-004 (Determinism) | 2 days |
| **US-003** | Template-Driven Generation | P1 | CAP-005 (CLI) | 2 days |

### The 5 Core Capabilities

#### CAP-001: Parse TTL → RDF Graph (2 days)
**Why Essential**: Foundation. Without this, nothing works.

```rust
use oxigraph::store::Store;
let store = Store::new()?;
store.load_from_path("ontology/feature-content.ttl", GraphFormat::Turtle)?;
```

**Not Implementing**:
- ❌ Multiple file loading
- ❌ Remote file fetching
- ❌ Format auto-detection
- ❌ Streaming for large files

---

#### CAP-002: Execute SPARQL SELECT (2 days)
**Why Essential**: Extract data from RDF. Heart of μ₂ stage.

```rust
let query = "SELECT ?title ?priority WHERE { ?s sk:title ?title ; sk:priority ?priority }";
let results = store.query(query)?;
```

**Not Implementing**:
- ❌ CONSTRUCT queries
- ❌ Query optimization
- ❌ Query validation beyond syntax
- ❌ Streaming results

---

#### CAP-003: Render Tera Template (1 day)
**Why Essential**: Transform data to code. Heart of μ₃ stage.

```rust
let tera = Tera::new("templates/*.tera")?;
let context = tera::Context::from_serialize(sparql_results)?;
let output = tera.render("spec.tera", &context)?;
fs::write("generated/spec.md", output)?;
```

**Not Implementing**:
- ❌ Template inheritance
- ❌ Custom filters
- ❌ Template caching
- ❌ Error recovery

---

#### CAP-004: Deterministic Output (1 day)
**Why Essential**: SHA256 hashes match. Proves constitutional equation.

```rust
#[test]
fn test_determinism() {
    let output1 = generate_from_ttl("feature.ttl");
    let output2 = generate_from_ttl("feature.ttl");
    assert_eq!(sha256(&output1), sha256(&output2));
}
```

**Implementation**:
- Sort RDF triples before querying
- Use `ORDER BY` in SPARQL queries
- No timestamps in templates
- No randomness anywhere

---

#### CAP-005: ggen sync Command (2 days)
**Why Essential**: User-facing CLI. Makes it usable.

```rust
fn sync() -> Result<()> {
    let config: GgenConfig = toml::from_str(&fs::read_to_string("ggen.toml")?)?;
    for gen in config.generation {
        let store = load_ttl(&gen.source)?;
        let results = execute_sparql(&store, &gen.query)?;
        let output = render_template(&gen.template, results)?;
        fs::write(&gen.output, output)?;
    }
    Ok(())
}
```

**Minimal ggen.toml**:
```toml
[[generation]]
source = "ontology/feature-content.ttl"
query = "SELECT ?title WHERE { ?s sk:title ?title }"
template = "templates/spec.tera"
output = "generated/spec.md"
```

---

## DEFERRED 80% (Nice to Have - Phase 2)

**Total Effort**: ~32 developer-days
**Value Delivered**: 20% (quality-of-life improvements)

### User Stories in Phase 2

| Story | Title | Priority | Deferred Feature | Workaround |
|-------|-------|----------|------------------|------------|
| **US-004** | Idempotent Transformations | P2 | DEF-003: Idempotence Checks | Regenerate everything, ignore git noise |
| **US-005** | Cryptographic Provenance | P2 | DEF-002: Receipts | Use git commit hashes |

### The 8 Deferred Features

| # | Feature | Why Deferred | Workaround | Implement When |
|---|---------|--------------|------------|----------------|
| **DEF-001** | SHACL Validation | Can validate manually | Use external validator | After MVP proves value |
| **DEF-002** | Cryptographic Receipts | Not blocking generation | Use git commit hashes | When compliance demands proof |
| **DEF-003** | Idempotence Checks | Determinism is enough initially | Regenerate everything | When developers complain about git noise |
| **DEF-004** | Multi-File Loading | Single file works for MVP | Concatenate files | When specs grow > 1000 lines |
| **DEF-005** | CONSTRUCT Queries | SELECT covers 95% of cases | Creative SELECT patterns | When template logic becomes complex |
| **DEF-006** | Template Inheritance | Can duplicate code initially | Copy/paste template blocks | When template duplication becomes painful |
| **DEF-007** | Error Recovery | Fail-fast is acceptable | Fix and re-run | When used in CI/CD |
| **DEF-008** | Performance Optimization | Specs are small | Wait a few seconds | When specs exceed 100K triples |

---

## MVP Success Criteria

**Can you**:
1. ✅ Write `feature-content.ttl` (RDF spec)
2. ✅ Write `spec.tera` (Tera template)
3. ✅ Define SPARQL query in `ggen.toml`
4. ✅ Run `ggen sync`
5. ✅ Get `spec.md` generated from TTL
6. ✅ Run `ggen sync` again → Same output (deterministic)
7. ✅ Change TTL → Run `ggen sync` → Updated spec.md

**If YES to all → MVP is complete!**

---

## Validation Strategy

### Phase 1: Testcontainers (Current)
- ✅ **RDF Fixtures**: `tests/integration/fixtures/feature-content.ttl` (35 triples)
- ✅ **Python Tests**: `test_ggen_sync.py` validates end-to-end workflow
- ✅ **Constitutional Equation**: Tests verify `spec.md = μ(feature.ttl)`
- ✅ **Idempotence**: Tests verify `μ∘μ = μ`
- ✅ **Determinism**: SHA256 hash comparison across runs

### Phase 2: MVP Implementation (Next)
Once the 5 core capabilities are implemented:
1. Replace fixtures with real ggen v6 execution
2. Validate all MVP success criteria pass
3. Use ggen v6 to regenerate its own specification (self-hosting)

---

## RDF Markers in Specification

All user stories in `ontology/feature-content.ttl` are now marked with:

```turtle
# Core 20% Example
:us-001 a sk:UserStory ;
    sk:implementationPhase "MVP-Phase-1" ;
    sk:eightyTwentyCategory "Core-20-Percent" ;
    sk:estimatedEffort "4 days (2 for CAP-001 + 2 for CAP-002)" ;
    # ... other properties

# Deferred 80% Example
:us-004 a sk:UserStory ;
    sk:implementationPhase "Phase-2-Deferred" ;
    sk:eightyTwentyCategory "Deferred-80-Percent" ;
    sk:deferRationale "Determinism ensures same output; idempotence is optimization" ;
    sk:workaround "Always regenerate all files initially" ;
    sk:implementWhen "When developers complain about git diffs" ;
    # ... other properties
```

---

## What This Proves

**With just 20% effort (8 days)**:

✅ **RDF-first methodology works**
- Specs in machine-readable format
- Code generated automatically
- No manual interpretation gap

✅ **Constitutional equation proven**
- code = μ(spec.ttl)
- Deterministic transformation
- Cryptographically verifiable

✅ **Zero drift**
- Spec changes → Code regenerates
- No manual sync needed
- Impossible for spec and code to diverge

---

## Next Steps After MVP

1. **Dogfood**: Use ggen v6 to regenerate its own specs
2. **Showcase**: Demo to 5 developers, get feedback
3. **Iterate**: Add most-requested deferred feature
4. **Scale**: Apply to larger project (ggen marketplace)
5. **Refine**: Based on real usage, optimize bottlenecks

---

## The 80/20 Philosophy

**Focus on**:
- Does it work?
- Is it deterministic?
- Can I generate code from specs?

**Ignore (initially)**:
- Is it fast?
- Is it perfect?
- Does it handle edge cases?

**8 days to prove concept > 40 days to build perfect tool that nobody uses**

---

**Remember**: The goal isn't to build the perfect tool. The goal is to **prove RDF-first methodology eliminates drift**. MVP accomplishes that with 20% effort.
