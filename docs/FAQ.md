# Frequently Asked Questions

## About ggen

### What is ggen?

ggen is a specification-driven code generation tool that transforms RDF ontologies into typed source code. It works through a five-stage deterministic pipeline:

1. **μ₁ (Load)** — Load RDF ontology
2. **μ₂ (Extract)** — Execute SPARQL queries
3. **μ₃ (Render)** — Transform results with Tera templates
4. **μ₄ (Canonicalize)** — Normalize output
5. **μ₅ (Receipt)** — Generate cryptographic proof

### Why would I use ggen?

You should use ggen if:
- You want code generation **driven by a domain ontology** (single source of truth)
- You need **deterministic, reproducible** code generation
- You want to **generate multiple languages** from one ontology
- You need **offline code generation** (no network dependency)
- You want **type-safe code** based on a formal schema

### Is ggen a code formatter?

No. ggen generates code from ontologies. It's similar to:
- OpenAPI → API server generators (like Swagger Codegen)
- Protocol Buffers → language bindings
- GraphQL schema → API resolvers

It's _not_ a formatter like Prettier or Black.

### How is ggen different from OpenAPI/Swagger?

| Feature | ggen | OpenAPI |
|---------|------|---------|
| **Source format** | RDF/Turtle | JSON/YAML |
| **Expressiveness** | Full semantic web | REST-specific |
| **Code generation** | Any language | REST/SDK only |
| **Offline** | ✅ Yes (embedded) | ❌ Requires internet |
| **Deterministic** | ✅ Yes (with receipts) | ❌ Tool-dependent |

---

## Ontology Embedding

### Why embed ontologies?

Embedding means including ontologies in the binary at compile time. Benefits:

1. **Zero network dependency** — Generate code completely offline
2. **Reproducible builds** — Same input always produces same output
3. **Faster lookup** — <1 microsecond (vs 100 ms network)
4. **Smaller binary** — Only 12 core ontologies (448 KB)

### Which ontologies are embedded?

Currently 12 W3C standard ontologies:
- RDF, RDFS, OWL (foundational)
- Dublin Core, DCAT (metadata)
- FOAF, vCard (people/orgs)
- SKOS, PROV (controlled vocab)
- And 3 more...

See `ggen ontology list --embedded`.

### Can I embed custom ontologies?

Not yet. Currently only W3C standards are embedded (compiled from source).

**Future options:**
- **Phase 7**: Feature-gate to customize which ontologies are embedded
- **Phase 8**: Custom embedding build process

**Current solution**: Use marketplace packages or load from file.

### Why two-tier architecture (embedded + marketplace)?

**Embedded tier** (compile-time):
- Small (12 ontologies, 448 KB)
- Fast (<1 μs)
- Always available offline

**Marketplace tier** (runtime):
- Large (100+ ontologies)
- Slower (100 ms first load)
- Requires network (or cache)
- Optional (only install what you need)

This splits the tradeoff: small + fast core, large + optional extras.

---

## Installation & Configuration

### Do I need Rust installed?

To install ggen from cargo: **Yes, you need Rust.**

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

But binary distributions are planned for v26.6.x.

### Can I use ggen offline?

**Yes**, if you:
1. Use only embedded ontologies (12 W3C standards)
2. Use marketplace packages you've already cached
3. Load ontologies from local files

### How do I use ggen in CI/CD?

Use lock files for reproducibility:

```bash
# 1. Commit ggen.lock to version control
git add ggen.lock

# 2. In CI, use --locked flag
ggen sync --locked

# 3. Fail if generated code changes
git diff --exit-code || exit 1
```

### Can I use ggen with Docker?

Yes:

```dockerfile
FROM rust:1.91 as builder
RUN cargo install ggen-cli

FROM debian:bookworm-slim
COPY --from=builder /usr/local/cargo/bin/ggen /usr/bin/
RUN ggen sync
```

---

## Code Generation

### What formats can ggen generate?

Any format is possible via Tera templates. Common targets:
- **JSON/YAML** — Data formats
- **TypeScript/JavaScript** — Web
- **Python/Go/Rust** — Backend
- **SQL** — Database schemas
- **Markdown** — Documentation
- **Custom** — Anything text-based

### Can ggen generate tests?

Not directly. But you can:

1. Generate test fixtures from ontology
2. Generate test templates (parameterized)
3. Use generated code in tests

Example:
```sparql
SELECT ?testName ?input ?expectedOutput
WHERE {
  ?test a ex:TestCase .
  ?test ex:name ?testName .
  ?test ex:input ?input .
  ?test ex:expectedOutput ?expectedOutput .
}
```

Then render as test code.

### How do I generate multiple files?

Use multiple `[[pipelines.steps]]` in `ggen.toml`:

```toml
[[pipelines.steps]]
name = "models"
sparql = "SELECT ?class ..."
output_file = "src/models.rs"

[[pipelines.steps]]
name = "api"
sparql = "SELECT ?endpoint ..."
output_file = "src/api.rs"

[[pipelines.steps]]
name = "docs"
sparql = "SELECT ?doc ..."
output_file = "docs/api.md"
```

Run all:
```bash
ggen sync

# Or specific ones
ggen sync --step models
ggen sync --steps models,api
```

### What if my template needs custom logic?

Use Tera filters and functions:

```jinja2
{# Built-in Tera filters #}
{{ text | uppercase }}
{{ items | length }}
{{ date | date(format="%Y-%m-%d") }}

{# Custom filters (registered in ggen) #}
{{ uri | to_namespace }}
{{ class | to_rust_type }}
{{ property | to_json_field }}
```

Custom filters are defined in ggen-core source.

---

## Marketplace & Packages

### Is there a marketplace?

The marketplace infrastructure is implemented (Phase 4 complete). The public marketplace registry is coming in Phase 7.

Currently you can:
- Use built-in packages (install from local cache)
- Load ontologies from URLs
- Load from files

### How do I add my ontologies to the marketplace?

Not yet available. Coming in Phase 7:

1. Create account on registry
2. Package your ontology
3. Submit for review
4. Publish

### Can I use a private marketplace?

Not yet. Coming in Phase 7 as part of enterprise features.

### How do marketplace packages work?

Packages are **versioned ontologies**:

```bash
# Install
ggen ontology install financial/banking@1.2.1

# Automatic caching
~/.ggen/packages/financial/banking/1.2.1/

# Checksum verification
sha256: abc123...def456...

# Use in ggen.toml
ontology_uri = "marketplace:financial/banking@1.2.1"
```

---

## Performance & Reproducibility

### Is code generation deterministic?

**Yes**. Every `ggen sync` with the same inputs produces identical outputs.

Verified via SHA-256 hashing:

```bash
ggen sync
cat .ggen/receipts/latest.json | jq .output_hash
# sha256:abc123...

ggen sync  # Run again
cat .ggen/receipts/latest.json | jq .output_hash
# sha256:abc123...  (identical!)
```

### How fast is ggen?

Typical performance:
- **Embedded ontologies**: <200 ms total
- **Marketplace (cached)**: <300 ms total
- **Marketplace (first load)**: ~1 second (download overhead)

Benchmark:
```bash
ggen sync --audit
# μ₁: 45 ms
# μ₂: 128 ms
# μ₃: 234 ms
# μ₄: 12 ms
# μ₅: 8 ms
# Total: 427 ms
```

### Why are my builds slow?

Likely causes:
1. **First marketplace load** — Downloads large package (~500 ms)
2. **Complex SPARQL** — Query has many patterns or OPTIONAL clauses
3. **Large ontology** — Millions of triples (slow to process)
4. **Slow disk** — NAS or cloud storage

Solutions:
1. Cache packages: `ggen sync` second time is faster
2. Simplify SPARQL: Remove unnecessary OPTIONAL patterns
3. Add LIMIT to SPARQL: `LIMIT 1000`
4. Use embedded ontologies (fastest)

### How do I verify reproducibility?

```bash
# First run
ggen sync

# Save output hash
HASH1=$(cat .ggen/receipts/latest.json | jq -r .output_hash)

# Second run
ggen sync

# Compare hashes
HASH2=$(cat .ggen/receipts/latest.json | jq -r .output_hash)

if [ "$HASH1" = "$HASH2" ]; then
  echo "✅ Reproducible!"
else
  echo "❌ Hashes differ"
fi
```

---

## Testing & Validation

### How do I test generated code?

1. **Unit test the generation**:
   ```bash
   cargo test -p ggen-core --test pipeline_embedded_ontologies_test
   ```

2. **Integration test the generated code**:
   ```bash
   ggen sync
   cargo test
   ```

3. **Verify reproducibility**:
   ```bash
   ggen sync --audit
   ```

### What are the 8 Canonical Proof Gates?

ggen validates generated code through 8 gates:

| Gate | Validates |
|------|-----------|
| 1. Schema validation | SPARQL SELECT matches schema |
| 2. Ontology consistency | OWL/RDFS constraints |
| 3. Projection proof | Generated code matches ontology |
| 4. Compilation proof | Code compiles without errors |
| 5. Receipt validation | Cryptographic signature valid |
| 6. Ethos conformance | Code follows style guidelines |
| 7. Observability proof | OTEL spans present |
| 8. Causality proof | Input hashes → output hashes |

Pass all 8 to ensure quality.

### What's the test coverage?

Current coverage (Phase 5):
- **Unit tests**: 89% coverage
- **Integration tests**: 92% coverage
- **E2E tests**: 100% (all workflows tested)
- **Overall**: 92% coverage

View in [IMPLEMENTATION_CHECKLIST.md](./IMPLEMENTATION_CHECKLIST.md).

---

## Troubleshooting

### I get "Ontology not found"

**Quick fix**:
```bash
ggen ontology list --embedded
ggen ontology install <package>
```

See [TROUBLESHOOTING.md](./TROUBLESHOOTING.md#error-ontology-not-found).

### SPARQL query returns no results

**Diagnosis**:
```bash
# Run with --debug to see triples
ggen sync --debug

# Or manually query ontology
ggen debug-sparql "SELECT * WHERE { ?x ?y ?z . } LIMIT 10"
```

### My templates are wrong

**Fix**:
```bash
# See what SPARQL returns
ggen debug-sparql "SELECT ?x ?y WHERE { ... } LIMIT 5"

# Use those column names in template
```

See [TROUBLESHOOTING.md](./TROUBLESHOOTING.md#error-template-rendering-error).

---

## Roadmap

### Completed (v26.5.28)
- ✅ Ontology embedding (12 core ontologies)
- ✅ Full μ₁–μ₅ pipeline
- ✅ Offline mode support
- ✅ Lock file support
- ✅ 120+ comprehensive tests

### Planned (v26.6.x)
- 🔜 Binary distributions (no Rust required)
- 🔜 Public marketplace registry
- 🔜 Private registry support
- 🔜 GUI marketplace browser

### Future (v27.x)
- 🔜 Custom ontology embedding
- 🔜 Parallel pipeline stages
- 🔜 LLM-assisted code generation
- 🔜 Performance optimizations

---

## Contributing

### How do I contribute?

1. Fork: https://github.com/seanchatmangpt/ggen
2. Branch: `git checkout -b feature/my-feature`
3. Code: Write tests first (Chicago TDD)
4. Push: `git push origin feature/my-feature`
5. PR: Open pull request

See [CONTRIBUTING.md](../CONTRIBUTING.md) for details.

### How do I report a bug?

1. Check [TROUBLESHOOTING.md](./TROUBLESHOOTING.md)
2. Search existing [issues](https://github.com/seanchatmangpt/ggen/issues)
3. Create new issue with:
   - Error message (full output)
   - Minimal reproducible example
   - System info (`ggen --version`, `rustc --version`)

### How do I request a feature?

Open a [discussion](https://github.com/seanchatmangpt/ggen/discussions) or [issue](https://github.com/seanchatmangpt/ggen/issues) with:
- Use case (why do you need this?)
- Example (show what you want)
- Impact (who else might benefit?)

---

## More Help

- **Getting Started**: [GETTING_STARTED.md](./GETTING_STARTED.md)
- **Usage Guide**: [USAGE_GUIDE.md](./USAGE_GUIDE.md)
- **Troubleshooting**: [TROUBLESHOOTING.md](./TROUBLESHOOTING.md)
- **API Reference**: [API_REFERENCE.md](./API_REFERENCE.md)
- **GitHub Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions

Still stuck? [Open an issue](https://github.com/seanchatmangpt/ggen/issues/new) and we'll help!
