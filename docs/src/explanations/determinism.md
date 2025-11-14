# Determinism

ggen guarantees deterministic code generation: same inputs always produce byte-identical outputs.

## Why Determinism Matters

**Determinism** means: Same inputs → identical outputs, every time.

**Critical for:**
- Reproducible builds (CI/CD verification)
- Git-friendly diffs (only meaningful changes)
- Cacheable outputs (build system optimization)
- Trustworthy regeneration (no unexpected changes)

## The Guarantee

```
Same RDF graph + Same template + Same variables
    ⇒ Byte-identical output
    ⇒ Same SHA-256 hash
```

This holds across:
- Machines (Mac, Linux, Windows)
- Environments (dev, CI, production)
- Time (today or next year)
- Users (different developers)

## How ggen Achieves Determinism

### 1. Content Hashing

All inputs are hashed using SHA-256:
- RDF graph (sorted N-Quads)
- SHACL shapes (sorted N-Quads)
- Template frontmatter (canonical YAML)
- SPARQL query results (ordered JSON)

### 2. Sorted RDF Graphs

RDF triples are sorted lexicographically before hashing:

```turtle
# Input (order may vary)
pc:Product pc:name "Widget" .
pc:Product pc:price 99.99 .

# Sorted N-Quads (deterministic)
<http://ex.org/Product> <http://ex.org/name> "Widget" .
<http://ex.org/Product> <http://ex.org/price> "99.99"^^xsd:decimal .
```

### 3. Ordered SPARQL Results

SPARQL queries **must include `ORDER BY`**:

```sparql
SELECT ?property ?type
WHERE { ?property rdfs:domain ?class . }
ORDER BY ?property  -- Required for determinism
```

ggen enforces `ORDER BY` in matrix queries.

### 4. Version-Locked Templates

Marketplace gpacks use lockfiles:

```toml
# ggen.lock
[gpacks]
"io.ggen.rust.models" = "0.2.1"
```

Same version → same template → same output.

## Manifest Key

Every generation produces a manifest key (SHA-256 hash):

```
K = SHA256(seed || graph_hash || shapes_hash || frontmatter_hash || rows_hash)
```

Changing any input changes the manifest key.

## Validation

ggen's determinism is validated by a 782-line end-to-end test:

- Real RDF graphs (no mocks)
- Real SPARQL queries
- Real file I/O
- Real template rendering
- Byte-identical verification

## Best Practices

1. **Always use `ORDER BY` in SPARQL queries**
2. **Pin gpack versions in production**
3. **Commit lockfiles to version control**
4. **Validate in CI** (regenerate and check git diff)
5. **Avoid timestamps in templates**

## See Also

- [Determinism Reference](../reference/rdf-sparql.md#deterministic-processing)
- [Create Templates Guide](../how-to-guides/create-templates.md)
- [Deploy Production Guide](../how-to-guides/deploy-production.md)

