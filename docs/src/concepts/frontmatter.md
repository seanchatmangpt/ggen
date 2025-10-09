# Frontmatter schema (v1)

```yaml
to: path/with/{{ vars }}
vars: { seed: cosmos }
rdf:
  - "graphs/core.ttl"
  - "graphs/x.jsonld"
shape:
  - "graphs/shapes/domain.ttl"
sparql:
  vars:
    - name: slug
      query: "SELECT ?slug WHERE { ?s <urn:ex#slug> ?slug } LIMIT 1"
  matrix:
    query: "SELECT ?id WHERE { ?s <urn:ex#id> ?id } ORDER BY ?id"
    bind: { id: "?id" }
determinism:
  sort: id
  seed: "{{ seed }}"
```

Validation JSON Schema: `schema/frontmatter.schema.json`.
