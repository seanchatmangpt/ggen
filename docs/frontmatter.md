# Frontmatter schema (v1)

```yaml
to: path/with/{{ vars }}
vars: { seed: cosmos }
rdf:
  include: [graphs/core.ttl, graphs/x.jsonld]
  inline:
    - mediaType: text/turtle
      text: |
        @prefix ex: <urn:ex#>.
        [] a ex:Thing ; ex:name "{{ name }}".
shape:
  include: [shapes/domain.ttl]
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
