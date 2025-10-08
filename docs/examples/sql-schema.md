# Example: SQL from ontology

```yaml
---
to: db/{{ table }}.sql
rdf: { include: [graphs/domain.ttl] }
sparql:
  matrix:
    query: |
      PREFIX nk: <https://neako.app/onto#>
      SELECT ?table ?col ?dtype WHERE {
        ?c a nk:Class ; nk:sqlName ?table .
        ?c nk:property [ nk:sqlName ?col ; nk:sqlType ?dtype ] .
      } ORDER BY ?table ?col
    bind: { table: "?table", col: "?col", dtype: "?dtype" }
determinism: { seed: schema-1, sort: table }
---
CREATE TABLE {{ table }} (
  {{ col }} {{ dtype }}
);
```

Run:

```bash
rgen gen db schema
```
