<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Example: SQL from ontology](#example-sql-from-ontology)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Example: SQL from ontology

```yaml
---
to: db/{{ table }}.sql
rdf:
  - "graphs/domain.ttl"
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
ggen gen db schema
```
