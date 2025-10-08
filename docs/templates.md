# Templates

Location:
```
templates/<scope>/<action>/*.tmpl
```

Structure:
- Frontmatter (YAML) header
- Body (rendered)

Common keys:
- `to:` output path
- `vars:` defaults
- `rdf:` includes/inline graphs
- `shape:` SHACL shape files
- `sparql.vars:` single-value bindings
- `sparql.matrix:` fan-out rows
- `determinism:` seed, sort key
