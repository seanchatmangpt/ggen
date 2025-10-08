# Determinism

Manifest key:
```
K = SHA256(seed || graph_hash || shapes_hash || frontmatter_hash || rows_hash)
```

- Graph hash: sorted N-Quads.
- Shapes hash: sorted N-Quads.
- Frontmatter hash: rendered header + body bytes.
- Rows hash: ordered serialization of matrix rows.

Same inputs ⇒ byte-identical outputs. Unchanged `K` ⇒ skip writes.
