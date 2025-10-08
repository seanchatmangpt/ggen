# CLI

```bash
rgen list
rgen show <scope> <action> [--vars k=v ...]
rgen validate <scope> <action> [--vars k=v ...]
rgen gen <scope> <action> [--vars k=v ...] [--dry-run]
rgen graph export <scope> <action> --fmt ttl|jsonld
```

Precedence: `CLI --vars` > `SPARQL vars` > `frontmatter vars`.
