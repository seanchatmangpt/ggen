# CLI

```bash
rgen list
rgen show <scope> <action> [--vars k=v ...]
rgen validate <scope> <action> [--vars k=v ...]
rgen gen <scope> <action> [--vars k=v ...] [--dry]
rgen graph export <scope> <action> --fmt ttl|jsonld
```

Precedence: `CLI --vars` > `SPARQL vars` > `frontmatter vars`.

## Dry-Run Mode

Preview template rendering without writing files:

```bash
rgen gen --template templates/api/endpoint/rust.tmpl --var name=User --dry
```

Dry-run behavior:
- RDF graphs are loaded (read-only)
- SPARQL queries execute normally
- Templates render completely
- Output shows what would be written
- No files are created or modified
- No shell commands execute (when implemented)
- No injections occur (when implemented)
