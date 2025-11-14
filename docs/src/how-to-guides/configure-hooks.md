# How to Configure Hooks

Guide to setting up Git hooks and automation with ggen.

## What Are Hooks?

Hooks automate tasks during Git lifecycle events (pre-commit, post-merge, etc.).

## Creating Hooks

### Pre-Commit Hook

Validate ontology before commit:

```bash
ggen hook create pre-commit \
  --name validate-ontology \
  --command "ggen graph validate domain.ttl"
```

### Post-Merge Hook

Regenerate code after merge:

```bash
ggen hook create post-merge \
  --name regenerate-code \
  --command "ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/"
```

## Hook Lifecycle Phases

ggen supports 15 standard lifecycle phases:

- `pre-init`, `post-init`
- `pre-commit`, `post-commit`
- `pre-merge`, `post-merge`
- `pre-push`, `post-push`
- `pre-build`, `post-build`
- `pre-test`, `post-test`
- `pre-deploy`, `post-deploy`
- `pre-clean`

## Common Hook Patterns

### Validate Ontology

```bash
ggen hook create pre-commit \
  --name validate \
  --command "ggen graph validate domain.ttl --shacl shapes.ttl"
```

### Regenerate Code

```bash
ggen hook create post-merge \
  --name regenerate \
  --command "ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/"
```

### Run Tests

```bash
ggen hook create pre-push \
  --name test \
  --command "cargo test"
```

## Listing Hooks

View all registered hooks:

```bash
ggen hook list
```

**Output:**
```json
{
  "hooks": [
    {
      "phase": "pre-commit",
      "name": "validate-ontology",
      "command": "ggen graph validate domain.ttl"
    }
  ]
}
```

## Removing Hooks

Remove a hook:

```bash
ggen hook remove pre-commit validate-ontology
```

## Monitoring Hooks

Monitor hook execution:

```bash
ggen hook monitor
```

Shows real-time hook execution logs.

## Best Practices

1. **Keep hooks fast:** Pre-commit hooks should complete in <5 seconds
2. **Make hooks idempotent:** Safe to run multiple times
3. **Fail fast:** Return error codes on validation failures
4. **Document hooks:** Explain what each hook does
5. **Test hooks:** Verify hooks work before committing

## Troubleshooting

### Hook Not Executing

```bash
# Verify hook is registered
ggen hook list

# Check Git hook file exists
ls -la .git/hooks/pre-commit

# Test hook manually
.git/hooks/pre-commit
```

### Hook Too Slow

```bash
# Use timeout for long operations
ggen hook create pre-commit \
  --name validate \
  --command "timeout 5s ggen graph validate domain.ttl"
```

## Next Steps

- **CLI reference:** [Hook Commands Reference](../reference/cli.md#hook-commands)
- **Lifecycle explanation:** See lifecycle documentation
- **Automation guide:** See automation examples

