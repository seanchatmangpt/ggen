<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Hooks & Lifecycle Reference](#hooks--lifecycle-reference)
  - [Hook Lifecycle](#hook-lifecycle)
  - [Hook Types](#hook-types)
    - [System Hooks](#system-hooks)
      - [`pre-commit`](#pre-commit)
      - [`post-merge`](#post-merge)
      - [`post-checkout`](#post-checkout)
    - [Lifecycle Hooks](#lifecycle-hooks)
      - [`before-init`](#before-init)
      - [`after-init`](#after-init)
      - [`before-discover`](#before-discover)
      - [`after-discover`](#after-discover)
      - [`before-validate`](#before-validate)
      - [`after-validate`](#after-validate)
      - [`before-generate`](#before-generate)
      - [`after-generate`](#after-generate)
      - [`before-format`](#before-format)
      - [`after-format`](#after-format)
      - [`before-test`](#before-test)
      - [`after-test`](#after-test)
  - [Hook Configuration](#hook-configuration)
    - [Create a Hook](#create-a-hook)
    - [Hook Configuration File](#hook-configuration-file)
    - [List Hooks](#list-hooks)
    - [Remove Hook](#remove-hook)
    - [Enable/Disable Hook](#enabledisable-hook)
    - [Monitor Hooks](#monitor-hooks)
  - [Hook Environment Variables](#hook-environment-variables)
  - [Common Hook Patterns](#common-hook-patterns)
    - [Pattern 1: Validate Before Commit](#pattern-1-validate-before-commit)
    - [Pattern 2: Auto-Generate After Merge](#pattern-2-auto-generate-after-merge)
    - [Pattern 3: Run Tests After Generation](#pattern-3-run-tests-after-generation)
    - [Pattern 4: Backup Before Changes](#pattern-4-backup-before-changes)
    - [Pattern 5: Notify After Generation](#pattern-5-notify-after-generation)
  - [Hook Ordering](#hook-ordering)
  - [Error Handling](#error-handling)
  - [Troubleshooting](#troubleshooting)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Hooks & Lifecycle Reference

Complete reference for lifecycle hooks, events, and automation.

## Hook Lifecycle

ggen projects follow a standardized lifecycle with hooks at each phase:

```
┌─────────────────────────────────────────┐
│   Initialization Phase                  │
│   (Project setup, configuration)        │
└────────────┬────────────────────────────┘
             │
    ┌────────▼─────────┐
    │ before-init      │
    │ after-init       │
    └────────┬─────────┘
             │
┌────────────▼────────────────────────────┐
│   Discovery Phase                       │
│   (Find templates, ontologies, queries) │
└────────────┬────────────────────────────┘
             │
    ┌────────▼──────────────┐
    │ before-discover       │
    │ after-discover        │
    └────────┬──────────────┘
             │
┌────────────▼────────────────────────────┐
│   Validation Phase                      │
│   (Check ontology, syntax)              │
└────────────┬────────────────────────────┘
             │
    ┌────────▼──────────────┐
    │ before-validate       │
    │ after-validate        │
    └────────┬──────────────┘
             │
┌────────────▼────────────────────────────┐
│   Generation Phase                      │
│   (Run templates)                       │
└────────────┬────────────────────────────┘
             │
    ┌────────▼──────────────┐
    │ before-generate       │
    │ after-generate        │
    └────────┬──────────────┘
             │
┌────────────▼────────────────────────────┐
│   Formatting Phase                      │
│   (Apply formatters)                    │
└────────────┬────────────────────────────┘
             │
    ┌────────▼──────────────┐
    │ before-format         │
    │ after-format          │
    └────────┬──────────────┘
             │
┌────────────▼────────────────────────────┐
│   Testing Phase                         │
│   (Run tests)                           │
└────────────┬────────────────────────────┘
             │
    ┌────────▼──────────────┐
    │ before-test           │
    │ after-test            │
    └────────┬──────────────┘
             │
       ┌─────▼──────┐
       │   Complete │
       └────────────┘
```

## Hook Types

### System Hooks

System hooks are triggered by Git or project events.

#### `pre-commit`
Runs before files are committed.

**When:** Before `git commit`
**Exit code:** 0 = allow commit, non-zero = block
**Use case:** Validate ontology before committing changes

```bash
ggen hook create pre-commit \
  --name validate-ontology \
  --command "ggen graph query --sparql 'SELECT ?s WHERE {?s a rdfs:Class}' | wc -l"
```

#### `post-merge`
Runs after merging branches (e.g., git pull).

**When:** After `git merge` or `git pull`
**Use case:** Regenerate code after merging ontology changes

```bash
ggen hook create post-merge \
  --name regenerate-after-merge \
  --command "ggen template generate-rdf --ontology domain.ttl --template rust-models"
```

#### `post-checkout`
Runs after checking out branches.

**When:** After `git checkout`
**Use case:** Sync dependencies or regenerate based on branch

```bash
ggen hook create post-checkout \
  --name sync-branch \
  --command "ggen project gen"
```

### Lifecycle Hooks

Lifecycle hooks run during ggen operations.

#### `before-init`
Runs before project initialization.

**When:** Start of `ggen project init`
**Use case:** Pre-flight checks, prerequisite validation

```bash
ggen hook create before-init \
  --name check-prerequisites \
  --command "which rustc && which cargo"
```

#### `after-init`
Runs after project initialization completes.

**When:** End of `ggen project init`
**Use case:** Post-setup configuration, welcome message

```bash
ggen hook create after-init \
  --name setup-complete \
  --command "echo 'Project initialized! Run: ggen project gen'"
```

#### `before-discover`
Runs before discovering templates and ontologies.

**When:** Start of template discovery phase
**Use case:** Prepare workspace, update registries

#### `after-discover`
Runs after templates/ontologies are discovered.

**When:** End of discovery phase
**Use case:** Log available templates, validate counts

#### `before-validate`
Runs before validating ontology.

**When:** Start of validation phase
**Use case:** Check SHACL shapes, lint RDF

```bash
ggen hook create before-validate \
  --name check-shacl \
  --command "ggen graph query --sparql 'CONSTRUCT {?s ?p ?o} WHERE {?s ?p ?o}' | wc -l"
```

#### `after-validate`
Runs after validation completes.

**When:** End of validation phase
**Use case:** Report validation results

#### `before-generate`
Runs before code generation.

**When:** Start of generation phase
**Use case:** Backup files, log generation params

```bash
ggen hook create before-generate \
  --name backup-generated \
  --command "cp -r src/ src.backup"
```

#### `after-generate`
Runs after code generation completes.

**When:** End of generation phase
**Use case:** Run tests, update docs

```bash
ggen hook create after-generate \
  --name run-tests \
  --command "cargo test"
```

#### `before-format`
Runs before applying code formatters.

**When:** Start of formatting phase
**Use case:** Check formatter availability

#### `after-format`
Runs after formatting completes.

**When:** End of formatting phase
**Use case:** Verify formatted output

#### `before-test`
Runs before running tests.

**When:** Start of testing phase
**Use case:** Setup test environment

#### `after-test`
Runs after tests complete.

**When:** End of testing phase
**Use case:** Generate test reports, cleanup

## Hook Configuration

### Create a Hook

```bash
ggen hook create <type> \
  --name <hook_name> \
  --command "<shell_command>" \
  [--description "<description>"] \
  [--env <key=value>] \
  [--timeout <seconds>]
```

**Example:**
```bash
ggen hook create pre-commit \
  --name validate-ontology \
  --command "ggen graph query --sparql 'SELECT ?s WHERE {?s a rdfs:Class}'" \
  --description "Validate ontology before commit" \
  --timeout 30
```

### Hook Configuration File

Hooks are stored in `.ggen/hooks.toml`:

```toml
[[hook]]
name = "validate-ontology"
type = "pre-commit"
command = "ggen graph query --sparql 'SELECT ?s WHERE {?s a rdfs:Class}'"
description = "Validate ontology structure"
timeout = 30
enabled = true

[[hook]]
name = "regenerate-after-merge"
type = "post-merge"
command = "ggen template generate-rdf --ontology domain.ttl --template rust-models"
description = "Regenerate code after merging changes"
timeout = 120
enabled = true
```

### List Hooks

```bash
ggen hook list
```

**Output:**
```
Registered Hooks:
  pre-commit
    • validate-ontology (enabled) - Validate ontology structure
  post-merge
    • regenerate-after-merge (enabled) - Regenerate after merging
```

### Remove Hook

```bash
ggen hook remove --name <hook_name>
```

### Enable/Disable Hook

Edit `.ggen/hooks.toml` and set `enabled = true/false`.

### Monitor Hooks

View hook execution logs:

```bash
ggen hook monitor [--type <hook_type>] [--tail 50]
```

## Hook Environment Variables

Available environment variables in hook commands:

| Variable | Value | Availability |
|----------|-------|---------------|
| `GGEN_HOME` | ggen install directory | All hooks |
| `GGEN_PROJECT_ROOT` | Project root directory | All hooks |
| `GGEN_ONTOLOGY_FILE` | Primary ontology file | Lifecycle hooks |
| `GGEN_TEMPLATE_NAME` | Template being run | Generation hooks |
| `GGEN_ENVIRONMENT` | dev/prod/test | All hooks |
| `GIT_BRANCH` | Current Git branch | Git hooks |
| `GIT_COMMIT` | Current commit hash | Git hooks |

**Example using variables:**
```bash
ggen hook create post-merge \
  --name log-merge \
  --command "echo 'Merged into $GIT_BRANCH - regenerating from $GGEN_ONTOLOGY_FILE'"
```

## Common Hook Patterns

### Pattern 1: Validate Before Commit

Ensure ontology is valid before allowing commit:

```bash
ggen hook create pre-commit \
  --name validate-ontology \
  --command "ggen graph query --sparql 'SELECT ?s WHERE {?s a rdfs:Class}' | grep -q . && echo 'Valid' || (echo 'Invalid ontology'; exit 1)" \
  --timeout 30
```

### Pattern 2: Auto-Generate After Merge

Automatically regenerate code when ontology is merged:

```bash
ggen hook create post-merge \
  --name auto-regenerate \
  --command "ggen template generate-rdf --ontology $GGEN_ONTOLOGY_FILE --template rust-models && cargo fmt" \
  --timeout 120
```

### Pattern 3: Run Tests After Generation

Validate generated code by running tests:

```bash
ggen hook create after-generate \
  --name validate-generated \
  --command "cargo test --lib 2>&1 | tee generation-test.log" \
  --timeout 300
```

### Pattern 4: Backup Before Changes

Backup generated files before regeneration:

```bash
ggen hook create before-generate \
  --name backup-current \
  --command "mkdir -p backups && cp -r src/ backups/src.$(date +%s)" \
  --timeout 60
```

### Pattern 5: Notify After Generation

Send notification after code generation:

```bash
ggen hook create after-generate \
  --name notify-complete \
  --command "curl -X POST https://hooks.example.com -d 'Generated for $GIT_BRANCH'" \
  --timeout 10
```

## Hook Ordering

Hooks of the same type execute in registration order:

```bash
# Registered in this order:
ggen hook create pre-commit --name first-check --command "echo 'First'"
ggen hook create pre-commit --name second-check --command "echo 'Second'"

# Executes as: first-check → second-check
```

## Error Handling

Hook behavior on error:

| Hook Type | On Failure |
|-----------|-----------|
| `pre-commit` | Blocks commit (exit code non-zero) |
| `post-merge` | Warning, continues (logged) |
| `before-*` | Warning, continues to next phase |
| `after-*` | Error logged, continues |

Override behavior with `--continue-on-error` flag (if needed).

## Troubleshooting

**Q: Hook not executing**
A: Check enabled status: `ggen hook list`. Enable with `enabled = true` in `.ggen/hooks.toml`.

**Q: Hook timeout not working**
A: Ensure timeout value is set (in seconds). Default is 300s (5 minutes).

**Q: Git hook not triggered**
A: Check Git hook location: `.git/hooks/<hook-name>`. Should link to ggen hook system.

**Q: Environment variables not available**
A: Not all variables available for all hook types. See availability table above.

## See Also

- [How to Configure Hooks](../how-to-guides/configure-hooks.md) - Configuration guide
- [Lifecycle Reference](../reference/cli.md) - CLI command reference
- [Ontology-Driven Development](../explanations/ontology-driven.md) - Development workflow
