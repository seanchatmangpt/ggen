<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [8. DRY RUN **](#8-dry-run-)
  - [Context](#context)
  - [Connections](#connections)
  - [Implementation](#implementation)
    - [Invocation](#invocation)
    - [Behavior](#behavior)
    - [Output](#output)
    - [JSON Output](#json-output)
    - [Actions](#actions)
    - [Change Detection](#change-detection)
  - [Use Cases](#use-cases)
    - [Safe Exploration](#safe-exploration)
    - [Configuration Verification](#configuration-verification)
    - [CI/CD Verification](#cicd-verification)
    - [Learning](#learning)
  - [The Deeper Pattern](#the-deeper-pattern)
  - [Relationship to Other Safety Patterns](#relationship-to-other-safety-patterns)
  - [When This Pattern Breaks](#when-this-pattern-breaks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 8. DRY RUN **

*Look before you leap. Ask before you change.*

---

## Context

You are about to run **[THE SINGLE COMMAND](01-single-command.md)**. The pipeline will load ontologies, execute inference, generate code, and write files. Once written, files are changed. In a version-controlled project, changes can be reverted—but reverting is work you'd rather avoid.

You want to know *what will happen* before it happens. You want to see the future without committing to it.

---

❖ ❖ ❖

**Irreversible actions should be previewable. The cost of looking should be zero.**

The forces:
- Users need confidence before generating
- Generated files may overwrite existing work
- Errors in configuration should be caught early
- The full pipeline should run (not just validation)

Without preview:
- Users run blind, hoping for the best
- Mistakes are discovered after damage is done
- Learning the system requires real file changes
- CI/CD pipelines can't safely verify configuration

**Therefore:**

**Provide a dry-run mode that executes the complete pipeline without writing files. Show what would be generated, what would change, and what would remain unchanged. Make the dry-run output identical in structure to real output.**

The dry-run should:
- Load and validate the manifest
- Load the ontology
- Execute inference rules
- Execute generation rules
- Render all templates
- **Stop before writing files**
- Report what *would have been* written

---

❖ ❖ ❖

## Connections

This pattern modifies **[THE SINGLE COMMAND](01-single-command.md)** via a flag.

- **[VALIDATION GATE](09-validation-gate.md)** is similar but stops earlier
- **[ERROR SIGNALS](12-error-signals.md)** still apply in dry-run
- **[AUDIT TRAIL](14-audit-trail.md)** can be generated (but not written)

---

## Implementation

### Invocation

```bash
ggen sync --dry-run
```

### Behavior

The dry-run flag affects only the final step—file writing:

```
Normal execution:         Dry-run execution:
──────────────────       ──────────────────
Load manifest    ✓       Load manifest    ✓
Load ontology    ✓       Load ontology    ✓
Run inference    ✓       Run inference    ✓
Run generation   ✓       Run generation   ✓
Write files      ✓       Write files      ✗ (skip)
Return result    ✓       Return preview   ✓
```

Everything runs except the `std::fs::write()` calls.

### Output

Dry-run produces the same structured output:

```
ggen sync v5.0.0 (dry-run mode)

Would sync 5 files:

  [CREATE]  src/generated/user.rs (1,247 bytes)
  [CREATE]  src/generated/order.rs (982 bytes)
  [UPDATE]  src/generated/product.rs (1,456 bytes)
  [SKIP]    src/generated/mod.rs (unchanged)
  [CREATE]  src/generated/types.rs (324 bytes)

Inference rules: 2 executed
Generation rules: 3 executed
Duration: 45ms

No files were modified (dry-run).
```

### JSON Output

For CI/CD integration:

```bash
ggen sync --dry-run --format json
```

```json
{
  "status": "success",
  "dry_run": true,
  "files_synced": 0,
  "files": [
    {
      "path": "src/generated/user.rs",
      "size_bytes": 1247,
      "action": "would_create"
    },
    {
      "path": "src/generated/product.rs",
      "size_bytes": 1456,
      "action": "would_update"
    }
  ],
  "inference_rules_executed": 2,
  "generation_rules_executed": 3,
  "duration_ms": 45
}
```

### Actions

| Action | Meaning |
|--------|---------|
| `would_create` | File doesn't exist, would be created |
| `would_update` | File exists, content would change |
| `unchanged` | File exists, content identical |
| `would_skip` | Rule skipped (skip_empty, mode=Create) |

### Change Detection

To determine if a file would change, dry-run:

1. Renders the new content
2. Reads the existing file (if present)
3. Compares via hash

```rust
fn would_change(new_content: &str, existing_path: &Path) -> bool {
    if !existing_path.exists() {
        return true;  // would_create
    }
    let existing = std::fs::read_to_string(existing_path).unwrap_or_default();
    existing != new_content  // would_update vs unchanged
}
```

---

## Use Cases

### Safe Exploration

First time using ggen sync:

```bash
$ ggen sync --dry-run
# See what would happen before committing
```

### Configuration Verification

After modifying ggen.toml:

```bash
$ ggen sync --dry-run
# Verify changes have expected effect
```

### CI/CD Verification

In a pull request:

```yaml
- name: Verify generation is up-to-date
  run: |
    ggen sync --dry-run --format json > preview.json
    # Check that no files would change
    if jq '.files[] | select(.action == "would_update")' preview.json | grep -q .; then
      echo "Generated files are out of date!"
      exit 1
    fi
```

### Learning

Understanding a new project:

```bash
$ ggen sync --dry-run --verbose
# See exactly what the manifest produces
```

---

## The Deeper Pattern

DRY RUN is about **separating decision from action**.

The decision—what files to generate, with what content—is computed by the pipeline. The action—writing to disk—is a separate concern.

By separating them, we enable:
- **Safe exploration**: Run the decision without the action
- **Verification**: Check the decision matches expectations
- **Debugging**: Isolate decision problems from action problems

This is a fundamental principle: **make actions reversible or previewable**.

File writes are not easily reversible (version control helps, but requires commits). Therefore, file writes should be previewable via dry-run.

---

## Relationship to Other Safety Patterns

| Pattern | What it provides |
|---------|------------------|
| **DRY RUN** | Preview all changes without writing |
| **[VALIDATION GATE](09-validation-gate.md)** | Check configuration without generating |
| **[FORCE OVERWRITE](10-force-overwrite.md)** | Override protections to write anyway |

These three patterns form a safety spectrum:

```
VALIDATION GATE      DRY RUN           Normal            FORCE OVERWRITE
(check only)         (preview)         (safe write)      (override)
    ←───────────────────────────────────────────────────────→
  Safest                                                  Riskiest
```

---

## When This Pattern Breaks

DRY RUN struggles when:

- The preview is expensive (as expensive as generating)
- External effects occur during generation (API calls, database writes)
- File comparison is complex (binary files, whitespace normalization)

ggen's dry-run handles these partially:

- Generation is relatively fast; preview cost is acceptable
- ggen has no external effects beyond file writes
- File comparison is exact (hash-based)

For systems with external effects, dry-run becomes more complex—you may need mock environments or transaction rollback.

The pattern remains: users should be able to see what will happen before it happens.
