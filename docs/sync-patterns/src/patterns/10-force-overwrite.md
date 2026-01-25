<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [10. FORCE OVERWRITE *](#10-force-overwrite-)
  - [Context](#context)
  - [Connections](#connections)
  - [Implementation](#implementation)
    - [Invocation](#invocation)
    - [Behavior](#behavior)
    - [Output](#output)
    - [Audit Trail](#audit-trail)
  - [Use Cases](#use-cases)
    - [Clean Slate](#clean-slate)
    - [Development Iteration](#development-iteration)
    - [Recovery](#recovery)
  - [Force and Dry Run](#force-and-dry-run)
  - [The Danger Zone](#the-danger-zone)
    - [Version Control Integration](#version-control-integration)
  - [The Deeper Pattern](#the-deeper-pattern)
  - [Safety Spectrum](#safety-spectrum)
  - [When This Pattern Breaks](#when-this-pattern-breaks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 10. FORCE OVERWRITE *

*Sometimes you must break the rules you set for yourself.*

---

## Context

**[GENERATION RULES](06-generation-rules.md)** support a `mode` field that controls file behavior:

- `Create` — Only write if file doesn't exist
- `Overwrite` — Always write
- `Merge` — Preserve marked sections

The `Create` mode exists to protect hand-edited files. Once you've modified a generated file, you don't want the generator to clobber your changes.

But sometimes, you know better. You've made a mess and want to reset. The ontology changed fundamentally, and you need a clean slate. The protections that serve you daily now block you.

---

❖ ❖ ❖

**Safety mechanisms must be overridable, or they become prisons.**

The forces:
- File protection exists for good reason (preserving human edits)
- Sometimes protection must be bypassed (intentional reset)
- The bypass should be explicit (not the default)
- The bypass should be complete (all protections lifted)

Without an override:
- Users work around the tool (delete files manually)
- Partial resets leave inconsistent state
- The tool feels like an obstacle

**Therefore:**

**Provide a force flag that overrides all file protections. When force is active, treat all rules as Overwrite mode. Make force explicit and observable—never silent.**

The force flag should:
- Override `Create` mode to `Overwrite`
- Write even when existing content differs
- Log what was overwritten
- Combine with other flags (like `--dry-run --force` to preview)

---

❖ ❖ ❖

## Connections

This pattern modifies **[THE SINGLE COMMAND](01-single-command.md)** via a flag.

- **[DRY RUN](08-dry-run.md)** can combine with force: `--dry-run --force`
- **[GENERATION RULES](06-generation-rules.md)** defines the modes being overridden
- **[AUDIT TRAIL](14-audit-trail.md)** records forced overwrites

---

## Implementation

### Invocation

```bash
ggen sync --force
```

### Behavior

The force flag affects mode interpretation:

```rust
fn should_write(rule: &GenerationRule, path: &Path, force: bool) -> bool {
    if force {
        return true;  // Always write when forced
    }

    match rule.mode {
        GenerationMode::Create => !path.exists(),
        GenerationMode::Overwrite => true,
        GenerationMode::Merge => true,  // TODO: handle merge logic
    }
}
```

### Output

Force generates explicit warnings:

```
$ ggen sync --force

ggen sync v5.0.0 (force mode)

Syncing 5 files:

  [FORCE]   src/generated/user.rs (overwriting Create mode)
  [FORCE]   src/generated/order.rs (overwriting Create mode)
  [WRITE]   src/generated/product.rs
  [WRITE]   src/generated/mod.rs
  [WRITE]   src/generated/types.rs

⚠  2 files overwritten with --force
   Changes may have been lost. Check version control.

5 files synced in 52ms
```

### Audit Trail

Forced writes are recorded:

```json
{
  "files": [
    {
      "path": "src/generated/user.rs",
      "action": "force_overwrite",
      "original_mode": "Create",
      "previous_hash": "abc123...",
      "new_hash": "def456..."
    }
  ],
  "force_mode": true,
  "force_overwrites": 2
}
```

---

## Use Cases

### Clean Slate

After major ontology changes:

```bash
# Preview what force would do
ggen sync --dry-run --force

# Actually reset everything
ggen sync --force
```

### Development Iteration

During rapid iteration:

```bash
# Keep regenerating while developing templates
ggen sync --force --rule structs
```

### Recovery

After accidental edits to generated files:

```bash
# Restore generated files to canonical state
ggen sync --force
```

---

## Force and Dry Run

The combination `--dry-run --force` is powerful for previewing:

```bash
$ ggen sync --dry-run --force

Would sync 5 files (force mode):

  [WOULD FORCE]  src/generated/user.rs
      Current: 1,234 bytes, modified 2 hours ago
      New: 1,247 bytes
      Mode: Create (would be overridden)

  [WOULD WRITE]  src/generated/product.rs
      (normal overwrite)
```

This shows exactly what `--force` would do without doing it.

---

## The Danger Zone

Force is intentionally dangerous. The system warns clearly:

```
⚠ WARNING: --force will overwrite files regardless of mode.
  Files with mode=Create may contain human edits.
  These edits will be lost.

  Use --dry-run --force to preview changes first.
```

### Version Control Integration

The safest force workflow:

```bash
# 1. Ensure clean git state
git status  # Should be clean

# 2. Preview force
ggen sync --dry-run --force

# 3. Execute force
ggen sync --force

# 4. Review changes
git diff

# 5. Commit or revert
git add . && git commit -m "Regenerate with --force"
# OR
git checkout -- src/generated/
```

---

## The Deeper Pattern

FORCE OVERWRITE is about **explicit override**.

Good systems have protections. Great systems let you bypass those protections when you know what you're doing.

The key is **explicitness**:
- The user must explicitly request force (`--force`)
- The system explicitly warns about consequences
- The output explicitly shows what was forced
- The audit explicitly records forced actions

This is different from removing the protections entirely. The protections exist; force overrides them temporarily, knowingly, observably.

---

## Safety Spectrum

Force sits at the risky end of the safety spectrum:

```
VALIDATION GATE      DRY RUN           Normal            FORCE OVERWRITE
(check only)         (preview)         (safe write)      (override)
    ←───────────────────────────────────────────────────────→
  Safest                                                  Riskiest
```

Use force sparingly:
- Most of the time, normal mode with appropriate rule modes is sufficient
- When force is needed, combine with dry-run first
- After force, verify with version control

---

## When This Pattern Breaks

FORCE OVERWRITE struggles when:

- Users use force habitually (bypassing all safety)
- Force causes data loss that version control can't recover
- Force becomes the only way to make the tool work

ggen guards against these:

- Force emits visible warnings
- Force is logged in audit trail
- Normal mode with Overwrite rules is usually sufficient

If force becomes necessary frequently, the real problem is likely:
- Incorrect rule modes (should be Overwrite, not Create)
- Ontology changes that aren't reflected in templates
- A workflow that generates files meant for human editing

The pattern remains: force exists for exceptional cases, not normal operation.
