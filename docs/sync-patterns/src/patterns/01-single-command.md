# 1. THE SINGLE COMMAND **

*A narrow gate that opens into a wide garden.*

---

## Context

You are building a code generation system. Over time, the system has accumulated many commands: `generate`, `validate`, `template`, `init`, `update`, `check`, `build`. Each command has its own flags, its own behavior, its own mental model. Users must learn which command to use when, how commands interact, and what order to run them in.

The cognitive load grows. The documentation grows. The confusion grows.

---

❖ ❖ ❖

**A code generation system with many commands forces users to become experts in the tool rather than experts in their domain.**

When there are many commands:
- Users must remember which command does what
- Users must learn the correct sequence of commands
- Commands may have overlapping functionality
- Commands may have conflicting flags
- The mental model becomes the tool, not the work

When there is one command:
- Users learn one thing
- The system decides the correct sequence
- There is no overlap or conflict
- The mental model is the work itself

**Therefore:**

**Provide exactly one command that does everything. Let the system figure out what "everything" means based on the manifest and the flags provided.**

The command should:
- Be simple to type and remember
- Accept optional flags that modify behavior
- Derive all other decisions from the manifest
- Execute the complete pipeline atomically

---

❖ ❖ ❖

## Connections

This pattern is the root of the language. All other patterns are invoked through it.

- **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** provides the configuration that THE SINGLE COMMAND reads
- **[THREE-LAYER ARCHITECTURE](03-three-layer-architecture.md)** structures how the command is implemented
- **[DRY RUN](08-dry-run.md)** and **[VALIDATION GATE](09-validation-gate.md)** modify the command's behavior
- **[ERROR SIGNALS](12-error-signals.md)** communicates the command's outcome

---

## Implementation

In ggen v5, the single command is:

```bash
ggen sync
```

That's it. There are no other commands. Everything happens through `sync`.

### The Flags

The command accepts flags that modify its behavior without changing its nature:

| Flag | Purpose |
|------|---------|
| `--manifest PATH` | Point to a different manifest file |
| `--dry-run` | Preview without writing |
| `--validate-only` | Check without generating |
| `--force` | Overwrite protected files |
| `--audit` | Record what happened |
| `--rule NAME` | Run only one rule |
| `--verbose` | Show detailed output |
| `--format json` | Output as JSON |
| `--timeout MS` | Set execution limit |

Each flag invokes a different pattern while keeping the command unified.

### The Pipeline

When you run `ggen sync`, this happens:

```
ggen.toml → Load Ontology → Execute Inference → Execute Generation → Write Files
    ↓              ↓               ↓                   ↓                  ↓
 MANIFEST      ONTOLOGY        INFERENCE          GENERATION           OUTPUT
 AS TRUTH      LOADING        ENRICHMENT           RULES          DETERMINISTIC
```

The user types one command. The system orchestrates many patterns.

### What Replaced

Before THE SINGLE COMMAND, ggen had:

- `ggen generate` — Run code generation
- `ggen validate` — Check ontology and templates
- `ggen template` — Manage templates
- `ggen init` — Initialize a project
- `ggen update` — Update generated code
- `ggen check` — Verify configuration
- ...and more

Now there is only `ggen sync`. The functionality is not lost—it is unified.

### Examples

```bash
# The primary workflow (99% of usage)
ggen sync

# From a specific manifest
ggen sync --manifest project/ggen.toml

# Safe exploration
ggen sync --dry-run

# CI/CD integration
ggen sync --format json --timeout 30000

# Focused regeneration
ggen sync --rule structs --force
```

---

## The Deeper Pattern

THE SINGLE COMMAND is not just about reducing commands. It is about **shifting cognitive load**.

In a multi-command system, the user must hold the system's structure in their head: "First I validate, then I generate, unless I need to update, in which case..."

In a single-command system, the user holds only their intent: "I want my code synchronized with my knowledge."

The system handles the structure. The user handles the domain.

This is the liberation that THE SINGLE COMMAND provides: **freedom from the tool to focus on the work**.

---

## When This Pattern Breaks

THE SINGLE COMMAND can become unwieldy if:

- The flags proliferate beyond comprehension
- The command does too many unrelated things
- Users need fine-grained control that one command cannot provide

ggen sync avoids these traps by:

- Keeping flags focused on behavior modification, not feature selection
- Deriving all generation logic from the manifest, not the command
- Providing escape hatches (`--rule`) for focused operation

If you find yourself wanting a second command, first ask: "Can this be a flag? Can this be a manifest section? Can this be a separate tool entirely?"

THE SINGLE COMMAND works because the command is a verb—**sync**—not a feature set.
