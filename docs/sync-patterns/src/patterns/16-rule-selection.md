<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [16. RULE SELECTION *](#16-rule-selection-)
  - [Context](#context)
  - [Connections](#connections)
  - [Implementation](#implementation)
    - [Invocation](#invocation)
    - [Behavior](#behavior)
    - [SyncOptions](#syncoptions)
    - [Filtering Logic](#filtering-logic)
    - [Error Handling](#error-handling)
  - [Use Cases](#use-cases)
    - [Template Iteration](#template-iteration)
    - [Debugging](#debugging)
    - [Selective Regeneration](#selective-regeneration)
    - [Performance Profiling](#performance-profiling)
  - [Combining with Other Flags](#combining-with-other-flags)
  - [Inference Rules Selection](#inference-rules-selection)
  - [The Deeper Pattern](#the-deeper-pattern)
  - [When Rule Selection Is Wrong](#when-rule-selection-is-wrong)
  - [When This Pattern Breaks](#when-this-pattern-breaks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 16. RULE SELECTION *

*Sometimes you need a scalpel, not a sledgehammer.*

---

## Context

**[THE SINGLE COMMAND](01-single-command.md)** runs the entire pipeline. This is usually what you want—consistent, complete generation. But sometimes:

- You're iterating on a single template
- You want to regenerate only one file
- You're debugging why one rule fails
- You want to measure one rule's performance

Running the full pipeline is overkill. You know exactly what you want to run.

---

❖ ❖ ❖

**Precision beats thoroughness when you know what you need.**

The forces:
- Full pipeline runs are slow when you only need one thing
- Debugging is easier with isolated execution
- Some rules may fail; you want to run the others
- Iteration speed matters for development

Without selection:
- Every change triggers full regeneration
- Debugging requires mental isolation
- One broken rule blocks all generation
- Development velocity suffers

**Therefore:**

**Allow selecting specific rules by name. When a rule is selected, run only that rule (still executing necessary prerequisites like ontology loading). Make selection work with all other flags (dry-run, force, etc.).**

Rule selection should:
- Accept rule names via flag
- Execute only the named rules
- Still load the ontology (prerequisite)
- Still run inference (for generation rules)
- Work with `--dry-run`, `--force`, `--verbose`

---

❖ ❖ ❖

## Connections

This pattern modifies **[THE SINGLE COMMAND](01-single-command.md)** via a flag.

- **[GENERATION RULES](06-generation-rules.md)** define the rules that can be selected
- **[DRY RUN](08-dry-run.md)** combines: `--rule structs --dry-run`
- **[FORCE OVERWRITE](10-force-overwrite.md)** combines: `--rule structs --force`

---

## Implementation

### Invocation

```bash
# Run only the 'structs' generation rule
ggen sync --rule structs

# Run only the 'enums' generation rule
ggen sync --rule enums

# Run multiple rules (each --rule is additive)
ggen sync --rule structs --rule enums
```

### Behavior

When rules are selected:

```
Full execution:           Selected execution (--rule structs):
──────────────────       ────────────────────────────────────
Load manifest    ✓       Load manifest    ✓
Load ontology    ✓       Load ontology    ✓
Run inference    ✓       Run inference    ✓ (needed for generation)
Run all gen      ✓       Run 'structs'    ✓
Write all files  ✓       Write struct files ✓
```

Inference still runs because generation rules may depend on inferred facts.

### SyncOptions

```rust
pub struct SyncOptions {
    // ... other fields

    /// If set, run only these rules (by name)
    pub selected_rules: Option<Vec<String>>,
}
```

### Filtering Logic

```rust
fn should_execute_rule(&self, rule: &GenerationRule) -> bool {
    match &self.options.selected_rules {
        None => true,  // No filter, run all
        Some(selected) => selected.contains(&rule.name),
    }
}
```

### Error Handling

Unknown rule names produce clear errors:

```
$ ggen sync --rule struucts

error[E0001]: Unknown rule 'struucts'

  Available generation rules:
    - structs
    - enums
    - mod-file

  Help: Did you mean 'structs'?
```

---

## Use Cases

### Template Iteration

Developing a new template:

```bash
# Make change to templates/struct.tera
# Regenerate only affected files
ggen sync --rule structs

# Repeat
```

This is much faster than full regeneration.

### Debugging

Isolating a failing rule:

```bash
# Find the failing rule
ggen sync
# Error in rule 'complex-validation'

# Run only that rule with verbose output
ggen sync --rule complex-validation --verbose
```

### Selective Regeneration

After changing only entity definitions:

```bash
# Only regenerate struct files
ggen sync --rule structs --force
```

### Performance Profiling

Measuring one rule's performance:

```bash
# Time just the expensive rule
time ggen sync --rule complex-join --verbose
```

---

## Combining with Other Flags

Rule selection works with all other flags:

```bash
# Preview what 'structs' would generate
ggen sync --rule structs --dry-run

# Force regenerate 'enums'
ggen sync --rule enums --force

# Verbose output for 'mod-file'
ggen sync --rule mod-file --verbose

# JSON output for CI
ggen sync --rule structs --format json

# Combined
ggen sync --rule structs --dry-run --force --verbose
```

---

## Inference Rules Selection

Currently, `--rule` applies to generation rules only. Inference rules always run when generation is requested (they're prerequisites).

Future enhancement:

```bash
# Run only specific inference rule (no generation)
ggen sync --inference-rule derive-types

# Skip inference entirely
ggen sync --rule structs --skip-inference
```

---

## The Deeper Pattern

RULE SELECTION is about **precision**.

**[THE SINGLE COMMAND](01-single-command.md)** provides unity—one command for everything.
**RULE SELECTION** provides focus—one rule when that's what you need.

These are not contradictions. The command is singular; the scope is configurable. You still use `ggen sync`—you just tell it what scope to sync.

This aligns with the principle of **progressive disclosure**:
- Beginners use `ggen sync` (simple, complete)
- Experts use `ggen sync --rule X` (focused, fast)

The interface grows with the user's needs.

---

## When Rule Selection Is Wrong

Rule selection is for **development** and **debugging**. It's not for production:

**❌ Don't do this in CI/CD:**
```bash
ggen sync --rule structs
ggen sync --rule enums
ggen sync --rule mod-file
```

**✅ Do this in CI/CD:**
```bash
ggen sync
```

Why? Running rules separately:
- May miss dependencies
- Loses atomicity
- Complicates error handling
- Adds overhead

In production, run the full pipeline. Let the system decide what's needed.

---

## When This Pattern Breaks

RULE SELECTION struggles when:

- Rules have complex dependencies (which must run first?)
- Selection is needed for inference rules
- Many rules need selection (becomes tedious)

ggen manages this partially:

- Inference always runs (simplifies dependencies)
- Multiple `--rule` flags can select sets
- Most use cases need one or two rules

For complex selection needs, consider:
- Rule groups (future: `--rule-group api`)
- Regex selection (future: `--rule-pattern "entity-*"`)

The pattern remains: when you know what you need, you can ask for exactly that.
