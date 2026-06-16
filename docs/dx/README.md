<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Developer Experience (DX) Definition of Done — ggen v26.5.28](#developer-experience-dx-definition-of-done--ggen-v26528)
  - [Overview](#overview)
  - [Quick Summary](#quick-summary)
  - [The 7 Dimensions (Detailed)](#the-7-dimensions-detailed)
    - [1. Command Ergonomics](#1-command-ergonomics)
    - [2. Error Messages](#2-error-messages)
    - [3. Workflow Efficiency](#3-workflow-efficiency)
    - [4. IDE Integration](#4-ide-integration)
    - [5. Documentation Surface](#5-documentation-surface)
    - [6. Hot Reload / Iteration](#6-hot-reload--iteration)
    - [7. Defaults](#7-defaults)
  - [Definition of Done Verification](#definition-of-done-verification)
    - [Gate 1: Command Ergonomics](#gate-1-command-ergonomics)
    - [Gate 2: Error Messages](#gate-2-error-messages)
    - [Gate 3: Workflow Efficiency](#gate-3-workflow-efficiency)
    - [Gate 4: IDE Integration](#gate-4-ide-integration)
    - [Gate 5: Documentation](#gate-5-documentation)
    - [Gate 6: Hot Reload](#gate-6-hot-reload)
    - [Gate 7: Defaults](#gate-7-defaults)
  - [How to Use This Document](#how-to-use-this-document)
  - [See Also](#see-also)
  - [Questions?](#questions)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Developer Experience (DX) Definition of Done — ggen v26.5.28

## Overview

This directory defines what makes development **frictionless** across 7 critical dimensions. When all 7 dimensions are satisfied, DX is "done"—developers can iterate fast, stay in flow, and spend time building features, not fighting tools.

## Quick Summary

| Dimension | What It Is | Success Looks Like |
|-----------|-----------|-------------------|
| **Command Ergonomics** | Single entry point via `just` | `just check`, `just test-unit`, `just pre-commit` all work with sensible timeouts |
| **Error Messages** | Guidance to root cause + recovery | Compiler errors name the file:line, rule violated, and how to fix it |
| **Workflow Efficiency** | Keyboard shortcuts & automation | `jc` → `just check`, `cargo watch`, git hooks, <2s feedback loops |
| **IDE Integration** | LSP, debugging, live diagnostics | rust-analyzer shows errors as you type; Go to Definition/Find References work |
| **Documentation** | In-editor quick references | CLAUDE.md pinned; error messages link to docs; no web search needed |
| **Hot Reload** | Fast feedback loops | `just check` <5s, `just test-unit` <10s, cargo watch auto-runs |
| **Defaults** | Sensible project-wide conventions | Chicago TDD (no mocks), type-first, Result<T,E>, OTEL tracing all default |

## The 7 Dimensions (Detailed)

### 1. Command Ergonomics
**What:** All dev tasks callable via `just <task>` with sensible timeouts.

**Why:** Developers should never hunt for the right command. One tool (just), predictable syntax, timeout defaults reduce cognitive load.

**Done When:**
- ✅ 15+ critical commands defined in justfile (check, test-unit, test, lint, pre-commit, slo-check, audit, bench, doc, test-doc, test-marketplace, test-mutation, sync, sync-dry, timeout-check)
- ✅ `just --list` shows all tasks with one-liner descriptions
- ✅ Each command runs with a sensible timeout (or escalates intelligently)
- ✅ No direct `cargo` or `cargo make` calls required; all workflows route through `just`

**Examples:**
- `just pre-commit` before git commit (fmt → check → lint → test-unit)
- `just slo-check` before release (performance validation)
- `just sync-dry` before committing artifacts

**See:** `justfile` in repo root

---

### 2. Error Messages
**What:** Errors and warnings that guide developers to root cause and recovery path.

**Why:** A cryptic error stops a developer cold. Clear, actionable messages let them diagnose and fix without external help. Reduces time-to-resolution from hours to minutes.

**Done When:**
- ✅ Error messages include file:line and context (not just a line number)
- ✅ Error messages name the rule/invariant violated (reference to CLAUDE.md or diagnostic code)
- ✅ Error messages include recovery path ("Fix by adding X" or "See docs/troubleshooting.md")
- ✅ Validation errors link to diagnostic codes (GGEN-TPL-001, GGEN-OUT-001, etc.)
- ✅ Timeout errors suggest root cause and how to debug

**Diagnostic Codes:**
- **GGEN-TPL-001** — Template variable not in SELECT
- **GGEN-OUT-001** — output_file pattern has unbound variable
- **GGEN-YIELD-001** — output_file escapes project root
- **GGEN-RULE-001** — {file = ...} binding points to missing file
- **GGEN-QUERY-002** — SELECT * disables provision checks
- **E0011 / E0013** — CONSTRUCT/SELECT lacks ORDER BY (WARNING, ERROR in Strict Mode)

**Examples:**
```
❌ Bad:  "error: type mismatch"
✅ Good: "error[E0308]: expected `String`, found `&str`\n  → src/main.rs:42:15\n   = help: try: `let x = \"hello\".to_string();`"

❌ Bad:  "Timeout"
✅ Good: "error: command timed out (120s)\n  → just test\n  = This may indicate:\n  - First compile: `cargo clean && just test`\n  - Slow test: add `#[ignore]`\n  = See: docs/troubleshooting.md#timeout"
```

**See:** `CLAUDE.md` (Diagnostic Codes section), `.claude/rules/coding-agent-mistakes.md`

---

### 3. Workflow Efficiency
**What:** Keyboard shortcuts, aliases, and automation that let developers stay in their editor.

**Why:** Context switching (editor → terminal → editor) breaks flow. Fast shortcuts + background automation = 5x faster iteration.

**Done When:**
- ✅ Common tasks bound to keyboard shortcuts (Ctrl+Shift+B for build, Ctrl+Shift+T for test)
- ✅ Aliases defined for common `just` commands: `jc` → `just check`, `jt` → `just test-unit`, `jp` → `just pre-commit`
- ✅ `cargo watch` integration: `cargo watch -x 'just test-unit'` auto-runs tests on save
- ✅ Git hooks enforce quality gates before commit (pre-commit runs `just pre-commit`)
- ✅ Git hooks prevent force-push to main (pre-push validation)
- ✅ OTEL tracing toggled via env var, not code (e.g., `RUST_LOG=trace,ggen_ai=trace`)
- ✅ Edit-save-test feedback loop <2 seconds

**Recommended Aliases:**
```bash
alias jc='just check'      # Quick syntax check
alias jt='just test-unit'  # Run unit tests
alias jl='just lint'       # Check style
alias jp='just pre-commit' # Full pre-commit gate
alias js='just sync-dry'   # Preview sync
```

**Setup:**
```bash
# Add to ~/.bashrc or ~/.zshrc
cargo install cargo-watch
cargo watch -x 'just test-unit'
```

**Examples:**
- Developer saves file → cargo watch triggers → `just test-unit` runs in 8s → feedback
- Developer types alias `jc` → Enter → `just check` runs in 3s → no context switch

**See:** `.claude/settings.json` (env vars, hooks)

---

### 4. IDE Integration
**What:** LSP, debugging, and editor support that surfaces compiler knowledge without context switches.

**Why:** The best error is one that appears before you compile. LSP + live diagnostics = fix issues in-editor.

**Done When:**
- ✅ rust-analyzer-lsp enabled in `.claude/settings.json`
- ✅ Compiler errors visible as red squiggles in editor (not after compile)
- ✅ Hover over symbol shows type signature, trait bounds, visibility
- ✅ **Go to Definition** jumps to actual definition (respecting re-exports)
- ✅ **Find All References** finds all usages across workspace (semantic, not text)
- ✅ **Go to Implementation** finds all trait implementers
- ✅ Code completion suggests symbols from all crates
- ✅ Rename refactoring updates all usages atomically
- ✅ Call hierarchy shows callers/callees
- ✅ Breakpoint debugging works (set breakpoints, step, inspect vars)

**LSP Navigation Rules:**

| Task | ❌ Forbidden | ✅ Required | Why |
|------|-----------|----------|-----|
| Find struct/trait by name | Grep "struct Foo" in .rs | LSP workspaceSymbol | Grep finds text; LSP understands semantics |
| Find all usages | Grep -r "foo(" | LSP findReferences | LSP eliminates false positives (comments, strings) |
| Find trait implementers | Grep -r "impl Foo" | LSP goToImplementation | LSP finds blanket impls, generics that grep misses |
| Get type signature | Read source, guess | LSP hover | LSP shows resolved types + trait bounds |
| Trace callers | Manual grep chain | LSP prepareCallHierarchy → incomingCalls | LSP gives complete call graph |

**Setup:**
```json
// .claude/settings.json
{
  "enabledPlugins": {
    "rust-analyzer-lsp@claude-plugins-official": true
  }
}
```

**Examples:**
- Hover over `PackRegistry` → see full type signature + trait bounds
- Right-click on trait → "Find Implementations" → see all 12 implementers
- Rename `SyncOptions` field → all 47 usages updated atomically
- Set breakpoint, step through code, inspect `manifest` variable at breakpoint

**See:** `.claude/rules/rust/lsp.md` (full LSP-first contract)

---

### 5. Documentation Surface
**What:** Quick references and in-editor help that answer "how do I...?" without leaving the IDE.

**Why:** Documentation not in-editor won't be read. A 5-second lookup in CLAUDE.md beats a 5-minute web search.

**Done When:**
- ✅ CLAUDE.md pinned in editor (single source of truth for rules, commands, workflow)
- ✅ LSP docstring examples compile and run (`just test-doc` validates)
- ✅ Error messages link to relevant docs (e.g., "See docs/troubleshooting.md#timeout")
- ✅ `just --list` shows one-liner for each command (inline help)
- ✅ `.claude/rules/` organized by topic (rust/, andon/, otel-validation.md, coding-agent-mistakes.md)
- ✅ Rules auto-load based on front-matter (priority, category, auto_load flags)
- ✅ Architecture reference updated (COMPRESSED_REFERENCE.md)
- ✅ Troubleshooting guide covers common errors
- ✅ QUICK_START guide for new developers (<5 min to first success)

**Documentation Map:**
| File | Purpose | Audience |
|------|---------|----------|
| `CLAUDE.md` | Global rules, architecture, commands, workflow | All developers |
| `.claude/rules/_core/absolute.md` | Non-negotiable rules (Andon, just, batch operations) | All developers (critical) |
| `.claude/rules/rust/lsp.md` | LSP-first navigation (forbidden vs. required) | Rust developers |
| `.claude/rules/rust/testing.md` | Chicago TDD (no mocks, real collaborators) | Test engineers |
| `.claude/rules/otel-validation.md` | OTEL span verification for LLM/external services | LLM developers |
| `docs/architecture/COMPRESSED_REFERENCE.md` | C4 model, sync flow, error map, stub classification | Architects |
| `docs/troubleshooting.md` | Common errors, root causes, fixes | Debugging developers |
| `docs/QUICK_START.md` | New developer onboarding (<5 min to first success) | New developers |

**Examples:**
- Developer asks "What's the workflow?" → CLAUDE.md shows 4-step cycle (Spec → TDD → Validate → Sync)
- Developer sees timeout error → error message links docs/troubleshooting.md#timeout
- New developer → docs/QUICK_START.md → 5 minutes → `just test-unit` passes

**See:** `CLAUDE.md`, `.claude/rules/`, `docs/`

---

### 6. Hot Reload / Iteration
**What:** Fast feedback loops that let developers verify changes in <2 seconds.

**Why:** Feedback delay kills productivity. <2s edit-save-test loop keeps flow. >10s loop breaks concentration.

**Done When:**
- ✅ `just check` completes in <5s (workspace check, no build)
- ✅ `just test-unit` completes in <10s (unit tests only)
- ✅ `cargo watch -x 'just test-unit'` auto-runs on file save
- ✅ Incremental compilation <2s (dependency caching optimized)
- ✅ First build <15s (SLO target)
- ✅ Doc examples validated without full workspace rebuild
- ✅ Test failure includes exact line and assertion

**Fast Feedback Loops:**
| Loop | Command | Timeout | Triggers | Verifies |
|------|---------|---------|----------|----------|
| Syntax | `just check` | <5s | After save | Compilation, no syntax errors |
| Unit test | `just test-unit` | <10s | After impl | RED-GREEN-REFACTOR cycle |
| Lint | `just lint` | <60s | Before pre-commit | Style, clippy warnings |
| Doc test | `just test-doc` | <60s | After docstring | Examples compile |
| OTEL | `RUST_LOG=trace just test` | varies | LLM features | Spans exist, attributes populated |

**Cargo Watch Setup:**
```bash
cargo watch -x 'just test-unit'
# On save: triggers → runs test-unit in 3-10s → feedback
```

**Performance Targets** (from `.claude/settings.json`):
- First build: <15s
- Incremental: <2s
- RDF processing: <5s
- Generation memory: <100MB
- CLI scaffolding: <3s

**Examples:**
- Edit src/lib.rs → save → cargo watch triggers → 8s later: unit tests green
- Add docstring example → save → rust-analyzer highlights error → fix → save → 45s: doc test validates
- Edit test → save → 2s: test fails → fix → save → 2s: test passes

**See:** `justfile`, `Makefile.toml`, `.claude/settings.json`

---

### 7. Defaults
**What:** Sensible project-wide conventions that reduce decisions and configuration burden.

**Why:** Defaults are the path of least resistance. Good defaults (type-first, Chicago TDD) make the right thing easy.

**Done When:**
- ✅ Rust edition pinned (2021) and enforced
- ✅ RUST_VERSION pinned (1.91.1)
- ✅ RNG_SEED fixed (42) for determinism
- ✅ RUST_BACKTRACE enabled (1) for error messages
- ✅ Chicago TDD assumed (no mocks, real collaborators)
- ✅ Type-first design default (encode invariants in types)
- ✅ OTEL tracing enabled (toggled via RUST_LOG env var)
- ✅ Error handling: Result<T,E> required (zero unwrap/expect in production)
- ✅ Logging uses structured log! macros (not print!/println!)
- ✅ Formatting auto-applied (just fmt before commit)
- ✅ SLO-first (performance targets defined before implementation)

**Environment Defaults** (`.claude/settings.json`):
```json
{
  "env": {
    "RUST_VERSION": "1.91.1",
    "RUST_BACKTRACE": "1",
    "RNG_SEED": "42",
    "CARGO_TERM_COLOR": "always",
    "ANDON_MONITORING": "true",
    "CHICAGO_TDD": "true",
    "TEST_COVERAGE_TARGET": "80",
    "RDF_SOURCE_OF_TRUTH": ".specify"
  }
}
```

**Code Defaults:**
| Default | Enforced By | Impact |
|---------|------------|--------|
| Chicago TDD, no mocks | Code review, testing-forbidden.md | Tests prove real behavior |
| Type-first design | Architecture guidance, code review | Compile-time guarantees |
| Result<T,E> over unwrap | Clippy rule, code review | Graceful error handling |
| log! macros, not print! | Clippy, code review | Production-grade observability |
| RDF is source of truth (.ttl) | Workflow, ggen sync receipt | Single source of truth |
| Deterministic (RNG_SEED=42) | Test infrastructure | Reproducible failures |
| Formatting enforced | pre-commit hook, lint gate | Consistent code style |
| OTEL tracing available | Feature implementation | LLM calls visible without code change |

**Examples:**
- New developer: "How do I structure a test?" → Chicago TDD default → no mocks, real collaborators
- Developer writes `unwrap()` → clippy flags it → replace with proper error handling
- Edit generated .md → ggen sync overwrites it → lesson: edit .ttl, not .md
- LLM call not working? → `export RUST_LOG=trace,ggen_ai=trace && cargo test` → OTEL spans visible

**See:** `.claude/settings.json`, `.claude/rules/`

---

## Definition of Done Verification

Before declaring DX "done," check all 7 gates:

### Gate 1: Command Ergonomics
```bash
just --list  # ≥15 tasks visible
just check   # Runs in <5s
just test-unit  # Runs in <10s
```

### Gate 2: Error Messages
- [ ] New error includes file:line + context
- [ ] Validation error includes GGEN-* code
- [ ] Recovery path documented (e.g., "See docs/X.md")

### Gate 3: Workflow Efficiency
```bash
alias jc='just check'  # Aliases defined
cargo watch -x 'just test-unit'  # cargo watch works
# Git hooks enforce quality gates
```

### Gate 4: IDE Integration
- [ ] rust-analyzer-lsp enabled
- [ ] Go to Definition works
- [ ] Find References finds all usages
- [ ] Hover shows type signatures

### Gate 5: Documentation
- [ ] CLAUDE.md covers all critical commands
- [ ] Error messages link to docs
- [ ] `.claude/rules/` organized by topic
- [ ] QUICK_START.md exists

### Gate 6: Hot Reload
```bash
just check        # <5s
just test-unit    # <10s
cargo watch       # Auto-runs on save
```

### Gate 7: Defaults
- [ ] Chicago TDD enforced (no mocks in production)
- [ ] Type-first design default
- [ ] Result<T,E> required (zero unwrap)
- [ ] OTEL tracing toggleable via env var
- [ ] SLO targets in .claude/settings.json

---

## How to Use This Document

1. **New Developer**: Start with **Dimension 5 (Documentation)** → QUICK_START.md
2. **Debugging**: Check **Dimension 2 (Error Messages)** for diagnostic codes
3. **Slow Iteration**: Check **Dimension 6 (Hot Reload)** → optimize build times
4. **Unknown Commands**: Check **Dimension 1 (Command Ergonomics)** → `just --list`
5. **Slow IDE**: Check **Dimension 4 (IDE Integration)** → rust-analyzer setup
6. **Architecture Decision**: Check **Dimension 7 (Defaults)** → encode invariant in types

---

## See Also

- `DEFINITION_OF_DONE.json` — Detailed JSON spec with all examples and verification steps
- `CLAUDE.md` — Global rules, architecture, workflow, diagnostic codes
- `.claude/rules/` — Topic-specific rules (LSP, testing, OTEL, etc.)
- `justfile` — All available commands
- `docs/architecture/COMPRESSED_REFERENCE.md` — System architecture
- `docs/troubleshooting.md` — Common errors and solutions

---

## Questions?

- **"How do I run X?"** → See CLAUDE.md (Commands section) or `just --list`
- **"Why is my build slow?"** → See docs/troubleshooting.md#build-slow
- **"What's the testing policy?"** → See `.claude/rules/rust/testing.md` (Chicago TDD)
- **"How do I verify my LLM call?"** → See `.claude/rules/otel-validation.md` (OTEL spans)

---

**Version:** 6.0.0 | **Last Updated:** 2026-06-14
