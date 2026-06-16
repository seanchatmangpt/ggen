<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [DX Definition of Done — Completion Checklist](#dx-definition-of-done--completion-checklist)
  - [Dimension 1: Command Ergonomics ✓](#dimension-1-command-ergonomics-%E2%9C%93)
    - [Commands (All Must Exist)](#commands-all-must-exist)
    - [Verification](#verification)
  - [Dimension 2: Error Messages ✓](#dimension-2-error-messages-%E2%9C%93)
    - [Error Message Quality](#error-message-quality)
    - [Diagnostic Codes (All Must Be Documented)](#diagnostic-codes-all-must-be-documented)
    - [Recovery Paths](#recovery-paths)
    - [Verification](#verification-1)
  - [Dimension 3: Workflow Efficiency ✓](#dimension-3-workflow-efficiency-%E2%9C%93)
    - [Keyboard Shortcuts](#keyboard-shortcuts)
    - [Aliases](#aliases)
    - [Automation](#automation)
    - [Feedback Loop Latency](#feedback-loop-latency)
    - [Verification](#verification-2)
  - [Dimension 4: IDE Integration ✓](#dimension-4-ide-integration-%E2%9C%93)
    - [LSP Setup](#lsp-setup)
    - [Live Diagnostics](#live-diagnostics)
    - [Navigation (All Must Work)](#navigation-all-must-work)
    - [Refactoring](#refactoring)
    - [Debugging](#debugging)
    - [Performance Profiling](#performance-profiling)
    - [Verification](#verification-3)
  - [Dimension 5: Documentation Surface ✓](#dimension-5-documentation-surface-%E2%9C%93)
    - [Single Source of Truth](#single-source-of-truth)
    - [Rules Organization](#rules-organization)
    - [Front-Matter](#front-matter)
    - [LSP Docstring Examples](#lsp-docstring-examples)
    - [Error Message Links](#error-message-links)
    - [Command Help](#command-help)
    - [Architecture Reference](#architecture-reference)
    - [Troubleshooting Guide](#troubleshooting-guide)
    - [Quick Start Guide](#quick-start-guide)
    - [Verification](#verification-4)
  - [Dimension 6: Hot Reload / Iteration ✓](#dimension-6-hot-reload--iteration-%E2%9C%93)
    - [Command Latency](#command-latency)
    - [Incremental Compilation](#incremental-compilation)
    - [Hot Reload Integration](#hot-reload-integration)
    - [Feedback Clarity](#feedback-clarity)
    - [Doc Test Validation](#doc-test-validation)
    - [Performance Targets](#performance-targets)
    - [Verification](#verification-5)
  - [Dimension 7: Defaults ✓](#dimension-7-defaults-%E2%9C%93)
    - [Environment Defaults](#environment-defaults)
    - [Code Defaults](#code-defaults)
    - [Verification](#verification-6)
  - [Overall Completion Status](#overall-completion-status)
  - [Sign-Off](#sign-off)
  - [Notes](#notes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# DX Definition of Done — Completion Checklist

Use this checklist to verify that all 7 DX dimensions are complete. Each dimension has a gate. All gates must pass before DX is "done."

---

## Dimension 1: Command Ergonomics ✓

**Gate:** All critical tasks callable via `just <task>` with sensible timeouts.

### Commands (All Must Exist)
- [ ] `just check` — Compilation check (workspace) — **Timeout: <5s**
- [ ] `just test` — Full test suite (unit + integration) — **Timeout: <30s**
- [ ] `just test-unit` — Unit tests only — **Timeout: <10s**
- [ ] `just test-lib` — Lib tests inside crates — **Timeout: <30s**
- [ ] `just test-doc` — Validate /// Examples blocks — **Timeout: <60s**
- [ ] `just test-bdd` — BDD aspirational specs (opt-in) — **Timeout: <300s**
- [ ] `just test-marketplace` — Fast marketplace validation — **Timeout: <30s**
- [ ] `just test-marketplace-full` — Full marketplace gate (network) — **Timeout: varies**
- [ ] `just test-mutation` — Mutation testing (≥60% score) — **Timeout: <5min**
- [ ] `just lint` — Clippy + rustfmt — **Timeout: <60s**
- [ ] `just fmt` — Format all code — **Timeout: <30s**
- [ ] `just fmt-check` — Check formatting without modifying — **Timeout: <30s**
- [ ] `just pre-commit` — Full gate (fmt → check → lint → test-unit) — **Timeout: <2min**
- [ ] `just slo-check` — Performance SLO validation — **Timeout: varies**
- [ ] `just audit` — Security vulnerability scan — **Timeout: <30s**
- [ ] `just doc` — Build API docs — **Timeout: <30s**
- [ ] `just bench` — Run Criterion benchmarks — **Timeout: <5min**
- [ ] `just sync` — Full μ₁-μ₅ pipeline with receipt — **Timeout: varies**
- [ ] `just sync-dry` — Preview sync without writing — **Timeout: varies**
- [ ] `just timeout-check` — Verify timeout command exists — **Timeout: <2s**

### Verification
```bash
# All commands exist
just --list | wc -l  # Should show ≥20 commands

# Timeouts respected
time just check      # Should be <5s
time just test-unit  # Should be <10s
time just lint       # Should be <60s

# No direct cargo calls needed
# Verify: no workflows in docs say "cargo test", "cargo build", etc.
grep -r "^cargo " docs/  # Should find nothing (only "just" commands)
```

**Status:** ☐ Complete

---

## Dimension 2: Error Messages ✓

**Gate:** Errors and warnings must guide developers to root cause and recovery path.

### Error Message Quality
- [ ] Compiler errors include **file:line** and context (not just line number)
  ```
  ✅ error[E0308]: expected `String`, found `&str`
     → src/main.rs:42:15
     = help: try: `let x = "hello".to_string();`
  
  ❌ error: type mismatch
  ```

- [ ] Validation errors link to **diagnostic codes** (GGEN-TPL-001, GGEN-OUT-001, etc.)
  ```
  ✅ error[GGEN-TPL-001]: Template variable {{ var }} not in SELECT
     → ggen.toml:15:5
     = recovery: Add {{ var }} to SELECT clause
  ```

- [ ] Timeout errors suggest **root cause** and debugging steps
  ```
  ✅ error: command timed out (120s)
     → just test (full test suite)
     = This may indicate:
     - First compile: `cargo clean && just test`
     - Slow test: add `#[ignore]`
     = See: docs/troubleshooting.md#timeout
  
  ❌ Timeout
  ```

- [ ] Error messages **avoid jargon** and explain in developer terms
  ```
  ✅ "Variable `x` is not a string"
  ❌ "Type `&str` is not assignable to `String`"
  ```

### Diagnostic Codes (All Must Be Documented)
- [ ] GGEN-TPL-001 — Template variable not in SELECT
- [ ] GGEN-OUT-001 — output_file pattern has unbound variable
- [ ] GGEN-YIELD-001 — output_file escapes project root
- [ ] GGEN-RULE-001 — {file = ...} binding points to missing file
- [ ] GGEN-QUERY-002 — SELECT * disables provision checks
- [ ] E0011 / E0013 — CONSTRUCT/SELECT lacks ORDER BY

### Recovery Paths
- [ ] Each error code has a documented recovery path
  ```
  error[GGEN-TPL-001]: ... 
  = recovery: Add {{ var }} to SELECT clause
  = see: docs/troubleshooting.md#ggen-tpl-001
  ```

### Verification
```bash
# New error messages reviewed for actionability
# Check: does error message guide user to fix?

# Diagnostic codes documented
grep -r "GGEN-" .claude/rules/ docs/  # Should find all 6 codes

# Error messages link to docs
grep -r "docs/troubleshooting.md" src/  # Should find error message linking
```

**Status:** ☐ Complete

---

## Dimension 3: Workflow Efficiency ✓

**Gate:** Common dev tasks take <5s or have cargo watch integration.

### Keyboard Shortcuts
- [ ] Ctrl+Shift+B bound to build command
- [ ] Ctrl+Shift+T bound to test command
- [ ] Ctrl+Shift+L bound to lint command
- [ ] Other shortcuts documented in docs/QUICK_START.md

### Aliases
- [ ] `jc` aliases to `just check`
- [ ] `jt` aliases to `just test-unit`
- [ ] `jl` aliases to `just lint`
- [ ] `jp` aliases to `just pre-commit`
- [ ] `js` aliases to `just sync-dry`

**Setup:**
```bash
# Add to ~/.bashrc or ~/.zshrc
alias jc='just check'
alias jt='just test-unit'
alias jl='just lint'
alias jp='just pre-commit'
alias js='just sync-dry'
```

### Automation
- [ ] `cargo watch` integration works
  ```bash
  cargo watch -x 'just test-unit'
  # On save: auto-runs unit tests
  ```

- [ ] Git pre-commit hook enforces quality gates
  ```bash
  cat .git/hooks/pre-commit
  # Should call: just pre-commit
  ```

- [ ] Git pre-push hook prevents force-push to main
  ```bash
  cat .git/hooks/pre-push
  # Should check: git branch matches feature pattern
  ```

- [ ] OTEL tracing toggleable via env var (no code changes)
  ```bash
  export RUST_LOG=trace,ggen_ai=trace
  cargo test  # Now shows OTEL spans
  ```

### Feedback Loop Latency
- [ ] Edit → save → test feedback <2 seconds
  ```bash
  # Measure: edit file → save → `cargo watch` runs test
  # Should see: test output in <2s
  ```

### Verification
```bash
# Aliases defined
alias jc  # Should show: just check

# cargo watch works
cargo watch -x 'just test-unit' &
sleep 1
touch src/lib.rs  # Modify a file
# Should see: cargo watch triggers, tests run

# Git hooks exist
ls -la .git/hooks/pre-commit
ls -la .git/hooks/pre-push

# OTEL toggle works
export RUST_LOG=trace,ggen_ai=trace
cargo test --lib 2>&1 | grep "llm\."  # Should find OTEL spans
```

**Status:** ☐ Complete

---

## Dimension 4: IDE Integration ✓

**Gate:** LSP, debugging, and editor support surface compiler knowledge without context switches.

### LSP Setup
- [ ] rust-analyzer-lsp enabled in `.claude/settings.json`
  ```json
  {
    "enabledPlugins": {
      "rust-analyzer-lsp@claude-plugins-official": true
    }
  }
  ```

### Live Diagnostics
- [ ] Compiler errors appear as red squiggles (not after compile)
- [ ] Hover over symbol shows type signature
- [ ] Hover shows trait bounds and visibility
- [ ] Syntax errors caught before running `just check`

### Navigation (All Must Work)
- [ ] **Go to Definition** (`F12` or Cmd+Click)
  - Jumps to actual definition
  - Respects re-exports
  - Works across all 15 crates

- [ ] **Find All References** (Ctrl+Shift+F10)
  - Finds all usages of symbol
  - Eliminates false positives (comments, strings)
  - Workspace-wide

- [ ] **Go to Implementation** (Ctrl+Shift+I)
  - Finds all types implementing trait
  - Includes blanket impls
  - Works for project-defined traits

- [ ] **Find Symbol** (Ctrl+T or Cmd+T)
  - `LSP workspaceSymbol` works
  - Type name → jumps to definition
  - Works across all crates

- [ ] **Code Completion** (Ctrl+Space)
  - Suggests symbols from all crates
  - Respects visibility (pub vs. private)
  - Shows documentation on hover

### Refactoring
- [ ] **Rename** (Ctrl+H)
  - Renames all usages atomically
  - No missed references
  - Works across crates

### Debugging
- [ ] Breakpoints can be set
- [ ] Step over / step into works
- [ ] Variable inspection shows values
- [ ] Call stack visible

### Performance Profiling
- [ ] Flame graphs can be generated
- [ ] Memory profiling integrated
- [ ] Criterion benchmarks viewable

### Verification
```bash
# LSP enabled
grep "rust-analyzer-lsp" .claude/settings.json  # Should find: true

# LSP working
# In editor: hover over any symbol → should show type signature
# In editor: press Ctrl+T → should open symbol search
# In editor: right-click → "Go to Definition" → should jump

# Test in editor:
# 1. Open any .rs file
# 2. Hover over a function name → should show signature
# 3. Ctrl+Click on function → should jump to definition
# 4. Ctrl+Shift+F10 on function → should show all references
```

**Status:** ☐ Complete

---

## Dimension 5: Documentation Surface ✓

**Gate:** Quick references and in-editor help answer "how do I...?" without leaving IDE.

### Single Source of Truth
- [ ] CLAUDE.md exists and covers:
  - [ ] All commands (just check, just test, etc.)
  - [ ] Architecture reference (15 crates, cross-cutting patterns)
  - [ ] Workflow (4-step cycle: Spec → TDD → Validate → Sync)
  - [ ] Diagnostic codes (GGEN-TPL-001, etc.)
  - [ ] Evidence-First Principle (no fabrication)
  - [ ] Rules (agent coordination, tool restrictions)

### Rules Organization
- [ ] `.claude/rules/` organized by topic:
  - [ ] `_core/absolute.md` — Non-negotiable rules
  - [ ] `_core/workflow.md` — 4-step development cycle
  - [ ] `rust/lsp.md` — LSP-first navigation
  - [ ] `rust/testing.md` — Chicago TDD
  - [ ] `rust/testing-forbidden.md` — Forbidden London TDD patterns
  - [ ] `rust/elite-mindset.md` — Type-first design
  - [ ] `rust/performance.md` — Performance SLOs
  - [ ] `andon/signals.md` — Stop the line protocol
  - [ ] `otel-validation.md` — OTEL span verification
  - [ ] `coding-agent-mistakes.md` — Mandatory gate (5 mistakes, 6-question contract)
  - [ ] `validation-persistence.md` — Keep working until validation passes
  - [ ] `testing-anti-cheating.md` — Multi-surface corroboration

### Front-Matter
- [ ] All rules have front-matter with:
  ```markdown
  ---
  auto_load: true/false
  category: string (rust, andon, testing, etc.)
  priority: critical/high/medium/low
  version: semver
  ---
  ```

- [ ] Auto-load rules processed on session start
  ```json
  // .claude/hooks/session-start.sh
  // Should load all rules with auto_load: true
  ```

### LSP Docstring Examples
- [ ] All `/// Examples` blocks compile and run
  ```bash
  just test-doc  # Validates all examples
  ```

- [ ] Examples are realistic (not stub code)
- [ ] Examples show actual use cases

### Error Message Links
- [ ] Compiler errors link to relevant docs
  ```
  = see: docs/troubleshooting.md#build-slow
  = see: .claude/rules/rust/lsp.md#forbidden
  ```

### Command Help
- [ ] `just --list` shows all tasks with one-liner descriptions
  ```bash
  just --list
  # Output should include:
  # check              Compilation check
  # test-unit          Unit tests only
  # test               Full test suite
  # (etc.)
  ```

### Architecture Reference
- [ ] `docs/architecture/COMPRESSED_REFERENCE.md` exists with:
  - [ ] C4 model (context, containers, components, code)
  - [ ] Real sync flow diagram
  - [ ] Error map (all error paths documented)
  - [ ] Stub registry (54 stubs classified)
  - [ ] LSP-surveyed (all public APIs documented)

### Troubleshooting Guide
- [ ] `docs/troubleshooting.md` covers:
  - [ ] Timeout errors (why, how to debug)
  - [ ] Flaky tests (causes, fixes)
  - [ ] Build failures (common causes)
  - [ ] LSP not working (setup verification)
  - [ ] Link to each GGEN-* diagnostic code

### Quick Start Guide
- [ ] `docs/QUICK_START.md` exists with:
  - [ ] System requirements (Rust version, rustup)
  - [ ] Installation (<5 min)
  - [ ] First command (`just test-unit`)
  - [ ] Common tasks (editing, testing, committing)
  - [ ] Troubleshooting links

### Verification
```bash
# CLAUDE.md exists and is comprehensive
wc -l CLAUDE.md  # Should be >500 lines

# All rules documented
ls -la .claude/rules/
# Should include: _core/, rust/, andon/, otel-validation.md, etc.

# Error messages link to docs
grep -r "docs/" src/ | head -5
# Should find error messages with doc links

# `just --list` shows descriptions
just --list | head -10
# Should see: "check              Compilation check"

# Architecture reference exists
ls -la docs/architecture/COMPRESSED_REFERENCE.md
# Should exist

# Troubleshooting guide exists
ls -la docs/troubleshooting.md
# Should exist

# Quick start exists
ls -la docs/QUICK_START.md
# Should exist

# All docstring examples compile
just test-doc
# Should pass all doctests
```

**Status:** ☐ Complete

---

## Dimension 6: Hot Reload / Iteration ✓

**Gate:** Fast feedback loops that let developers verify changes in <2 seconds.

### Command Latency
- [ ] `just check` completes in **<5s**
  ```bash
  time just check  # Should show: real 0m4.xxx
  ```

- [ ] `just test-unit` completes in **<10s**
  ```bash
  time just test-unit  # Should show: real 0m8.xxx
  ```

- [ ] `just lint` completes in **<60s**
  ```bash
  time just lint  # Should show: real 0m45.xxx
  ```

- [ ] `just test-doc` completes in **<60s**
  ```bash
  time just test-doc  # Should show: real 0m50.xxx
  ```

### Incremental Compilation
- [ ] First build <15s (SLO target)
  ```bash
  cargo clean
  time cargo build  # Should show: real 0m14.xxx
  ```

- [ ] Incremental build <2s (after small change)
  ```bash
  # Edit one line in src/lib.rs
  time cargo check  # Should show: real 0m1.xxx
  ```

### Hot Reload Integration
- [ ] `cargo watch` auto-runs on file change
  ```bash
  cargo watch -x 'just test-unit' &
  sleep 1
  touch src/lib.rs
  # Should see test output within 3 seconds
  ```

### Feedback Clarity
- [ ] Test failures show exact line and assertion
  ```
  ✅ thread 'main' panicked at 'assertion failed: 5 == 3'
     → src/tests.rs:42:5
  
  ❌ test failed
  ```

- [ ] Compiler errors show context
  ```
  ✅ error[E0308]: expected `String`, found `&str`
     → src/main.rs:42:15
  
  ❌ type error
  ```

### Doc Test Validation
- [ ] `just test-doc` doesn't require full workspace rebuild
- [ ] Examples validated independently
- [ ] Output visible in <60s

### Performance Targets
- [ ] SLO targets defined in `.claude/settings.json`:
  ```json
  {
    "SLO_FIRST_BUILD_MAX": "15s",
    "SLO_INCREMENTAL_MAX": "2s",
    "SLO_RDF_PROCESSING_MAX": "5s",
    "SLO_GENERATION_MEMORY_MAX": "100MB",
    "SLO_CLI_SCAFFOLDING_MAX": "3s"
  }
  ```

- [ ] `just slo-check` validates against targets
  ```bash
  just slo-check  # Should pass (all SLOs met)
  ```

### Verification
```bash
# Check latencies
time just check       # <5s
time just test-unit   # <10s
time just lint        # <60s

# Check incremental build
cargo clean && time cargo build  # <15s
touch src/lib.rs && time cargo check  # <2s

# Check cargo watch
cargo watch -x 'just test-unit' &
sleep 1
touch src/lib.rs
# Should see: "Running `cargo ...` (exit code: 0)"

# Check SLO targets
grep "SLO_" .claude/settings.json
```

**Status:** ☐ Complete

---

## Dimension 7: Defaults ✓

**Gate:** Sensible project-wide conventions reduce decisions and configuration burden.

### Environment Defaults
- [ ] Rust version pinned (`.claude/settings.json` + `rust-toolchain.toml`)
  ```json
  "RUST_VERSION": "1.91.1"
  ```

- [ ] RUST_BACKTRACE enabled (1)
  ```json
  "RUST_BACKTRACE": "1"
  ```

- [ ] RNG_SEED fixed (42) for determinism
  ```json
  "RNG_SEED": "42"
  ```

- [ ] CARGO_TERM_COLOR always
  ```json
  "CARGO_TERM_COLOR": "always"
  ```

- [ ] Monitoring flags enabled
  ```json
  "ANDON_MONITORING": "true",
  "CHICAGO_TDD": "true",
  "DFLSS_MODE": "true"
  ```

- [ ] Coverage target set (80%)
  ```json
  "TEST_COVERAGE_TARGET": "80"
  ```

- [ ] RDF source of truth
  ```json
  "RDF_SOURCE_OF_TRUTH": ".specify"
  ```

### Code Defaults
- [ ] Chicago TDD enforced (no mocks)
  ```bash
  # Zero London TDD in production
  grep -r "mockall\|#\[automock\]" src/  # Should find NOTHING
  grep -r "struct Mock" src/  # Should find NOTHING
  ```

- [ ] Type-first design default
  - [ ] Invariants encoded in types (not assertions)
  - [ ] Newtype wrappers for domain concepts
  - [ ] Typestate for state transitions
  - [ ] PhantomData for compile-time guarantees

- [ ] Result<T,E> required (zero unwrap/expect in production)
  ```bash
  # Zero unwrap/expect in production code
  grep -r "\.unwrap()\|\.expect(" src/ | grep -v "src/bin" | grep -v tests  # Should find NOTHING
  ```

- [ ] Logging uses log! macros (not print!/println!)
  ```bash
  # No print! or println! in libraries
  grep -r "print!\|println!" crates/ggen-*/src/  # Should find NOTHING
  ```

- [ ] RDF is source of truth (.ttl not .md)
  - [ ] Edit `.specify/*.ttl`, run `ggen sync`
  - [ ] Never manually edit generated .md files
  - [ ] Workflow enforces this

- [ ] Determinism (RNG_SEED=42)
  - [ ] Random number generators seeded
  - [ ] Tests run deterministically
  - [ ] Same seed = same output

- [ ] Formatting enforced (just fmt)
  - [ ] `just fmt` before commit
  - [ ] `just fmt-check` in pre-commit hook
  - [ ] Consistent code style across codebase

- [ ] OTEL tracing available
  - [ ] Toggleable via `RUST_LOG=trace,ggen_ai=trace`
  - [ ] No code changes needed
  - [ ] Spans emitted for LLM calls, external services

- [ ] SLO-first (performance targets before implementation)
  - [ ] Targets defined in `.claude/settings.json`
  - [ ] `just slo-check` validates before release
  - [ ] Performance regression caught early

### Verification
```bash
# Check environment defaults
grep "RUST_VERSION\|RUST_BACKTRACE\|RNG_SEED" .claude/settings.json

# Check no mocks in production
grep -r "mockall" src/  # Should find NOTHING

# Check no unwrap in production
grep -r "\.unwrap()" src/ | grep -v tests | grep -v bin  # Should find NOTHING

# Check no print in libraries
grep -r "println!" crates/ggen-*/src/  # Should find NOTHING

# Check formatting enforced
ls -la .git/hooks/pre-commit
grep "fmt-check" .git/hooks/pre-commit  # Should find it

# Check OTEL toggle works
export RUST_LOG=trace,ggen_ai=trace
cargo test --lib 2>&1 | grep "llm\."  # Should find OTEL spans

# Check SLO targets
grep "SLO_" .claude/settings.json
just slo-check  # Should pass
```

**Status:** ☐ Complete

---

## Overall Completion Status

| Dimension | Status | Notes |
|-----------|--------|-------|
| 1. Command Ergonomics | ☐ Complete | 20+ commands with sensible timeouts |
| 2. Error Messages | ☐ Complete | GGEN-* codes, recovery paths documented |
| 3. Workflow Efficiency | ☐ Complete | Aliases, cargo watch, git hooks, <2s feedback |
| 4. IDE Integration | ☐ Complete | rust-analyzer-lsp, Go to Def, Find Refs, debugging |
| 5. Documentation | ☐ Complete | CLAUDE.md, .claude/rules/*, QUICK_START, troubleshooting |
| 6. Hot Reload | ☐ Complete | <5s check, <10s test-unit, cargo watch, incremental <2s |
| 7. Defaults | ☐ Complete | Chicago TDD, type-first, Result<T,E>, OTEL, SLO-first |

**Final Status:** ☐ **DX DEFINITION OF DONE — ALL 7 DIMENSIONS COMPLETE**

---

## Sign-Off

- [ ] All 7 dimensions verified
- [ ] All gates passed
- [ ] DX is frictionless
- [ ] Developers can iterate fast
- [ ] New developers can be productive in <5 min

**Verified by:** _________________ **Date:** _________

---

## Notes

- This checklist should be reviewed **quarterly** as the project evolves
- Pain points discovered in daily development should trigger updates
- Measurement: track (1) time-to-first-success for new devs, (2) time-to-resolution for common errors, (3) feedback loop latency, (4) defect escape rate

**See Also:** `DEFINITION_OF_DONE.json` (detailed spec), `README.md` (quick reference)
