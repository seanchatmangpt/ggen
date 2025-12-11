# Quickstart Guide: Validating ACI Optimization

**Feature**: 003-optimize-aci-anthropic
**Date**: 2025-12-11
**Purpose**: Step-by-step guide to validate Agent-Computer Interface improvements

## Overview

This guide shows how to validate that ggen's ACI optimization delivers on its success criteria:
- **SC-001**: 90% tool selection accuracy
- **SC-002**: 95% Andon signal interpretation accuracy
- **SC-003**: 60% reduction in SLO violations
- **SC-004**: 80% principle reference rate without prompting
- **SC-005**: 40% faster time to first clean compilation
- **SC-008**: 50% reduction in defect escape rate

## Prerequisites

Before validating ACI improvements, ensure you have:

```bash
# 1. Rust toolchain installed
rustc --version  # Should be 1.74+

# 2. cargo-make installed
cargo install cargo-make

# 3. Branch checked out
git checkout 003-optimize-aci-anthropic

# 4. Dependencies up to date
cargo update
```

## Validation Workflow (5 Steps)

### Step 1: Validate Enhanced Tool Documentation (P1)

**Goal**: Verify cargo make targets have comprehensive descriptions following Anthropic's ACI guidelines.

```bash
# 1.1 Check Makefile.toml has enhanced descriptions
cat Makefile.toml | grep -A 20 '\[tasks.check\]'

# Expected: Multi-line description with:
# - Purpose: What the target does
# - Timing: When to use it
# - SLO: Performance threshold (<5s)
# - Example outputs: RED/GREEN signal formats
# - Error recovery: What to do when it fails

# 1.2 Validate description length (must be >100 chars)
cargo make --list-all-steps | head -20

# Expected: Detailed descriptions, not sparse one-liners
```

**Success Criteria**: Each cargo make target description includes all 5 components (purpose, timing, SLO, examples, recovery) and exceeds 100 characters.

---

### Step 2: Validate Poka-Yoke Tool Design (P2)

**Goal**: Verify tools prevent mistakes automatically through timeouts, warnings-as-errors, and quality gates.

```bash
# 2.1 Verify automatic timeout enforcement
grep -r 'command = "timeout"' Makefile.toml

# Expected: All critical targets use timeout wrapper
# - cargo make check → timeout 5s
# - cargo make test → timeout 60s
# - cargo make lint → timeout 10s

# 2.2 Verify warnings-as-errors enforcement
grep 'RUSTFLAGS.*-D warnings' Makefile.toml

# Expected: RUSTFLAGS = "-D warnings" set on cargo make check

# 2.3 Test timeout enforcement (intentional hang detection)
# This should FAIL with timeout after 5s (RED Andon signal)
timeout 5s cargo check --all-targets

# Expected output:
# "error: build failed"
# OR "Command timed out after 5 seconds"
```

**Success Criteria**:
- All cargo make targets have explicit timeouts
- Warnings treated as errors (no YELLOW signals allowed)
- Timeouts prevent hanging builds

---

### Step 3: Validate Auto-Invoked Constitution Skill (P3)

**Goal**: Verify constitution loads automatically when working on ggen code based on keywords.

```bash
# 3.1 Check skill file exists
ls -lh ~/.claude/skills/ggen/constitution.md

# Expected: File present with YAML frontmatter

# 3.2 Verify YAML frontmatter structure
head -30 ~/.claude/skills/ggen/constitution.md

# Expected frontmatter:
# ---
# description: >
#   ggen Constitution v1.0.0 architectural principles.
#   Auto-invoke WHEN: "ggen", "cargo make", "unwrap", "TDD"
#   Do NOT load for: "general Rust", "external project"
# trigger_keywords: [cargo make, unwrap, expect, Result<T,E>, ...]
# exclusion_keywords: [other project, external codebase, general Rust]
# version: "1.0.0"
# ---

# 3.3 Validate keyword counts
grep 'trigger_keywords:' ~/.claude/skills/ggen/constitution.md | wc -w
# Expected: ≥10 ggen-specific keywords

grep 'exclusion_keywords:' ~/.claude/skills/ggen/constitution.md | wc -w
# Expected: ≥3 boundary conditions
```

**Success Criteria**:
- Constitution skill file exists in `~/.claude/skills/ggen/`
- YAML frontmatter includes WHEN + WHEN NOT patterns
- ≥10 trigger keywords (prevents false negatives)
- ≥3 exclusion keywords (prevents contamination)

---

### Step 4: Run Comprehensive Test Suite

**Goal**: Verify all ACI validation tests pass, measuring tool selection accuracy, timeout enforcement, and skill invocation.

```bash
# 4.1 Run ACI test suite
cargo make test-aci

# Expected: New test directory tests/aci/ with 3 test modules:
# - tests/aci/tool_selection_tests.rs
# - tests/aci/timeout_enforcement_tests.rs
# - tests/aci/skill_invocation_tests.rs

# 4.2 Check test coverage
cargo tarpaulin --out Stdout --exclude-files 'tests/*' --target-dir target/tarpaulin

# Expected: 80%+ coverage on validation logic

# 4.3 Verify specific test scenarios pass
cargo test --test tool_selection_tests test_agent_selects_check_for_compilation -- --nocapture

# Expected output:
# test test_agent_selects_check_for_compilation ... ok
```

**Success Criteria**:
- All tests in `tests/aci/` pass (100% pass rate)
- Test coverage ≥80% on new validation logic
- Measurable improvements in tool selection accuracy (SC-001: 90%)

---

### Step 5: Measure Success Criteria (Evidence Collection)

**Goal**: Quantitatively verify ACI optimization delivers on 8 success criteria.

```bash
# 5.1 Create evidence directory
mkdir -p specs/003-optimize-aci-anthropic/evidence

# 5.2 Measure SC-001: Tool selection accuracy (90% target)
cargo test --test tool_selection_tests -- --nocapture > \
  specs/003-optimize-aci-anthropic/evidence/tool-selection-accuracy.txt

# 5.3 Measure SC-003: SLO violation reduction (60% target)
# Before: Count pre-optimization SLO violations
git log --all --grep="SLO" --before="2025-12-11" | wc -l

# After: Count post-optimization SLO violations (should be 60% lower)
cargo make slo-check 2>&1 | tee specs/003-optimize-aci-anthropic/evidence/slo-violations.txt

# 5.4 Measure SC-005: Time to first clean compilation (40% improvement target)
# Baseline: Clean build from scratch
cargo clean && time cargo make check 2>&1 | tee specs/003-optimize-aci-anthropic/evidence/baseline-compile-time.txt

# Optimized: With enhanced tool descriptions
time cargo make check 2>&1 | tee specs/003-optimize-aci-anthropic/evidence/optimized-compile-time.txt

# 5.5 Verify constitution skill auto-invocation (SC-004: 80% reference rate)
# Manual test: Start Claude Code conversation mentioning "cargo make"
# Expected: Constitution skill loads automatically (verify in conversation context)
```

**Success Criteria Evidence Files**:
- `tool-selection-accuracy.txt` → SC-001 (90% accuracy)
- `slo-violations.txt` → SC-003 (60% reduction)
- `baseline-compile-time.txt` + `optimized-compile-time.txt` → SC-005 (40% improvement)
- Manual Claude Code session log → SC-004 (80% auto-invocation rate)

---

## Validation Checklist

Use this checklist to verify all ACI improvements are production-ready:

### P1: Enhanced Tool Documentation
- [ ] All cargo make targets have descriptions >100 characters
- [ ] Each description includes purpose, timing, SLO, examples, recovery
- [ ] Descriptions follow Anthropic's 3-part ACI framework
- [ ] Tool selection tests pass with 90%+ accuracy

### P2: Poka-Yoke Tool Design
- [ ] All critical targets use `timeout` wrapper
- [ ] Timeouts match documented SLOs (check: 5s, test: 60s, lint: 10s)
- [ ] RUSTFLAGS="-D warnings" enforced on cargo make check
- [ ] Quality gate tests verify all signals GREEN before completion
- [ ] Timeout enforcement tests pass (SLO violation detection works)

### P3: Auto-Invoked Constitution Skill
- [ ] Constitution packaged as skill in `~/.claude/skills/ggen/constitution.md`
- [ ] YAML frontmatter includes WHEN + WHEN NOT patterns
- [ ] ≥10 trigger keywords present (cargo make, unwrap, TDD, RDF, etc.)
- [ ] ≥3 exclusion keywords present (other project, external codebase, general Rust)
- [ ] Skill auto-invokes in Claude Code sessions with ggen keywords
- [ ] Skill does NOT load for non-ggen Rust conversations

### Test Suite
- [ ] All tests in `tests/aci/` pass (0 failures)
- [ ] Test coverage ≥80% on validation logic
- [ ] Tool selection tests measure SC-001 (90% accuracy)
- [ ] Timeout enforcement tests measure SC-003 (60% SLO violation reduction)
- [ ] Skill invocation tests measure SC-004 (80% auto-load rate)

### Evidence Collection
- [ ] Evidence directory created: `specs/003-optimize-aci-anthropic/evidence/`
- [ ] Tool selection accuracy measured and documented
- [ ] SLO violation reduction quantified (before vs after)
- [ ] Compile time improvement measured (baseline vs optimized)
- [ ] Constitution auto-invocation verified in real Claude Code session
- [ ] All 8 success criteria have supporting evidence files

---

## Troubleshooting

### Issue 1: Makefile.toml descriptions don't show in cargo make --list-all-steps

**Symptom**: Running `cargo make --list-all-steps` shows only task names, not descriptions.

**Cause**: cargo-make may need `--verbose` flag to display full descriptions.

**Fix**:
```bash
cargo make --list-all-steps --verbose
```

---

### Issue 2: Timeout enforcement not working

**Symptom**: `cargo make check` hangs indefinitely despite timeout in Makefile.toml.

**Cause**: `timeout` command may not be installed or may be GNU vs BSD variant.

**Fix**:
```bash
# macOS: Install GNU timeout via coreutils
brew install coreutils
# Then use 'gtimeout' instead of 'timeout' in Makefile.toml

# Linux: timeout should be available by default
which timeout
```

---

### Issue 3: Constitution skill not auto-loading

**Symptom**: Claude Code doesn't load constitution when mentioning "cargo make" or "unwrap".

**Cause**: Skill file may not be in correct location or YAML frontmatter is malformed.

**Diagnosis**:
```bash
# 1. Verify skill file location
ls ~/.claude/skills/ggen/constitution.md

# 2. Validate YAML frontmatter syntax
head -20 ~/.claude/skills/ggen/constitution.md | yamllint -

# 3. Check Claude Code skill loading logs (if available)
# Look for "Loading skill: ggen-constitution" in Claude Code output
```

**Fix**:
```bash
# If file missing, copy from spec artifacts
cp specs/003-optimize-aci-anthropic/artifacts/constitution.md \
   ~/.claude/skills/ggen/constitution.md

# If YAML malformed, fix frontmatter syntax (ensure '---' delimiters)
```

---

### Issue 4: Tests in tests/aci/ not found by cargo test

**Symptom**: Running `cargo test --test tool_selection_tests` fails with "no test target named 'tool_selection_tests'".

**Cause**: Test files may not be properly configured in Cargo.toml workspace.

**Fix**:
```bash
# 1. Verify test files exist
ls tests/aci/

# 2. Check Cargo.toml includes tests/aci/ in workspace members or test configuration
grep -A 5 '\[workspace\]' Cargo.toml

# 3. Run tests from workspace root (not from crate subdirectory)
cd /path/to/ggen  # Workspace root
cargo test --test tool_selection_tests
```

---

## Next Steps After Validation

Once all validation steps pass:

1. **Create Pull Request**: Branch `003-optimize-aci-anthropic` → `master`
2. **Run Full CI Pipeline**: `cargo make ci` (includes all quality gates)
3. **Document Results**: Update evidence files with final metrics
4. **Update Constitution**: If ACI optimization reveals new principles, amend constitution
5. **Train Team**: Share quickstart guide with team for consistent ACI usage

---

## References

- **Feature Spec**: `specs/003-optimize-aci-anthropic/spec.md`
- **Implementation Plan**: `specs/003-optimize-aci-anthropic/plan.md`
- **Data Model**: `specs/003-optimize-aci-anthropic/data-model.md`
- **Research Findings**: `specs/003-optimize-aci-anthropic/research.md`
- **Anthropic Guidance**: "Building Effective Agents" (Dec 19, 2024) - Appendix 2: Prompt Engineering Your Tools
- **ggen Constitution**: `.specify/memory/constitution.md` v1.0.0

---

**Validation complete when all checklists passed and evidence collected.**
