---
description: Fill gaps using 80/20 approach, validate Definition of Done (DfLSS quality gates), automatically merge to master, and close branch with evidence collection for Rust/cargo make workflows.
handoffs:
  - label: Implement Missing Items
    agent: speckit.implement
    prompt: Continue implementation to complete remaining checklist items
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## ⚡ RDF-First Architecture

**Validate TTL sources**: feature.ttl, plan.ttl, tasks.ttl (SHACL validation)
**Evidence collection**: Save test results, traces, benchmark data to `FEATURE_DIR/evidence/`

All quality gates validate against TTL specifications. Evidence artifacts prove compliance with RDF-defined requirements.

## Outline

This command enforces the Definition of Done (§5) and automates the Branch Lifecycle completion (§6) before creating a pull request. Customized for **ggen's Rust/cargo make/Chicago TDD workflow with RDF-first specifications**.

### 1. Prerequisites Check

Run `.specify/scripts/bash/check-prerequisites.sh --json --require-tasks --include-tasks` from repo root and parse JSON for:
- `FEATURE_DIR`
- `FEATURE_SPEC`
- `IMPL_PLAN`
- `TASKS`
- `BRANCH`
- `AVAILABLE_DOCS`

For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").

### 2. Fill Gaps Using 80/20 Approach

Before validating Definition of Done, identify and fill critical gaps using Design for Lean Six Sigma (DfLSS) 80/20 principle.

#### 2.1 Gap Analysis

Scan for incomplete work:

**Checklist Gaps**:
```bash
# Find incomplete checklist items
grep -r "^\- \[ \]" "$FEATURE_DIR/checklists/" 2>/dev/null
```

**Task Gaps**:
```bash
# Find incomplete tasks
grep "^\- \[ \] T[0-9]" "$TASKS"
```

**Missing Evidence**:
```bash
# Check evidence directory
ls -la "$FEATURE_DIR/evidence/" 2>/dev/null || echo "No evidence directory"
```

#### 2.2 80/20 Gap Prioritization

Apply Pareto Principle to identify the **vital few** gaps:

**Priority 1: RED Andon Signals** (20% of gaps, 80% of value)
- Compilation errors blocking merge
- Test failures preventing CI pass
- Missing critical evidence for success criteria
- Incomplete user story implementations

**Priority 2: YELLOW Andon Signals** (next 20%)
- Clippy warnings not addressed
- Format issues
- Minor test coverage gaps
- Documentation incomplete

**Priority 3: Nice-to-have** (remaining 60%, defer)
- Additional test cases
- Extra documentation
- Performance optimizations
- Code cleanup

#### 2.3 Automated Gap Filling

For each Priority 1 gap, apply automated fixes:

**If compilation errors exist**:
```bash
# Try to fix compilation
cargo make check 2>&1 | tee /tmp/check-errors.txt

# If errors found, analyze and fix
if [ $? -ne 0 ]; then
  echo "🔴 RED: Compilation errors detected"
  # Apply common fixes (missing imports, type mismatches, etc.)
  # Use cargo fix if available
  cargo fix --allow-dirty --allow-staged 2>/dev/null || true
  cargo make check
fi
```

**If test failures exist**:
```bash
# Run tests and capture failures
cargo make test 2>&1 | tee /tmp/test-failures.txt

# If failures found
if [ $? -ne 0 ]; then
  echo "🔴 RED: Test failures detected"
  # Analyze failure patterns
  # Apply fixes based on Chicago TDD principles
  # Re-run tests to verify
fi
```

**If evidence missing**:
```bash
# Create evidence directory if missing
mkdir -p "$FEATURE_DIR/evidence/"

# Capture critical evidence
cargo make test > "$FEATURE_DIR/evidence/test-results.txt" 2>&1
cargo make lint > "$FEATURE_DIR/evidence/lint-output.txt" 2>&1
cargo make check 2>&1 | grep "Finished" > "$FEATURE_DIR/evidence/compile-time.txt"

# Optional: Capture SLO metrics
cargo make slo-check > "$FEATURE_DIR/evidence/slo-metrics.txt" 2>&1 || true
```

**If checklists incomplete**:
- Auto-mark items as `[X]` if corresponding work is complete
- Example: If tests exist for feature X, mark "Write tests for X" as complete
- Verify observable state changes (Chicago TDD principle)

**If tasks incomplete**:
- Auto-mark tasks as `[X]` if deliverables exist
- Example: If file `src/foo.rs` exists and tests pass, mark "T042 Implement foo.rs" complete
- Cross-reference with git log to verify work was done

#### 2.4 Gap-Fill Report

Generate report of auto-filled gaps:

```text
✅ 80/20 Gap-Fill Complete

Priority 1 (RED signals) - FIXED:
- [AUTO] Fixed 2 compilation errors (cargo fix)
- [AUTO] Captured evidence: test-results.txt, lint-output.txt, slo-metrics.txt
- [MANUAL] 3 test failures remain (require investigation)

Priority 2 (YELLOW signals) - DEFERRED:
- 5 clippy warnings (non-blocking)
- Format check will be run in validation

Priority 3 (Nice-to-have) - SKIPPED:
- Additional test coverage (current: 85%, target: 80%)
- Performance benchmarks (optional for this feature)

🔴 REMAINING RED SIGNALS:
- 3 test failures in ggen-domain (test_foo, test_bar, test_baz)

Action: Fix remaining RED signals manually or STOP here
```

**If ANY RED signals remain after gap-fill**: STOP and report.

User must decide:
- **Option 1**: Fix RED signals manually, then re-run `/speckit.finish`
- **Option 2**: Accept partial completion, create PR with known issues (NOT RECOMMENDED)

### 3. Definition of Done Validation

Execute validation checks in sequence. **STOP on first failure** and report what needs fixing.

#### 3.1 Checklist Validation

Scan all checklist files in `FEATURE_DIR/checklists/`:
- Count total items: `- [ ]` or `- [X]` or `- [x]`
- Count completed: `- [X]` or `- [x]`
- Count incomplete: `- [ ]`

**Requirements**:
- **ALL** checklist items MUST be complete (`- [X]`)
- Generate status table:

```text
| Checklist | Total | Completed | Incomplete | Status |
|-----------|-------|-----------|------------|--------|
| requirements.md | 99 | 99 | 0 | ✓ PASS |
```

**If ANY incomplete items**: STOP and report:
```text
❌ Definition of Done FAILED: Checklists incomplete

Incomplete items:
- requirements.md: 15 items pending (CHK011, CHK012, ...)
- security.md: 3 items pending (SEC001, SEC002, SEC003)

Action: Run /speckit.implement to complete remaining work
```

#### 3.2 Task Completion Validation

Parse `TASKS` file:
- Extract all task lines matching `- [ ] [TID]` or `- [X] [TID]`
- Count completed vs incomplete

**Requirements**:
- **ALL** tasks MUST be marked `[X]`

**If ANY incomplete tasks**: STOP and report:
```text
❌ Definition of Done FAILED: Tasks incomplete

Incomplete tasks:
- [ ] T042 [US3] Implement database integration
- [ ] T043 [US3] Add error handling tests
... (show max 10, then "and N more")

Action: Complete tasks or run /speckit.implement
```

#### 3.3 Code Quality Validation (Rust/cargo make)

Run quality gates in sequence (follow Andon Signal Protocol):

**🟢 Check: Fast compilation check**:
```bash
timeout 15s cargo make check
```

**Expected**: Exit code 0, output contains "Finished" or GREEN signal

**🟢 Test: Run all tests with coverage**:
```bash
timeout 300s cargo make test
```

**Expected**: Exit code 0, all tests passing, no RED signals

**🟢 Lint: Clippy lint check**:
```bash
timeout 30s cargo make lint
```

**Expected**: Exit code 0, no clippy warnings (RUSTFLAGS=-D warnings enforced)

**🟢 Format: Verify formatting**:
```bash
timeout 10s cargo make fmt-check
```

**Expected**: Exit code 0, code formatted correctly

**Requirements**:
- Exit code 0 for ALL commands
- No RED Andon signals (compilation errors, test failures)
- No YELLOW signals blocking release (clippy warnings, format issues)

**If ANY failures**: STOP and report:
```text
❌ Definition of Done FAILED: Quality gates failed

🔴 RED ANDON SIGNALS:
- Check: error[E0425]: cannot find value `foo` in this scope
- Tests: 3 tests failing (test_foo, test_bar, test_baz)

🟡 YELLOW ANDON SIGNALS:
- Lint: warning: unused variable `x` (#[warn(unused_variables)])
- Format: Files require formatting (run cargo make fmt)

Action: Fix all RED signals before proceeding. YELLOW signals must be addressed for release.
```

Include full error output for failed commands.

#### 3.4 Evidence Validation

Check `FEATURE_DIR/evidence/` directory:
- Directory exists
- Contains at least 1 file
- Total size > 0 bytes
- Evidence files documented in implementation-progress-summary.md or similar

**Recommended evidence files**:
- `baseline-*.txt` - Baseline measurements
- `post-opt-*.txt` - Post-optimization measurements
- `*-accuracy-summary.md` - Validation reports
- `*-verification.md` - Success criteria verification
- `test-output.txt` - Test suite results
- `lint-clean.txt` - Clippy output
- `slo-metrics.txt` - SLO compliance data

**If missing or empty**: WARN (non-blocking):
```text
⚠️ WARNING: No evidence captured

Evidence directory is empty or missing. Consider capturing:
- Test output (cargo make test > evidence/test-results.txt)
- Coverage report (cargo tarpaulin --out Lcov && genhtml ...)
- Lint output (cargo make lint > evidence/lint-clean.txt)
- Build logs (cargo make ci > evidence/ci-output.txt)
- SLO metrics (cargo make slo-check > evidence/slo-metrics.txt)
- Performance benchmarks (cargo make bench)

Continue anyway? (yes/no)
```

Wait for user response. If "no", STOP.

### 4. Pre-Merge Preparation

All validations passed. Prepare for merge:

#### 4.1 Update Spec Status

Edit `FEATURE_SPEC`:
- Find line matching `**Status**: Draft` or `**Status**: In Progress`
- Replace with `**Status**: Complete`
- Add completion timestamp: `**Completed**: YYYY-MM-DD`

Example:
```markdown
**Branch**: `003-optimize-aci-anthropic` | **Created**: 2025-12-11 | **Status**: Complete | **Completed**: 2025-12-11
```

#### 4.2 Merge Master into Feature Branch

Follow §6 Branch Lifecycle "Finish (before PR)":

```bash
git fetch origin master
git merge origin/master
```

**Note**: ggen uses `master` as main branch (not `main`).

**If merge conflicts**: STOP and report:
```text
❌ Merge conflicts detected

Conflicts in:
- Cargo.toml
- crates/ggen-core/src/lib.rs

Action: Resolve conflicts manually using Chicago TDD approach:
  1. Resolve conflicts
  2. cargo make check (verify compilation)
  3. cargo make test (verify tests pass)
  4. git add . && git commit
  5. Re-run /speckit.finish
```

**If merge succeeds**: Continue.

#### 4.3 Verify Quality Post-Merge

Re-run quality gates to ensure merge didn't break anything:

```bash
cargo make check && cargo make lint && cargo make test
```

**If ANY failures**: STOP and report:
```text
❌ Post-merge validation FAILED

🔴 RED ANDON SIGNAL: The merge from master introduced failures:
- Tests: 2 new failures (test_integration, test_domain)

Action: Fix merge-related issues following Andon Signal Protocol:
  1. STOP the line (don't proceed)
  2. Investigate root cause
  3. Fix issues
  4. Verify fix (cargo make test)
  5. Re-run /speckit.finish
```

### 5. Generate PR Description

Create comprehensive PR description from artifacts:

#### 5.1 Load PR Context

Read the following files:
- `FEATURE_SPEC` - For executive summary, user stories, success criteria
- `TASKS` - For implementation summary
- `IMPL_PLAN` (if exists) - For technical context
- `FEATURE_DIR/evidence/` - List evidence files
- `VALIDATION_REPORT.md` (if exists) - Final validation results

#### 5.2 Generate PR Template

Use this structure:

```markdown
# [Feature Name from spec.md]

## Summary

[Executive summary from spec.md - 2-3 sentences]

## User Stories Implemented

[List each user story with checkbox]
- ✅ **US1**: [Brief description]
- ✅ **US2**: [Brief description]
...

## Technical Approach

[Brief summary from plan.md if exists, otherwise from spec requirements]

**Architecture Decisions**:
- Type-first design with zero-cost abstractions
- Chicago TDD with real collaborators
- Result<T,E> error handling (no unwrap/expect in production)
- cargo make for build automation with timeout enforcement

## Changes

### Crates Modified
[List crates with file counts]
- `ggen-core`: 5 files
- `ggen-domain`: 3 files
- `ggen-cli`: 8 files

### New Files Created
[Count from tasks.md]
- Total: 42 files
- Tests: 15 files
- Source: 27 files

## Quality Verification

- ✅ All checklist items complete ([N] total)
- ✅ All tasks complete ([M] total)
- ✅ Tests passing (cargo make test) - [X] tests, 0 failures
- ✅ Lint clean (cargo make lint) - 0 clippy warnings
- ✅ Format check (cargo make fmt-check) - formatted correctly
- ✅ Compilation (cargo make check) - clean build
- ✅ Evidence captured ([K] files)

## Andon Signal Status

**All signals GREEN** 🟢
- No RED signals (compilation errors, test failures)
- No YELLOW signals (clippy warnings, format issues)
- SLO compliance verified (if applicable)

## Evidence

[Link to evidence files in feature directory]
- [Test results](specs/NNN-feature/evidence/test-results.txt)
- [Coverage report](specs/NNN-feature/evidence/coverage-summary.txt)
- [Lint output](specs/NNN-feature/evidence/lint-clean.txt)
- [SLO metrics](specs/NNN-feature/evidence/slo-metrics.txt)
- [Validation report](specs/NNN-feature/VALIDATION_REPORT.md)

## Success Criteria Met

[List success criteria from spec.md with checkboxes - all checked]
- ✅ SC-001: [Criterion description] ([Target vs Measured])
- ✅ SC-002: [Criterion description] ([Target vs Measured])
...

## Constitutional Alignment

[Reference to constitutional principles followed]
- ✅ Section II: Type-First Thinking (Rust idioms, zero-cost abstractions)
- ✅ Section III: Error Handling Standards (Result<T,E>, no production panics)
- ✅ Section IV: Testing Standards (Chicago TDD, observable outputs)
- ✅ Section VI: Andon Signal Protocol (RED/YELLOW/GREEN monitoring)
- ✅ Section IX: cargo make Protocol (mandatory timeout enforcement)

## Related

- Spec: `specs/NNN-feature/spec.md`
- Tasks: `specs/NNN-feature/tasks.md`
- Plan: `specs/NNN-feature/plan.md`
- Constitution: `.specify/memory/constitution.md`

---

🤖 Generated with [Claude Code](https://claude.com/claude-code) via `/speckit.finish`
🦀 Built with Rust + cargo make + Chicago TDD
```

Save to temporary file: `/tmp/pr-description-$BRANCH.md`

### 6. Create and Automatically Merge Pull Request

Use GitHub CLI to create PR and automatically merge:

#### 6.1 Create Pull Request

```bash
gh pr create \
  --title "[Feature Name from spec.md]" \
  --body "$(cat /tmp/pr-description-$BRANCH.md)" \
  --base master \
  --head "$BRANCH"
```

**Note**: ggen uses `master` as base branch (not `main`).

**If PR creation fails**: Report error and provide manual instructions:
```text
❌ PR creation failed

Error: [gh CLI error message]

Manual steps:
1. Go to: https://github.com/seanchatmangpt/ggen/compare/master...$BRANCH
2. Click "Create Pull Request"
3. Use generated description from: /tmp/pr-description-$BRANCH.md
```

**If PR creation succeeds**: Capture PR number and continue.

```bash
# Capture PR number from URL
PR_NUMBER=$(gh pr list --head "$BRANCH" --json number --jq '.[0].number')
echo "✅ PR #$PR_NUMBER created"
```

#### 6.2 Wait for CI Pipeline

Monitor CI pipeline status before merging:

```bash
# Wait for all checks to complete
echo "⏳ Waiting for CI pipeline to complete..."
gh pr checks "$PR_NUMBER" --watch --interval 10

# Verify all checks passed
gh pr checks "$PR_NUMBER" || {
  echo "❌ CI checks failed. Cannot auto-merge."
  exit 1
}
```

**If CI fails**: STOP and report failures. User must fix and re-run.

#### 6.3 Automatically Merge to Master

All checks passed. Merge PR automatically:

```bash
# Merge using squash strategy (cleaner history)
gh pr merge "$PR_NUMBER" \
  --squash \
  --delete-branch \
  --body "Automated merge via /speckit.finish 🤖"

echo "✅ PR #$PR_NUMBER merged to master"
echo "✅ Remote branch $BRANCH deleted automatically"
```

**Merge Options**:
- `--squash`: Single commit (default, cleaner history)
- `--merge`: Preserve all commits
- `--rebase`: Linear history
- `--delete-branch`: Auto-delete remote branch after merge

**If merge fails**:
```text
❌ Auto-merge FAILED

Possible reasons:
- Branch protection requires manual reviews
- Merge conflicts detected
- Insufficient permissions

Manual fallback:
1. Visit PR: https://github.com/seanchatmangpt/ggen/pull/$PR_NUMBER
2. Get required approvals
3. Merge manually
4. Then run cleanup: git checkout master && git pull && git branch -d $BRANCH
```

STOP here if merge failed.

### 7. Cleanup Branch Locally

PR successfully merged. Clean up local environment:

#### 7.1 Switch to Master and Update

```bash
# Switch back to master
git checkout master

# Pull latest changes (includes merged PR)
git pull origin master

echo "✅ Switched to master and pulled latest changes"
```

#### 7.2 Delete Local Feature Branch

```bash
# Delete local branch (safe, already merged to remote)
git branch -d "$BRANCH" 2>/dev/null || {
  # If -d fails (sees unmerged commits due to squash), force delete
  echo "⚠️ Squash merge detected, force-deleting local branch..."
  git branch -D "$BRANCH"
}

echo "✅ Local branch $BRANCH deleted"
```

**Why force delete may be needed**: Squash merges create a new commit, so git sees the original commits as "unmerged". This is safe because the PR was already merged remotely.

#### 7.3 Verify Cleanup Complete

```bash
# Verify branch is fully removed
git branch -a | grep "$BRANCH" && {
  echo "⚠️ Branch still exists in 'git branch -a' output"
  echo "This is normal if remote tracking refs are cached."
  echo "Run: git fetch --prune"
} || echo "✅ Branch fully cleaned up"

# Show current master HEAD
echo ""
echo "Current master HEAD:"
git log -1 --oneline
```

### 8. Final Report

Output summary:

```text
✅ /speckit.finish COMPLETE - FEATURE MERGED TO MASTER

Branch: $BRANCH → MERGED & CLOSED
PR: #$PR_NUMBER (https://github.com/seanchatmangpt/ggen/pull/$PR_NUMBER)
Status: ✅ Merged to master and branch deleted

80/20 Gap-Fill Results:
- ✅ Priority 1 (RED signals): All fixed
- ⚠️ Priority 2 (YELLOW signals): Deferred (non-blocking)
- ⏭️ Priority 3 (Nice-to-have): Skipped (focus on vital few)

Validation Results:
- ✅ Checklists: [N] items complete
- ✅ Tasks: [M] items complete
- ✅ Tests: PASS ([X] tests, 0 failures)
- ✅ Lint: PASS (0 clippy warnings)
- ✅ Format: PASS (code formatted)
- ✅ Check: PASS (clean compilation)
- ✅ Evidence: [K] files captured

CI Pipeline: ✅ ALL CHECKS PASSED

Merge Details:
- Strategy: Squash (single commit)
- Remote branch: Deleted automatically
- Local branch: Deleted automatically
- Master updated: Yes (git pull completed)

Spec updated: specs/NNN-feature/spec.md (Status: Complete)
Andon Signals: 🟢 ALL GREEN

Current Branch: master
Latest Commit: [commit hash from git log]

🎉 Feature complete! No further action required.
```

## Error Handling

- **Gap-fill failures**: If Priority 1 (RED) gaps remain after auto-fill, STOP and require manual fixes
- **Validation failures**: STOP immediately, report specific failures, suggest fixes
- **Merge conflicts**: STOP, provide conflict resolution guidance using Chicago TDD
- **PR creation failures**: Provide manual fallback instructions
- **CI pipeline failures**: STOP before merge, report failures, require fixes
- **Auto-merge failures**: Provide manual merge instructions with PR link
- **Branch cleanup failures**: Warn but continue (non-critical)
- **Network errors**: Retry once, then provide manual instructions
- **Andon signals**: Treat RED as fatal, YELLOW as warnings (must address for release)

## Operating Rules

1. **80/20 Gap-Fill FIRST**: Identify and fix Priority 1 (RED) gaps before validation (DfLSS)
2. **STOP on RED signals**: Do not proceed past Priority 1 failures (Andon Protocol)
3. **Defer YELLOW signals**: Priority 2 gaps are non-blocking but should be documented
4. **Skip nice-to-have**: Priority 3 gaps deferred to future work (focus on vital few)
5. **Evidence is CRITICAL**: Warn loudly if missing (but allow override for non-production features)
6. **Atomic operations**: Each cargo make command must succeed before continuing
7. **Deterministic output**: Same state = same validation results (Principle VII)
8. **Automated merge**: Merge PR automatically after CI passes (no manual intervention)
9. **Automated cleanup**: Delete branches automatically after successful merge
10. **User confirmation**: Ask before overriding warnings (empty evidence, YELLOW signals, etc.)
11. **Constitution enforcement**: This command enforces §5 (Definition of Done) and §6 (Branch Lifecycle) non-negotiably
12. **Rust idioms**: Verify Result<T,E> usage, no unwrap/expect in production code (§VII)
13. **Chicago TDD**: Validate tests verify observable state, not implementation (§IV)

## ggen-Specific Adaptations

### Differences from Astro TypeScript/pnpm version:

1. **80/20 Gap-Fill**: Automatically fills Priority 1 (RED) gaps before validation (NEW)
2. **Automated Merge**: Merges PR automatically after CI passes (NEW)
3. **Automated Cleanup**: Deletes branches automatically after merge (NEW)
4. **Build system**: cargo make (not pnpm) with mandatory timeouts
5. **Quality gates**: check + lint + fmt-check + test (not lint + test + build)
6. **Main branch**: master (not main)
7. **Error handling**: Verify Result<T,E> patterns, flag unwrap/expect in production
8. **Test philosophy**: Chicago TDD (state-based, real collaborators) not London School
9. **Andon signals**: RED/YELLOW/GREEN monitoring integrated into quality gates
10. **SLO compliance**: Optional check for performance-critical features
11. **Constitution**: Reference ggen constitutional principles in PR description

### Rust-Specific Quality Checks:

```bash
# Compilation check (fast feedback, <5s target)
timeout 15s cargo make check

# Clippy lint (catches common mistakes, warnings-as-errors)
timeout 30s cargo make lint

# Format verification (rustfmt with ggen style)
timeout 10s cargo make fmt-check

# Test suite (Chicago TDD, all tests must pass)
timeout 300s cargo make test

# Optional: SLO compliance check
timeout 30s cargo make slo-check
```

## Notes

- This command assumes master branch is `master` (ggen convention)
- PR template can be customized via `.specify/templates/pr-template.md` (if exists)
- Evidence files should be committed before running this command
- **Fully automated**: Creates PR, waits for CI, merges, deletes branches automatically
- **80/20 Pareto Principle**: Focuses on fixing the vital few (Priority 1) gaps only
- Post-merge cleanup is **automated** (no manual steps required)
- Andon Signal Protocol (§VI) is enforced throughout validation
- cargo make targets must have timeout enforcement (§IX)
- Constitution alignment verification is mandatory for production features
- **Single command**: From incomplete feature → merged to master + branch closed
