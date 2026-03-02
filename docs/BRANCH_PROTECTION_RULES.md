<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Branch Protection Rules](#branch-protection-rules)
  - [Overview](#overview)
  - [Protected Branches](#protected-branches)
    - [Master Branch (`master`)](#master-branch-master)
    - [Develop Branch (`develop`)](#develop-branch-develop)
  - [Setting Up Branch Protection via GitHub UI](#setting-up-branch-protection-via-github-ui)
    - [Master Branch Setup](#master-branch-setup)
    - [Develop Branch Setup](#develop-branch-setup)
  - [Setting Up Branch Protection via GitHub API](#setting-up-branch-protection-via-github-api)
    - [Create Protection Rule](#create-protection-rule)
    - [Check Current Protection](#check-current-protection)
    - [Update Protection Rule](#update-protection-rule)
    - [Remove Protection Rule](#remove-protection-rule)
  - [CI Status Checks Required](#ci-status-checks-required)
    - [Rust-Based Checks](#rust-based-checks)
    - [Erlang-Based Checks](#erlang-based-checks)
  - [Status Check Configuration](#status-check-configuration)
    - [Branches Requiring Status Checks](#branches-requiring-status-checks)
    - [Strict Status Checks](#strict-status-checks)
  - [Code Review Requirements](#code-review-requirements)
    - [Pull Request Review Policy](#pull-request-review-policy)
    - [CODEOWNERS File](#codeowners-file)
  - [Bypass Rules](#bypass-rules)
    - [Who Can Bypass Protection](#who-can-bypass-protection)
    - [When to Bypass](#when-to-bypass)
  - [Troubleshooting](#troubleshooting)
    - ["Required status check is expected but was not provided"](#required-status-check-is-expected-but-was-not-provided)
    - ["This branch has 1 behind" or "This branch has conflicts"](#this-branch-has-1-behind-or-this-branch-has-conflicts)
    - ["This branch can't be deleted (branch is protected)"](#this-branch-cant-be-deleted-branch-is-protected)
    - ["Merge blocked: review from a code owner is required"](#merge-blocked-review-from-a-code-owner-is-required)
  - [Best Practices](#best-practices)
    - [For Developers](#for-developers)
    - [For Code Owners](#for-code-owners)
    - [For Maintainers](#for-maintainers)
  - [Monitoring](#monitoring)
    - [View Protection Status](#view-protection-status)
    - [Audit Trail](#audit-trail)
  - [Related Documents](#related-documents)
  - [Quick Reference](#quick-reference)
    - [Master Branch Protection Checklist](#master-branch-protection-checklist)
    - [Required Status Checks](#required-status-checks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Branch Protection Rules

This document describes the branch protection rules configured for the ggen repository to ensure code quality and prevent accidental deployments.

## Overview

Branch protection rules enforce:
- CI/CD pipeline must pass before merge
- Minimum code review approvals
- Stale review dismissal on new commits
- Dismissal of pull request reviews
- Require status checks to pass
- Require branches to be up to date before merging

## Protected Branches

### Master Branch (`master`)

**Purpose**: Production-ready code. All releases are tagged from master.

**Protection Rules**:

1. **Require CI to pass**
   - All GitHub Actions workflows must succeed
   - Required checks:
     - `file-organization` - Verify file placement
     - `comprehensive-test` - Full test suite
     - `build-matrix` - Build across platforms
     - `fmt` - Code formatting
     - `clippy` - Linting
     - `coverage` - Code coverage
     - `erlang-build` - Erlang compilation
     - `erlang-tests` - Erlang test suite
     - `erlang-lint` - Erlang code quality

2. **Require pull request reviews**
   - Minimum 1 approval required
   - Dismiss stale pull request approvals on new commits
   - Require review from code owners

3. **Require branches to be up to date**
   - Must rebase/merge with base branch before merge
   - Prevents merge conflicts

4. **Require signed commits**
   - (Optional) All commits must be signed with GPG key

### Develop Branch (`develop`)

**Purpose**: Integration branch for next release. Allows more flexibility than master.

**Protection Rules**:

1. **Require CI to pass**
   - Same checks as master
   - Allows tests to be more experimental

2. **Require pull request reviews**
   - Minimum 1 approval
   - Stale reviews dismissed on new commits

3. **Allow force pushes**
   - Permits force pushes to develop (not recommended)

## Setting Up Branch Protection via GitHub UI

### Master Branch Setup

1. Go to **Settings → Branches**
2. Click **Add rule** under Branch protection rules
3. Enter `master` as the branch name pattern
4. Enable these options:
   - [x] Require a pull request before merging
   - [x] Require approvals (1)
   - [x] Dismiss stale pull request approvals when new commits are pushed
   - [x] Require status checks to pass before merging
   - [x] Require branches to be up to date before merging
   - [x] Include administrators

5. Under "Status checks that are required to pass":
   ```
   file-organization
   comprehensive-test
   build-matrix
   fmt
   clippy
   coverage
   erlang-build
   erlang-tests
   erlang-lint
   erlang-ci-status
   ```

6. Click **Create** or **Update**

### Develop Branch Setup

1. Follow same steps as master
2. Enable fewer restrictions for experimental work
3. Keep CI requirements enforced

## Setting Up Branch Protection via GitHub API

### Create Protection Rule

```bash
# Protect master branch
gh api repos/seanchatmangpt/ggen/branches/master/protection \
  --input - << 'EOF'
{
  "required_status_checks": {
    "strict": true,
    "contexts": [
      "file-organization",
      "comprehensive-test",
      "build-matrix",
      "fmt",
      "clippy",
      "coverage",
      "erlang-build",
      "erlang-tests",
      "erlang-lint"
    ]
  },
  "required_pull_request_reviews": {
    "dismiss_stale_reviews": true,
    "require_code_owner_reviews": true,
    "required_approving_review_count": 1
  },
  "enforce_admins": true,
  "allow_force_pushes": false,
  "allow_deletions": false
}
EOF
```

### Check Current Protection

```bash
gh api repos/seanchatmangpt/ggen/branches/master/protection
```

### Update Protection Rule

```bash
gh api repos/seanchatmangpt/ggen/branches/master/protection \
  -f required_approving_review_count=2
```

### Remove Protection Rule

```bash
gh api repos/seanchatmangpt/ggen/branches/master/protection \
  --input /dev/null \
  -X DELETE
```

## CI Status Checks Required

### Rust-Based Checks

| Check | Purpose | Timeout | File Pattern |
|-------|---------|---------|--------------|
| `file-organization` | Enforce file placement rules | 5m | All |
| `comprehensive-test` | Unit + integration tests | 30m | `crates/**` |
| `build-matrix` | Cross-platform builds | 30m | `crates/**`, `Cargo.*` |
| `fmt` | Rustfmt formatting | 5m | `**/*.rs` |
| `clippy` | Linting (warnings-as-errors) | 5m | `**/*.rs` |
| `coverage` | Code coverage (80%+ required) | 20m | `tests/**` |

### Erlang-Based Checks

| Check | Purpose | Timeout | File Pattern |
|-------|---------|---------|--------------|
| `erlang-build` | Compile (OTP 24-27) | 30m | `crates/tps-jidoka/**`, `crates/tai-erlang-autonomics/**` |
| `erlang-tests` | EUnit + Common Tests | 45m | `tests/**` |
| `erlang-lint` | Format + Dialyzer | 20m | `**/*.erl` |
| `erlang-ci-status` | Overall Erlang CI | - | - |

## Status Check Configuration

### Branches Requiring Status Checks

```yaml
Required Checks:
  - file-organization              # ALWAYS
  - comprehensive-test             # ALWAYS
  - build-matrix (ubuntu-latest)   # ALWAYS
  - fmt                            # ALWAYS
  - clippy                         # ALWAYS
  - erlang-build (OTP 26)          # IF erlang code changed
  - erlang-tests (OTP 26)          # IF erlang code changed
```

### Strict Status Checks

When enabled, branch must be:
1. Up to date with base branch (no conflicts)
2. All required status checks passing
3. At least X approvals (default: 1)

## Code Review Requirements

### Pull Request Review Policy

1. **Minimum Approvals**: 1
2. **Dismiss Stale Reviews**: YES (dismissed when new commits pushed)
3. **Code Owner Review**: YES (if CODEOWNERS file exists)
4. **Require Review When Last Commit Pushed by Submitter**: NO

### CODEOWNERS File

Create `.github/CODEOWNERS` to define reviewers by file:

```
# Rust code
crates/ggen-* @seanchatmangpt @reviewer1

# Erlang code
crates/tps-jidoka/ @seanchatmangpt @erlang-reviewer
crates/tai-erlang-* @seanchatmangpt @erlang-reviewer

# Documentation
docs/ @seanchatmangpt

# GitHub Actions
.github/workflows/ @seanchatmangpt

# Default owners
* @seanchatmangpt
```

## Bypass Rules

### Who Can Bypass Protection

- Repository administrators (with enforcement)
- GitHub Apps with `administration` permission

### When to Bypass

**Only for critical hotfixes:**

1. Severe production bug affecting customers
2. Security vulnerability requiring immediate patch
3. Infrastructure emergency requiring quick deployment

**Bypass process:**

1. Create emergency branch from master
2. Apply minimal fix
3. Bypass protection with documented reason
4. Create follow-up PR with full review

```bash
# Bypass protection (admin only)
gh api repos/seanchatmangpt/ggen/branches/master/protection \
  -f enforce_admins=false
```

## Troubleshooting

### "Required status check is expected but was not provided"

**Problem**: Status check hasn't run yet or doesn't exist

**Solution**:
1. Wait for GitHub Actions to complete
2. Verify workflow file exists in `.github/workflows/`
3. Check workflow triggers (push, pull_request)
4. Verify status check name matches exactly

### "This branch has 1 behind" or "This branch has conflicts"

**Problem**: Branch not up to date with base branch

**Solution**:
1. Click "Update branch" button on PR
2. Or locally: `git pull origin master && git push`

### "This branch can't be deleted (branch is protected)"

**Problem**: Attempting to delete protected branch

**Solution**:
1. Only admins can delete protected branches
2. Remove protection rule first if necessary
3. Or rename branch instead

### "Merge blocked: review from a code owner is required"

**Problem**: Code owner review required but not provided

**Solution**:
1. Identify code owner from `.github/CODEOWNERS`
2. Request review from that person
3. Wait for approval

## Best Practices

### For Developers

- Always create feature branches from master
- Keep PRs focused (one feature per PR)
- Add descriptive PR titles and descriptions
- Request reviews early
- Respond promptly to review feedback
- Rebase before merge to keep history clean

### For Code Owners

- Review PRs within 24 hours
- Request changes only for critical issues
- Approve with confidence when ready
- Ask questions before blocking
- Leave constructive feedback

### For Maintainers

- Monitor protection rule compliance
- Update status check lists when adding new CI jobs
- Review protection rules quarterly
- Document bypass decisions
- Track protection metric dashboard

## Monitoring

### View Protection Status

```bash
# Check master branch protection
gh api repos/seanchatmangpt/ggen/branches/master/protection

# Check all protected branches
gh api repos/seanchatmangpt/ggen/branches --jq '.[] | select(.protected == true) | .name'
```

### Audit Trail

GitHub provides audit log of:
- Branch protection rule changes
- Bypass events
- Status check results
- Review approvals/dismissals

Access via: **Settings → Security → Audit log**

## Related Documents

- [ERLANG_RELEASE_PROCESS.md](./ERLANG_RELEASE_PROCESS.md) - Release procedures
- [CI_CD_PIPELINE.md](../,.github/CI_CD_PIPELINE.md) - CI/CD setup details
- [CONTRIBUTING.md](../.github/CONTRIBUTING.md) - Contribution guidelines

## Quick Reference

### Master Branch Protection Checklist

- [x] Require PR before merging
- [x] Require 1 approval
- [x] Dismiss stale reviews
- [x] Require status checks to pass
- [x] Require up-to-date branch
- [x] Enforce admins

### Required Status Checks

- ✅ file-organization
- ✅ comprehensive-test
- ✅ build-matrix
- ✅ fmt
- ✅ clippy
- ✅ coverage
- ✅ erlang-build (on erlang changes)
- ✅ erlang-tests (on erlang changes)
- ✅ erlang-lint (on erlang changes)

---

**Last Updated**: 2026-01-26
**Maintainer**: Sean Chat Management Team
