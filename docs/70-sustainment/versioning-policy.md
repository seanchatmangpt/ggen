<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Versioning & Release Policy](#versioning--release-policy)
  - [Table of Contents](#table-of-contents)
  - [Executive Summary](#executive-summary)
  - [Semantic Versioning (SemVer)](#semantic-versioning-semver)
    - [Version Format: MAJOR.MINOR.PATCH](#version-format-majorminorpatch)
    - [Version Components](#version-components)
      - [MAJOR Version](#major-version)
      - [MINOR Version](#minor-version)
      - [PATCH Version](#patch-version)
    - [Version Examples](#version-examples)
  - [Release Cycle](#release-cycle)
    - [Release Schedule](#release-schedule)
    - [Monthly Patch Release (v6.0.0 → v6.0.1 → v6.0.2 ...)](#monthly-patch-release-v600-%E2%86%92-v601-%E2%86%92-v602-)
    - [Quarterly Feature Release (v6.0 → v6.1 → v6.2 → v6.3)](#quarterly-feature-release-v60-%E2%86%92-v61-%E2%86%92-v62-%E2%86%92-v63)
    - [Annual Major Release (v5 → v6 → v7)](#annual-major-release-v5-%E2%86%92-v6-%E2%86%92-v7)
  - [Branch Strategy](#branch-strategy)
    - [Git Branch Model](#git-branch-model)
    - [Branch Naming Conventions](#branch-naming-conventions)
    - [Main Branch Rules](#main-branch-rules)
    - [Release Branch Rules](#release-branch-rules)
    - [Develop Branch Rules](#develop-branch-rules)
  - [Git Tagging](#git-tagging)
    - [Tag Format](#tag-format)
    - [Tagging Rules](#tagging-rules)
    - [Tag Automation](#tag-automation)
  - [Deprecation Policy](#deprecation-policy)
    - [Deprecation Timeline](#deprecation-timeline)
    - [Deprecation Process](#deprecation-process)
    - [Deprecation Communication](#deprecation-communication)
  - [Support Policy](#support-policy)
    - [Active Support Timeline](#active-support-timeline)
    - [Support Tiers](#support-tiers)
    - [Support Definition](#support-definition)
  - [Security Updates](#security-updates)
    - [CVE Response Process](#cve-response-process)
    - [Security Patch Priority](#security-patch-priority)
    - [Security Update Coordination](#security-update-coordination)
  - [Release Notes & CHANGELOG](#release-notes--changelog)
    - [CHANGELOG.md Format](#changelogmd-format)
      - [TOML Format Change](#toml-format-change)
  - [&#91;5.3.0&#93; - 2025-10-15](#530---2025-10-15)
    - [Added](#added)
    - [🗑️ Deprecated & Removed](#-deprecated--removed)
    - [📚 Documentation](#-documentation)
    - [🙏 Thanks](#-thanks)
  - [Upgrading to v6.0.0](#upgrading-to-v600)
    - [From v5.x](#from-v5x)
    - [Support](#support)
  - [Version Bumping Checklist](#version-bumping-checklist)
  - [Definition of Done](#definition-of-done)
  - [Receipt Contract](#receipt-contract)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Versioning & Release Policy

**Classification**: Internal | **Version**: 1.0.0 | **For**: ggen v6.0.0 and Future Releases

**Authority**: Chief Technology Officer | **Last Updated**: January 2026 | **Next Review**: April 2026

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Semantic Versioning (SemVer)](#semantic-versioning-semver)
3. [Release Cycle](#release-cycle)
4. [Branch Strategy](#branch-strategy)
5. [Git Tagging](#git-tagging)
6. [Deprecation Policy](#deprecation-policy)
7. [Support Policy](#support-policy)
8. [Security Updates](#security-updates)
9. [Release Notes & CHANGELOG](#release-notes--changelog)
10. [Version Compatibility Matrix](#version-compatibility-matrix)
11. [Release Process](#release-process)
12. [Version Bumping Checklist](#version-bumping-checklist)
13. [Definition of Done](#definition-of-done)

---

## Executive Summary

**Versioning & Release Policy** defines how ggen versions are numbered, released, supported, and deprecated. This policy enables:

- **Predictability**: Users know what to expect from version numbers
- **Compatibility**: Clear guidance on upgrade paths, breaking changes, deprecations
- **Support**: Clear timeline for when versions are actively supported
- **Quality**: Rigorous testing before releases, security patches prioritized
- **Communication**: Detailed release notes explaining what changed and why

**Core Principles**:
- All versions follow Semantic Versioning (MAJOR.MINOR.PATCH)
- Releases follow predictable schedule (monthly patches, quarterly features, annual majors)
- Breaking changes clearly marked and deprecated for 2 major versions
- Active support for 3 major versions (current + 2 prior)
- Security patches issued within 7 days of CVE discovery
- All releases documented with detailed CHANGELOG

**Current Version**: 6.0.0 (Released: January 18, 2026)

---

## Semantic Versioning (SemVer)

### Version Format: MAJOR.MINOR.PATCH

```
v6.0.0
│ │ │
│ │ └─ PATCH: Bug fixes, security patches (backward compatible)
│ └──── MINOR: New features (backward compatible)
└────── MAJOR: Breaking changes (incompatible)
```

### Version Components

#### MAJOR Version
- **Incremented when**: Breaking changes introduced (incompatible with previous versions)
- **Examples**:
  - Change public API signature (function parameter order)
  - Remove deprecated feature (after 2-version deprecation window)
  - Change configuration format (TOML structure incompatible)
  - Change RDF ontology in incompatible way
- **Migration**: Users must update code + configuration
- **Frequency**: Annually (v6 → v7 typically in January)

**Example**:
```
v5.0.0 → v6.0.0 (MAJOR)
- Removed deprecated ggen validate command (deprecated since v4.2)
- Changed TOML config format (requires migration script)
- Breaking API change: generate() now returns Result<T,E> (was panic)
→ Users must: Update code, run migration script, re-deploy
```

#### MINOR Version
- **Incremented when**: New features added (backward compatible, no breaking changes)
- **Examples**:
  - Add new CLI command (backward compatible)
  - Add new RDF ontology domain
  - Add new template feature
  - Add new SPARQL pattern
- **Migration**: None (drop-in upgrade)
- **Frequency**: Quarterly (v6.0 → v6.1 → v6.2 → v6.3 → v7.0)

**Example**:
```
v6.0.0 → v6.1.0 (MINOR)
- Added support for AWS Marketplace (new feature)
- Added new --profile CLI flag (new feature)
- Added SPARQL query optimizer (internal improvement)
→ Users can: Simply upgrade, no changes needed
```

#### PATCH Version
- **Incremented when**: Bug fixes, security patches (backward compatible)
- **Examples**:
  - Fix SKU replication race condition
  - Fix template rendering edge case
  - Apply security patch (dependency upgrade)
  - Fix documentation issue
- **Migration**: None (drop-in upgrade, no risk)
- **Frequency**: Monthly (v6.0.0 → v6.0.1 → v6.0.2, etc.)

**Example**:
```
v6.0.0 → v6.0.1 (PATCH)
- Fixed memory leak in catalog controller
- Fixed race condition in Firestore writes
- Upgraded cryptographic dependency (security)
→ Users should: Upgrade immediately (low risk)
```

### Version Examples

| Version | Type | Change | Reason |
|---------|------|--------|--------|
| v5.0.0 | MAJOR | Breaking API change | Removed deprecated features, rewrote core |
| v5.1.0 | MINOR | New marketplace feature | Added GCP Marketplace support |
| v5.1.1 | PATCH | Bug fix | Fixed SKU duplication issue |
| v5.2.0 | MINOR | New SPARQL patterns | Added inference engine |
| v5.2.1 | PATCH | Security patch | Updated cryptography library |
| v5.3.0 | MINOR | New CLI command | Added speckit validation |
| v6.0.0 | MAJOR | Unified pipeline (μ) | Breaking: removed old generate commands |
| v6.0.1 | PATCH | Hot-reload bug fix | Fixed policy hot-reload race condition |
| v6.1.0 | MINOR | Azure support | New cloud provider support |
| v7.0.0 | MAJOR | (future) | TBD (plan for 2027) |

---

## Release Cycle

### Release Schedule

```
┌─────────────────────────────────────────────────────────────┐
│                    ANNUAL RELEASE CALENDAR                   │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│ JANUARY      Release v6.0.0 (major annual release)          │
│              ├─ v6.0.0-rc.1 (release candidate, Dec)        │
│              └─ v6.0.0 (GA, Jan 18)                         │
│                                                               │
│ MARCH        v6.1.0 Feature Release (Q1 features)           │
│              ├─ Feature freeze: Feb 28                       │
│              ├─ v6.1.0-rc.1 (March 1)                       │
│              └─ v6.1.0 GA (March 15)                        │
│                                                               │
│ JUNE         v6.2.0 Feature Release (Q2 features)           │
│              ├─ Feature freeze: May 31                       │
│              ├─ v6.2.0-rc.1 (June 1)                        │
│              └─ v6.2.0 GA (June 15)                         │
│                                                               │
│ SEPTEMBER    v6.3.0 Feature Release (Q3 features)           │
│              ├─ Feature freeze: Aug 31                       │
│              ├─ v6.3.0-rc.1 (Sept 1)                        │
│              └─ v6.3.0 GA (Sept 15)                         │
│                                                               │
│ DECEMBER     v6.0.x patch releases (as needed)              │
│              └─ v6.x.x patches issued on-demand (weekly)    │
│                                                               │
│ JANUARY 2027 v7.0.0 major release (next year)              │
│              └─ Planning starts: Q3 2026                    │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

### Monthly Patch Release (v6.0.0 → v6.0.1 → v6.0.2 ...)

**Timing**: Last Monday of each month at 03:00 UTC (low-traffic window)

**Process**:
1. **Patch Accumulation** (4 weeks): Collect all bug fixes, minor improvements
2. **Staging Test** (2 days): Test accumulated fixes on staging environment
3. **Release Notes Draft** (1 day): Document all fixes + security improvements
4. **Release** (1 day): Build binary, tag git, publish, notify users
5. **Monitoring** (1 week): Watch for regressions

**Criteria for Patch Release**:
- ✅ All tests pass (unit + integration)
- ✅ No new warnings/errors in compilation
- ✅ All fixes documented in CHANGELOG
- ✅ Security patches applied (if CVEs present)
- ✅ Performance SLOs verified (no regressions)

**Typical Patches Include**:
- Bug fixes (race conditions, edge cases)
- Dependency security updates
- Documentation improvements
- Performance optimizations
- No new features (reserved for MINOR)

### Quarterly Feature Release (v6.0 → v6.1 → v6.2 → v6.3)

**Timing**: 15th of March, June, September, December

**Process**:
1. **Feature Planning** (6 weeks): Determine Q1/Q2/Q3/Q4 feature set
2. **Feature Development** (8 weeks): Develop + test features
3. **Feature Freeze** (2 weeks before release): No new features, bug fixes only
4. **Release Candidate** (1 week): v6.1.0-rc.1 deployed to staging + early customers
5. **GA Release** (1 day): v6.1.0 released publicly
6. **Monitoring** (2 weeks): Watch for regressions, fast-track patch if needed

**Criteria for Feature Release**:
- ✅ All features tested (unit + integration + staging UAT)
- ✅ No regressions from previous version
- ✅ Backward compatible (no breaking changes)
- ✅ Documentation updated (new features documented)
- ✅ Performance SLOs maintained or improved
- ✅ Security review completed (no new CVEs)

**Typical Features Include**:
- New cloud provider support (Azure, AWS)
- New CLI commands
- New ontology domains
- New SPARQL patterns
- Performance improvements
- Enhanced documentation

### Annual Major Release (v5 → v6 → v7)

**Timing**: January 15th (annual)

**Process**:
1. **Planning** (Q3-Q4 previous year): Identify breaking changes, plan v7 features
2. **Development** (Oct-Dec): Implement breaking changes + new features
3. **Deprecation Cleanup** (Nov-Dec): Remove features deprecated 2 versions ago
4. **Release Candidate** (Dec 1-15): v6.0.0-rc.1 through rc.N
5. **GA Release** (Jan 18): v6.0.0 released publicly
6. **Support**: v6 actively supported (current major)
7. **Monitoring** (Q1): Watch for widespread issues, fast-track patch if needed

**Criteria for Major Release**:
- ✅ All breaking changes documented in CHANGELOG (migration guide required)
- ✅ All new major features tested + documented
- ✅ Deprecated features removed (after 2-version window)
- ✅ Performance improvements validated
- ✅ Security review + penetration testing completed
- ✅ Government contract compliance verified (FedRAMP, TAI 2030)

**Typical Major Changes Include**:
- API redesigns
- Core algorithm improvements
- Dependency major upgrades
- New major feature (e.g., multi-tenancy)
- Deprecated feature removal
- Breaking configuration changes

---

## Branch Strategy

### Git Branch Model

```
main (production)
  ├─ release/v6.0  (release branch, patch releases)
  │   ├─ v6.0.0 (tag)
  │   ├─ v6.0.1 (tag)
  │   └─ v6.0.2 (tag)
  │
  ├─ release/v6.1  (release branch, patch releases)
  │   ├─ v6.1.0 (tag)
  │   └─ v6.1.1 (tag)
  │
  └─ develop (development branch)
      ├─ feature/aws-support (feature branch)
      ├─ feature/multi-tenancy (feature branch)
      ├─ fix/sku-race-condition (bugfix branch)
      └─ docs/versioning-guide (docs branch)
```

### Branch Naming Conventions

| Branch Type | Pattern | Example | Purpose |
|---|---|---|---|
| **Main** | `main` | `main` | Production-ready code, always stable |
| **Release** | `release/v6.x` | `release/v6.0` | Patch releases for a major.minor |
| **Develop** | `develop` | `develop` | Integration branch for features |
| **Feature** | `feature/[name]` | `feature/aws-support` | New feature development |
| **Bugfix** | `fix/[name]` | `fix/sku-race-condition` | Bug fix development |
| **Docs** | `docs/[name]` | `docs/versioning-guide` | Documentation updates |
| **Chore** | `chore/[name]` | `chore/update-deps` | Maintenance, dependency updates |

### Main Branch Rules

**main** is production code, always stable:

- ✅ Always release-candidate or GA quality
- ✅ All tests passing (100% pass rate required)
- ✅ No WIP (work-in-progress) commits
- ✅ Every commit is a potential release
- ✅ Protected: requires PR review + CI passing before merge
- ✅ Protected: requires signed commits (GPG signature)

### Release Branch Rules

**release/v6.x** is for patch releases:

- ✅ Created from main when major.minor released (e.g., v6.0.0)
- ✅ Only accepts backported bug fixes (cherry-pick from develop)
- ✅ No new features (reserved for develop → next minor)
- ✅ Patch versions tagged from this branch
- ✅ Once v7.0.0 released, no more v6.x patches (except security)

### Develop Branch Rules

**develop** is integration branch:

- ✅ Features merged from feature branches
- ✅ Bug fixes merged from fix branches
- ✅ All tests passing (required before merge)
- ✅ Feature freeze 2 weeks before release
- ✅ Used to stage next feature release

---

## Git Tagging

### Tag Format

```
v6.0.0          (GA release)
v6.0.0-rc.1     (release candidate 1)
v6.0.0-rc.2     (release candidate 2)
v6.0.0-beta     (beta version)
```

### Tagging Rules

**Rule 1: Only tag from main or release branches**
```bash
# ✅ CORRECT: Tag from main branch (production release)
git tag -s v6.0.0 -m "ggen v6.0.0 GA release"

# ✅ CORRECT: Tag from release branch (patch release)
git tag -s v6.0.1 -m "ggen v6.0.1 patch release (bug fix)"

# ❌ WRONG: Tag from feature branch
git checkout feature/aws-support
git tag -s v6.0.0  # DON'T DO THIS
```

**Rule 2: Use signed tags (GPG signature required)**
```bash
git tag -s v6.0.0 -m "Release message"  # -s = signed
```

**Rule 3: Tag message includes release notes summary**
```
v6.0.0 GA Release

New Features:
- Unified generation pipeline (μ₁-μ₅)
- Policy hot-reload support
- Multi-cloud support (AWS + GCP + Azure)

Breaking Changes:
- Removed deprecated ggen validate command
- Changed TOML config format

See CHANGELOG.md for full details.
```

**Rule 4: Annotated tags (not lightweight)**
```bash
# ✅ CORRECT: Annotated tag
git tag -s -a v6.0.0 -m "Release message"

# ❌ WRONG: Lightweight tag (no signature, no message)
git tag v6.0.0
```

### Tag Automation

**CI/CD Pipeline** automatically tags on release:

```yaml
# .github/workflows/release.yml
on:
  push:
    branches:
      - main
  workflow_dispatch:
    inputs:
      version:
        description: "Release version (e.g., v6.0.1)"
        required: true

jobs:
  release:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Create Git tag
        run: |
          git tag -s v${{ inputs.version }} \
            -m "Release v${{ inputs.version }}"
      - name: Push tag
        run: git push origin v${{ inputs.version }}
```

---

## Deprecation Policy

### Deprecation Timeline

```
DEPRECATION TIMELINE:

Version v5.0 → Feature marked deprecated (e.g., "ggen validate" command)
  "This command is deprecated and will be removed in v7.0.0."

Version v5.1, v5.2, v5.3 → Still supported (2 minor versions)
  Warnings in logs: "Using deprecated feature [X], will be removed in v7.0.0"

Version v6.0 → 1 major version remaining (final warning)
  Deprecation warning: "DEPRECATED: Feature [X] removed in v7.0.0"
  User must migrate NOW

Version v7.0 → REMOVED (no longer available)
  Feature completely gone. Users who didn't migrate get compile errors.
```

### Deprecation Process

**Step 1: Deprecate in MINOR release**
```rust
// CHANGELOG: Marked for deprecation in v5.0.0
#[deprecated(since = "5.0.0", note = "Use new_command instead")]
pub fn old_command() { ... }
```

**Step 2: Keep for 2 MINOR versions**
```
v5.0.0: deprecated
v5.1.0: deprecated (1st minor version after)
v5.2.0: deprecated (2nd minor version after)
v5.3.0: deprecated (still, but approaching removal)
```

**Step 3: Final warning in next MAJOR**
```rust
// v6.0.0: Last chance! This will be removed in v7.0.0
#[deprecated(since = "6.0.0", note = "Will be removed in v7.0.0")]
pub fn old_command() { ... }
```

**Step 4: Remove in next MAJOR**
```rust
// v7.0.0: REMOVED
// pub fn old_command() { }  ← DELETED
```

### Deprecation Communication

**Each deprecation is communicated via**:

1. **CHANGELOG.md** (detailed migration guide)
   ```markdown
   ## Deprecated Features (removed in v7.0.0)

   ### ggen validate command (deprecated in v5.0.0)
   - **Reason**: Functionality merged into ggen sync
   - **Replacement**: Use `ggen sync --validate_only true`
   - **Timeline**: Removed in v7.0.0
   - **Migration**:
     ```bash
     # OLD: ggen validate spec.ttl
     # NEW:
     ggen sync --validate_only true
     ```
   ```

2. **Release Notes** (summary of deprecations)
   ```
   v6.0.0 Release Notes

   Breaking Changes & Deprecations:
   - REMOVED: ggen validate (deprecated since v5.0.0)
     → Use: ggen sync --validate_only true
   - DEPRECATED: ggen generate (will be removed in v7.0.0)
     → Use: ggen sync instead
   ```

3. **Compiler Warnings** (at build/runtime)
   ```
   warning: use of deprecated function `old_command`:
   Use new_command instead, will be removed in v7.0.0
   ```

4. **Runtime Warnings** (in logs)
   ```
   [WARN] Using deprecated feature 'ggen validate'.
          This will be removed in v7.0.0.
          Please migrate to: ggen sync --validate_only true
   ```

---

## Support Policy

### Active Support Timeline

```
Current Year (2026):
v6.x (released Jan 2026) ←───── ACTIVELY SUPPORTED (current)
v5.x (released Jan 2025) ←───── ACTIVELY SUPPORTED (n-1)
v4.x (released Jan 2024) ←───── ACTIVELY SUPPORTED (n-2)
v3.x (released Jan 2023) ← UNSUPPORTED (EOL'd Jan 2026)

Each major version: 24 months of support (current + 2 prior)
```

### Support Tiers

| Version | Status | Patch Frequency | Bug Fixes | Security | EOL Date |
|---------|--------|---|---|---|---|
| v6.x (current) | Active | Monthly | ✅ Yes | ✅ Yes | Jan 2027 |
| v5.x (n-1) | Supported | As-needed | ✅ Yes | ✅ Yes | Jan 2026 |
| v4.x (n-2) | Supported | As-needed | ✅ Critical bugs | ✅ Yes | Jan 2025 |
| v3.x (n-3+) | EOL | None | ❌ No | ❌ No | Jan 2023 |

### Support Definition

**Active Support** (current + 2 prior versions):
- ✅ Bug fixes issued regularly (monthly patch releases)
- ✅ Security patches within 7 days of CVE discovery
- ✅ Documentation updated
- ✅ Customer support available (SLA: 24-hour response)

**Extended Support** (3+ versions old):
- ⚠️ Critical security patches only (no bug fixes)
- ⚠️ Customer support available (SLA: 5-day response)
- ⚠️ Migration to current version strongly recommended

**End of Life** (4+ versions old):
- ❌ No support (customers must upgrade or maintain themselves)
- ❌ No security patches
- ❌ No customer support

---

## Security Updates

### CVE Response Process

**Timeline**:

1. **CVE Reported** (t=0)
   - Security researcher or vendor discovers vulnerability
   - CVE ID assigned (e.g., CVE-2026-0123)
   - Details shared with ggen maintainers

2. **Triage** (t=1 day)
   - ggen team assesses impact
   - Determine affected versions (v6.x, v5.x, v4.x?)
   - Severity rating (CRITICAL / HIGH / MEDIUM / LOW)

3. **Development** (t=2-3 days)
   - Fix implemented in develop branch
   - Fix tested (unit + integration tests)
   - Security review completed

4. **Patch Release** (t=4-5 days)
   - Patch released to all affected versions (e.g., v6.0.2, v5.3.1, v4.4.2)
   - Release notes include security advisory
   - Customers notified via email + security mailing list

5. **Disclosure** (t=7 days)
   - Public disclosure of vulnerability
   - Customers given 7+ days notice before disclosure
   - Advisory published on security page

### Security Patch Priority

| Severity | Impact | Response SLO | Example |
|---|---|---|---|
| **CRITICAL** | Remote code execution, privilege escalation | 24 hours | Deserialization vulnerability |
| **HIGH** | Data exposure, authentication bypass | 2-3 days | SQL injection in query builder |
| **MEDIUM** | Denial of service, information disclosure | 7 days | Unvalidated input in CLI |
| **LOW** | Minor information disclosure | 30 days | Timing attack in comparison |

### Security Update Coordination

**Responsible Disclosure** (coordinated with vendor):

1. Private notification to affected parties
2. Embargo period (7 days minimum) before public disclosure
3. Patch released during embargo
4. Public advisory released after embargo
5. CVE details published

**Example Security Advisory** (in CHANGELOG):

```markdown
## Security Patches (v6.0.2 - Released 2026-01-28)

### CVE-2026-0123: RDF Injection in Marketplace Processor
**Severity**: HIGH
**Affected Versions**: v6.0.0, v6.0.1, v5.3.x, v5.2.x, v5.1.x
**Fix**: v6.0.2, v5.3.2
**Description**:
Improper input validation in marketplace RDF processor allows
SPARQL injection via untrusted marketplace data. Attacker can
read/modify marketplace catalog.

**Impact**:
- Confidentiality: HIGH (catalog data exposed)
- Integrity: HIGH (catalog data modified)
- Availability: LOW (service still operational)

**Mitigation**:
- Upgrade to v6.0.2 or v5.3.2 immediately
- Or: Apply input validation patch manually
- Or: Disable marketplace processor until patched

**Timeline**:
- Reported: 2026-01-20
- Fixed: 2026-01-26
- Released: 2026-01-28
- Disclosed: 2026-01-28

**Credits**: Jane Smith <jane@security.com>

**References**:
- https://nvd.nist.gov/vuln/detail/CVE-2026-0123
- GitHub issue #1234
```

---

## Release Notes & CHANGELOG

### CHANGELOG.md Format

Every release includes detailed CHANGELOG entry:

```markdown
# CHANGELOG

All notable changes to ggen are documented in this file.

The format is based on Keep a Changelog (https://keepachangelog.com/),
and this project adheres to Semantic Versioning (https://semver.org/).

## [6.0.0] - 2026-01-18

### Added
- New unified generation pipeline (μ₁-μ₅)
- Policy hot-reload support
- AWS Marketplace integration (scope MOD)
- Multi-cloud deployment templates
- SPARQL query optimizer
- New CLI command: ggen sync

### Changed
- Renamed ggen generate → ggen sync (see migration guide below)
- Updated TOML configuration format (see migration guide)
- Improved error messages with context
- Refactored RDF processor for 20% performance gain

### Deprecated
- ggen generate command (deprecated, use ggen sync)
- Old TOML format (use migration script)
- SPARQL 1.0 compatibility (SPARQL 1.1 only)

### Removed
- ggen validate command (removed, use ggen sync --validate_only)
- Legacy template engine (removed, use Tera)

### Fixed
- Fixed memory leak in Catalog Controller
- Fixed race condition in Firestore concurrent writes
- Fixed template rendering edge case with nested loops

### Security
- Updated cryptographic library (Oxigraph 0.5.3)
- Fixed RDF injection vulnerability (CVE-2026-0099)
- Implemented input validation for marketplace data

### Performance
- 30% faster RDF processing (optimized SPARQL)
- 20% faster template rendering (Tera optimizer)
- Reduced memory footprint by 15%

### Documentation
- Added AWS deployment guide
- Added SPARQL optimization guide
- Added troubleshooting section

### Migration Guide

#### ggen generate → ggen sync
```bash
# OLD:
ggen generate --template spec.tera --output code.rs

# NEW:
ggen sync
```

#### TOML Format Change
```toml
# OLD:
[templates]
output_dir = "src/generated"

# NEW:
[generation]
templates_dir = ".specify/templates"
output_dir = "src/generated"

# Use migration script:
ggen migrate-config --input ggen.toml.old --output ggen.toml
```

## [5.3.0] - 2025-10-15

### Added
- New SPARQL inference engine
- Support for OWL 2 Full profile

... [rest of changelog]
```

### Release Notes (for each release)

Release notes are more user-friendly summary:

```markdown
# ggen v6.0.0 Release Notes (January 18, 2026)

**Download**: https://github.com/seanchatmangpt/ggen/releases/tag/v6.0.0

## What's New in v6.0.0?

### 🚀 Major New Features

**Unified Generation Pipeline (μ)**
The core generation process is now a unified pipeline: Normalize → Extract → Emit → Canonicalize → Receipt.
This replaces separate ggen generate/template/validate commands with a single `ggen sync` command.

**Policy Hot-Reload**
Safe configuration changes (pricing, audit thresholds, feature flags) can now be applied without restart.
Use: `ggen sync --policy <config>`

**Multi-Cloud Support**
ggen now supports AWS and Azure in addition to GCP. Deployment templates provided for all three.

### 📈 Performance Improvements

- RDF processing: 30% faster (SPARQL optimizer)
- Template rendering: 20% faster (Tera optimizations)
- Memory usage: 15% reduction

### 🔒 Security

- Updated cryptographic dependencies
- Fixed RDF injection vulnerability (CVE-2026-0099)
- Enhanced input validation for marketplace data

### ⚠️ Breaking Changes

**ggen generate → ggen sync**
The `ggen generate` command is replaced with `ggen sync`. Update your scripts:
```bash
# OLD: ggen generate --template spec.tera
# NEW: ggen sync
```

**TOML Configuration Format**
The ggen.toml configuration file has a new format. Run migration script:
```bash
ggen migrate-config --input ggen.toml.old --output ggen.toml
```

### 🗑️ Deprecated & Removed

- Removed: ggen validate command
- Deprecated: Legacy template engine (use Tera)
- Deprecated: SPARQL 1.0 (SPARQL 1.1 required)

### 📚 Documentation

New guides:
- AWS Marketplace Deployment
- SPARQL Query Optimization
- Policy Configuration

### 🙏 Thanks

Special thanks to GSA for the AWS Marketplace MOD, and to all contributors.

---

## Upgrading to v6.0.0

### From v5.x

See [MIGRATION_GUIDE.md](MIGRATION_GUIDE.md) for step-by-step upgrade instructions.

### Support

- Docs: https://ggen.io/docs
- Issues: https://github.com/seanchatmangpt/ggen/issues
- Chat: https://ggen.io/chat
```

---

## Version Compatibility Matrix

### Dependency Versions (Locked in Cargo.lock)

```
ggen v6.0.0 uses:
├─ Rust 1.91.1 (stable)
├─ Tokio 1.47 (async runtime)
├─ Oxigraph 0.5.1 (RDF processing)
├─ Tera 1.20 (templates)
├─ Serde 1.0 (serialization)
├─ Clap 4.5 (CLI)
├─ Criterion 0.7 (benchmarks)
└─ [27 total crates]

Compatibility:
- Rust: 1.91.1 + (forward compatible with 1.92, 1.93, etc.)
- Tokio: 1.47 + (forward compatible with 1.48, 1.49, etc.)
- Oxigraph: 0.5.1 (pin to avoid RDF API breaking changes)
- All others: Cargo Workspace manages versions
```

### Cloud Provider Compatibility

| Provider | Tested Versions | Status | Support |
|---|---|---|---|
| **GCP** | GKE 1.27-1.29, Firestore, Cloud Run | ✅ Tested | Active |
| **AWS** | EKS 1.27-1.29, DynamoDB, Lambda (v6.1+) | ✅ Tested | v6.1+ |
| **Azure** | AKS 1.27-1.29, CosmosDB, Container Apps (v6.2+) | ✅ Tested | v6.2+ |

### Client Library Compatibility

| Language | Lib Version | Tested | EOL |
|---|---|---|---|
| **Rust** | v6.0.0 crate | 1.91.1 | Jan 2027 |
| **Node.js** | v6.0.0 npm | 18 LTS | Jan 2027 |
| **Python** | v6.0.0 pip | 3.9+ | Jan 2027 |
| **Go** | v6.0.0 module | 1.21+ | Jan 2027 |

---

## Release Process

### Pre-Release Checklist

Before releasing a version, verify:

- [ ] **Code Quality**
  - [ ] All tests passing (unit + integration)
  - [ ] No compiler warnings
  - [ ] No clippy lints
  - [ ] Code review completed

- [ ] **Documentation**
  - [ ] CHANGELOG.md updated with all changes
  - [ ] Release notes drafted
  - [ ] API docs updated (rustdoc)
  - [ ] User guide updated
  - [ ] Migration guide prepared (if breaking changes)

- [ ] **Security**
  - [ ] CVE scan passed (no new vulnerabilities)
  - [ ] Dependency audit passed
  - [ ] Security review completed (if applicable)
  - [ ] No hardcoded secrets in code

- [ ] **Performance**
  - [ ] Benchmark SLOs verified (no regressions)
  - [ ] Load test passed (if applicable)
  - [ ] Memory profiling completed

- [ ] **Government Compliance**
  - [ ] SBOM (Software Bill of Materials) generated
  - [ ] Audit trail verified
  - [ ] FedRAMP impact assessed
  - [ ] Compliance controls updated (if applicable)

### Release Steps

1. **Create Release Branch** (if patch)
   ```bash
   git checkout -b release/v6.0.1
   ```

2. **Update Version**
   ```bash
   # Update Cargo.toml
   sed -i 's/version = "6.0.0"/version = "6.0.1"/' Cargo.toml

   # Update version in code
   sed -i 's/const VERSION: &str = "6.0.0"/const VERSION: &str = "6.0.1"/' src/lib.rs
   ```

3. **Update CHANGELOG**
   ```bash
   # Add v6.0.1 section to CHANGELOG.md
   # Document all fixes + security patches
   ```

4. **Commit Changes**
   ```bash
   git add Cargo.toml src/lib.rs CHANGELOG.md
   git commit -m "chore: Bump version to v6.0.1"
   ```

5. **Create Signed Tag**
   ```bash
   git tag -s v6.0.1 -m "ggen v6.0.1 patch release"
   ```

6. **Push to GitHub**
   ```bash
   git push origin release/v6.0.1
   git push origin v6.0.1  # Push tag
   ```

7. **Build Release**
   ```bash
   cargo build --release
   cargo make release-validate
   ```

8. **Create GitHub Release**
   - Draft release notes
   - Attach binary artifacts
   - Mark as latest release

9. **Publish to Registries**
   ```bash
   cargo publish  # Publish to crates.io
   npm publish    # Publish to npm registry
   ```

10. **Announce Release**
    - Email security list (security@ggen.io)
    - Post on GitHub Releases
    - Update website/blog
    - Notify government customers (if applicable)

11. **Monitor Deployment**
    - Watch for issues in first 24 hours
    - Fast-track patch if critical issue found

---

## Version Bumping Checklist

**Before merging a version bump, verify:**

- [ ] **Version Format**
  - [ ] Version follows MAJOR.MINOR.PATCH format
  - [ ] Version is higher than previous (no downgrades)
  - [ ] Version matches across all files (Cargo.toml, src/lib.rs, package.json, etc.)

- [ ] **Git Tag**
  - [ ] Tag created with git tag -s v...
  - [ ] Tag has meaningful message
  - [ ] Tag signed with GPG key
  - [ ] Tag points to correct commit

- [ ] **Changelog**
  - [ ] CHANGELOG.md has entry for new version
  - [ ] Entry includes: Added, Changed, Deprecated, Removed, Fixed, Security, Performance
  - [ ] Breaking changes clearly marked
  - [ ] Migration guide included (if needed)

- [ ] **Documentation**
  - [ ] API docs updated (if API changed)
  - [ ] User guide updated (if user-facing changes)
  - [ ] Deprecation notices added (if applicable)
  - [ ] README updated (if necessary)

- [ ] **Tests**
  - [ ] All tests passing (0 failures)
  - [ ] Test coverage maintained or improved (>80%)
  - [ ] New tests added (if new features)
  - [ ] Integration tests passing

- [ ] **Build & Release**
  - [ ] cargo build succeeds
  - [ ] cargo test succeeds
  - [ ] cargo check succeeds
  - [ ] cargo make release-validate passes
  - [ ] Binary artifacts built successfully

- [ ] **Security**
  - [ ] CVE scan passed (cargo audit)
  - [ ] No new vulnerabilities
  - [ ] Dependencies up-to-date (security patches)
  - [ ] Secrets not committed

---

## Definition of Done

Version release is complete when:

- ✅ Version number updated (Cargo.toml, code, docs)
- ✅ Git tag created + signed (v6.0.0 format)
- ✅ CHANGELOG.md updated (all changes documented)
- ✅ Release notes written (user-friendly summary)
- ✅ All tests passing (unit + integration, 0 failures)
- ✅ CVE scan passed (no new vulnerabilities)
- ✅ Performance SLOs verified (no regressions)
- ✅ FedRAMP compliance verified (if government release)
- ✅ SBOM generated (Software Bill of Materials)
- ✅ Binary artifacts built + tested
- ✅ GitHub release created (with release notes)
- ✅ Published to registries (crates.io, npm, etc.)
- ✅ Security list notified (for security patches)
- ✅ Government customers notified (if required)
- ✅ Release announced (GitHub, email, website)
- ✅ Monitoring activated (watch for issues 24-48h)

---

## Receipt Contract

**This document is a binding policy specification. Every release must produce:**

1. **Version Receipt**: JSON record of version metadata (number, date, branch, commit hash)
2. **Build Receipt**: JSON record of build artifacts (binary checksums, build time, SLO metrics)
3. **Test Receipt**: JSON record of test results (pass rate, coverage, duration)
4. **Security Receipt**: JSON record of CVE scan (CVE count, severity levels, patches applied)
5. **SBOM Receipt**: Software Bill of Materials (component versions, licenses, checksums)
6. **Release Receipt**: JSON record of release publication (registry, timestamp, URL)

**All receipts stored in `/ggen/receipts/releases/v6.0.0/` and cryptographically signed.**

**Verification**: Users can audit any release by requesting `/ggen/receipts/releases/v*/`

---

**Last Updated**: January 2026 | **Next Review**: April 2026
