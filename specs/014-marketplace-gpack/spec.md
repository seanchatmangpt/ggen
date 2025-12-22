<!-- Generated from feature.ttl - DO NOT EDIT MANUALLY -->

# Marketplace Gpack Distribution Retrofit

**Feature Branch**: `014-marketplace-gpack`
**Feature Number**: 014
**Created**: 2025-12-21
**Status**: Specification Phase (RDF-First)

---

## Overview

**Problem Statement**: The ggen marketplace currently uses a custom distribution method that requires reinventing package distribution infrastructure. Users must rely on ggen's proprietary distribution system rather than leveraging the standard Rust ecosystem.

**Solution**: Retrofit the marketplace to use `*-gpack` packages publishable to crates.io, enabling:
- Standard Rust ecosystem integration
- Deterministic, reproducible distribution
- Community-driven discovery via crates.io
- Quality-based package recommendations
- FMEA-validated installation safety

**Scope**: Transition 84 existing marketplace packages from custom distribution to crates.io-compatible gpack format without breaking changes.

---

## User Stories

### Story 1 (P1): Package Developers Publish Gpack to crates.io

**JTBD**: When I have a marketplace package, I want to publish it to crates.io as a gpack so it reaches all Rust developers without custom tooling

**Acceptance Criteria**:

1. **Basic Publication**
   - Given: Package maintainer with marketplace package (pkg, version, README, validation report)
   - When: `ggen marketplace publish --package pkg --target crates.io`
   - Then: Package published as `pkg-gpack` to crates.io with correct Cargo.toml metadata
   - Verification: Package appears in crates.io within 30 seconds, searchable by name

2. **Dependency Resolution**
   - Given: Marketplace package with declared dependencies on other marketplace packages
   - When: Package is published to crates.io
   - Then: Dependencies resolved to their corresponding gpack crates.io packages
   - Verification: Cargo can resolve all transitive dependencies without conflicts

3. **Validation Requirement**
   - Given: Marketplace package with missing FMEA validation report
   - When: Publish is attempted
   - Then: Publication blocked with clear error message requiring FMEA validation
   - Verification: User receives actionable error, can fix and retry

---

### Story 2 (P1): Users Install Gpack Packages from crates.io

**JTBD**: When I need a marketplace package, I want to install it from crates.io using `ggen marketplace install` so it integrates seamlessly with my workflow

**Acceptance Criteria**:

1. **Basic Installation**
   - Given: User with ggen CLI v5.3+ and crates.io internet access
   - When: `ggen marketplace install pkg-gpack --from crates.io`
   - Then: Package downloaded, installed with all dependencies, available for use
   - Verification: Package files in standard location, can be imported/used immediately

2. **Installation Validation**
   - Given: Gpack with FMEA validation report in crates.io
   - When: Installation occurs
   - Then: FMEA validation checked, poka-yoke guards applied during installation
   - Verification: Installation logs show validation passed, guards confirmed active

3. **Offline Installation**
   - Given: User who previously installed gpack packages
   - When: Network unavailable but cached packages available
   - Then: Installation proceeds with cached versions without network access
   - Verification: Installation succeeds, warnings indicate cached versions

---

### Story 3 (P1): Users Search crates.io for Marketplace Packages

**JTBD**: When I'm looking for a specific marketplace capability, I want to search crates.io for gpack packages so I can discover community solutions

**Acceptance Criteria**:

1. **Basic Search**
   - Given: User searching crates.io for `gpack` packages
   - When: Using crates.io search or `ggen marketplace search gpack`
   - Then: Results show all published gpack packages with download counts, descriptions
   - Verification: Top 10 gpack packages returned within 2 seconds

2. **Search Filtering**
   - Given: Search results for gpack packages
   - When: User filters by category (RDF, validation, ontology) or quality tier
   - Then: Results filtered to matching packages with quality indicators shown
   - Verification: Filters reduce results correctly, quality tier displayed (bronze/silver/gold)

3. **Quality Indicators**
   - Given: Gpack package with FMEA validation report and download history
   - When: Appears in search results
   - Then: Quality metrics displayed (FMEA pass/fail, download trend, last update)
   - Verification: Users can make informed decisions based on quality indicators

---

### Story 4 (P2): Deterministic Dependency Resolution

**JTBD**: When I install a gpack with dependencies, I want version conflicts resolved automatically so installation is deterministic and reproducible

**Acceptance Criteria**:

1. **Lockfile Generation**
   - Given: Gpack manifest with version range constraints (e.g., >=1.0,<2.0)
   - When: Installation completes
   - Then: `ggen.lock` file generated with exact pinned versions
   - Verification: Lock file reproducibly installs same versions on different systems

2. **Conflict Detection**
   - Given: Two dependencies requiring incompatible versions
   - When: Installation attempted
   - Then: Clear error identifying conflict with suggested resolutions
   - Verification: User sees which packages conflict and understands resolution paths

---

### Story 5 (P2): FMEA Validation During Installation

**JTBD**: When I install a gpack, I want FMEA controls validated automatically so I have confidence the package is safe and well-tested

**Acceptance Criteria**:

1. **FMEA Validation Check**
   - Given: Gpack with FMEA validation report (critical RPN > 200)
   - When: Installation occurs
   - Then: FMEA validation checked, controls applied (directory separation, trait boundaries)
   - Verification: Installation logs show FMEA status, guards confirmed active

2. **Critical Failure Blocking**
   - Given: Gpack with unresolved critical FMEA failure modes (RPN >= 200)
   - When: User attempts installation with default settings
   - Then: Installation blocked with clear error, suggesting manual review or `--force-fmea`
   - Verification: Cannot accidentally install unsafe packages without explicit override

---

### Story 6 (P3): Package Recommendations

**JTBD**: When I search for a package, I want recommendations for high-quality alternatives so I can choose packages that fit my needs best

**Acceptance Criteria**:

1. **Quality Tier Recommendations**
   - Given: Multiple gpack packages with different quality levels
   - When: User searches with `--quality gold` or similar
   - Then: Results sorted by quality tier (gold/silver/bronze)
   - Verification: Gold tier packages (FMEA passed, >100 downloads, recent) appear first

---

## Functional Requirements (FR-XXX)

| ID | Requirement | Description |
|---|---|---|
| FR-001 | Gpack Format Specification | Define `*-gpack` format compatible with crates.io Cargo.toml |
| FR-002 | Manifest Schema | YAML/TOML schema with metadata, dependencies, FMEA references |
| FR-003 | Crates.io API Integration | Publish via Cargo and programmatic API |
| FR-004 | Installation Resolver | Download, resolve dependencies, install from crates.io |
| FR-005 | Validation Hooks | Execute FMEA checks and poka-yoke guards during install |
| FR-006 | Offline Support | Cache packages locally, enable offline installation |
| FR-007 | Search via SPARQL | Query marketplace metadata with flexible filtering |
| FR-008 | Quality Indicators | Show FMEA status, download trend, update recency |
| FR-009 | Lockfile Format | Generate ggen.lock for deterministic installation |
| FR-010 | Conflict Resolution | Detect version conflicts with actionable suggestions |
| FR-011 | FMEA Integration | Fetch and validate FMEA reports during installation |
| FR-012 | Poka-Yoke Guards | Apply error-prevention controls (directory separation, traits) |
| FR-013 | Recommendation Engine | Rank packages by quality tier (gold/silver/bronze) |

---

## Success Criteria (SC-XXX) - Measurable Outcomes

| ID | Criterion | Measurement | Verification Method |
|---|---|---|---|
| SC-001 | Backward Compatibility | 100% of 84 packages convert to gpack | Automated conversion + smoke test |
| SC-002 | Publish Latency | Published packages searchable ≤30 seconds | Publish 3 test packages, measure |
| SC-003 | Install Performance | Installation ≤30 seconds (5-10MB, 25Mbps) | Benchmark 10 representative packages |
| SC-004 | Search Latency | Search returns top 20 results ≤1 second | Load test 100 concurrent searches |
| SC-005 | FMEA Coverage | 100% of installations have FMEA audit entry | Analyze audit trail across 100+ installs |
| SC-006 | Zero Breaking Changes | Existing CLI workflows unchanged | Integration tests pass for all CLI commands |
| SC-007 | Deterministic Distribution | Identical outputs across systems (SHA256 match) | Cross-platform install with hash verification |

---

## Key Domain Entities

### GpackManifest
Manifest describing a gpack package compatible with crates.io
- `crate_name`: String (must end with `-gpack`)
- `version`: Version (semantic versioning)
- `description`: String (1-500 chars)
- `dependencies`: Map<String, VersionRange>
- `fmea_reference`: Url (to FMEA validation report)
- `quality_tier`: Enum (bronze|silver|gold)
- `homepage`: Url
- `documentation`: Url

**Relationships**: publishedTo CratesIndex, validatedBy FmeaValidation

### CratesIndex
Crates.io registry for gpack packages
- `crate_name`: String (unique)
- `latest_version`: Version
- `download_count`: Integer
- `last_updated`: DateTime (RFC3339)
- `metadata`: Json (quality indicators, FMEA status)

**Relationships**: contains GpackManifest, indexed_by SearchEngine

### InstallationMetadata
Record of gpack installation with validation results
- `crate_name`: String (installed gpack)
- `version`: Version (exact version)
- `installed_at`: DateTime (RFC3339)
- `fmea_validation_passed`: Boolean
- `guards_applied`: List<String> (poka-yoke guards)
- `dependencies_resolved`: Map<String, Version>

### FmeaValidation
FMEA validation report for gpack
- `crate_name`: String
- `version`: Version
- `failure_modes`: List<FailureMode>
- `critical_failures`: Integer (count of RPN >= 200)
- `controls_implemented`: List<String>
- `validation_date`: DateTime (RFC3339)

### PokayokeGuards
Error-prevention controls applied during installation
- `guard_type`: Enum (directory_separation|trait_boundary|path_protection|version_constraint)
- `enabled`: Boolean
- `violation_count`: Integer

### LockFile
Deterministic lock file (ggen.lock) pinning exact versions
- `version`: String (lock format version)
- `packages`: Map<String, Version> (exact pinned)
- `checksums`: Map<String, String> (SHA256)
- `generated_at`: DateTime (RFC3339)

---

## Edge Cases & Error Scenarios

| ID | Scenario | Expected Behavior | Acceptance |
|---|---|---|---|
| E-001 | Version Conflict | Installation fails, shows conflicting packages/versions | Error identifies packages + possible resolutions |
| E-002 | Network Failure | Resume from checkpoint, no re-download | Partial downloads cached, retry continues |
| E-003 | Missing FMEA | Installation proceeds with warning | Success with FMEA warning in logs |
| E-004 | Legacy Packages | Old + new packages coexist peacefully | Both legacy and gpack usable together |
| E-005 | Cache Invalidation | Detect newer versions available when reconnected | System prompts to update when network restored |

---

## Documented Assumptions

| ID | Assumption | Rationale |
|---|---|---|
| A-001 | Crates.io API maintains backward compatibility (2+ years) | Reduces brittleness if API changes |
| A-002 | All gpack versions follow SemVer (major.minor.patch) | Enables deterministic constraint resolution |
| A-003 | Ggen generates deterministic, reproducible outputs | Prevents supply chain attacks, enables verification |
| A-004 | Crates.io is standard Rust ecosystem distribution | Aligns with conventions, maximizes discoverability |
| A-005 | Marketplace maintainers maintain current FMEA reports | Ensures quality indicators remain accurate |

---

## Implementation Context: 80/20 Analysis

### 20% Effort → 80% Impact (Critical Path)

**Estimated**: 80-120 hours

1. Gpack format specification (YAML/Cargo.toml compatible)
2. Crates.io API integration (publish/download)
3. CLI: `ggen marketplace publish` → crates.io
4. Installation resolver (download + dependency resolution)
5. FMEA validation hook integration

### 80% Effort → 20% Impact (Supporting Work)

**Estimated**: 320-480 hours

1. Search SPARQL query optimization
2. Recommendation algorithm refinement
3. Quality tier heuristics
4. Cross-platform offline cache
5. Version conflict resolution variations
6. Dependency scenario testing
7. 84-package migration documentation
8. Performance optimization

---

## Specification Summary

- **User Stories**: 6 (3 P1, 2 P2, 1 P3)
- **Functional Requirements**: 13 (FR-001 → FR-013)
- **Success Criteria**: 7 measurable outcomes
- **Key Entities**: 8 domain entities
- **Edge Cases**: 5 documented scenarios
- **Assumptions**: 5 documented
- **Existing Packages**: 84 (migration target)
- **Backward Compatibility**: Required (100%)
- **Determinism**: Required (byte-identical across systems)
- **FMEA Integration**: Required (validation during install)

---

## RDF-First Specification

**Source of Truth**: `specs/014-marketplace-gpack/ontology/feature.ttl`
**Generated Artifact**: `specs/014-marketplace-gpack/spec.md` (this file)

**Constitutional Equation**: `spec.md = μ(feature.ttl)`

All specifications are Turtle/RDF ontologies. This markdown file is generated from feature.ttl using Tera templates. **DO NOT EDIT THIS FILE MANUALLY** - edit feature.ttl instead.

**Regenerate markdown**:
```bash
ggen render .specify/templates/spec.tera specs/014-marketplace-gpack/ontology/feature.ttl > specs/014-marketplace-gpack/spec.md
```

**Validate against SHACL**:
```bash
ggen validate specs/014-marketplace-gpack/ontology/feature.ttl
```

---

**Generated with**: ggen v6 ontology-driven specification system
**Generated**: 2025-12-21
