# Backward Compatibility Policy

**Version**: 1.0.0-rc1
**Last Updated**: 2025-01-11
**Status**: Production Ready

## Overview

This document defines ggen's commitment to backward compatibility and provides guidelines for users, contributors, and maintainers.

## Core Principles

### 1. Semantic Versioning Guarantee

We strictly follow [Semantic Versioning 2.0.0](https://semver.org/):

```
MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]
```

- **MAJOR** (1.x.x → 2.x.x): Breaking changes allowed
- **MINOR** (1.1.x → 1.2.x): New features, fully backward compatible
- **PATCH** (1.1.1 → 1.1.2): Bug fixes only, fully backward compatible

### 2. Stability Guarantees

#### Public API (✅ Guaranteed Stable)
- CLI commands and flags
- `make.toml` configuration format
- Lifecycle phase names and behavior
- Hook execution order
- State file format (`.ggen/state.json`)
- Cache key generation algorithm
- Environment variable names
- Exit codes

#### Internal API (⚠️ No Guarantees)
- Rust library internals (unless explicitly documented as public API)
- Internal data structures
- Private functions and modules
- Implementation details

#### Experimental Features (🧪 May Change)
- Features marked with `[experimental]` in documentation
- CLI commands with `--experimental` flag
- Configuration options marked as `unstable`

## Breaking Changes Policy

### What Constitutes a Breaking Change

Breaking changes include but are not limited to:
- Removing CLI commands or flags
- Changing command behavior in incompatible ways
- Removing or renaming lifecycle phases
- Changing hook execution order
- Modifying `.ggen/state.json` format (unless forward compatible)
- Changing cache key generation (invalidates all caches)
- Removing or renaming environment variables
- Changing exit codes for specific errors

### How We Handle Breaking Changes

1. **Advance Notice** (2+ minor versions)
   - Announce deprecation in changelog
   - Add deprecation warnings in CLI
   - Document migration path

2. **Migration Guide**
   - Provide step-by-step upgrade instructions
   - Include automated migration tools when possible
   - Offer compatibility shims for transitional period

3. **Major Version Release**
   - Bundle all breaking changes in major version
   - Release comprehensive upgrade guide
   - Maintain previous major version for 12 months (security fixes)

## Deprecation Process

### Timeline

```
Version N:   Feature working normally
Version N+1: Deprecation warning added, feature still works
Version N+2: Deprecation warning continues
Version N+3: Feature removed in next major version
```

### Deprecation Warnings

Deprecated features will show warnings like:

```bash
⚠️  Warning: The 'old-command' command is deprecated and will be removed in v2.0.0
    Use 'new-command' instead. See: https://docs.ggen.io/migration/v2
```

### Deprecation Tracking

All deprecations are:
- Listed in CHANGELOG.md under "Deprecated" section
- Documented in migration guides
- Tagged with GitHub issues for tracking
- Announced in release notes

## Compatibility Matrix

### make.toml Configuration

| Version | Format Changes | Forward Compatible | Backward Compatible |
|---------|---------------|-------------------|---------------------|
| 1.0.x   | Initial format | N/A | N/A |
| 1.1.x   | Added fields only | ✅ Yes | ✅ Yes |
| 1.2.x   | Optional new sections | ✅ Yes | ✅ Yes |
| 2.0.x   | Schema changes | ❌ No | ⚠️ Partial |

### State File (.ggen/state.json)

| Version | Schema | Can Read Older | Can Write Compatible |
|---------|--------|---------------|---------------------|
| 1.0.x   | v1 | N/A | ✅ Yes |
| 1.1.x   | v1 extended | ✅ Yes | ✅ Yes |
| 2.0.x   | v2 | ✅ Yes (migration) | ❌ No |

### CLI Commands

| Category | Stability | Breaking Changes Allowed |
|----------|-----------|------------------------|
| Core commands (init, build, test) | Stable | Major versions only |
| Lifecycle commands | Stable | Major versions only |
| AI commands | Experimental | Any version (with warning) |
| MCP integration | Beta | Minor versions (with migration) |

## Compatibility Testing

### Automated Tests

We maintain compatibility tests for:
- ✅ Reading old make.toml files (last 2 major versions)
- ✅ Reading old state files (last 2 major versions)
- ✅ CLI command interface stability
- ✅ Hook execution order consistency
- ✅ Cache key determinism

### CI/CD Checks

Every pull request runs:
- Compatibility test suite against previous versions
- make.toml schema validation
- State file migration tests
- CLI interface regression tests

## Migration Support

### Automated Migration Tools

```bash
# Upgrade make.toml to new format
ggen migrate make.toml --from 1.0 --to 2.0

# Migrate state files
ggen migrate state --from 1.0 --to 2.0

# Check compatibility
ggen check-compatibility --target-version 2.0
```

### Manual Migration Guides

Location: `docs/MIGRATION_GUIDE_vX.md`

Each major version includes:
- Step-by-step upgrade instructions
- Code examples (before/after)
- Common pitfalls and solutions
- Rollback procedures

## Version Support Lifecycle

### Support Tiers

**Tier 1: Active Development**
- Latest major version (1.x.x)
- Full feature development
- Bug fixes within 48 hours
- Security patches within 24 hours

**Tier 2: Maintenance**
- Previous major version (0.x.x)
- Security fixes only
- Critical bug fixes
- 12 months support after new major release

**Tier 3: End of Life (EOL)**
- Versions older than Tier 2
- Community support only
- No official patches
- Upgrade strongly recommended

### Current Support Status

| Version | Status | Support Until | Notes |
|---------|--------|--------------|-------|
| 1.0.x   | Active Development | Ongoing | Current stable |
| 0.9.x   | Maintenance | 2026-01-11 | Security fixes only |
| 0.8.x and older | EOL | N/A | Upgrade required |

## API Stability Levels

### Level 1: Locked (✅ Fully Stable)
- Will never break within major version
- Examples: CLI commands, make.toml schema
- Changes require major version bump

### Level 2: Stable (✅ Generally Stable)
- Backward compatible within major version
- May add new features
- Examples: Hook system, lifecycle phases

### Level 3: Beta (⚠️ Mostly Stable)
- May have minor breaking changes in minor versions
- Will provide migration path
- Examples: MCP integration, AI features

### Level 4: Experimental (🧪 May Change)
- No compatibility guarantees
- May change significantly
- Examples: New experimental features

## User Guarantees

### What We Guarantee

1. **No Breaking Changes in Patch Releases**
   - 1.0.0 → 1.0.1: Safe upgrade, no changes needed

2. **Backward Compatible Minor Releases**
   - 1.0.x → 1.1.x: Safe upgrade, new features available

3. **Migration Path for Major Releases**
   - 1.x.x → 2.x.x: Documented upgrade, tools provided

4. **State File Forward Compatibility**
   - Newer versions can always read older state files

5. **make.toml Stability**
   - Existing make.toml files work without modification in same major version

### What We Don't Guarantee

1. **Internal Rust API Stability**
   - Library internals may change in any release

2. **Experimental Features**
   - Features marked experimental may change significantly

3. **Performance Characteristics**
   - May optimize algorithms, changing performance

4. **Output Format Details**
   - Terminal output formatting may improve

5. **Error Messages**
   - Error messages may be improved for clarity

## Breaking Change Examples

### ✅ Allowed in Major Versions

```bash
# v1.x: Old command
ggen build --output dist/

# v2.x: New command (BREAKING)
ggen lifecycle run build --output dist/
```

### ❌ Not Allowed in Minor Versions

```bash
# v1.0: Working command
ggen lifecycle run build

# v1.1: Cannot remove this (BREAKING)
# Must wait for v2.0
```

### ✅ Allowed in Minor Versions

```bash
# v1.0: Basic command
ggen lifecycle run build

# v1.1: Added optional flag (NOT BREAKING)
ggen lifecycle run build --parallel
```

## Compatibility Reporting

### How to Report Issues

If you encounter compatibility issues:

1. Open GitHub issue with:
   - Old version number
   - New version number
   - Exact error message
   - Steps to reproduce

2. Label: `compatibility-issue`

3. Expected response time:
   - Critical: 24 hours
   - High: 48 hours
   - Medium: 1 week

## Community Feedback

We welcome feedback on this policy:
- GitHub Discussions: Compatibility category
- RFC process for major policy changes
- Quarterly review of compatibility issues

---

**Questions?** Open a discussion at: https://github.com/seanchatmangpt/ggen/discussions

**Report a compatibility issue**: https://github.com/seanchatmangpt/ggen/issues/new?template=compatibility.md
