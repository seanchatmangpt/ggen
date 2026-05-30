<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Migration Automation - File Index](#migration-automation---file-index)
  - [Quick Navigation](#quick-navigation)
    - [🚀 Get Started](#-get-started)
    - [📚 Documentation](#-documentation)
    - [🛠️ Scripts](#-scripts)
    - [📊 Reports](#-reports)
  - [File Summary](#file-summary)
  - [Usage Paths](#usage-paths)
    - [Path 1: Quick Start (10 minutes)](#path-1-quick-start-10-minutes)
    - [Path 2: Full Understanding (30 minutes)](#path-2-full-understanding-30-minutes)
    - [Path 3: Single Command Testing (5 minutes)](#path-3-single-command-testing-5-minutes)
  - [Directory Structure](#directory-structure)
  - [Command Categories](#command-categories)
    - [HIGH Priority (30 commands)](#high-priority-30-commands)
    - [MEDIUM Priority (25 commands)](#medium-priority-25-commands)
    - [LOW Priority (12 commands)](#low-priority-12-commands)
  - [Key Concepts](#key-concepts)
    - [Pattern Established (10 core commands)](#pattern-established-10-core-commands)
    - [Automation Coverage (67 commands)](#automation-coverage-67-commands)
    - [Validation (7 phases)](#validation-7-phases)
  - [Timeline](#timeline)
  - [Support](#support)
  - [Contributing](#contributing)
  - [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Migration Automation - File Index

## Quick Navigation

### 🚀 Get Started
- [QUICK_REFERENCE.md](QUICK_REFERENCE.md) - TL;DR and quick commands

### 📚 Documentation
- [README.md](README.md) - Complete usage guide
- [MIGRATION_PLAN.md](MIGRATION_PLAN.md) - Detailed strategy
- [AUTOMATION_SUMMARY.md](AUTOMATION_SUMMARY.md) - High-level overview

### 🛠️ Scripts
- [generate_cli_wrapper.sh](generate_cli_wrapper.sh) - Single command generator
- [migrate_remaining_commands.sh](migrate_remaining_commands.sh) - Batch migration
- [validate_migration.sh](validate_migration.sh) - Validation pipeline

### 📊 Reports
- [../docs/v2_migration/AUTOMATION_TEST_REPORT.md](../docs/v2_migration/AUTOMATION_TEST_REPORT.md) - Test results

## File Summary

| File | Lines | Purpose |
|------|-------|---------|
| `generate_cli_wrapper.sh` | 150 | Generate single CLI wrapper |
| `migrate_remaining_commands.sh` | 300 | Batch migrate 67 commands |
| `validate_migration.sh` | 400 | 7-phase validation |
| `README.md` | 600 | Usage guide |
| `MIGRATION_PLAN.md` | 800 | Detailed strategy |
| `QUICK_REFERENCE.md` | 200 | Quick start |
| `AUTOMATION_SUMMARY.md` | 400 | Overview |
| `INDEX.md` | 50 | This file |
| **Total** | **2,900** | **All files** |

## Usage Paths

### Path 1: Quick Start (10 minutes)

```bash
# 1. Read quick reference
cat QUICK_REFERENCE.md

# 2. Run migration
./migrate_remaining_commands.sh

# 3. Validate
./validate_migration.sh --quick
```

### Path 2: Full Understanding (30 minutes)

```bash
# 1. Read README
cat README.md

# 2. Review migration plan
cat MIGRATION_PLAN.md

# 3. Run dry-run
./migrate_remaining_commands.sh --dry-run

# 4. Execute migration
./migrate_remaining_commands.sh

# 5. Full validation
./validate_migration.sh
```

### Path 3: Single Command Testing (5 minutes)

```bash
# Generate one command
./generate_cli_wrapper.sh template show

# Review generated file
cat cli/src/commands/template/show.rs

# Test compilation
cargo check -p ggen-cli
```

## Directory Structure

```
scripts/v2_migration/
├── INDEX.md                          ← This file
├── README.md                         ← Usage guide
├── QUICK_REFERENCE.md                ← Quick start
├── MIGRATION_PLAN.md                 ← Detailed plan
├── AUTOMATION_SUMMARY.md             ← Overview
├── generate_cli_wrapper.sh           ← Single generator
├── migrate_remaining_commands.sh     ← Batch migration
└── validate_migration.sh             ← Validation

docs/v2_migration/
└── AUTOMATION_TEST_REPORT.md         ← Test results

Generated during migration:
scripts/v2_migration/
└── migration_log_YYYYMMDD_HHMMSS.txt ← Migration log
└── validation_log_YYYYMMDD_HHMMSS.txt← Validation log
```

## Command Categories

### HIGH Priority (30 commands)
→ See [MIGRATION_PLAN.md](MIGRATION_PLAN.md#high-priority-30-commands)

### MEDIUM Priority (25 commands)
→ See [MIGRATION_PLAN.md](MIGRATION_PLAN.md#medium-priority-25-commands)

### LOW Priority (12 commands)
→ See [MIGRATION_PLAN.md](MIGRATION_PLAN.md#low-priority-12-commands)

## Key Concepts

### Pattern Established (10 core commands)
- CLI layer with clap Args
- Domain layer with business logic
- Runtime execution wrapper
- Chicago TDD tests

### Automation Coverage (67 commands)
- Template-based generation
- Placeholder domain logic
- Unit test scaffolding
- Priority-based migration

### Validation (7 phases)
- Syntax check
- Compilation
- Unit tests
- Integration tests
- E2E tests
- Architecture review
- Performance benchmarks

## Timeline

| Phase | Duration | Type |
|-------|----------|------|
| Script creation | 2 hours | ✅ Done |
| Migration execution | 10 min | 🤖 Auto |
| Validation | 5 min | 🤖 Auto |
| Domain logic | 8-12 hrs | 👤 Manual |
| Integration tests | 4-6 hrs | 👤 Manual |
| Final validation | 30 min | 🤖 Auto |

## Support

- **Questions**: GitHub Discussions
- **Issues**: GitHub Issues
- **Code review**: GitHub PRs

## Contributing

To improve automation:

1. Edit scripts in `scripts/v2_migration/`
2. Test with dry-run mode
3. Update documentation
4. Submit PR

## License

Same as ggen project

---

**Last updated**: 2025-11-01
**Status**: Ready for execution
**Next**: Wait for sparc-coder to complete 10 core commands
