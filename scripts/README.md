# Ggen Scripts - Dogfooding Tools

This directory contains scripts for using ggen to improve ggen itself.

## Quick Start

```bash
# Check for panic points
./check-no-panic-points.sh

# Fix panic points automatically
cargo script fix-panic-points.rs --dry-run
cargo script fix-panic-points.rs

# Install git hooks
ggen lifecycle run setup-git-hooks
```

## Scripts

### check-no-panic-points.sh
Scans production code for `.expect()` and `.unwrap()` calls.

**Usage:**
```bash
./check-no-panic-points.sh
```

**Exit codes:**
- 0: No panic points found
- 1: Panic points found

### fix-panic-points.rs
Automatically fixes panic points in Rust code.

**Usage:**
```bash
# Dry run (preview fixes)
cargo script fix-panic-points.rs --dry-run

# Apply fixes
cargo script fix-panic-points.rs

# Fix specific directories
cargo script fix-panic-points.rs cli/src ggen-core/src
```

**Features:**
- Replaces `.expect()` with `.map_err()`
- Replaces `.unwrap()` with safe alternatives
- Preserves safe patterns
- Detailed reporting

## Documentation

See `docs/DOGFOODING_GUIDE.md` for complete usage guide.
