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

### validate-marketplace-package.sh
End-to-end validation of marketplace packages in a clean Docker environment.

**Usage:**
```bash
# Validate a package
./validate-marketplace-package.sh io.ggen.nextjs.ontology-crud

# Keep container for debugging
./validate-marketplace-package.sh io.ggen.nextjs.ontology-crud --keep-container
```

**What it does:**
1. Creates clean Docker container (node:20-bookworm)
2. Installs system dependencies (git, curl, build-essential)
3. Installs Rust toolchain via rustup
4. Clones and builds ggen from source
5. Installs the specified marketplace package
6. Runs `npm install` and `npm run regenerate`
7. Validates generated files
8. Runs `npm run build` (if available)
9. Reports detailed validation results

**Exit codes:**
- 0: Success
- 1: Invalid arguments
- 2: Docker not available
- 3: Container creation failed
- 4: Build failed
- 5: Package installation failed
- 6: Validation failed

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
