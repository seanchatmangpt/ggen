# Marketplace CLI Commands

## Overview

The ggen marketplace CLI provides commands for publishing, installing, searching, and managing gpack packages.

## Commands

### ggen marketplace publish

Publish a package to crates.io as a gpack.

```bash
ggen marketplace publish [PATH] [OPTIONS]

Arguments:
  PATH    Path to package directory (default: current dir)

Options:
  --dry-run       Validate without publishing
  --no-verify     Skip FMEA validation (NOT RECOMMENDED)
  --token TOKEN   Crates.io API token (or set CARGO_REGISTRY_TOKEN)
  -v, --verbose   Verbose output

Examples:
  # Publish current directory
  ggen marketplace publish

  # Dry run to validate
  ggen marketplace publish --dry-run

  # Publish specific package
  ggen marketplace publish /path/to/my-package
```

### ggen marketplace install

Install a package from crates.io.

```bash
ggen marketplace install [PACKAGE] [OPTIONS]

Arguments:
  PACKAGE    Package name (with or without -gpack suffix)

Options:
  --version VER   Version constraint (default: latest)
  --offline       Install from cache only
  --force         Skip FMEA warnings (NOT RECOMMENDED)
  --no-cache      Bypass cache, always download
  -v, --verbose   Verbose output

Examples:
  # Install latest version
  ggen marketplace install chatman-gpack

  # Install specific version
  ggen marketplace install chatman-gpack@1.2.3

  # Install with version constraint
  ggen marketplace install chatman-gpack@^1.0

  # Offline install from cache
  ggen marketplace install chatman-gpack --offline
```

### ggen marketplace search

Search for packages in the marketplace.

```bash
ggen marketplace search [QUERY] [OPTIONS]

Arguments:
  QUERY    Search term (supports partial matching)

Options:
  --min-quality TIER   Filter by tier (gold|silver|bronze)
  --fmea-only          Only show FMEA-validated packages
  --category CAT       Filter by category
  --author AUTHOR      Filter by author
  --limit N            Max results (default: 20)
  --json               Output as JSON
  --sort FIELD         Sort by: quality, downloads, name, date

Examples:
  # Basic search
  ggen marketplace search bibliography

  # Search Gold-tier packages only
  ggen marketplace search bibliography --min-quality gold

  # Search FMEA-validated packages
  ggen marketplace search bibliography --fmea-only

  # JSON output for scripting
  ggen marketplace search bibliography --json
```

### ggen marketplace list

List installed packages.

```bash
ggen marketplace list [OPTIONS]

Options:
  --outdated    Show only packages with available updates
  --json        Output as JSON
  --path        Show installation paths
  --tiers       Show quality tier for each package

Examples:
  # List all installed packages
  ggen marketplace list

  # Show only outdated packages
  ggen marketplace list --outdated

  # JSON output
  ggen marketplace list --json
```

### ggen marketplace update

Update installed packages.

```bash
ggen marketplace update [PACKAGE] [OPTIONS]

Arguments:
  PACKAGE    Package name (omit for all packages)

Options:
  --dry-run     Show what would be updated without updating
  --force       Force update even if FMEA warnings
  -v, --verbose Verbose output

Examples:
  # Update all packages
  ggen marketplace update

  # Update specific package
  ggen marketplace update chatman-gpack

  # Dry run to see what would update
  ggen marketplace update --dry-run
```

### ggen marketplace validate

Validate a package before publishing.

```bash
ggen marketplace validate [PATH] [OPTIONS]

Arguments:
  PATH    Path to package directory (default: current dir)

Options:
  --fmea          Run full FMEA validation
  --strict        Fail on warnings
  -v, --verbose   Verbose output

Examples:
  # Validate current package
  ggen marketplace validate

  # Validate with FMEA
  ggen marketplace validate --fmea

  # Strict mode
  ggen marketplace validate --strict
```

## Quality Tiers

Packages are classified into quality tiers based on:

| Tier | Criteria | Symbol |
|------|----------|--------|
| Gold | FMEA passed + 100+ downloads + <30 days old | 🥇 |
| Silver | FMEA passed + 10-100 downloads OR 30-90 days old | 🥈 |
| Bronze | Basic validation + any downloads | 🥉 |
| Unrated | New package, not yet rated | ○ |

## Environment Variables

| Variable | Description |
|----------|-------------|
| `CARGO_REGISTRY_TOKEN` | Crates.io API token for publishing |
| `GGEN_CACHE_DIR` | Override default cache directory |
| `GGEN_OFFLINE` | Set to "1" for offline mode |

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Invalid arguments |
| 3 | Network error |
| 4 | Validation failed |
| 5 | FMEA blocked |
| 6 | Package not found |

## Examples

### Publishing Workflow

```bash
# 1. Validate package
ggen marketplace validate --fmea

# 2. Dry run publish
ggen marketplace publish --dry-run

# 3. Publish to crates.io
ggen marketplace publish
```

### Installation Workflow

```bash
# 1. Search for packages
ggen marketplace search bibliography --min-quality gold

# 2. Install package
ggen marketplace install academic-bibliography-manager-gpack

# 3. Verify installation
ggen marketplace list
```

### Update Workflow

```bash
# 1. Check for updates
ggen marketplace list --outdated

# 2. Update all
ggen marketplace update

# Or update specific package
ggen marketplace update chatman-gpack
```
