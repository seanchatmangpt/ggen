# Marketplace Testing and Validation

This document describes how to test the ggen marketplace functionality end-to-end in an isolated environment.

## Overview

The marketplace testing workflow validates the complete package lifecycle:
1. **Create** marketplace packages with proper structure
2. **Publish** packages to a local registry
3. **Install** packages from the registry
4. **Validate** installed packages work correctly
5. **Simulate** package usage

## Quick Start

```bash
# Run the complete marketplace test
./scripts/test-marketplace.sh

# Run test and preserve the test directory for inspection
KEEP_TEST_DIR=1 ./scripts/test-marketplace.sh
```

## Test Structure

### Created Packages

The test creates three realistic marketplace packages:

#### 1. ggen-cli-command-pattern (templates)
**Purpose**: Template for generating new CLI commands with proper structure

**Features**:
- Clap argument parsing
- Production-ready error handling
- Comprehensive test generation
- Support for commands with subcommands

**Usage Example**:
```bash
ggen template generate ggen-cli-command-pattern \
  --command_name "market verify" \
  --module_name "market/verify" \
  --description "Verify marketplace package signatures"
```

**Generated Files**:
- `cli/src/cmds/{module_path}.rs` - Command implementation
- `tests/{module_name}_test.rs` - Integration tests
- `docs/commands/{command_name}.md` - Documentation

#### 2. ggen-lifecycle-phase (templates)
**Purpose**: Template for creating new lifecycle phases

**Features**:
- Dependency management
- Both bash and rust script types
- Watch mode support
- Proper error handling

**Usage Example**:
```bash
ggen template generate ggen-lifecycle-phase \
  --phase_name "security-scan" \
  --description "Run security vulnerability scanning" \
  --dependencies "build,test" \
  --script_type "bash"
```

**Generated Files**:
- `make.toml` entries for the new phase

#### 3. ggen-error-handling (patterns)
**Purpose**: Production-ready error handling patterns

**Features**:
- No `.expect()` or `.unwrap()` in production code
- Proper `Result<T>` types
- Custom error types with `thiserror`
- Comprehensive error messages

**Usage Example**:
```bash
ggen template generate ggen-error-handling \
  --module_name "marketplace" \
  --error_types "PackageNotFound,InvalidSignature,NetworkError"
```

**Generated Files**:
- `{module_name}/error.rs` - Error type definitions

## Test Workflow

### Step 1: Environment Setup
```bash
TEST_DIR="/tmp/ggen-marketplace-test-$$"
REGISTRY_DIR="$TEST_DIR/registry"
INSTALL_DIR="$TEST_DIR/installed"
PACKAGES_DIR="$TEST_DIR/packages"
```

Creates isolated test environment with:
- **Registry**: Local marketplace registry
- **Installed**: Package installation directory
- **Packages**: Source packages before publication

### Step 2: Package Creation

Each package includes:
- `package.toml` - Package metadata (name, version, category, dependencies)
- `template.tmpl` - Template content with variable substitution
- `README.md` - Usage documentation
- `CHANGELOG.md` - Version history (optional)

Example `package.toml` structure:
```toml
[package]
name = "ggen-cli-command-pattern"
version = "0.1.0"
description = "Template for creating new ggen CLI commands"
category = "templates"
author = "ggen-team"
license = "MIT"

[template]
type = "cli-command"
language = "rust"

[variables]
command_name = { type = "string", required = true }
module_name = { type = "string", required = true }
description = { type = "string", required = true }
```

### Step 3: Registry Initialization

Creates a local marketplace registry with:
- `registry.toml` - Registry configuration
- `packages/` - Published packages directory
- `index.json` - Package index for discovery

Example `registry.toml`:
```toml
[registry]
name = "test-registry"
version = "1.0.0"
base_url = "file:///tmp/ggen-marketplace-test-123/registry"

[metadata]
created = "2025-01-11T12:34:56Z"
description = "Local test registry for ggen marketplace"
```

### Step 4: Package Publishing

For each package:
1. Copy package directory to registry
2. Create package metadata JSON
3. Update registry index

Package metadata example:
```json
{
  "name": "ggen-cli-command-pattern",
  "version": "0.1.0",
  "published_at": "2025-01-11T12:34:56Z",
  "downloads": 0,
  "verified": true
}
```

### Step 5: Package Listing

Registry index structure:
```json
{
  "registry": "test-registry",
  "version": "1.0.0",
  "packages": [
    {
      "name": "ggen-cli-command-pattern",
      "version": "0.1.0",
      "category": "templates",
      "description": "Template for creating new ggen CLI commands",
      "path": "packages/ggen-cli-command-pattern"
    }
  ],
  "updated_at": "2025-01-11T12:34:56Z"
}
```

### Step 6: Package Installation

For each package:
1. Create installation directory
2. Copy package files from registry
3. Create installation metadata

Installation metadata:
```json
{
  "package": "ggen-cli-command-pattern",
  "version": "0.1.0",
  "installed_at": "2025-01-11T12:34:56Z",
  "registry": "file:///tmp/ggen-marketplace-test-123/registry"
}
```

### Step 7: Package Validation

Validates each installed package:
- ✅ Required files present (`package.toml`, `README.md`, `template.tmpl`)
- ✅ Package metadata structure correct
- ✅ All required fields in `package.toml`

### Step 8: Usage Simulation

Simulates actual template generation:
```bash
# Simulates: ggen template generate ggen-cli-command-pattern
#   --command_name "market verify"
#   --module_name "market/verify"
#   --description "Verify marketplace package signatures"

Would generate:
  ✓ cli/src/cmds/market/verify.rs
  ✓ tests/market_verify_test.rs
  ✓ docs/commands/market-verify.md
```

## Test Output

### Success Output
```
========================================
Ggen Marketplace Dogfooding Test
========================================

Step 1: Creating test environment
✓ Test directories created

Step 2: Creating test packages
Creating package: ggen-cli-command-pattern...
✓ ggen-cli-command-pattern created
Creating package: ggen-lifecycle-phase...
✓ ggen-lifecycle-phase created
Creating package: ggen-error-handling...
✓ ggen-error-handling created

Step 3: Initializing local marketplace registry
✓ Registry initialized

Step 4: Testing marketplace operations
Publishing packages to local registry...
  📦 Publishing ggen-cli-command-pattern...
  ✓ Published ggen-cli-command-pattern
  📦 Publishing ggen-lifecycle-phase...
  ✓ Published ggen-lifecycle-phase
  📦 Publishing ggen-error-handling...
  ✓ Published ggen-error-handling

Updating registry index...
✓ Registry index updated

Step 5: Listing available packages
Available packages in test registry:

  📦 ggen-cli-command-pattern v0.1.0 (templates)
     Template for creating new ggen CLI commands

  📦 ggen-lifecycle-phase v0.1.0 (templates)
     Template for creating new lifecycle phases

  📦 ggen-error-handling v0.1.0 (patterns)
     Production-ready error handling patterns

Step 6: Installing packages
Installing ggen-cli-command-pattern...
✓ Installed ggen-cli-command-pattern
Installing ggen-lifecycle-phase...
✓ Installed ggen-lifecycle-phase
Installing ggen-error-handling...
✓ Installed ggen-error-handling

Step 7: Validating installed packages
Validating ggen-cli-command-pattern...
  ✓ package.toml
  ✓ README.md
  ✓ template.tmpl
  ✓ package.toml structure valid

Validating ggen-lifecycle-phase...
  ✓ package.toml
  ✓ README.md
  ✓ template.tmpl
  ✓ package.toml structure valid

Validating ggen-error-handling...
  ✓ package.toml
  ✓ README.md
  ✓ template.tmpl
  ✓ package.toml structure valid

Step 8: Simulating package usage
Testing ggen-cli-command-pattern template...
Would generate:
  ✓ cli/src/cmds/market/verify.rs
  ✓ tests/market_verify_test.rs
  ✓ docs/commands/market-verify.md

========================================
Test Summary
========================================

✓ All tests passed!

Results:
  ✓ Created 3 test packages
  ✓ Initialized local registry
  ✓ Published packages to registry
  ✓ Installed packages successfully
  ✓ Validated package structure
  ✓ Simulated package usage

Test environment location:
  Registry: /tmp/ggen-marketplace-test-63097/registry
  Packages: /tmp/ggen-marketplace-test-63097/installed
```

## Directory Structure

```
/tmp/ggen-marketplace-test-XXXXX/
├── registry/                         # Local marketplace registry
│   ├── registry.toml                # Registry configuration
│   ├── index.json                   # Package index
│   └── packages/                    # Published packages
│       ├── ggen-cli-command-pattern/
│       │   ├── package.toml
│       │   ├── template.tmpl
│       │   ├── README.md
│       │   ├── CHANGELOG.md
│       │   └── metadata.json
│       ├── ggen-lifecycle-phase/
│       │   ├── package.toml
│       │   ├── template.tmpl
│       │   └── README.md
│       └── ggen-error-handling/
│           ├── package.toml
│           ├── template.tmpl
│           └── README.md
├── installed/                        # Installed packages
│   ├── ggen-cli-command-pattern/
│   │   ├── .installed               # Installation metadata
│   │   ├── package.toml
│   │   ├── template.tmpl
│   │   ├── README.md
│   │   └── CHANGELOG.md
│   ├── ggen-lifecycle-phase/
│   │   ├── .installed
│   │   ├── package.toml
│   │   ├── template.tmpl
│   │   └── README.md
│   └── ggen-error-handling/
│       ├── .installed
│       ├── package.toml
│       ├── template.tmpl
│       └── README.md
└── packages/                         # Source packages (before publishing)
    ├── ggen-cli-command-pattern/
    ├── ggen-lifecycle-phase/
    └── ggen-error-handling/
```

## Inspecting Test Results

```bash
# Preserve test directory
KEEP_TEST_DIR=1 ./scripts/test-marketplace.sh

# Find test directory
TEST_DIR=$(ls -td /tmp/ggen-marketplace-test-* | head -1)
echo "Test directory: $TEST_DIR"

# Explore registry
cat "$TEST_DIR/registry/index.json"
ls -la "$TEST_DIR/registry/packages/"

# Explore installed packages
ls -la "$TEST_DIR/installed/"
cat "$TEST_DIR/installed/ggen-cli-command-pattern/.installed"

# View package contents
cat "$TEST_DIR/installed/ggen-cli-command-pattern/package.toml"
cat "$TEST_DIR/installed/ggen-cli-command-pattern/template.tmpl"
```

## Integration with CI/CD

Add to `.github/workflows/marketplace-test.yml`:

```yaml
name: Marketplace Test
on: [push, pull_request]

jobs:
  marketplace-validation:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Run Marketplace Test
        run: ./scripts/test-marketplace.sh

      - name: Upload Test Artifacts
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: marketplace-test-results
          path: /tmp/ggen-marketplace-test-*/
```

## Extending the Test

### Adding New Test Packages

1. Add package creation in Step 2:
```bash
cd "$PACKAGES_DIR"
mkdir -p new-package-name
cd new-package-name

cat > package.toml << 'EOF'
[package]
name = "new-package-name"
# ... package metadata
EOF

cat > template.tmpl << 'EOF'
# ... template content
EOF

cat > README.md << 'EOF'
# ... documentation
EOF
```

2. Update package list in Steps 5-7:
```bash
for package in "package1" "package2" "new-package-name"; do
    # Install/validate new package
done
```

### Adding Validation Rules

Add custom validation in Step 7:
```bash
# Validate template syntax
if ! ggen template validate "$INSTALL_DIR/$package/template.tmpl"; then
    echo "✗ Invalid template syntax"
    validation_passed=false
fi

# Validate README links
if ! markdown-link-check "$INSTALL_DIR/$package/README.md"; then
    echo "✗ Broken links in README"
    validation_passed=false
fi
```

## Success Metrics

- ✅ All packages created successfully
- ✅ Registry initialization complete
- ✅ All packages published to registry
- ✅ All packages installed correctly
- ✅ Package structure validation passes
- ✅ Usage simulation demonstrates functionality

## Troubleshooting

### Test Fails on Package Validation

Check if required files are present:
```bash
ls -la /tmp/ggen-marketplace-test-*/installed/package-name/
```

### Registry Index Not Created

Verify registry directory permissions:
```bash
ls -la /tmp/ggen-marketplace-test-*/registry/
```

### Installation Fails

Check registry package structure:
```bash
ls -la /tmp/ggen-marketplace-test-*/registry/packages/
```

## Next Steps

1. **Implement Real Commands**: Convert simulated commands to actual ggen implementations
2. **Add Remote Registry**: Test with remote marketplace registry
3. **Performance Testing**: Measure package installation speed
4. **Security Validation**: Add signature verification tests
5. **Version Management**: Test package upgrades and downgrades

## Related Documentation

- [Phase 1: Basic Dogfooding](../01-basic-dogfooding.md) - Marketplace self-hosting
- [Marketplace Guide](../../marketplace.md) - Complete marketplace documentation
- [Template System](../../templates.md) - Template creation and usage

---

**This test validates the complete marketplace workflow and serves as a foundation for real marketplace implementation.**
