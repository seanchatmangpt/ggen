# Phase 1: Basic Dogfooding

> **Priority**: **High** | **Timeline**: Weeks 1-4 | **Complexity**: Low

Focus on immediate, high-value dogfooding activities that require minimal infrastructure but provide maximum development value.

## 🎯 Objectives

1. Replace all direct cargo commands with `ggen lifecycle`
2. Self-host ggen patterns in marketplace
3. Generate repetitive code from templates
4. Integrate lifecycle into CI/CD pipeline

## 📋 Implementation Tasks

### Task 1: Lifecycle Integration (Week 1)

**Goal**: Replace all cargo commands with ggen lifecycle commands.

#### 1.1 Create Lifecycle Configuration

Create `make.toml` for ggen development:

```bash
cd /path/to/ggen
cat > make.toml << 'EOF'
[tasks.init]
description = "Initialize ggen development environment"
script = '''
echo "Initializing ggen development environment..."
rustup component add clippy rustfmt
cargo install cargo-audit
echo "✓ Development environment ready"
'''

[tasks.build]
description = "Build ggen CLI"
command = "cargo"
args = ["build", "--release"]

[tasks.test]
description = "Run comprehensive test suite"
script = '''
echo "Running unit tests..."
cargo test --lib

echo "Running integration tests..."
cargo test --test '*'

echo "Running doc tests..."
cargo test --doc
'''

[tasks.lint]
description = "Run clippy and formatting checks"
script = '''
echo "Running clippy..."
cargo clippy --all-targets --all-features -- -D warnings

echo "Checking formatting..."
cargo fmt -- --check
'''

[tasks.security]
description = "Run security audit"
command = "cargo"
args = ["audit"]

[tasks.validate]
description = "Validate production readiness"
dependencies = ["lint", "test", "security"]
script = '''
echo "Running production readiness validation..."
./scripts/check-no-panic-points.sh
echo "✓ Production readiness validation complete"
'''

[tasks.deploy]
description = "Publish to crates.io"
script = '''
if [ "$DEPLOY_ENV" != "production" ]; then
    echo "❌ Can only deploy to production"
    exit 1
fi

echo "Building release..."
cargo build --release

echo "Running final validation..."
cargo test --release

echo "Publishing to crates.io..."
cargo publish
'''
EOF
```

#### 1.2 Replace CI/CD Commands

Update `.github/workflows/ci.yml`:

```yaml
name: CI
on: [push, pull_request]

jobs:
  build-and-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          profile: minimal
          override: true

      - name: Install ggen
        run: cargo install --path .

      # ✅ Use ggen lifecycle instead of direct cargo commands
      - name: Initialize Environment
        run: ggen lifecycle run init

      - name: Build
        run: ggen lifecycle run build

      - name: Run Tests
        run: ggen lifecycle run test

      - name: Lint
        run: ggen lifecycle run lint

      - name: Security Audit
        run: ggen lifecycle run security

      - name: Validate Production Readiness
        run: ggen lifecycle run validate

      - name: Deploy to Production
        if: github.ref == 'refs/heads/master'
        env:
          DEPLOY_ENV: production
          CARGO_REGISTRY_TOKEN: ${{ secrets.CARGO_REGISTRY_TOKEN }}
        run: ggen lifecycle run deploy
```

#### 1.3 Update Documentation

Update `README.md` and `docs/development.md` to reflect lifecycle usage:

```markdown
## Development Workflow

### Build ggen
```bash
# ❌ Old way
cargo build --release

# ✅ New way (dogfooding)
ggen lifecycle run build
```

### Run Tests
```bash
# ❌ Old way
cargo test

# ✅ New way (dogfooding)
ggen lifecycle run test
```

### Deploy
```bash
# ❌ Old way
cargo publish

# ✅ New way (dogfooding)
DEPLOY_ENV=production ggen lifecycle run deploy
```
```

**Success Criteria**:
- [ ] `make.toml` created with all lifecycle phases
- [ ] CI/CD updated to use lifecycle commands
- [ ] Documentation updated
- [ ] All developers using lifecycle commands

---

### Task 2: Marketplace Self-Hosting (Week 2)

**Goal**: Publish ggen's own patterns as reusable marketplace packages.

#### 2.1 Create CLI Command Pattern Package

Create template for generating new CLI commands:

```bash
mkdir -p marketplace-packages/ggen-cli-command-pattern
cd marketplace-packages/ggen-cli-command-pattern

cat > package.toml << 'EOF'
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
command_name = { type = "string", description = "Command name (e.g., 'market verify')" }
module_name = { type = "string", description = "Rust module name (e.g., 'market/verify')" }
description = { type = "string", description = "Command description" }
EOF

cat > template.tmpl << 'EOF'
// cli/src/cmds/{{module_path}}.rs
use anyhow::Result;
use clap::Args;

/// {{description}}
#[derive(Debug, Args)]
pub struct {{command_struct}} {
    // Add command-specific arguments here
}

impl {{command_struct}} {
    pub fn run(&self) -> Result<()> {
        // TODO: Implement command logic
        println!("Running {{command_name}}...");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_{{test_name}}() -> Result<()> {
        let cmd = {{command_struct}} {};
        cmd.run()?;
        Ok(())
    }
}
EOF

cat > README.md << 'EOF'
# Ggen CLI Command Pattern

Template for generating new ggen CLI commands with proper structure.

## Usage

```bash
ggen market add "ggen-cli-command-pattern"

ggen template generate ggen-cli-command-pattern \
  --command_name "market verify" \
  --module_name "market/verify" \
  --description "Verify marketplace package signatures"
```

## Generated Files

- `cli/src/cmds/{module_path}.rs` - Command implementation
- `tests/{module_name}_test.rs` - Integration tests
- `docs/commands/{command_name}.md` - Command documentation
EOF
```

#### 2.2 Create Lifecycle Phase Pattern Package

```bash
mkdir -p marketplace-packages/ggen-lifecycle-phase
cd marketplace-packages/ggen-lifecycle-phase

cat > package.toml << 'EOF'
[package]
name = "ggen-lifecycle-phase"
version = "0.1.0"
description = "Template for creating new lifecycle phases"
category = "templates"
author = "ggen-team"
license = "MIT"

[template]
type = "lifecycle-phase"
language = "toml"

[variables]
phase_name = { type = "string", description = "Phase name (e.g., 'security-scan')" }
description = { type = "string", description = "Phase description" }
dependencies = { type = "array", description = "Dependent phases", default = [] }
EOF

cat > template.tmpl << 'EOF'
[tasks.{{phase_name}}]
description = "{{description}}"
{{#if dependencies}}
dependencies = [{{#each dependencies}}"{{this}}"{{#unless @last}}, {{/unless}}{{/each}}]
{{/if}}
script = '''
echo "Running {{phase_name}}..."
# TODO: Add phase implementation
'''
EOF
```

#### 2.3 Create Marketplace Package Pattern

```bash
mkdir -p marketplace-packages/ggen-marketplace-package
cd marketplace-packages/ggen-marketplace-package

cat > package.toml << 'EOF'
[package]
name = "ggen-marketplace-package"
version = "0.1.0"
description = "Template for creating new marketplace packages"
category = "templates"
author = "ggen-team"
license = "MIT"

[template]
type = "marketplace-package"
language = "toml"

[variables]
package_name = { type = "string", description = "Package name" }
description = { type = "string", description = "Package description" }
category = { type = "string", description = "Package category" }
template_type = { type = "string", description = "Template type" }
EOF
```

#### 2.4 Publish to Marketplace

```bash
# Publish CLI command pattern
cd marketplace-packages/ggen-cli-command-pattern
ggen market publish

# Publish lifecycle phase pattern
cd ../ggen-lifecycle-phase
ggen market publish

# Publish marketplace package pattern
cd ../ggen-marketplace-package
ggen market publish
```

**Success Criteria**:
- [ ] 3+ ggen patterns published to marketplace
- [ ] Templates tested with actual ggen development
- [ ] Documentation updated with marketplace examples
- [ ] At least 1 external user adopts a package

---

### Task 3: Template-Driven Command Generation (Week 3)

**Goal**: Generate new CLI commands from templates instead of manual coding.

#### 3.1 Generate New Command Using Template

Example: Adding `ggen market verify` command

```bash
# Install the pattern from marketplace
ggen market add "ggen-cli-command-pattern"

# Generate the command
ggen template generate ggen-cli-command-pattern \
  --command_name "market verify" \
  --module_name "market/verify" \
  --description "Verify marketplace package signatures"

# This generates:
# - cli/src/cmds/market/verify.rs
# - tests/market_verify_test.rs
# - docs/commands/market-verify.md
```

#### 3.2 Integrate Generated Command

Update `cli/src/cmds/market/mod.rs`:

```rust
// Add generated module
pub mod verify;

// Add to command enum
#[derive(Debug, Subcommand)]
pub enum MarketCommand {
    // ... existing commands ...

    /// Verify marketplace package signatures
    Verify(verify::Verify),
}

// Add to match statement
pub fn run(&self) -> Result<()> {
    match self {
        // ... existing matches ...
        MarketCommand::Verify(cmd) => cmd.run(),
    }
}
```

#### 3.3 Implement Command Logic

Edit the generated `cli/src/cmds/market/verify.rs`:

```rust
impl Verify {
    pub fn run(&self) -> Result<()> {
        println!("Verifying package signatures...");

        // Implement actual verification logic
        let registry = MarketplaceRegistry::load()?;

        for package in registry.packages() {
            let is_valid = package.verify_signature()
                .map_err(|e| anyhow::anyhow!("Failed to verify {}: {}", package.name(), e))?;

            if is_valid {
                println!("✓ {} - signature valid", package.name());
            } else {
                println!("✗ {} - signature invalid", package.name());
            }
        }

        Ok(())
    }
}
```

#### 3.4 Test Generated Command

```bash
# Run tests
ggen lifecycle run test

# Test command manually
ggen market verify

# Expected output:
# Verifying package signatures...
# ✓ ggen-cli-command-pattern - signature valid
# ✓ ggen-lifecycle-phase - signature valid
# ✓ rig-mcp-integration - signature valid
```

**Success Criteria**:
- [ ] New CLI command generated in <15 minutes
- [ ] Generated code follows ggen conventions
- [ ] Tests pass without modification
- [ ] Documentation generated automatically

---

### Task 4: CI/CD Lifecycle Integration (Week 4)

**Goal**: Complete CI/CD pipeline using only ggen lifecycle commands.

#### 4.1 Update GitHub Actions Workflow

Create `.github/workflows/dogfooding-ci.yml`:

```yaml
name: Dogfooding CI
on:
  push:
    branches: [master, develop]
  pull_request:
    branches: [master]

env:
  RUST_BACKTRACE: 1
  CARGO_TERM_COLOR: always

jobs:
  lifecycle-validation:
    name: Lifecycle Validation
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          profile: minimal
          components: clippy, rustfmt
          override: true

      - name: Cache cargo registry
        uses: actions/cache@v3
        with:
          path: ~/.cargo/registry
          key: ${{ runner.os }}-cargo-registry-${{ hashFiles('**/Cargo.lock') }}

      - name: Cache cargo build
        uses: actions/cache@v3
        with:
          path: target
          key: ${{ runner.os }}-cargo-build-${{ hashFiles('**/Cargo.lock') }}

      - name: Install ggen
        run: cargo install --path . --force

      # Phase 1: Initialize
      - name: Lifecycle Init
        run: ggen lifecycle run init

      # Phase 2: Build
      - name: Lifecycle Build
        run: ggen lifecycle run build

      # Phase 3: Test
      - name: Lifecycle Test
        run: ggen lifecycle run test

      # Phase 4: Lint
      - name: Lifecycle Lint
        run: ggen lifecycle run lint

      # Phase 5: Security
      - name: Lifecycle Security
        run: ggen lifecycle run security
        continue-on-error: true

      # Phase 6: Validate Production Readiness
      - name: Lifecycle Validate
        run: ggen lifecycle run validate

      - name: Upload Test Results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: test-results
          path: target/test-results/

  marketplace-validation:
    name: Marketplace Package Validation
    runs-on: ubuntu-latest
    needs: lifecycle-validation
    if: github.ref == 'refs/heads/master'
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install --path . --force

      - name: Validate Marketplace Packages
        run: |
          cd marketplace-packages
          for package in */; do
            echo "Validating $package"
            cd "$package"
            ggen market validate
            cd ..
          done

  deploy:
    name: Deploy to Production
    runs-on: ubuntu-latest
    needs: [lifecycle-validation, marketplace-validation]
    if: github.ref == 'refs/heads/master' && github.event_name == 'push'
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install --path . --force

      - name: Deploy
        env:
          DEPLOY_ENV: production
          CARGO_REGISTRY_TOKEN: ${{ secrets.CARGO_REGISTRY_TOKEN }}
        run: ggen lifecycle run deploy
```

#### 4.2 Add Pre-commit Hooks

Create `.githooks/pre-commit`:

```bash
#!/bin/bash
set -e

echo "Running pre-commit validation..."

# Use ggen lifecycle for validation
ggen lifecycle run lint
ggen lifecycle run test

echo "✓ Pre-commit validation passed"
```

Install hooks:

```bash
git config core.hooksPath .githooks
chmod +x .githooks/pre-commit
```

#### 4.3 Create Development Scripts

Create `scripts/dev.sh`:

```bash
#!/bin/bash
# Development workflow using ggen lifecycle

set -e

case "$1" in
  start)
    echo "Starting ggen development environment..."
    ggen lifecycle run init
    ;;

  build)
    ggen lifecycle run build
    ;;

  test)
    ggen lifecycle run test
    ;;

  watch)
    cargo watch -x "test" -x "clippy"
    ;;

  validate)
    ggen lifecycle run validate
    ;;

  *)
    echo "Usage: $0 {start|build|test|watch|validate}"
    exit 1
    ;;
esac
```

**Success Criteria**:
- [ ] CI/CD uses only ggen lifecycle commands
- [ ] All tests pass in CI
- [ ] Pre-commit hooks use lifecycle
- [ ] Development scripts use lifecycle

---

## 📊 Phase 1 Success Metrics

### Adoption Metrics
- [ ] 100% of cargo commands replaced with `ggen lifecycle`
- [ ] 3+ marketplace packages published
- [ ] 5+ CLI commands generated from templates
- [ ] CI/CD fully integrated with lifecycle

### Quality Metrics
- [ ] All tests pass in CI
- [ ] No direct cargo usage in scripts
- [ ] Documentation reflects lifecycle usage
- [ ] Pre-commit hooks prevent direct cargo usage

### Efficiency Metrics
- [ ] New CLI command creation time: <15 minutes (from 60+ minutes)
- [ ] CI/CD pipeline execution time: <10 minutes
- [ ] Development setup time: <5 minutes

---

## 🎯 Next Steps

After completing Phase 1:

1. **Measure Impact**: Track time savings and quality improvements
2. **Gather Feedback**: Survey team on dogfooding experience
3. **Document Learnings**: Update templates based on usage
4. **Plan Phase 2**: Begin RDF modeling and AI integration

**Continue to**: [Phase 2: Advanced Dogfooding](02-advanced-dogfooding.md)
