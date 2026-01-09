# Upgrading to ggen v6.0.0

ggen v6.0.0 represents a significant expansion with 8 new crates (44% growth), PaaS capabilities, AI-native workflows, and enhanced error-proofing. This guide helps you migrate from v5.1.0 smoothly.

## Table of Contents

- [What's New vs. Breaking Changes](#whats-new-vs-breaking-changes)
- [For CLI Users (90%)](#for-cli-users-90-of-users)
- [For Library Users (10%)](#for-library-users-10-of-users)
- [For Workspace Users](#for-workspace-users)
- [Configuration Changes](#configuration-changes)
- [Risk Assessment](#risk-assessment)
- [Step-by-Step Instructions](#step-by-step-upgrade-instructions)
- [Troubleshooting](#troubleshooting)
- [Rollback Procedure](#rollback-procedure)
- [FAQ](#faq)

---

## What's New vs. Breaking Changes

| Category | v5.1.0 | v6.0.0 | Impact |
|----------|--------|--------|--------|
| **Active Crates** | 18 crates | 26 crates | +44% growth |
| **New Features** | Core generation | + PaaS, AI, KNHK | Major expansion |
| **CLI Commands** | 35 verbs | +8 PaaS verbs | Feature-flagged |
| **Build Time (Full)** | 30-45s | 38-55s | +20-30% |
| **Build Time (Incremental)** | <5s | <5s | No change ✅ |
| **Breaking Changes** | N/A | Library imports only | CLI unchanged ✅ |

### New Capabilities Table

| Feature | Description | Status | Enable With |
|---------|-------------|--------|-------------|
| **ggen-ai** | GPT-4/Claude integration | ✅ Stable | `--features ai` |
| **ggen-paas** | Docker/K8s/Terraform gen | ✅ Stable | `--features paas` |
| **ggen-dod** | MAPE-K governance | ✅ Stable | Default |
| **ggen-spec-validator** | RDF validation | ✅ Stable | Default |
| **knhk-etl** | ETL pipeline | ✅ Stable | Default |
| **knhk-hot** | C FFI optimization | ⚠️ Experimental | Default |
| **knhk-lockchain** | Provenance tracking | ✅ Stable | Default |
| **knhk-otel** | OpenTelemetry | ✅ Stable | Default |

---

## For CLI Users (90% of Users)

### ✅ No Action Required

**Zero breaking changes** to CLI syntax. All v5.1.0 commands work identically in v6.0.0.

```bash
# These work exactly as before - no changes needed
ggen sync
ggen template generate --template hello.tera
ggen marketplace search rust
ggen graph query schema.ttl
```

### Optional: Enable New Features

```bash
# Option 1: Core features only (fastest, same as v5.1.0)
cargo install ggen-cli

# Option 2: Add PaaS support (Docker, K8s, Terraform generation)
cargo install ggen-cli --features paas

# Option 3: Add AI support (GPT-4, Claude integration)
cargo install ggen-cli --features ai

# Option 4: All features enabled
cargo install ggen-cli --features full

# Verify installation
ggen --version  # Should show: ggen 6.0.0
```

### New Commands Available (Opt-In)

**With `--features paas`**:
```bash
ggen paas generate-docker schema/    # → Dockerfile + docker-compose.yml
ggen paas generate-k8s schema/       # → deployment.yaml, service.yaml
ggen paas generate-terraform schema/ # → main.tf, variables.tf
ggen paas validate --spec .specify/  # Validate infrastructure specs
```

**With `--features ai`**:
```bash
ggen ai create "Blog with posts and comments"  # → RDF ontology
ggen ai generate --description "REST API controller"
ggen ai sparql --description "Find all required properties"
ggen ai refactor --code src/main.rs --focus performance
```

### Example Workflow (CLI users)

```bash
# 1. Install v6.0.0
cargo install ggen-cli --force

# 2. Your existing projects work unchanged
cd my-project
ggen sync  # Works identically to v5.1.0

# 3. Try new features (optional)
cargo install ggen-cli --features ai,paas --force
ggen ai create "E-commerce system"  # New capability
ggen paas generate-all schema/      # New capability
```

---

## For Library Users (10% of Users)

### Breaking Changes: Import Paths Only

v6.0.0 reorganizes internal modules for clarity. Update your imports:

**Before (v5.1.0)**:
```rust
// ❌ Old import paths (deprecated in v6)
use ggen_core::types::{ProtectedPath, PathProtectionError};
use ggen_core::validation::Validator;
use ggen_domain::marketplace::Client;
```

**After (v6.0.0)**:
```rust
// ✅ New import paths (v6.0.0+)
use ggen_core::protection::{ProtectedPath, PathProtectionError};
use ggen_core::validation::rules::Validator;
use ggen_domain::marketplace::client::Client;
```

### Module Reorganization Table

| Old Path (v5.1.0) | New Path (v6.0.0) | Reason |
|-------------------|-------------------|--------|
| `ggen_core::types` | `ggen_core::protection` | Clearer domain separation |
| `ggen_core::validation` | `ggen_core::validation::rules` | Better module organization |
| `ggen_domain::marketplace` | `ggen_domain::marketplace::client` | Explicit module structure |

### Automated Migration Script

Save as `migrate-to-v6.sh`:

```bash
#!/bin/bash
# Automated v5.1.0 → v6.0.0 import path migration

echo "Starting ggen v6.0.0 migration..."

# Backup all Rust files
find . -type f -name "*.rs" -exec cp {} {}.bak \;

# Apply import path transformations
find . -type f -name "*.rs" -exec sed -i \
  -e 's/ggen_core::types::/ggen_core::protection::/g' \
  -e 's/ggen_core::validation::/ggen_core::validation::rules::/g' \
  -e 's/ggen_domain::marketplace::/ggen_domain::marketplace::client::/g' \
  {} \;

echo "✅ Migration complete. Backup files saved with .bak extension."
echo "Next steps:"
echo "  1. cargo check   # Verify no compilation errors"
echo "  2. cargo test    # Run your test suite"
echo "  3. rm **/*.bak   # Remove backups if all looks good"
```

```bash
chmod +x migrate-to-v6.sh
./migrate-to-v6.sh
cargo check  # Verify no errors
```

### New Poka-Yoke Features (Optional)

v6.0.0 introduces type-safe error prevention patterns you can adopt:

```rust
use ggen_core::protection::ProtectedPath;

// v6.0.0: Type-safe path protection (prevents accidental overwrites)
let protected = ProtectedPath::new("/src/generated/models.rs")?;
protected.validate_write()?;  // Compiler-enforced safety check

// v6.0.0: Enhanced error context with automatic tracing
use ggen_core::error::GgenError;

fn generate() -> Result<(), GgenError> {
    ggen_core::generate(config)
        .map_err(|e| GgenError::Generation {
            context: "Failed to generate models".into(),
            source: e,
        })
}
```

---

## For Workspace Users

### New Crates Added (8 total)

If you depend on ggen as a workspace, you now have access to:

```toml
[dependencies]
# Existing crates (unchanged)
ggen-core = "6.0.0"
ggen-domain = "6.0.0"

# NEW in v6.0.0 - Definition of Done
ggen-dod = "6.0.0"  # MAPE-K governance, observability

# NEW in v6.0.0 - Specification validation
ggen-spec-validator = "6.0.0"  # RDF validation

# NEW in v6.0.0 - KNHK Systems (6 crates)
knhk-etl = "0.1.0"          # ETL pipeline
knhk-hot = "1.0.0"          # C FFI optimization
knhk-connectors = "0.1.0"   # Kafka/HTTP connectors
knhk-lockchain = "0.1.0"    # Merkle-linked receipts
knhk-otel = "0.1.0"         # OpenTelemetry
knhk-orchestrator = "0.1.0" # ETL → KGC-4D bridge
```

### Build Time Impact

| Build Type | v5.1.0 | v6.0.0 | Change | Mitigation |
|------------|--------|--------|--------|------------|
| **Full build** (clean) | 30-45s | 38-55s | +20-30% | One-time cost |
| **Incremental build** | <5s | <5s | No change | ✅ Default |
| **cargo make check** | <5s | <5s | No change | ✅ Fast validation |
| **Test suite** | <30s | <35s | +15% | Acceptable |

**Recommendation**: Use incremental builds (default) and `cargo make check` for fast feedback.

### Workspace Configuration

No changes needed to your root `Cargo.toml`:

```toml
[workspace]
members = [
    "crates/*",  # Your existing crates work unchanged
]

[dependencies]
ggen = "6.0.0"  # Just update version - everything else compatible
```

---

## Configuration Changes

### ggen.toml (No Changes Required)

Your existing `ggen.toml` works without modification:

```toml
[project]
name = "my-project"
version = "1.0.0"

[ontology]
source = "schema/"  # ✅ Unchanged

[generation]
output_dir = "src/generated"  # ✅ Unchanged
```

### ggen-paas.toml (New, Optional)

Only needed if using `--features paas`:

```toml
[paas]
platform = "kubernetes"
namespace = "production"

[deployment]
strategy = "rolling"
replicas = 3

[infrastructure]
terraform_version = "1.5+"
provider = "aws"
region = "us-east-1"
```

### Feature Flags Reference

| Flag | Features Enabled | Use Case |
|------|------------------|----------|
| `default` | Core generation only | Most users, fastest builds |
| `paas` | + Docker/K8s/Terraform | Infrastructure generation |
| `ai` | + GPT-4/Claude | AI-assisted development |
| `full` | All features | Advanced users |

```bash
# Examples
cargo install ggen-cli                      # Core only
cargo install ggen-cli --features paas      # + Infrastructure
cargo install ggen-cli --features ai,paas   # + AI + Infrastructure
cargo install ggen-cli --features full      # Everything
```

---

## Risk Assessment

| User Type | Risk Level | Action Required | Rollback Time | Notes |
|-----------|------------|-----------------|---------------|-------|
| **CLI Users** | 🟢 Low | None | N/A | Zero breaking changes |
| **Library Users** | 🟡 Moderate | Update imports | <1 hour | Automated script provided |
| **Workspace Users** | 🟢 Low | Update version | <5 minutes | Backward compatible |

### Detailed Risk Analysis

**🟢 Low Risk for CLI Users**:
- All v5.1.0 commands work identically
- New features are opt-in (feature flags)
- No workflow changes required
- Can upgrade without testing (but testing recommended)

**🟡 Moderate Risk for Library Users**:
- Import path updates required (automated script available)
- Compiler will catch all issues (type-safe migration)
- Estimated migration time: 30-60 minutes
- Rollback is simple (revert commit)

---

## Step-by-Step Upgrade Instructions

### For CLI Users

```bash
# Step 1: Backup current installation (optional but recommended)
which ggen        # Note current path
ggen --version    # Note current version

# Step 2: Install v6.0.0
cargo install ggen-cli --force

# Step 3: Verify installation
ggen --version  # Should show: ggen 6.0.0

# Step 4: Test existing workflows
cd your-project
ggen sync  # Should work identically to v5.1.0

# Step 5 (Optional): Enable new features
cargo install ggen-cli --features ai,paas --force

# ✅ Done! No further action needed.
```

### For Library Users

```bash
# Step 1: Update Cargo.toml
sed -i 's/ggen = "5.1"/ggen = "6.0"/g' Cargo.toml
sed -i 's/ggen-core = "5.1"/ggen-core = "6.0"/g' Cargo.toml
sed -i 's/ggen-domain = "5.1"/ggen-domain = "6.0"/g' Cargo.toml

# Step 2: Run automated migration script
./migrate-to-v6.sh  # See script above

# Step 3: Verify compilation
cargo check

# Step 4: Fix any remaining import errors
# (Compiler will show exact locations with suggestions)
cargo build

# Step 5: Run tests
cargo test

# Step 6: Commit changes
git add -A
git commit -m "chore: Migrate to ggen v6.0.0"

# ✅ Migration complete!
```

---

## Troubleshooting

### Issue: "Cannot find ProtectedPath in ggen_core::types"

**Cause**: Using old v5.1.0 import path.

**Fix**:
```rust
// ❌ Old (v5.1.0)
use ggen_core::types::ProtectedPath;

// ✅ New (v6.0.0)
use ggen_core::protection::ProtectedPath;
```

### Issue: "Feature 'paas' not enabled"

**Cause**: Trying to use PaaS commands without feature flag.

**Fix**:
```bash
cargo install ggen-cli --features paas --force
```

Or edit `Cargo.toml`:
```toml
[dependencies]
ggen-cli = { version = "6.0", features = ["paas"] }
```

### Issue: "Build time increased significantly"

**Cause**: Full builds now include 8 additional crates.

**Fix**: Use incremental builds (default) and `cargo make check` for fast validation:
```bash
cargo make check      # <5s (unchanged)
cargo make test-unit  # <16s (slightly slower)
```

**Explanation**: Full clean builds are rare. Day-to-day incremental builds remain <5s.

### Issue: "ggen-paas.toml not found"

**Cause**: PaaS features expect configuration file (but it's optional).

**Fix**: Either:
1. Create `ggen-paas.toml` (see template above)
2. Use default settings (no config needed for basic usage)
3. Disable PaaS feature if not needed

### Issue: "Type mismatch after migration"

**Cause**: Incomplete import path updates.

**Fix**:
```bash
# Run cargo check to see all type errors
cargo check

# The compiler will show exact locations and suggest fixes
# Example output:
# error[E0433]: failed to resolve: use of undeclared type `ProtectedPath`
#   --> src/main.rs:10:5
#    |
# 10 |     use ggen_core::types::ProtectedPath;
#    |                   ^^^^^ use of undeclared type
#    |
# help: consider importing this struct
#    |
# 10 |     use ggen_core::protection::ProtectedPath;
```

---

## Rollback Procedure

If you need to revert to v5.1.0:

### CLI Users

```bash
# Uninstall v6.0.0
cargo uninstall ggen-cli

# Reinstall v5.1.0
cargo install ggen-cli --version 5.1.0

# Verify
ggen --version  # Should show: ggen 5.1.0
```

### Library Users

```bash
# Option A: Git revert (if committed)
git revert HEAD

# Option B: Manual rollback
# 1. Revert Cargo.toml
sed -i 's/ggen = "6.0"/ggen = "5.1"/g' Cargo.toml

# 2. Restore backed-up source files
find . -name "*.rs.bak" -exec bash -c 'mv "$0" "${0%.bak}"' {} \;

# 3. Verify
cargo check
cargo clean
```

---

## FAQ

**Q: Do I need to update if I only use the CLI?**

A: No. v6.0.0 is 100% backward compatible for CLI users. Update at your convenience to access new features like PaaS and AI generation.

**Q: Will my existing templates work?**

A: Yes. All Tera templates work unchanged. No template syntax changes in v6.0.0.

**Q: What's the actual performance impact?**

A:
- **Incremental builds**: No change (<5s)
- **Full clean builds**: +20-30% due to 8 new crates (rare occurrence)
- **Daily development**: No noticeable difference

**Q: Can I use v6 features without updating my code?**

A: Yes. New features are opt-in via feature flags (`--features paas,ai`). Your existing code works unchanged.

**Q: How do I know if I'm using library features?**

A: Check your `Cargo.toml`. If it includes `ggen-core`, `ggen-domain`, or other ggen crates as dependencies, you're a library user and need to update imports.

**Q: What if the automated migration script doesn't work?**

A: Run `cargo check` - the Rust compiler will show all remaining import errors with precise locations and fix suggestions. The compiler is your guide.

**Q: Is v5.1.0 still supported?**

A: Yes. v5.1.0 receives security fixes until **Q2 2026**. End-of-life: **Q3 2026**.

**Q: Can I mix v5 and v6 crates?**

A: No. All ggen crates must be on the same major version. Use `cargo tree | grep ggen` to audit your dependencies.

**Q: Do I need to update ggen.toml?**

A: No. Your existing `ggen.toml` works unchanged. `ggen-paas.toml` is only needed if using PaaS features.

**Q: What are the benefits of upgrading?**

A:
- **AI-native workflows**: Generate code from natural language
- **Infrastructure generation**: Docker/K8s/Terraform from RDF
- **Poka-Yoke error prevention**: Manufacturing-grade quality gates
- **Enhanced observability**: MAPE-K governance and provenance tracking
- **Future-proof**: v6.0.0 is the foundation for upcoming features

---

## Detailed Upgrade Documentation

For comprehensive migration guides and feature documentation:

- **CLI Migration**: [docs/upgrade/cli-v5-to-v6.md](docs/upgrade/cli-v5-to-v6.md)
- **Library Migration**: [docs/upgrade/library-v5-to-v6.md](docs/upgrade/library-v5-to-v6.md)
- **PaaS Features Guide**: [docs/features/paas.md](docs/features/paas.md)
- **AI Features Guide**: [crates/ggen-ai/README.md](crates/ggen-ai/README.md)
- **Poka-Yoke Patterns**: [docs/patterns/poka-yoke.md](docs/patterns/poka-yoke.md)
- **KNHK Systems**: [docs/architecture/knhk-integration.md](docs/architecture/knhk-integration.md)
- **v6.0.0 Release Notes**: [CHANGELOG.md#600](CHANGELOG.md#600)
- **Full v6 Documentation**: [docs/INDEX.md](docs/INDEX.md)

---

## Quick Reference

### Version Comparison

| Aspect | v5.1.0 | v6.0.0 |
|--------|--------|--------|
| Crates | 18 | 26 (+44%) |
| CLI Commands | 35 | 43 (+8 PaaS) |
| Features | Core only | Core + AI + PaaS + KNHK |
| Build Time | 30-45s | 38-55s (+20%) |
| Incremental | <5s | <5s (unchanged) |

### Migration Checklist

- [ ] Read this guide completely
- [ ] Backup current installation/code
- [ ] For CLI: `cargo install ggen-cli --force`
- [ ] For Library: Run migration script
- [ ] Test with `ggen sync` on existing project
- [ ] Run `cargo check` (library users)
- [ ] Run test suite
- [ ] Commit changes
- [ ] Update CI/CD if needed
- [ ] Document any custom changes

---

**Ready to upgrade?** Start with the [Step-by-Step Instructions](#step-by-step-upgrade-instructions) for your user type.
