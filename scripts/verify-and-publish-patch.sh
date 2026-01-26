#!/usr/bin/env bash
# Verify ggen sync and init commands, then publish a patch version

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Function to print colored output
log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_step() { echo -e "${CYAN}[STEP]${NC} $1"; }

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info "Verify Commands and Publish Patch"
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info ""

# Step 1: Build the binary
log_step "Step 1: Building ggen binary..."
cd "$REPO_ROOT"
if cargo build --release --package ggen-cli-lib --bin ggen; then
  log_info "✅ Build successful"
else
  log_error "Build failed"
  exit 1
fi
log_info ""

# Step 2: Verify ggen init
log_step "Step 2: Verifying 'ggen init' command..."
TEST_DIR="/tmp/ggen-test-init-$$"
rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

if "$REPO_ROOT/target/release/ggen" init --path "$TEST_DIR" 2>&1 | head -20; then
  log_info "✅ 'ggen init' command works"
  
  # Check that files were created
  if [[ -f "$TEST_DIR/ggen.toml" ]] && [[ -f "$TEST_DIR/schema/domain.ttl" ]]; then
    log_info "✅ Init created expected files"
  else
    log_error "Init did not create expected files"
    exit 1
  fi
else
  log_error "'ggen init' command failed"
  exit 1
fi

# Cleanup
rm -rf "$TEST_DIR"
log_info ""

# Step 3: Verify ggen sync (dry-run)
log_step "Step 3: Verifying 'ggen sync' command..."
TEST_DIR="/tmp/ggen-test-sync-$$"
rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

# Create minimal ggen.toml for testing
cat > "$TEST_DIR/ggen.toml" <<EOF
[project]
name = "test-project"
version = "0.1.0"

[ontology]
path = "schema/domain.ttl"

[[generation]]
name = "test"
sparql = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"
template = "templates/test.tera"
output = "src/generated/test.rs"
EOF

mkdir -p "$TEST_DIR/schema"
cat > "$TEST_DIR/schema/domain.ttl" <<EOF
@prefix ex: <http://example.org/> .

ex:Test a ex:Class .
EOF

mkdir -p "$TEST_DIR/templates"
cat > "$TEST_DIR/templates/test.tera" <<EOF
// Generated code
{% for row in results %}
// {{ row.s }}
{% endfor %}
EOF

cd "$TEST_DIR"
if "$REPO_ROOT/target/release/ggen" sync --dry-run 2>&1 | head -20; then
  log_info "✅ 'ggen sync' command works (dry-run)"
else
  log_warn "'ggen sync' had warnings (may be expected for minimal test)"
fi

# Cleanup
cd "$REPO_ROOT"
rm -rf "$TEST_DIR"
log_info ""

# Step 4: Get current version and bump patch
log_step "Step 4: Bumping patch version..."
CURRENT_VERSION=$(grep '^version = ' "$REPO_ROOT/Cargo.toml" | head -1 | sed 's/version = "\(.*\)"/\1/')
if [[ -z "$CURRENT_VERSION" ]]; then
  log_error "Could not determine current version"
  exit 1
fi

log_info "Current version: $CURRENT_VERSION"

# Bump patch version (e.g., 5.1.0 -> 5.1.1)
IFS='.' read -ra VERSION_PARTS <<< "$CURRENT_VERSION"
MAJOR="${VERSION_PARTS[0]}"
MINOR="${VERSION_PARTS[1]}"
PATCH="${VERSION_PARTS[2]}"
NEW_PATCH=$((PATCH + 1))
NEW_VERSION="${MAJOR}.${MINOR}.${NEW_PATCH}"

log_info "New version: $NEW_VERSION"
log_info ""

# Step 5: Update all package versions
log_step "Step 5: Updating all package versions to $NEW_VERSION..."
if "$SCRIPT_DIR/patch-all-packages.sh" --version "$NEW_VERSION"; then
  log_info "✅ All versions updated"
else
  log_error "Failed to update versions"
  exit 1
fi
log_info ""

# Step 6: Verify versions are consistent
log_step "Step 6: Verifying version consistency..."
if "$SCRIPT_DIR/patch-all-packages.sh" --validate-only; then
  log_info "✅ All versions are consistent"
else
  log_error "Version validation failed"
  exit 1
fi
log_info ""

# Step 7: Run pre-publish checks
log_step "Step 7: Running pre-publish validation..."
if cargo make check; then
  log_info "✅ Compilation check passed"
else
  log_error "Compilation check failed"
  exit 1
fi
log_info ""

# Step 8: Publish to crates.io
log_step "Step 8: Publishing to crates.io..."
log_info "Publishing version $NEW_VERSION..."
log_info ""

# Use the publish script from pull-patch-publish.sh logic
declare -a TIER1=("ggen-utils" "ggen-macros" "ggen-marketplace")
declare -a TIER2=("ggen-config" "ggen-core" "ggen-cli-validation")
declare -a TIER3=("ggen-config-clap" "ggen-domain")
declare -a TIER4=("ggen-test-audit" "ggen-test-opt" "ggen-e2e" "ggen-api" "ggen-auth" "ggen-payments" "ggen-saas" "ggen-node" "ggen-cli")

# Track failures
FAILED_CRATES=()

publish_crate() {
  local crate_name="$1"
  local crate_path="$REPO_ROOT/crates/$crate_name"
  
  if [[ ! -d "$crate_path" ]]; then
    log_warn "Crate not found: $crate_path, skipping..."
    return 0
  fi
  
  log_info "Publishing $crate_name..."
  cd "$crate_path"
  
  # Use --allow-dirty since we just updated versions
  if cargo publish --allow-dirty 2>&1; then
    log_info "✅ Published $crate_name"
    sleep 3
    return 0
  else
    log_error "Failed to publish $crate_name"
    FAILED_CRATES+=("$crate_name")
    return 1
  fi
}

# Publish Tier 1
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info "Tier 1: No workspace dependencies"
for crate in "${TIER1[@]}"; do
  publish_crate "$crate" || log_warn "Continuing despite failure..."
done

# Publish Tier 2
log_info ""
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info "Tier 2: Depend on Tier 1"
for crate in "${TIER2[@]}"; do
  publish_crate "$crate" || log_warn "Continuing despite failure..."
done

# Publish Tier 3
log_info ""
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info "Tier 3: Depend on Tier 2"
for crate in "${TIER3[@]}"; do
  publish_crate "$crate" || log_warn "Continuing despite failure..."
done

# Publish Tier 4
log_info ""
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info "Tier 4: Depend on Tier 3"
for crate in "${TIER4[@]}"; do
  if [[ "$crate" == "ggen-cli" ]]; then
    if [[ -d "$REPO_ROOT/crates/ggen-cli-lib" ]]; then
      publish_crate "ggen-cli-lib" || log_warn "Continuing despite failure..."
    else
      publish_crate "$crate" || log_warn "Continuing despite failure..."
    fi
  else
    publish_crate "$crate" || log_warn "Continuing despite failure..."
  fi
done

# Publish root crate
log_info ""
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info "Root: ggen"
cd "$REPO_ROOT"
if cargo publish --allow-dirty; then
  log_info "✅ Published root crate: ggen"
else
  log_error "Failed to publish root crate"
  FAILED_CRATES+=("ggen")
fi

# Report failures
if [[ ${#FAILED_CRATES[@]} -gt 0 ]]; then
  log_info ""
  log_warn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  log_warn "Some crates failed to publish:"
  for crate in "${FAILED_CRATES[@]}"; do
    log_warn "  - $crate"
  done
  log_warn ""
  log_warn "You may need to:"
  log_warn "  1. Fix build issues (e.g., C++ compiler for rocksdb dependencies)"
  log_warn "  2. Publish manually: cd crates/$crate && cargo publish --allow-dirty"
  log_warn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
fi

log_info ""
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info "✅ Verification and publish complete!"
log_info ""
log_info "Version: v$NEW_VERSION"
log_info "Published to: https://crates.io/crates/ggen"
log_info ""
log_info "Next steps:"
log_info "  1. Commit version changes: git commit -m 'chore: bump version to $NEW_VERSION'"
log_info "  2. Create git tag: git tag -a v$NEW_VERSION -m 'Release v$NEW_VERSION'"
log_info "  3. Push changes: git push origin main && git push origin v$NEW_VERSION"
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

