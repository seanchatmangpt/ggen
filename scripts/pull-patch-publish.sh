#!/usr/bin/env bash
# Pull remote changes, patch all packages, and publish to crates.io
# End-to-end release workflow

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

# Parse arguments
SKIP_PULL=false
SKIP_PATCH=false
SKIP_PUBLISH=false
DRY_RUN=false

while [[ $# -gt 0 ]]; do
  case $1 in
    --skip-pull)
      SKIP_PULL=true
      shift
      ;;
    --skip-patch)
      SKIP_PATCH=true
      shift
      ;;
    --skip-publish)
      SKIP_PUBLISH=true
      shift
      ;;
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    -h|--help)
      echo "Usage: $0 [OPTIONS]"
      echo ""
      echo "Pull, patch, and publish workflow"
      echo ""
      echo "Options:"
      echo "  --skip-pull      Skip pulling from remote"
      echo "  --skip-patch     Skip patching packages"
      echo "  --skip-publish   Skip publishing to crates.io"
      echo "  --dry-run        Show what would be done without making changes"
      echo "  -h, --help       Show this help message"
      echo ""
      exit 0
      ;;
    *)
      log_error "Unknown option: $1"
      exit 1
      ;;
  esac
done

log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info "Pull, Patch, and Publish Workflow"
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info ""

# Step 1: Pull from remote
if [[ "$SKIP_PULL" == "false" ]]; then
  log_step "Step 1: Pulling from remote..."
  cd "$REPO_ROOT"
  
  if [[ "$DRY_RUN" == "true" ]]; then
    log_info "Would run: git pull"
  else
    if git pull; then
      log_info "✅ Pulled from remote"
    else
      log_error "Failed to pull from remote"
      exit 1
    fi
  fi
  log_info ""
else
  log_info "Skipping pull (--skip-pull flag)"
  log_info ""
fi

# Step 2: Patch all packages
if [[ "$SKIP_PATCH" == "false" ]]; then
  log_step "Step 2: Patching all packages..."
  
  # First validate
  log_info "Validating current state..."
  if [[ "$DRY_RUN" == "true" ]]; then
    log_info "Would run: ./scripts/patch-all-packages.sh --validate-only"
  else
    if "$SCRIPT_DIR/patch-all-packages.sh" --validate-only; then
      log_info "✅ All versions are consistent"
    else
      log_warn "Version inconsistencies found, but continuing..."
    fi
  fi
  
  log_info ""
else
  log_info "Skipping patch (--skip-patch flag)"
  log_info ""
fi

# Step 3: Publish to crates.io
if [[ "$SKIP_PUBLISH" == "false" ]]; then
  log_step "Step 3: Publishing to crates.io..."
  
  # Get current version
  CURRENT_VERSION=$(grep '^version = ' "$REPO_ROOT/Cargo.toml" | head -1 | sed 's/version = "\(.*\)"/\1/' || echo "")
  if [[ -z "$CURRENT_VERSION" ]]; then
    log_error "Could not determine version"
    exit 1
  fi
  
  log_info "Current version: $CURRENT_VERSION"
  log_info ""
  
  if [[ "$DRY_RUN" == "true" ]]; then
    log_info "Would publish all crates to crates.io"
    log_info "Publish order:"
    log_info "  Tier 1: ggen-utils, ggen-macros, ggen-marketplace"
    log_info "  Tier 2: ggen-config, ggen-core, ggen-cli-validation"
    log_info "  Tier 3: ggen-config-clap, ggen-domain"
    log_info "  Tier 4: ggen-cli-lib, ggen-api, ggen-auth, ggen-payments, ggen-saas, ggen-test-audit, ggen-test-opt, ggen-e2e, ggen-node"
    log_info "  Root: ggen"
  else
    log_info "Publishing crates in dependency order..."
    log_info ""
    
    # Define publish order (from previous successful publish)
    declare -a TIER1=("ggen-utils" "ggen-macros" "ggen-marketplace")
    declare -a TIER2=("ggen-config" "ggen-core" "ggen-cli-validation")
    declare -a TIER3=("ggen-config-clap" "ggen-domain")
    declare -a TIER4=("ggen-test-audit" "ggen-test-opt" "ggen-e2e" "ggen-api" "ggen-auth" "ggen-payments" "ggen-saas" "ggen-node" "ggen-cli")
    
    # Function to publish a crate
    publish_crate() {
      local crate_name="$1"
      local crate_path="$REPO_ROOT/crates/$crate_name"
      
      if [[ ! -d "$crate_path" ]]; then
        log_warn "Crate not found: $crate_path, skipping..."
        return 0
      fi
      
      log_info "Publishing $crate_name..."
      cd "$crate_path"
      
      if cargo publish; then
        log_info "✅ Published $crate_name"
        # Small delay to allow crates.io to propagate
        sleep 3
        return 0
      else
        log_error "Failed to publish $crate_name"
        return 1
      fi
    }
    
    # Publish Tier 1
    log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    log_info "Tier 1: No workspace dependencies"
    for crate in "${TIER1[@]}"; do
      if ! publish_crate "$crate"; then
        log_error "Failed to publish Tier 1 crate: $crate"
        exit 1
      fi
    done
    
    # Publish Tier 2
    log_info ""
    log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    log_info "Tier 2: Depend on Tier 1"
    for crate in "${TIER2[@]}"; do
      if ! publish_crate "$crate"; then
        log_error "Failed to publish Tier 2 crate: $crate"
        exit 1
      fi
    done
    
    # Publish Tier 3
    log_info ""
    log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    log_info "Tier 3: Depend on Tier 2"
    for crate in "${TIER3[@]}"; do
      if ! publish_crate "$crate"; then
        log_error "Failed to publish Tier 3 crate: $crate"
        exit 1
      fi
    done
    
    # Publish Tier 4
    log_info ""
    log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    log_info "Tier 4: Depend on Tier 3"
    for crate in "${TIER4[@]}"; do
      # Handle ggen-cli which might be ggen-cli-lib
      if [[ "$crate" == "ggen-cli" ]]; then
        if [[ -d "$REPO_ROOT/crates/ggen-cli-lib" ]]; then
          if ! publish_crate "ggen-cli-lib"; then
            log_error "Failed to publish Tier 4 crate: ggen-cli-lib"
            exit 1
          fi
        else
          if ! publish_crate "$crate"; then
            log_error "Failed to publish Tier 4 crate: $crate"
            exit 1
          fi
        fi
      else
        if ! publish_crate "$crate"; then
          log_error "Failed to publish Tier 4 crate: $crate"
          exit 1
        fi
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
      exit 1
    fi
    
    log_info ""
    log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    log_info "✅ All crates published successfully!"
    log_info ""
    log_info "Version: v$CURRENT_VERSION"
    log_info "Published to: https://crates.io/crates/ggen"
  fi
else
  log_info "Skipping publish (--skip-publish flag)"
fi

log_info ""
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log_info "✅ Workflow complete!"
log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

