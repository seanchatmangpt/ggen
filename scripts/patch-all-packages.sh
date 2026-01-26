#!/usr/bin/env bash
# Patch all workspace packages
# Updates versions, dependencies, and ensures consistency across all crates

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
log_debug() { echo -e "${BLUE}[DEBUG]${NC} $1"; }
log_step() { echo -e "${CYAN}[STEP]${NC} $1"; }

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Parse arguments
DRY_RUN=false
UPDATE_VERSION=""
UPDATE_DEPENDENCIES=false
VALIDATE_ONLY=false
PATCH_TYPE="all"

while [[ $# -gt 0 ]]; do
  case $1 in
    --version)
      UPDATE_VERSION="$2"
      shift 2
      ;;
    --update-deps)
      UPDATE_DEPENDENCIES=true
      shift
      ;;
    --validate-only)
      VALIDATE_ONLY=true
      shift
      ;;
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --patch-type)
      PATCH_TYPE="$2"
      shift 2
      ;;
    -h|--help)
      echo "Usage: $0 [OPTIONS]"
      echo ""
      echo "Patch all workspace packages (crates)"
      echo ""
      echo "Options:"
      echo "  --version VERSION     Update all package versions to VERSION"
      echo "  --update-deps         Update workspace dependency versions"
      echo "  --validate-only       Only validate, don't make changes"
      echo "  --dry-run             Show what would be done without making changes"
      echo "  --patch-type TYPE     Type of patch: 'version', 'deps', 'all' (default: all)"
      echo "  -h, --help            Show this help message"
      echo ""
      echo "Examples:"
      echo "  $0 --version 5.2.0              # Update all versions to 5.2.0"
      echo "  $0 --update-deps                # Update workspace dependencies"
      echo "  $0 --validate-only              # Validate version consistency"
      echo "  $0 --version 5.2.0 --dry-run   # Preview version update"
      exit 0
      ;;
    *)
      log_error "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Get workspace members from Cargo.toml
get_workspace_members() {
  # Extract members array from workspace section, handling both single-line and multi-line formats
  awk '
    /^\[workspace\]/ { in_workspace=1; next }
    in_workspace && /^\[/ { exit }
    in_workspace && /^members = \[/ { 
      in_members=1
      # Check if closing bracket on same line
      if (/\]/) { 
        in_members=0
        exit
      }
      next
    }
    in_members && /\]/ { exit }
    in_members {
      # Extract quoted strings, handling inline comments
      gsub(/#.*$/, "")  # Remove comments
      while (match($0, /"[^"]+"/)) {
        member = substr($0, RSTART+1, RLENGTH-2)
        gsub(/^[ \t]+|[ \t]+$/, "", member)  # Trim whitespace
        if (member != "") print member
        $0 = substr($0, RSTART+RLENGTH)
      }
    }
  ' "$REPO_ROOT/Cargo.toml" | while read -r member; do
    # Handle both relative paths and full paths
    if [[ "$member" == /* ]]; then
      echo "$member"
    else
      echo "$REPO_ROOT/$member"
    fi
  done
}

# Get current version from root Cargo.toml
get_root_version() {
  grep '^version = ' "$REPO_ROOT/Cargo.toml" | head -1 | sed 's/version = "\(.*\)"/\1/' || echo ""
}

# Update version in a Cargo.toml file
update_version_in_file() {
  local file="$1"
  local new_version="$2"
  
  if [[ ! -f "$file" ]]; then
    log_warn "File not found: $file"
    return 1
  fi
  
  if [[ "$DRY_RUN" == "true" ]]; then
    log_debug "Would update version in $file to $new_version"
    return 0
  fi
  
  # Use sed to update version, handling both quoted and unquoted
  if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS sed
    sed -i '' "s/^version = \".*\"/version = \"$new_version\"/" "$file"
    sed -i '' "s/^version = '.*'/version = \"$new_version\"/" "$file"
  else
    # Linux sed
    sed -i "s/^version = \".*\"/version = \"$new_version\"/" "$file"
    sed -i "s/^version = '.*'/version = \"$new_version\"/" "$file"
  fi
  
  log_info "Updated version in $file"
}

# Update VERSION file
update_version_file() {
  local new_version="$1"
  local version_file="$REPO_ROOT/VERSION"
  
  if [[ "$DRY_RUN" == "true" ]]; then
    log_debug "Would update VERSION file to $new_version"
    return 0
  fi
  
  echo "$new_version" > "$version_file"
  log_info "Updated VERSION file"
}

# Validate version consistency
validate_versions() {
  log_step "Validating version consistency..."
  
  local root_version=$(get_root_version)
  if [[ -z "$root_version" ]]; then
    log_error "Could not determine root version"
    return 1
  fi
  
  log_info "Root version: $root_version"
  
  # Check VERSION file
  if [[ -f "$REPO_ROOT/VERSION" ]]; then
    local version_file=$(cat "$REPO_ROOT/VERSION" | tr -d '[:space:]')
    if [[ "$version_file" != "$root_version" ]]; then
      log_error "VERSION file ($version_file) does not match Cargo.toml ($root_version)"
      return 1
    fi
    log_info "✅ VERSION file matches: $version_file"
  else
    log_warn "VERSION file not found"
  fi
  
  # Check all workspace crates
  local errors=0
  local members=$(get_workspace_members)
  
  for member in $members; do
    local cargo_toml="$member/Cargo.toml"
    if [[ ! -f "$cargo_toml" ]]; then
      log_warn "Cargo.toml not found: $cargo_toml"
      continue
    fi
    
    local crate_version=$(grep '^version = ' "$cargo_toml" | head -1 | sed 's/version = "\(.*\)"/\1/' || echo "")
    if [[ -z "$crate_version" ]]; then
      log_warn "Could not determine version in $cargo_toml"
      continue
    fi
    
    if [[ "$crate_version" != "$root_version" ]]; then
      log_error "Version mismatch in $cargo_toml: $crate_version (expected $root_version)"
      errors=$((errors + 1))
    else
      log_debug "✅ $(basename $member): $crate_version"
    fi
  done
  
  if [[ $errors -gt 0 ]]; then
    log_error "Found $errors version inconsistency(ies)"
    return 1
  fi
  
  log_info "✅ All versions are consistent: $root_version"
  return 0
}

# Update all package versions
update_all_versions() {
  local new_version="$1"
  
  log_step "Updating all package versions to $new_version..."
  
  # Update root Cargo.toml
  log_info "Updating root Cargo.toml..."
  update_version_in_file "$REPO_ROOT/Cargo.toml" "$new_version"
  
  # Update VERSION file
  if [[ -f "$REPO_ROOT/VERSION" ]]; then
    log_info "Updating VERSION file..."
    update_version_file "$new_version"
  fi
  
  # Update all workspace crates
  local members=$(get_workspace_members)
  local updated=0
  
  for member in $members; do
    local cargo_toml="$member/Cargo.toml"
    if [[ -f "$cargo_toml" ]]; then
      update_version_in_file "$cargo_toml" "$new_version"
      updated=$((updated + 1))
    fi
  done
  
  log_info "Updated versions in $updated crates"
  
  # Validate after update
  if [[ "$DRY_RUN" == "false" ]]; then
    log_info "Validating updated versions..."
    if validate_versions; then
      log_info "✅ All versions updated and validated"
    else
      log_error "Version validation failed after update"
      return 1
    fi
  fi
}

# Update workspace dependencies
update_workspace_dependencies() {
  log_step "Updating workspace dependencies..."
  
  if [[ "$DRY_RUN" == "true" ]]; then
    log_debug "Would update workspace dependencies"
    return 0
  fi
  
  # This would update workspace.dependencies in root Cargo.toml
  # For now, just log that it would be done
  log_info "Workspace dependency updates would be applied here"
  log_warn "Manual dependency updates may be needed"
}

# Main execution
main() {
  log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  log_info "Patch All Packages Script"
  log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  log_info ""
  
  if [[ "$DRY_RUN" == "true" ]]; then
    log_warn "DRY RUN MODE - No changes will be made"
    log_info ""
  fi
  
  # Validate only mode
  if [[ "$VALIDATE_ONLY" == "true" ]]; then
    if validate_versions; then
      log_info "✅ Validation passed"
      exit 0
    else
      log_error "❌ Validation failed"
      exit 1
    fi
  fi
  
  # Version update
  if [[ -n "$UPDATE_VERSION" ]]; then
    if [[ "$PATCH_TYPE" == "version" || "$PATCH_TYPE" == "all" ]]; then
      update_all_versions "$UPDATE_VERSION"
    fi
  fi
  
  # Dependency update
  if [[ "$UPDATE_DEPENDENCIES" == "true" ]]; then
    if [[ "$PATCH_TYPE" == "deps" || "$PATCH_TYPE" == "all" ]]; then
      update_workspace_dependencies
    fi
  fi
  
  # If no specific action, just validate
  if [[ -z "$UPDATE_VERSION" ]] && [[ "$UPDATE_DEPENDENCIES" == "false" ]]; then
    log_info "No update specified, running validation..."
    if validate_versions; then
      log_info "✅ Validation passed"
      exit 0
    else
      log_error "❌ Validation failed"
      exit 1
    fi
  fi
  
  log_info ""
  log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  log_info "✅ Patch complete!"
  log_info ""
  
  if [[ "$DRY_RUN" == "true" ]]; then
    log_warn "This was a dry-run. No changes were made."
    log_info "Run without --dry-run to apply changes."
  else
    log_info "Next steps:"
    log_info "  1. Review changes: git diff"
    log_info "  2. Test build: cargo make check"
    log_info "  3. Commit changes: git commit -m 'chore: update all package versions to X.Y.Z'"
  fi
  log_info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
}

# Run main
main "$@"

