#!/bin/bash
# archive-files.sh - Archive files to .archive/ directory with manifest
#
# Usage: ./archive-files.sh <file-list> [options]
# Arguments:
#   file-list             Path to file containing list of files to archive (one per line)
# Options:
#   --dry-run             Show what would be archived without making changes
#   --no-cargo-update     Don't update Cargo.toml
#   --no-import-cleanup   Don't remove import statements
#   -h, --help            Show this help message

set -euo pipefail

# Default values
FILE_LIST=""
DRY_RUN=false
UPDATE_CARGO=true
CLEANUP_IMPORTS=true
REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || echo ".")
ARCHIVE_DIR="${REPO_ROOT}/.archive"
MANIFEST_FILE="${ARCHIVE_DIR}/MANIFEST.md"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Helper functions
log_info() {
  echo -e "${GREEN}[INFO]${NC} $1" >&2
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $1" >&2
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $1" >&2
}

log_debug() {
  echo -e "${BLUE}[DEBUG]${NC} $1" >&2
}

show_help() {
  sed -n '2,13p' "$0"
  exit 0
}

# Parse arguments
if [[ $# -eq 0 ]]; then
  show_help
fi

FILE_LIST="$1"
shift

while [[ $# -gt 0 ]]; do
  case $1 in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --no-cargo-update)
      UPDATE_CARGO=false
      shift
      ;;
    --no-import-cleanup)
      CLEANUP_IMPORTS=false
      shift
      ;;
    -h|--help)
      show_help
      ;;
    *)
      log_error "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Verify inputs
if [[ ! -f "$FILE_LIST" ]]; then
  log_error "File list not found: $FILE_LIST"
  exit 1
fi

if ! git rev-parse --git-dir >/dev/null 2>&1; then
  log_error "Not a git repository"
  exit 1
fi

log_info "Starting archival process..."
log_info "Repository root: $REPO_ROOT"
log_info "Archive directory: $ARCHIVE_DIR"
log_info "File list: $FILE_LIST"
[[ $DRY_RUN == true ]] && log_warn "DRY RUN MODE - no changes will be made"

# Create archive directory
if [[ ! -d "$ARCHIVE_DIR" ]]; then
  if [[ $DRY_RUN == false ]]; then
    mkdir -p "$ARCHIVE_DIR"
    log_info "Created archive directory: $ARCHIVE_DIR"
  else
    log_debug "Would create: $ARCHIVE_DIR"
  fi
fi

# Initialize manifest
if [[ $DRY_RUN == false ]]; then
  {
    echo "# Archive Manifest"
    echo ""
    echo "**Created**: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "**Reason**: Archival of files unmodified 7+ days or marked deprecated"
    echo ""
    echo "## Recovery"
    echo ""
    echo "To restore archived files:"
    echo "\`\`\`bash"
    echo "git log --follow -- .archive/"
    echo "git show <commit>:.archive/<path> > <original-path>"
    echo "\`\`\`"
    echo ""
    echo "## Archived Files"
    echo ""
  } > "$MANIFEST_FILE"
fi

# Process file list
TOTAL_FILES=0
ARCHIVED_FILES=0
FAILED_FILES=0
declare -a FAILED_LIST=()

# Count total files for progress reporting
TOTAL_FILE_COUNT=$(grep -cv "^#\|^$" "$FILE_LIST" 2>/dev/null || echo 1)

while IFS= read -r FILE; do
  # Skip empty lines and comments
  [[ -z "$FILE" || "$FILE" =~ ^# ]] && continue

  TOTAL_FILES=$((TOTAL_FILES + 1))

  # Show progress indicator
  PERCENT=$((TOTAL_FILES * 100 / TOTAL_FILE_COUNT))
  printf "\r${BLUE}[%3d%%]${NC} Processing %d/%d files..." $PERCENT $TOTAL_FILES $TOTAL_FILE_COUNT >&2

  # Determine archive path (preserve directory structure)
  ARCHIVE_SUBDIR=$(dirname "$FILE" | sed 's|^./||' | tr '/' '-')
  ARCHIVE_FILENAME=$(basename "$FILE")
  ARCHIVE_PATH="${ARCHIVE_DIR}/${ARCHIVE_SUBDIR}-${ARCHIVE_FILENAME}"

  if [[ ! -f "$FILE" ]]; then
    log_warn "File not found: $FILE"
    FAILED_FILES=$((FAILED_FILES + 1))
    FAILED_LIST+=("$FILE")
    continue
  fi

  if [[ $DRY_RUN == false ]]; then
    # Move file
    mkdir -p "$(dirname "$ARCHIVE_PATH")"
    mv "$FILE" "$ARCHIVE_PATH"

    # Add to manifest
    {
      echo "- **Original Path**: \`$FILE\`"
      echo "  - **Archived**: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
      echo "  - **Archive Path**: \`.archive/${ARCHIVE_SUBDIR}-${ARCHIVE_FILENAME}\`"
      echo "  - **Reason**: unmodified 7+ days or marked deprecated"
      echo ""
    } >> "$MANIFEST_FILE"

    ARCHIVED_FILES=$((ARCHIVED_FILES + 1))
  else
    log_debug "Would move: $FILE → ${ARCHIVE_PATH#$REPO_ROOT/}"
  fi
done < "$FILE_LIST"

# Clear progress indicator line
echo -ne "\r${NC}\033[K" >&2

# Update Cargo.toml if needed
if [[ $UPDATE_CARGO == true && $DRY_RUN == false && -f "$REPO_ROOT/Cargo.toml" ]]; then
  log_info "Updating Cargo.toml..."

  # Find crates that were archived (look for Cargo.toml files in archive list)
  ARCHIVED_CRATES=()
  while IFS= read -r FILE; do
    [[ -z "$FILE" || "$FILE" =~ ^# ]] && continue

    # Check if this is a Cargo.toml that was archived
    if [[ "$FILE" == *"Cargo.toml" ]]; then
      # Extract crate name from path (e.g., crates/old-crate/Cargo.toml -> old-crate)
      CRATE_NAME=$(echo "$FILE" | sed -E 's|^.*crates/([^/]+)/Cargo.toml$|\1|')
      if [[ "$CRATE_NAME" != "$FILE" ]]; then
        ARCHIVED_CRATES+=("$CRATE_NAME")
        log_debug "Removing archived crate from workspace: $CRATE_NAME"
      fi
    fi
  done < "$FILE_LIST"

  # Update Cargo.toml to remove archived crates from [workspace] members
  if [[ ${#ARCHIVED_CRATES[@]} -gt 0 ]]; then
    for crate in "${ARCHIVED_CRATES[@]}"; do
      # Use sed to remove the crate from workspace members list
      # Pattern: 'crates/crate-name' or '"crates/crate-name"'
      sed -i.bak -e "/members = \[/,/\]/s|\"crates/$crate\"|<REMOVED>|g" "$REPO_ROOT/Cargo.toml"
      sed -i -e "/members = \[/,/\]/s|crates/$crate|<REMOVED>|g" "$REPO_ROOT/Cargo.toml"

      # Clean up any leftover <REMOVED> placeholders and extra commas
      sed -i -e 's|<REMOVED>,||g' -e 's|,<REMOVED>||g' -e 's|<REMOVED>||g' "$REPO_ROOT/Cargo.toml"

      log_info "Removed crate from workspace: $crate"
    done

    # Clean up backup file if sed created one
    [[ -f "$REPO_ROOT/Cargo.toml.bak" ]] && rm "$REPO_ROOT/Cargo.toml.bak"
  fi
fi

# Clean up imports if needed
if [[ $CLEANUP_IMPORTS == true && $DRY_RUN == false ]]; then
  log_info "Cleaning up imports..."

  # Find all archived module paths
  declare -a ARCHIVED_MODULES=()
  while IFS= read -r FILE; do
    [[ -z "$FILE" || "$FILE" =~ ^# ]] && continue

    # Skip Cargo.toml files
    [[ "$FILE" == *"Cargo.toml" ]] && continue

    # Extract module path from file path (e.g., crates/old/src/module.rs -> crates::old::module)
    MODULE_PATH=$(echo "$FILE" | sed 's|crates/||; s|/src||; s|/|::|g; s|\.rs$||')
    [[ -n "$MODULE_PATH" && "$MODULE_PATH" != "$FILE" ]] && ARCHIVED_MODULES+=("$MODULE_PATH")
  done < "$FILE_LIST"

  # Remove imports of archived modules from active .rs files
  find "$REPO_ROOT" -name "*.rs" -not -path "$ARCHIVE_DIR/*" -not -path "./target/*" 2>/dev/null | while read file; do
    for module in "${ARCHIVED_MODULES[@]}"; do
      # Remove use statements for archived modules
      if grep -q "use $module" "$file" 2>/dev/null; then
        sed -i.bak "/use $module/d" "$file"
        log_debug "Removed import of $module from $file"
      fi
    done

    # Clean up backup
    [[ -f "${file}.bak" ]] && rm "${file}.bak"
  done
fi

# Summary
echo ""
log_info "=========================================="
log_info "Archival Summary"
log_info "=========================================="
log_info "Total files in list: $TOTAL_FILES"
log_info "Successfully archived: $ARCHIVED_FILES"
log_info "Failed: $FAILED_FILES"
log_info "Archive directory: $ARCHIVE_DIR"
log_info "Manifest file: $MANIFEST_FILE"

if [[ $FAILED_FILES -gt 0 ]]; then
  log_warn "Failed files:"
  printf '%s\n' "${FAILED_LIST[@]}" | sed 's/^/  - /'
fi

[[ $DRY_RUN == true ]] && log_warn "DRY RUN - no changes made"

exit 0
