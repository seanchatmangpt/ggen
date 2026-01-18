#!/bin/bash
# identify-archived.sh - Identify files unmodified 7+ days or marked deprecated
#
# Usage: ./identify-archived.sh [options]
# Options:
#   -d, --days N          Check for files unmodified N+ days (default: 7)
#   -o, --output FILE     Write output to FILE (default: stdout)
#   -j, --json            Output as JSON instead of plain text
#   -h, --help            Show this help message

set -euo pipefail

# Default values
DAYS_THRESHOLD=7
OUTPUT_FILE=""
JSON_MODE=false
REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || echo ".")

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

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

show_help() {
  sed -n '2,12p' "$0"
  exit 0
}

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -d|--days)
      DAYS_THRESHOLD="$2"
      shift 2
      ;;
    -o|--output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    -j|--json)
      JSON_MODE=true
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

# Verify git repository
if ! git rev-parse --git-dir >/dev/null 2>&1; then
  log_error "Not a git repository. Please run from ggen workspace root."
  exit 1
fi

log_info "Identifying files unmodified ${DAYS_THRESHOLD}+ days or marked deprecated..."
log_info "Repository root: $REPO_ROOT"

# Calculate cutoff date (N days ago)
if command -v gdate &> /dev/null; then
  CUTOFF_DATE=$(gdate -d "${DAYS_THRESHOLD} days ago" +%s)
else
  CUTOFF_DATE=$(date -v-${DAYS_THRESHOLD}d +%s 2>/dev/null || date -d "${DAYS_THRESHOLD} days ago" +%s)
fi

# Array to store results
declare -a SEVEN_DAY_OLD=()
declare -a DEPRECATED=()
declare -a ALL_FILES=()

# Phase 1: Find files unmodified 7+ days
log_info "Phase 1: Scanning for files unmodified ${DAYS_THRESHOLD}+ days..."

while IFS= read -r file; do
  # Get last modification timestamp
  if [[ -f "$file" ]]; then
    if command -v gstat &> /dev/null; then
      MOD_TIME=$(gstat -c%Y "$file" 2>/dev/null || stat -f%m "$file" 2>/dev/null || echo 0)
    else
      MOD_TIME=$(stat -c%Y "$file" 2>/dev/null || stat -f%m "$file" 2>/dev/null || echo 0)
    fi

    # Check if file is older than threshold
    if [[ $MOD_TIME -lt $CUTOFF_DATE ]]; then
      SEVEN_DAY_OLD+=("$file")
    fi
  fi
done < <(find "$REPO_ROOT" \
  -type f \
  \( -name "*.rs" -o -name "*.md" -o -path "*/tests/*" \) \
  -not -path "./.git/*" \
  -not -path "./.archive/*" \
  -not -path "./target/*" \
  -not -path "*/node_modules/*" \
  2>/dev/null || true)

log_info "Found ${#SEVEN_DAY_OLD[@]} files unmodified ${DAYS_THRESHOLD}+ days"

# Phase 2: Find files marked deprecated
log_info "Phase 2: Scanning for #[deprecated] attributes..."

while IFS= read -r file; do
  if grep -q "#\[deprecated\]" "$file" 2>/dev/null; then
    DEPRECATED+=("$file")
  fi
done < <(find "$REPO_ROOT" \
  -type f \
  -name "*.rs" \
  -not -path "./.git/*" \
  -not -path "./.archive/*" \
  -not -path "./target/*" \
  2>/dev/null || true)

log_info "Found ${#DEPRECATED[@]} files marked deprecated"

# Phase 3: Consolidate and deduplicate
log_info "Phase 3: Consolidating and deduplicating..."

# Combine arrays
for file in "${SEVEN_DAY_OLD[@]}"; do
  ALL_FILES+=("$file")
done

if [[ ${#DEPRECATED[@]} -gt 0 ]]; then
  for file in "${DEPRECATED[@]}"; do
    ALL_FILES+=("$file")
  done
fi

# Remove duplicates and sort
FINAL_FILES=($(printf '%s\n' "${ALL_FILES[@]}" | sort -u))

log_info "Total unique files to archive: ${#FINAL_FILES[@]}"

# Phase 4: Output results
if $JSON_MODE; then
  # JSON output
  echo "{"
  echo "  \"timestamp\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\","
  echo "  \"days_threshold\": $DAYS_THRESHOLD,"
  echo "  \"total_files\": ${#FINAL_FILES[@]},"
  echo "  \"seven_day_old_count\": ${#SEVEN_DAY_OLD[@]},"
  echo "  \"deprecated_count\": ${#DEPRECATED[@]},"
  echo "  \"files\": ["

  for i in "${!FINAL_FILES[@]}"; do
    echo -n "    \"${FINAL_FILES[$i]}\""
    if [[ $i -lt $((${#FINAL_FILES[@]} - 1)) ]]; then
      echo ","
    else
      echo ""
    fi
  done

  echo "  ]"
  echo "}"
else
  # Plain text output
  for file in "${FINAL_FILES[@]}"; do
    echo "$file"
  done
fi

# Write to output file if specified
if [[ -n "$OUTPUT_FILE" ]]; then
  if $JSON_MODE; then
    {
      echo "{"
      echo "  \"timestamp\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\","
      echo "  \"days_threshold\": $DAYS_THRESHOLD,"
      echo "  \"total_files\": ${#FINAL_FILES[@]},"
      echo "  \"files\": ["

      for i in "${!FINAL_FILES[@]}"; do
        echo -n "    \"${FINAL_FILES[$i]}\""
        if [[ $i -lt $((${#FINAL_FILES[@]} - 1)) ]]; then
          echo ","
        else
          echo ""
        fi
      done

      echo "  ]"
      echo "}"
    } > "$OUTPUT_FILE"
  else
    printf '%s\n' "${FINAL_FILES[@]}" > "$OUTPUT_FILE"
  fi
  log_info "Results written to: $OUTPUT_FILE"
fi

log_info "Identification complete!"
exit 0
