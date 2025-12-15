#!/bin/bash
# generate-archive-report.sh - Generate archival summary report
#
# Usage: ./generate-archive-report.sh [options]
# Options:
#   -f, --format FORMAT   Output format: text, json, markdown (default: markdown)
#   -o, --output FILE     Write output to FILE (default: stdout)
#   -h, --help            Show this help message

set -euo pipefail

# Default values
OUTPUT_FORMAT="markdown"
OUTPUT_FILE=""
REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || echo ".")
ARCHIVE_DIR="${REPO_ROOT}/.archive"
MANIFEST_FILE="${ARCHIVE_DIR}/MANIFEST.md"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
  echo -e "${GREEN}[INFO]${NC} $1" >&2
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $1" >&2
}

show_help() {
  sed -n '2,13p' "$0"
  exit 0
}

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -f|--format)
      OUTPUT_FORMAT="$2"
      shift 2
      ;;
    -o|--output)
      OUTPUT_FILE="$2"
      shift 2
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

# Verify archive exists
if [[ ! -d "$ARCHIVE_DIR" ]]; then
  log_error "Archive directory not found: $ARCHIVE_DIR"
  exit 1
fi

if [[ ! -f "$MANIFEST_FILE" ]]; then
  log_error "Manifest file not found: $MANIFEST_FILE"
  exit 1
fi

log_info "Generating archival report..."

# Collect statistics
TOTAL_ARCHIVED=$(find "$ARCHIVE_DIR" -type f ! -name "MANIFEST.md" | wc -l)
MANIFEST_ENTRIES=$(grep -c "^- \*\*Original Path\*\*" "$MANIFEST_FILE" 2>/dev/null || echo 0)
ARCHIVE_SIZE=$(du -sh "$ARCHIVE_DIR" | awk '{print $1}')
CREATION_DATE=$(head -3 "$MANIFEST_FILE" | grep "Created" | sed 's/.*: //')
DEPRECATION_COUNT=$(grep -c "marked deprecated" "$MANIFEST_FILE" 2>/dev/null || echo 0)
UNMODIFIED_COUNT=$((MANIFEST_ENTRIES - DEPRECATION_COUNT))

# Generate report
generate_text_report() {
  cat << EOF
ARCHIVAL REPORT
===============

Repository: $REPO_ROOT
Archive Location: $ARCHIVE_DIR
Created: $CREATION_DATE

SUMMARY
-------
Total Archived Files: $TOTAL_ARCHIVED
Manifest Entries: $MANIFEST_ENTRIES
Archive Size: $ARCHIVE_SIZE

BREAKDOWN
---------
Files Unmodified 7+ Days: $UNMODIFIED_COUNT
Files Marked Deprecated: $DEPRECATION_COUNT

RECENT ARCHIVES (Last 10)
-------------------------
EOF

  grep "^- \*\*Original Path\*\*" "$MANIFEST_FILE" 2>/dev/null | \
    tail -10 | \
    sed 's/^- \*\*Original Path\*\*: `/  - /' | \
    sed 's/`$//'
}

generate_markdown_report() {
  cat << EOF
# Archival Report

**Repository**: \`$REPO_ROOT\`
**Archive Location**: \`$ARCHIVE_DIR\`
**Created**: $CREATION_DATE

## Summary

| Metric | Value |
|--------|-------|
| Total Archived Files | $TOTAL_ARCHIVED |
| Manifest Entries | $MANIFEST_ENTRIES |
| Archive Size | $ARCHIVE_SIZE |

## Breakdown

- **Files Unmodified 7+ Days**: $UNMODIFIED_COUNT
- **Files Marked Deprecated**: $DEPRECATION_COUNT

## Recent Archives (Last 10)

\`\`\`
EOF

  grep "^- \*\*Original Path\*\*" "$MANIFEST_FILE" 2>/dev/null | \
    tail -10 | \
    sed 's/^- \*\*Original Path\*\*: `//' | \
    sed 's/`$//'

  cat << EOF
\`\`\`

## Full Manifest

See \`.archive/MANIFEST.md\` for complete inventory.

## Recovery Instructions

To restore an archived file:

\`\`\`bash
# Find the file in git history
git log --follow -- .archive/path-to-file

# Restore to original location
git show <commit>:.archive/path-to-file > original/path/file
\`\`\`

To undo archival completely:

\`\`\`bash
# Find archival commit
git log --oneline | grep -i archive

# Revert it
git revert <archival-commit>
\`\`\`
EOF
}

generate_json_report() {
  cat << EOF
{
  "report": {
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "repository": "$REPO_ROOT",
    "archive": {
      "location": "$ARCHIVE_DIR",
      "created": "$CREATION_DATE",
      "size": "$ARCHIVE_SIZE"
    },
    "statistics": {
      "total_archived_files": $TOTAL_ARCHIVED,
      "manifest_entries": $MANIFEST_ENTRIES,
      "unmodified_7plus_days": $UNMODIFIED_COUNT,
      "marked_deprecated": $DEPRECATION_COUNT
    }
  }
}
EOF
}

# Generate report based on format
case $OUTPUT_FORMAT in
  text)
    REPORT=$(generate_text_report)
    ;;
  markdown)
    REPORT=$(generate_markdown_report)
    ;;
  json)
    REPORT=$(generate_json_report)
    ;;
  *)
    log_error "Unknown format: $OUTPUT_FORMAT (use: text, json, markdown)"
    exit 1
    ;;
esac

# Output report
if [[ -n "$OUTPUT_FILE" ]]; then
  echo "$REPORT" > "$OUTPUT_FILE"
  log_info "Report written to: $OUTPUT_FILE"
else
  echo "$REPORT"
fi

log_info "Report generation complete!"
exit 0
