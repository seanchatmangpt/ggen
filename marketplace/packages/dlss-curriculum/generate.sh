#!/usr/bin/env bash
# DLSS Curriculum - ggen sync pipeline
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"
echo "=== DLSS Black Belt Curriculum Generator ==="
echo "Using ggen sync pipeline (mu1-mu5)"
ggen sync --verbose
echo ""
echo "Generated $(find output/ -type f 2>/dev/null | wc -l) files in output/"
