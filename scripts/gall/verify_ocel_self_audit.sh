#!/usr/bin/env bash
# Runs the verify binary target to enforce 5 Completeness Rules on the self-audit log and coverage matrix.
set -euo pipefail

# Ensure we run from workspace root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/../.."

echo "=== Running verify_audit binary ==="
cargo run -p ggen-graph --bin verify_audit
