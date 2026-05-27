#!/usr/bin/env bash
# Runs the emit binary target to generate self-audit log and coverage matrix.
set -euo pipefail

# Ensure we run from workspace root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/../.."

echo "=== Running emit_audit binary ==="
cargo run -p ggen-graph --bin emit_audit
