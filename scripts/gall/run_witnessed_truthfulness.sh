#!/usr/bin/env bash
# ==============================================================================
# run_witnessed_truthfulness.sh
# Thin launcher for the Witnessed Agent Truthfulness verifiers.
# ==============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$WORKSPACE_ROOT"

echo "=== Running Witnessed Agent Truthfulness Pipeline ==="

# Clean up stale audit artifacts to prevent leakage of old namespace terms
echo "Cleaning up stale audit files..."
rm -f crates/ggen-graph/audit/*.ttl
rm -f crates/ggen-graph/audit/*.json
rm -rf crates/ggen-graph/audit/transcripts
mkdir -p crates/ggen-graph/audit/transcripts

# Build all binaries first to ensure we have the latest
cargo build -p ggen-graph --bins

# Initialize/emit compliant coverage matrix and self-audit files
./target/debug/emit_audit

# Initialize a clean, conforming doctest_results.ttl and doctest_results.json by running doctests observer first
./target/debug/gall_observe_doctests

# 1. Run baseline/initial observers
./target/debug/gall_observe_worktree
./target/debug/gall_observe_docs_tree

# 2. Materialize initial evidence graph
./target/debug/gall_materialize_evidence_graph

# 3. Actuate initial code evaluation
./target/debug/gall_actuate_code_evaluation

# 4. Check decision delta for BoundaryExecutionRequest and execute adapters if needed
DELTA_FILE="crates/ggen-graph/audit/gall_decision.delta.ttl"
if [ -f "$DELTA_FILE" ] && grep -q "BoundaryExecutionRequest" "$DELTA_FILE"; then
    echo "BoundaryExecutionRequest found in decision delta. Running adapters..."
    
    # Run all boundary adapters to consume the requests
    ./target/debug/gall_observe_commands
    ./target/debug/gall_observe_doctests
    ./target/debug/gall_observe_clean_room
    ./target/debug/gall_observe_sabotage
    
    # Re-materialize evidence graph with transcripts/results
    ./target/debug/gall_materialize_evidence_graph
    
    # Re-actuate code evaluation to evaluate final shapes
    ./target/debug/gall_actuate_code_evaluation
else
    echo "No BoundaryExecutionRequest found. Skipping adapter execution."
fi

# 5. Adjudicate final witnessed truthfulness status
./target/debug/gall_adjudicate_witnessed_truthfulness

echo "=== Pipeline Completed Successfully ==="
exit 0
