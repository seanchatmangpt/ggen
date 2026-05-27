#!/usr/bin/env bash
# ==============================================================================
# verify_agent_truthfulness.sh
# Workspace root orchestrator for Witnessed Agent Truthfulness validation.
# Exit code: 0 on success, 1 on failure
# ==============================================================================
set -euo pipefail

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$WORKSPACE_ROOT"

echo "=== Starting Witnessed Agent Truthfulness Orchestrator ==="

# 1. Compile all required verifier binaries
echo "Building verifier binaries..."
cargo build -p ggen-graph \
    --bin gall_observe_worktree \
    --bin gall_observe_commands \
    --bin gall_observe_doctests \
    --bin gall_observe_clean_room \
    --bin gall_observe_docs_tree \
    --bin gall_observe_sabotage \
    --bin gall_materialize_evidence_graph \
    --bin gall_actuate_code_evaluation \
    --bin gall_adjudicate_witnessed_truthfulness \
    -j 2

# Make scripts executable
chmod +x scripts/gall/*.sh
chmod +x scripts/gall/external/*.sh

# 2. Invoke the pipeline launcher
./scripts/gall/run_witnessed_truthfulness.sh

echo "=== Witnessed Agent Truthfulness Adjudication Passed ==="
exit 0
