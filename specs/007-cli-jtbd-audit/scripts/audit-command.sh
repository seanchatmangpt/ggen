#!/bin/bash
# Audit script for ggen CLI JTBD evaluation
# Usage: ./scripts/audit-command.sh <category> <command> <subcommand>
# Example: ./scripts/audit-command.sh template generate

set -euo pipefail

FEATURE_DIR="$(pwd)"
CATEGORY="${1:-template}"
CMD_NAME="${2:-generate}"
FULL_CMD="$CATEGORY ${CMD_NAME}"
CMD_SLUG="${CATEGORY}-${CMD_NAME}"

EVIDENCE_DIR="$FEATURE_DIR/evidence/$CATEGORY"
mkdir -p "$EVIDENCE_DIR"

echo "🔍 Auditing: ggen $FULL_CMD"
echo "   Evidence dir: $EVIDENCE_DIR"
echo ""

# Test 1: Help works
echo "[1/4] Testing --help..."
if cargo run --release --quiet -- $FULL_CMD --help > "$EVIDENCE_DIR/$CMD_SLUG-help.log" 2>&1; then
  echo "✅ Help works"
else
  echo "❌ Help failed (exit code: $?)"
fi

# Test 2: No arguments (should show usage or error)
echo "[2/4] Testing with no arguments..."
if cargo run --release --quiet -- $FULL_CMD > "$EVIDENCE_DIR/$CMD_SLUG-no-args.log" 2>&1; then
  echo "⚠️  No-args succeeded (unexpected - may indicate missing validation)"
else
  EXIT_CODE=$?
  if [ "$EXIT_CODE" -eq 1 ] || [ "$EXIT_CODE" -eq 2 ]; then
    echo "✅ No-args failed appropriately (exit code: $EXIT_CODE)"
  else
    echo "❌ No-args failed with unexpected code: $EXIT_CODE"
  fi
fi

# Test 3: Invalid flag
echo "[3/4] Testing invalid flag..."
if cargo run --release --quiet -- $FULL_CMD --invalid-flag 2> "$EVIDENCE_DIR/$CMD_SLUG-invalid-flag.log" >/dev/null; then
  echo "⚠️  Invalid flag succeeded (unexpected)"
else
  echo "✅ Invalid flag rejected (exit code: $?)"
fi

# Test 4: Create YAML template
echo "[4/4] Creating audit template..."
cat > "$EVIDENCE_DIR/$CMD_SLUG.yaml" << 'EOF'
command: PLACEHOLDER
version_tested: 4.0.0
date: 2024-12-14
tester: claude-code

functional_correctness:
  executes: PLACEHOLDER
  help_works: PLACEHOLDER
  happy_path: PLACEHOLDER
  error_handling: PLACEHOLDER
  exit_codes: PLACEHOLDER

agent_score: 0
agent_breakdown:
  parseable_output: 0
  error_messages: 0
  idempotency: 0
  progress_feedback: 0
  dry_run: 0
  documentation: 0
  exit_codes: 0

avatar_notes:
  claude_code: ""
  cursor_ai: ""
  copilot: ""
  aider: ""
  devin: ""
  openhands: ""
  windsurf: ""

maturity_level: L0
maturity_blockers: []

evidence_files:
  - ${CMD_SLUG}-help.log
  - ${CMD_SLUG}-no-args.log
  - ${CMD_SLUG}-invalid-flag.log

recommendations: []
EOF

echo "✅ Template created"
echo ""
echo "📋 Edit with findings from logs above."
