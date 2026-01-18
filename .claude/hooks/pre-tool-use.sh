#!/bin/bash
# Pre-Tool-Use Validation - Poka-Yoke Safety Gates
set -euo pipefail
TOOL_NAME="${CLAUDE_TOOL_NAME:-unknown}"
TOOL_INPUT="${CLAUDE_TOOL_INPUT:-}"

# Parse inputs
COMMAND=$(echo "$TOOL_INPUT" | jq -r '.command // empty' 2>/dev/null || echo "")
FILE_PATH=$(echo "$TOOL_INPUT" | jq -r '.file_path // empty' 2>/dev/null || echo "")

# Gate 1: Block direct cargo (must use cargo make)
if [[ "$COMMAND" =~ ^cargo\ (check|test|build|clippy|run) ]]; then
  echo "✗ BLOCKED: Direct cargo forbidden. Use cargo make."
  exit 2
fi

# Gate 2: Block destructive operations
if [[ "$COMMAND" =~ (rm\ -rf\ /|git\ push.*--force|sudo) ]]; then
  echo "✗ BLOCKED: Destructive operation"
  exit 2
fi

# Gate 3: Validate file paths
if [[ -n "$FILE_PATH" ]]; then
  # Block path traversal
  [[ "$FILE_PATH" == *".."* ]] && { echo "✗ Path traversal"; exit 2; }
  # Block secrets
  [[ "$FILE_PATH" =~ \.(env|pem|key)$ ]] && { echo "✗ Protected file"; exit 2; }
  # Block generated markdown (edit .ttl source)
  [[ "$FILE_PATH" =~ \.specify/.*\.md$ ]] && { echo "✗ Edit .ttl not .md"; exit 2; }
fi

echo "✓ Validation passed"
exit 0
