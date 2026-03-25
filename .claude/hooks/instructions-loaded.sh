#!/usr/bin/env bash
# InstructionsLoaded hook — logs which instruction files were loaded for debugging
# Fires when Claude Code loads CLAUDE.md and rule files at session start
# Output: appends to .claude/logs/instructions-loaded.log
# Performance target: <10ms

# Skip in CI environments
[[ -n "${CI:-}" ]] && exit 0

LOG_DIR="$(git rev-parse --show-toplevel 2>/dev/null)/.claude/logs"
LOG_FILE="${LOG_DIR}/instructions-loaded.log"

# Ensure log directory exists
mkdir -p "${LOG_DIR}" 2>/dev/null

# Read the hook payload from stdin (JSON with loaded file paths)
PAYLOAD=$(cat)

# Append timestamped entry to log (redirect all output to file, not stdout)
{
  echo "=== $(date -u +%Y-%m-%dT%H:%M:%SZ) ==="
  echo "${PAYLOAD}" | python3 -c "
import sys, json
try:
    data = json.load(sys.stdin)
    files = data.get('files', data.get('paths', []))
    for f in files:
        print(f'  LOADED: {f}')
    if not files:
        print(f'  (raw) {json.dumps(data)[:200]}')
except Exception as e:
    print(f'  (parse error: {e})')
" 2>/dev/null
  echo ""
} >> "${LOG_FILE}" 2>/dev/null

# Hook must exit 0 and produce no stdout (return empty JSON to Claude)
echo '{}'
