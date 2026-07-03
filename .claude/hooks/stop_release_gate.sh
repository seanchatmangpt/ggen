#!/usr/bin/env bash
# Stop hook. Budget: 30s (see .claude/settings.json).
#
# Purpose: mechanical enforcement of "stop the line" (.claude/rules/andon/signals.md
# / CLAUDE.md Andon Protocol). This does NOT re-run the full test suite on
# every turn end — `just test`/`just slo-check` can take minutes, and a Stop
# hook that slow would make every turn slow. Instead it checks the Andon flag
# that post_tool_use_emitter.sh raises the moment a compiler error, test
# failure, or panic is observed in a Bash command's output during this
# session, and clears the moment a subsequent `just check`/`just test`/`just
# lint` run comes back clean. If the flag is up, the last known signal was
# never fixed — block the stop and say so.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

read_input  # drain stdin exactly once, into $HOOK_INPUT

if andon_is_raised; then
    FLAG="$(andon_flag_path)"
    REASON="$(jq -r '.reason // "unknown"' "$FLAG" 2>/dev/null)"
    EXCERPT="$(jq -r '.excerpt // ""' "$FLAG" 2>/dev/null)"
    stop_block "Andon flag is raised (${REASON}): ${EXCERPT}
Fix the underlying issue and re-run the failing \`just\` command (check/test/lint) to clear the flag before ending this turn."
    exit 0
fi

exit 0
