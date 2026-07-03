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
    AGE="$(andon_age_seconds)"
    FLAG_SESSION="$(andon_session_id)"
    THIS_SESSION="$(current_session_id)"

    # Gap fix: the flag has no expiry/session binding, so a crashed session's
    # never-cleared flag would otherwise block a brand new, unrelated
    # session's very first Stop forever with no indication it isn't this
    # turn's own unresolved failure. Still block either way (safe default —
    # never silently drop a real signal) but make the provenance explicit so
    # whoever reads it can judge whether to fix it or just clear it by hand.
    PROVENANCE=""
    if [[ -n "$FLAG_SESSION" && -n "$THIS_SESSION" && "$FLAG_SESSION" != "$THIS_SESSION" ]]; then
        PROVENANCE=" (raised by a different session: ${FLAG_SESSION})"
    fi
    AGE_NOTE=""
    if [[ "$AGE" -ge 0 ]]; then
        AGE_NOTE=" ${AGE}s ago"
        if andon_is_stale; then
            AGE_NOTE="${AGE_NOTE} — older than ${ANDON_STALE_SECONDS}s, likely stale/from a crashed session"
        fi
    fi

    stop_block "Andon flag raised${AGE_NOTE}${PROVENANCE}: ${REASON}
Excerpt: ${EXCERPT}
Fix the underlying issue and re-run the failing \`just\` command (check/test/lint) to clear the flag, or if this is confirmed stale/from an unrelated session: rm ${FLAG}"
    exit 0
fi

exit 0
