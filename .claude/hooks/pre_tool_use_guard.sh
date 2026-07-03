#!/usr/bin/env bash
# PreToolUse hook, matcher: Edit|Write (see .claude/settings.json). Budget: 10s.
#
# Purpose: hard-deny direct edits to surfaces that must only change through
# their own authoritative process — regenerating from RDF, re-signing a
# receipt, or a git hook re-install. Editing these files by hand silently
# desyncs them from the process that is supposed to own them (see
# .claude/rules/coding-agent-mistakes.md, "Legacy Path Contamination" /
# "Contract Drift").

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

read_input  # drain stdin exactly once, into $HOOK_INPUT

# Edit and Write both carry the target path at tool_input.file_path.
FILE_PATH="$(jqf '.tool_input.file_path')"
[[ -z "$FILE_PATH" ]] && exit 0

is_protected() {
    case "$1" in
        */crates/ggen-cli/src/generated_commands.rs|crates/ggen-cli/src/generated_commands.rs)
            REASON="generated_commands.rs is ggen sync output — edit the .specify/*.ttl spec and run ggen sync instead"
            return 0 ;;
        */.ggen/receipts/*|.ggen/receipts/*)
            REASON="receipts are cryptographic provenance sealed by ggen sync — they cannot be hand-edited"
            return 0 ;;
        */.ggen/keys/*|.ggen/keys/*)
            REASON="signing keys must not be edited by an agent"
            return 0 ;;
        */.git/hooks/*|.git/hooks/*)
            REASON="git hooks are installed by scripts/hooks/ — edit the source script, not the installed copy"
            return 0 ;;
    esac
    return 1
}

REASON=""
if is_protected "$FILE_PATH"; then
    ensure_evidence_dir >/dev/null
    jq -nc --arg ts "$(now)" --arg path "$FILE_PATH" --arg reason "$REASON" \
        '{timestamp:$ts, file_path:$path, reason:$reason, action:"denied"}' \
        >> "${EVIDENCE_DIR}/denied_edits.jsonl" 2>/dev/null || true

    deny_tool "Denied: ${FILE_PATH} — ${REASON}"
    exit 0
fi

exit 0
