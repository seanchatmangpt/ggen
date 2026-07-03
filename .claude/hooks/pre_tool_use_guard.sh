#!/usr/bin/env bash
# PreToolUse hook, matcher: Edit|Write|Bash (see .claude/settings.json). Budget: 10s.
#
# Purpose: hard-deny direct edits to surfaces that must only change through
# their own authoritative process — regenerating from RDF, re-signing a
# receipt, or a git hook re-install. Editing these files by hand silently
# desyncs them from the process that is supposed to own them (see
# .claude/rules/coding-agent-mistakes.md, "Legacy Path Contamination" /
# "Contract Drift").
#
# Gap fix: this hook originally only matched Edit|Write, so Bash could freely
# `echo ... > .ggen/receipts/x.json` or `rm -rf .ggen/keys` with zero
# enforcement — the exact class of write this hook exists to stop. It also
# matched the raw file_path string with bash case-glob, so a symlink or a
# `./`-prefixed / differently-cased path bypassed the check entirely even
# though it resolved to a protected file. Both are fixed below.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

read_input  # drain stdin exactly once, into $HOOK_INPUT

TOOL_NAME="$(jqf '.tool_name')"

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

log_denial() {
    local path="$1" reason="$2"
    ensure_evidence_dir >/dev/null
    jq -nc --arg ts "$(now)" --arg path "$path" --arg reason "$reason" \
        '{timestamp:$ts, file_path:$path, reason:$reason, action:"denied"}' \
        >> "${EVIDENCE_DIR}/denied_edits.jsonl" 2>/dev/null || true
}

# normalize_path PATH — resolve symlinks/`./`/`../` without requiring the
# final path component to exist (Write may target a brand-new file), relative
# to WORKSPACE_ROOT if given as a relative path. Uses python3 (always present
# in this repo's toolchain) rather than `realpath`, whose -m/-e semantics
# differ between GNU and BSD and aren't guaranteed portable.
normalize_path() {
    local p="$1"
    [[ "$p" != /* ]] && p="${WORKSPACE_ROOT}/${p}"
    python3 -c "import os,sys; print(os.path.realpath(sys.argv[1]))" "$p" 2>/dev/null || printf '%s' "$p"
}

if [[ "$TOOL_NAME" == "Edit" || "$TOOL_NAME" == "Write" ]]; then
    FILE_PATH="$(jqf '.tool_input.file_path')"
    [[ -z "$FILE_PATH" ]] && exit 0

    NORMALIZED="$(normalize_path "$FILE_PATH")"
    REASON=""
    if is_protected "$NORMALIZED"; then
        log_denial "$FILE_PATH" "$REASON"
        deny_tool "Denied: ${FILE_PATH} — ${REASON}"
        exit 0
    fi
    exit 0
fi

if [[ "$TOOL_NAME" == "Bash" ]]; then
    COMMAND="$(jqf '.tool_input.command')"
    [[ -z "$COMMAND" ]] && exit 0

    # Heuristic, not a shell parser: block when the command both (a) names a
    # protected path/glob and (b) uses a token that plausibly writes/deletes
    # (redirection, cp/mv/sed -i/rm/tee/install/truncate). This has false-
    # negative room (arbitrarily obfuscated commands can dodge it) but that's
    # true of any regex-based guard; it closes the wide-open "Bash bypasses
    # PreToolUse entirely" gap without trying to be a full shell-semantics
    # checker.
    PROTECTED_GLOB='(^|[[:space:]/])(crates/ggen-cli/src/generated_commands\.rs|\.ggen/receipts/|\.ggen/keys/|\.git/hooks/)'
    WRITE_TOKEN='(>|>>|\bcp\b|\bmv\b|\bsed\b[^|;&]*-i|\brm\b|\btee\b|\binstall\b|\btruncate\b|\bdd\b)'

    if echo "$COMMAND" | grep -qE "$PROTECTED_GLOB" && echo "$COMMAND" | grep -qE "$WRITE_TOKEN"; then
        log_denial "(bash command)" "command appears to write/delete a protected path: ${COMMAND:0:200}"
        deny_tool "Denied: this command appears to write or delete a protected path (crates/ggen-cli/src/generated_commands.rs, .ggen/receipts/*, .ggen/keys/*, or .git/hooks/*). These must only change through their own authoritative process (ggen sync, key rotation, scripts/hooks/ install) — not a direct shell write."
        exit 0
    fi
    exit 0
fi

exit 0
