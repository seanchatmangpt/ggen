#!/usr/bin/env bash
# Shared helpers for .claude/hooks/*.sh.
#
# Claude Code hook stdin schema (all events): a single JSON object with at
# least {session_id, transcript_path, cwd, hook_event_name}. Tool events add
# {tool_name, tool_input} (PreToolUse) or {tool_name, tool_input,
# tool_response} (PostToolUse). UserPromptSubmit adds {prompt}.
#
# Output convention used by every hook here: a JSON object on stdout with a
# "hookSpecificOutput" key (additionalContext / permissionDecision / etc.).
# Never write anything else to stdout — Claude Code parses it as the hook's
# structured result.

set -uo pipefail

WORKSPACE_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
EVIDENCE_DIR="${WORKSPACE_ROOT}/.claude/evidence"

now() { date -u +"%Y-%m-%dT%H:%M:%SZ"; }

# Stdin can only be read once per process. Every hook script must call
# read_input at the very top (before any command substitution touches
# stdin) so HOOK_INPUT is populated in the *main* shell, not a subshell that
# vanishes when the substitution ends.
HOOK_INPUT="{}"
read_input() {
    local buf
    buf="$(cat)"
    [[ -n "$buf" ]] && HOOK_INPUT="$buf"
}

# jqf JQ_FILTER [DEFAULT] — run jq -r against $HOOK_INPUT (already captured
# by read_input). Never reads stdin itself.
jqf() {
    local filter="$1" default="${2:-}"
    local out
    out="$(jq -r "$filter" <<<"$HOOK_INPUT" 2>/dev/null)"
    if [[ -z "$out" || "$out" == "null" ]]; then
        printf '%s' "$default"
    else
        printf '%s' "$out"
    fi
}

ensure_evidence_dir() {
    mkdir -p "$EVIDENCE_DIR"
    printf '%s' "$EVIDENCE_DIR"
}

andon_flag_path() {
    printf '%s/andon.flag' "$(ensure_evidence_dir)"
}

andon_is_raised() {
    [[ -f "$(andon_flag_path)" ]]
}

# raise_andon REASON EXCERPT — record a stop-the-line signal.
raise_andon() {
    local reason="$1" excerpt="${2:-}"
    jq -n --arg ts "$(now)" --arg reason "$reason" --arg excerpt "$excerpt" \
        '{raised_at:$ts, reason:$reason, excerpt:$excerpt}' > "$(andon_flag_path)" 2>/dev/null || true
}

clear_andon() {
    rm -f "$(andon_flag_path)"
}

# additional_context TEXT — emit the standard UserPromptSubmit/SessionStart payload.
additional_context() {
    jq -n --arg ctx "$1" '{"hookSpecificOutput":{"additionalContext":$ctx}}'
}

# deny_tool REASON — emit a PreToolUse hard-deny payload.
deny_tool() {
    jq -n --arg reason "$1" \
        '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":$reason}}'
}

# stop_block REASON — emit a Stop-hook payload that prevents the turn from ending.
stop_block() {
    jq -n --arg reason "$1" '{"decision":"block","reason":$reason}'
}
