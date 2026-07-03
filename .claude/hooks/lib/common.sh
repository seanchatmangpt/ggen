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
#
# Note: an Andon flag/raise_andon/stop_block mechanism (bash-regex scanning of
# Bash output for compiler errors/test failures, gating the Stop event) used
# to live here. Removed — it was a stale, weaker reimplementation of
# ~/anti-llm-cheat-lsp, an already-integrated, tested tool (AhoCorasick +
# tree-sitter AST + receipt validation; wired into this repo's own CI via
# `just lsp-check` → `cargo cicd lsp check`). See git history for the removed
# code if this ever needs resurrecting for a fast per-turn signal that CI
# can't provide.

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

# additional_context TEXT EVENT_NAME — emit the standard UserPromptSubmit/SessionStart payload.
# EVENT_NAME defaults to "UserPromptSubmit".
additional_context() {
    local event="${2:-UserPromptSubmit}"
    jq -n --arg ctx "$1" --arg evt "$event" \
        '{"hookSpecificOutput":{"hookEventName":$evt,"additionalContext":$ctx}}'
}

# deny_tool REASON — emit a PreToolUse hard-deny payload.
deny_tool() {
    jq -n --arg reason "$1" \
        '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":$reason}}'
}
