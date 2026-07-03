#!/usr/bin/env bash
# PostToolUse hook, matcher: Bash|Edit|Write (see .claude/settings.json). Budget: 10s.
#
# Purpose: implement the Andon "stop the line" protocol mechanically
# (.claude/rules/andon/signals.md) instead of relying on the agent to notice
# its own compiler errors. When a Bash command's output contains a CRITICAL
# signal (compiler error, failing test, panic), raise a flag file; the Stop
# hook (stop_release_gate.sh) refuses to end the turn while it exists. A
# subsequent clean run of the same class of command (check/test/lint) clears
# it. Edit/Write events are not Andon-relevant and pass through untouched.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

read_input  # drain stdin exactly once, into $HOOK_INPUT

TOOL_NAME="$(jqf '.tool_name')"
[[ "$TOOL_NAME" != "Bash" ]] && exit 0

COMMAND="$(jqf '.tool_input.command')"
# Different Claude Code versions have shipped Bash tool_response as
# {stdout,stderr} or as a single string — collect whichever is present.
OUTPUT="$(jq -r '
    if (.tool_response | type) == "object"
    then [(.tool_response.stdout // ""), (.tool_response.stderr // "")] | join("\n")
    else (.tool_response // "" | tostring)
    end
' <<<"$HOOK_INPUT" 2>/dev/null)"

[[ -z "$OUTPUT" ]] && exit 0

CRITICAL_PATTERN='error(\[E[0-9]+\])?:|error\[E[0-9]+\]|panicked at|test .* FAILED|FAILED\.|failures:'

if echo "$OUTPUT" | grep -qE "$CRITICAL_PATTERN"; then
    EXCERPT="$(echo "$OUTPUT" | grep -E -m 1 "$CRITICAL_PATTERN" | head -c 200)"
    raise_andon "andon signal in: ${COMMAND:0:80}" "$EXCERPT"
    exit 0
fi

# A clean run of a gating command clears a previously-raised flag.
if echo "$COMMAND" | grep -qE '\bjust\s+(check|test|test-unit|test-lib|lint|pre-commit)\b'; then
    if andon_is_raised; then
        clear_andon
    fi
fi

exit 0
