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

# Gap fix: scanning was previously unconditional on ANY Bash output, so
# `cat`-ing a doc/log/comment/this-very-script that merely contains the
# substring "error:" (a diagnostic-code table, a stale log line, a fixed-now
# note) raised Andon on nothing having actually failed. Only scan output that
# came from a command that could plausibly produce a real compiler/test
# signal in the first place.
BUILD_COMMAND_PATTERN='\bjust\s+[a-z][a-z-]*\b|\bcargo\s+(check|test|build|clippy|fmt|bench|run|doc|publish)\b|\brustc\b'
if ! echo "$COMMAND" | grep -qE "$BUILD_COMMAND_PATTERN"; then
    exit 0
fi

# Different Claude Code versions have shipped Bash tool_response as
# {stdout,stderr} or as a single string — collect whichever is present.
OUTPUT="$(jq -r '
    if (.tool_response | type) == "object"
    then [(.tool_response.stdout // ""), (.tool_response.stderr // "")] | join("\n")
    else (.tool_response // "" | tostring)
    end
' <<<"$HOOK_INPUT" 2>/dev/null)"

[[ -z "$OUTPUT" ]] && exit 0

# Case-insensitive (-i) to catch capitalized `Error:` (clap and several CLI
# tools use it); widened to catch fatal signals/aborts that don't say "error:"
# or "panicked at" (stack overflow, SIGSEGV/SIGABRT, a bare assertion failure
# printed before the harness gets to emit its "test result: FAILED" summary).
CRITICAL_PATTERN='error(\[E[0-9]+\])?:|panicked at|test .* FAILED|FAILED\.|failures:|has overflowed its stack|SIGSEGV|SIGABRT|fatal runtime error|process didn.t exit successfully|assertion `.*` failed|assertion failed'

if echo "$OUTPUT" | grep -qEi "$CRITICAL_PATTERN"; then
    EXCERPT="$(echo "$OUTPUT" | grep -Ei -m 1 "$CRITICAL_PATTERN" | head -c 200)"
    # Flatten embedded newlines/tabs so the flag's `reason` stays one line and
    # scannable; keep the full command (not an 80-char slice that silently
    # cut off the actual subcommand behind a long `cd`/env-var prefix) so the
    # evidence file always has what actually triggered it.
    COMMAND_FLAT="$(tr '\n\t' '  ' <<<"$COMMAND")"
    raise_andon "andon signal in: ${COMMAND_FLAT:0:500}" "$EXCERPT"
    exit 0
fi

# A clean run of a gating command clears a previously-raised flag.
if echo "$COMMAND" | grep -qE '\bjust\s+(check|test|test-unit|test-lib|test-doc|lint|pre-commit)\b'; then
    if andon_is_raised; then
        clear_andon
    fi
fi

exit 0
