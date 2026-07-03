#!/usr/bin/env bash
# SessionStart hook. Budget: 15s (see .claude/settings.json).
#
# Purpose: give the agent accurate, live workspace state before the first
# prompt — branch, uncommitted changes, a fast compile signal. Doctrine
# itself (just-not-cargo, Chicago TDD, fix-forward, LSP-first) already lives
# in CLAUDE.md and is loaded unconditionally, so this hook does not repeat it
# — it only adds what CLAUDE.md cannot know: current repo state.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

read_input  # drain stdin exactly once, into $HOOK_INPUT

# Gap fix: every downstream helper (additional_context, jqf) shells out to
# jq. If jq isn't on PATH (minimal/sandboxed PATH, a container missing
# it), those calls fail silently under `set -uo pipefail` (no -e) and the hook
# still exits 0 with completely empty stdout — the agent gets zero session
# context with no indication anything went wrong. Fail loudly instead, with a
# plain-text payload that doesn't depend on jq at all.
if ! command -v jq >/dev/null 2>&1; then
    printf '{"hookSpecificOutput":{"additionalContext":"session-start: jq not found on PATH \\u2014 workspace state unavailable this turn"}}\n'
    exit 0
fi

cd "$WORKSPACE_ROOT" 2>/dev/null || { additional_context "session-start: not inside the ggen git repo" "SessionStart"; exit 0; }

BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo unknown)"
UNCOMMITTED="$(git status --porcelain 2>/dev/null | wc -l | tr -d ' ')"

# Fast, bounded compile signal. `cargo check --workspace` can run well past the
# 15s hook budget on a cold cache, so this checks one small, always-present
# crate (ggen-config) as a canary rather than the whole workspace.
COMPILE_STATE="UNKNOWN"
if command -v cargo >/dev/null 2>&1; then
    # Gap fix: piping `timeout ... | tail -5` loses timeout's own exit code
    # (the pipeline reports tail's exit status), and a killed cold-cache
    # compile prints a non-empty "Compiling ggen-config v..." line before
    # being SIGTERM'd — so the old `[[ -z "$OUT" ]]` timeout check never
    # actually fired; it silently reported "clean" for a run that never
    # finished. Capture to a temp file so `timeout`'s exit code (124 = killed)
    # is checked directly.
    OUT_FILE="$(mktemp -t ggen-session-start-cargo.XXXXXX)"
    trap 'rm -f "$OUT_FILE"' EXIT
    timeout 10s cargo check -p ggen-config >"$OUT_FILE" 2>&1
    CARGO_RC=$?
    OUT="$(tail -5 "$OUT_FILE")"
    if [[ $CARGO_RC -eq 124 ]]; then
        COMPILE_STATE="TIMED OUT (>10s, cold cache?) — run: just check"
    elif echo "$OUT" | grep -qE 'error(\[E[0-9]+\])?:'; then
        COMPILE_STATE="ERRORS (ggen-config) — run: just check"
    else
        COMPILE_STATE="clean (ggen-config canary)"
    fi
fi

CONTEXT="$(jq -rn \
    --arg branch "$BRANCH" \
    --arg uncommitted "$UNCOMMITTED" \
    --arg compile "$COMPILE_STATE" \
    '"=== ggen @ \($branch) ===
uncommitted changes: \($uncommitted)
compile canary: \($compile)"'
)"

additional_context "$CONTEXT" "SessionStart"
exit 0
