#!/usr/bin/env bash
# SessionStart hook. Budget: 15s (see .claude/settings.json).
#
# Purpose: give the agent accurate, live workspace state before the first
# prompt — branch, uncommitted changes, a fast compile signal, and whether a
# stop-the-line (Andon) flag is already raised from a prior turn. Doctrine
# itself (just-not-cargo, Chicago TDD, fix-forward, LSP-first) already lives
# in CLAUDE.md and is loaded unconditionally, so this hook does not repeat it
# — it only adds what CLAUDE.md cannot know: current repo state.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

read_input  # drain stdin exactly once, into $HOOK_INPUT

cd "$WORKSPACE_ROOT" 2>/dev/null || { additional_context "session-start: not inside the ggen git repo"; exit 0; }

BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo unknown)"
UNCOMMITTED="$(git status --porcelain 2>/dev/null | wc -l | tr -d ' ')"

# Fast, bounded compile signal. `cargo check --workspace` can run well past the
# 15s hook budget on a cold cache, so this checks one small, always-present
# crate (ggen-config) as a canary rather than the whole workspace.
COMPILE_STATE="UNKNOWN"
if command -v cargo >/dev/null 2>&1; then
    OUT="$(timeout 10s cargo check -p ggen-config 2>&1 | tail -5)"
    if echo "$OUT" | grep -qE 'error(\[E[0-9]+\])?:'; then
        COMPILE_STATE="ERRORS (ggen-config) — run: just check"
    elif [[ -z "$OUT" ]]; then
        COMPILE_STATE="TIMED OUT (>10s) — run: just check"
    else
        COMPILE_STATE="clean (ggen-config canary)"
    fi
fi

ANDON_NOTE=""
if andon_is_raised; then
    REASON="$(jq -r '.reason // "unknown"' "$(andon_flag_path)" 2>/dev/null)"
    ANDON_NOTE="ANDON FLAG RAISED from a prior turn: ${REASON}. Fix and re-run the failing command before ending this turn."
fi

CONTEXT="$(jq -rn \
    --arg branch "$BRANCH" \
    --arg uncommitted "$UNCOMMITTED" \
    --arg compile "$COMPILE_STATE" \
    --arg andon "$ANDON_NOTE" \
    '"=== ggen @ \($branch) ===
uncommitted changes: \($uncommitted)
compile canary: \($compile)
\(if $andon != "" then "\n\($andon)\n" else "" end)"'
)"

additional_context "$CONTEXT"
exit 0
