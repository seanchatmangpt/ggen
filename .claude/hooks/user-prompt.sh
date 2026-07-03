#!/usr/bin/env bash
# UserPromptSubmit hook. Budget: 5s (see .claude/settings.json).
#
# Purpose: catch a small number of high-precision, high-cost mistakes before
# the agent starts working — not a general lint pass over prose. Each check
# below corresponds to a documented absolute rule (CLAUDE.md /
# .claude/rules/_core/absolute.md) whose violation is expensive to undo
# (a destructive git op, a direct `cargo` invocation bypassing `just`, a
# forbidden tool). Silent (no additionalContext) when nothing matches —
# noisy heuristics that fire on ordinary phrasing train the agent (and the
# user) to ignore the hook.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "${SCRIPT_DIR}/lib/common.sh"

read_input  # drain stdin exactly once, into $HOOK_INPUT

PROMPT="$(jqf '.prompt')"
[[ -z "$PROMPT" ]] && exit 0

NOTES=()

# Direct cargo invocation instead of the just entry point (absolute rule #4).
if echo "$PROMPT" | grep -qiE '\brun\b.*\bcargo\s+(check|test|build|clippy|fmt|bench)\b|\bcargo\s+make\b'; then
    NOTES+=("This repo requires \`just <task>\` as the entry point (never bare cargo or cargo-make) — see .claude/rules/_core/absolute.md rule 4.")
fi

# Destructive git ops the repo forbids outright (fix-forward only).
if echo "$PROMPT" | grep -qiE 'git\s+reset\s+--hard|git\s+push[^|&]*--force[^|&]*\b(main|master)\b|git\s+clean\s+-[a-z]*f'; then
    NOTES+=("CLAUDE.md: fix forward only — git reset --hard and force-push to main/master are forbidden. Add a commit instead.")
fi

# Forbidden tool family (CLAUDE.md Tool Restrictions).
if echo "$PROMPT" | grep -qiE 'desktop-commander'; then
    NOTES+=("mcp__desktop-commander__* is explicitly forbidden in this repo — use Read/Write/Edit/Glob/Grep/Bash instead.")
fi

# RDF-is-truth: editing generated markdown instead of the TTL source.
if echo "$PROMPT" | grep -qiE '\.specify.*\.(md|markdown)\b' ; then
    NOTES+=("RDF is the source of truth here — edit .specify/*.ttl, not the generated .md.")
fi

if [[ ${#NOTES[@]} -eq 0 ]]; then
    exit 0
fi

JOINED="$(printf '%s\n' "${NOTES[@]}")"
additional_context "$JOINED"
exit 0
