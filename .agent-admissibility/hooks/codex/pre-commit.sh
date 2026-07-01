#!/usr/bin/env sh
# Pre-commit admission gate: refuse a commit whose staged law-surface files
# violate project canon. Install as .git/hooks/pre-commit (or call from one).
# Emitted by `ggen lsp emit-pack`. Part of the Agent Admissibility Pack.
set -eu

# Staged, added/copied/modified law-surface files.
FILES="$(git diff --cached --name-only --diff-filter=ACM 2>/dev/null \
  | grep -E '\.(ttl|nt|nq|rq|sparql|tera)$|ggen\.toml$' || true)"
[ -z "$FILES" ] && exit 0

REPORT="$(mktemp)"
# parse_paths splits on whitespace/newlines, so the newline-separated list is fine.
if ggen lsp check --files "$FILES" >"$REPORT" 2>&1; then
  rm -f "$REPORT"
  exit 0
fi
"$(dirname "$0")/refusal.sh" "staged law-surface files" "$REPORT"
STATUS=$?
rm -f "$REPORT"
exit $STATUS
