#!/usr/bin/env sh
# Pre-edit admission gate: refuse an edit to a ggen law-surface file that
# violates project canon. Usage: pre-edit.sh <file>
# Emitted by `ggen lsp emit-pack`. Part of the Agent Admissibility Pack.
set -eu
FILE="${1:-}"
[ -z "$FILE" ] && exit 0

case "$FILE" in
  *.ttl|*.nt|*.nq|*.rq|*.sparql|*.tera|*ggen.toml) ;;
  *) exit 0 ;;  # not a law surface — allow
esac

REPORT="$(mktemp)"
if ggen lsp check --files "$FILE" >"$REPORT" 2>&1; then
  rm -f "$REPORT"
  exit 0
fi
"$(dirname "$0")/refusal.sh" "$FILE" "$REPORT"
STATUS=$?
rm -f "$REPORT"
exit $STATUS
