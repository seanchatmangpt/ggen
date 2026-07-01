#!/usr/bin/env sh
# Post-edit advisory: re-check a law-surface file after an edit and warn (does
# not block). Usage: post-edit.sh <file>
# Emitted by `ggen lsp emit-pack`. Part of the Agent Admissibility Pack.
set -eu
FILE="${1:-}"
[ -z "$FILE" ] && exit 0

case "$FILE" in
  *.ttl|*.nt|*.nq|*.rq|*.sparql|*.tera|*ggen.toml) ;;
  *) exit 0 ;;
esac

REPORT="$(mktemp)"
if ! ggen lsp check --files "$FILE" >"$REPORT" 2>&1; then
  echo "ggen-lsp: WARNING — $FILE still has law violations after edit:" >&2
  grep -E '"code"|"message"' "$REPORT" >&2 || cat "$REPORT" >&2
fi
rm -f "$REPORT"
exit 0
