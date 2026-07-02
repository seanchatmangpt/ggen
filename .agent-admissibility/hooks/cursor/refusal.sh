#!/usr/bin/env sh
# Format a ggen-lsp law-surface refusal. Usage: refusal.sh <subject> <json-report-path>
# Emitted by `ggen lsp emit-pack`. Part of the Agent Admissibility Pack.
SUBJECT="${1:-law-surface files}"
REPORT="${2:-}"
echo "──────────────────────────────────────────────────────────" >&2
echo " ggen-lsp: EDIT REFUSED — $SUBJECT violate project canon"   >&2
echo "──────────────────────────────────────────────────────────" >&2
if [ -n "$REPORT" ] && [ -f "$REPORT" ]; then
  # Show the ERROR-level diagnostics (code + message) the gate produced.
  grep -E '"code"|"message"|"severity"' "$REPORT" >&2 || cat "$REPORT" >&2
fi
echo "" >&2
echo " The LSP and this hook enforce the same canon. Fix the law" >&2
echo " violation above, then retry. (v30.1.1: no law-surface edit" >&2
echo " without LSP + hook agreement.)"                            >&2
exit 1
