#!/usr/bin/env bash
set -euo pipefail
# Usage: accept-receipt.sh --file <receipts.jsonl> --json <receipt-json-string>
file=""
json=""
while [ $# -gt 0 ]; do
  case "$1" in
    --file) file="$2"; shift 2 ;;
    --json) json="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[ -n "$file" ] || { echo "usage: accept-receipt.sh --file <receipts.jsonl> --json <receipt-json-string>" >&2; exit 2; }
[ -n "$json" ] || { echo "usage: accept-receipt.sh --file <receipts.jsonl> --json <receipt-json-string>" >&2; exit 2; }
command -v jq >/dev/null 2>&1 || { echo "accept-receipt.sh requires jq on PATH (brew install jq)" >&2; exit 2; }
prev="genesis"
if [ -s "$file" ]; then
  prev=$(tail -n 1 "$file" | shasum -a 256 | cut -d " " -f 1)
fi
line=$(echo "$json" | jq -c --arg prev "$prev" '. + {prev_hash: $prev}')
echo "$line" >> "$file"
echo "$line"
