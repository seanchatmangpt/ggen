#!/usr/bin/env bash
# append_precision_measurement.sh — the accumulation half of the
# fire-precision instrument (L5 push, track B).
#
# measure_fire_precision_multi_session.py computes the per-session firing
# structure; this script turns each run into one dated, append-only row in
# PRECISION_LEDGER.md so the "measured on real captured data OVER TIME" L5
# bar can be approached the only way it can be: by actually accumulating
# rows across real sessions on real calendar dates. Running this once does
# not close the dimension — the ledger's own header says so — but every run
# adds one genuine longitudinal data point.
#
# Usage:
#   scripts/append_precision_measurement.sh <transcript.jsonl> [more.jsonl ...]
set -euo pipefail

cd "$(dirname "$0")/.."   # pack root
LEDGER="PRECISION_LEDGER.md"

if [[ $# -lt 1 ]]; then
    echo "usage: $0 <transcript.jsonl> [more.jsonl ...]" >&2
    exit 2
fi

args=()
for t in "$@"; do
    [[ -f "$t" ]] || { echo "no such transcript: $t" >&2; exit 2; }
    args+=(--transcript "$t")
done

report="$(mktemp)"
trap 'rm -f "$report"' EXIT
python3 scripts/measure_fire_precision_multi_session.py "${args[@]}" --out "$report"

python3 - "$report" "$LEDGER" "$@" <<'PY'
import json, sys, datetime, pathlib

report_path, ledger_path, *transcripts = sys.argv[1:]
report = json.load(open(report_path))
today = datetime.date.today().isoformat()

rows = []
for s in report.get("sessions", []):
    rows.append(
        "| {date} | `{name}` | {turns} | {gq} | {topics} | {fired} | {derived} |".format(
            date=today,
            name=pathlib.Path(s.get("transcript", "?")).name,
            turns=s.get("total_turns", "?"),
            gq=s.get("turn_kind_counts", {}).get("GroundingQuestion", 0),
            topics=len(s.get("repeated_grounding_topics", [])),
            fired=len(s.get("hook_actions_fired", [])),
            derived=s.get("total_derived_triples", "?"),
        )
    )

ledger = pathlib.Path(ledger_path)
text = ledger.read_text()
marker = "<!-- APPEND ROWS BELOW — one per measured session; never edit or delete prior rows -->"
assert marker in text, f"ledger marker missing from {ledger_path}"
text = text.replace(marker, marker + "\n" + "\n".join(rows), 1)
ledger.write_text(text)
print(f"appended {len(rows)} row(s) to {ledger_path}")
PY
