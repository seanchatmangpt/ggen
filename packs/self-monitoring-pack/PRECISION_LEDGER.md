# Fire-Precision Ledger

Append-only longitudinal record of the escalation hook's real firing behavior across real
Claude Code sessions, one row per measured session. This is the accumulation instrument for
Matrix 2's Fire-precision dimension: its L5 bar is "precision measured on real captured data
**over time**, not only fixtures" — a bar that can only close by rows accumulating across
real calendar dates, never by any single run. The instrument existing does not close the
dimension; the ledger's row count and date spread are the actual evidence.

Row producer: `scripts/append_precision_measurement.sh <transcript.jsonl> [...]` — runs
`measure_fire_precision_multi_session.py` (the same hook.ttl CONSTRUCT text, extracted
verbatim, executed by rdflib's real SPARQL 1.1 engine over turns captured by
`transcript_to_turtle.py`'s disclosed heuristic) and appends one dated row per session.

**Honest scope limit (inherited from the measurer, disclosed there in full):** rows record
real firing STRUCTURE (turn counts, GroundingQuestion counts, repeated-topic counts, hook
firings, derived triples) — the structural input to a precision metric. A TRUE
precision/recall number additionally needs per-session human ground-truth labels (as
README's Stage 3 produced for the original archived session), which no row below claims to
carry.

| Date | Session transcript | Turns | GroundingQuestion turns | Repeated topics | Hook actions fired | Derived triples |
|---|---|---|---|---|---|---|
<!-- APPEND ROWS BELOW — one per measured session; never edit or delete prior rows -->
| 2026-07-19 | `8089721d-6c81-4500-8d5a-7a92e9a72471.jsonl` | 545 | 0 | 0 | 0 | 0 |
