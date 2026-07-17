# Dogfood Lifecycle Pack

Operation Dogfood (v26.7.13): the machinery by which Multifractal Workflow governs its own
Claude Code session lifecycle. Every tool call the operator makes becomes an admitted RDF
observation that `ggen` can validate and receipt — MFW as customer zero, not MFW as an artifact
generator.

This is the crown-I bootstrap seed: MFW's own `ggen` validates MFW's own session capture,
driven by MFW's own hook, on MFW's own session.

## The loop

```text
tool call  --PostToolUse hook-->  session-<id>.ttl  --ggen graph validate-->  chained
 (capture)     (one dfl:ToolEvent      (admitted RDF          (parse + SHACL)   receipt
               node per event)          observation)                     (blake3 hash-chain)
```

1. **Capture** — `hooks/dogfood-lifecycle-capture.sh` (a PostToolUse hook) appends one
   `dfl:ToolEvent` Turtle node per tool event to `.cargo-cicd/lifecycle/session-<id>.ttl`,
   content-addressing the tool input/result with real blake3 digests (`urn:blake3:<hex>`).
2. **Validate** — `hooks/dogfood-lifecycle-session-end.sh` runs
   `ggen graph validate --files <session.ttl> --shapes shapes.ttl` over every captured log
   (Turtle parse plus SHACL shape-conformance against `dfl:ToolEventShape`/`dfl:SessionShape`).
3. **Receipt** — the same script appends a hash-chained validation receipt per log to
   `.cargo-cicd/lifecycle/receipts.jsonl` (`payload_hash`/`prev_chain_hash`/`chain_hash`; see
   "Scope and named follow-ups" below for exactly what this does and does not guarantee).

## Files

| Path | Role |
|---|---|
| `ontology.ttl` | Session-lifecycle vocabulary (PROV-O / DCTERMS / SKOS / OWL-Time + disclosed `dfl:` terms) |
| `shapes.ttl` | SHACL shape a tool-event node must satisfy (session ref, tool name, ordering, outcome, agent, used/generated) |
| `fixtures/session-good.ttl` | Well-formed sample session log |
| `fixtures/session-malformed.ttl` | Deliberately broken sample (the parse falsifier) |
| `hooks/dogfood-lifecycle-capture.sh` | PostToolUse capture hook (canonical copy; the live install lives in `.claude/hooks/`) |
| `hooks/dogfood-lifecycle-session-end.sh` | Session-end validator + hash-chained receipt writer |
| `hooks/dogfood-lifecycle-receipt-spotcheck.sh` | Bash-only recompute smoke test for the receipt chain (not a trust boundary — see follow-ups) |

## Installation (local)

The repository gitignores `.claude/` (developer-local config), so the wiring is installed
locally, not committed. Copy the hooks and add the PostToolUse matcher to `.claude/settings.json`:

```json
{
  "matcher": "Bash|Edit|Write|Read|Grep|Glob|Task|WebFetch|WebSearch",
  "hooks": [
    { "type": "command", "command": "<repo>/.claude/hooks/dogfood-lifecycle-capture.sh" }
  ]
}
```

Run the validator at session end: `bash .claude/hooks/dogfood-lifecycle-session-end.sh`.

## Verification (live, this session)

The hook fired on real tool calls (`Write`, `Bash`) and captured them to
`session-1f9798ec-f62d-48bb-80a0-e9817fafdb71.ttl`; the session-end validator parsed all logs
(exit 0); a malformed log failed (exit 1):

```text
$ ggen graph validate --files fixtures/session-good.ttl              # -> exit 0, 67 quads
$ ggen graph validate --files fixtures/session-malformed.ttl         # -> exit 1, names the file
$ bash hooks/dogfood-lifecycle-session-end.sh                        # -> VALID, receipts appended
$ (inject a malformed session log) -> session-end                    # -> exit 1 (fail-closed)
```

## Receipt chain verification (live, this session)

Ran the upgraded `hooks/dogfood-lifecycle-session-end.sh` twice against the real session logs in
`.cargo-cicd/lifecycle/` (`session-1f9798ec-...ttl`, `session-test-hook-001.ttl`), then verified
the resulting chain with `hooks/dogfood-lifecycle-receipt-spotcheck.sh`:

```text
$ bash hooks/dogfood-lifecycle-session-end.sh          # -> appended 2 chained records (run 1)
$ bash hooks/dogfood-lifecycle-session-end.sh          # -> appended 2 more (run 2), chain continues
$ bash hooks/dogfood-lifecycle-receipt-spotcheck.sh
spotcheck: OK — 4 chained record(s) recompute correctly, head chain_hash=16a0015b...
```

Confirmed by hand that the 4 chained lines form a genuine chain (first `prev_chain_hash` =
genesis / 64 zeros; each subsequent `prev_chain_hash` equals the prior record's `chain_hash`):

```text
session-1f9798ec-...ttl  prev=0000...0000  chain=7eeb34bd...
session-test-hook-001.ttl prev=7eeb34bd...  chain=6de8e478...
session-1f9798ec-...ttl  prev=6de8e478...  chain=cee91fbd...
session-test-hook-001.ttl prev=cee91fbd...  chain=16a0015b...
```

Fail-closed confirmed with two induced-tamper tests against scratch copies of `receipts.jsonl`
(never against the tracked/real file): flipping the last record's `chain_hash` to `f`×64 made
`spotcheck` exit 1 naming that record's `chain_hash mismatch`; flipping an earlier record's
`prev_chain_hash` to `e`×64 also made `spotcheck` exit 1 (caught as a `chain_hash mismatch`,
since the tampered `prev_chain_hash` feeds the recompute). Restoring the backup made `spotcheck`
pass again (`diff` byte-identical to the pre-tamper backup).

Note: `.cargo-cicd/lifecycle/` is gitignored (see `.gitignore:39`), so the receipts written
during this verification run are local runtime data, not part of this change's tracked diff.

## Scope and named follow-ups

- **SHACL shape-conformance: live.** `ggen graph validate --files X --shapes Y` (landed in
  commit 523cc6e4, reusing praxis-graphlaw's existing `GraphLawStore::validate_shacl`) performs
  real SHACL constraint evaluation, not just Turtle parse validation. `hooks/dogfood-
  lifecycle-session-end.sh` now passes `--shapes shapes.ttl` on every invocation, so every
  captured session log is checked against `dfl:ToolEventShape`/`dfl:SessionShape` (session ref,
  tool name, ordering, outcome, agent, used/generated), not merely parsed. Verified this session:
  a well-formed fixture (`fixtures/session-good.ttl`) reports `shapes_conform: true`; a
  hand-built fixture with a `skos:notation` value outside the closed tool-name vocabulary
  (parse-valid, shape-invalid) makes both the raw `ggen graph validate --files ... --shapes ...`
  call and the wrapper script exit non-zero naming the focus node, source shape, and message
  text — not a bare parse error, proving `--shapes` is actually evaluated and not just present
  in the args list.
- **Receipt chain (production side, bash-only): done.** `hooks/dogfood-lifecycle-session-end.sh`
  appends a genuinely hash-chained record per validated session log —
  `payload_hash = blake3(canonical sorted-key JSON of {blake3, parse_valid, session_log,
  tool_events})`, `prev_chain_hash` = the previous record's `chain_hash` (or 64 `"0"` characters
  / genesis for the first chained record), `chain_hash = blake3(prev_chain_hash_hex ++
  payload_hash_hex)`. This is the **same shape** as ggen's own `.ggen-v2/receipt-log.jsonl` chain
  (genesis-seeded, each record feeds the next) but **not byte-compatible** with it: ggen's actual
  chain (`crates/praxis-core/src/receipt_record.rs` `recompute_chain_hash`,
  `crates/praxis-core/src/law.rs` `build_admission_frame`/`chain_from_frame`) hashes a 99-byte
  `OcelCausalFrame` carrying `instruction_id`, `node_kind`, `ts_ns`, `andon`, `obligation_count`,
  and packed object refs through `bcinr-powl-receipt`'s `chain()` call — reproducing that exactly
  needs Rust struct/byte-layout code, not `bash`+`jq`+`b3sum`. Verified this session: two
  consecutive runs of the upgraded script against the real logs in `.cargo-cicd/lifecycle/`
  produced 4 chained records whose `payload_hash`/`chain_hash`/adjacency all recompute correctly
  (see `hooks/dogfood-lifecycle-receipt-spotcheck.sh`), and two induced-tamper tests (a flipped
  `chain_hash`, a flipped `prev_chain_hash`) were both caught (non-zero exit, named record).
  Pre-upgrade flat-format lines in `receipts.jsonl` (written before this change) have no
  `chain_hash` field and are not retroactively chained; the chain begins fresh at genesis on the
  first post-upgrade record, and the script never rewrites a previously-written line
  (append-only).
- **Receipt chain (consumption side): still a named Rust follow-up.** A trustworthy
  `ggen receipt verify`/`ggen receipt history`-equivalent — a fail-closed CLI walk of the full
  chain, wired through `praxis-core`'s `ReceiptStore`/`ReceiptRecord` the way `cargo-cicd`'s OCEL
  ledger already is — has not been built. `hooks/dogfood-lifecycle-receipt-spotcheck.sh` is a
  **bash-only spot-check**, useful as a local smoke test, but it is explicitly **not** that trust
  boundary: it has no CLI ergonomics, no coverage of ggen's own richer `OcelCausalFrame` chain,
  and is not wired into any admission/gate path. Building the real equivalent is Rust work
  (new crate/module code) and is out of scope for this pack-only cycle.
- The closed tool-name scheme covers nine worker tools
  (`Bash Edit Write Read Grep Glob Task WebFetch WebSearch`); other tools are not captured.
- Outcome is the tool-level result (`dfl:Ok` unless the tool itself errored/was blocked); the
  underlying command exit lives inside the content-addressed `prov:generated` payload.

## Concurrency fix (found live, this session)

`hooks/dogfood-lifecycle-capture.sh` fires once per tool call, and Claude Code dispatches
concurrent tool calls (a batch, or parallel subagent tool use) as concurrent hook invocations.
The original version had two bugs under concurrency: (1) `seq=$(grep -c ...)` read the current
event count with no lock, so two concurrent invocations could read the same count and both write
the same `dfl:sequenceIndex`; (2) each event node was written via several separate `printf ...
>> file` calls, and separate `write(2)` calls from concurrent processes can interleave lines,
corrupting the Turtle block structure even when the sequence numbers happened to differ. Both
were observed live: this session's own accumulated `session-<id>.ttl` (5000+ lines, 541 events)
failed `ggen graph validate --files` with a genuine parse error at the exact point two
invocations' output had interleaved (confirmed by inspection: two `<urn:dfl:event:...:173>`
declarations, output from two hook invocations that read the same pre-lock sequence count).

Fixed with a portable `mkdir`-based lock (atomic, no external dependency — `flock` is not part
of base macOS) around the read-seq + append critical section, and by building each event node as
one string and appending it with a single `printf` (one `write(2)` call) rather than several.
Verified: 20 truly-concurrent invocations (`&` + `wait`) against a scratch session file produced
exactly 20 distinct sequence indices (no duplicates) and parsed cleanly; the normal sequential
path was re-checked unchanged and now also conforms to `shapes.ttl` under `--shapes`.

The real corrupted historical log was deliberately **not** hand-edited — `.cargo-cicd/lifecycle/`
is gitignored runtime data, and its `receipts.jsonl` already recorded `parse_valid:false` for
that log truthfully at the time; rewriting it to look clean would falsify the very audit trail
this pack exists to keep honest. The corruption predates and is unrelated to the concurrency fix
above; it is fixed going forward, not retroactively.

## Fence

Observation only. No class, predicate, or individual is named `authorize` / `permit` / `grant`
/ `actuate` / `execute`; `dfl:Blocked` records that a call did not complete, never a statement
about authority. Admission, permission, and receipts are separate governed surfaces.

## See also

- `ontology.ttl` — the vocabulary and its disclosed minted-term justifications
- `docs/releases/v26.7.13/` — the Operation Dogfood release documents
- `crates/ggen/src/verbs/handlers.rs` — `handle_graph_validate` (the multi-file validator) and
  `handle_receipt_verify`/`handle_receipt_history` (the chain-hash recompute discipline this
  pack's bash-only chain mirrors the shape of, but not the byte-level algorithm of)
- `crates/praxis-core/src/receipt_record.rs` — `ReceiptRecord::recompute_chain_hash`, ggen's
  actual (Rust-only) chain algorithm
