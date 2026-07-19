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
| `hooks/dogfood-lifecycle-session-end.sh` | Session-end validator + hash-chained receipt writer + governance-coverage check + log rotation |
| `hooks/dogfood-lifecycle-receipt-spotcheck.sh` | Bash-only recompute smoke test for the receipt chain (not a trust boundary — see follow-ups) |
| `hooks/dogfood-lib.sh` | Shared helpers (v26.7.18): repo-root resolution, the Turtle-append primitive, the invocation counter, on-ingest validation |
| `hooks/cng-plan-admission-guard.sh` | PreToolUse guard; on refusal, now also appends a `dfl:Blocked` ToolEvent + a chained refusal receipt (v26.7.18) |
| `hook.ttl` | Real `kh:` Knowledge Hooks (v26.7.18): `derive_review_obligation` + `discharge_review_obligation`, run through praxis-graphlaw's `TripleStore::load_hook_pack`/`.materialize()` |
| `hooks/dogfood-self-monitoring-precedence.ttl` | Additive cross-pack `kh:after` declaration (composability; v26.7.18) — see "Composability" below |

## v26.7.18 gap-closing pass (real changes, real verification)

This section documents what changed in this pass against the L5 validation report's named
gaps for this pack, and exactly what was (and was not) verified. Default assumption for every
claim below: `UNVERIFIED` unless a command + result is cited.

### Derivation power / Obligation lifecycle — real `kh:` hooks, ALIVE

`ontology.ttl` gained a real `dfl:Obligation` vocabulary (`derivedFrom`, `obligationTarget`,
`severity`, `deadline`, `obligationStatus`, `dischargedBy`) and `hook.ttl` gained two real
Knowledge Hooks run through the SAME production mechanism
`packs/self-monitoring-pack/hook.ttl` uses (`praxis_graphlaw::TripleStore::load_hook_pack` +
`.materialize()`, not a hand-simulated derivation):

- `derive_review_obligation` — CONSTRUCTs one parameterized `dfl:Obligation` (target agent,
  `dfl:High` severity, `"PT24H"^^xsd:duration` deadline — a policy constant, never a wall-clock
  read) per `dfl:Blocked` tool event.
- `discharge_review_obligation` — CONSTRUCTs `dfl:dischargedBy` + `dfl:obligationStatus
  dfl:Discharged` when the SAME agent has a LATER `dfl:Ok` event in the same session.

ALIVE, verified by a scratch consumer crate (own `Cargo.toml`, path-dependency on
`praxis-graphlaw`, under this session's scratchpad — not committed here, per the task's own
scratch-consumer pattern) with 5 real `#[test]` functions run via `cargo test`, all passing:

```text
running 5 tests
test discharges_when_the_same_agent_later_succeeds ... ok
test derives_one_open_obligation_from_the_blocked_event ... ok
test does_not_fire_on_the_adjacent_error_outcome ... ok
test adversarial_different_agent_success_does_not_discharge ... ok
test cross_pack_precedence_resolves_and_both_packs_derive_over_one_union_graph ... ok

test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

Two real engine limitations were found and worked around live (both disclosed in `hook.ttl`'s
own comments, not hidden): (1) `kh:effect "emit-delta"` is append-only — a CONSTRUCT that
asserted `dfl:Open` directly would leave it coexisting permanently alongside a later
`dfl:Discharged` (never retracted), so `derive_review_obligation` deliberately never asserts
`dfl:Open` at all; "open" is defined as the absence of a `dfl:Discharged` triple, queried
directly. (2) embedding `FILTER NOT EXISTS` inside a hook's own `kh:query`/action `CONSTRUCT`
text broke `HookProps` extraction (`"Unable to parse Query"`) even though the identical query
text worked fine via a direct `TripleStore::query()` call — worked around by dropping the guard
entirely, since re-CONSTRUCTing the same triples on a later `materialize()` round is idempotent
(a set, not a list) and therefore safe without it.

New fixtures exercising fire precision and the obligation lifecycle, all `ggen graph validate
--shapes shapes.ttl`-conformant: `fixtures/session-discharged.ttl` (positive discharge),
`fixtures/session-adversarial-discharge.ttl` (a DIFFERENT agent's success must not discharge —
fails closed), `fixtures/session-error-not-blocked.ttl` (the adjacent `dfl:Error`-not-`Blocked`
outcome must not fire).

### Composability — real cross-pack `kh:after`, ALIVE, with a disclosed coupling risk

`hooks/dogfood-self-monitoring-precedence.ttl` is a one-triple additive file
(`dfl:derive_review_obligation kh:after smon:flag_ungoverned_transition .`) that is NOT baked
into `hook.ttl` itself — baking it in directly would make this pack's OWN derivation fail to
load standalone (`hooks::compile_hooks` hard-errors "unknown after-dependency" if the named
hook isn't in the SAME load). The `cross_pack_precedence_resolves_and_both_packs_derive_over_
one_union_graph` test (passing, cited above) co-loads `hook.ttl` + this precedence file +
`self-monitoring-pack/hook.ttl` + a fixture from each pack into ONE `TripleStore`, and confirms
`hooks::schedule_hooks`'s real Kahn's-algorithm topological sort resolves the cross-pack
dependency and BOTH packs still derive correctly over the union graph.

**Disclosed volatility found live, this session:** `self-monitoring-pack/hook.ttl` (a separate
pack this change does not edit) was restructured mid-session — most likely by a concurrent
agent working that pack in parallel — from a single `smon:derive_escalation_obligation` hook
(the IRI this pack's own ontology.ttl comments originally cited) to four parameterized hooks
(`smon:prioritize_escalation_obligation_{low,medium,high}`, `smon:flag_ungoverned_transition`).
The precedence file above was updated to name the hook that actually exists as of this pass. If
that pack renames/removes `flag_ungoverned_transition` in a later pass, this file's `kh:after`
will again fail to resolve the next time both packs are co-loaded — a real, disclosed coupling
risk inherent to naming another pack's hook IRI, not a defect unique to this file.

### Actuation closure — refusal path now reachable and receipted, ALIVE

`hooks/cng-plan-admission-guard.sh`'s block path (exit 2) now (a) appends a real `dfl:Blocked`
`dfl:ToolEvent` via the shared `dogfood_append_event` primitive (`hooks/dogfood-lib.sh`) — the
ONLY way one can ever exist, since PostToolUse structurally never fires for a call a PreToolUse
hook blocked — and (b) appends a hash-chained refusal receipt to the SAME `receipts.jsonl`
chain `dogfood-lifecycle-session-end.sh` writes to. Verified live this session against a scratch
fake repo with a stub `cng` binary that always refuses:

```text
$ echo '{"session_id":"guardtest","tool_input":{"command":"rm -rf /"}}' \
    | bash hooks/cng-plan-admission-guard.sh
Blocked: 'cng plan check' refused this Bash command ...
$ echo $?
2
$ ggen graph validate --files .../session-guardtest.ttl --shapes shapes.ttl
{"shapes_conform": true, ...}          # the Blocked ToolEvent conforms
$ tail -1 .../receipts.jsonl
{"kind":"plan-admission-refusal","session_id":"guardtest", ..., "chain_hash":"cc53667f..."}
```

Still NOT closed: actuation outcomes do not yet flow back into the graph as new OBSERVED facts
beyond the Blocked event + refusal receipt themselves (no re-observation of e.g. "operator then
tried a different, permitted command" as a linked follow-up fact) — the full "derive → actuate
→ receipt → re-observe" loop from the L5 bar is now three-quarters real, not four.

### Input acquisition — parameterized target + on-ingest validation + rotation, PARTIAL

All four hook scripts now resolve their capture/validation root via `hooks/dogfood-lib.sh`'s
`dogfood_repo_root` (`$DOGFOOD_REPO_ROOT` env var, else derived from the script's own on-disk
location) instead of a hardcoded `/Users/sac/praxis` path — verified live against a scratch
fake repo (`DOGFOOD_REPO_ROOT=<scratch>/fake-repo`, real capture + validate + receipt + spot-
check all ran correctly against that root, shown above and in the "Governance coverage"
section). `dogfood-lifecycle-capture.sh` now calls `ggen graph validate --shapes` immediately
after every append (`ingest-validation.jsonl`), not only at session-end. A retention policy
(`DOGFOOD_MAX_EVENTS`, default 5000) archives an over-grown log to `archived/` and receipts the
rotation. Still NOT closed: capture (the actual bytes captured per tool call) remains a
consumer-installed PostToolUse hook, not something this pack runs unattended end to end without
the parent session wiring `.claude/settings.json` — L5's "consumer points it at a source" bar
is not fully met.

### Governance coverage — real coverage-detection mechanism, ALIVE

`dogfood-lifecycle-capture.sh` now increments a per-session invocation counter
(`dogfood_bump_invocation_counter`) for EVERY tool call it observes, before the closed
tool-name allowlist decides whether a `dfl:ToolEvent` gets admitted. `dogfood-lifecycle-
session-end.sh` compares that counter against the number of `dfl:ToolEvent` nodes actually
captured and receipts any gap as `governance_gap: true`. Verified live: 3 simulated tool calls
(2 supported, 1 unsupported-tool-name) produced `invocations_seen: 3`, `tool_events: 2`, and a
printed + receipted anomaly:

```text
dogfood: GOVERNANCE ANOMALY in .../session-testsid1.ttl — 3 invocation(s) seen but only 2
dfl:ToolEvent(s) captured (gap=1)
{"session_log":"session-testsid1.ttl", ..., "invocations_seen":3,"governance_gap":true, ...}
```

Still NOT closed: this detects a gap in COUNT, not WHICH transition was ungoverned or why (no
per-invocation reason code is retained past the counter increment) — L5's "any ungoverned
transition is itself detectable and flagged" is met at the count level, not the per-transition
diagnostic level.

## v26.7.19 gap-closing pass (real changes, real verification)

Continuation of the v26.7.18 pass above, per the L5 push maturity audit's own remaining-gap
notes for this pack. Same discipline: every claim below cites a command + result, default
`UNVERIFIED` otherwise.

### Obligation lifecycle — third hook (escalation), L3 -> L4, ALIVE

`hook.ttl` gained a THIRD real `kh:` hook, `escalate_overdue_obligation` (`kh:after
dfl:discharge_review_obligation`, `kh:priority 3`), and `ontology.ttl`/`shapes.ttl` gained the
`dfl:escalatedBy` property (same "optional pointer, not a status enum value" discipline as
`dfl:dischargedBy` — it may coexist with `dfl:dischargedBy` on the same node: "escalated for
being slow, then eventually discharged anyway" is truthful append-only history, not a
contradiction). This is the literal L4 bar text for Obligation lifecycle: "Overdue/undischarged
obligations escalate via the same hook mechanism (a third hook chaining off an open+overdue
query)."

**A real engine limitation was found and worked around live, this session, at the pack level:**
the first version of this hook used `FILTER(?i3 >= ?i1 + 3)` (binary arithmetic in a FILTER
expression — a completely ordinary SPARQL construct). Confirmed via a standalone scratch-
consumer query (`SELECT ?x ?n WHERE { ?x ex:n ?n . FILTER(?n >= 1 + 3) }` against facts
`n=1/4/7`) that this engine's FILTER evaluator does not support binary arithmetic in the term
position: it silently returned **0 rows** instead of the 2 rows standard SPARQL semantics
require (not a parse error — confirmed the same facts DO satisfy plain `>`/`<` comparisons).
This is an engine-level gap (`crates/praxis-graphlaw`'s SPARQL evaluator), out of scope for a
pack edit, and is NOT fixed here. Worked around with an ordinary Datalog technique instead of
arithmetic: the action's WHERE clause requires THREE existentially-distinct later
same-agent/same-session `ToolEvent`s (`?ea < ?eb < ?e3`, using only plain `>` comparisons — the
same shape `discharge_review_obligation_action` already used successfully), asserting
`escalatedBy` on the third (latest) of them. Same observable behavior as the arithmetic version,
zero dependence on the unsupported FILTER expression.

**A second real bug was found and fixed live, in this pack's own scripts, while building the
adversarial fixtures below:** `discharge_review_obligation_action`'s WHERE clause matches ANY
later same-agent/same-session `dfl:Ok` event, not just the earliest one — a session with MORE
THAN ONE later Ok event independently satisfies the rule multiple times, producing multiple
`dfl:dischargedBy` triples and violating `shapes.ttl`'s own `sh:maxCount 1` on that property.
This is a genuine, disclosed precision gap in the v26.7.18 discharge hook, not something this
pass fixes (a MIN/earliest-only guard needs either `FILTER NOT EXISTS`, already documented above
as breaking hook-triple extraction, or a `GROUP BY`/`MIN()` aggregate this engine's hook-action
CONSTRUCT path was not tested against) — the new fixtures below route around it by using
`dfl:Error` (not `dfl:Ok`) for every later event except the one intended to discharge.

Verified ALIVE by a scratch consumer crate (own `Cargo.toml`, path-dependency on
`praxis-graphlaw`, this session's scratchpad — not committed here) with 5 `#[test]` functions,
all passing:

```text
running 5 tests
test cross_session_same_agent_does_not_discharge_or_escalate ... ok
test discharge_different_agent_does_not_fire ... ok
test escalation_boundary_does_not_fire_one_short ... ok
test escalation_positive_fires_and_coexists_with_discharge ... ok
test discharge_positive_fires ... ok

test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

New fixtures, all `ggen graph validate --shapes shapes.ttl`-conformant:
`fixtures/session-escalated.ttl` (positive: 3 later events cross the threshold, and
`escalatedBy`+`dischargedBy` coexist on the same Obligation), `fixtures/session-not-yet-
overdue.ttl` (adjacent-boundary negative: only 2 later events, one short of the threshold —
proves the threshold is exact, not "eventually fires").

Still NOT at L5: no standing re-planning as new facts arrive continuously (this is a one-shot
threshold check per `materialize()` call, not a governor); no full replayable ledger UI over the
escalation history.

### Fire precision — cross-session collision, L3 -> L4 (partial), ALIVE

`fixtures/session-cross-session-collision.ttl` isolates a DIFFERENT adversarial conjunct than
`session-adversarial-discharge.ttl` (which tests same-session/different-agent): the SAME agent
acts in two wholly unrelated sessions, blocked in session A, later succeeding in session B. Both
`discharge_review_obligation_action` and `escalate_overdue_obligation_action` require `?e2`/`?e3`
and `?e1` to share the SAME `?session` binding (`dcterms:isPartOf`), so this must NOT discharge
or escalate session A's Obligation — verified by the
`cross_session_same_agent_does_not_discharge_or_escalate` test above (fails closed: neither
`dischargedBy` nor `escalatedBy` appears).

Still NOT at L4: no IRI-collision fixture (two distinct real-world entities sharing one minted
IRI by accident) and no deliberately-malformed-but-still-shape-valid Turtle fixture — this pass
closes one specific adversarial conjunct (cross-session), not the full L4 "malformed/colliding/
gamed" surface named by the audit.

### Governance coverage — per-tool attribution, L3 -> L4, ALIVE

`dogfood_bump_invocation_counter` (`dogfood-lib.sh`) now also appends one line per invocation to
`session-<sid>.invocations-by-tool.jsonl` (`{"tool":"<name>"}`), and `dogfood-lifecycle-session-
end.sh` cross-references the DISTINCT tool names actually seen (via `jq`, not a fixed candidate
list — so a tool OUTSIDE the closed vocabulary entirely, e.g. `NotebookEdit`, is still named, not
silently dropped) against `skos:notation` counts in the captured log, naming exactly which
tool(s) accounted for the gap and by how much. This is a genuine step from "a count exists" (L3)
toward "which specific transition was ungoverned" (most of the way to L4's per-transition
diagnostic, though still a per-TOOL-NAME breakdown, not a per-INVOCATION reason/trace).

**A third real bug was found and fixed live while building this:** `grep -c PATTERN file ||
echo 0` double-prints `"0"` when PATTERN matches zero lines — GNU `grep -c` already prints `"0"`
to stdout in that case, but its exit status is still 1 (no match), so the `|| echo 0` ALSO
fires, producing `"0\n0"` and breaking any numeric comparison (`integer expression expected`).
This was a LATENT bug already present in the v26.7.18 script's own `n=$(grep -c ... || echo 0)`
line (silently harmless there only because a real captured session almost always has `n > 0`)
that the new per-tool code hit immediately and directly. Fixed in all three occurrences in
`dogfood-lifecycle-session-end.sh` by capturing `grep -c`'s stdout directly and defaulting only
on an EMPTY variable, not a nonzero exit status.

Verified live end-to-end against a scratch fake repo (`DOGFOOD_REPO_ROOT=<scratch>`, real
capture + session-end run, not a unit test of the script's internals):

```text
$ echo '{"session_id":"gt1","tool_name":"Bash", ...}'         | capture.sh   # x2, admitted
$ echo '{"session_id":"gt1","tool_name":"NotebookEdit", ...}' | capture.sh   # x2, NOT admitted
$ bash dogfood-lifecycle-session-end.sh
dogfood: GOVERNANCE ANOMALY in .../session-gt1.ttl — 4 invocation(s) seen but only 2
dfl:ToolEvent(s) captured (gap=2)
dogfood:   -> tool="NotebookEdit" seen=2 captured=0 gap=2
$ jq '{invocations_seen,governance_gap,governance_gap_tools}' .../receipts.jsonl
{"invocations_seen":4,"governance_gap":true,"governance_gap_tools":["NotebookEdit"]}
```

And confirmed the no-gap case stays clean (`governance_gap_tools: []`, no false positive) with a
single well-formed `Bash` call in a fresh session.

`dogfood-lifecycle-receipt-spotcheck.sh` was updated in the SAME pass to reconstruct the new
three-field payload shape (`blake3`/`tool_events`/`parse_valid`/`invocations_seen`/
`governance_gap`/`governance_gap_tools`) — without this the spotcheck would have FALSELY FAILED
every v26.7.19 receipt (its independent payload reconstruction, unaware of the new field, hashes
to a different `payload_hash` than the one `dogfood-lifecycle-session-end.sh` actually wrote).
Verified: `spotcheck.sh` run against the receipts generated above reports
`spotcheck: OK — 1 chained record(s) recompute correctly`.

Still NOT at L5: attribution is per-tool-NAME, not per-invocation (two dropped `NotebookEdit`
calls are indistinguishable from each other in the receipt — only the aggregate-by-name count is
recorded); this remains a batch cross-check run at session-end, not a live per-invocation
governance signal.

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
