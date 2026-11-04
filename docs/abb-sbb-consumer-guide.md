# abb-sbb-consumer-guide

The end-to-end ABB/SBB consumer loop — author → plan → admit → manufacture →
verify → replay — with commands and real outputs. Every output block below was
either executed this session (exit code recorded) or is quoted verbatim from
the fleet's first real consumer run
(`docs/jira/v26.11.3/receipts/ea-run-transcript.md`, lane K6); nothing is
invented. Source of truth for behavior: `crates/ggen-abb-sbb/src/lib.rs`.

- **Date**: 2026-11-03
- **Work order**: `docs/jira/v26.11.3/WO-09-working-example.md`
- **Executable subject**: `crates/ggen-abb-sbb/fixtures/ea-graph.json`
  (schema `ggen.ea.graph.v1`)

## 1. Author the EA graph

An EA graph declares one strategy, its capabilities, one ABB per capability,
one contract per ABB, candidate SBBs (small black boxes — immutable
marketplace packs), qualifications binding SBB digests to contract digests,
and packs. Two authoring routes:

**JSON exemplar route (canonical, working today).** Author
`ea.graph.json` directly against the exemplar
`crates/ggen-abb-sbb/fixtures/ea-graph.json` (schema `ggen.ea.graph.v1`,
serde `deny_unknown_fields` — unknown fields refuse with
`MALFORMED_GRAPH`). This is the route every real consumer run to date used,
including K6's (`ea-run-transcript.md` §0: "Hand-built JSON route
(WO-08 interim route)").

Field table (each maps 1:1 to the serde struct in
`crates/ggen-abb-sbb/src/lib.rs`):

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Must equal `ggen.ea.graph.v1` |
| `strategy` | `{id, title}` | Exactly one |
| `capabilities` | `[{id, strategy}]` | Each names its strategy |
| `abbs` | `[{id, capability, contract}]` | Architecture building block |
| `contracts` | `[{id, abb, version, …}]` | Caps + ports — see (a) |
| `candidate_sbbs` | `[{id, abb, pack, digest, …}]` | See (b) |
| `qualifications` | `[{id, sbb, sbb_digest, …}]` | See (c) |
| `packs` | `[{id}]` | Pack ids disjoint from every element id (`PACK_CONFLATION` otherwise) |

(a) `contracts` fields: `id`, `abb`, `version`, `max_artifacts`,
`authority_ceiling` (NONE/SELECT/CONSTRUCT/DO — manufacture never exceeds
CONSTRUCT), `required_ports`.

(b) `candidate_sbbs` fields: `id`, `abb`, `pack`, `digest` (content
digest, §3), `mutable` (must be false), `authority`,
`provides_ports`, `artifacts` (`[{path, template}]`).

(c) `qualifications` fields: `id`, `sbb`, `sbb_digest`, `contract`,
`contract_digest`, `verdict` (QUALIFIED/REFUTED); digests must be
kernel-computed.

**Turtle route (schema landed; projection pending).** WO-04 landed the
canonical declarative schema —
`ontologies/shapes/ea-graph.shacl.ttl` (SHACL, `geag:` namespace, every
node shape `sh:closed true`, mirroring the serde structs field-for-field;
receipt `docs/jira/v26.11.3/receipts/g7-schema.md`) — plus the parity court
`crates/ggen-config/tests/ea_graph_schema_parity_test.rs`. WO-04 did NOT
land the Turtle→`ea.graph.json` projection rule (its receipt says the
projection is "a later WO"). Until it lands, the JSON exemplar route is the
only runnable path; author against the SHACL shapes if you author in
Turtle, then serialize by hand per the field table above.

Template placeholders inside `artifacts[].template` may use exactly seven
keys: `{{strategy}} {{capability}} {{abb}} {{sbb}} {{contract}}
{{contract_version}} {{graph_digest}}` — any other key refuses with
`UNBOUND_PLACEHOLDER` at admission (kernel `TEMPLATE_KEYS`,
`src/lib.rs:916`).

## 2. Plan per ABB

`plan(graph, abb_id, authority)` returns the `SELECT‖MANUFACTURE` decision —
a value, never an action. The decision law, verbatim from the kernel
docstring (`crates/ggen-abb-sbb/src/lib.rs:847`):

> Selection is deterministic: the lowest-id admissible candidate wins.

Candidates are sorted by id; each is probed through the full admission gate
in order. The first that admits wins and the decision is `Select`. If every
candidate refuses, the decision is `Manufacture`, which carries:

- `required_ports` — every port the contract requires;
- `missing_ports` — required ports NO candidate provides (a candidate
  refused for another reason still counts as providing its ports);
- `refusals` — EVERY per-candidate refusal, in candidate-id order. No
  refusal is dropped or summarized.

A real plan output showing a selection (from `fixtures/ea-graph.json`, both
candidates qualified — `Select` on lowest id `sbb:ingest-0000`; run this
session):

```text
$ cargo run -p ggen-abb-sbb --example validate_ea_graph -- \
    crates/ggen-abb-sbb/fixtures/ea-graph.json
# exit 0; plan decision table section:
  Select { abb: "abb:event-ingest", sbb: "sbb:ingest-0000", authority: Select }
```

A real plan output showing `Manufacture` with carried refusals (from K6's
first real consumer run over the ggen self-manufacture graph,
`ea-run-transcript.md` §2 — 3 of 5 candidate SBBs unqualified; every
per-candidate refusal carried, none dropped):

```text
Select { abb: "abb:docs", sbb: "sbb:mdbook-pattern-language-pack" }
Manufacture { abb: "abb:admission-kernel", refusals: [SbbUnqualified{..}] }
Manufacture { ... refusals: [SbbUnqualified{...}] }   (x3, one per refused candidate)
```

Plan authority gates: `NONE` refuses `INSUFFICIENT_AUTHORITY` (a request at
NONE cannot yield a SELECT decision); `DO` refuses `AUTHORITY_EXCEEDED`.

## 3. Admit at CONSTRUCT

Admission re-validates the graph, computes its canonical digest once
(`EaGraph::digest()` — order-canonical: element delivery order never moves
the digest), and refuses on the first failed gate. The engine's Stage 2c
call pins the subject:

- `expected_graph_digest: Some(ea_graph.digest())` — the request is bound
  to the exact parsed graph; a mutated graph between parse and admit
  refuses `GRAPH_DIGEST_MISMATCH { expected, actual }`.
- Envelope caps: `admit_with_caps(&graph, &req, caps)` with caps mapped by
  ggen-engine from the declared `res:Envelope` (`abb_envelope_caps`,
  SPARQL over `.specify/envelope-shapes.ttl` → `EnvelopeCaps{max_artifacts}`,
  clamped to `DEFAULT_MAX_ARTIFACTS = 64`). The effective artifact ceiling
  is `min(contract.max_artifacts, caps.max_artifacts)` — an override can
  only tighten a contract, never loosen it. `None` uses the default 64.

Typed refusal shapes when pinning or caps mismatch (verbatim error strings
from the kernel `Refusal` enum):

```text
GRAPH_DIGEST_MISMATCH: expected <expected>, actual <actual>
EXCEEDS_CONTRACT: <N> artifacts > max <M> — shrink the realization below the
  contract ceiling, or re-qualify against the current contract digest
```

Court witness: `pinned_digest_refuses_mutated_graph`
(`crates/ggen-engine/tests/abb_sbb_stage2c_wiring_e2e.rs`) refuses through
the real `admit_with_caps` seam on a mutated subject and passes on the
unmutated control (G1 receipt, court 3). Full listing of every refusal code:
`Refusal` enum, `crates/ggen-abb-sbb/src/lib.rs:221-314`.

Other admission gates, in order: graph validate (dangling refs, duplicate
ids, pack conflation) → digest pin → authority ≤ CONSTRUCT → SBB exists /
ABB match / digest known / not mutable / content digest matches
(`SBB_DIGEST_MISMATCH`) → qualification present, non-stale
(`STALE_QUALIFICATION`), contract unchanged (`CONTRACT_CHANGED`) →
artifact count ≤ ceiling, required ports provided → SBB authority ≤
contract ceiling → artifact paths safe+unique, placeholders bound.

## 4. Manufacture → receipt

`manufacture(&admitted, &generator)` compiles the admitted SBB into
deterministic artifacts — pure, writes nothing. Each artifact is prefixed
with a provenance header; the receipt is `ggen.abb-sbb.manufacture-receipt.v1`.

Provenance header anatomy (one line, prepended to every artifact body;
format from `src/lib.rs:1004`):

```text
ggen-provenance: graph=<graph_digest> strategy=<strategy.id>
  capability=<capability.id> abb=<abb.id>
  contract=<contract.id>@<contract_digest> sbb=<sbb.id>
  sbb_digest=<sbb content digest> generator=<generator.id>@<generator.version>
```

Each header field binds the artifact to exactly one admitted element:
`graph` pins the subject graph; `strategy`/`capability`/`abb`/`contract`
pin the architecture chain; `contract_digest` pins the contract's exact
bytes; `sbb_digest` pins the realization's content digest (so a later
`STALE_QUALIFICATION`/`SBB_DIGEST_MISMATCH` traces back to a changed
realization); `generator` pins who rendered it. Minted in `manufacture()`
from the sealed `Admitted` value — fields cannot be edited after admission
(sealed constructor, compile-fail falsifiers in the `Admitted` docstring).

Receipt field table (matches `pub struct ManufactureReceipt`,
`src/lib.rs:403-425`, field for field, `deny_unknown_fields`):

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Constant `ggen.abb-sbb.manufacture-receipt.v1` |
| `graph_digest` | string | Canonical digest of the admitted graph |
| `strategy` | string | Strategy id |
| `capability` | string | Capability id |
| `abb` | string | ABB id |
| `contract` | string | Contract id |
| `contract_digest` | string | Canonical digest of the contract |
| `sbb` | string | SBB id |
| `sbb_digest` | string | Kernel `sbb_content_digest` of the realization |
| `pack` | string | Pack id (identity space disjoint from ABB/SBB) |
| `qualification` | string | Qualification id admission was granted under |
| `generation` | u64 | Lease/receipt generation (`#[serde(default)]`; 0 = legacy) |
| `generator` | `{id, version}` | Generator identity bound into every receipt |
| `templates_digest` | string | Canonical digest of the sorted artifact specs |
| `outputs` | `[{path, digest}]` | Per-artifact sha256 digests |
| `authority` | authority | Always NONE (receipt itself grants nothing) |
| `ceiling` | authority | Always CONSTRUCT |
| `receipt_digest` | string | Canonical digest of the receipt with this field empty |

## 5. verify_receipt + replay

`verify_receipt(&receipt)` checks tamper evidence without
re-manufacturing: recomputes the receipt digest (all fields, with
`receipt_digest` empty) and refuses `RECEIPT_TAMPERED { declared, actual }`
on any edit; then pins `schema` and authority fields. A mutated field is
caught because it moves the canonical digest — every field is covered,
including every `outputs` entry.

`replay(&receipt, &graph, &generator)` re-admits and re-manufactures from
the graph, then compares exactly 9 fields in fixed order; the FIRST
mismatch refuses:

```text
REPLAY_MISMATCH: <field>
```

The 9 compared fields (kernel `replay`, `src/lib.rs:1097-1116`, in order):

1. `contract_digest`
2. `sbb_digest`
3. `qualification`
4. `generation`
5. `generator`
6. `templates_digest`
7. `outputs`
8. `pack`
9. `receipt_digest`

A `graph_digest` mismatch during replay's re-admission is also mapped to
`REPLAY_MISMATCH { field: "graph_digest" }`. The offending field is always
named — never a bare failure. Court: `crates/ggen-abb-sbb/tests/` refusal
crosswalk + falsifier tests (`cargo test -p ggen-abb-sbb --test
falsifiers`); the marketplace fixture pins the exact admitted projection:
`cargo test -p ggen-abb-sbb --test marketplace_fixture` →
`1 passed; 0 failed`, exit 0 (run this session).

## 6. envelope_court exit codes

`envelope_court(&Envelope)` (`crates/ggen-abb-sbb/src/envelope_court.rs`)
replays the receipt against the declared cap table and graph-digest
binding. The verdict maps to shell exit codes
(`ReplayVerdict::exit_code`, envelope_court.rs:113):

| Exit | Verdict | Meaning |
|---|---|---|
| 0 | `Pass` | Caps declared; measured ≤ ceiling; envelope digest binds receipt |
| 1 | `VacuousGate` | Receipt consumed an undeclared cap id — vacuity, NEVER a pass |
| 2 | `Refused` | `CAP_CEILING_MISMATCH` (declared < measured) or `ENVELOPE_GRAPH_MISMATCH` |

A court that cannot flip is FAIL. Non-vacuity is enforced: the court tests
mutate the cap table and require the verdict to change
(`crates/ggen-abb-sbb/tests/envelope_courts.rs`; run this session:
`cargo test -p ggen-abb-sbb --test envelope_courts` → `4 passed; 0
failed`, exit 0). Freshness semantics are NOT duplicated here — delegated
to `verify_receipt` (one calculus, one implementation; envelope_court.rs
module doc, WO-06 R8).

## 7. Sync integration

At a repo root with `ea.graph.json`, `ggen sync run` runs Stage 2c EA
admission (wired in `crates/ggen-engine/src/sync.rs` per WO-01/G1, CLI
flag per K8):

- Missing `ea.graph.json` refuses by default (fail-closed):
  `[FM-LAW-015]` naming the file and the `--skip-ea-admission` opt-out.
- A pack manifest that is unreadable or kernel-refused refuses
  `[FM-PACK-015]` (fail-closed block landed by K8; malformed no longer
  means skip).
- The opt-out is CLI-visible: `ggen sync run --help` lists
  `--skip-ea-admission` (K8 receipt, §Commands).

The refusal shapes as a reader actually sees them — real outputs from K6's
Stage 2c run (`ea-run-transcript.md` §3, scratch frontmatter subject, real
binary v26.10.12 built from the current tree):

```text
$ ggen sync run
# exit 1
[FM-LAW-015] EA SBB admission for 'sbb:docs-diataxis-pack' refused:
SBB_UNQUALIFIED: sbb:docs-diataxis-pack — qualify the SBB against
its contract, then re-admit
(direct kernel display form, one line:
"SBB_UNQUALIFIED: sbb:docs-diataxis-pack — qualify the SBB
against its contract, then re-admit")
```

```text
# graph copy reduced to the qualified SBB only:
$ ggen sync run
# exit 0; SyncReport JSON emitted (pipeline proceeded past Stage 2c)
```

With no `ea.graph.json` at the root, the default gate refuses naming the
flag (K8 court `sync_run_skip_ea_admission_flag_opt_out_of_fm_law_015`):
`[FM-LAW-015] EA admission gate ... --skip-ea-admission`; passing
`--skip-ea-admission` admits and writes.

## 8. The full loop, executable

Run from the repo root against the fixture. Every command below was run
this session with its real exit code; a fresh reader reproduces the loop
byte-identically from this section alone.

```console
$ cargo run -p ggen-abb-sbb --example validate_ea_graph -- \
    crates/ggen-abb-sbb/fixtures/ea-graph.json
schema ok: ggen.ea.graph.v1
validate: ok
graph_digest: sha256:76752facbad789df2fd7d11bbf2b1914a56bd232a2a63f4438c03a08cec828f5

-- contract digests --
  contract:ingest = sha256:e107ace33101539d8aac875844b63d488395e35ba572e009bc3a7b603dbf5fdf

-- sbb content digests --
  sbb:ingest-0000 = sha256:70e6782cb595b2868f61919b848ac161fd2e2ea67cb8f1b5a17475d59d2f0930
  sbb:ingest-0001 = sha256:c39b7f9e68a36050bb5da7c0f0ca7188e54239abbfab3f59885e2481218f1f78

-- admission probe (CONSTRUCT) --
  ADMITTED sbb:ingest-0000 (qualification qual:ingest-0000)
  ADMITTED sbb:ingest-0001 (qualification qual:ingest-0001)

-- plan decision table (SELECT) --
  Select { abb: "abb:event-ingest", sbb: "sbb:ingest-0000", authority: Select }
# exit 0
```

The validate example IS the loop's file→admission path: `parse_graph`
from disk → `validate` → kernel-computed digests for authoring
qualifications (§1) → per-SBB admission probe at CONSTRUCT with the graph
digest pinned (`expected_graph_digest: Some(g.digest())`, §3) → the
`plan` decision table (§2).

```console
$ cargo test -p ggen-abb-sbb --test marketplace_fixture
test result: ok. 1 passed; 0 failed; ... finished in 0.00s
# exit 0 — the exact admitted SBB projection (receipt §5) pins the fixture
```

```console
$ cargo test -p ggen-abb-sbb --test envelope_courts
test result: ok. 4 passed; 0 failed; ... finished in 0.02s
# exit 0 — envelope court verdicts, vacuity flips, exit-code mapping (§6)
```

The repo-root `ggen sync run` leg of the loop is BLOCKED on this tree by
two pre-existing integration gaps (below); the scratch-subject run in §7
is the witnessed Stage 2c execution.

## 9. Honest limitations (current as of 2026-11-03)

K6's first consumer run surfaced three blockers; current status, each
re-checked on disk this session:

1. **Chain rule legacy (K6 DEF-K6-1) — OPEN.** The repo ledger (`.ggen-v2/`
   record 198+) and head receipt still declare `chain_rule =
   praxis-chain/v2-fold`; the current engine (graphlaw 26.10.10+)
   recognizes only `graphlaw-chain/v2-fold` (const at graphlaw
   `src/receipt_chain/record.rs:461`). Verified open this session: the
   head `.ggen-v2/receipt.json` still carries the `praxis-chain` name.
   Effect: `ggen receipt verify` → FM-CHAIN-014; `ggen receipt history` →
   FM-CHAIN-007 at index 198; any repo-root sync refuses before Stage 2c.
   Remediation is the code's own message: re-seal from
   `ggen receipt history` / git.
2. **Declarative-path admission (K6 DEF-K6-2) — OPEN.**
   `/Users/sac/ggen/ggen.toml` classifies DeclarativeRules → `sync()`
   returns early via `generation_rules::run` (`sync.rs:369-371`), and
   `generation_rules.rs` contains zero `ea.graph`/`abb_sbb` references
   (grep count 0 this session) — Stage 2c EA admission does not run on
   the declarative-rules path even with a clean chain.
3. **CLI skip flag (K6 DEF-K6-3) — CLOSED by K8.** `--skip-ea-admission`
   is now on the `ggen sync run` verb surface, threaded to
   `SyncOptions.skip_ea_admission` (default false = refuse by default),
   with three real-binary courts in
   `crates/ggen-engine/tests/cli_boundary.rs` (32 passed; 0 failed per
   K8's receipt, including the SHIM-COURT flip witness).

Also noted honestly: WO-04 landed the SHACL schema and parity court but
NOT the Turtle→JSON projection rule (§1); and the WO-09 acceptance item
listing this guide in the milestone README is left to the coordinator
(this lane does not touch other docs).
