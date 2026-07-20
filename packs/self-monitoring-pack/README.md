# Self-Monitoring Pack

A vocabulary and a real Knowledge Hook for self-monitoring Claude Code's own answer-format
pattern, dogfooding the EXACT `kh:Hook` SPARQL-CONSTRUCT mechanism already proven this session
for the Solvane Global bribery case's obligation derivation
(`crates/multifractal-workflow/fixtures/bribery-case/hook.ttl` +
`crates/multifractal-workflow/tests/bribery_case_fixture.rs`), applied here to conversational
turns instead of compliance cases.

## The rule this hook encodes

```text
grounding_question(Q) ∧ same_system(Q, Q_prev) ∧ prior_response_was_survey(Q_prev)
  → derive(escalate_to_build)
```

Two turns are both tagged `smon:turnKind smon:GroundingQuestion` with the SAME
`dcterms:subject` (the task brief's "grounding_topic" — see ontology.ttl's header for why this
reuses Dublin Core's `dcterms:subject` directly rather than minting a bridge term), and the turn
immediately following the FIRST GroundingQuestion is tagged `smon:SurveyResponse` (not
`RunResponse`/`BlockerResponse`) — the hook derives an `smon:EscalationObligation` node linking
both turns with a `smon:reason` literal.

## CLASSIFICATION-IS-INPUT FENCE (read this first)

**The turn-kind classification is an INPUT to the hook, not something the hook derives.**
`hook.ttl`'s SPARQL CONSTRUCT query only ever READS `smon:turnKind`, `dcterms:subject`,
`smon:sequenceIndex`, and `smon:immediatelyFollows` — every one of those is a fixture
hand-asserted fact. The hook never reads turn TEXT (no turn-content literal is even modeled in
this vocabulary) and never classifies anything. Datalog/SPARQL-CONSTRUCT pattern-matches over
ALREADY-CLASSIFIED facts; it does not classify raw natural-language text. Building an NLP
classifier that reads Claude Code's actual output and assigns `smon:turnKind` is a **separate,
explicitly out-of-scope problem** this pack does not attempt, does not stub, and does not claim
to solve.

## Files

| Path | Role |
|---|---|
| `ontology.ttl` | Turn-lifecycle vocabulary (PROV-O / DCTERMS / SKOS + disclosed `smon:` terms) |
| `hook.ttl` | The real `kh:Hook` (SPARQL CONSTRUCT) that derives `smon:EscalationObligation` |
| `shapes.ttl` | SHACL shapes for `smon:Turn` and `smon:EscalationObligation` nodes |
| `fixtures/pattern-fires.ttl` | POSITIVE fixture: same-topic repeat GroundingQuestion after a survey-only response |
| `fixtures/pattern-does-not-fire.ttl` | NEGATIVE fixture: first response was a RunResponse, not a survey |
| `fixtures/pattern-different-topic.ttl` | NEGATIVE fixture: second GroundingQuestion is a different topic (same_system fails) |
| `fixtures/session-real.ttl` | REAL data: this session's own transcript (1720 turns, 10326 triples) |
| `fixtures/session-real-broad-topic.ttl` | DISCLOSED counterfactual (not the default) for the sensitivity check |
| `scripts/transcript_to_turtle.py` | Capture script: real `.jsonl` transcript → `smon:` Turtle (disclosed heuristic, not NLU) |
| `scripts/broaden_topic_experiment.py` | Disclosed counterfactual post-processor for sensitivity analysis only |

The live verification harness is real Rust integration tests, not part of this pack's own files
(packs are data, not crates): `crates/praxis-graphlaw/tests/self_monitoring_hook_actuation.rs`
(hand-built fixtures, `TripleStore` only) and
`crates/praxis-graphlaw/tests/self_monitoring_real_session_actuation.rs` (this session's real
transcript data, cross-validated through BOTH `TripleStore` and a real `oxigraph::store::Store`
running hook.ttl's CONSTRUCT text verbatim — see "Stage 3" below).

## Vocabulary design notes

- **Session reference reuses `dfl:Session`.** This pack does not mint its own `smon:Session`
  class; `dcterms:isPartOf` on a `smon:Turn` points at `packs/dogfood-lifecycle-pack`'s own
  `dfl:Session` — the same Claude Code session concept, applying this task's own
  IRI-reuse-across-files doctrine one level up, at the vocabulary layer.
- **`grounding_topic` is `dcterms:subject`, not a minted term.** Dublin Core's "the topic of the
  resource" fits exactly, including the task brief's "free-text or IRI tag" requirement (DCMI
  permits either a literal keyword or a controlled-vocabulary term as the value) — checked and
  used directly, no bridge term.
- **`smon:immediatelyFollows` exists because of a confirmed engine boundary, not preference.**
  Reading `crates/praxis-graphlaw/src/sparql/plan.rs::extract_expression` this session confirmed
  `Expression::Add`/`Subtract` are NOT matched (they fall into the `_ => PlanExpression::Done`
  catch-all) — so `FILTER(?ir = ?i1 + 1)` is not a proven capability of this engine. This pack
  instead mints a direct turn-adjacency edge and expresses "the turn immediately following X" as
  a plain graph-pattern join, staying entirely within confirmed-supported SPARQL (BGP joins +
  `Greater`/`NotEqual` comparisons — both already exercised live by
  `crates/praxis-graphlaw/tests/soc2_hook_actuation.rs` and the bribery-case hook).
- **`smon:EscalationObligation` uses a single blank node per firing — disclosed, bounded scope.**
  `crates/praxis-graphlaw/src/hooks/construct.rs::instantiate_term_pattern` echoes a CONSTRUCT
  template's blank-node label verbatim per solution row rather than minting a fresh blank node
  per binding (the same engine limitation `crates/multifractal-workflow/fixtures/bribery-case/
  hook.ttl`'s header already discloses). Unlike the bribery-case hook, which avoided blank nodes
  entirely, this hook's task-required reified node (linking both turns plus a reason literal)
  genuinely needs one. It is SAFE ONLY under the explicit constraint that at most one qualifying
  triple exists per `materialize()` call — true of every fixture here by design. A graph with two
  or more simultaneously-qualifying pairs in one `materialize()` pass would alias every pair onto
  the SAME blank node id (silent corruption, not merely cosmetic) — a named follow-up, not solved
  here.

## Verification (live, this session)

### Turtle/SHACL parse validity (`ggen graph validate --files`, multi-file capability)

```text
$ ggen graph validate --files ontology.ttl --files hook.ttl --files shapes.ttl \
    --files fixtures/pattern-fires.ttl --files fixtures/pattern-does-not-fire.ttl \
    --files fixtures/pattern-different-topic.ttl
{"files_checked": 6, ...}   # exit 0, every file parses; per-file BLAKE3 hash + quad count reported
```

### Real SHACL shape conformance (not just parse)

```text
$ ggen graph validate --files fixtures/pattern-fires.ttl \
    --files fixtures/pattern-does-not-fire.ttl \
    --files fixtures/pattern-different-topic.ttl --shapes shapes.ttl
{"files_checked": 3, "shapes_checked": 1, ...}   # exit 0, shapes_conform: true for all 3
```

Fail-closed confirmed with an induced-tamper negative control (against a scratch copy, never the
tracked fixture): replacing `smon:turnKind smon:GroundingQuestion` with an out-of-scheme
`smon:NotARealConcept` made validation exit 1, naming both offending focus nodes and the exact
violated shape message ("A turn must carry exactly one smon:turnKind from the closed 5-concept
scheme").

### Real SPARQL-CONSTRUCT hook actuation (`praxis_graphlaw::TripleStore::load_hook_pack` +
`.materialize()` + `.query()`, not a hand-simulated match)

```text
$ just test-bin self_monitoring_hook_actuation

running 3 tests
pattern-different-topic.ttl: derived EscalationObligation rows = []
pattern-does-not-fire.ttl: derived EscalationObligation rows = []
test hook_does_not_fire_when_the_second_grounding_question_is_a_different_topic ... ok
test hook_does_not_fire_when_prior_response_was_a_run_not_a_survey ... ok
pattern-fires.ttl: derived EscalationObligation rows = [("_:esc",
  "<.../fixtures/pattern-fires#turn-1>", "<.../fixtures/pattern-fires#turn-4>")]
derived smon:reason = "\"same grounding_topic asked twice by a GroundingQuestion turn, and the
  turn immediately following the first GroundingQuestion was only a SurveyResponse (not a
  RunResponse/BlockerResponse) -- escalate to an actual build/run rather than another survey\""
test hook_fires_on_repeat_grounding_question_after_survey_only_response ... ok

test result: ok. 3 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

The hook fires on `pattern-fires.ttl` (derives exactly 1 `smon:EscalationObligation`, correctly
linking `turn-1` as `smon:hasPriorGroundingQuestion` and `turn-4` as
`smon:hasGroundingQuestion`, with a `smon:reason` literal naming the pattern) and does NOT fire
on either negative fixture — each isolating a different conjunct of the rule
(`prior_response_was_survey` failing in `pattern-does-not-fire.ttl`; `same_system` failing in
`pattern-different-topic.ttl`).

## Stage 3: running the real hook against THIS session's real transcript

`crates/praxis-graphlaw/tests/self_monitoring_real_session_actuation.rs` loads
`fixtures/session-real.ttl` (extracted this session, live, from this session's own
`~/.claude/projects/-Users-sac-praxis/1f9798ec-....jsonl` transcript — 1720 turns, 10326
triples) into a REAL `oxigraph::store::Store` and runs `hook.ttl`'s literal
`smon:derive_escalation_obligation_action` CONSTRUCT query — extracted **verbatim** from
`hook.ttl` at test-run time (`extract_action_construct_query`, self-checked by
`construct_query_is_verbatim_hook_ttl_substring`), never re-typed — via oxigraph's own SPARQL
1.1 engine, which is wholly independent of `praxis_graphlaw::TripleStore`'s custom `sparql/`
planner+executor. Every finding below is cross-validated through BOTH engines.

### Primary result: zero firings on the real, unmodified session

```text
$ just test-bin self_monitoring_real_session_actuation
real session GroundingQuestion turns (seq, topic):
  [(1656, "status"), (1663, "cli-swarm"), (1703, "bribery-e2e-mfact-receipt-workflow")]
oxigraph CONSTRUCT over real session-real.ttl: 0 EscalationObligation node(s) derived
test real_session_default_heuristic_zero_escalations_oxigraph ... ok
TripleStore materialize() over real session-real.ttl: 0 EscalationObligation row(s)
test real_session_default_heuristic_zero_escalations_triplestore ... ok
```

This session really did contain 3 `GroundingQuestion`-shaped turns (`turn-1656` "so what is the
status", `turn-1663` "I want to know if it works, can it go from the CLI to arrazo to global
swarm?", `turn-1703`), and the real explicit user-frustration turn is really there too
(`turn-1681`, 2026-07-14T03:38:00Z: "ok the whole point is I keep telling you I want the end to
end of all of that... I just want you to finish the end to end"). But **the hook does NOT fire**
on the real data, because no two of the 3 `GroundingQuestion` turns share a `dcterms:subject`
under `transcript_to_turtle.py`'s default per-noun keyword tagger — this is a real, confirmed
**false negative** relative to what a human reading the transcript would call the same
underlying pattern. See "adversarial check (2)" below for the precise, investigated cause.

### Adversarial check (1): GroundingQuestion → RunResponse/BlockerResponse never fires

This session's real data happens to contain zero natural instances of a `GroundingQuestion`
immediately followed by a `RunResponse`/`BlockerResponse` (all 3 real `GroundingQuestion` turns'
immediate followers are `Other` or `SurveyResponse`), so
`adversarial_run_and_blocker_responses_never_fire_inside_real_session_graph` injects one
fully-isolated synthetic pair of each kind (reserved topic literals, never used by any real
turn) directly into the real 10326-triple graph and re-runs the SAME unmodified CONSTRUCT query:

```text
oxigraph CONSTRUCT over real session + adversarial Run/BlockerResponse pairs: 0 node(s)
TripleStore materialize() over real session + adversarial Run/BlockerResponse pairs: 0 row(s)
test adversarial_run_and_blocker_responses_never_fire_inside_real_session_graph ... ok
```

Both engines agree: correctly-handled grounding questions (followed by an actual run or a named
blocker) never trigger the obligation, even embedded inside the real, dense session graph, not
only in isolated hand-built fixtures.

### Adversarial check (2): the false negative, investigated (not silently under-reported)

Two independent, real causes were found, both confirmed by direct inspection of the actual
transcript text — not merely one:

1. **Topic-tag granularity.** `turn-1656`'s topic tags to `"status"`, `turn-1663`'s to
   `"cli-swarm"` — different canonical-keyword sets under the default per-noun tagger, even
   though both are the same underlying "does the whole system work end-to-end?" question. This
   session's own later turn (`turn-1698`, real, human-typed) names the pattern explicitly after
   the fact: *"the pattern across this conversation was: you'd ask a grounding question ('does
   this work end to end?', 'how far has this evolved?'), and I would answer it accurately but
   abstractly."*
2. **Turn-kind granularity — a second, independent cause, not just topic tagging.** `turn-1657`
   (the real reply to `turn-1656`) is a bulleted `**v26.7.13 status:**` recap of 13 commits — by
   any human reading, a survey/status response — but it matches none of
   `transcript_to_turtle.py`'s `SURVEY_PATTERNS` regexes (no `real_edge`/`crown-witness`/mermaid
   marker), so it classifies as `Other`, not `SurveyResponse`. Even a broadened topic tag alone
   would NOT have made the hook fire on the `(1656, 1663)` pair, because `hook.ttl`'s WHERE
   clause specifically requires `?resp turnKind SurveyResponse`, and `turn-1657` fails that
   check under the default heuristic.

Neither cause was fixed in the pack's default heuristic (see "what this does NOT prove" below —
fixing a text classifier is explicitly out of this pack's scope), but both are named precisely,
with the exact real turn numbers and exact real text, not hand-waved.

### Adversarial check (3): topic-tag (and turn-kind) sensitivity, demonstrated not just argued

`scripts/broaden_topic_experiment.py` is a disclosed, CLEARLY-LABELED counterfactual (never the
pack's default) that mechanically rewrites `session-real.ttl`: (a) upgrades any `Other`-tagged
turn immediately following a `GroundingQuestion` to `SurveyResponse` (it already failed the
run/blocker checks, so by elimination it was some kind of survey reply), and (b) collapses all 3
real `GroundingQuestion` turns' `dcterms:subject` to one canonical literal
`"e2e-capability-grounding"`. Re-running the SAME unmodified `hook.ttl` CONSTRUCT against this
counterfactual:

```text
$ just test-bin self_monitoring_real_session_actuation
oxigraph CONSTRUCT over session-real-broad-topic.ttl: 3 EscalationObligation node(s)
earliest derivable EscalationObligation repeat turn = seq 1663,
  real explicit frustration turn = seq 1681 (18 turns later in the real transcript)
test broadened_topic_experiment_fires_correctly_and_before_real_frustration_turn_oxigraph ... ok
```

Given correctly classified/tagged input, the SAME unmodified hook derives exactly the 3 pairs
implied by 3 mutually-same-topic `GroundingQuestion` turns — `(1656,1663)`, `(1656,1703)`,
`(1663,1703)` — and the earliest one (`turn-1663`) precedes `turn-1681`'s real explicit
frustration by 18 real turns (~14 minutes of real wall-clock session time, 03:24:04Z →
03:38:00Z). **This is the central finding of Stage 3: the Datalog/CONSTRUCT mechanism is
correct and would have derived the escalation obligation before the human had to say it
explicitly, given correctly classified input.** The gap is entirely upstream, in classification
granularity, not in the derivation rule.

This sensitivity check also surfaced a REAL, previously-only-theorized engine limitation for the
first time: `hook.ttl`'s own header already discloses that `TripleStore`'s CONSTRUCT engine
echoes the template's blank-node label (`_:esc`) verbatim per solution row instead of minting a
fresh node per row (standard SPARQL 1.1 semantics, which oxigraph correctly implements). Every
prior fixture only ever fired once per `materialize()` call, so this was never actually
exercised. On the 3-row-firing broadened counterfactual, it now is:

```text
TripleStore materialize() over session-real-broad-topic.ttl: 4 EscalationObligation row(s):
  [("_:esc", turn-1656, turn-1703), ("_:esc", turn-1663, turn-1703),
   ("_:esc", turn-1656, turn-1663), ("_:esc", turn-1663, turn-1663)]
test broadened_topic_experiment_via_triplestore_exhibits_disclosed_blank_node_aliasing ... ok
```

All 3 real pairs alias onto ONE `_:esc` blank node, and the resulting read-back is the
**cross product** of the 2 distinct priors × 2 distinct repeats that node accumulated — 4 rows,
including a nonsensical self-referencing `(turn-1663, turn-1663)` pair that never existed in any
individual firing. This is real, verified data corruption, exactly matching (and now confirming
empirically, not just theoretically) `hook.ttl`'s own disclosed "SAFE ONLY... at most ONE
qualifying triple per `materialize()` call" caveat. `oxigraph::store::Store` — a real, standard
SPARQL 1.1 engine — does not have this bug; `TripleStore` does.

## What this proves

Given correctly classified input facts (`smon:turnKind`, `dcterms:subject`,
`smon:immediatelyFollows` already asserted), the SPARQL-CONSTRUCT Knowledge Hook mechanism
correctly and deterministically derives the "repeated grounding question after a survey-only
response" escalation pattern — confirmed on hand-built fixtures (Stage 1) AND, this session, on
this session's own real transcript data under a disclosed counterfactual classification (Stage
3's adversarial check 3) — cross-validated by two independent SPARQL engines
(`praxis_graphlaw::TripleStore` and a real `oxigraph::store::Store`), and shown to correctly
reject the pattern when a genuine conjunct is missing (adversarial check 1), even embedded
inside a real, dense, 10000+-triple session graph.

## What this does NOT prove or achieve

- **Not an automatic, unbypassable, general classifier of arbitrary future conversations.**
  `transcript_to_turtle.py`'s turn-kind and topic-tag classification remains a disclosed, simple,
  auditable regex/keyword heuristic — not a solved NLU problem. It has confirmed real false
  positives (e.g. `turn-1703`, tagged `GroundingQuestion` by matching `\bdoes it work\b` inside
  quoted reflective prose, not a fresh question) and confirmed real false negatives (`turn-1656`
  vs `turn-1663`'s topic split; `turn-1657`'s missed `SurveyResponse` tag — both documented above
  with exact turn numbers and exact real text).
- **Nothing currently wires this to fire automatically mid-session.** Claude Code has no native
  hook point that classifies a raw text turn the way `PostToolUse` fires on a tool-call event —
  there is no "PostTurn" or "PostResponse" hook in this tool today. This pack, as built, is a
  **post-hoc analysis tool** run manually against a saved `.jsonl` transcript after the fact, not
  a live guardrail that could have interrupted this session in real time.
- **Not a live-firing safeguard.** Even with a perfect classifier, nothing in this pack today
  would surface `smon:EscalationObligation` to the user or the model mid-conversation; it is a
  derivation over already-captured facts, read back by a human or a separate process after the
  session (or the relevant span of it) has already happened.
- **The blank-node-aliasing engine limitation is real, not merely disclosed-but-hypothetical.**
  Confirmed above under adversarial check (3): `TripleStore`'s CONSTRUCT engine corrupts results
  under genuinely multi-row firing. A production deployment MUST use either a per-pair minted
  IRI scheme (not built here) or a real SPARQL-1.1-compliant engine (`oxigraph`, demonstrated
  above to behave correctly) — not `TripleStore` as-is, if more than one pair can ever
  simultaneously qualify.

## Honest next step, if someone wanted to close the remaining gap

Closing "does not prove" item 2 (no live mid-session guardrail) would require two separate,
currently-unbuilt pieces, neither faked or stubbed here: (a) a materially better turn
classifier — realistically an LLM-based classifier call (not a regex list) that assigns
`smon:turnKind`/`dcterms:subject` per turn with real confidence, since this session's own
findings show a fixed keyword list both over- and under-fires; and (b) an actual Claude Code
hook point that fires after each assistant turn is produced (not merely `PostToolUse`, which
only sees tool invocations, never freeform text turns) — which does not exist in this tool today
and would need to be proposed/built upstream, not assumed. Until both exist, this pack remains
exactly what Stage 1 scoped it as: a real, verified derivation mechanism proven against
already-classified facts, now also proven against a real session's facts once those facts are
classified correctly — not a running guardrail.

## Scope and named follow-ups

- **Turn-kind classification is entirely out of scope**, by design — see the
  CLASSIFICATION-IS-INPUT FENCE above. No classifier exists in this pack.
- **Single-firing-per-`materialize()` constraint on the blank-node CONSTRUCT** — see "Vocabulary
  design notes" above. A production deployment processing a real, long-running session log with
  potentially multiple simultaneous qualifying pairs would need a per-pair minted IRI scheme
  (not built here) rather than the shared blank-node label this pack's fixtures are scoped to.
- **`smon:immediatelyFollows` is itself an input a capture hook would need to assert** (mirroring
  how `packs/dogfood-lifecycle-pack`'s capture hook assigns `dfl:sequenceIndex`) — no such
  capture hook exists yet for conversational turns; this pack is the vocabulary + derivation
  hook only, not the capture wiring.

## L5-push additions (this pass) and honest re-score

Real, verified additions made against `docs/packs/L5_VALIDATION_REPORT.md`'s named per-dimension
gaps for this pack (all commands below actually run this pass, not narrated):

- **Derivation power (L2 -> L3):** `ontology.ttl` adds `smon:severity`/`smon:Severity`
  (Low/Medium/High) and `smon:deadlineHint`/`smon:DeadlineHint` (ActNow/NextSession) to
  `smon:EscalationObligation`. `hook.ttl`'s three `smon:prioritize_escalation_obligation_{low,
  medium,high}` hooks (replacing the old unparameterized `smon:derive_escalation_obligation`,
  removed to avoid a duplicate-node regression — see hook.ttl's own note) compute severity from a
  REAL `COUNT` aggregate (via a sub-`SELECT ... GROUP BY`) over how many same-topic
  `GroundingQuestion` turns exist in the session — confirmed live against the real
  `praxis_graphlaw::TripleStore` engine (COUNT, GROUP BY, sub-SELECTs-in-WHERE, and `>=` all
  verified to evaluate correctly via a throwaway scratch harness this pass, never committed).
  **Still not L4/L5:** no obligation *chaining* into ordered discharge plans, and no standing
  re-derivation as new turns arrive live — still one `materialize()` per invocation.
- **Obligation lifecycle (L1 -> L3):** `smon:status`/`smon:ObligationStatus`
  (Open/Discharged/Refused), `smon:dischargedAt`, `smon:refusalReason` added.
  `shapes.ttl`'s `EscalationObligationShape` enforces "Discharged requires dischargedAt" /
  "Refused requires refusalReason" as **pure SHACL Core** (`sh:or` + `sh:not` + `sh:hasValue`) —
  an earlier draft used `sh:sparql`, which this pass discovered is a **silent no-op**
  in this engine (`SHACL_SPARQL_BOUNDARY = "CORE_ONLY"` in
  `crates/praxis-graphlaw/src/shacl/model.rs` rejects every `sh:sparql` constraint at load time —
  a real, confirmed engine limitation, not a bug in this pack's shape). `queries/
  open_obligations.rq` is a real, checked-in, engine-verified query for open obligations (its own
  header discloses a second real engine finding: `ORDER BY` on this engine's `query()` path
  silently returns zero rows, so it is intentionally omitted rather than shipped broken).
  `fixtures/pattern-fires-discharged.ttl` demonstrates the Discharged state and its enforced
  constraint (a real induced-tamper test: deleting `smon:dischargedAt` makes `ggen graph
  validate --shapes` fail closed, confirmed live). **Still not L4/L5:** no overdue-obligation
  escalation, and no hook-driven closed loop from discharge back into new facts.
- **Governance coverage (L1 -> L3, one governed pattern -> two, plus a catch-all):**
  `smon:UngovernedTransition`/`smon:hasUngovernedQuestion`/`smon:hasUngovernedResponse` (ontology)
  + `smon:flag_ungoverned_transition` (hook.ttl) positively detect a `GroundingQuestion` turn
  whose response is `smon:Other` — neither the one pattern this pack governs nor the two kinds
  already proven (README's adversarial check 1) never to warrant escalation.
  `fixtures/pattern-ungoverned.ttl` proves it fires (confirmed live, this pass, against the real
  engine) and does not false-positive on any other fixture. **Still not L5:** scoped to the one
  process this pack models (grounding-question/response), not literally every transition in
  Claude Code's behavior — a real, positive, narrow catch-all, not total coverage.
- **Actuation closure (L1 -> L2, partial):** `scripts/actuate_escalation.py` is a real, shipped
  actuation script: loads ontology+hook+input, extracts every `kh:Action`'s CONSTRUCT text
  VERBATIM from `hook.ttl`, executes it via `rdflib`'s independent SPARQL 1.1 engine (this pack's
  established dual-engine-verification convention), and writes a receipt JSON (SHA-256 hash of
  derived triples, timestamp, per-hook fire/no-fire) — confirmed live against
  `fixtures/pattern-fires.ttl` (fires), `fixtures/pattern-does-not-fire.ttl` (refuses to fire,
  receipted `any_hook_fired: false`), `fixtures/pattern-ungoverned.ttl` (the catch-all fires),
  and a missing-input-file case (real refusal path, `"outcome": "refused"`, exit 1, receipt still
  written). **Still not L3/L4/L5:** this script must still be invoked by a human/CI job — there
  is no live, unattended trigger, and no re-observe step feeding the receipt back into the graph
  as a new fact (the closed-loop bar). Named accurately as a step toward L3, not a claim of L3.
- **Input acquisition (L2 -> L3, partial):** `scripts/capture_and_validate.sh` chains
  `transcript_to_turtle.py` (capture) directly into `ggen graph validate --shapes` (admission) as
  ONE command — closing the exact gap the audit and maturity doc both named ("transcript_to_
  turtle.py exists but isn't wired"). Confirmed live against a real transcript this pass (capture
  succeeds, `shapes_conform: true`). **Still not L4/L5:** one invocation per transcript, not a
  continuous/SHACL-gated-on-ingest daemon, and the pack does not yet own retention policy.
- **Fire precision (L4, evidence strengthened, not yet L5):**
  `scripts/measure_fire_precision_multi_session.py` extends README's Stage 3 analysis (previously
  ONE archived transcript, analyzed once) across 4 additional real, independently-dated
  `-Users-sac-ggen` session transcripts this pass — `fixtures/precision_report_multi_session.json`
  is the real, checked-in output (313 real turns across 4 sessions; zero false-fire regressions —
  the hook correctly stayed silent on all 4, consistent with the original single-session finding
  that it under-fires on unfixed topic-tag/turn-kind granularity, never over-fires). **Honestly
  still not L5:** this is real firing counts over multiple real dated sessions (genuine "over
  time" data, not a repeated snapshot), but it does NOT include independent human-verified
  ground-truth labels for these 4 additional sessions the way Stage 3 did for the original one —
  a true precision/recall metric needs that labeling, which this pass's time budget did not
  cover. See the script's own module docstring for this exact scope limit.
- **Composability (blocked, not attempted as a fake):** the audit's own next-step names
  `dogfood-lifecycle-pack` as "the natural candidate" for a real co-fire test — but that pack has
  **zero `kh:Hook` definitions** (confirmed: `find packs/dogfood-lifecycle-pack -name hook.ttl` →
  empty; its `hooks/` directory is shell scripts, not `kh:` Turtle), so there is no second hook to
  co-fire with anywhere in this repo. Building one would require editing
  `packs/dogfood-lifecycle-pack/`, explicitly off-limits for this task (another agent may be
  working on it concurrently). No RDF precedence facts were added pointing at a nonexistent hook
  — that would be exactly the "fake it" failure this exercise exists to catch. Composability
  stays at its prior level; the concrete blocker is named, not hidden.

## L5-push, round 3: what changed and honest re-score

Round 2's composability blocker above is now **stale**: `dogfood-lifecycle-pack` gained two real
`kh:Hook` definitions in the same overall pass (`hook.ttl`, `derive_review_obligation` +
`discharge_review_obligation`) plus a one-triple additive `hooks/dogfood-self-monitoring-
precedence.ttl` declaring `dfl:derive_review_obligation kh:after smon:flag_ungoverned_transition`
— a real cross-pack reference to THIS pack's hook IRI. Rather than trust that pack's own README
narration of a co-fire test in an uncommitted scratch consumer, this round independently
reproduced the claim from scratch:

- **Composability (L2 → L3, independently verified, not merely cited):** built a fresh scratch
  consumer (own `Cargo.toml`, path-dependency on `praxis-graphlaw`, under this session's
  scratchpad, not committed here) that concatenates `self-monitoring-pack/hook.ttl` +
  `dogfood-lifecycle-pack/hook.ttl` + `dogfood-lifecycle-pack/hooks/dogfood-self-monitoring-
  precedence.ttl` into ONE hook pack, loads both packs' `ontology.ttl`, then both packs'
  `fixtures/pattern-fires.ttl` / `fixtures/session-discharged.ttl`, and calls the real
  `TripleStore::load_hook_pack` + `.materialize()` + `.query()` pipeline. Confirmed live, this
  pass:
  ```text
  smon:EscalationObligation rows: 1
  dfl:Obligation rows: 1
  dfl:Obligation Discharged rows: 1
  PASS: both packs' hooks derive correctly over one union graph, with the cross-pack
  kh:after precedence triple loaded and resolved.
  ```
  This is the literal L3 bar text: "Proven co-fire behavior: hooks from ≥2 packs derive correctly
  over one union graph." **Still not L4:** this pack's own `smon:` hooks do not yet reference
  `dfl:`'s derived facts BY DESIGN (obligation chaining across packs, the L4 bar) — the only
  cross-pack link that exists is the OTHER pack's `kh:after` naming one of THIS pack's hook IRIs
  for scheduling order, not a fact-level reference either pack's CONSTRUCT actually consumes.
- **Obligation lifecycle (L3 → L4):** added `smon:escalate_overdue_obligation` (`hook.ttl`), a
  new `kh:Hook` — the SAME derivation mechanism every other hook in this file uses, not a new
  code path — that fires when a High-severity, `smon:deadlineHint smon:ActNow`
  `smon:PrioritizedEscalationObligation` is still `smon:status smon:Open` at a point where the
  session has moved on to a later turn (i.e. never discharged/refused before the ActNow deadline
  was structurally violated). CONSTRUCTs a new `smon:OverdueEscalationObligation` linked back via
  the new `smon:escalates` property (both new in `ontology.ttl`, plus a matching
  `smon:OverdueEscalationObligationShape` in `shapes.ttl`). This is the literal L4 bar text:
  "Overdue/undischarged obligations escalate via the same hook mechanism."
  **A real engine limitation was found and worked around, disclosed in `hook.ttl`'s own
  comments:** an earlier draft computed "the latest turn's index" via a `MAX(...)`
  aggregate subquery and `FILTER`ed against it — this ALWAYS returned zero rows, because the
  aggregate's output binding was an untyped plain literal rather than `xsd:integer`-typed,
  silently breaking the numeric `>` comparison. Confirmed via a throwaway scratch harness
  (isolated `SELECT`-only queries, no hook engine involved) before touching `hook.ttl`. Worked
  around by dropping the aggregate for a direct existential match (`?later` with
  `?i3 > ?i2`), confirmed live to fire. `fixtures/pattern-overdue.ttl` (new) is a real hook-
  derivation INPUT fixture (not hand-asserted) proving the positive fire; the pack's 4
  pre-existing fixtures (`pattern-fires.ttl`, `pattern-does-not-fire.ttl`,
  `pattern-different-topic.ttl`, `pattern-ungoverned.ttl`) were all re-run and confirmed to
  produce ZERO `OverdueEscalationObligation` rows (no false positive):
  ```text
  pattern-overdue.ttl: High/Open PrioritizedEscalationObligation rows = 3
  pattern-overdue.ttl: OverdueEscalationObligation rows = 3
  pattern-fires.ttl: OverdueEscalationObligation rows = 0
  pattern-does-not-fire.ttl: OverdueEscalationObligation rows = 0
  pattern-different-topic.ttl: OverdueEscalationObligation rows = 0
  pattern-ungoverned.ttl: OverdueEscalationObligation rows = 0
  PASS: escalate_overdue_obligation fires exactly on the overdue case and nowhere else.
  ```
  (The multiplicity of 3 mirrors this pack's own pre-existing severity hooks' combinatorial
  behavior over multiple same-topic question pairs — not a bug unique to this hook.) **Still not
  L5:** this is one `materialize()` per invocation over a static input, not a standing governor
  that re-plans/re-escalates continuously as new turns arrive live.
- **Actuation closure (partial progress toward L4, disclosed scope):** `scripts/
  actuate_escalation.py` gained `--emit-receipt-graph` (renders the same JSON receipt as a new
  `smon:ActuationReceipt` RDF individual — new class + `smon:receiptHash`/`smon:actuatedAt`/
  `smon:anyHookFired`/`smon:actuatedInput` properties, all new in `ontology.ttl` — in a NEW
  Turtle file, never appended to `hook.ttl`/`ontology.ttl` themselves) and `--verify-round-trip`
  (re-loads `ontology.ttl` + the original input + that new receipt file into a FRESH `rdflib`
  graph and confirms the receipt fact is present and queryable — the actual re-observe step, not
  narrated). Confirmed live against `fixtures/pattern-fires.ttl`:
  ```text
  "receipt_graph_file": ".../pattern-fires-20260719T055317Z.receipt.ttl",
  "round_trip": { "round_trip_verified": true, "actuation_receipt_count": 1 }
  ```
  This is real progress toward the L4 bar ("Actuation outcomes flow back into the graph as new
  facts"), honestly scoped: it closes the loop for the RECEIPT record itself (which hooks fired,
  a hash, a timestamp), not per-derived-individual linkage back to the SPECIFIC blank-node
  obligation actuated — Turtle blank-node labels are not stable/referenceable across separate
  file parses (the same disclosed constraint `fixtures/pattern-fires-discharged.ttl`'s header
  already names). **Still not L4/L5:** no unattended trigger (a human/CI job must still invoke
  the script), and the receipt-graph fact is not automatically fed back into a LIVE session's
  graph, only proven round-trippable in a fresh, offline re-parse.

**Verification commands actually run this round** (not narrated): `cargo test -p praxis-graphlaw
--test self_monitoring_hook_actuation --test self_monitoring_real_session_actuation` (all 9
pre-existing tests still pass, unchanged, after every edit above); `ggen graph validate` against
`ontology.ttl`/`hook.ttl`/`shapes.ttl`/`fixtures/pattern-overdue.ttl` (parse-valid, and
`shapes_conform: true` against the new shape); a scratch `cargo run` composability binary and a
scratch `cargo run --bin overdue_check` binary (both under this session's scratchpad, not
committed here); and the `actuate_escalation.py --emit-receipt-graph --verify-round-trip`
invocation shown above.

## Fence

Observation/derivation only. No class, predicate, or individual is named `authorize` / `permit`
/ `grant` / `actuate` / `execute`; `smon:EscalationObligation` records a pattern-match finding,
never a compliance verdict or an instruction actually carried out.

## See also

- `ontology.ttl` — the vocabulary and its disclosed minted-term justifications
- `hook.ttl` — the `kh:Hook` definition and its engine-limitation disclosures
- `crates/praxis-graphlaw/tests/self_monitoring_hook_actuation.rs` — hand-built-fixture live
  verification harness (Stage 1)
- `crates/praxis-graphlaw/tests/self_monitoring_real_session_actuation.rs` — real-session,
  dual-engine (`TripleStore` + real `oxigraph::store::Store`) live verification harness (Stage 3)
- `scripts/transcript_to_turtle.py` / `scripts/broaden_topic_experiment.py` — the capture script
  and the disclosed sensitivity-analysis counterfactual, both documented in their own module
  docstrings
- `crates/multifractal-workflow/fixtures/bribery-case/` — the precedent this pack mirrors
- `packs/dogfood-lifecycle-pack/` — the sibling pack whose `dfl:Session`/`dfl:Outcome` patterns
  this pack's `dcterms:isPartOf`/closed-SKOS-scheme design directly follows
