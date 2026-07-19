# M&A Case Study — Standing Reconciliation

Mirrors `docs/releases/v26.7.14/THESIS.md` Section 35.9's Fortune-5 crown claim-reconciliation
table shape, scoped to this pack. Per Section 33.12, "The M&A case is PLANNED future work, not
v26.7.14 implementation standing" — this table exists to record real progress made toward that
future work in one session without rounding the overall case study up to ALIVE. Per Appendix
N.6 item 2, "every component being real implies the full chain is real" is a claim this document
must refuse: M&A-C1 through M&A-C3 being real does not make M&A-C4 through M&A-C6 real, and this
table does not imply otherwise.

## M&A-C1 .. M&A-C6 claim table

| ID | Exact claim | Standing this session | Promotion evidence |
|---|---|---|---|
| M&A-C1 | Public-vocabulary fit determined for the M&A domain's 10 candidate concepts | ALIVE, narrowly: vocabulary-fit research completed for 10 concepts against FIBO, GLEIF LEI RDF, OMG Commons, W3C ODRL, and PROV-O | `ontology.ttl`'s header (grounding method + per-term file:line citations into the vendored FIBO copy at `crates/praxis-graphlaw/ontologies/industry/financial/fibo-master/`); `pack.toml`'s `description` field; `COMPETENCY_QUESTIONS.md` (10 numbered concepts, each tagged CLEAN_FIT/STRAINED_FIT/bridge term) |
| M&A-C2 | Ontology + SHACL shapes pack built and validates | ALIVE, narrowly: `ontology.ttl` and `shapes.ttl` exist, are well-formed Turtle, and `ggen graph validate` reports `shapes_conform: true` against the main case fixture; the adversarial-negative fixture correctly fails with 2 named violations | `packs/ma-case-study-pack/{ontology.ttl,shapes.ttl}`; `ggen graph validate --files packs/ma-case-study-pack/fixtures/case.ttl,packs/ma-case-study-pack/ontology.ttl --shapes packs/ma-case-study-pack/shapes.ttl` → `{"files_checked": 2, "shapes_checked": 1, ...\"shapes_conform\": true}` for both files (re-run this session); same command against `adversarial-negative.ttl` → `SHACL validation failed, 2 violation(s)` (hasOwnershipPercentage range, missing `cmns-cls:isClassifiedBy`), matching the violations the fixture's own header discloses |
| M&A-C3 | Case fixture + Knowledge Hook built and tested | ALIVE, narrowly: one hook (`derive_ma_regulatory_filing_obligation`) fires correctly over the main case and the adversarial-positive fixture, and correctly does not fire over the adversarial-negative fixture; a fourth test confirms the hook-declared obligation catalog individual's closed property set | `crates/praxis-graphlaw/tests/ma_case_hook_actuation.rs`; `just test-bin ma_case_hook_actuation` → `test result: ok. 4 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out` (re-run this session, output byte-identical to the session's original run) |
| M&A-C4 | PDDL8 compliance / deal-progression planning model exists for the case | PLANNED — not built. `pddl-domain.ttl` (313 lines) DOES exist as disconnected RDF data, but no projector, grounder, or solver step is wired to it anywhere in the repo. **Correction (2026-07-19, L5-push pass):** the file's own header previously cited `crates/multifractal-workflow/tests/ma_case_pddl.rs` as live proof of a real SPARQL-extraction → PDDL8 → grounder/solver round-trip; `crates/multifractal-workflow` does not exist anywhere in this workspace (confirmed this session), so that citation was false. The file's header was corrected in place to disclose this rather than removed | None. Would require the same `sc:hasObligation` RDF-triple → `pddl:init` PDDL atom-literal projection bribery-case's own `DESIGN.md` documents as its Stage 2, applied to `ma:hasRegulatoryFilingObligation` and the other 5 processes' derivable facts, PLUS standing up the currently-nonexistent `crates/multifractal-workflow` extraction/grounding pipeline itself — engine-level shared infrastructure this pack cannot safely build in isolation |
| M&A-C5 | POWL v2 + Arazzo + Erlang/OTP dispatch chain exists for the case | PLANNED — not built. No POWL projection, no Arazzo workflow document, no Erlang/BEAM dispatch path exists for this case, matching zero of the Fortune-5 crown's F5-C6/F5-C7/F5-C8 structure | None |
| M&A-C6 | Multi-party Little's Law queue observation (`L_j = λ_j W_j`) across the 6 concurrent processes | PLANNED — not built. `case.ttl` asserts static instance data across all 6 processes named in Section 33.12 (buyer diligence, seller disclosure, financing, regulatory review, board authority, shareholder action) at a single point in time; no time-series/queue-arrival observation, no `λ_j`/`W_j` measurement, and no stage-local queue instrumentation exists | None. `DESIGN.md`'s own "Which concurrent external processes it models" section states the relation holds without needing prediction of any single process's outcome, but that is an architectural claim, not a measured `L_j = λ_j W_j` observation — no observation has been taken |

## What M&A-C1..C3 do NOT claim

M&A-C1 through M&A-C3 being ALIVE does not mean "the M&A case study is ALIVE." It means: a
vocabulary-fit pass, a validating ontology/shapes pack, and one tested Knowledge Hook exist and
were independently re-run this session with matching output. Per Appendix N.6 item 2, this does
not imply the full PDDL8→POWL v2→Arazzo→Erlang chain (M&A-C4..C6) is real, built, or even
partially started — it is not. The overall case study's status, per `docs/releases/v26.7.14/
THESIS.md` Section 33.12, remains **PLANNED**.

## Round 3 (2026-07-18) — Question coverage + Reasoner independence progress

Not a new M&A-Cx row (M&A-C1..C3 already cover the vocabulary/shapes/hook triad); this records
progress on two of the Case-study-corpus-pack maturity dimensions in `docs/packs/
PACK_MATURITY_MODEL.md`'s Matrix 3, continuing from `docs/packs/L5_PUSH_RESULTS.md`'s Round 2
entry for this pack:

- **Question coverage**: 19 new `.rq` files committed in `queries/` (CQ1.1, CQ1.2, CQ2.1, CQ2.2,
  CQ3.2, CQ4.1, CQ4.2, CQ4.3, CQ6.1, CQ6.2, CQ6.3, CQ7.2, CQ7.3, CQ8.1, CQ8.2, CQ8.3, CQ8.4, CQ9.1,
  CQ9.2), on top of the 3 already committed in Round 2 (CQ3.1, CQ7.1, CQ10.1) — 22 of
  `COMPETENCY_QUESTIONS.md`'s 31 numbered CQs now have a committed query, each executed for real
  (not merely written) with an automated row-count assertion against `fixtures/case.ttl` and/or
  `fixtures/case-2.ttl` via the scratch consumer at `/private/tmp/claude-501/
  -Users-sac-ggen/70ac08c7-8655-49c4-baa2-018bc441bb4c/scratchpad/l5-round3-ma-case-study-pack/`.
  Remaining uncovered: CQ3.3 (no `odrl:action` data asserted on any `DueDiligenceItem` fixture),
  CQ5.1-5.3 (regulatory filing obligation is hook-derived, not directly asserted; not attempted
  this round), CQ10.2-10.4 (no `hasClosingDateTime`/termination-provision data in any fixture),
  CQX.1/CQX.2 (cross-concept views need a direct DueDiligenceItem->deal link this pack's ontology
  does not currently model). This is a real, disclosed remaining gap, not the "every CQ" L3 bar
  in full.
- **A real engine limitation was found and worked around, not ignored**: a first draft of CQ4.3
  and CQ8.3 used SPARQL 1.1 `FILTER NOT EXISTS`; a minimal repro (2-triple fixture, scratch
  consumer) proved this engine's `query()` path does not correlate the outer variable binding
  inside `FILTER NOT EXISTS` — it silently returns the same rows as if the filter were absent, no
  error. Both queries were rewritten to `MINUS`, which the same repro confirmed works correctly.
  Separately, a first draft of CQ8.2 used the property-path operator `ex:supersedes+`; a repro
  proved this silently returns 0 rows (not an error) on this engine. Rewritten to explicit UNION
  hops. See each query file's own header for the specific repro evidence.
- **Reasoner independence**: all 22 committed queries were re-executed against a SECOND engine
  (`oxigraph` 0.5.9, a crates.io dependency already used elsewhere in this workspace, not a path
  dependency back to any shared crate) via `src/bin/oxigraph_check.rs` in the same scratch
  consumer. All 30 fixture/query row-count checks matched the praxis-graphlaw-verified expected
  counts exactly, including the two MINUS-based queries and the multi-hop UNION query — a real,
  executed second-engine reproduction, not a plausibility argument.
- **No regression**: `cargo test -p praxis-graphlaw --test ma_case_hook_actuation` re-run this
  round: 4 passed, 0 failed (unchanged from Round 2's own re-run). `ggen graph validate --files
  ontology.ttl --shapes shapes.ttl` and the same merged with `fixtures/case.ttl`/`case-2.ttl`:
  `shapes_conform: true` in all three cases (re-run this round).

- **Committed, re-runnable CQ evidence (2026-07-19, L5-push pass)**: every `.rq` file's expected
  answer up to this point had been verified only in an ephemeral scratch consumer outside this
  repository (per each query's own header) — a real, disclosed gap named in
  `docs/packs/L5_VALIDATION_REPORT.md`'s "Question coverage" line. Closed for 6 of the 24
  committed queries (CQ1.1, CQ1.2, CQ2.1, CQ3.1, CQ3.2, CQ7.1 — concepts 1, 2, 3, 7) by
  `crates/praxis-graphlaw/tests/ma_case_study_competency_questions.rs`: each query is loaded via
  `include_str!` (byte-identical to the committed `.rq` file, not retyped) and executed for real
  via `praxis_graphlaw::TripleStore::query` against `fixtures/case.ttl` and, for the two queries
  whose own headers already claimed a cross-instance-different-answer property (CQ1.1, CQ7.1),
  also against `fixtures/case-2.ttl` — asserting the exact literal answer each header claims, not
  merely that the query parses. `cargo test -p praxis-graphlaw --test
  ma_case_study_competency_questions`: 9 passed, 0 failed (verified this session; also re-ran
  `ma_case_hook_actuation`: 4 passed, 0 failed, no regression). REMAINING GAP, disclosed not
  fixed: 18 of 24 `.rq` files (CQ3.3, CQ4.*, CQ5.*, CQ6.*, CQ8.*, CQ9.*, CQ10.*, CQX.*) are still
  verified only by header comment/scratch consumer, not by a committed test — this pass closes
  part of the gap, not all of it.

## See Also

- `docs/releases/v26.7.14/THESIS.md` Section 33.12 — the source-of-truth PLANNED ruling this
  table operationalizes
- `docs/releases/v26.7.14/THESIS.md` Section 35.9 and Appendix N.6 — the Fortune-5 crown
  claim-reconciliation table and refusal list this table's shape and discipline mirror
- `packs/ma-case-study-pack/DESIGN.md` — full design account of what was and was not built this
  session, including the two false-friend FIBO terms checked and rejected
- `crates/praxis-graphlaw/tests/ma_case_hook_actuation.rs` — the test evidence backing M&A-C3
- `docs/releases/v26.7.14/RELEASE_CONTROL.md` Sec. 5, item 5 — the milestone-level open-items
  register entry this pack's progress updates
