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
