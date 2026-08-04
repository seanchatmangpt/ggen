# Proof-Carrying Software Manufacturing — PRD Traceability

Maps the Proof-Carrying Software Manufacturing Platform PRD's functional and
architecture requirements to the real, committed work on
`agent/ggen-legacy-rebuild-v26.8.1` that satisfies each one. Every row cites a
commit or file that exists in this repository — nothing here is aspirational
unless explicitly marked so in the "Status" column.

## Functional requirements

| Req | Requirement | Status | Evidence |
|-----|-------------|--------|----------|
| FR1 | Admission pipeline — requirements enter only as machine-readable ontology facts | SATISFIED | `ontology/v26.8.1/{ontology,legacy-capabilities,document-evidence}.ttl` + `ontology/v26.8.1/shapes.ttl` (SHACL admission); no v26.8.1 generated surface is authored as raw prose |
| FR2 | External, non-self-certifying verification | SATISFIED | `tools/v26.8.1/subsystem_evidence_manifest.py` (generator) vs. `tools/v26.8.1/src/bin/subsystem_verifier.rs` (independent re-executor — re-hashes files, re-runs tests, re-derives git HEAD, refuses self-certification via `THIS_BINARY_SOURCE_REL` check) |
| FR3 | Fail-closed by default | SATISFIED (+ standing lint) | Commit `1a4bdee26` fixed a real fail-open defect in `coverage_projection::run_subsystem_verifier`. `tools/v26.8.1/guard_fail_open_subprocess.py` (commit `95200fc15`) is the standing lint for this defect class — mechanical, best-effort, verified against both a clean codebase (0 findings) and a reconstructed fixture of the original bug (1 finding, correctly flagged) |
| FR4 | Closed disposition vocabulary for legacy sunset | SATISFIED | `ontology/v26.8.1/shapes.ttl`'s `LegacyDispositionEnumShape` + per-disposition evidence shapes (`LegacyEquivalenceVerifierShape`, `LegacyReplacedPathsShape`, `LegacyArchivedPathShape`, `LegacyRefusedShape`); 0/65 `DISPOSITION_UNKNOWN` in the committed catalog |
| FR5 | Idempotent regeneration | SATISFIED | Commit `58a5c13be` fixed `legacy_archaeology.py`'s `emit()`, which previously discarded curated disposition data on every regeneration. Verified: fresh `mine`/`emit` reproduces the committed 65-individual file with 0 `DISPOSITION_UNKNOWN` and `pyshacl` `CONFORMS: True` |
| FR6 | Content-head, not literal-head, freshness | SATISFIED | Commit `80dcda3dd` replaced literal `git rev-parse HEAD` equality in `DOCUMENT_HEAD_STALE` with git-ancestry reasoning (content-head must be an ancestor of source_head, narrow one-commit-lag exemption for known generated-evidence artifacts) |
| FR7 | Replay from clean-room checkout | IN PROGRESS | `tools/v26.8.1/clean_room.py` (fresh git worktree, no cache reuse) + `just v26-8-1-replay`; a real clean-room run is executing as of this doc's commit to confirm reproducibility at the current head — see PR #540 for the live result |

## Architecture requirements

| Req | Requirement | Status | Evidence |
|-----|-------------|--------|----------|
| AR1 | Layered trust stack (admit → plan → manufacture → verify → standing → receipt → replay) | SATISFIED for the manufacture→replay half | `ontology → legacy_archaeology.py/subsystem_evidence_manifest.py (manufacture) → subsystem_verifier.rs (independent verify) → ggen-v26-8-1-verifier (standing) → .ggen-v2/receipt.json (chained BLAKE3, signed) → clean_room.py (replay)`. The plan/authorization layers are PDDL-based elsewhere in this repo (`planning/v26.8.1/`) but not yet wired into this exact chain — see Roadmap |
| AR2 | Every boundary crossing is a typed refusal with a dedicated sabotage test | SATISFIED | `tools/v26.8.1/src/main.rs`'s `document_evidence_sabotage_tests` (13 tests, one per refusal code: `DOCUMENT_EVIDENCE_MISSING`, `DOCUMENT_DIGEST_DRIFT`, `DOCUMENT_ROLE_INVALID`, `DOCUMENT_HEAD_STALE`, etc.) |
| AR3 | Sabotage portfolio as first-class deliverable (positive witness + true negative control per invariant) | SATISFIED | `tools/v26.8.1/coverage_sabotage_tests.py` (7 real cases against the live repo) + the 13-test document-evidence suite above; each subsystem's manifest carries both `positive_witness_reports` and `negative_falsifier_reports` with `is_true_negative_control` flagged explicitly |
| AR4 | Separation of manufacturing and observation | SATISFIED | `tools/v26.8.1/src/bin/project_coverage.rs` (writes `coverage-matrix.csv`, the only authorized writer, emits a receipted report) vs. `tools/v26.8.1/src/main.rs`'s crown (`verify_coverage_matrix_is_read_only` — recomputes in memory, byte-compares, refuses `GENERATED_COVERAGE_DRIFT`, never writes) |

## Non-functional requirements

- **Determinism under regeneration**: commit `e045b6e0c` removed a `time.time()` timestamp from `document-evidence-index.md`'s own content — verified two consecutive regenerations at a fixed head now produce byte-identical output.
- **No vacuous evidence**: at commit `48bdfb9bb` (2026-08-01, when this row was first written), 65 legacy-capability equivalence cases lived in `packs/legacy-equivalence-verifier-pack` and used real git-history reads + live-absence checks (ARCHIVED) or actual invocation of the removed surface expecting non-zero exit (REFUSED) — no recovery action that unconditionally exits 0. **Superseded by commit `0605c688f`** (2026-08-02, "remove migrated v26.8.1 enterprise corpus"): that pack's content, along with the rest of the v26.8.1 corpus, was migrated out to `seanchatmangpt/ggen-legacy` (coordinate pinned in `config/ggen-legacy-corpus.toml`; see `docs/architecture-foundry/REPOSITORY_ROLES.md`'s "Migrated v26.8.1 corpus" section). `packs/legacy-equivalence-verifier-pack/` contains no pack artifacts in this repository as of this correction — verified via `find packs/legacy-equivalence-verifier-pack -type f`, which returns only a stray, gitignored `.ggen/keys/{signing,verifying}.key` pair left over from a prior local `ggen sync run`, no `pack.toml`/`ontology.ttl`/`templates/`. Cite the corpus repository (or the migration receipt under `migrations/ggen-v26.8.1/`), not this path, for the equivalence-case evidence going forward.
- **Cost-bounded verification**: not yet instrumented as a receipted metric — see Roadmap (economic instrumentation).

## Roadmap (PRD §6 — explicitly speculative in the source PRD, not implemented)

The source PRD marks these as "speculative, explore-mode" extensions, not committed
requirements. None are implemented in this pass; each would be a substantial,
separately-scoped effort:

- **Explicit actuation broker** — structurally separating selection/authorization/execution so `P(unreceipted actuation) = 0` is enforced, not conventional. Currently nothing stops a future change from bypassing the crown and writing `coverage-matrix.csv` directly except the crown's own read-only byte-compare on the *next* verification run — that's detection, not prevention.
- **Cross-repository receipt federation** (`ggen` ↔ `ggen-legacy`) — today's receipt chain is single-repo.
- **Standing as a SPARQL query surface** — currently a JSON report a human or script reads, not a federated queryable endpoint.
- **Self-describing falsifiers shipped as sibling artifacts** — sabotage tests exist but are not packaged as a `generator --sabotage-check` a zero-context external auditor could invoke.
- **Economic instrumentation as receipted evidence** — `economics` subsystem's evidence remains honestly thin (see `docs/v26.8.1/80-economics/`); cost-per-verified-artifact and human-attention-minutes-displaced are not yet measured or receipted.

## Falsifiers (from the source PRD, unchanged)

The platform fails its own thesis if: a human must read generated implementation to
trust a release; the same component is both producer and sole verifier of a claim; a
disposition ships as `UNKNOWN`; a receipt cannot be replayed to the same standing from
a clean checkout; or a refusal code exists with no test proving it fires on the
condition it names. Every one of these was a real defect found and fixed in this
session (see commits `1a4bdee26`, `58a5c13be`, `80dcda3dd`, `c376aa50b`) — the bar
going forward is that none of them survive as *undetected* categories, not that they
never occur again.
