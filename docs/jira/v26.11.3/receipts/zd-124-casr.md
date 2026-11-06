# ZD-124: CONSUMER_AUTHORED_SURFACE_REFUSED census and fix

Standing: ALIVE

## Law read

`scripts/validate_combinatorial_maximalism_pack.py:117-119` pins the CMD consumer
(`examples/combinatorial-maximalism`) authored surface at exactly
`{ggen.toml, ontology.ttl}` and refuses any third file. The wave's self-host sync
commits (`ed3f1dfb1`, `bda447441`, Nov 4) minted `ggen.lock` into 39 example
consumers, including this one. `ggen.lock` is ggen sync runtime state, not
consumer-authored surface — the sibling verifier
(`scripts/validate_lean4_rust_pipeline.py:34-35`) already classifies it as
`GGEN_ROOT_STATE_FILES`, not authored. Verdict: the artifact-commit decision was
wrong, not the verifier; the verifier is unchanged (fail-closed preserved).

## Fix (artifact-side, verifier untouched)

- Untracked and removed `examples/combinatorial-maximalism/ggen.lock`;
  `.gitignore` gains an annotated entry so the sync-minted lock never recurs in
  the committed tree. In CI the fresh checkout has no lock; each sync mints it
  locally and `ggen receipt verify` consumes it.
- `scripts/verify-combinatorial-maximalism-pack.sh`: consumer syncs and the
  negative-court sync now pass the named `--skip-ea-admission` opt-out
  (FM-LAW-015, `scripts/validate-examples.sh:133-140` precedent). The zero-code
  contract pins the authored surface at exactly two files, so an `ea.graph.json`
  cannot lawfully exist in this consumer; the pack's own admission machinery
  (gates 010-120, receipt courts) carries the constitutional surface.
- Same script, disclosed (not silent): the `premature-authority` court now greps
  `'candidate-prematurely-authorized|authorization-without-verification'`. Gate
  060 fires two co-true violation rows after the instrument-gain calibration
  landing (`1f466fc30`) reshaped the specimen ontology; the sync error renders
  only the ORDER BY-first row, so the second label never reached stderr. The
  refusal itself is unchanged (sync still exits 1); the alternation accepts
  either truthful violation label. Court non-vacuity preserved: killing the
  mutation's refusal still fails the court.
- Portability fix, same script: census line used GNU-only `find -printf`;
  replaced with `find -exec basename` (BSD+GNU identical semantics). This is
  what made local verification executable at all.

## Local verification (ALIVE x2)

`CARGO_TARGET_DIR=target bash scripts/verify-combinatorial-maximalism-pack.sh`
— run 1 exit 0, run 2 exit 0, both printing
`combinatorial-maximalism-pack: ALIVE` (static verifier GREEN, census 2 files,
sync+receipt verify, fmt/clippy/test of generated cell, actuate/replay/tamper
courts, 4 negative gate courts, replay-stable double sync). Local-only
limitation: the final `rg` forbidden-surface sweep is skipped on this host
(`rg` absent); it runs on ubuntu-latest CI runners. A pre-ladder
`rm -f examples/combinatorial-maximalism/ggen.lock` is needed locally because
the prior run's sync mints it; a fresh CI checkout starts clean.

## Census of failing checks at head e4f231a55 (12 failed runs)

| Workflow (job) | Class | Cause (one line) |
|---|---|---|
| Combinatorial Maximalism pack (Manufacture, actuate, receipt, replay) | CASR exact-set |
`ggen.lock` committed into consumer dir; FIXED here |
| ggen-first Lean 4 to Rust (prove-emit-execute) | FM-LAW-015 | `examples/ggen-first-lean4-rust`
lacks `ea.graph.json`; new fail-closed EA gate vs old consumer |
| Non-LLM SAFe EA Self-Play (manufacture-and-self-play) | FM-LAW-015 |
`examples/safe-ea-strategy-self-play` lacks `ea.graph.json` |
| Fortune 5 Architecture (kernel-and-law) | FM-LAW-015 | `examples/fortune5-architecture` lacks
`ea.graph.json` |
| Examples core-team constructor (Construct core-team example closure) | other-typed | mixed:
FM-WRITE-005 clobber refusals (committed generated outputs drifted), FM-CONFIG-003 (industry
packs reference `/Users/sac/ggen-marketplace` absolute paths — env-dependent), FM-WRITE-012 stale
lease |
| Examples repair constructor (Construct and verify repair candidate) | other-typed |
REPAIR-CONSTRUCTOR-DRIFT: `live-examples.yml` matches neither pinned pre- nor post-repair state |
| PCQ Marketplace ALIVE (manufacture-and-verify) | other-typed | build exit 101 + evidence upload
found no `examples/pcq-marketplace/.ggen/**` (sync refused before artifacts existed) |
| TCPS Generated Validation (product evidence + Auto Select 8^4) | premise-rot |
`tests/product_evidence/tests.rs:132` MISSING_ARTIFACT `examples/tcps-generated/ggen.lock` —
never committed; the lock-committing wave missed this consumer |
| Book Chicago TDD (Real-state gap closure) | other-typed | consumer sync inside test hits
FM-WRITE-005 clobber refusal; test asserts subprocess success |
| GBB Certification Program (TAI standards suites) | other-typed | BUILD_BROKEN:
`examples/tai-enterprise-rebuild/consumer...` No such file or directory (exit 3) |
| Cyberpunk TV Platform (Manufacture, polyglot Chicago TDD...) | other-typed | npm install
failure in "Install exact JavaScript closure" (exit 1) |
| ggen Self-Host Retrofit (observe-manufacture-falsify-replay) | UNVERIFIED | `--log-failed`
unavailable ("log not found") — not classifiable from CI logs |

Count note: 12 failed checks at head e4f231a55, not 19; the 19 figure includes
stragglers at parent 6f723715b (same classes) plus cancelled/in-progress runs
(Crown Conjecture Lean cancelled; CI, GALL-001 Replay Court, ggen LSP Runtime
Crown still in progress at census time).

## Standing

- CASR class: ALIVE (ladder x2 locally, exit 0, receipts above).
- Non-CASR classes: UNVERIFIED/PARTIAL — one-line causes only, owned by other
  lanes (examples, engine FM-LAW-015 rollout, workflow pins).
