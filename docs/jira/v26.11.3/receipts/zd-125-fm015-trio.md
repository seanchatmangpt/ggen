# ZD-125: FM-LAW-015 trio — old consumers vs the new fail-closed EA gate

Lane: ZD-125. Date: 2026-11-04. Repo: `/Users/sac/ggen`, branch `v26-11-3-wave` (head c6425e3cb).
Scope: the three FM-LAW-015-failing consumers from the ZD-124 census, each remediated per the
ZD-124 reconciliation (authored-surface-law opt-out vs real graph).

## Law read + decision (per consumer)

- **ggen-first-lean4-rust** — the consumer's authored-surface law pins the root at exactly
  `{ggen.toml, ontology.ttl}` (plus `ggen.lock` classified as `GGEN_ROOT_STATE_FILES`):
  `scripts/validate_lean4_rust_pipeline.py`, `AUTHORED_CONSUMER_FILES` /
  `CONSUMER_ROOT_SURFACE_REFUSED` (root *files* only; top-level `generated/` is a directory and
  outside the refusal). An `ea.graph.json` cannot lawfully exist at this root → the disclosed
  opt-out is the lawful path (`scripts/validate-examples.sh:133-140` precedent, named in-file).
  **Decision: DISCLOSED OPT-OUT.**
- **fortune5-architecture** — no authored-surface verifier refuses an extra root file, and the
  consumer genuinely manufactures an artifact surface (4 docs rendered from
  `packs/fortune5-architecture-pack`). **Decision: REAL KERNEL-PINNED GRAPH (XD-14 pattern).**
- **safe-ea-strategy-self-play** — same reasoning; manufactures 6 artifacts (generated Rust cell
  + scenarios.json + use-case doc from `packs/safe-ea-strategy-self-play-pack`).
  **Decision: REAL KERNEL-PINNED GRAPH (XD-14 pattern).**

## Changes

1. `examples/fortune5-architecture/ea.graph.json` (new, authored): strategy → capability → ABB →
   contract (`max_artifacts` 8, `CONSTRUCT` ceiling, one port pair) → candidate SBB
   `sbb:fortune5-architecture-docs` (pack `pack:fortune5-architecture-pack`) declaring the 4 doc
   artifacts; one `QUALIFIED` qualification pinning the kernel-computed `sbb_content_digest`
   `sha256:13ab37a3a176e93ea0362f4e36b8a413bfa86e43db2e6118b1f38f1e8cd6cef0` and
   `contract_digest` `sha256:d361d6cd21fb29e3c5bf5f913024e8daf7c9b677f2bdade88b32f37cf99e23e5`.
2. `examples/safe-ea-strategy-self-play/ea.graph.json` (new, authored): candidate SBB
   `sbb:self-play-manufacture` (pack `pack:safe-ea-strategy-self-play-pack`) declaring all 6
   artifacts (generated/Cargo.toml, generated/src/main.rs, generated/src/bin/verifier.rs,
   generated/tests/process_boundary.rs, generated/scenarios.json,
   docs/SAFE_EA_SELF_PLAY_USE_CASES.md); `QUALIFIED` qualification pinning kernel-computed
   `sbb_content_digest` `sha256:977bea7671a5d89e6c4a1972a0e2980b82a5b06015e3a8e833ab9937619c7a39`
   and `contract_digest` `sha256:1f4fea0069b09c8c3fe3b6ce3889cb8d3e1a767180fd9b39942ad32f7f97b775`.
3. `.github/workflows/ggen-first-lean4-rust.yml` ("Manufacture Lean bootstrap" step): sync gains
   `--skip-ea-admission` with an in-step disclosure comment citing
   `validate_lean4_rust_pipeline.py` as the authored-surface law.
4. `scripts/verify-lean4-rust-pipeline.sh` (all 3 `"$GGEN" sync run` sites): same flag + disclosure
   comment. The `assert_graph_refusal` sabotage courts are unaffected — their refusals fire on
   their own laws (unproved theorem, SLO ticks, quorum, mtls, otel), not on EA-gate absence; a
   skip is loud (the receipt records the gate was not evaluated).

## Kernel validation (real output)

`cargo run -q -p ggen-abb-sbb --example validate_ea_graph -- <graph>` for both new graphs:
`schema ok: ggen.ea.graph.v1` / `validate: ok`; admission probe (CONSTRUCT) `ADMITTED` for both
SBBs with their qualifications; plan decision table `Select { … authority: Select }`. Digests were
read from this validator's own kernel output and pinned (no hand-computed hashes).

## Local verification (ALIVE, exit 0, this session, `target/debug/ggen` @ 26.11.3)

| Consumer | Command | Result |
|---|---|---|
| fortune5-architecture | `ggen sync run` (no flag; gate satisfied by real graph) | exit 0; receipt minted; `ggen receipt verify` exit 0 |
| safe-ea-strategy-self-play | `ggen sync run` (no flag) | exit 0; receipt minted; `ggen receipt verify` exit 0 |
| ggen-first-lean4-rust | `ggen sync run --skip-ea-admission` | exit 0; `ggen receipt verify` exit 0 |
| lean4 static verifier | `python3 scripts/validate_lean4_rust_pipeline.py` | GREEN (authored surface intact, no `ea.graph.json`) |
| YAML | `python3 -c yaml.safe_load` over all `.github/workflows/*.yml` | parse OK |

Local-only limitations: the Lean compile step and the Fortune 5 / self-play downstream assertions
(gate sabotage, doc content checks) were not re-run — they are unchanged by this lane and CI
executes them on ubuntu-latest. `bash -n` on the edited verify script: OK.

## Standing

ALIVE for all three consumers at the sync gate (exit 0 ×3, receipts verified ×3). PARTIAL overall:
CI-run-class verification (Lean toolchain build, downstream doc/gate assertions) is UNVERIFIED
here and remains with CI. Fail-closed law held: two of the three fixes are real admission
artifacts, not bypasses; the single opt-out is disclosed, law-cited, and receipt-recorded.

## Tree note

Sync runs mint `.ggen-v2/` receipt state under the three consumer dirs (expected runtime state;
other lanes' in-flight edits share this checkout). Nothing committed, per instruction.
