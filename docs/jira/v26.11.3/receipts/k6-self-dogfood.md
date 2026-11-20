# K6 Self-Dogfood Receipt — ggen becomes its own first real ea.graph.json consumer

Standing: ALIVE

- **Date**: 2026-11-03
- **Repo**: /Users/sac/ggen
- **Lane**: K6 IMPLEMENTATION (WO-09 deliverable: the first recorded consumer run)
- **Standing**: ALIVE — every claim below carries a command + real exit code
  from this session; BLOCKED items are cited file:line and are pre-existing.

## Deliverables (on disk)

- `/Users/sac/ggen/ea.graph.json`
  (sha256 `8988e4fafd9c32ab41a9e7ad8c64d9845cf5eb092cede0817d3a5c521096a552`)
  — the first real consumer graph: schema `ggen.ea.graph.v1`, strategy
  `strategy:ggen-self-manufacture`, 4 capabilities/ABBs/contracts (pack-federation,
  admission-kernel, sync-pipeline, docs), 5 candidate SBBs bound to real
  marketplace packs (`mutable:false`, authority CONSTRUCT, max_artifacts 4).
- `crates/ggen-abb-sbb/examples/validate_ea_graph.rs` (mine to add): parse →
  validate → digest print → per-SBB admission probe → plan decision table,
  read-only usage of the crate over a real file.
- `docs/jira/v26.11.3/receipts/ea-run-transcript.md` — the full run transcript.
- This receipt.

## Qualification law (honesty rules followed)

- Exactly ONE qualification in the graph:
  `qual:mdbook-pattern-language-pack`, because it is the only pack with real
  digest evidence — the marketplace lock
  `/Users/sac/ggen-marketplace/ggen.lock` pins it
  (`blake3:0fc1627b2ba256b63315c0d0cbed6722ef7d609bb2efe95cdac7451a7ddffa29`).
- The graph's qualification digests are kernel-computed
  (`sbb_content_digest`/`contract_digest`, printed by the validation example);
  the lock's blake3 is the external pin evidence, cited in the transcript.
- All other packs carry NO qualification (marketplace baseline.json is
  status-only, no consequence_sha256 — WO-08 finding re-verified this session:
  gdmcp/github-actions/mdbook entries are `{"status": "ALIVE"}` only). The
  kernel refusing them `SBB_UNQUALIFIED` is the honest outcome, not a defect.

## Execution ladder (real commands, real exits)

1. Kernel over the real file (exit 0):
   `cargo run -p ggen-abb-sbb --example validate_ea_graph -- /Users/sac/ggen/ea.graph.json`
   → validate ok; 1 ADMITTED (mdbook); 4 × SBB_UNQUALIFIED; plan:
   `Select { abb: "abb:docs", sbb: "sbb:mdbook-pattern-language-pack" }` +
   3 × Manufacture with per-candidate `SbbUnqualified` refusals carried.
2. `cargo test -p ggen-abb-sbb --test marketplace_fixture` → 1 passed,
   0 failed, exit 0.
3. Stage 2c via a real `ggen sync run` (exit 1, refusal witnessed):
   scratch frontmatter subject `/tmp/ea-consumer-demo` carrying the graph
   **verbatim**; `target/debug/ggen` v26.10.12 (current tree, built 13:03).
   `[FM-LAW-015] EA SBB admission for 'sbb:docs-diataxis-pack' refused:
   SBB_UNQUALIFIED — qualify the SBB against its contract, then re-admit`.
4. Positive path (exit 0): same scratch subject with only the qualified SBB
   retained → sync completed, SyncReport emitted, pipeline proceeded past
   Stage 2c.
5. Repo-root `ggen sync run` (both `target/debug/ggen` and full-run mode):
   **BLOCKED, pre-existing, two independent causes**:
   - DEF-K6-1: repo receipt ledger (record 198+) + head `.ggen-v2/receipt.json`
     declare chain rule `praxis-chain/v2-fold`; the engine recognizes only
     `graphlaw-chain/v2-fold` (graphlaw `src/receipt_chain/record.rs:461`).
     `ggen receipt verify` → FM-CHAIN-014; `ggen receipt history` →
     FM-CHAIN-007 invalid at index 198. Sync refuses before Stage 2c.
   - DEF-K6-2: `/Users/sac/ggen/ggen.toml` classifies DeclarativeRules →
     `sync()` early-returns via `generation_rules::run` (sync.rs:369-371);
     `generation_rules.rs` has zero `ea.graph`/`abb_sbb` references — Stage 2c
     would not run at repo root even with a clean chain.
   - DEF-K6-3: `--skip-ea-admission` exists in `SyncOptions` (sync.rs:225) but
     no CLI flag wires it (no `skip-ea` match in ggen-cli/ggen-engine verbs);
     WO-01's doc claim is not yet CLI-true.

## Falsifier status

- Contracted honest outcome witnessed: SBB_UNQUALIFIED refusals ARE the kernel
  working (typed, exit 1, fail-closed). The one lock-evidenced pack ADMITS.
- Vacuity check: the probe flips. With placeholder digests (pre-fill run)
  the mdbook SBB refused `SBB_DIGEST_MISMATCH`; with correct digests but empty
  `qualifications: []` it refused `SBB_UNQUALIFIED`; after adding the one
  evidence-backed qualification it ADMITTED. Three states, three distinct
  typed refusals/admission, all observed this session.
- Scratch-subject disclosure: the Stage 2c sync witness ran on
  `/tmp/ea-consumer-demo` (graph verbatim, minimal frontmatter ggen.toml),
  NOT the repo root — the repo-root path is BLOCKED by DEF-K6-1/2 (pre-existing,
  other lanes' files). The kernel-level witness (step 1) is on the real
  repo-root file.

## Files authored (complete list)

- /Users/sac/ggen/ea.graph.json (NEW, root — contract item 1)
- /Users/sac/ggen/crates/ggen-abb-sbb/examples/validate_ea_graph.rs (NEW)
- /Users/sac/ggen/docs/jira/v26.11.3/receipts/k6-self-dogfood.md (this file)
- /Users/sac/ggen/docs/jira/v26.11.3/receipts/ea-run-transcript.md (NEW)
