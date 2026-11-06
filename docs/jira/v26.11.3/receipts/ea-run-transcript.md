# EA Run Transcript — first real ea.graph.json consumer run (WO-09 / K6)

Standing: ALIVE

- **Date**: 2026-11-03
- **Repo**: /Users/sac/ggen
- **Lane**: K6 IMPLEMENTATION (self-dogfood working example)
- **Standing**: ALIVE — every command below was run this session with its real
  exit code; nothing invented.
- **Subject**: `/Users/sac/ggen/ea.graph.json`
  (sha256 `8988e4fafd9c32ab41a9e7ad8c64d9845cf5eb092cede0817d3a5c521096a552`)

## 0. Subject and route

Hand-built JSON route (WO-08 interim route): `ea.graph.json` authored directly
against the exemplar `crates/ggen-abb-sbb/fixtures/ea-graph.json` shape, schema `ggen.ea.graph.v1`, with kernel-computed
digests pinned. Authored via a deterministic builder (`/tmp/build_ea.py`),
digests computed by the kernel itself via `examples/validate_ea_graph.rs`; qualification added only where external digest evidence exists.

## 1. Graph inventory

- 1 strategy `strategy:ggen-self-manufacture`
- 4 capabilities/ABBs/contracts: pack-federation, admission-kernel,
  sync-pipeline, docs clusters; authority_ceiling CONSTRUCT everywhere;
  max_artifacts 4.
- 5 candidate SBBs = real marketplace packs (mdbook-pattern-language-pack,
  docs-diataxis-pack, gdmcp-pack, github-actions-pack ×2 roles incl. one
  second-role SBB `sbb:admission-kernel-witness`) with
  `mutable: false`, authority CONSTRUCT.
- 1 qualification: `qual:mdbook-pattern-language-pack` —
  contract `contract:docs`, contract_digest `sha256:e85a7e156312daf9066cfe63892e9ccda6747d5fa402a4f62a3ad9802ea811fd` (kernel `contract_digest`), sbb_digest `sha256:bd4ffc31a6187a6dd5ec3b74649ed6ac7c21243b67657fd8b5f38b7f837b61b1` (kernel `sbb_content_digest`).
  External evidence: `/Users/sac/ggen-marketplace/ggen.lock`
  `[packs.mdbook-pattern-language-pack]`
  `content_hash = "blake3:0fc1627b2ba256b63315c0d0cbed6722ef7d609bb2efe95cdac7451a7ddffa29"`
  (the only lock-pinned READY pack per WO-08/SBB-inventory).
- Honest omissions: gdmcp/docs-diataxis/github-actions carry NO qualification
  (baseline.json status-only, no consequence_sha256 — WO-08 finding). The
  kernel refusing them `SBB_UNQUALIFIED` is the honest outcome.

## 2. Kernel over the real file (validate_ea_graph example)

Command: `cargo run -p ggen-abb-sbb --example validate_ea_graph -- /Users/sac/ggen/ea.graph.json`, exit 0.
Output (abridged; full text at `/tmp/ea-validate-transcript.txt`):

- `validate: ok`; graph_digest `sha256:5e71100acbf06a65...ad00baed8`
- ADMITTED sbb:mdbook-pattern-language-pack (qualification
  qual:mdbook-pattern-language-pack)
- 4 × REFUSED SBB_UNQUALIFIED (docs-diataxis, gdmcp, github-actions,
  admission-kernel-witness)
- plan decision table: `Select { abb: "abb:docs", sbb: "sbb:mdbook-pattern-language-pack" }`;
  3 × `Manufacture { ... refusals: [SbbUnqualified{...}] }` — every per-candidate
  refusal carried, none dropped.

## 3. Stage 2c via real `ggen sync run` (scratch frontmatter subject)

`/tmp/ea-consumer-demo` (scratch subject, disclosed as sibling-subject run:
identical graph file verbatim, minimal frontmatter `ggen.toml`). Binary:
`/Users/sac/ggen/target/debug/ggen` v26.10.12 built 2026-11-03 13:03 from
the current tree (Stage 2c + fail-loud wiring included).

- Refusal run: `ggen sync run` → **exit 1**,
  `[FM-LAW-015] EA SBB admission for 'sbb:docs-diataxis-pack' refused:
  SBB_UNQUALIFIED: sbb:docs-diataxis-pack — qualify the SBB against its
  contract, then re-admit`
- Positive run: graph copy reduced to the qualified SBB only → `ggen sync run`
  → **exit 0**, SyncReport JSON emitted (pipeline proceeded past Stage 2c).

## 4. Repo-root run: BLOCKED (pre-existing, two independent causes)

1. **FM-CHAIN-007/009 (pre-existing)**: the repo ledger (record 198+) and the
   head `.ggen-v2/receipt.json` declare chain rule `praxis-chain/v2-fold`; the current engine (graphlaw 26.10.10+) recognizes only
   `graphlaw-chain/v2-fold` (const in graphlaw
   `src/receipt_chain/record.rs:461`). `ggen receipt verify` → FM-CHAIN-014
   unrecognized chain rule; `ggen receipt history` → FM-CHAIN-007 invalid at
   index 198. Any repo-root sync refuses before Stage 2c.
2. **Stage 2c not wired on the declarative-rules path**: `/Users/sac/ggen/ggen.toml`
   classifies DeclarativeRules → `sync()` returns early via
   `generation_rules::run` (sync.rs:369-371); `generation_rules.rs` contains
   no `ea.graph`/`abb_sbb` reference — EA admission would not have run at
   repo root even with a clean chain. This is an integration gap for WO-01's
   claim of Stage 2c as ggen-engine's admission gate, scoped to this
   repo's own sync path.

## 5. Defects filed (not patched — other lanes own src/**)

- DEF-K6-1: FM-CHAIN-007/009 repo ledger legacy rule name (pre-existing;
  remediation per code's own message: re-seal from `receipt history`/git).
- DEF-K6-2: declarative-rules sync path (`generation_rules::run`) lacks
  Stage 2c EA admission wiring (sync.rs:369 early return).
- DEF-K6-3: `--skip-ea-admission` exists in `SyncOptions` but is not wired
  to any CLI flag (WO-01 doc claims "wired in ggen-cli"; grep finds no
  `skip-ea` in ggen-cli/ggen-engine verbs).

## 6. Test receipt

`cargo test -p ggen-abb-sbb --test marketplace_fixture` →
`test marketplace_fixture_is_the_exact_admitted_projection ... ok`
(1 passed / 0 failed), exit 0.
