# ZD-67: EA Admission Graphs, Batch 2 (ZB-13 items 3-4)

Status: ALIVE (all commands run this session, exits cited inline)

## Subject

- Repo `/Users/sac/ggen`, branch `v26-11-3-wave`, head at session start `58a3b4108` (uncommitted working tree; coordinator owns git).

## What landed (per-root)

### 1. packs/cyberpunk-tv-platform — ea.graph.json (NEW) + gate migration

- `packs/cyberpunk-tv-platform/ea.graph.json` — real SBB graph: strategy → cap
  `cap:tv-runtime-manufacture` → ABB → contract `contract:tv-manufacture`
  (max_artifacts 32, CONSTRUCT ceiling, 4 ports) → candidate SBB `sbb:tv-runtime-manufacture`
  (pack `pack:cyberpunk-tv-platform`) with 27 artifacts generated mechanically from the
  27 `[[generation.rules]]` in `ggen.toml` (path+template pairs), no empty-SBB bypass.
- Digests pinned from the real kernel (`cargo run -p ggen-abb-sbb --example validate_ea_graph`):
  - sbb content digest `sha256:4bccb830f0edc6b4028eba7498ef6d6203873ae212f36c482ae2ae67d5ae1e07`
  - contract digest `sha256:b29da0149bfdc1c7b91053e05e6d83eebb63a393ac46581b323036e85fcb62e2`
  - graph digest after pinning: `sha256:df1e7cca147bfff4242724b5aa7d31fa6c1dbed74e789be43f9e5c60c7876460`
  - admission probe: `ADMITTED sbb:tv-runtime-manufacture (qualification qual:tv-runtime-manufacture)`
- `ggen.toml` `[validation]` migrated `shacl = ["ontology/platform-shapes.ttl"]` → 15 SPARQL
  gate files in `gates/` (FM-LAW-017 remediation; the 11 NodeShapes remain as data under
  `ontology/`, enforced 1:1 — per-shape files plus 4 count-arm files for the minCount 6/3/3/2
  constraints, ASK form, `true = violation`). The `shacl` key is gone — legacy path removed.
- Pre-existing manifest defect found and fixed en route: `queries/extract-governance.rq`
  re-bound `?settlement_standing` via BIND (FM-GRAPH-013); fixed with a projection-level
  `COALESCE(... AS ?settlement_standing)` expression, template contract preserved
  (`governance.json.tera` still consumes `row["settlement_standing"]`).
- Sync receipts (repo-built `./target/debug/ggen` v26.11.3, cwd `packs/cyberpunk-tv-platform`):
  - `ggen sync run` exit 0 (run 1, wrote outputs); re-run exit 0, 54 "unchanged" decisions,
    `graph_hash_hex c63f8da20150851fa87b5769894a8e793d45c39ef687a40cfe6d39bf062d5f29` — idempotent.
- Disclosure: `.github/workflows/cyberpunk-tv-platform.yml` shows ` M` in git status but was
  NOT edited by this lane (another lane owns it; I read it only).

### 2. examples/tai-enterprise-rebuild — ea.graph.json (NEW)

- `examples/tai-enterprise-rebuild/ea.graph.json` — real SBB graph modeling the pack-driven manufacture:
  strategy → `cap:tai-rebuild-manufacture` → ABB → contract `contract:tai-manufacture`
  (max_artifacts 32, CONSTRUCT, 4 ports) → `sbb:tai-rebuild-manufacture`
  (pack `pack:tai-enterprise-rebuild-pack`) with 19 artifacts derived mechanically from the
  pack's template tree (`packs/tai-enterprise-rebuild-pack/templates/**.tmpl` → paths minus
  `.tmpl`), no empty-SBB bypass.
- Digests pinned from the real kernel:
  - sbb digest `sha256:834829767447892c05d2361b0fa6e64c177d470716eabbdaa3ea30a2857d3449`
  - contract digest `sha256:45919e789dde55d6078f89b15c77569ead082664017b02d202e314d4525a699e`
  - graph digest after pinning: `sha256:53af6280c6c5bafdac6f6188ef6f854fd5136e97dd9eb0209fac5203993bc9e3`
  - admission probe: `ADMITTED sbb:tai-rebuild-manufacture (qualification qual:tai-rebuild-manufacture)`.
- Sync receipts (cwd `examples/tai-enterprise-rebuild`):
  - run 1 exit 0 (wrote `consumer/tai-enterprise-rebuild/...` outputs).
  - mid-session the pack changed under us (another lane) → `FM-PACK-008` on re-run; re-locked
    per the engine's own remediation (deleted sync-generated `ggen.lock`, re-sync): exit 0.
  - final re-run exit 0, 38 "unchanged" decisions,
    `graph_hash_hex 31ea6ef908bfba00f9ce635204a56a2c263c7cb5dc9ad9a7f888c6ee713d3049` — idempotent.
- Disclosure (not my edit): `examples/tai-enterprise-rebuild/ggen.toml` diff
  (`extra_ontologies = ["cmd.ttl", "options.ttl"]` on the pack mount) was present before my
  first sync and is NOT this lane's edit — another lane owns it. `ggen.lock` is sync-generated.

### 3. exact-head-producer-and-receiver inspection (no workflow edit made)

- The failing run (41816929852, head `58a3b4108`) failed in "Execute self-hosted manufacture"
  with `[FM-LAW-015] .../self-host/lsp-contract/ea.graph.json is absent and no admission
  opt-out was given`. The workflow's sync cwd (`cd self-host/lsp-contract`) is CORRECT — the
  invoked root carries the graph at the head CI checked out... it would, except the file is
  UNTRACKED: `git log -- self-host/lsp-contract/ea.graph.json` is empty; `git ls-files` does
  not list it. ZD-40 landed it only in the working tree; it never got committed, so every CI
  checkout is a checkout that predates the file. FIX = coordinator commits `self-host/lsp-contract/ea.graph.json`.
  No `.github/workflows` edit is warranted; the workflow is right, the commit surface is wrong.
- Local verification: `ggen sync run` in `self-host/lsp-contract` exit 0 with the graph present
  (`/tmp/lsp.log`), admission probe `ADMITTED sbb:lsp-contract-manufacture (qualification qual:lsp-contract-manufacture)`.

### 4. FM-WRITE-005: examples/zero-code-inventoryctl/docs/command-reference.md

- Template status markers: header says "GENERATED by `ggen sync` from the admitted CLI
  ontology. Do not edit this file." — template-law, not hand-law; regen is the lawful side
  (GGM-SRC-002). force:true NOT set.
- Regenerated via the real sync (`ggen sync run` in `examples/zero-code-inventoryctl`, exit 0).
  The committed copy was stale by exactly one EOF newline; the regenerated copy restores it
  (`git diff` = single `+` blank line at EOF).
- Admission probe on that root's graph: `ADMITTED sbb:inventoryctl-cli
  (qualification qual:inventoryctl-cli)`.

## Cross-root verification ladder

`cargo run --locked -p ggen-abb-sbb --example validate_ea_graph -- <graph>` for all four
graphs (tai, cyberpunk, lsp-contract, inventoryctl): four ADMITTED lines, zero REFUSED.
Per-root `ggen sync run` exits: tai 0, cyberpunk 0 (×2 idempotent), inventoryctl 0,
lsp-contract 0.

## Lane-collision disclosures

- `target/debug/ggen` binary vanished mid-run (another lane's build action); rebuilt
  `cargo build -p ggen-cli-lib --bin ggen --locked` — 3m56s, Finished.
- tai pack content changed mid-session (FM-PACK-008 witness) → re-lock executed.
- `examples/tai-enterprise-rebuild/ggen.toml` and `.github/workflows/*.yml` diffs are other
  lanes' work; untouched by this lane.

## Uncommitted surface (coordinator commits; nothing staged by this lane)

- NEW `packs/cyberpunk-tv-platform/ea.graph.json`
- NEW `packs/cyberpunk-tv-platform/gates/` (15 .rq files)
- MOD `packs/cyberpunk-tv-platform/ggen.toml` — [validation] gates migration
- MOD `packs/cyberpunk-tv-platform/queries/extract-governance.rq`
- NEW `examples/tai-enterprise-rebuild/ea.graph.json`
- NEW `examples/tai-enterprise-rebuild/ggen.lock` (sync-generated)
- MOD `examples/zero-code-inventoryctl/docs/command-reference.md` (regen)
- UNTRACKED `self-host/lsp-contract/ea.graph.json` (ZD-40's file, verified ADMITTED + sync 0;
  commit = the actual CI fix, no workflow edit)
- Sync receipts under `packs/cyberpunk-tv-platform/.ggen-v2/` (sync-generated)

## Falsifiers run

- Admission with PENDING digest refused (SBB_DIGEST_MISMATCH) before pinning → pinning is
  non-vacuous (the gate really checks).
- Empty-artifacts SBB refused by kernel (lib.rs:707) — modeled none empty.
- FM-LAW-017 fired on the old `shacl` key for real before migration.
- FM-GRAPH-013 fired on the pre-fix `queries/extract-governance.rq` re-bind for real.
