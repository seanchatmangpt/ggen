# Receipt: ds-3 — PG-2 Slice 5, pack integration surface

**Subject**: `/Users/sac/ggen/crates/ggen-sjira` (one canonical checkout)
**Standing**: ALIVE — all gates executed this session, exit 0.

## μ/diff (what landed)

Owned lane files:
- `crates/ggen-sjira/src/shacl.rs` (NEW ~140L) — adapter over the real
  `ggen_graph::validate_shacl` (crates/ggen-graph/src/shacl.rs:74,
  Oxigraph-backed). No validator re-implementation. Maps ggen-graph's
  `ShaclViolation` to the `{shape, focus, path}` triple the Elixir
  `GgenIgniter.SemanticJira.Shacl` court reports; typed refusal
  `ShaclRefusal::ShaclViolations` (text `REFUSED:SHACL_VIOLATIONS`), IO
  failures `ShaclRefusal::Io`. `run(pack_dir, data_path)` is the gate-shaped
  entry mirroring shacl.ex `run/2`. Disclosed UNSUPPORTED: constraints beyond
  the `sh:minCount`-over-`sh:targetClass` subset (pattern/datatype/closed/
  SPARQL constraints) — follow-on: extend ggen-graph's validate_shacl or a
  constraint walk in the adapter.
  - Note: ggen-graph's `validate_shacl` signature is `(&str, &[&str])` —
    string-based, file reads live in the adapter.
- `crates/ggen-sjira/src/target_pack.rs` (NEW ~230L incl. tests) — port of
  `GgenIgniter.SemanticJira.TargetPack.enforce!/2` (target_pack.ex, 81L):
  `enforce(rows, packs_root) -> BTreeMap<pack, digest>`; typed refusals
  `TargetUnknown{pack, work_order}` (text `REFUSED:TARGET_PACK_UNKNOWN pack=... work_order=...`),
  `SymlinkEscape` (`REFUSED:PACK_SYMLINK_ESCAPE`),
  `FileUnreadable` (`REFUSED:PACK_FILE_UNREADABLE`), `NotADir`.
  Digest law: sha256 over sorted `(rel path, sha256(bytes))` pairs.
  Disclosed divergence from `PackLock.digest_checked/1` per charter: this
  port's canonical digest form has NOT been byte-compared against the Elixir
  digest — confirm before cross-repo replay equality is claimed.
- `crates/ggen-sjira/src/pack_health.rs` (NEW ~215L) — Phase 1 port of
  `GgenIgniter.SemanticJira.PackHealth` (pack_health.ex, 235L): load
  `ontology.ttl`, execute EVERY `gates/*.rq` through the real engine,
  refuse zero gates (`HealthRefusal::NoGates`), attribute gate failures to
  the gate itself (`HealthRefusal::Gate{name, path, detail}`), fail-closed
  ontology refusals (`HealthRefusal::Ontology`). Phase 2's template
  rendering is a disclosed UNSUPPORTED — no Rust template renderer exists
  this slice; `HealthSummary.unsupported` carries the disclosure string and
  names the follow-on (`render.rs` port of `GgenIgniter.Render` +
  `build_bindings/2`).
- `crates/ggen-sjira/tests/pack_test.rs` (NEW ~155L) — real mini-pack
  fixture `tests/fixtures/pack/` (ontology.ttl + gates/term-closure.rq +
  shapes/work-order.shacl.ttl), exercised through the real ggen-graph
  validator and real Oxigraph engine — no mocks. Term-closure flip witnessed
  in both courts: gate row count flips 1→0 when `sj:term` lands; shape court
  flips conform → `REFUSED:SHACL_VIOLATIONS` when `sj:capabilityId` is
  dropped. `TargetUnknown` names the first declaring work order (WO-2 case).
- `crates/ggen-sjira/Cargo.toml` — added `ggen-graph` (workspace path dep,
  the disclosed Slice-5 prerequisite) and `oxigraph` (same engine ggen-graph
  wraps; ggen-graph has NO public SPARQL-execute seam — its sparql module is
  parse-only and shacl.rs's `solutions()` is private — so pack_health
  executes gates through workspace oxigraph directly; follow-on: a
  ggen-graph `run_query(store, q)` seam), plus `[dev-dependencies] tempfile`.
- `crates/ggen-sjira/src/lib.rs` — wired `shacl`/`target_pack`/`pack_health`
  modules; updated the deferred-modules doc line.

## Commands/exits

- `cargo test -p ggen-sjira --test pack_test` → 6/6 ok (twice, after fix).
- `cargo test -p ggen-sjira --lib` → 38/38 ok, 5 consecutive runs stable
  (after the edits_bridge flake fix).
- Filtered gates: `--lib shacl` 3/3, `--lib target_pack` 6/6, `--lib pack_health` 5/5.
- Full gate `cargo test -p ggen-sjira` → exit 0; 19 targets,
  211 tests, 0 failures (log /tmp/ds3_final2.log).

## Disclosed minimal unblocks (other lanes' files, SLA)

1. `src/generated.rs:548` — unclosed delimiter (missing `)`); owner lane
   landed further edits on the file during my run, disclosing here only.
2. `tests/execute_test.rs:264` — ExecuteOpts initializer missing
   `lease`/`epoch_watermark`/`epoch_extensions` fields as the lease lane
   landed them; added `None`/`None`/`Default::default()`.
3. `src/edits_bridge.rs` TempTree::new — REAL FLAKE FOUND + fixed: tags
   `"eb-tamper"` reused by two tests (lines ~737/750) made the pid+tag temp
   path race remove_dir_all/create_dir_all between parallel threads
   (witnessed AlreadyExists failure once, passes 5 consecutive full runs
   after per-call counter). Owner lane should review.
4. One lib warning fix in my own pack_health.rs (unused BTreeMap import).

## Pre-existing vs session-introduced

- `execute.rs:455` unused-var warning, descriptor_test/edits_bridge_test
  warnings: pre-existing, other lanes' files, not touched.
- The one-time sccache/SIGKILL build collapse was shared-target contention
  with a concurrent lane build — infra, not code.

## Verification ladder

Narrow (per-module unit, 14 tests) → integration (pack_test 6 through real
ggen-graph validator + real Oxigraph engine, real fixture files on disk) →
full `cargo test -p ggen-sjira` exit 0 (211 tests). No mocks anywhere;
assertions on real state (row counts, refusal text, digest determinism,
symlink refusal on a real symlink, real fixture pack directory).

## Standing + follow-ons

- Standing: ALIVE (commands above, this session).
- UNSUPPORTED (disclosed): template rendering — follow-on `render.rs` port.
- UNSUPPORTED (disclosed): SHACL constraints beyond minCount/targetClass
  subset; ggen-graph SPARQL-execute seam absent (oxigraph used directly).
- UNVERIFIED: cross-repo digest equality with Elixir `PackLock.digest_checked`
  canonical form (divergence disclosed above).
- Git: never run (charter).
