# G4 Receipt — WO-03: compose plan persistence + topological order (2026-11-03)

Lane: IMPLEMENTATION (g4-composer). Repo: /Users/sac/ggen (canonical checkout).
Owned files only: `crates/ggen-marketplace/src/packs_registry/composer.rs`,
`crates/ggen-cli/src/cmds/pack.rs` (compose verb region), plus the compose test
file for WO-03 tests.

## Diff

### crates/ggen-marketplace/src/packs_registry/composer.rs
- `PackCompositionPlan`: added `serde::Serialize` derive; added
  `transitive_deps: BTreeMap<String, BTreeSet<String>>` field — transitive
  closure over declared dependency edges (BFS per pack, BTree output).
- Added `PackCompositionPlan::to_json()` (pretty JSON, typed error).
- Kernel unit tests added: `compose_order_is_topological_for_three_chain`
  (3-chain fixture a<-b<-c, asserts exact order + closure contents),
  `plan_json_round_trips` (serialize -> re-parse -> field asserts).

### crates/ggen-cli/src/cmds/pack.rs
- Removed `order.sort()` from the compose verb and the false comment claiming
  the kernel's order "has unstable tie-breaks ... nondeterministic across
  identical runs". Replaced with a comment citing composer.rs's deterministic
  Kahn with BTreeSet pop_first tie-break.
- Added `--out <path>` flag: `persist_plan` helper (extracted below the verb
  to keep the verb under the Poka-Yoke FM-1.1 complexity ceiling) writes the
  plan via `plan.to_json()` to the given path.
- Compose JSON now also carries `transitive_deps`.
- Updated the verb's doc-comment JSON shape (was stale: no transitive_deps).

### crates/ggen-cli/tests/pack_compose_test.rs
- New test `compose_order_respects_dependency_before_dependent` using the
  affidavit-consumer triple (requirer sorts alphabetically FIRST, so sorted
  order provably breaks dependency-before-dependent).
- Replaced the stale "order must be sorted ascending" pin (which encoded the
  removed canonicalization) with a composed-set membership assertion; the
  byte-identical determinism assertion is kept.

## Commands / exits (all run this session)

| Command | Exit | Result |
|---|---|---|
| `cargo test -p ggen-marketplace --lib packs_registry::composer` | 0 | 5 passed / 0 failed (incl. 2 new kernel tests) |
| `cargo build -p ggen-cli-lib --bin ggen` | 0 | Finished |
| `cargo test -p ggen-cli-lib --test pack_compose_test` | 0 | 6 passed / 0 failed (incl. new RED-derived test) |
| Shim-court: re-inserted `order.sort()` into pack.rs (SHIM-COURT), rebuild exit 0, run test | test FAILED (exit 1) | RED witnessed under sorted code |
| Restore: `cp /tmp/pack.rs.wo03.bak pack.rs`, sha256 both sides | 0 | byte-exact (`e902f27c...` both) |
| `ggen pack compose --packs affidavit-consumer-pack,semantic-gate-witness-court-pack,wasi-json-abi-pack --out /tmp/wo03-plan.json` | 0 | stdout order `[semantic-gate..., wasi-json-abi..., affidavit-consumer-pack]` (topological), file keys == stdout keys (6 keys incl. transitive_deps) |
| `cargo check -p ggen-marketplace -p ggen-cli-lib` warnings count | 0 | no new warnings; no other `PackCompositionPlan {` constructors exist |

## Non-vacuity

The first shim attempt (ash-extension core/starter pair) passed under the
shim — those names sort in the same relative order as the topological
relation, so the pair could not discriminate sorted-vs-topological. The test
was strengthened to the affidavit-consumer triple before the RED run; the
RED was witnessed against the discriminating pair only. Disclosed honestly:
the original ash-extension form of the test was non-discriminating and was
replaced, not retained.

## Environment notes (honest classification)

- ggen-engine was compile-broken by another lane's in-flight edits early in
  the session (E0308/E0592/E0599, 4 errors); I did not touch it. Build
  cleared by the owning lane within the SLA window (polled ~7.5 min).
- After my first pack.rs edit, `ggen-cli-lib` failed FM-1.1 (5 offenders:
  admit/manufacture/plan/replay/verify_receipt) — all in OTHER lanes' files
  (cmds/ea/*, cmds/maximalism, cmds/sbb, cmds/vision2030), not compose. My
  verb passed the guard; helper extraction of `persist_plan` kept it that
  way. FM-1.1 cleared by owning lanes within the SLA window (~7.5 min poll).
- No git commands run, per lane contract.
- Pre-shim backup of pack.rs deleted after byte-exact restore; no temp
  artifacts left (`/tmp/wo03-*.json` removed).

## Standing

ALIVE — commands and exits above are from this session, on the canonical
checkout, against the real compiled binary and real pack corpora.
