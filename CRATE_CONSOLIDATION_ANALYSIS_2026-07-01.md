# Crate Consolidation Analysis — 2026-07-01

Synthesis of independent per-crate agent analyses against the workspace's 15 active members
plus dormant/untracked directories. This document does not overwrite
`CRATE_CONSOLIDATION_PROPOSAL.md` (untracked, never executed) — it cross-checks and updates it.

---

## 1. Crates to REMOVE outright

| Crate | LOC | Dependents | Tests | Blast radius if removed |
|---|---|---|---|---|
| `genesis-core` | 256 | 0 (active workspace) | 0 | None in active build. Only referenced by dormant, non-member crates `genesis-wasm-shell` and `ggen-membrane` (excluded from `Cargo.toml` `members`). No active crate (`stpnt`, `genesis-core-v2`, `ggen-core`, `ggen-cli`) depends on it — `stpnt` depends on `genesis-core-v2` instead. Recommend salvaging `Pair2`/`RelationPage`/`Construct8Packet` no_std primitives into `genesis-core-v2` behind a feature flag before deletion if future wasm work needs them. |
| `stpnt` | 832 | 0 | 0 | None in active build — grep of all `Cargo.toml` files found no incoming references; not wired into any binary entry point (`ggen-cli`, `ggen-daemon`). It depends on `genesis-core-v2`, `genesis-types-v2`, `ggen-core`, but nothing depends back. This is a Decorative Completion pattern per `.claude/rules/coding-agent-mistakes.md` §1.1: compiles, has no test files, produces no observable effect on the rest of the system. |

**Note on `stpnt`:** the prior proposal recommends KEEP for `stpnt` on narrative grounds ("canonical checkpoint validation tool") while its own dependency audit says "Incoming Workspace Referrers: None (leaf tool)." This synthesis treats zero dependents + zero tests + no CLI wiring as REMOVE-grade evidence and overrides the prior narrative-only KEEP. See §4 for full disagreement discussion.

**Placeholders (from baseline audit, not independently re-verified here but consistent with all agent findings):** `ggen-pack-clap-noun-verb`, `ggen-pack-lsp-max` — empty directories with no `Cargo.toml`, not workspace members, zero blast radius.

---

## 2. Crates to CONSOLIDATE

| Source | Target | LOC | Dependents | Tests | Justification |
|---|---|---|---|---|---|
| `star-toml` | `ggen-config` | 4,521 | 1 (`ggen-config`) | 1 file | Zero outgoing workspace deps, exactly one incoming referrer. API (`Validate`, `Validator`, `ErrorKind`, `load_file`, `from_str`, `find_config_file`, `ValidationErrors`) is threaded through `ggen-config`'s `config`, `config_lib/parser`, `config_lib/error`, `config_lib/validator`, `config_lib/schema` modules — real, exercised code, not dead weight, but single-consumer. |
| `ggen-a2a-mcp` | `ggen-lsp` | 11,762 | 3 (`ggen-cli`, `ggen-lsp-a2a`, `ggen-lsp-mcp`) | 1 file | Part of a 4-crate fragmented LSP/A2A/MCP network layer around `ggen-lsp`. Weak test signal (1 file for 11.7k LOC) relative to size. |
| `ggen-lsp-mcp` | `ggen-lsp` | 1,441 | 2 (`ggen-cli`, `ggen-lsp-a2a`) | 5 files | Thin adapter exposing `ggen-lsp`'s repair-route engine as an MCP tool; single dependency (`ggen-lsp`), no independent logic. |
| `ggen-lsp-a2a` | `ggen-lsp` | 158 | 0 | 1 file | Leaf crate, zero incoming workspace dependents (nothing depends on it), exists purely to re-expose `ggen-lsp-mcp` route tools as an A2A adapter — adds a dependency hop with no consumer. |
| `genesis-schema-v2` | `genesis-types-v2` | 249 | 1 (`genesis-core-v2`) | 0 files | Single 249-line `lib.rs` of OpenAPI/YAWL data structs, zero tests, one consumer, no outgoing workspace deps. Conceptually duplicates `genesis-types-v2`'s role as a foundational-types crate. |
| `genesis-core-v2` | `genesis-core` | 2,742 | 1 (`stpnt`) | 2 files | Real, actively-used crate (Pattern trait + registry for 43 YAWL patterns) but duplicates the conceptual namespace of the (soon-relocated) `genesis-core` primitives. Thin test coverage. Note: since this synthesis recommends REMOVING `genesis-core` outright (§1) rather than keeping it as a merge target, this consolidation direction needs resolution — see migration sketch below. |

### Migration sketches

**`star-toml` → `ggen-config`:** Move `crates/star-toml/src/*.rs` into `crates/ggen-config/src/star_toml/` as a module; update `config`, `config_lib/parser`, `config_lib/error`, `config_lib/validator`, `config_lib/schema` imports from `star_toml::*` to `crate::star_toml::*`; remove `star-toml` from workspace `members` and from `ggen-config/Cargo.toml` path deps; re-run `ggen-config`'s 1 existing star-toml-touching test plus full `ggen-config` suite.

**LSP family (`ggen-a2a-mcp`, `ggen-lsp-mcp`, `ggen-lsp-a2a`) → `ggen-lsp`:** Add `mcp` and `a2a` Cargo features to `ggen-lsp`. Copy `ggen-lsp-mcp/src/*` → `ggen-lsp/src/mcp/` gated `#[cfg(feature = "mcp")]`; copy `ggen-a2a-mcp/src/*` → `ggen-lsp/src/a2a_mcp/` (also feature `mcp` or a shared `a2a` feature, since `ggen-lsp-a2a` depends on both `ggen-lsp-mcp` and `ggen-a2a-mcp`); copy `ggen-lsp-a2a/src/lib.rs` (158 lines) → `ggen-lsp/src/a2a/` gated `#[cfg(feature = "a2a")]`. Update `ggen-cli/Cargo.toml` to depend on `ggen-lsp` with `features = ["mcp", "a2a"]` instead of the four separate path deps. Preserve the 1+5+1 = 7 existing test files as `ggen-lsp` integration tests under the new feature gates. Net: 4 crates → 1.

**`genesis-schema-v2` → `genesis-types-v2`:** Move the OpenAPI/YAWL structs from `crates/genesis-schema-v2/src/lib.rs` into a new `crates/genesis-types-v2/src/schema.rs` module; update `genesis-core-v2/Cargo.toml` to drop the `genesis-schema` workspace dep and use `genesis_types::schema::*` instead; remove `genesis-schema-v2` from `members`.

**`genesis-core` (no_std primitives) + `genesis-core-v2` (Pattern/registry):** Given §1 recommends removing the standalone `genesis-core` (zero live dependents), the correct order is: (a) if the no_std primitives (`Pair2`, `RelationPage`, `Construct8Packet`) are still wanted, fold them into `genesis-core-v2/src/no_std/` behind a `no_std`-compatible feature flag *first*; (b) then delete the standalone `genesis-core` package and repoint the two dormant, non-member crates (`genesis-wasm-shell`, `ggen-membrane`) at `genesis-core-v2` if/when they are ever reactivated — no action required now since they are not compiled. This reconciles the prior proposal's "merge genesis-core into genesis-core-v2" direction with this synthesis's "genesis-core has zero live dependents → REMOVE" finding: same end-state (one crate, `genesis-core-v2`), reached via removal-then-salvage rather than a live merge.

---

## 3. Crates to KEEP as-is

| Crate | LOC | Dependents | Reason |
|---|---|---|---|
| `ggen-core` | 142,904 | 5 | Central μ₁–μ₅ pipeline engine, 116 test files, unique responsibility, high regression risk if merged. |
| `ggen-cli` (pkg `ggen-cli-lib`) | 51,403 (15,567 per narrower count) | 0 (terminal node; consumed as the `ggen` binary, not a lib dependency) | Root CLI binary crate; 105 test files incl. e2e/sabotage/doctor-adversarial suites; separating CLI from core keeps LSP/MCP integrations lean. |
| `ggen-config` | 9,644 | 4 (`ggen-core`, `ggen-cli`, `ggen-lsp`, `ggen-marketplace`) | Shared config/receipt (Ed25519 envelope chain) logic; merging into any one consumer risks a dependency cycle. |
| `ggen-graph` | 10,268 | 3 (`ggen-cli`, `ggen-core`, `ggen-lsp`) | Isolated, deliberately-separate RDF/SPARQL/BLAKE3-receipt substrate; 21 test files, 31+ `#[test]` fns, required for GALL security-boundary checkpoints. |
| `ggen-marketplace` | 25,699 | 1 (`ggen-core`, feature-gated) | Heavy net/git deps (`reqwest`, `git2`) kept optional/feature-gated off `ggen-core`'s default build; 6 test files, clean single dependency (`ggen-config`). |
| `ggen-lsp` | 23,129 | 1 (`ggen-cli`) | 36 test files; designated absorption TARGET for the LSP/MCP/A2A consolidation (§2), not itself a consolidation source. |
| `genesis-types-v2` | 1,038 | 2 (`genesis-core-v2`, `stpnt`) | Zero outgoing deps, real fan-in, 2 test files; keeping it lightweight avoids compilation cascades into KNHK V2 consumers. |
| `cpmp` | 2,901 | 0 | Self-contained leaf CLI tool (capability scanner/classifier/receipt engine) with its own binary entry point (clap-noun-verb `#[verb]`), 3 test files, no domain overlap with any other crate. |
| `clnrm-core-patched` | n/a (not separately analyzed above; carried from baseline) | n/a | Required registry override patch for cleanroom/testcontainers E2E under the pinned toolchain; removing breaks the patch. |

---

## 4. Cross-check against `CRATE_CONSOLIDATION_PROPOSAL.md`

**Status of prior proposal:** untracked in git, never committed, none of its 4 phases have been executed — current `Cargo.toml` still lists all four LSP-family crates and both `genesis-core`/`genesis-core-v2` as separate members, exactly matching the proposal's documented "before" state.

### Agreements
- `ggen-a2a-mcp`, `ggen-lsp-mcp`, `ggen-lsp-a2a` → consolidate into `ggen-lsp` behind `mcp`/`a2a` features (identical target and mechanism in both analyses).
- `star-toml` → consolidate into `ggen-config` (both name this exact merge, prior proposal frames it as "either/or," this synthesis resolves it to CONSOLIDATE given single-consumer + no fan-out).
- KEEP: `ggen-core`, `ggen-cli`(-lib), `ggen-graph`, `ggen-config`, `ggen-marketplace`, `cpmp`, `genesis-types-v2` — identical verdicts and largely identical rationale (coupling shape, feature-gating heavy deps, deliberate architectural separation).
- `genesis-core` / `genesis-core-v2` are recognized by both as a duplicated-naming pair that should collapse to one package.
- `genesis-schema-v2` "safe to merge with genesis-types-v2" — prior proposal notes this but nominally recommends KEEP "for clean interface contracts"; this synthesis's independent agent found 0 tests, 1 dependent, 249 LOC and recommends actually executing the consolidation rather than deferring it. Direction agrees; verdict strength differs (this synthesis is more decisive: CONSOLIDATE not KEEP).

### Disagreements

1. **`stpnt`: prior = KEEP, this synthesis = REMOVE.**
   The prior proposal's own dependency audit records "Incoming Workspace Referrers: None (leaf tool)" — identical to this synthesis's finding — but still recommends KEEP based on narrative value ("canonical checkpoint validation tool"). This synthesis treats the combination of **zero dependents + zero test files + no CLI/binary wiring** as meeting the Decorative Completion bar defined in this repo's own `.claude/rules/coding-agent-mistakes.md` §1.1: a crate that compiles and exists but produces no observable effect on the rest of the system. Since neither analysis found a caller, and the prior proposal's KEEP rationale cites no evidence beyond the crate's name/description, this synthesis overrides it to REMOVE (or archive outside `members`, matching the pattern already applied to the 5 dormant crates), pending confirmation that no undocumented consumer exists.

2. **`genesis-core`: prior = "merge INTO genesis-core-v2" (implying genesis-core-v2 absorbs genesis-core's content), this synthesis = REMOVE genesis-core, optionally salvage primitives into genesis-core-v2 first.**
   This is a difference in mechanics, not end-state: both proposals converge on "one genesis-core package, feature-gated no_std/v2." The prior proposal frames it as a merge (both crates contribute code, one package survives under the `genesis-core` name with `std`/`v2` features). This synthesis frames it as removal-with-optional-salvage (since `genesis-core` has zero *active* dependents today — its only referrers are dormant, non-member crates — there is no live migration to perform; if the no_std primitives are wanted later they should be moved into `genesis-core-v2` behind a feature flag, then the standalone `genesis-core` package deleted). Net effect is the same (2 crates → 1), but this synthesis is more conservative about doing unnecessary work merging code that has no current caller.

3. **Package-name nuance on `ggen-cli`:** the prior proposal and this synthesis both correctly identify there is no separate `crates/ggen` directory — the root binary is produced by `crates/ggen-cli` (package `ggen-cli-lib`). No disagreement, just confirming alignment on a point that could otherwise cause confusion (`ggen-cli-lib` reported at both 15,567 and 51,403 LOC across the two constituent analyses above, likely reflecting different inclusion of test files — flagged here rather than silently reconciled, since neither analysis is fully explicit about scope).

---

## 5. Prioritized action list

1. **Delete the 2 empty placeholder directories** (`ggen-pack-clap-noun-verb`, `ggen-pack-lsp-max`) — zero risk, zero blast radius, no `Cargo.toml` to update elsewhere. Do this first because it's free and unblocks nothing but costs nothing either.

2. **Resolve the `stpnt` disagreement before touching it.** Before removal, run one more confirming check: `grep -rn 'stpnt' --include=Cargo.toml crates/` to be certain no in-flight branch or recently-added crate depends on it, then remove it from `members` and delete the directory (or move to an `archive/` path consistent with the 5 already-dormant crates). Do this early since it's a pure subtraction with no consumer to update.

3. **Consolidate `star-toml` into `ggen-config`.** Single consumer, mechanical migration (move files, update imports, drop workspace member). Do this before the LSP consolidation because it's lower-risk/smaller and validates the "fold single-consumer crate into its only caller" pattern the LSP merge will reuse at larger scale.

4. **Execute the LSP/MCP/A2A consolidation** (`ggen-a2a-mcp`, `ggen-lsp-mcp`, `ggen-lsp-a2a` → `ggen-lsp` behind `mcp`/`a2a` features). This is the highest-value, highest-LOC consolidation (11,762 + 1,441 + 158 = 13,361 LOC absorbed) and reduces 4 crates to 1. Do this after step 3 since the migration pattern (move `src/`, gate with a feature, update the sole consumer's `Cargo.toml`) is now proven. Update `ggen-cli/Cargo.toml` last, once `ggen-lsp` builds clean with both features.

5. **Consolidate `genesis-schema-v2` into `genesis-types-v2`.** Small (249 LOC), single consumer (`genesis-core-v2`), no tests to migrate — low risk, do any time, but sequence after the LSP work since it's independent and lower priority.

6. **Resolve `genesis-core` / `genesis-core-v2`.** Decide whether the no_std primitives in `genesis-core` (`Pair2`, `RelationPage`, `Construct8Packet`) are still wanted. If yes, migrate them into `genesis-core-v2` behind a feature flag; if no, skip migration. Either way, remove the standalone `genesis-core` package from `members` last, since it requires the salvage decision to be made first and has zero active dependents so there is no urgency.

7. **Re-run `just check && just test && just lint` after each step**, not just at the end — per this repo's Andon Signals rule, stop and fix immediately if any gate goes red before proceeding to the next consolidation step.

---

## Evidence sources

All LOC, dependents, and test-file counts in this report are taken verbatim from the 16 per-crate agent analyses supplied for synthesis (each analysis independently derived from `Cargo.toml` dependency grep, `find`/`wc -l` on `src/`, and test-directory enumeration), cross-referenced against `CRATE_CONSOLIDATION_PROPOSAL.md` (25,925 bytes, untracked, unexecuted) and the baseline audit's executive summary (25 directories under `crates/`: 15 active members, 1 dormant-but-listed member, 1 patched override, 8 untracked directories).
