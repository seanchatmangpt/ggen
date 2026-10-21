# Publish Ladder — v26.10.20

Ordered crates.io publication sequence for the workspace (v26.10.12).
Established by lane H09 dry-runs 2026-10-20. Root blocker resolved:
`ggen-abb-sbb` had never been published (crates.io max: none; workspace:
26.10.12), which blocked `ggen-engine` (path+version dep
`ggen-abb-sbb = { version = "26.10.11" }`).

## Publishable crates (grep `publish` ground truth)

| order | crate | publish flag | internal path deps | first-publish? |
|---|---|---|---|---|
| 1 | `ggen-abb-sbb` | implicit true | none | **YES** |
| 2 | `ggen-config` | implicit true | none | no |
| 3 | `ggen-marketplace` | implicit true | ggen-config | no |
| 4 | `ggen-graph` | implicit true | none (ggen-engine is dev-dep only) | no |
| 5 | `ggen-engine` | explicit true (SJIRA-15) | abb-sbb, graph, marketplace, config | no |
| 6 | `ggen` (root) | implicit true | config, marketplace (via workspace) | no |

Everything else is `publish = false`: ggen-cli, ggen-lsp, ggen-mcp,
ggen-cheat-scanner, ggen-architecture, powl2-decompose, bcinr-pddl,
bcinr-mfw-ir, pm4pytest-cli.

## The ladder (exact commands)

Run from repo root `/Users/sac/ggen`. Each step's precondition is the
previous step's crates.io confirmation. `--allow-dirty` because the tree
carries uncommitted lane work; drop it for the real run after commit.

```sh
# Step 1 — ggen-abb-sbb (first publish, 26.10.12)
cargo publish --allow-dirty -p ggen-abb-sbb
# Precondition: dry-run exit 0 (achieved 2026-10-20, see "abb-sbb fixes").
# Verify: https://crates.io/crates/ggen-abb-sbb shows 26.10.12.

# Step 2 — ggen-config
cargo publish --allow-dirty -p ggen-config

# Step 3 — ggen-marketplace (dep: ggen-config >=26.10.8, <26.15 — satisfied)
cargo publish --allow-dirty -p ggen-marketplace

# Step 4 — ggen-graph (no runtime internal deps; ggen-engine edge is dev-only)
cargo publish --allow-dirty -p ggen-graph

# Step 5 — ggen-engine (deps abb-sbb/graph/marketplace/config, all now live)
cargo publish --allow-dirty -p ggen-engine

# Step 6 — ggen (root; resolves ggen-config/ggen-marketplace from registry)
cargo publish --allow-dirty -p ggen
```

## abb-sbb fixes landed (lane H09, crates/ggen-abb-sbb/Cargo.toml)

1. **E0583 `generated` module missing from tarball** (the real blocker):
   repo-wide `.gitignore:158` pattern `generated/` excluded
   `src/generated/{mod,refusals}.rs` from `cargo package`. Fixed with an
   explicit `[package] include` list (Cargo.toml, README.md, /LICENSE,
   src/**/*, benches/**/*) — include overrides ignore rules.
2. **bench target resolvability**: `benches/**/*` kept in include because the
   `[[bench]] name = "manufacture"` target must exist inside the package.
3. **slimming**: `tests/`, `examples/`, `fixtures/`, `bench/` (stale
   receipt.json artifact) excluded — not needed by consumers.
4. Metadata was already complete (description, license=MIT, readme,
   keywords, categories). No path-only deps (cargo tree verified: serde,
   serde_json, sha2, thiserror, toml, star-toml — all registry). No
   workspace-inheritance issues (`version.workspace`, `[lints] workspace`
   both resolve at package time).

Dry-run triage history: run 1 failed on transient target-dir fingerprint
race (concurrent lanes sharing `target/`, not a metadata error); run 2
failed E0583 (real, fixed above); run 3 hit the same target-dir race
(serde_core OUT_DIR deleted mid-compile by a concurrent lane); run 4 with
an isolated `CARGO_TARGET_DIR` — **exit 0**. Receipt: `Packaged 11 files,
109.5KiB (29.8KiB compressed)` → `Uploading ggen-abb-sbb v26.10.12` →
`aborting upload due to dry run`. Expected warnings only (ignored
tests/examples, deliberate per the include list).

**Run dry-runs with an isolated target dir while other lanes are active**:
`CARGO_TARGET_DIR=target-h09-pubcheck cargo publish --dry-run --allow-dirty -p ggen-abb-sbb`
; delete the lane target dir after (lease, not asset).

## Post-publish version-requirement state

After abb-sbb 26.10.12 publishes, dependent pins already resolve — no
mandatory bumps:

- `crates/ggen-engine/Cargo.toml:99` — `ggen-abb-sbb = { version = "26.10.11" }`
  caret-semver: matches >=26.10.11,<27 → resolves 26.10.12. Optional hygiene
  bump to `"26.10.12"` (cosmetic; req-and-sha drift policy owns the canonical pin).
- `crates/ggen-engine/Cargo.toml:105` — `ggen-graph = "26.10.12"` — exact match.
- `crates/ggen-engine/Cargo.toml:111` — `ggen-marketplace = ">=26.10.10, <26.15"` — satisfied.
- `crates/ggen-engine/Cargo.toml` — `ggen-config = ">=26.10.10, <26.15"` — satisfied.
- `crates/ggen-marketplace/Cargo.toml:65` — `ggen-config = ">=26.10.8, <26.15"` — satisfied.

## Known non-blockers / notes

- The publish-verification build compiles registry crates (incl.
  `wasm4pm-compat 26.8.7` via star-toml chain) — expected, takes minutes on
  cold cache. Use a lane build root if `target/` is contended
  (`CARGO_TARGET_DIR`), or serialize against other lanes (first run hit a
  fingerprint ENOENT race).
- `--allow-dirty` required until the abb-sbb diff lands in a commit.
