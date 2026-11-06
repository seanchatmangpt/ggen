# ZD-120: ggen Phase 3 Ladder — Publish Preflight (Registry Census + Identity)

Standing: ALIVE (read-only, executed). Publish status: BLOCKED (no login token)

- **Date**: 2026-11-04
- **Ticket**: ZP-9 Part 3 Stage 0 (ggen Phase 3 publish ladder)
- **Repo/branch**: /Users/sac/ggen @ v26-11-3-wave, HEAD `e4f231a55d9e641f60529498449d1f51077488fe`
- **Work order**: ZD-120 (read-only preflight; no publishes, no manifest edits)

## 1. `cargo whoami` verdict

**No such tool.** `cargo whoami` is not a built-in cargo subcommand (cargo
1.98.0-nightly a595d0da2), and no `cargo-whoami` crate exists on crates.io
(`cargo install cargo-whoami` → "could not find `cargo-whoami` in registry
`crates-io` with version `*`"). Identity verified indirectly via the crates.io
API: every recent version of every census crate was published by
`seanchatmangpt`. Additionally, **no `~/.cargo/credentials` file exists on
this machine** — any actual publish is blocked on `cargo login`. Census status:
ALIVE (read-only, executed). Publish status: BLOCKED (no login token).

## 2. Registry census (2026-11-04, crates.io API v1, 1 req/sec spacing)

Workspace version: `26.11.3` (root Cargo.toml). Manifest publish flags
re-read on disk this session; registry data fetched live with
`GET https://crates.io/api/v1/crates/<name>` (+ `/versions`), 1s sleep
between calls.

| crate | manifest publish flag | registry max_version (semver-max) | newest non-yanked | 26.11.3 on registry? |
|---|---|---|---|---|
| ggen (root) | publishable (no `publish = false`) | 26.15.2 — **yanked**, 2026-07-25 | 26.10.12 (2026-10-21) | **no** |
| ggen-graph | publishable (no `publish = false`, comment block at crates/ggen-graph/Cargo.toml:37-49) | 26.15.2 (2026-07-25) | 26.10.12 (2026-10-21) | **no** |
| ggen-abb-sbb | publishable (no `publish = false`) | 26.10.12 (2026-10-21) | 26.10.12 | **no** |
| ggen-scip | publishable (no `publish = false`) | — | — | **crate does not exist on crates.io** (never published) |
| ggen-config | publishable (no `publish = false`) | 26.15.2 (2026-07-25) | 26.10.12 (2026-10-21) | **no** |
| ggen-marketplace | publishable (no `publish = false`) | 26.15.2 (2026-07-25) | 26.10.12 (2026-10-21) | **no** |
| ggen-engine | `publish = true` (crates/ggen-engine/Cargo.toml:44) | 26.10.12 (2026-10-21) | 26.10.12 | **no** |
| ggen-cli-lib (crates/ggen-cli) | `publish = false` (crates/ggen-cli/Cargo.toml:55) | 26.7.3 (2026-07-03, historical, pre-flip) | 26.7.3 | no — unpublishable by design (real dep on ggen-engine) |

Other workspace crates (`ggen-lsp`, `ggen-mcp`, `ggen-cheat-scanner`,
`tools/*`, `examples/*`, vendored copies `bcinr-pddl`, `powl2-decompose`,
`bcinr-mfw-ir`, `ggen-architecture`) are all `publish = false` — not part of
the ladder.

## 3. ggen-engine registry max (publishes last in the ladder)

ggen-engine registry max = **26.10.12** (published 2026-10-21 by
seanchatmangpt). No 26.11.3 exists. CI is unaffected by the 26.11.3 absence
today because CI resolves workspace deps from path, not registry. But
ggen-graph and ggen-engine's manifests pin sibling deps with version reqs, so
the publish ladder must follow dependency order: dependencies reach 26.11.3 on
the registry before dependents attempt their own publish, with ggen-engine
last.

## 4. Findings

1. **No 26.11.3 exists anywhere on the registry.** Every census crate's newest
   non-yanked version is 26.10.12 (2026-10-21).
2. **26.15.2 CalVer anomaly.** ggen, ggen-graph, ggen-config, and
   ggen-marketplace carry a 26.15.2 version published 2026-07-25 — higher
   semver, older date than 26.10.12. crates.io's `max_version` field is a
   semver max, so it reports 26.15.2, not the newest 26.10.12. Consequence for
   the ladder: a dependency req like `^26.10` can semver-resolve to 26.15.2
   (older content, from July) rather than the newest 26.10.x; exact pins (as
   ggen-graph uses) avoid this, and 26.11.3 > 26.15.2 is FALSE semver-wise, so
   26.11.3 will NOT become max_version for these four crates after publish —
   `max_version` will keep reporting 26.15.2. Consumers should pin exact or
   `=26.11.3`.
3. **ggen-scip has never been published** — first-time publisher in this
   ladder. `cargo publish --dry-run -p ggen-scip` should be run in Stage 1
   (not run here; this receipt is read-only).
4. **Identity gate**: no local cargo login token. Stage-1+ publish steps are
   BLOCKED on `cargo login` (seanchatmangpt token).

## 5. What was NOT done

- No publishes. No manifest edits. No `cargo publish --dry-run` runs
  (Stage 1 does those). Read-only census only.
- No worktree creation, no git state changes (coordinator owns transitions).
