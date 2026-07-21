# Generation 2: ggen-verify-pack wired into ggen's own live sync

## Objective (as directed)

Wire `ggen-verify-pack` evidence producers into ggen's own live `ggen.toml`, sync, and
verification path, and prove that valid/missing/sabotaged evidence causally changes a real
sync decision — not merely that evidence files are generated.

## What was wrong (root cause)

`ggen.toml`'s `[ontology].imports` admits pack **ontology facts** into the union graph but
never routes through `crate::pack::resolve()` (`crates/ggen-engine/src/sync.rs:208`), which is
the only path that evaluates a pack's `gates/*.rq`. `ggen-constitution-pack` and
`ggen-release-pack` were already imported this way — facts only, gates never evaluated.
`ggen-verify-pack` was not imported at all: ggen's own build/test/clippy/byte-identity state
was never observed or judged by ggen's own sync.

A prior, uncommitted Generation 1 attempt (found in worktree `retrofit/ggen-self-g1`, branch
head `d1943aafe`) had already discovered the correct mechanism for *this repo's own* schema —
ggen's `ggen.toml` is the **declarative-rules** schema (`ggen_config::manifest::GgenManifest`),
not the frontmatter schema (`ggen_engine::config::GgenConfig`) that ggen-verify-pack's own
generic README consumer instructions (`[packs]` + `[law] reflexive = true`) assume. My first
attempt this generation used the README's generic instructions and would have been silently
wrong for this repo. Corrected by adopting G1's approach: `[ontology].imports` (facts) +
`[validation].gates` (explicit gate wiring), documented inline in `ggen.toml` with the reason
gates 030/040/050 are intentionally not wired (this schema has no `[law].reflexive`, so those
gates could never fire — wiring them would be decorative).

## Proof: causal, bidirectional, independently reproduced

1. **Real defects found and fixed**, not staged:
   - `crates/ggen-graph/tests/post_chatman_roundtrip_full_test.rs:461` — `clippy::useless_vec`
   - `crates/ggen-engine/src/repl.rs:34` — `clippy::drain_collect` (perf)
   - `crates/ggen-cli/tests/packs/security/security_tests.rs:61` — `clippy::useless_vec`
   Each independently re-verified with a targeted `cargo clippy -p <crate>` before the
   full-workspace re-run confirmed exit 0 workspace-wide.

2. **Positive path**: after fixing all 3, re-running the evidence emitter flipped
   `clippy-reference-gate`'s recorded `ver:exitCode` 101→0, and the live `ggen sync run`'s
   gate-020 refusal row count dropped 3→2, citing exactly the two remaining real red checks
   (`byte-identity`, `test-workspace`). Per-check granularity is real, not blanket.

3. **Negative path (adversarial sabotage)**: `evidence/ontology.ttl`'s real
   `ver:check-build`'s `ver:exitCode 0` was tampered to `17` (a fabricated fact, build was
   never actually broken). `ggen sync run` immediately refused, citing exactly
   `{checkName=build, exitCode=17}` as the first cited row — the injected lie, not a stale or
   unrelated fact. Evidence restored to its real value immediately after.

This satisfies the required test: `valid evidence → consumed → permits`;
`sabotaged/absent evidence → consumed → refuses`, both via the same live judgment path
(`packs/ggen-verify-pack/gates/020_evidence_green.rq`, `[FM-LAW-018]`), not a special-cased
demo path.

## What remains open (named, not masked)

- **BUG-004** (`docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md`): `ggen agent
  install`'s "already installed" check reads a fixed global path instead of the project-local
  lockfile. Pre-existing, already tracked, not introduced by this generation. Causes 3 real
  failures in `crates/ggen-cli/tests/agent_lifecycle_test.rs`, which is why the
  `test-workspace` required check is honestly red. Not fixed here: a lockfile-resolution
  rearchitecture is out of the smallest-coherent-change scope for this generation.
  **Deliberately not allowlisted** via `ver:KnownDivergence` — that mechanism excepts a named
  check by whole check-name, and `test-workspace` is one aggregate `cargo test --workspace`
  check; excepting it would blanket-mask any *future* regression in the same run, which is
  exactly the false-standing failure this wiring exists to prevent.
- `byte-identity` stays red as a **transitive** consequence of `test-workspace` being red (its
  own internal re-sync hits the same gate) — this is correct reflexive behavior by design
  (mirrors gate 030's documented one-generation staleness lag), not an independent defect. An
  earlier version of this note misclassified it as one; corrected here.

## Standing

Sync for ggen's own repo does not currently reach Green — it refuses, correctly, citing real,
named, pre-existing evidence (BUG-004). This is the intended behavior of the mechanism just
wired: it is worth more, honestly, than a decorative Green would have been. No claim of Managed
or L5 standing is made.
