# Make fortune5-required-capabilities-pack portable to an arbitrary consumer

## Status (updated after real implementation)

**DONE.** Fixed for real in `~/ggen-marketplace`, worktree
`.worktrees/GM-02-fortune5-required-capabilities-portability`, branch
`story/GM-02-fortune5-required-capabilities-portability`, commit
`6c2170848d10072e1972f730ae49d2b9c023a0b2` (never touched `main`, never pushed).

- **Story 02-1** (parameterize the dependency): `templates/Cargo.toml.tmpl` no
  longer contains the literal `../../../../crates/ggen-marketplace` path; the
  dependency now comes from a `{% for row in results %}` loop over a new
  ontology fact `f5:cargo-dependency` (`f5:CargoDependencySpec`,
  `f5:dependencyCrateName`/`f5:dependencyPath`) via a new
  `queries/dependency.rq`.
- **Story 02-2** (own its symbols): re-measured `fortune5.rs` (still 2181 LOC;
  `Fortune5Reference::prove_all()` transitively touches the whole file, not a
  clean 5-symbol cut). Chose Option A — extracted the complete, verbatim,
  self-contained `fortune5.rs` (real deps only: `ed25519-dalek`, `serde`,
  `sha2`, `tera`, `hex`, `thiserror`) into a new standalone
  `ggen-fortune5-capabilities` vendor crate
  (`templates/vendor/src/lib.rs.tmpl` + `templates/vendor/Cargo.toml.tmpl`),
  generated fresh on every `ggen sync run`. `templates/src/lib.rs.tmpl` and
  `templates/tests/full_contract.rs.tmpl` now `use
  ggen_fortune5_capabilities::{...}`; zero `ggen_marketplace` references
  remain in generated output (grep-confirmed).
- **Story 02-3** (prove it): real before/after scratch-consumer fixture at
  `~/fortune5-scratch-consumer/{before,after}/packs/fortune5-required-capabilities-pack`,
  outside both `ggen-marketplace` and `beam4pm`. BEFORE: `ggen sync run` exits
  0 but `cargo build` fails with the real captured error
  `` error: failed to load manifest for dependency `ggen-marketplace` `` /
  `Caused by: failed to read '.../before/crates/ggen-marketplace/Cargo.toml'`
  / `No such file or directory (os error 2)`. AFTER: `ggen sync run` exits 0,
  `cargo build` exits 0 (`Finished \`dev\` profile [unoptimized + debuginfo]
  target(s) in 11.75s`), and `cargo test` passes both `full_contract.rs` tests
  including the real `Fortune5Reference::prove_all()` 19×3=57-obligation
  execution reaching `Fortune5Standing::Alive`
  (`test generated_inventory_and_executed_crown_close ... ok`,
  `test generated_inventory_has_unique_order_slug_and_variant ... ok`).

Chicago testing discipline confirmed:
`grep -rn "unittest.mock|Mock(|MagicMock|patch(|monkeypatch|mockall|jest.mock" packs/fortune5-required-capabilities-pack`
→ zero matches. `git diff --stat` on `gates/`, `queries/capabilities.rq`, and
`queries/requirements.rq` is empty, as required — the fix is confined to the
Cargo dependency mechanism and the vendored crate.

All three stories' acceptance bullets are satisfied with cited command output.
Out of scope, per this ticket's own Non-goals (not attempted): the general
ggen-core pack-dependency-resolution mechanism (see
[01-PACK-COMPOSITION-GAP](01-PACK-COMPOSITION-GAP.md)),
`fortune5-testing-bblock-pack`'s separate hardcoding defect (see
[03-FORTUNE5-TESTING-BBLOCK-PORTABILITY](03-FORTUNE5-TESTING-BBLOCK-PORTABILITY.md)),
and the bundle-manifest/`mix ggen_igniter.fortune5_ready` installer task. The fix
commit is local to the story branch in the worktree — not merged to `main`, not
pushed, per the run's hard constraints.

Part of the v26.9.1 Fortune-5-readiness-bundle initiative. Background: a real,
approved planning session (2026-09-01) explored `~/ggen`, `~/ggen-marketplace`,
`~/ggen_igniter`, and `~/beam4pm` and found no working "pull one bundle, become
Fortune 5 ready" path exists today, for three reasons — no pack-to-pack
composition mechanism in ggen-core itself, two of the four fortune5-adjacent
packs hardcoded to ggen-marketplace's own repo, and no real `ggen.toml`
TOML I/O or external-`ggen`-subprocess call path in `ggen_igniter` yet. This
ticket is the second reason's fix for one of the two hardcoded packs,
`fortune5-required-capabilities-pack`
(`packs/fortune5-required-capabilities-pack/` in
`/Users/sac/ggen-marketplace`). See `00-OVERVIEW.md` (once written) for the
full three-ticket bundle context and `01-*`/`03-*` for the sibling packs. This
document does not re-derive the background; it cites it.

## Defect (verified against the real files, not assumed)

`packs/fortune5-required-capabilities-pack/templates/Cargo.toml.tmpl`:

```toml
[dependencies]
ggen-marketplace = { path = "../../../../crates/ggen-marketplace" }
```

A literal, unparameterized relative path. It resolves correctly only when the
pack is generated four directories below a checkout that happens to have
`crates/ggen-marketplace` at that exact relative offset — i.e. only inside
`ggen-marketplace`'s own repo tree, never inside an arbitrary consumer project
(confirmed: no such `crates/` directory exists under `~/beam4pm` or any other
non-ggen-marketplace repo scanned during the planning session).

`packs/fortune5-required-capabilities-pack/templates/src/lib.rs.tmpl` (lines
3-4, 25, 34, 36):

```rust
use ggen_marketplace::{
    Fortune5Assessment, Fortune5Reference, Fortune5Standing, ALL_FORTUNE5_CAPABILITIES,
    ...
};
...
pub fn verify() -> Result<Fortune5Assessment, Box<dyn std::error::Error>> {
    ...
    let proof = Fortune5Reference::new(root.path().join("fortune5")).prove_all()?;
    if proof.assessment_receipt.assessment.standing != Fortune5Standing::Alive {
```

The generated verifier crate has a real, load-bearing compile-time dependency
on `ggen-marketplace`'s own hand-written Rust crate
(`/Users/sac/ggen/crates/ggen-marketplace`, the repo where `Fortune5Assessment`,
`Fortune5Reference`, `Fortune5Standing`, `ALL_FORTUNE5_CAPABILITIES`, and
`REQUIRED_PROOF_SURFACES` are actually defined —
`crates/ggen-marketplace/src/marketplace/fortune5.rs`,
`crates/ggen-marketplace/src/marketplace/mod.rs`,
`crates/ggen-marketplace/src/lib.rs` re-export them). No consumer project
vendors or depends on that crate today.

**Real symbol sizes** (checked at
`/Users/sac/ggen/crates/ggen-marketplace/src/marketplace/fortune5.rs`, 2181
lines total, before assuming either fix path):

| Symbol | Line range | Approx. LOC |
|---|---|---|
| `Fortune5Standing` (enum) | 300-322 | ~22 |
| `Fortune5Assessment` (struct + impl) | 523-575 | ~53 |
| `Fortune5Reference` (struct + impl, includes `prove_all`) | 648-1330 | ~682 |
| `ALL_FORTUNE5_CAPABILITIES` (const array, 19 entries) | 89-109 | ~20 |
| `REQUIRED_PROOF_SURFACES` (const array, 3 entries) | 22-31 | ~9 |

`Fortune5Reference` alone is ~682 lines embedded in a 2181-line file that also
defines unrelated symbols the verifier does not use
(`ConflictDimension`, `ReceiptChain`, `BundleRegistry`, `AndonController`,
`RetrySchedule`, `TraceContext`, etc. — confirmed by `impl` block scan of the
same file). This is real evidence, not an assumption, for Story 02-2's
crate-vs-template decision below: ~780 combined LOC across the four/five
symbols the verifier actually imports is large enough that hand-duplicating
it into a Tera/EEx template is a real regression risk (drift between the
template copy and the source-of-truth crate on every future change to
`fortune5.rs`), and the file's mixed-concern shape means a template
transcription cannot cleanly take "just what's used" without either dragging
in unrelated types or hand-pruning — itself an ongoing maintenance cost.

## Story 02-1 — Parameterize the Cargo dependency path

Replace the literal `../../../../crates/ggen-marketplace` path in
`templates/Cargo.toml.tmpl` with a Tera variable (or an admitted RDF fact in
`packs/fortune5-required-capabilities-pack/ontology.ttl`, following this
pack's existing pattern of driving template content from ontology data)
naming the dependency source — either a path relative to the *consumer's*
project root (only correct if Story 02-2 chooses standalone-crate publication
and the consumer vendors it at a known location) or a crates.io/git dependency
declaration (correct regardless of where the consumer's project lives on
disk).

### Acceptance

- `templates/Cargo.toml.tmpl` contains no literal `../../../../crates/...`
  path; the dependency source is a template variable resolved from either a
  Tera context value or an ontology-admitted fact, checked by reading the
  post-fix template file directly (not by generation succeeding alone —
  compilation success does not prove the path is portable, only that it
  happened to resolve in this run).
- The chosen variable/fact is documented in the pack's `README.md` or
  `ontology.ttl` comments so a future pack consumer knows what to set it to.
- No other line of `Cargo.toml.tmpl` changes (`name`, `version`, `edition`,
  `publish`, `serde_json`, `tempfile` all stay as-is) — this story is scoped
  to the one hardcoded path.

## Story 02-2 — Decouple from the external `ggen-marketplace` crate

Resolve the `Fortune5Assessment`/`Fortune5Reference`/`Fortune5Standing`/
`ALL_FORTUNE5_CAPABILITIES`/`REQUIRED_PROOF_SURFACES` dependency using one of
the two options below, **chosen only after** re-confirming the real LOC/
complexity table above against the current file content at the time the story
is executed (the table above is a snapshot; `fortune5.rs` may have grown or
shrunk by then — re-run the same `wc -l` / `grep -n "^pub struct\|^impl"`
check before committing to a path, do not reuse this document's numbers
without re-verifying).

- **Option A — publish as a standalone crate.** Extract the five symbols (and
  their real transitive dependencies inside `fortune5.rs` — check for calls
  into `ConflictDimension`, `ReceiptChain`, or other symbols in the same file
  before assuming a clean cut) into a new crate (e.g.
  `ggen-fortune5-capabilities`) with no dependency on the rest of
  `ggen-marketplace`. Given the ~682-line size of `Fortune5Reference` alone,
  this is the presumptively preferred option per the planning session's
  guidance ("preferred if small enough" — the measured size argues this is
  not small).
- **Option B — re-express as generated code in the template.** Only if the
  re-verified LOC at execution time is materially smaller than this
  document's snapshot (e.g. the symbols get refactored down before this story
  lands), inline equivalent logic directly into
  `templates/src/lib.rs.tmpl` so the generated verifier crate has zero
  external `ggen-marketplace` dependency at all.

### Acceptance

- The real LOC/complexity of `Fortune5Assessment`, `Fortune5Reference`,
  `Fortune5Standing`, `ALL_FORTUNE5_CAPABILITIES`, and
  `REQUIRED_PROOF_SURFACES` is re-measured against the current
  `crates/ggen-marketplace/src/marketplace/fortune5.rs` (or wherever the
  symbols live if moved before this story executes) at the time of
  execution, and the choice between Option A and Option B is justified in
  the commit message or a ticket comment citing the actual re-measured
  numbers — reusing this document's 2026-09-01 snapshot numbers without
  re-checking is not acceptable evidence.
- Whichever option is chosen, the generated verifier crate
  (`templates/src/lib.rs.tmpl` + `templates/Cargo.toml.tmpl` together) has
  zero remaining reference to a path inside `ggen-marketplace`'s own repo
  tree (`crates/ggen-marketplace` or any sibling path under it).
- `use ggen_marketplace::{...}` in `templates/src/lib.rs.tmpl` is either
  removed (Option B) or replaced with `use ggen_fortune5_capabilities::{...}`
  (or whatever the standalone crate is actually named — Option A), matching
  whichever real crate was published.

## Story 02-3 — Real generation against a fresh scratch consumer project

Prove the fix by generating this pack against a project that is not
`ggen-marketplace` itself and not `beam4pm` (a truly fresh scratch directory,
e.g. under `/private/tmp/` or the session scratchpad, containing only a
minimal `ggen.toml` and a placeholder `Cargo.toml`/`mix.exs` as applicable) —
this is the actual falsifier the planning session called out: the old,
unfixed pack would either fail with a "no such file or directory" on the
literal `../../../../crates/ggen-marketplace` path, or (if by directory-depth
coincidence it resolved to *something*) fail `cargo build` with an unresolved
crate/module error, whichever manifests first.

### Acceptance

- A regression fixture reproduces the pre-fix failure: generating the
  unfixed pack (checked out at the commit before Story 02-1/02-2 land, or a
  copy of the pre-fix templates) against the same fresh scratch consumer
  project produces a real, observed failure — either `cargo generate`/`ggen
  sync run` erroring on the missing path, or a subsequent `cargo build`
  erroring on an unresolved `ggen-marketplace` crate — with the exact error
  text captured as evidence, not asserted from reading the template.
- The fixed pack (post Story 02-1 and 02-2), generated against the same
  fresh scratch consumer project, produces zero errors: `ggen sync run` (or
  the pack's own generation entry point) exits 0, and `cargo build` (or
  `cargo check`) on the generated verifier crate exits 0, with the actual
  command output captured as evidence.
- The scratch consumer project used for both runs is the same fixture across
  the before/after comparison (same relative layout, same `ggen.toml`), so
  the only variable between the two runs is the pack's own fix — proving the
  fix caused the before/after delta, not an unrelated environment
  difference.
- This story runs after both Story 02-1 and Story 02-2 are complete; it is
  the acceptance gate for the pack as a whole, not an independent unit of
  work that can land alone.

## Non-goals

- This ticket does not build a general ggen-core pack-dependency-resolution
  mechanism — see the sibling ticket for `[dependencies]`-table support (out
  of scope here; `pack.toml`'s `deny_unknown_fields` schema in
  `ggen-engine/src/pack.rs` lines 70-93 stays closed for this ticket's
  purposes).
- This ticket does not resurrect the dead `PackComposer`/`DependencyGraph`
  subsystem in `ggen-marketplace::packs_registry` into a CLI verb.
- This ticket does not fix `fortune5-testing-bblock-pack` (the sibling pack
  hardcoded to `crates/ggen-cli`/`target/debug/ggen` detection) — that is a
  separate ticket in this same bundle.
- This ticket does not build the bundle-manifest/`mix
  ggen_igniter.fortune5_ready` installer task — that depends on this pack
  (and its sibling) being portable first, and is tracked separately.
- This ticket does not claim the fixed pack makes any consumer project
  "Fortune 5 ready" in a business/certification sense (no cloud marketplace
  listing, no SOC2 audit, no pentest) — it closes a portability defect in a
  documentation/scaffold-generation pack, nothing more.

## Definition of done

- `templates/Cargo.toml.tmpl` has no literal path outside the pack's own
  directory tree.
- `templates/src/lib.rs.tmpl` has no `use` statement referencing
  `ggen_marketplace` (unless Option A's replacement standalone crate is
  itself named `ggen_marketplace`, which it should not be, to avoid
  reintroducing the same coupling under a different guise).
- Story 02-2's option choice is justified against real, re-measured LOC at
  execution time, cited in the landing commit.
- Story 02-3's before/after scratch-consumer generation runs both have real,
  captured command output — the before run fails, the after run exits 0.
- No file under `packs/fortune5-required-capabilities-pack/gates/` or
  `queries/` is modified by this ticket (the SPARQL gates and query surface
  are unrelated to the two hardcoding defects fixed here).

## See Also

- `packs/fortune5-required-capabilities-pack/templates/Cargo.toml.tmpl` — the
  literal-path defect
- `packs/fortune5-required-capabilities-pack/templates/src/lib.rs.tmpl` — the
  external-crate-dependency defect
- `/Users/sac/ggen/crates/ggen-marketplace/src/marketplace/fortune5.rs` — the
  real source of `Fortune5Assessment`/`Fortune5Reference`/`Fortune5Standing`/
  `ALL_FORTUNE5_CAPABILITIES`/`REQUIRED_PROOF_SURFACES`
- `/Users/sac/ggen/crates/ggen-engine/src/pack.rs` lines 70-93 — the closed
  `pack.toml` schema this ticket does not touch
- `~/beam4pm/docs/jira/v26.8.31/04-jira-epics-stories-acceptance.md` — the
  epic/story/acceptance-bullet convention this document follows
- `~/ggen/docs/jira/v26.8.16/00-OVERVIEW.md`,
  `~/ggen/docs/jira/v26.8.16/01-COMMIT-BOUNDARY.md` — this repo's own jira
  document style (terse, evidence-cited, Definition of done, See Also)
