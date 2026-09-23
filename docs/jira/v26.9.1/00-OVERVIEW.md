# Fortune-5-Ready Bundle Installer v26.9.1 — Overview

Index ticket for the v26.9.1 ticket set produced from a real, approved planning
session (2026-09-01) that explored `~/ggen`, `~/ggen-marketplace`, `~/ggen_igniter`,
and `~/beam4pm` to answer one question: can a consuming project become "Fortune 5
ready" by pulling in one ggen-marketplace pack bundle in a single command? The
session found no such path exists today, and — critically — found it is not a
missing-packs problem. All the needed packs (`fortune5-architecture-pack`,
`fortune5-deployment-blocks-pack`, `fortune5-required-capabilities-pack`,
`fortune5-testing-bblock-pack`) already exist in `~/ggen-marketplace`. The real gap
is three concrete, independently-verified defects: (1) ggen itself has no
pack-to-pack composition/dependency mechanism (`pack.toml`'s schema is closed, and
the real `PackComposer`/`DependencyGraph` subsystem in
`ggen-marketplace::packs_registry` is dead code with zero CLI callers); (2) two of
the four fortune5-adjacent packs are hardcoded to ggen-marketplace's own repo
layout and cannot generate correctly against an arbitrary consumer; and (3)
`ggen_igniter` has real precedent machinery for "one command wires a new capability
into a consumer project" but zero real `ggen.toml` TOML I/O and zero code that
shells out to an external `ggen sync run` binary. Every ticket in this set was
verified against the live filesystem in the session that wrote it — file paths,
line numbers, and command output are cited per ticket, not asserted from memory.
beam4pm (`~/beam4pm`) is the first real consumer used to prove the eventual bundle
installer end-to-end.

Since this set was written, real implementation work has run against it (see
`full_results.json` in the implementing session): tickets 01, 02, and 04 are
**DONE** for real; ticket 03 is **PARTIAL** — a real fix landed plus one
out-of-scope CLI-argument-drift defect was found and disclosed rather than
fixed; ticket 07 is a new sub-ticket filed during ticket 04's work (an
upstream `fortune5-deployment-blocks-pack` sync-portability defect); and
tickets 05 and 06 depend on `ggen_igniter` work that is now
implemented-but-unmerged (5 story branches in `~/ggen_igniter`, none merged to
its `main`) — see each ticket's own updated Status section for detail.

## Tickets

1. [01-PACK-COMPOSITION-GAP](01-PACK-COMPOSITION-GAP.md) — MEASURE/DOCUMENT ONLY (DONE):
   `pack.toml`'s closed schema has no `[dependencies]` field, the real
   `PackComposer`/`DependencyGraph` subsystem is dead code with zero CLI callers,
   zero of 211 packs in the corpus declare a dependency, and the formal
   composition algebra is aspirational thesis work, not a built mechanism.
2. [02-FORTUNE5-REQUIRED-CAPABILITIES-PORTABILITY](02-FORTUNE5-REQUIRED-CAPABILITIES-PORTABILITY.md)
   (DONE)
   — fix `fortune5-required-capabilities-pack`'s literal
   `../../../../crates/ggen-marketplace` `Cargo.toml.tmpl` path and its generated
   verifier crate's real compile-time dependency on ggen-marketplace's own
   hand-written `fortune5.rs` symbols, proven by real generation against a fresh
   scratch consumer project.
3. [03-FORTUNE5-TESTING-BBLOCK-PORTABILITY](03-FORTUNE5-TESTING-BBLOCK-PORTABILITY.md)
   (PARTIAL)
   — decouple `fortune5-testing-bblock-pack`'s `testing_bblock.py.tmpl` from
   ggen's own repo layout (`crates/ggen-cli`, `target/debug/ggen`,
   `cargo test -p ggen-cli-lib`) via ontology-admitted consumer facts and
   template-time binding, with all nine suites run for real against a scratch
   consumer project. The portability fix itself landed real (branch
   `story/GM-03-fortune5-testing-bblock-portability`, commit `fe8e7ad49`,
   unmerged); 6 of 9 suites report `BUILD_BROKEN` against the scratch consumer,
   traced to one real, cited, pre-existing CLI-argument-syntax drift
   (`ggen bblock inspect/plan` now requires `--group-id`/`--provider` flags,
   not positional args) that is out of this ticket's own scope.
4. [04-ARCHITECTURE-DEPLOYMENT-BLOCKS-REVERIFICATION](04-ARCHITECTURE-DEPLOYMENT-BLOCKS-REVERIFICATION.md)
   (DONE)
   — re-verify by live `ggen sync run`, not source review alone, that
   `fortune5-architecture-pack` and `fortune5-deployment-blocks-pack` — judged
   portable by the initial exploration pass — actually generate correctly against
   a fresh scratch consumer. Live-verified: `fortune5-architecture-pack` is
   portable; `fortune5-deployment-blocks-pack` is NOT (real `FM-PACK-005`
   failure, zero templates directory) and was filed as new sub-ticket 07 rather
   than fixed inline, per this ticket's own instruction.
5. [05-FORTUNE5-READY-BUNDLE-INSTALLER](05-FORTUNE5-READY-BUNDLE-INSTALLER.md)
   (DEPENDS ON UNMERGED WORK — implemented, not merged)
   — the centerpiece epic: `mix ggen_igniter.fortune5_ready`, a new bundle-manifest +
   installer task (GGEN-1801 through GGEN-1807) giving `ggen_igniter` real
   `ggen.toml` TOML parse/merge/serialize and a real `System.cmd/3` shellout to
   `ggen sync run`, modeled on `ggen_igniter.install.ex`'s existing pipeline shape.
   All 7 stories are now implemented for real across 5 sequential story
   branches in `~/ggen_igniter`, each committed only — none merged to its
   `main`. Parity-validated against the real `ggen` binary. See the ticket's
   own Status section and `~/ggen_igniter/docs/jira/v26.9.1/00-OVERVIEW.md`.
6. [06-BEAM4PM-TRIAL-AND-GATE-M2-PREREQUISITE](06-BEAM4PM-TRIAL-AND-GATE-M2-PREREQUISITE.md)
   (NOT STARTED — unblocked by ticket 05's unmerged work)
   — prove the bundle installer end-to-end against beam4pm in an isolated
   worktree, gated by a hard prerequisite: `scripts/gate_m2_check.sh`'s live,
   reproducible defect (a `build_broken` outcome on the `ggen_igniter`
   reconciliation leg destructively deleted 320 files / 22,636 lines before manual
   revert) must be fixed at the root first. Neither the prerequisite fix nor
   the beam4pm trial itself has been attempted; ticket 05's upstream work being
   implemented-but-unmerged only removes the blocker, it does not advance this
   ticket's own acceptance.
7. [07-FORTUNE5-DEPLOYMENT-BLOCKS-NOT-SYNC-PORTABLE](07-FORTUNE5-DEPLOYMENT-BLOCKS-NOT-SYNC-PORTABLE.md)
   (NOT STARTED — filed, not fixed)
   — new sub-ticket filed during ticket 04's work: `fortune5-deployment-blocks-pack`
   fails `ggen sync run --dry-run` against a scratch consumer (`FM-PACK-005`,
   zero templates directory), and separately, `ggen bblock list` was found to
   read a binary-embedded catalog rather than the `[packs]`-wired pack's own
   catalog. Both defects documented with command+output evidence; neither is
   fixed by this sub-ticket, per the parent ticket's explicit file-not-fix
   instruction.

## Definition of done for the set

- Ticket 01's findings (closed `pack.toml` schema, dead `PackComposer`, zero
  dependency-declaring packs in the 211-pack corpus) stand as documentation only —
  no code change required for this ticket, referenced rather than re-derived by
  every other ticket in the set.
- Tickets 02 and 03 each land a real fix, proven by a before/after scratch-consumer
  generation run: the pre-fix pack fails against a project unrelated to
  ggen-marketplace, the post-fix pack generates (and, for ticket 03, executes all
  nine suites) cleanly against the same fixture.
- Ticket 04 either confirms both remaining fortune5 packs generate cleanly against
  a live scratch consumer, or — if either fails — files a new sub-ticket naming the
  specific defect rather than silently absorbing it into a "portable" claim.
- Ticket 05's `mix ggen_igniter.fortune5_ready` (GGEN-1801–1807) passes every
  story's acceptance against real evidence: live-verified pack portability, a real
  `load/1` TOML parse resolving both the array-of-tables and table-of-tables
  `[packs]` schemas, dedup-aware merge, idempotent serialize-back, a real
  `ggen sync run` shellout, real per-gate verification, and a Chicago-style ExUnit
  end-to-end test with zero `Mox`/mock usage confirmed by grep.
- Ticket 06's hard prerequisite — `gate_m2_check.sh`'s destructive
  delete-before-regenerate-succeeds defect — is fixed at the root and re-verified
  clean (`git status --short` identical before/after) before the beam4pm trial's
  own acceptance bullets can be considered satisfied.
- The beam4pm trial itself lands entirely inside an isolated worktree/branch: two
  new pack entries added to `ggen.toml` with the existing five packs and the
  `gh-terraform-pack` decline comment byte-identical, `ggen sync run` and
  `gate_m2_check.sh` both exit 0, all 7 + 2 pack gates pass for real — committed
  only, **no merge to `beam4pm`'s `main`** without a separate explicit
  confirmation outside this ticket set's scope.
- No ticket in this set claims a consuming project is "Fortune 5 ready" in a
  business/certification sense (no marketplace listing, no SOC2 audit, no
  pentest, no RBAC k8s runtime) — every acceptance bullet is scoped to generated
  scaffold/documentation/contract surfaces and live-verified portability, stated
  explicitly per ticket so "instantly ready" is never overclaimed once this ships.

## See Also

- `~/beam4pm/docs/jira/v26.8.29/22-gate-closure-delta-current-head.md` — beam4pm's
  own gate-status accounting this set's ticket 06 must reconcile against once
  `gate_m2_check.sh` is fixed.
- `~/beam4pm/docs/jira/v26.8.31/04-jira-epics-stories-acceptance.md` — the
  epic/story/acceptance-bullet format convention every ticket in this set follows.
- `~/ggen/docs/jira/v26.8.16/00-OVERVIEW.md` — the terse, evidence-cited overview
  style this document matches.
- `~/ggen-marketplace/docs/thesis/09-pack-algebra.md` — the aspirational pack
  composition algebra this set deliberately works around rather than implements.
