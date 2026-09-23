# Re-verify fortune5-architecture-pack and fortune5-deployment-blocks-pack portability

## Status (updated after real implementation)

**DONE** (verification-only ticket; no code change required in
`~/ggen-marketplace`, so no worktree/branch/commit there, per this ticket's own
"no branch needed if verification-only" allowance). Two markdown files were
written in `~/ggen` as the deliverable:
`docs/jira/v26.9.1/04-ARCHITECTURE-DEPLOYMENT-BLOCKS-REVERIFICATION.md` (this
file — a "Closing notes (live-run results, 2026-09-01)" section with cited
command+output evidence per pack) and the new sub-ticket
[07-FORTUNE5-DEPLOYMENT-BLOCKS-NOT-SYNC-PORTABLE](07-FORTUNE5-DEPLOYMENT-BLOCKS-NOT-SYNC-PORTABLE.md).

**`fortune5-architecture-pack`: portable, LIVE-VERIFIED.** A fresh scratch
consumer (`~/f5arch-scratch-home`, unrelated to beam4pm/ggen-marketplace) was
wired via `ggen.toml` to the real `~/ggen-marketplace` checkout by path, with a
from-scratch ~21KB `ontology.ttl` authored to satisfy all 7 gates' admitted-fact
schema (one `Tier0` `ArchitectureAsset` with all 7 required policy individuals,
a `PromotionGate`, `AutonomicPolicy`, `DflssProgram`, `WorkflowEngineAdapter`,
and all 43 WCP01–WCP43 `WorkflowPatternEvidence` individuals). `ggen sync run
--dry-run` then `ggen sync run` (real write, via the underlying docker
invocation with an added `-v /Users/sac/ggen-marketplace:...:ro` mount) both
exited 0 and wrote all 4 named artifacts non-empty
(`FORTUNE5_ARCHITECTURE_CATALOG.md`, `FORTUNE5_ARCHITECTURE_DAG.dot`,
`FORTUNE5_CONTROL_MATRIX.md`, `FORTUNE5_WORKFLOW_PATTERN_COVERAGE.md`), with
zero unrendered `{{`/`{%`/`ERROR`/`undefined` tokens and content traceable to
the scratch ontology's own facts, not leaked beam4pm/ggen-marketplace content.
All 7 gates passed silently. Files retained on disk at
`~/f5arch-scratch-home/docs/` as evidence.

**`fortune5-deployment-blocks-pack`: NOT portable, LIVE-VERIFIED BROKEN — filed,
not fixed, per this ticket's explicit instruction.** Same scratch-project
procedure at `~/f5deploy-scratch-home`: `ggen sync run --dry-run` exited 1 with
`[FM-PACK-005] pack \`fortune5-deployment-blocks\`: zero templates under
.../fortune5-deployment-blocks-pack/templates` — confirmed the `templates/`
directory genuinely does not exist in that pack. Separately, `ggen bblock list`
(the pack's own documented consumption path) returned the identical catalog
JSON (`catalog_digest: 299e75f3...`) from a second scratch project with **zero**
packs wired in `[packs]` at all — proving `ggen bblock` reads a
binary-embedded catalog, not the `[packs]`-wired pack's own catalog, with no
`--pack`/`--path` flag on any `ggen bblock` subcommand to route around it. Both
defects are documented with full command+output evidence in the new sub-ticket
[07-FORTUNE5-DEPLOYMENT-BLOCKS-NOT-SYNC-PORTABLE](07-FORTUNE5-DEPLOYMENT-BLOCKS-NOT-SYNC-PORTABLE.md),
which is real (confirmed by reading it in full) and matches this summary. That
ticket is filed but not itself worked — fixing `fortune5-deployment-blocks-pack`
is ticket 07's job, not this one's.

Chicago testing discipline: no test/mock code was written (pure CLI
verification + docs); `grep -rn "unittest.mock|Mock(|MagicMock|patch(|monkeypatch"`
over the scratch project dirs returned zero matches.

Part of the v26.9.1 Fortune-5-ready-bundle ticket set. Context: a prior, approved
planning session explored `~/ggen`, `~/ggen-marketplace`, `~/ggen_igniter`, and `~/beam4pm`
to determine whether ggen-marketplace's `fortune5-*` packs and `ggen_igniter` can let any
consuming project become "Fortune 5 ready" by pulling in one pack bundle. That session
found this does not exist today for three reasons (no pack-to-pack composition mechanism
in ggen-core; two of the four fortune5-adjacent packs hardcoded to ggen-marketplace's own
repo; `ggen_igniter` has no TOML I/O or external-`ggen`-binary invocation yet) and
recommended fixing the two non-portable packs plus building a bundle-manifest installer
proven end-to-end against beam4pm — see the sibling tickets in this directory for that
work. This ticket is scoped narrowly to a gap inside that finding: the other two packs,
`fortune5-architecture-pack` and `fortune5-deployment-blocks-pack`, were judged portable,
but only by reading their source, never by running them.

## Problem

The initial exploration pass found no hardcoded external-crate/external-repo dependency in
either pack:

- `vendor/ggen-marketplace/packs/fortune5-architecture-pack/` (in beam4pm's checkout; same
  content at `~/ggen-marketplace/packs/fortune5-architecture-pack/`) — 21 ontology classes,
  7 SPARQL gates (`gates/010_program_and_asset_contract.rq`,
  `gates/020_tier01_control_closure.rq`, `gates/030_security_observability_contract.rq`,
  `gates/040_reliability_and_promotion_contract.rq`,
  `gates/050_broker_only_autonomics.rq`, `gates/060_workflow_pattern_evidence.rq`,
  `gates/070_supervision_dflss_knhk_contract.rq`), 4 templates
  (`templates/architecture_catalog.md.tmpl`, `templates/architecture_dag.dot.tmpl`,
  `templates/control_matrix.md.tmpl`, `templates/workflow_pattern_coverage.md.tmpl`).
- `vendor/ggen-marketplace/packs/fortune5-deployment-blocks-pack/` — 15 atomic + 4
  composite `bb:BlockGroup` individuals covering aws/azure/gcp, 2 gates
  (`gates/010_catalog_contract.rq`, `gates/020_safe_acyclic_broker_only.rq`), one catalog
  artifact (`catalog/fortune5-bblocks.json`), broker-only/no-direct-cloud-actuation by
  design.

That "no hardcoded dependency found" result was reached by reading `pack.toml`,
`ontology.ttl`, and the templates — never by actually running `ggen sync run` with either
pack wired into a project other than ggen-marketplace itself. A source read misses at least
two failure classes a live run would catch: a template referencing a path relative to
ggen-marketplace's own directory layout that happens not to appear as a literal string
match during review (e.g. built via Tera path concatenation rather than a bare string), and
a SPARQL gate whose `FROM`/service clause or RDF prefix assumes ontology individuals that
only exist in ggen-marketplace's own `ontology.ttl` and are silently absent — not
erroring — in an unrelated consumer's graph. This is the same category of gap the two
already-known-broken packs (`fortune5-required-capabilities-pack`,
`fortune5-testing-bblock-pack`) demonstrate is real for this pack family generically: both
of those also looked plausible until someone actually traced what they generate and
against what target.

## Scope

In scope: proving, by real command execution against a fresh scratch consumer project
unrelated to ggen-marketplace, that both packs generate real non-error output when wired
via a `ggen.toml` `[packs]` entry and run with `ggen sync run`.

Out of scope: fixing `fortune5-required-capabilities-pack` or `fortune5-testing-bblock-pack`
(tracked in the sibling ticket covering the two hardcoded packs); building the
bundle-manifest installer (`mix ggen_igniter.fortune5_ready`, tracked separately); the
general ggen-core pack-dependency-resolution algebra or resurrecting
`PackComposer`/`DependencyGraph` into a CLI verb (both are explicit non-goals of the whole
v26.9.1 bundle effort, not just this ticket); closing any gap from the earlier
7-dimension Fortune-5-readiness audit beyond generation-time portability.

## Plan

1. Create a fresh scratch consumer project outside both `~/ggen` and `~/ggen-marketplace`
   (e.g. `/tmp/fortune5-portability-scratch/` or an equivalent throwaway directory), with a
   minimal `ggen.toml` (`[project]`, `[ontology] source = "ontology.ttl"` pointing at an
   empty or minimal admitted-facts `ontology.ttl`, no relation to beam4pm's or
   ggen-marketplace's own ontology content) — modeled on the `[packs]` table shape already
   proven at `~/beam4pm/ggen.toml` (see the `[packs]` block wiring
   `beam4pm-process-model`, `beam4pm-pro-infra`, `github-actions-pack`,
   `beam4pm-ai-contracts`, `beam4pm-pro-entitlement` by `path = "vendor/..."` entries).
2. Wire `fortune5-architecture-pack` alone into the scratch project's `ggen.toml` by path
   (pointing at `~/ggen-marketplace/packs/fortune5-architecture-pack`, not a beam4pm-local
   vendor copy, so the run genuinely exercises an unrelated consumer). Run
   `ggen sync run` for real and capture the exit code and stdout/stderr verbatim.
3. Confirm the 4 templated artifacts exist, are non-empty, and are free of unresolved Tera
   placeholders or error markers: `architecture_catalog.md`, `architecture_dag.dot`,
   `control_matrix.md`, `workflow_pattern_coverage.md` (exact filenames per the 4
   `.tmpl` sources listed above, in whatever output location the pack's own `pack.toml`
   template-output-path convention specifies).
4. Repeat steps 2-3 for `fortune5-deployment-blocks-pack` in a second scratch project (or a
   second isolated `[packs]` entry in the same scratch project if the two packs do not
   collide on ontology individuals or output paths — confirm no collision before combining,
   do not assume it) — confirm the `bb:BlockGroup` catalog projection generates and is
   non-empty against the pack's `catalog/fortune5-bblocks.json` seed data and the 2 gates
   (`gates/010_catalog_contract.rq`, `gates/020_safe_acyclic_broker_only.rq`) both pass on
   the scratch project's own (unrelated) ontology graph.
5. If either run fails, errors, produces an empty/placeholder-laden artifact, or a gate
   fails against a genuinely minimal/unrelated ontology, capture the exact error and file
   a new sub-ticket describing the specific defect found — do not fix it inline in this
   ticket, and do not silently patch the pack and call this ticket's finding "portable
   after all" without the sub-ticket documenting what broke.

## Acceptance

- A fresh scratch consumer project exists (not beam4pm, not ggen-marketplace itself, not
  any existing fixture under either repo), with its own minimal `ggen.toml` and
  `ontology.ttl` committed or retained as evidence alongside this ticket's closing notes.
- `ggen sync run` was executed for real against that scratch project with
  `fortune5-architecture-pack` wired by `path`, its real exit code and stdout/stderr are
  cited (not paraphrased), and exit code 0 with all 4 named artifacts
  (`architecture_catalog.md`, `architecture_dag.dot`, `control_matrix.md`,
  `workflow_pattern_coverage.md`) present and non-empty is the pass condition — file
  existence alone is not sufficient; each artifact must contain real generated content
  (no unresolved template placeholders, no empty gate-result sections) traceable to the
  scratch ontology's own admitted facts, not to any beam4pm- or ggen-marketplace-specific
  content leaking in.
- `ggen sync run` was executed for real against a scratch project with
  `fortune5-deployment-blocks-pack` wired by `path`, exit code and stdout/stderr cited, and
  the `bb:BlockGroup` catalog projection is present, non-empty, and both gates
  (`010_catalog_contract.rq`, `020_safe_acyclic_broker_only.rq`) pass against the scratch
  project's own minimal ontology graph.
- If any hidden hardcoding, missing-individual gate failure, or non-portable path/name
  assumption is discovered in either pack during this live run, it is filed as a new,
  separate sub-ticket in this directory (not folded into this ticket's own acceptance,
  which is about closing the *verification* gap, not guaranteeing a defect-free result) —
  and this ticket's own closing note states explicitly which of the two packs (if either)
  triggered a sub-ticket.
- If both packs generate cleanly, this ticket's closing note states so explicitly per pack,
  citing the real command and artifact evidence, and updates the v26.9.1 overview ticket's
  status table (if one exists at ticket-set-write time) to move both packs from "portable,
  unverified" to "portable, live-verified."

## Definition of done

- Both packs have been run for real against at least one scratch consumer project each,
  with command output and artifact contents cited as evidence in this ticket's own closing
  notes (not merely asserted).
- Zero claims in this ticket's closing notes rest on source review alone where a live run
  was performed and available; where a live run could not be performed (e.g. a real
  environment blocker), that blocker is named explicitly rather than the gap being
  silently absorbed into a "portable" claim.
- No fix work for `fortune5-required-capabilities-pack` or `fortune5-testing-bblock-pack`
  was performed under this ticket's scope — that remains the sibling ticket's job.

## Closing notes (live-run results, 2026-09-01)

- **`fortune5-architecture-pack`: portable, live-verified.** Scratch project
  `~/f5arch-scratch-home` (own minimal `ggen.toml` + a from-scratch 21-class-covering
  `ontology.ttl` satisfying all 7 gates' admitted-fact requirements — one
  `EnterpriseArchitectureProgram`, one Tier0 `ArchitectureAsset` with every required
  policy individual, all 43 `WorkflowPatternEvidence` individuals), pack wired by
  `path = "/Users/sac/ggen-marketplace/packs/fortune5-architecture-pack"` (the real
  marketplace checkout, not a beam4pm-vendored copy). `ggen sync run` (both
  `--dry-run` and the real write) exited 0 and wrote all 4 named artifacts
  (`docs/FORTUNE5_ARCHITECTURE_CATALOG.md` 874B, `docs/FORTUNE5_ARCHITECTURE_DAG.dot`
  242B, `docs/FORTUNE5_CONTROL_MATRIX.md` 694B,
  `docs/FORTUNE5_WORKFLOW_PATTERN_COVERAGE.md` 9120B), all non-empty, all free of
  unresolved Tera placeholders/error markers (`grep -n "{{\|{%\|ERROR\|undefined"`
  over the 4 files: zero matches), all content traceable to the scratch ontology's
  own admitted facts (`SCRATCH-PROGRAM-1`, `ASSET-1` "Scratch Payments Ledger
  Service", `CAP-1`, `REGION-1`) with no beam4pm/ggen-marketplace content leaking in.
  All 7 gates passed silently (no `[FM-LAW]`/gate-failure diagnostics in the sync
  output — a gate violation would have surfaced there and aborted the write). The
  sync engine's own closure hash log confirms it resolved the gate/template/ontology
  files from the real `~/ggen-marketplace/packs/fortune5-architecture-pack/` path,
  not any embedded or beam4pm-local copy.
- **`fortune5-deployment-blocks-pack`: NOT portable, live-verified broken.** Two
  compounding defects found and filed as a new sub-ticket,
  `07-FORTUNE5-DEPLOYMENT-BLOCKS-NOT-SYNC-PORTABLE.md`: (1) the pack ships zero
  `templates/*.tmpl`, so `ggen sync run` refuses it outright with exit code 1
  (`[FM-PACK-005] pack \`fortune5-deployment-blocks\`: zero templates`) before any
  gate or ontology content is even evaluated; (2) `ggen bblock list` — the pack's own
  documented consumption path — returns byte-identical catalog JSON (same
  `catalog_digest`) whether or not the pack is wired in `[packs]` at all, proving it
  reads a `ggen`-binary-embedded catalog rather than anything resolved through
  project wiring. Per this ticket's own Plan step 5 and Acceptance, the defect was
  filed as a new sub-ticket rather than fixed inline, and this ticket's own
  acceptance bullet on that pack is **not** satisfied as "portable" — it is satisfied
  as "verified, and found non-portable, with the specific defect documented."
- Environment note: the installed `ggen` CLI (`~/.local/bin/ggen`, v26.8.28) is a
  thin wrapper that shells out to `docker run ... -v "$PWD:/workspace" ...
  ghcr.io/seanchatmangpt/ggen-ecosystem:v26.8.28 ggen "$@"` — it mounts only `$PWD`
  into the container, so a pack path outside the scratch project's own directory
  (as this ticket's Plan explicitly requires — pointing at the real
  `~/ggen-marketplace/packs/...` checkout, not a vendored copy) is invisible inside
  the container unless mounted explicitly. Both live runs above used the underlying
  `docker run` invocation directly (bypassing the wrapper) with an additional
  `-v /Users/sac/ggen-marketplace:/Users/sac/ggen-marketplace:ro` mount so the
  `[packs]` path resolved for real, rather than vendoring the pack into the scratch
  project (which would have defeated the ticket's own "unrelated consumer" scoping).
  No `cargo build -p ggen-cli` was needed — the containerized v26.8.28 binary was
  already current and functional.

## See Also

- `~/beam4pm/ggen.toml` — the real `[packs]` wiring convention this ticket's scratch
  project models its own `ggen.toml` on.
- `~/ggen-marketplace/packs/fortune5-architecture-pack/` and
  `~/ggen-marketplace/packs/fortune5-deployment-blocks-pack/` — the two packs under
  re-verification (identical content is also vendored at
  `~/beam4pm/vendor/ggen-marketplace/packs/`, but the live run in this ticket must target
  an unrelated scratch consumer, not beam4pm itself).
- `~/beam4pm/docs/jira/v26.8.31/04-jira-epics-stories-acceptance.md` — the epic/story/
  acceptance-bullet format convention this ticket follows.
- `~/ggen/docs/jira/v26.8.16/00-OVERVIEW.md` and `01-COMMIT-BOUNDARY.md` — the terse,
  evidence-cited ticket style this document matches.
- `07-FORTUNE5-DEPLOYMENT-BLOCKS-NOT-SYNC-PORTABLE.md` — the sub-ticket filed from this
  ticket's live run, documenting `fortune5-deployment-blocks-pack`'s two compounding
  portability defects in full.
