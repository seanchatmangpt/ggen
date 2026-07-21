# ggen — 6 Real Load Paths (As-Found)

Status: as-found survey, not idealized. Sources: Agent A (repository.md), Agent B
(generation-system.md), Agent C (receipt-system.md — read of the live
`.ggen-v2/receipt.json`, not committed to disk by that agent in this pass; values
below are taken from that agent's reported summary), Agent E (architecture-semantics.md),
plus this pass's direct checks: `gh api repos/.../branches/main/protection` (404 — **no
branch protection**), `gh api repos/.../collaborators` (single collaborator,
`seanchatmangpt`, admin=true), `.github/workflows/release.yml` triggers (`push: tags:` +
`workflow_dispatch`), and absence of a `CODEOWNERS` file.

Every SPOF below is tagged `[BLOCKING]` (must be fixed or explicitly waived by the user)
or `[ACCEPTED]` (a deliberate, currently-tolerable single point of failure — still
recorded, not silently dropped). Unknowns are marked `Unknown` explicitly, never filled in.

---

## 1. Generation (ontology → code)

Path: `.specify/*.ttl` → `ggen sync run` (`crates/ggen-engine/src/sync.rs`, Stage 0
schema-dispatch → Stage 1 Resolve → Stage 2 Enrich → Stage 2b Law(optional) → Stage 3
Extract → Stage 4 Render → Stage 5 Write+receipt) → generated files (`.claude/rules/
architecture.md`, `docs/*.md`, `crates/**/generated_*.rs`) → `.ggen-v2/receipt.json`
(BLAKE3 chained, `praxis-core::ReceiptRecord`).

SPOFs:
- **[BLOCKING] `ggen.toml` two-schema ambiguity.** Dispatch between `GgenManifest`
  (declarative-rules) and `GgenConfig` (frontmatter) is a raw-text pre-parse
  (`has_generation_rules`, `generation_rules.rs:108`) with no cross-drift guard. A malformed
  or ambiguous `ggen.toml` silently routes to the wrong schema rather than refusing. No test
  found asserting the two schemas can't both partially match. Single load-bearing heuristic,
  no fallback.
- **[BLOCKING] Generated-table drift already manifested twice.** `.claude/rules/
  architecture.md`'s "Pack Inventory" table has been observed stale against disk twice this
  session (28-vs-32, then 32-vs-33 — Agent E finding #6). This is not hypothetical: the SPOF
  (nothing forces a resync before the generated doc is trusted) has already produced wrong
  documentation that downstream agents/readers consumed as ground truth.
- **[ACCEPTED] Single reflexive-receipt malformed-log-line skip.** Agent B: this is the one
  documented, sanctioned silent-skip in the whole pipeline (opt-in sub-step of Stage 1).
  Accepted because it is explicit and scoped, not because it is risk-free.
- **[BLOCKING] `write.rs` freeze-policy `checksum` mode has an unguarded default.**
  `freeze_slots_dir` is required for `checksum` freeze and returns a typed `[FM-WRITE-006]`
  error if missing — this is good (fails loud), but the `always` freeze mode skips
  unconditionally with no drift detection at all if a target exists; a hand-edit to a
  `mode=Overwrite` file with `always` freeze is invisible to the receipt.
- **[ACCEPTED] Stage 2 Enrich is single-pass, not fixed-point.** Documented behavior
  (Agent B #4); accepted as a known scope limit, not silently different from spec.

## 2. Self-generation (ggen generating its own docs/receipts)

Path: same pipeline as (1), applied reflexively to ggen's own `.specify/repo-facts.ttl`,
`.specify/generations.ttl` → this repo's own generated tables/receipt.

SPOFs:
- **[BLOCKING] Equivalence-map evidence lane not live for ggen's own receipt.** Agent C:
  of the 8 equivalence classes in the live `.ggen-v2/receipt.json`, only 2
  (`source`, `config`) are populated; `compiled_binary`, `docs`, `tests`, `receipts`,
  `evidence`, `gates` are all `Unknown`. PR #346's evidence producers
  (`packs/ggen-verify-pack`) exist in code but are **not wired into ggen's own sync loop** —
  a load path that exists but is not live, exactly the pattern the task brief named.
- **[BLOCKING] `standing_ceiling: "LegacyObserved"` is a hardcoded one-way cap**, not a
  computed lattice value — a single hardcoded string overrides whatever the live
  equivalence/gate evaluation would otherwise conclude.
- **[BLOCKING] `andon: "Green"` is unconditionally hardcoded** at
  `crates/ggen-engine/src/sync.rs:1857` (`write_receipt`), per Agent A/C — the Andon signal
  that is supposed to be the stop-the-line mechanism (`.claude/rules/andon/signals.md`) is
  not derived from gate results in this code path.
- **[ACCEPTED] Two independent, disagreeing obligation counters** (legacy
  `obligation_count: 4` vs `v2.obligation_count.Tracked{required:9,discharged:5}`) — by
  design per Agent C, documented in `PUBLICATION_JUDGMENT.md` condition 13. Accepted as
  disclosed, not silently reconciled.
- **[BLOCKING] Sabotage-fixture coverage gap.** 12 fixtures across 3 packs
  (`dogfood-lifecycle-pack` ×8, `ma-case-study-pack` ×1, `self-monitoring-pack` ×3) have no
  referencing Rust test by literal basename (Agent C finding #6, disclosed as an unverified
  upper bound). A pack's own self-generation gates can silently regress with no test to
  catch it.
- **[BLOCKING] `docs/GENERATIONS.md`/repo-facts.ttl regeneration byte-identity not
  re-verified this pass** (Agent A: "Unknown — the pack-count drift found above suggests the
  generated tables are currently stale"). Self-generation's core claim (regenerate ⇒
  byte-identical or intentionally-diverged-and-disclosed) is unverified on this exact
  worktree state.

## 3. Pull-request (contribution → merge)

Path: fork/branch → PR opened against `main` → `ci.yml`/`quality.yml`
(`pull_request` trigger) → human review/approval → merge.

SPOFs:
- **[BLOCKING] No branch protection on `main`.** Confirmed live:
  `gh api repos/seanchatmangpt/ggen/branches/main/protection` → `404 Branch not protected`.
  There is no required-review rule, no required-status-check rule, and no restriction on
  who can push directly to `main` or force-push it. CI running on `pull_request` is
  advisory only — nothing stops a direct push to `main` that bypasses CI entirely.
- **[BLOCKING] Single collaborator with admin rights, no CODEOWNERS.**
  `gh api repos/.../collaborators` → exactly one collaborator (`seanchatmangpt`,
  `admin=true`). No `CODEOWNERS` file found anywhere in the tree
  (`find . -iname CODEOWNERS` → empty). Every PR review, every merge decision, and every
  admin bypass of any future branch-protection rule is one person. This is the textbook
  SPOF the task brief named ("anyone with write access") — here it is sharper: there is
  currently exactly one such person and zero enforced review gate.
- **[BLOCKING] Both git hooks are `main`-only and silently no-op elsewhere.**
  Per `CLAUDE.md`: `pre-commit.sh`/`pre-push.sh` check the branch/ref and exit 0
  immediately for anything other than `main`. Every feature branch (including this one)
  gets **zero local hook-provided guarantee** — `just pre-commit` must be run manually, and
  nothing forces that before a PR is opened.
- **[ACCEPTED] `ci.yml`/`quality.yml` are `pull_request`-triggered CI**, which is the
  correct place for feature-branch validation per `CLAUDE.md`'s stated design. Accepted as
  intentional, contingent on branch protection actually requiring them to pass — which (see
  above) it currently does not.
- Existing open PRs as of this survey (Agent A): #356, #354, #353, #210 (open since
  2026-06-06) — a stale, long-open PR is itself evidence the single-reviewer path is a
  practical bottleneck, not just a theoretical one.

## 4. Release (tag → published artifact)

Path: `semantic-release.yml` (or manual) pushes a tag → `release.yml`
(`on: push: tags:` + `workflow_dispatch`) → build/test → GitHub Release + registry
publish (`publish-registry.yml`, `publish-candidate.yml`, `homebrew-release.yml`,
`release-debian.yml`, `docker-build-push.yml`).

SPOFs:
- **[BLOCKING — reported, not independently re-verified this pass] One release.yml gate
  broken for weeks.** The task brief names this as as-found evidence of a SPOF that already
  manifested; this survey did not re-derive the specific failing gate/date range
  independently (Unknown — flagged for a follow-up agent to pin down the exact commit range
  and gate name from Actions history; `gh run list --workflow=release.yml` was not run in
  this pass).
- **[BLOCKING] Tag push is the sole trigger for `release.yml`**, and tag-push authorization
  is gated only by repo write access — which (see PR SPOF above) is a single admin account
  with no branch protection forcing review before that account's own commits reach a
  tag-able state.
- **[ACCEPTED] `workflow_dispatch` fallback exists** specifically because
  `semantic-release.yml`-driven tag pushes and API-triggered `repository_dispatch` need a
  manual re-run path (documented in the workflow's own comments) — accepted as a reasonable
  redundancy, not itself a new SPOF, though it inherits the same single-admin gate.
- **[BLOCKING] `just sync`/`just sync-dry` recipes are currently broken** (confirmed by
  running them, per `CLAUDE.md`: `--audit`/`--dry_run true` don't exist on the live verb).
  If any release-adjacent automation calls these recipes rather than `ggen sync run
  --dry-run` directly, release-time sync would fail loudly (acceptable) or, if wrapped in
  `|| true` anywhere, fail silently (not checked in this pass — Unknown).
- Multiple independent publish targets (registry, homebrew, debian, docker) each with their
  own workflow — no evidence gathered this pass on whether a partial-publish (e.g. registry
  succeeds, homebrew fails) is detected/rolled back or left as silent partial state
  (Unknown, flagged).

## 5. Constitution (ccn: law → enforcement)

Path: `packs/ggen-constitution-pack/ontology.ttl` (14 `ccn:Law` individuals, I–XIV) →
SPARQL SELECT → `CONSTITUTION.md` + 2 internal-consistency gates (every Law has a
statement; `mechanized=true` requires `provenBy`).

SPOFs:
- **[BLOCKING] Constitution is a descriptive mirror only — not wired into admission.**
  Agent E, confirmed: `crates/praxis-core/src/law.rs`'s `DefaultLaw::admit` and
  `crates/ggen-engine/src/sync.rs`'s `write_receipt` (hardcoded `Andon::Green`, see §2) are
  independent of the constitution pack. A `ccn:Law` can be violated in spirit with zero
  effect on any live receipt/admission decision. Publication condition 12 remains PARTIAL.
- **[BLOCKING] Law XII cites a nonexistent test.** `reasoner_independence_e2e` — zero grep
  matches workspace-wide (Agent E finding #1). A law's own proof citation is currently
  false; the law's `ccn:mechanized` flag is not corrected to reflect this.
- **[ACCEPTED] Laws VII, XI, XIV explicitly `ccn:mechanized false`** with named gaps — this
  is honest disclosure, not a SPOF in itself, but each such law is, by definition, currently
  unenforced (accepted as disclosed non-enforcement, not silently claimed enforced).
- **[BLOCKING] No public/private namespace split, single `ccn:` namespace** — any pack or
  agent can assert `ccn:Law` individuals with no isolation between authoritative
  constitutional law and pack-local convention; nothing found gating who may add a `ccn:Law`.

## 6. Fleet-management (multi-repo / gh-terraform / GitHub Actions surface)

Path: `github-actions-pack` + `gh-terraform-pack` (merged, live on `main` via #347/#341/
#348/#334) → `scripts/gh/fleet-census.sh` → `docs/gh-terraform/FLEET-CENSUS.md` (labeled
"OBSERVATION, NOT DESIRED STATE").

SPOFs:
- **[BLOCKING] Fleet census is observation-only — no reconciliation loop confirmed live.**
  `FLEET-CENSUS.md` is explicitly read-only per its own header; no evidence found in this
  pass of an actuation path (e.g. `terraform apply` against the fleet) being gated by this
  census rather than run independently. If fleet state and census can diverge with nothing
  detecting it, the census is decorative for governance purposes even though it is honestly
  labeled (Unknown whether any consumer enforces freshness — flagged).
- **[BLOCKING] Single admin account is also the fleet-management actuator.** Same
  single-collaborator SPOF as §3, applied to any `terraform apply`/tag-push/collaborator
  mutation across the fleet — this survey did not enumerate `gh-terraform-pack`'s actual
  Terraform state/backend or who else has cloud-provider credentials (Unknown, out of
  scope for a repo-only survey — flagged for a follow-up with cloud IAM access).
- **[ACCEPTED] `docs/gh-terraform/` contains exactly one file** (not the "Japanese manuals,
  plural" the original task brief assumed — Agent E corrected this false premise). Accepted
  correction: fleet documentation surface is currently much thinner than assumed, which is
  itself worth flagging but is not a new SPOF beyond what's listed above.
- **[ACCEPTED, per Constitution] Terraform apply / tag push / release publish / secret or
  collaborator mutation are correctly treated as external-authorization-gated actuations**
  per the task's own non-termination law — none of those were executed by this survey; this
  document is the authorization-packet substitute (operation/target/plan-hash/etc. are N/A
  here because no actuation was performed, only observation).

---

## Cross-cutting SPOF (all 6 paths)

- **[BLOCKING] One person (`seanchatmangpt`) is a SPOF across every path**: sole repo
  collaborator with admin rights, sole implicit reviewer/approver (no CODEOWNERS, no branch
  protection), and — per the fleet-management gap above — plausibly the sole cloud-actuation
  identity as well. This single fact is the root cause underlying the PR, release, and
  fleet-management SPOFs above; it is listed once here and referenced, not re-derived, in
  each section.
- **[ACCEPTED] Non-termination law itself accepted and applied**: every gap above is
  recorded as an obligation (this document) rather than treated as a stop condition; the
  only genuinely stopped actuations are none — this was a pure observation/documentation
  pass.

## Explicit Unknowns (not silently filled)

- Exact failing gate/date-range for the "release.yml gate broken for weeks" claim.
- Whether any release-adjacent automation wraps the broken `just sync`/`just sync-dry`
  recipes in a silent-failure (`|| true`) path.
- Partial-publish detection/rollback across registry/homebrew/debian/docker release targets.
- Fleet census freshness enforcement and cloud-provider IAM/credential holders beyond the
  single GitHub collaborator.
- `docs/GENERATIONS.md`/`repo-facts.ttl` byte-identical regeneration on this exact worktree
  state (not re-run this pass).
- Full audit of all 54 `ALIVE` markers across the release/constitution packs (only 4
  spot-checked by Agent E).

## See Also

- `docs/retrofit/ggen/load-path/generation.mmd`
- `docs/retrofit/ggen/load-path/self-generation.mmd`
- `docs/retrofit/ggen/load-path/release.mmd`
- `docs/retrofit/ggen/load-path/constitution.mmd`
- `docs/retrofit/ggen/load-path/github.mmd`
- `.tcps/retrofit/ggen/load-path.ttl`
- `docs/retrofit/ggen/as-found/repository.md`, `generation-system.md`,
  `architecture-semantics.md` (as-found survey inputs)
- `docs/PUBLICATION_JUDGMENT.md` (self-hosting-ladder condition tracking referenced above)
