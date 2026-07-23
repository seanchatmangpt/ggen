# Authorization Packets — ggen Self-Retrofit

Items in this file are actuations that mutate GitHub repository settings, collaborator
configuration, or other externally-authorization-gated state. No agent in this retrofit has
executed any of them. Each entry is a full authorization packet per the non-termination law
(operation / target / plan hash / creates / updates / deletes / rollback / post-checks / receipt
path) — never a bare "blocked".

## AP-001 — Apply branch protection + CODEOWNERS to `seanchatmangpt/ggen`

- **Operation**: `gh api -X PUT repos/seanchatmangpt/ggen/branches/main/protection` (branch
  protection ruleset) + commit a `CODEOWNERS` file to `main`.
- **Target**: `seanchatmangpt/ggen`, branch `main`, GitHub repo settings (Settings → Branches)
  and collaborator/review configuration.
- **Why it's proposed**: confirmed this pass via `gh api repos/seanchatmangpt/ggen/branches/main/protection`
  → `404` (no branch protection exists), and exactly one repository collaborator carries
  `admin=true` with no `CODEOWNERS` file present. This is a cross-cutting SPOF across the
  pull-request, release, and fleet-management load paths modeled in
  `.tcps/retrofit/ggen/load-path.ttl` (item #7 in the intervention decision set,
  `docs/retrofit/ggen/intervention/decision.md`) — a single account can merge to `main`
  or push tags/releases with no second reviewer and no ownership routing.
- **Plan hash**: `22ca1e390ef7e0ce55d4f1b87fa8e65d5950ce5b1e196bc1b60b2012d777e2de` — computed
  2026-07-21 from `packs/gh-terraform-pack/ontology.ttl`'s `ghtf:MainBranchProtection` HCL body
  (already-generated desired state, PRs #341/#348/#334), translated to the equivalent GitHub
  REST API PUT body (`required_status_checks.strict=true`, contexts `Check/Test/Build/Doctest/
  CI Status`, `enforce_admins=true`, `required_pull_request_reviews.required_approving_review_count=1`,
  `require_code_owner_reviews=false`). Local computation only, not sent. Full plan body recorded
  in `.tcps/retrofit/ggen/controller/authorization-packets/AP-001.json`. Re-verified live against
  GitHub 2026-07-21 16:xx — target state unchanged (still 404 protection, still no CODEOWNERS,
  still single admin), plan hash remains valid.
- **Creates**: a branch protection rule on `main` (required reviews, required status checks
  from `ci.yml`/`quality.yml`); a `CODEOWNERS` file at repo root.
- **Updates**: repository settings (`branches/main/protection` object); no change to existing
  collaborator admin grants is proposed by this packet alone (that is AP-002, below).
- **Deletes**: nothing.
- **Rollback**: `gh api -X DELETE repos/seanchatmangpt/ggen/branches/main/protection` removes
  the protection rule; `git rm CODEOWNERS && git commit` removes the ownership file. Both are
  single-command, non-destructive to any existing commit history.
- **Post-checks**: `gh api repos/seanchatmangpt/ggen/branches/main/protection` returns non-404
  with the expected required-checks list; a test PR from a non-admin account is blocked from
  merging without review to confirm the rule is enforced, not just present.
- **Receipt path**: none written — this packet has not been executed. If authorized and run,
  the executing agent should record the `gh api` response body under
  `docs/retrofit/ggen/authorization-packets-executed/AP-001.json` as the receipt.
- **Coverage note (added 2026-07-21, self-retrofit Generation 4)**: AP-001 fully covers
  `retrofit:NoBranchProtection`. It only PARTIALLY covers `retrofit:SingleAdminCollaborator`
  (the CODEOWNERS half; the "exactly one admin collaborator" half is explicitly out of scope,
  see Updates above) and does NOT meaningfully cover `retrofit:CrossCuttingSingleAdmin` (whose
  own stated root cause — the sole admin account itself — is exactly what AP-001 declines to
  touch; neither ReleasePath's tag-push/workflow_dispatch exposure nor FleetManagementPath's
  cloud-actuation identity gap is addressed by branch-protection/CODEOWNERS at all). See AP-002.

**Non-actuation**: applying branch protection and modifying collaborator/CODEOWNERS
configuration are both externally-authorization-gated actuation classes under this session's
operating rules (repo-settings mutation and collaborator-config mutation). This packet is
written for the user's review and explicit go-ahead; no agent in this retrofit has attempted
`gh api -X PUT .../protection` or committed a `CODEOWNERS` file. The already-merged
`gh-terraform-pack` (PRs #341/#348/#334) is the most natural generator of the desired-state
Terraform/API payload once authorized — it was not invoked to actually apply anything here.

## AP-002 — Add a second admin collaborator to `seanchatmangpt/ggen`

- **Operation**: `gh api -X PUT repos/seanchatmangpt/ggen/collaborators/<second-account>
  -f permission=admin` (or `-f permission=maintain`/`write`, a narrower grant, at the user's
  discretion — see Design note below).
- **Target**: `seanchatmangpt/ggen`, repository collaborator list.
- **Why it's proposed**: `retrofit:SingleAdminCollaborator` and `retrofit:CrossCuttingSingleAdmin`
  (`.tcps/retrofit/ggen/load-path.ttl`) both name the same root cause — `seanchatmangpt` is the
  sole collaborator with `admin=true` (confirmed live, `gh api repos/seanchatmangpt/ggen/collaborators`
  → exactly one entry, re-verified 2026-07-21). AP-001's own branch-protection rule
  (`required_approving_review_count=1`) cannot be meaningfully satisfied with only one
  collaborator on the repository — there is no second reviewer to approve a PR the sole admin
  didn't author, and required-review protection would either deadlock every PR or need to be
  bypassed by the same admin account, defeating its purpose. AP-001 explicitly declines to
  change collaborator grants (see its Updates field); this packet names the actual gap AP-001
  leaves open.
- **Design note (this is NOT an internal design choice, per this session's DESIGN_UNCERTAINTY_LAW
  escalation exception)**: WHO the second admin/collaborator should be is a real external
  trust decision — a specific person or account the user actually trusts with this repository's
  keys — that cannot be derived from repository law or evidence. This packet cannot name a
  specific account; it names the operation and its consequences, and requires the user to
  supply the identity.
- **Plan hash**: not computed — cannot be, until the user supplies the target account identity
  and permission level (the exact API payload depends on both). Compute on request once those
  are known.
- **Creates**: a new collaborator-invitation entry on `seanchatmangpt/ggen` at the specified
  permission level (pending until accepted by the invited account).
- **Updates**: nothing existing.
- **Deletes**: nothing.
- **Rollback**: `gh api -X DELETE repos/seanchatmangpt/ggen/collaborators/<account>` removes the
  collaborator (or the pending invitation) cleanly; no effect on repository history or content.
- **Post-checks**: `gh api repos/seanchatmangpt/ggen/collaborators --jq '.[] | {login,admin:.permissions.admin}'`
  shows 2 entries; a test PR authored by the sole prior admin can be approved by the new
  collaborator to confirm AP-001's required-review rule is now actually satisfiable.
- **Receipt path**: none written — unexecuted. If authorized, record the `gh api` response body
  under `docs/retrofit/ggen/authorization-packets-executed/AP-002.json`.
- **Explicitly out of scope for this packet**: `retrofit:CrossCuttingSingleAdmin`'s ReleasePath
  leg (tag-push/`workflow_dispatch` protection on `release.yml` — GitHub's tag-protection rules
  are a separate settings surface from branch protection or collaborator grants, not addressed
  here) and its FleetManagementPath leg (`retrofit:FleetCloudIdentityUnverified`, cloud/IAM
  actuation identity for fleet-wide operations — no cloud IAM access exists in this environment
  to even observe current state, let alone propose a remediation; stays `Unknown`, not folded
  into this packet). A full close of `CrossCuttingSingleAdmin` needs AP-001 + AP-002 + a
  separate tag-protection packet + a separately-scoped cloud-identity investigation this session
  cannot perform.

**Non-actuation**: collaborator/permission mutation is an externally-authorization-gated
actuation class under this session's operating rules. No agent has attempted this API call.
Written for the user's review; requires the user to supply the target account identity before
it can even be planned precisely, let alone executed.

## AP-004 — Allow `main` to deploy to the `github-pages` environment on `seanchatmangpt/ggen`

- **Operation**: `gh api -X POST repos/seanchatmangpt/ggen/environments/github-pages/deployment-branch-policies
  -f name=main` (add `main` as an allowed deployment branch), optionally followed by
  `gh api -X DELETE repos/seanchatmangpt/ggen/environments/github-pages/deployment-branch-policies/36744262`
  (remove the stale `master` entry — safe either way, since `master` doesn't exist as a branch
  in this repo and can never itself trigger a deployment).
- **Target**: `seanchatmangpt/ggen`, the `github-pages` deployment environment's branch policy
  (Settings → Environments → github-pages → Deployment branches and tags).
- **Why it's proposed**: confirmed this pass via `gh api repos/seanchatmangpt/ggen/environments/
  github-pages` (`deployment_branch_policy: {protected_branches: false, custom_branch_policies:
  true}`) and `.../deployment-branch-policies` (`branch_policies: [{"name": "master", "id":
  36744262}]`) — every run of `.github/workflows/publish-registry.yml` ("Deploy Documentation to
  GitHub Pages", which declares `environment: {name: github-pages}`) fails in ~2 seconds with
  "Branch main is not allowed to deploy to github-pages due to environment protection rules,"
  before any build step executes. This repo's real default/only branch is `main`; the
  environment's policy naming `master` is a stale artifact (environment created 2025-10-09,
  predating or never updated for whatever point this repo's default branch became `main`) — a
  rename gap, not a deliberate restriction.
- **Plan hash**: not computed — this operation has no template/generated desired-state artifact
  to hash against (unlike AP-001's `gh-terraform-pack`-generated HCL); the API call itself is the
  plan, stated in full above. Re-verified live 2026-07-22 — target state unchanged (still only
  `master` listed, still no `main`).
- **Creates**: one new deployment-branch-policy entry (`name: main`) on the `github-pages`
  environment.
- **Updates**: nothing existing (the `master` entry is left in place unless the optional DELETE
  is also run).
- **Deletes**: nothing, unless the optional DELETE step is included (removes the dead `master`
  policy entry; harmless either way since no branch named `master` exists in this repo).
- **Rollback**: `gh api -X DELETE repos/seanchatmangpt/ggen/environments/github-pages/deployment-branch-policies/<new-id>`
  (the id returned by the POST response) removes the added policy entry; if the optional `master`
  DELETE was also run, `gh api -X POST .../deployment-branch-policies -f name=master` restores it.
  Both are single-command, non-destructive to any deployment history.
- **Post-checks**: `gh api repos/seanchatmangpt/ggen/environments/github-pages/deployment-branch-policies`
  shows `main` in the list; a subsequent push to `main` (or `gh workflow run publish-registry.yml`)
  no longer fails at the environment-protection-rule check (it may still fail further in, at the
  build steps — see the separate, real, disclosed docs/src/ gap named in
  `retrofit:GithubPagesEnvironmentStaleMasterPolicy`, which this packet does not claim to fix).
- **Receipt path**: none written — this packet has not been executed. If authorized and run, the
  executing agent should record the `gh api` response bodies under
  `docs/retrofit/ggen/authorization-packets-executed/AP-004.json`.
- **Explicitly out of scope for this packet**: the missing `docs/src/` mdbook source tree
  (deleted 2026-03-31, never restored) that would make `publish-registry.yml`'s build step fail
  even after this environment-policy fix and this session's separate toolchain-setup fix land —
  a documentation-scope/product decision (redirect to `book/`, restore content, or retire this
  workflow and its near-duplicate `deploy-docs.yml`), not an environment-permission question,
  and not addressed here.

## AP-005 — Provision a `HOMEBREW_TAP_TOKEN` secret for `seanchatmangpt/ggen`

- **Operation**: `gh secret set HOMEBREW_TAP_TOKEN -R seanchatmangpt/ggen` with a real GitHub
  Personal Access Token (classic or fine-grained) that has write/fork access to the external
  `seanchatmangpt/homebrew-tap` repository.
- **Target**: `seanchatmangpt/ggen`'s repository secrets (Settings → Secrets and variables →
  Actions), consumed by `.github/workflows/homebrew-release.yml`'s "Bump Homebrew Formula" step
  (`mislav/bump-homebrew-formula-action`).
- **Why it's proposed**: confirmed this pass via a real, live dispatch — `release.yml`'s own
  `Dispatch Homebrew Formula Update` job (part of a fully green `release.yml` run,
  databaseId 29968670738, triggered by the real `v26.7.52` release) successfully invoked
  `homebrew-release.yml`, which then failed with `HttpError: Resource not accessible by
  integration - https://docs.github.com/rest/repos/forks#create-a-fork`. Reading the failing
  step directly: `env: COMMITTER_TOKEN: ${{ secrets.HOMEBREW_TAP_TOKEN || secrets.GITHUB_TOKEN
  }}` — the workflow already anticipates needing a dedicated token and falls back to the
  default `GITHUB_TOKEN` when `HOMEBREW_TAP_TOKEN` isn't configured. That fallback is
  guaranteed to fail: the default `GITHUB_TOKEN` is scoped to the repository the workflow runs
  in and has no access — not even fork access — to a different, external repository
  (`seanchatmangpt/homebrew-tap`), confirmed via `gh api repos/seanchatmangpt/ggen/actions/
  secrets` returning zero secrets configured at any point checked this session. This is not a
  code bug: the automation is correctly written and was always waiting on this one credential.
- **Plan hash**: not computed — no template/generated desired-state artifact to hash against;
  the operation is fully specified above (set one named secret to a value the user must supply
  and this session cannot generate, since it requires real write authority over an external
  repository this session has no standing to act on).
- **Creates**: one new repository secret, `HOMEBREW_TAP_TOKEN`.
- **Updates**: nothing existing (this is the first time this secret would be set — confirmed
  zero secrets currently configured).
- **Deletes**: nothing.
- **Rollback**: `gh secret remove HOMEBREW_TAP_TOKEN -R seanchatmangpt/ggen` removes it cleanly;
  the workflow's own `|| secrets.GITHUB_TOKEN` fallback means removing the secret returns the
  workflow to its exact current (failing) state, not a worse one.
- **Post-checks**: `gh workflow run homebrew-release.yml -f tag=v26.7.52` (a real, already-proven
  dispatch path, needs no new release) completes with the "Bump Homebrew Formula" step
  succeeding; `gh api repos/seanchatmangpt/homebrew-tap/contents/Formula/ggen.rb` shows the
  formula's `version` field updated to match.
- **Receipt path**: none written — this packet has not been executed. If authorized and run,
  the executing agent should record confirmation (not the token itself) under
  `docs/retrofit/ggen/authorization-packets-executed/AP-005.json`.
- **Explicitly out of scope for this packet**: generating or choosing the PAT itself is not
  something this session can do — it requires a human with real GitHub account authority over
  `seanchatmangpt/homebrew-tap` to create a token and supply its value. This packet only
  specifies the mechanical operation (where the value goes, what consumes it, how to verify and
  roll back), not the credential's origin.

**Non-actuation**: secret provisioning is an externally-authorization-gated actuation class
under this session's operating rules, the same category as AP-001/AP-002/AP-004. No agent has
attempted `gh secret set` or requested/generated a token value. Written for the user's review;
requires the user to supply the actual PAT value before this can be executed.

**Non-actuation**: environment protection-rule mutation is a permission-adjacent repo-settings
actuation class under this session's operating rules, same category as AP-001's branch
protection. No agent has attempted either `gh api` call above. Written for the user's review and
explicit go-ahead.

## AP-008 — What should ggen's public documentation guide contain (docs-scope decision, not a GitHub mutation)

**Different gate class from AP-001/002/004/005/007 above**: not a GitHub repo-settings mutation
blocked by the explicit-`!` list, but a `DESIGN_UNCERTAINTY_LAW` escalation case in the same
spirit as AP-006 (`verifier_identity` trust model) — a real product-scope question this session
should surface cleanly rather than silently decide, since it fixes what public claim the project
makes about its own documentation.

- **Background**: two workflows try to deploy a documentation guide and both fail for the same
  underlying reason — `docs/src/` (the mdbook source tree `publish-registry.yml`'s
  `retrofit:GithubPagesEnvironmentStaleMasterPolicy` and `deploy-docs.yml` both build from) was
  deleted 2026-03-31 (commit `e6113738`, "delete 1,892 stale MD/TXT files", a deliberate cleanup
  pass) and never restored. `book/` is a real, actively-maintained mdbook project that exists
  today (`book/book.toml`'s title: "ggen: Manufacturing Level Five Packs") and is already
  content-verified as part of the real, working `publish-candidate.yml` pipeline (`Book gates
  (check_book / check_level_five)` step, `book/scripts/check_book.py` /
  `check_level_five.py`) — but it has never been built to HTML or deployed anywhere; it's
  correctness-checked source only. This session's fix to `deploy-docs.yml` (see the accompanying
  PR) removes its now-permanently-broken mdbook step and deploys only what's unambiguously real
  today (the `cargo doc` Rust API reference) — it does not resolve this question, and was not
  designed to.
- **The decision**: what should back a public documentation *guide* (as opposed to the
  auto-generated Rust API reference, which is unaffected either way):
  - **Design A — deploy `book/` as the public guide.** Point `deploy-docs.yml` (and/or
    `publish-registry.yml`, once AP-004 unblocks its environment policy) at `book/` instead of
    the deleted `docs/`. Smallest technical change, reuses already-maintained, already
    correctness-checked content. Risk: `deploy-docs.yml`'s own workflow name ("Deploy
    Documentation") and a root-level public URL both imply comprehensive product documentation,
    but `book/`'s actual title and scope are narrower ("Manufacturing Level Five Packs") —
    a real public-facing scope mismatch, not a technical one.
  - **Design B — restore `docs/src/` from git history.** Reverts the 2026-03-31 cleanup pass's
    own considered decision to remove 1,892 files judged stale at the time. Recommended
    *against*: nothing has re-verified that restored content is still accurate given four
    months of subsequent architecture change (this session alone touched dozens of crates,
    packs, and workflows) — resurrecting unverified stale content is a worse default than either
    A or C.
  - **Design C — retire the guide-deploy capability entirely, ship only the API reference.**
    Consistent with this session's own `retrofit:MarketplaceValidateRetiredVestigial` precedent
    (retire rather than repair when there's no real, current content to serve a workflow's
    stated purpose). Correct if the maintainer does not currently want a public guide site at
    all, or wants to defer that decision indefinitely. Loses whatever value a deployed guide
    (even `book/`'s narrower one) would provide.
  - No design is recommended over another here (unlike AP-001/AP-006/AP-007, where an internal
    technical-soundness argument favored one option) — A vs. C is a genuine product-positioning
    call about what ggen publicly claims to document, not a technical tradeoff this session can
    resolve by itself.
- **Plan hash**: not applicable — this packet requests a scope decision, not a mutation with a
  fixed desired-state payload. Whichever design is chosen becomes an ordinary content/workflow
  PR through this session's existing merge criteria (no `!` needed for that follow-on PR itself
  — only the choice of scope is what's being surfaced here).
- **Creates / Updates / Deletes**: nothing yet — this session has not picked a design.
- **Rollback**: not applicable (no actuation proposed).
- **Post-checks**: once a design is chosen and implemented, `deploy-docs.yml` (or
  `publish-registry.yml`, once AP-004 lands) should deploy real, current content matching
  whichever scope was chosen, observable at the live GitHub Pages URL.
- **Receipt path**: none — no design has been chosen.

**Non-actuation**: this packet requests a product-scope decision, not an infrastructure
mutation — no agent has picked a design or restored/redirected any documentation content beyond
what the accompanying PR already does (remove the dead mdbook step, keep the working API
reference). Written so the user can approve one design (or propose a fourth) without this
session silently deciding what ggen's public docs site claims to be.
