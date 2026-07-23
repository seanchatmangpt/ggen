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

**Non-actuation**: environment protection-rule mutation is a permission-adjacent repo-settings
actuation class under this session's operating rules, same category as AP-001's branch
protection. No agent has attempted either `gh api` call above. Written for the user's review and
explicit go-ahead.
