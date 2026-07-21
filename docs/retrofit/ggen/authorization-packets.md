# Authorization Packets — ggen Self-Retrofit Generation 1

Items in this file are actuations that mutate GitHub repository settings, collaborator
configuration, or other externally-authorization-gated state. No agent in this retrofit
generation executed any of them. Each entry is a full authorization packet per the
non-termination law (operation / target / plan hash / creates / updates / deletes /
rollback / post-checks / receipt path) — never a bare "blocked".

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
- **Plan hash**: not computed — no plan artifact was generated because the operation was
  never staged (see Non-actuation below). A `terraform plan`/`gh api --dry-run` equivalent
  would need to be run and hashed before this packet could be executed.
- **Creates**: a branch protection rule on `main` (required reviews, required status checks
  from `ci.yml`/`quality.yml`); a `CODEOWNERS` file at repo root.
- **Updates**: repository settings (`branches/main/protection` object); no change to existing
  collaborator admin grants is proposed by this packet alone (that is a separate, narrower
  authorization decision the user may want to make independently).
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

**Non-actuation**: applying branch protection and modifying collaborator/CODEOWNERS
configuration are both externally-authorization-gated actuation classes under this session's
operating rules (repo-settings mutation and collaborator-config mutation). This packet is
written for the user's review and explicit go-ahead; no agent in this retrofit generation
attempted `gh api -X PUT .../protection` or committed a `CODEOWNERS` file. The already-merged
`gh-terraform-pack` (PRs #341/#348/#334) is the most natural generator of the desired-state
Terraform/API payload once authorized — it was not invoked to actually apply anything here.
