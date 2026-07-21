# github-actions-pack

TCPS production-process pack for GitHub Actions. First-class pack, prefix
`gha: <http://seanchatmangpt.github.io/packs/github-actions#>`.

## The TCPS split

- `gh-terraform-pack` = 工場配置と管理標準 — Terraform is institutional state: what the
  factory (repos, teams, branch protection, secrets plumbing) *is*.
- `github-actions-pack` = 生産設備と工程標準 — Actions is the production process on events:
  what the factory *does* when a production obligation comes into existence.

## Four layers (ontology model)

1. **Event** — when a production obligation comes into existence (push, tag, PR,
   release-published, dispatch, merge_group, path filters).
2. **Workflow** — identity, purpose, inputs/outputs, secrets, concurrency, permission
   ceiling, evidence obligations, standing effect.
3. **Job + Step** — dependencies, runner, matrix, timeout, permissions, artifacts,
   evidence, failure classification. The model distinguishes step-executed ≠
   step-succeeded ≠ job-succeeded ≠ evidence-admitted ≠ standing-acquired.
4. **Production-output** — binary, test-report, docs, package, release-asset, SBOM,
   attestation, receipt, drift-report, standing-judgment.

## Refusals (gates)

Declared operations derive minimal `GITHUB_TOKEN` permissions. The pack REFUSES:
`write-all`; unspecified permissions (top-level and job-level both absent); unnecessary
`contents: write`; unbounded `pull_request_target`; mutable third-party action refs
(pinned 40-hex SHAs required); secret use without a declared reason.

## As-built inventory (origin/main `f775fbb8b`, 2026-07-20)

24 workflows. Full structured extraction in `SURVEY.md` (seed data for Build agents).
Ownership: `publish-candidate.yml` belongs to `ggen-release-pack`; `tcps-drift.yml`
(not yet on main) belongs to `gh-terraform-pack`; `tf-acceptance.yml` is hand-written —
this pack must not double-own any of them.

| Workflow | Triggers | Top-level permissions | Jobs | Unpinned refs |
|----------|----------|----------------------|------|---------------|
| automated-rollback.yml | dispatch | contents:write, deployments:write | 5 | 0 |
| ci.yml | PR, push main, merge_group | contents:read | 8 | 0 |
| deploy-docs.yml | push main (paths), dispatch | contents:write | 1 | 0 |
| docker-build-push.yml | tag v*, dispatch | contents:read, packages:write | 4 | 0 |
| docker.yml | tag v*, dispatch | contents:read | 1 | 0 |
| docs.yml | PR/push (md paths) | contents:read | 1 | 0 |
| erlang-ci.yml | push (paths), dispatch | contents:read | 5 | 0 |
| erlang-release.yml | erlang tags | contents:write | 5 | 0 |
| example-tpot2.yml | PR/push (example paths) | contents:read | 1 | 0 |
| generate-release-notes.yml | release, dispatch | (job-level only) | 1 | 0 |
| homebrew-release.yml | release | contents:read | 1 | 0 |
| marketplace-deploy.yml | push main (paths), dispatch | read + pages/id-token:write | 3 | 0 |
| marketplace-docs.yml | push main (paths), dispatch | (job-level only) | 1 | 0 |
| marketplace-test.yml | push main, dispatch | contents:read | 1 | 0 |
| marketplace-validate.yml | push (paths), dispatch | contents:read | 3 | 0 |
| marketplace.yml | push main (paths), dispatch | read + pages/id-token:write | 2 | 0 |
| publish-candidate.yml | push main | contents:write | 1 | 0 |
| publish-registry.yml | push main, dispatch | read + pages/id-token:write | 1 | 0 |
| quality.yml | PR, push main, merge_group | contents:read | 6 | 0 |
| release-debian.yml | tag v*.*.* | contents:write | 1 | 0 |
| release.yml | tag v*, dispatch | contents:write | 4 | 0 |
| secrets-sync.yml | push (tf paths), dispatch | contents:read (+ per-job) | 4 | 0 |
| semantic-release.yml | push main, dispatch | write x4 (incl. actions:write) | 2 | 0 |
| tf-acceptance.yml | dispatch | contents:read | 1 | **4** |

Security finding: 4 mutable action refs, all in `tf-acceptance.yml`
(`actions/checkout@v4`, `dtolnay/rust-toolchain@stable`, `extractions/setup-just@v2`,
`hashicorp/setup-terraform@v3`). No `write-all`, no `pull_request_target` anywhere.

## Layout

- `pack.toml` — pack identity
- `ontology.ttl` — gha: individuals (Build agents)
- `gates/*.rq` — refusal gates, UNION + FILTER NOT EXISTS, namespace-scoped (Build agents)
- `templates/*.tmpl` — frontmatter `to:`/`sparql:` templates (Build agents)
- `SURVEY.md` — as-built seed data (this survey)
- Tier-1 proof: `crates/ggen-engine/tests/github_actions_pack_e2e.rs`

## See Also

- `packs/gh-terraform-pack/` — institutional-state counterpart
- `packs/tcps-release-pack/` — proven CI-workflow-generating pack shape
