# github-actions-pack

TCPS production-process pack for GitHub Actions. First-class pack, prefix
`gha: <http://seanchatmangpt.github.io/packs/github-actions#>`.

**Schema-only, project-agnostic** (v0.2.0 rebuild — see pack.toml for the
full before/after). `ontology.ttl` carries **zero** `gha:Workflow` /
`gha:Job` / `gha:Step` / `gha:CompositeAction` individuals. This pack is
pure vocabulary; every real fact about a real project's real CI comes from
that project's own instance-data `.ttl`, authored in that project's own
repo. This is the same contract `dspy-pack` documents: *"ontology.ttl ships
zero individuals... a consumer project supplies its own individuals against
this vocabulary in its own .ttl."*

## The TCPS split

- `gh-terraform-pack` = 工場配置と管理標準 — Terraform is institutional state: what the
  factory (repos, teams, branch protection, secrets plumbing) *is*.
- `github-actions-pack` = 生産設備と工程標準 — Actions is the production process on events:
  what the factory *does* when a production obligation comes into existence.

## Five layers (ontology model)

1. **Event** — when a production obligation comes into existence (push, pull_request,
   merge_group, workflow_dispatch, workflow_call, schedule, release, issues,
   repository_dispatch, deployment, workflow_run). Fixed, real GitHub vocabulary —
   the only layer whose individuals genuinely are shared across every consumer,
   since every repo's workflows fire on the same event kinds.
2. **Workflow** — identity, purpose, triggers, inputs/outputs, secrets, concurrency,
   permission ceiling, evidence obligations, standing effect.
3. **Job** — dependencies, runner, matrix, timeout, permissions, artifacts,
   evidence, failure classification. The model distinguishes step-executed ≠
   step-succeeded ≠ job-succeeded ≠ evidence-admitted ≠ standing-acquired
   (`gha:executionStates`).
4. **Step** (new in v0.2.0) — what a job actually *does*: an ordered
   (`gha:stepOrder`) sequence of either `gha:usesAction` calls or
   `gha:runCommand` shell blocks, with `gha:withParam`/`gha:envVar` and
   `gha:continueOnError`. Without this layer nothing can render real
   `steps:` YAML — the prior revision had no way to express "this job runs
   `cargo test`" as a fact at all, only as a hand-written string.
5. **Production-output** — binary, test-report, docs, package, release-asset, SBOM,
   attestation, receipt, drift-report, standing-judgment. Plus a parallel
   **CompositeAction** shape (identical Step-reuse pattern) for reusable
   `.github/actions/<name>/action.yml` composite actions.

## Refusals (gates)

`gates/010_required.rq` — every non-`gha:ownedBy` `gha:Workflow` needs
`purpose`/`trigger`/`permissionCeiling`; every `gha:Job` needs `runner` and
permission coverage; every `gha:Step` needs `inJob`, `stepOrder`, and at
least one of `usesAction`/`runCommand`.

`gates/020_security.rq` REFUSES: `write-all`; permission-ceiling grants not
derivable from a declared `gha:performsOperation`; mutable third-party
action refs (`gha:usesAction` values must be pinned to a 40-hex commit SHA
— this branch is live for the first time in v0.2.0, since v0.1.0 never had
any individual populating `gha:usesAction`); unbounded `pull_request_target`
without `gha:prTargetJustification`; secret use without a declared
`gha:secretReason`.

## How a consumer uses this pack

1. Author your own instance-data `.ttl` in your own repo declaring
   `gha:Workflow`, `gha:Job`, and `gha:Step` individuals against this
   pack's `gha:` vocabulary — one `gha:Workflow` per `.github/workflows/`
   file you want generated, one `gha:Job` per job, one `gha:Step` per step
   (ordered via `gha:stepOrder`).
2. Wire this pack into your `ggen.toml` (either a `[[packs]]` entry, or an
   `[ontology].imports` line pointing at `packs/github-actions-pack/ontology.ttl`
   plus your own instance file — match whichever convention your project's
   `ggen.toml` already uses).
3. Add a `[[generation.rules]]` entry per template: `output_file` containing
   `{{ fileName }}` (or `{{ actionDirName }}` for composite actions) triggers
   ggen-engine's per-row rendering — one output file per row of the rule's
   own query, one row per `gha:Workflow`/`gha:CompositeAction`. See
   `templates/workflow.yml.tmpl`'s own header comment for the exact query
   shape expected.
4. Run your project's real sync/generate command. Diff the result against
   whatever you're migrating from before trusting it — see this pack's own
   validation against ggen's real `ci.yml` for the pattern.

Escape hatch: `gha:artifactBody` still exists on `gha:Workflow` for a
consumer who wants to migrate incrementally rather than fully modeling a
workflow's Jobs/Steps before generating anything — but composing real
`gha:Job`/`gha:Step` facts is the intended path, not the exception.

## Layout

- `pack.toml` — pack identity, schema-only contract stated in the description
- `ontology.ttl` — vocabulary only: Event/Workflow/Job/Step/Output/Operation/
  CompositeAction classes and properties. Zero individuals of the
  project-specific classes.
- `gates/*.rq` — refusal gates, UNION + FILTER NOT EXISTS, namespace-scoped
- `templates/*.tmpl` — `workflow.yml.tmpl`, `composite_action.yml.tmpl`

## Not ported in this revision

Two of the prior revision's templates (`reusable-rust-inspection` workflow +
caller example, `emit-evidence` composite action) were inspection-workflow
specifics with no generic value in their prior form and were retired, not
rebuilt, in this pass. A future revision wanting that functionality should
model it the same way — real `gha:Job`/`gha:Step` facts, not a blob.

## See Also

- `packs/gh-terraform-pack/` — institutional-state counterpart
- `packs/dspy-pack/` — the schema-only, zero-individuals contract this
  revision follows
