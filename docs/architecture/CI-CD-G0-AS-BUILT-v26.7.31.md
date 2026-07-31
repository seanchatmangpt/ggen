# ggen CI/CD G0 As-Built Inventory and Chesterton Fences

**Checkpoint**: G0 — inventory and fence  
**Implementation base**: `main@2364ccda8d1a38f14e314365583c99f3fb81357d`  
**Standing**: `PARTIAL_ALIVE`  
**Promotion authority**: none; G0 does not emit `ALIVE`

## Preserved system

G0 preserves the complete current GitHub Actions estate. It does not add, delete, disable, rename, or behaviorally alter a workflow. It does not change release, deployment, registry, secret, rollback, or source-mutation behavior. It adds an executable observation and refusal boundary around the estate that already exists.

The historical survey in `packs/github-actions-pack/SURVEY.md` observed 24 workflow files at `f775fbb8b147ccdde2e2629df944552ffbfc6214`. Between that exact tree and the implementation base, 25 workflow files were added and `marketplace-validate.yml` was retired. The resulting current inventory is 48 workflow files.

The admitted inventory is:

`packs/github-actions-pack/observations/g0-workflow-inventory-v26.7.31.toml`

Every workflow entry names:

- one semantic owner;
- its current purpose;
- its evidence output;
- its production output;
- its Chesterton-fence retirement condition.

Triggers, workflow-level and job-level permission ceilings, jobs, action invocations, run commands, mutable action references, and detectable evidence mechanisms are derived from the exact checked-in YAML. They are not copied into a second hand-maintained YAML model.

## Calculus

```text
checked-in .github/workflows/*.{yml,yaml}
        + admitted semantic inventory
        + exact-set closure
        + production-output ownership closure
        + derived trigger / permission / job / action / command facts
        = G0 as-built evidence
```

The inventory verifier is:

`scripts/ci/verify-g0-workflow-inventory.py`

It writes deterministic evidence to:

- `target/ci-g0/workflow-inventory.json`;
- `target/ci-g0/workflow-inventory.md`.

The topology analyzer is:

`scripts/ci/analyze-g0-workflow-topology.py`

It writes deterministic evidence to:

- `target/ci-g0/workflow-topology.json`;
- `target/ci-g0/workflow-topology.md`.

The strongest lawful claim is `PARTIAL_ALIVE`: the inventory is complete, owner and fence fields are present, no production output has two semantic owners, and the exact YAML topology is inspectable. G0 does not prove runtime equivalence, impact planning, external standing, generated YAML authority, or BRCE release admission.

## Refusals

Two checked-in negative fixtures execute through real Python subprocesses:

1. an inventory that omits a checked-in workflow is refused as `CI-G0-INVENTORY-001`;
2. two distinct owners assigned to one production output are refused as `CI-G0-OWNERSHIP-001`.

The fixture executor is:

`scripts/ci/test-g0-workflow-inventory.py`

The repository integration boundary is:

`crates/ggen-engine/tests/ci_g0_inventory_e2e.rs`

That test executes both analyzers against the real repository tree, verifies their generated filesystem evidence, then executes both refusal cases. It is automatically included in the existing required full-workspace integration job. No new workflow or Rust crate is introduced.

## Observed queue and duplication baseline

PR #524's documentation-only head `53f751b097993bca6bcd9e1f9d86b764620e0a4b` produced:

- 8 CI jobs;
- 6 Quality jobs;
- a skipped Docs workflow;
- 14 checkout job occurrences;
- 13 `setup-ggen-build` job occurrences.

The observed runs were still executing when G0 evidence was captured. Wall-clock duration is therefore `UNKNOWN`, not estimated.

The inventory verifier derives a duplicate command/action map across all 48 workflow files. It also exposes three shared production surfaces under singular semantic owners: GitHub Pages across five workflows, Docker Hub across two workflows, and GitHub Releases across four workflows.

The topology analyzer separately derives:

- event-to-workflow fan-out;
- duplicate trigger signatures;
- complete top-level and job-level permission observations;
- every `uses:` reference;
- mutable third-party action references that are not pinned to a full 40-hex commit SHA.

These measurements expose current overlap and permission/action debt without changing any caller during G0.

## Branch-protection boundary

The connected repository surface exposes workflow runs and check conclusions but does not expose authoritative branch-protection or ruleset configuration. The required-check set is therefore `UNKNOWN`.

`CI / CI Status` is recorded only as the observed current aggregate. It is not asserted to be the complete administrative requirement. The frozen target remains:

- `ci/admission`;
- `ci/inspection`;
- `ci/capabilities`;
- `ci/standing`.

## Fenced gaps and exclusions

- `.github/workflows/reusable-rust-inspection.yml` is absent at the implementation base. The PRD/ARD names it as an architectural seed, but G0 does not fabricate or activate it. G1 must resolve the source/projection discrepancy before shadow execution.
- `fortune5-bblock-normalize.yml` directly commits and pushes bounded normalization changes to a pull-request branch. G0 records its write ceiling, production output, and retirement fence; it does not alter that behavior.
- Mutable action references are inventoried and surfaced, not repaired during G0.
- Other mutation and release workflows remain fenced until an exact-head shadow replacement proves equivalent or stronger behavior and the relevant actuation is admitted through BRCE.
- A cache hit, workflow status label, or generated report does not establish release standing.

## Verification ladder

```bash
python3 scripts/ci/verify-g0-workflow-inventory.py
python3 scripts/ci/analyze-g0-workflow-topology.py
python3 scripts/ci/test-g0-workflow-inventory.py
cargo test -p ggen-engine --test ci_g0_inventory_e2e
```

The first command verifies exact workflow-set and ownership closure. The second derives event, permission, and action-reference topology. The third proves both refusal laws. The fourth proves that the existing Rust integration boundary can execute the complete G0 evidence system and inspect its emitted consequences.

## G0 checkpoint report

| Law state | Result |
|---|---|
| Parse current workflow files | `PARTIAL_ALIVE` |
| Route to one semantic inventory | `PARTIAL_ALIVE` |
| Admit exact 48-file closure | executable verifier |
| Derive trigger and permission topology | executable analyzer |
| Inventory mutable action references | diagnostic evidence |
| Refuse omitted workflow | `CI-G0-INVENTORY-001` |
| Refuse dual output ownership | `CI-G0-OWNERSHIP-001` |
| Actuate workflow changes | excluded |
| External standing | `UNKNOWN` until G3 |
| Release/deployment change | excluded |

G1 may begin only after this exact inventory remains green on the pull-request head and any current-tree drift is either admitted into the manifest or refused.
