# gall-core-pack

`gall-core-pack` is the constitutional ggen-first Gall checkpoint and execution
framework. A consumer declares its program, capability graph, dependency-closed
checkpoints, APS-grade work items, automation policy, evidence obligations,
exclusions, and optional release crown as RDF.

The ontology is the only authority. ggen manufactures the roadmap, dependency
graphs, Jira and GitHub tracker projections, coding-agent instructions,
operator CLI, scheduler, handoff bundles, tracker convergence, verification
state, receipts, replay, CI workflow, checkpoint evidence, standing ledger, and
crown report.

## What the pack ships

| Piece | Generated or authored surface | Role |
|---|---|---|
| Gall vocabulary | `ontology.ttl` | Programs, capabilities, checkpoints, APS work items, automation profiles, obligations, evidence, exclusions, standings, archetypes, and crowns |
| Constitutional gates | `gates/*.rq` | 33 fail-closed gates covering contracts, cardinality, DAGs, proof order, lifecycle values, automation policy, path safety, crowns, replay, freshness, and evidence |
| Roadmap | `docs/GALL_CHECKPOINT_ROADMAP.md` | Total checkpoint, ticket, and automation program |
| Checkpoint DAG | `docs/GALL_CHECKPOINT_DAG.dot` | Proof dependency graph |
| Work-item DAG | `docs/GALL_WORK_ITEM_DAG.dot` | Executable ticket dependency graph |
| Jira import | `jira/GALL_JIRA_WORK_ITEMS.csv` | Deterministic ticket import surface |
| Jira catalog | `docs/GALL_JIRA_TICKET_CATALOG.md` | Human-reviewable ticket bodies |
| Agent work orders | `docs/GALL_AGENT_WORK_ORDERS.md` | Normative `MUST`, `MUST NOT`, path, verification, evidence, and stop-condition contracts |
| Automation manifest | `automation/GALL_AUTOMATION_WORK_ITEMS.csv` | Complete machine input for scheduling and actuation |
| Receipt schema | `automation/schemas/gall-automation-receipt.schema.json` | Receipt envelope contract |
| Operator CLI | `scripts/gall/gall` | Noun-verb entry point for the complete lifecycle |
| Control plane | `scripts/gall/control_plane.py` | DAG validation, readiness, handoff, agent invocation, verification, completion, replay, and crown |
| Tracker synchronizer | `scripts/gall/tracker_sync.py` | Idempotent Jira Cloud, GitHub Issues, or file-tracker convergence |
| Receipt verifier | `scripts/gall/verify_automation_receipts.py` | Independent digest-chain and manifest-freshness verification |
| Checkpoint runner | `scripts/gall/run-checkpoints.sh` | Real actuation, witness, falsifier, receipt verification, and detached-worktree replay |
| Automation workflow | `.github/workflows/gall-control-plane.yml` | Pull-request planning plus protected manual advancement, tracker, agent, and crown jobs |
| Automation runbook | `docs/GALL_AUTOMATION_RUNBOOK.md` | Generated operator and secret configuration guide |
| Standing ledger | `docs/GALL_STATUS_LEDGER.md` | Derived `UNKNOWN`, `PARTIAL_ALIVE`, or `ALIVE` |
| Crown report | `docs/GALL_CROWN_REPORT.md` | Release closure after hard gates pass |

## APS as executable work-package law

The Agile Protocol Specification informs the work-package semantics. It is not a
runtime dependency and not a second implementation repository.

Gall mechanizes its stable principles:

- lifecycle states become controlled `gall:ProtocolState` values;
- metadata richness becomes mandatory identity, release, checkpoint, component,
  role, priority, and order fields;
- context-driven development becomes `gall:requiredContext`;
- governance becomes explicit assignee, reviewer, and approval authority;
- transparency becomes mandatory objective and rationale;
- adversarial review becomes a required falsification question;
- auditability becomes commands, evidence paths, and receipts;
- automation becomes generated tracker, agent, scheduler, replay, and CI
  surfaces.

The pack does not copy unfinished APS prose into tickets. It converts the stable
principles into graph constraints and generated consequences.

## Checkpoint contract

Every `gall:Checkpoint` must declare:

- one useful capability;
- one real runner command;
- exactly one positive witness;
- exactly one negative falsifier;
- exactly one receipt verifier;
- exactly one clean-replay command;
- every proof dependency;
- at least one implementation work item.

The generated checkpoint runner records reality. It does not promote itself.
SPARQL gates derive standing from admitted evidence on the next sync.

## Work-item contract

Every `gall:WorkItem` belongs to exactly one program and checkpoint. It must
contain enough information for a coding agent to execute without inventing
scope:

- stable identity and implementation order;
- controlled issue type, priority, and APS lifecycle state;
- objective and rationale;
- component, assignee role, reviewer role, and approval gate;
- dependencies and required context;
- allowed and forbidden write paths;
- one or more `MUST` rules;
- one or more `MUST NOT` rules;
- explicit out-of-scope behavior;
- acceptance criteria and definition of done;
- executable verification commands;
- repository evidence artifacts;
- adversarial questions.

Dependencies must be acyclic. Every prerequisite must have a lower
`gall:implementationOrder`. A cross-checkpoint dependency is legal only when the
checkpoint graph contains the corresponding proof dependency.

## Automation-profile contract

Every program with work items owns exactly one `gall:AutomationProfile`:

```turtle
ex:automation-profile a gall:AutomationProfile ;
    gall:automationProfileId "EX-AUTOMATION" ;
    gall:trackerProvider gall:FileTracker ;
    gall:executionMode gall:PlanOnly ;
    gall:agentMode gall:HandoffOnly ;
    gall:maxParallelism 4 ;
    gall:branchPattern "agent/{workItemId}" ;
    gall:runtimeDirectory ".gall" ;
    gall:receiptDirectory "receipts/gall" .
```

Allowed providers:

- `gall:JiraCloud`
- `gall:GitHubIssues`
- `gall:FileTracker`

Allowed actuation modes:

- `gall:PlanOnly`
- `gall:ApplyAllowed`

Allowed agent modes:

- `gall:HandoffOnly`
- `gall:CommandAgent`

Parallelism is bounded from 1 through 64. Branch patterns must include
`{workItemId}`. Runtime and receipt directories must remain project-relative and
outside `.git`.

## Zero-unreceipted actuation

Every external mutation follows:

```text
admitted graph
→ deterministic plan
→ intent receipt
→ explicit apply boundary
→ external response
→ result receipt
→ independent receipt verification
```

Plan mode is credential-free. Network or agent actuation requires all of:

1. policy admits `gall:ApplyAllowed`;
2. the operator explicitly requests apply;
3. required environment credentials exist;
4. an intent receipt is durable before execution;
5. the result is recorded afterward.

Automation receipts form a SHA-256 predecessor chain bound to the exact generated
automation manifest and Git revision. The independent verifier recomputes every
digest and rejects stale or detached receipt files.

## Operator CLI

```bash
bash scripts/gall/gall automation plan
bash scripts/gall/gall automation validate
bash scripts/gall/gall work status
bash scripts/gall/gall work next
bash scripts/gall/gall work dispatch WORK_ITEM_ID
bash scripts/gall/gall work dispatch-ready
bash scripts/gall/gall work verify WORK_ITEM_ID
bash scripts/gall/gall work complete WORK_ITEM_ID
bash scripts/gall/gall work advance
bash scripts/gall/gall tracker plan
bash scripts/gall/gall tracker apply
bash scripts/gall/gall checkpoint run
bash scripts/gall/gall receipt verify
bash scripts/gall/gall replay
bash scripts/gall/gall crown
```

`work next` returns only dependency-ready work, bounded by the declared maximum
parallelism. `work advance` executes verification for all currently ready items
and emits completion receipts only for green work.

## Coding-agent handoff and invocation

A safe handoff bundle is always available:

```bash
bash scripts/gall/gall work dispatch EX-GALL-001
```

It contains a work order, exact branch, full ticket row, dependencies, paths,
verification, and a bundle digest.

External invocation additionally requires `gall:CommandAgent`, apply permission,
and:

```text
GALL_AGENT_COMMAND=/path/to/agent --fixed-arguments
```

Then:

```bash
bash scripts/gall/gall work dispatch EX-GALL-001 --apply
```

The work-order path is appended as the final argument. Standard output, standard
error, intent, and result receipts remain in the generated runtime and receipt
directories.

## Tracker convergence

### File tracker

`gall:FileTracker` materializes one current Markdown work order per ticket under
the runtime directory. It is used by the real Chicago-TDD lifecycle without
network access.

### Jira Cloud

Required environment:

```text
JIRA_BASE_URL=https://your-domain.atlassian.net
JIRA_EMAIL=operator@example.com
JIRA_API_TOKEN=secret
```

Optional environment:

```text
JIRA_DEPENDENCY_LINK_TYPE=Blocks
GALL_JIRA_USE_COMPONENTS=1
GALL_JIRA_STATUS_MAP={"blocked":"Blocked","ready":"To Do","complete":"Done"}
```

The synchronizer searches by stable Gall identity, creates or updates issues,
inspects links before adding dependencies, and optionally converges lifecycle
through deployment-specific transition names.

### GitHub Issues

Required environment:

```text
GITHUB_TOKEN=secret
GITHUB_REPOSITORY=owner/repository
```

The synchronizer creates deterministic labels before issue upsert, identifies
each issue through its stable Gall label, updates the full work order, and closes
the issue only after a Gall completion receipt exists.

## Generated GitHub Actions workflow

Pull requests execute plan, source validation, ready-queue calculation, replay,
and receipt verification. `workflow_dispatch` can independently request:

- verification and completion of ready work;
- external tracker application;
- one coding-agent handoff or invocation;
- automation crown verification.

Tracker and agent jobs use the `gall-external-actuation` environment so repository
approval rules can protect credentials and execution.

## Planning and crown modes

### Planning mode

Declare the program, checkpoints, work items, and automation profile without a
crown. `ggen sync run` validates the graph and manufactures all planning,
execution, and automation surfaces. Missing execution evidence remains
`UNKNOWN`.

### Crown mode

A declared crown activates hard release gates. Every checkpoint must be included
and dependency-closed. Every checkpoint needs green exact-revision evidence,
and every work item needs a green completion receipt. The automation crown also
requires the checkpoint ledger to derive `ALIVE` and the receipt chain to verify
against the current manifest.

## Consumer wiring

```toml
[project]
name = "my-gall-program"

[ontology]
source = "ontology.ttl"

[packs]
gall-core-pack = { path = "../packs/gall-core-pack" }

[templates]
dir = "templates"

[law]
reflexive = true
```

After the first planning sync:

```bash
ggen sync run
git add .
git commit -m "seal Gall planning artifacts"
bash scripts/gall/gall automation validate
bash scripts/gall/gall work dispatch-ready
```

The checkpoint runner creates an evidence mini-pack. Wire it unlocked:

```toml
[packs]
gall-core-pack = { path = "../packs/gall-core-pack" }
gall-evidence = { path = "evidence/gall", lock = false }
```

Then activate the crown, seal the exact revision, rerun evidence, sync, and check
closure:

```bash
ggen sync run
git add .
git commit -m "seal Gall crown inputs"
bash scripts/gall/gall checkpoint run
ggen sync run
bash scripts/gall/gall crown
```

## Minimal program shape

```turtle
@prefix gall: <http://seanchatmangpt.github.io/packs/gall-core#> .
@prefix ex:   <https://example.org/gall/> .

ex:program a gall:GallProgram ;
    gall:programId "EXAMPLE" ;
    gall:releaseIdentity "v1" ;
    gall:jiraProjectKey "EX" ;
    gall:hasCheckpoint ex:checkpoint-000 ;
    gall:hasWorkItem ex:work-item-001 ;
    gall:hasAutomationProfile ex:automation-profile .

ex:automation-profile a gall:AutomationProfile ;
    gall:automationProfileId "EX-AUTOMATION" ;
    gall:trackerProvider gall:FileTracker ;
    gall:executionMode gall:PlanOnly ;
    gall:agentMode gall:HandoffOnly ;
    gall:maxParallelism 4 ;
    gall:branchPattern "agent/{workItemId}" ;
    gall:runtimeDirectory ".gall" ;
    gall:receiptDirectory "receipts/gall" .

ex:capability a gall:Capability ;
    gall:capabilityId "useful-system" ;
    gall:title "Useful executable system" .

ex:checkpoint-000 a gall:Checkpoint, gall:RequiredCheckpoint ;
    gall:checkpointId "EXAMPLE-GALL-000" ;
    gall:title "Executable floor" ;
    gall:producesCapability ex:capability ;
    gall:runnerCommand "bash scripts/checks/run.sh" ;
    gall:positiveWitness ex:witness ;
    gall:negativeFalsifier ex:falsifier ;
    gall:receiptObligation ex:receipt ;
    gall:replayObligation ex:replay ;
    gall:hasWorkItem ex:work-item-001 .

ex:witness a gall:PositiveWitness ;
    gall:name "useful-system-executes" ;
    gall:command "bash scripts/checks/witness.sh" .

ex:falsifier a gall:NegativeFalsifier ;
    gall:name "broken-system-is-refused" ;
    gall:command "bash scripts/checks/falsifier.sh" .

ex:receipt a gall:ReceiptObligation ;
    gall:name "receipt-chain-verifies" ;
    gall:command "ggen receipt verify" .

ex:replay a gall:ReplayObligation ;
    gall:name "clean-revision-replays" ;
    gall:command "bash scripts/checks/replay.sh" .

ex:work-item-001 a gall:WorkItem ;
    gall:workItemId "EX-GALL-001" ;
    gall:issueType gall:Task ;
    gall:summary "Build the executable floor" ;
    gall:objective "Create the first useful boundary-crossing system" ;
    gall:rationale "Later checkpoints need real executable evidence" ;
    gall:belongsToProgram ex:program ;
    gall:belongsToCheckpoint ex:checkpoint-000 ;
    gall:implementationOrder 10 ;
    gall:priority gall:Highest ;
    gall:component "runtime" ;
    gall:assigneeRole "Implementation agent" ;
    gall:reviewerRole "Evidence reviewer" ;
    gall:approvalGate "All checkpoint evidence is green" ;
    gall:protocolState gall:Draft ;
    gall:requiredContext "docs/architecture.md" ;
    gall:allowedPath "src/" ;
    gall:forbiddenPath "vendor/" ;
    gall:mustDo "Cross the real execution boundary" ;
    gall:mustNotDo "Do not replace execution with simulated success" ;
    gall:outOfScope "Unrelated feature work" ;
    gall:acceptanceCriterion "The useful system executes successfully" ;
    gall:definitionOfDone "Witness falsifier receipt and replay are green" ;
    gall:verificationCommand "bash scripts/checks/verify.sh" ;
    gall:evidenceArtifact "receipts/EXAMPLE-GALL-000.json" ;
    gall:adversarialQuestion "Would the verifier fail if useful behavior were removed" .
```

## Non-self-certification boundary

The pack deliberately separates:

1. ggen generation and graph admission;
2. real shell, filesystem, Git, tracker, and optional agent boundaries;
3. observed receipts;
4. independent receipt and crown verification.

Neither the generated runner nor the control plane may directly assert `ALIVE`.
A consumer that authors `gall:declaredStanding` is refused by name.
