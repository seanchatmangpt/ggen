# gall-core-pack

`gall-core-pack` is the constitutional ggen-first Gall checkpoint framework.
A consumer declares its program, capability graph, dependency-closed checkpoints,
APS-grade implementation work, evidence obligations, exclusions, and optional
release crown as RDF. The pack manufactures the roadmap, checkpoint and work-item
dependency graphs, Jira work package, coding-agent instructions, checkpoint runner,
evidence-derived standing ledger, and crown report. SPARQL gates make incomplete,
ambiguous, cyclic, or fraudulent systems unrepresentable at sync time.

## What the pack ships

| Piece | File | Role |
|---|---|---|
| Gall vocabulary | `ontology.ttl` | Programs, capabilities, checkpoints, APS work items, obligations, evidence, exclusions, standings, archetypes, and crowns |
| Constitutional gates | `gates/*.rq` | Contract completeness, cardinality, DAG closure, unique IDs, APS lifecycle values, Jira safety, no manual standing, safe paths and commands, crown coverage, green evidence, exact revision, clean replay, freshness, and exclusion consistency |
| Roadmap | `templates/checkpoint_roadmap.md.tmpl` | Generates `docs/GALL_CHECKPOINT_ROADMAP.md` from the graph |
| Checkpoint DAG | `templates/checkpoint_dag.dot.tmpl` | Generates `docs/GALL_CHECKPOINT_DAG.dot` |
| Work-item DAG | `templates/work_item_dag.dot.tmpl` | Generates `docs/GALL_WORK_ITEM_DAG.dot` in executable proof order |
| Jira import surface | `templates/jira_work_items.csv.tmpl` | Generates `jira/GALL_JIRA_WORK_ITEMS.csv` with one deterministic row per work item |
| Jira catalog | `templates/jira_ticket_catalog.md.tmpl` | Generates complete human-readable ticket bodies at `docs/GALL_JIRA_TICKET_CATALOG.md` |
| Coding-agent work orders | `templates/agent_work_orders.md.tmpl` | Generates normative `MUST`, `MUST NOT`, path, verification, evidence, review, and stop-condition instructions at `docs/GALL_AGENT_WORK_ORDERS.md` |
| Standing ledger | `templates/status_ledger.md.tmpl` | Derives `UNKNOWN`, `PARTIAL_ALIVE`, or `ALIVE` from admitted evidence |
| Crown report | `templates/crown_report.md.tmpl` | Renders release closure only after crown gates pass |
| Evidence runner | `templates/run_checkpoints_sh.tmpl` | Generates `scripts/gall/run-checkpoints.sh`; runs real commands and clean-worktree replay, then emits `evidence/gall/ontology.ttl` |

## APS as ticket law

The Agile Protocol Specification is used here as a source of work-package
semantics, not as a second implementation repository. Gall mechanizes these APS
principles:

- mutable and immutable lifecycle states become controlled `gall:ProtocolState`
  values;
- metadata richness becomes required ticket identity, release, checkpoint,
  component, role, priority, and ordering fields;
- context-driven development becomes `gall:requiredContext`;
- governance becomes explicit assignee, reviewer, and approval-gate authority;
- transparency becomes mandatory objective and rationale;
- adversarial agile review becomes a required falsification question;
- auditability becomes verification commands and repository evidence paths;
- automation becomes generated Jira, agent, dependency, and receipt surfaces.

The pack does not copy unfinished APS prose into tickets. It converts the stable
principles into executable graph constraints.

## The checkpoint contract

Every `gall:Checkpoint` must declare:

- one useful `gall:Capability`;
- one runner command;
- exactly one positive witness;
- exactly one negative falsifier;
- exactly one receipt verifier;
- exactly one replay command;
- every proof dependency;
- at least one implementation `gall:WorkItem`.

The generated runner records real exit codes. A negative falsifier command must
exit `0` only when the system correctly detects or refuses the deliberately bad
condition. The runner does not judge evidence and always exits `0`; pack gates
judge the emitted graph facts on the next sync.

## The work-item contract

Every `gall:WorkItem` belongs to exactly one program and checkpoint. It must
provide enough information for a coding agent to act without inventing scope:

- stable `gall:workItemId`;
- controlled Jira issue type, priority, and APS lifecycle state;
- deterministic implementation order and dependencies;
- summary, objective, and rationale;
- component, assignee role, reviewer role, and approval gate;
- required context files;
- allowed and forbidden write paths;
- one or more `MUST` rules;
- one or more `MUST NOT` rules;
- explicit out-of-scope behavior;
- acceptance criteria and definition of done;
- executable verification commands;
- repository evidence artifacts;
- adversarial review questions.

Ticket dependencies must be acyclic. A prerequisite must have a lower
`gall:implementationOrder`. Cross-checkpoint ticket dependencies are legal only
when the owning checkpoint depends on the prerequisite checkpoint.

## Generated Jira package

`jira/GALL_JIRA_WORK_ITEMS.csv` is a deterministic Jira mapping surface. It
contains project key, issue type, summary, full description, priority, labels,
component, external work-item identity, and dependency identities. Jira
installations differ in custom-field and link mappings, so import mapping remains
an external deployment action; the pack never performs an unreceipted network
write.

`docs/GALL_JIRA_TICKET_CATALOG.md` provides the same tickets in reviewable form.
`docs/GALL_AGENT_WORK_ORDERS.md` is the normative coding-agent execution surface.
Changing any ticket fact means changing the ontology and regenerating all three.

## Two operating modes

### Planning mode

Declare a `gall:GallProgram`, its checkpoints, and work items, but no
`gall:Crown` yet. `ggen sync run` validates the graph and generates the roadmap,
DAGs, Jira package, agent work orders, ledger, crown report, and executable
runner. Missing execution evidence is shown as `UNKNOWN`, not silently promoted.

### Crown mode

A declared `gall:Crown` activates hard release gates. Every program checkpoint
must be included, dependencies must be closed, and every included checkpoint
must have evidence showing:

- runner exit `0`;
- positive witness exit `0`;
- negative falsifier exit `0`;
- receipt verification exit `0`;
- detached clean-worktree replay exit `0`;
- a real git revision;
- no tracked source dirtiness;
- `gall:independentReplay true`;
- graph-hash freshness against the latest reflexive sync receipt.

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
git add .
git commit -m "seal Gall planning artifacts"
bash scripts/gall/run-checkpoints.sh
```

The runner creates an evidence mini-pack. Wire it unlocked because its content
changes on every real execution:

```toml
[packs]
gall-core-pack = { path = "../packs/gall-core-pack" }
gall-evidence = { path = "evidence/gall", lock = false }
```

Then add the crown to the ontology, commit the exact source revision, sync once,
rerun the evidence runner against that crown revision, and sync again:

```bash
ggen sync run
git add .
git commit -m "seal Gall crown inputs"
bash scripts/gall/run-checkpoints.sh
ggen sync run
```

Freshness follows the existing reflexive-receipt model: evidence is bound to the
latest completed sync's graph hash, so stale detection lags one sync. Running the
emitter immediately before the final crown sync closes that window.

## Minimal program shape

```turtle
@prefix gall: <http://seanchatmangpt.github.io/packs/gall-core#> .
@prefix ex:   <https://example.org/gall/> .

ex:program a gall:GallProgram ;
    gall:programId "EXAMPLE" ;
    gall:releaseIdentity "v1" ;
    gall:jiraProjectKey "EX" ;
    gall:hasCheckpoint ex:checkpoint-000 ;
    gall:hasWorkItem ex:work-item-001 .

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
    gall:mustNotDo "Do not replace execution with a simulated success" ;
    gall:outOfScope "Unrelated feature work" ;
    gall:acceptanceCriterion "The useful system executes successfully" ;
    gall:definitionOfDone "Witness falsifier receipt and replay are green" ;
    gall:verificationCommand "bash scripts/checks/verify.sh" ;
    gall:evidenceArtifact "receipts/EXAMPLE-GALL-000.json" ;
    gall:adversarialQuestion "Would the verifier fail if the useful behavior were removed" .
```

## Non-self-certification boundary

The pack deliberately uses two independent surfaces:

1. ggen generates the runners and planning artifacts and receives execution
   evidence as graph facts;
2. the runner invokes real shell commands, real git revision inspection, and a
   detached clean worktree.

The generated runner does not declare the checkpoint `ALIVE`. The SPARQL status
query and crown gates derive standing from the resulting evidence. A consumer
that writes `gall:declaredStanding` is refused by name.
