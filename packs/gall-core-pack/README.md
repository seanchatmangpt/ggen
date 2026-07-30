# gall-core-pack

`gall-core-pack` is the constitutional ggen-first Gall checkpoint framework.
A consumer declares its program, capability graph, dependency-closed checkpoints,
evidence obligations, exclusions, and optional release crown as RDF. The pack
manufactures the roadmap, dependency graph, checkpoint runner, evidence-derived
standing ledger, and crown report. SPARQL gates make incomplete or fraudulent
checkpoint systems unrepresentable at sync time.

## What the pack ships

| Piece | File | Role |
|---|---|---|
| Gall vocabulary | `ontology.ttl` | Programs, capabilities, checkpoints, obligations, evidence, exclusions, standings, archetypes, and crowns |
| Constitutional gates | `gates/*.rq` | Contract completeness, one-obligation cardinality, DAG closure, unique IDs, no manual standing, safe paths/commands, crown coverage, green evidence, exact revision, clean replay, freshness, and exclusion consistency |
| Roadmap | `templates/checkpoint_roadmap.md.tmpl` | Generates `docs/GALL_CHECKPOINT_ROADMAP.md` from the graph |
| Dependency DAG | `templates/checkpoint_dag.dot.tmpl` | Generates `docs/GALL_CHECKPOINT_DAG.dot` |
| Standing ledger | `templates/status_ledger.md.tmpl` | Derives `UNKNOWN`, `PARTIAL_ALIVE`, or `ALIVE` from admitted evidence |
| Crown report | `templates/crown_report.md.tmpl` | Renders release closure only after crown gates pass |
| Evidence runner | `templates/run_checkpoints_sh.tmpl` | Generates `scripts/gall/run-checkpoints.sh`; runs real commands and clean-worktree replay, then emits `evidence/gall/ontology.ttl` |

## The checkpoint contract

Every `gall:Checkpoint` must declare:

- one useful `gall:Capability`;
- one runner command;
- exactly one positive witness;
- exactly one negative falsifier;
- exactly one receipt verifier;
- exactly one replay command;
- every proof dependency.

The generated runner records real exit codes. A negative falsifier command must
exit `0` only when the system correctly detects or refuses the deliberately bad
condition. The runner does not judge evidence and always exits `0`; pack gates
judge the emitted graph facts on the next sync.

## Two operating modes

### Planning mode

Declare a `gall:GallProgram` and its checkpoints, but no `gall:Crown` yet.
`ggen sync run` validates the checkpoint contracts and generates the roadmap,
DAG, ledger, crown report, and executable runner. Missing execution evidence is
shown as `UNKNOWN`, not silently promoted.

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

## Minimal ontology shape

```turtle
@prefix gall: <http://seanchatmangpt.github.io/packs/gall-core#> .
@prefix ex:   <https://example.org/gall/> .

ex:program a gall:GallProgram ;
    gall:programId "EXAMPLE" ;
    gall:releaseIdentity "v1" ;
    gall:hasCheckpoint ex:checkpoint-000 .

ex:capability a gall:Capability ;
    gall:capabilityId "useful-system" ;
    gall:title "Useful executable system" .

ex:checkpoint-000 a gall:RequiredCheckpoint ;
    gall:checkpointId "EXAMPLE-GALL-000" ;
    gall:title "Executable floor" ;
    gall:producesCapability ex:capability ;
    gall:runnerCommand "bash scripts/checks/run.sh" ;
    gall:positiveWitness ex:witness ;
    gall:negativeFalsifier ex:falsifier ;
    gall:receiptObligation ex:receipt ;
    gall:replayObligation ex:replay .

ex:witness a gall:PositiveWitness ;
    gall:name "useful-output" ;
    gall:command "bash scripts/checks/witness.sh" .

ex:falsifier a gall:NegativeFalsifier ;
    gall:name "broken-output-refused" ;
    gall:command "bash scripts/checks/falsifier.sh" .

ex:receipt a gall:ReceiptObligation ;
    gall:name "receipt-valid" ;
    gall:command "ggen receipt verify" .

ex:replay a gall:ReplayObligation ;
    gall:name "clean-replay" ;
    gall:command "bash scripts/checks/replay.sh" .
```

## Non-self-certification boundary

The pack deliberately uses two independent surfaces:

1. ggen generates the runner and receives its evidence as graph facts;
2. the runner invokes real shell commands, real git revision inspection, and a
   detached clean worktree.

The generated runner does not declare the checkpoint `ALIVE`. The SPARQL status
query and crown gates derive standing from the resulting evidence. A consumer
that writes `gall:declaredStanding` is refused by name.
