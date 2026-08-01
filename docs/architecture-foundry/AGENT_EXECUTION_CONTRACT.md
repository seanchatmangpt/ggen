# ggen-legacy Agent Execution Contract

## Scope

This contract governs agents extracting architecture knowledge from `ggen`, constructing the `ggen-legacy` foundry corpus, generalizing reusable primitives, composing solution packs, and manufacturing reference architectures.

Repository constitutions remain superior authority. This contract narrows the foundry mission and does not weaken any existing verification law.

## Operating law

Agents shall:

- read all repository constitutions before acting;
- bind work to exact source and destination heads;
- use isolated worktrees for orthogonal changes;
- preserve provenance before moving or deleting source;
- emit real receipts from actual bytes and execution;
- cross real process, filesystem, protocol, graph, and verifier boundaries;
- maintain a machine-readable obligation list;
- stop the line on contradictory evidence;
- classify standing from executable predicates;
- continue from machine-readable refusal output until success or a genuine hard block.

Agents shall not:

- equate task completion with checkpoint promotion;
- copy code without lineage and disposition;
- fabricate evidence, hashes, receipts, telemetry, or execution;
- use prose as a substitute for verifier evidence;
- edit generated projections as authority;
- weaken a predicate to force admission;
- silently unify conflicting schemas or contracts;
- delete source before destination admission and recovery proof;
- leave an observed capability without a disposition obligation;
- claim clean-room standing from a developer checkout;
- stop because CI is queued, execution is long, or context is large.

## Agent roles

### 1. Observer agent

Owns one or more non-overlapping observer classes.

Required output:

```text
observer report
candidate records
admitted capability records
exclusion records
orphan report
BLAKE3 receipt
```

Promotion predicate:

```text
observer_executed=true
source_range_recorded=true
orphan_count=0
report_schema_valid=true
receipt_valid=true
```

### 2. Disposition agent

Owns a bounded capability family and resolves `PRESERVED`, `SUBSUMED`, `REPLACED`, `ARCHIVED`, or `REFUSED` from evidence.

Required output:

```text
capability-to-owner mapping
disposition rationale
positive witness
negative falsifier
migration consequence
recovery path
equivalence obligation or report
receipt
```

### 3. Extraction agent

Moves or references admitted historical material into `ggen-legacy`.

Required output:

```text
source provenance
destination record
cross-repository lineage record
replacement owner
compatibility or migration adapter
recovery procedure
receipt
```

No source deletion is permitted in the same unverified act that creates the destination.

### 4. Primitive-generalization agent

Converts admitted historical material into reusable architecture primitives.

Required output:

```text
primitive contract
preconditions
outputs
composition law
refusal modes
positive witness
negative falsifier
verifier
economic and operational implications
source lineage
receipt
```

### 5. Pack-composition agent

Combines admitted primitives into a solution architecture pack.

Required output:

```text
pack authority
required and optional primitives
composition constraints
technology mappings
migration plan
operating model
security and compliance evidence
capacity and cost model
manufacturing entrypoint
replay entrypoint
receipt
```

### 6. Equivalence agent

Executes legacy-versus-replacement comparison through the appropriate generic adapter family.

Required output:

```text
capability id
legacy command or boundary
replacement command or boundary
input fixture provenance
observable surfaces compared
expected differences
actual differences
standing
report digest
receipt
```

### 7. Independent verifier agent

Must not own the implementation it verifies.

Required output:

```text
input heads
input digests
schema results
witness results
falsifier results
replay result
lineage result
standing
refusal codes
receipt
```

### 8. Integration agent

Owns central integration only.

Responsibilities:

- verify worktree isolation and file ownership;
- merge one bounded wave at a time;
- inspect every conflict semantically;
- run all affected gates after each merge;
- preserve both sides when conflicts reflect independent valid evolution;
- regenerate projections from admitted authority;
- issue a combined integration receipt.

## Work partitioning

Parallel work is allowed only when file ownership and semantic ownership do not overlap.

Each dispatched task must declare:

```text
task_id
role
source_head
destination_head
owned_paths
read_only_paths
capability_scope
required_inputs
expected_outputs
promotion_predicates
sabotage_cases
```

Agents may read shared ontology and manifests, but only the designated authority owner may modify them during a wave.

## Evidence ladder

Every promoted artifact requires the strongest applicable evidence:

1. syntax and schema validity;
2. structural and semantic validation;
3. real positive execution;
4. real negative falsification;
5. deterministic replay;
6. exact-head binding;
7. cross-repository lineage verification;
8. independent standing decision.

A lower rung cannot substitute for an applicable higher rung.

## Gall checkpoints

A Gall checkpoint is an executable proof boundary, not an approval meeting.

Each checkpoint record contains:

```text
checkpoint_id
scope
source_heads
predicates
executed_commands
reports
receipts
negative_controls
standing
unresolved_obligations
```

Allowed standing values:

- `UNKNOWN`
- `PARTIAL_ALIVE`
- `ALIVE`
- `BLOCKED`
- `BUILD_BROKEN`
- `UNSUPPORTED`

An agent may recommend promotion. Only the designated external verifier may assign final checkpoint standing.

## Hard-block definition

A hard block requires one of:

- inaccessible required source or dependency;
- unavailable credential required for a real boundary;
- irrecoverable historical evidence;
- legally prohibited transfer or processing;
- genuinely non-derivable business decision requiring accountable human authority.

The block report must identify the exact missing input, the failed predicate, attempted lawful alternatives, and the minimum human decision required.

## Commit and PR discipline

- Push coherent, bounded checkpoint commits.
- State exact head in every evidence-bearing PR update.
- Do not mix unrelated workstreams.
- Do not rewrite shared history after external evidence binds a head.
- Keep PR descriptions generated from current evidence rather than session memory.
- Never use administrative merge bypass to override an admission refusal.

## Terminal execution rule

Agents continue until either:

```text
standing=ALIVE
```

or a valid hard-block report is emitted.

Intermediate honest refusal is required evidence, but it is not permission to stop.
