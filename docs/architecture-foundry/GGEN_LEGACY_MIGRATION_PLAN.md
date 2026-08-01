# ggen to ggen-legacy Extraction and Reconstitution Plan

## Mission

Separate the reusable repository-manufacturing kernel from historically valuable solution material without destroying provenance, behavior, or future reuse.

The program must leave:

- `ggen` as a minimal, independently verified manufacturing kernel;
- `ggen-legacy` as an executable enterprise architecture foundry corpus;
- a complete cross-repository lineage and equivalence portfolio;
- a repeatable path for manufacturing future Fortune-scale solution architectures.

## Non-goals

This program does not:

- dump removed files into an archive;
- preserve every implementation as active code;
- treat documentation as proof of migration;
- use file movement as evidence of semantic ownership;
- permit copied code without provenance and disposition;
- certify either repository from hand-authored status fields;
- close work because an agent finished successfully.

## Workstream A — Exact-head baseline

1. Resolve the exact heads of `ggen`, PR #540, `ggen-legacy`, and all dependent branches.
2. Record dirty-state, untracked files, submodules, generated surfaces, and toolchain closure.
3. Produce a signed baseline manifest for both repositories.
4. Refuse execution when a reported head differs from the external Git head.
5. Bind all later reports to these exact heads.

### Gate A

```text
source_heads_resolved=true
working_trees_clean=true
baseline_manifests_valid=true
external_head_agreement=true
```

## Workstream B — Exhaustive observation

Run all observer classes across source, history, releases, workflows, and deleted surfaces.

Minimum classes:

- CLI declarations, aliases, default verbs, exit codes, and diagnostics;
- configuration structs, serialized field names, environment variables, and schema versions;
- public functions, traits, modules, crates, and removed owners;
- generated surfaces, templates, frontmatter, and output modes;
- filesystem writes and mutation boundaries;
- receipts, schemas, verification reports, and replay formats;
- workflow commands, `just` recipes, release scripts, and binary names;
- LSP commands, capabilities, diagnostics, and protocol behavior;
- pack, marketplace, and dependency-resolution formats;
- tags, released binaries, migration commits, and removal commits;
- security, economics, operations, governance, and organizational constraints.

Every observer class emits:

```text
observer_class
source_range
files_examined
commits_examined
candidates_observed
capabilities_admitted
duplicates_collapsed
candidates_excluded
exclusion_reasons
orphans
receipt
```

### Gate B

```text
observer_classes_unattempted=0
orphan_candidates=0
inventory_complete=true
```

A completed observer that finds zero candidates is valid. An observer that did not run is not equivalent to zero findings.

## Workstream C — Capability admission and disposition

1. Deduplicate candidates by semantic identity rather than filename.
2. Admit each capability with source commit, source path, symbol, observer class, content digest, and observable contract.
3. Assign subsystem ownership.
4. Assign one provisional disposition.
5. Refuse contradictory ownership or silent schema unification.
6. Require explicit ambiguity handling for dual schemas and overlapping command families.

### Gate C

```text
unknown_capabilities=0
capabilities_without_provenance=0
contradictory_owners=0
```

Final dispositions may remain unresolved until equivalence evidence exists, but every unresolved disposition must have a typed blocking obligation.

## Workstream D — Kernel versus corpus classification

Evaluate every admitted component against the repository ownership predicates in `REPOSITORY_ROLES.md`.

For each component, emit:

```text
component_id
current_repository
current_owner
candidate_repository
classification_rationale
kernel_generality_witness
corpus_value_witness
migration_dependencies
migration_order
recovery_path
```

No component moves solely because it is old. No component remains solely because other code depends on it.

### Gate D

```text
unclassified_components=0
classification_conflicts=0
migration_dependencies_closed=true
```

## Workstream E — Cross-repository extraction

For every component moving to `ggen-legacy`:

1. Capture immutable source provenance.
2. Preserve lawful history through Git-native transfer, immutable source reference, or content-addressed snapshot.
3. Create the destination corpus record.
4. Generate the cross-repository lineage receipt.
5. Create or update the replacement owner in `ggen`.
6. Add compatibility or migration adapters where the contract crosses repositories.
7. Add positive, negative, and replay evidence.
8. Remove the source implementation only after destination admission and replacement verification.

### Gate E

```text
moved_components_without_lineage=0
moved_components_without_destination=0
moved_components_without_recovery=0
source_removed_before_destination_admission=0
```

## Workstream F — Primitive generalization

Historical implementation is not yet a reusable architecture primitive.

For each candidate primitive:

1. identify the invariant architectural intent;
2. separate organization-specific detail from reusable constraint;
3. define inputs, outputs, preconditions, refusals, and composition law;
4. bind the primitive to source witnesses and failure evidence;
5. define economic, operational, governance, data, integration, and security implications;
6. create a generic verifier;
7. prove the primitive against at least one positive witness and one negative falsifier;
8. record limitations and non-applicability conditions.

### Gate F

```text
primitives_without_contract=0
primitives_without_witness=0
primitives_without_falsifier=0
primitives_without_verifier=0
```

## Workstream G — bblocks and solution packs

Compose admitted primitives into reusable manufacturing products.

A bblock must define:

- semantic inputs;
- generated surfaces;
- runtime boundaries;
- verification suites;
- receipts;
- replay obligations;
- compatibility constraints;
- typed refusal modes.

A solution pack must define:

- business and architecture objectives;
- required and optional bblocks;
- composition constraints;
- organizational assumptions;
- technology mappings;
- migration patterns;
- operating model;
- cost and capacity model;
- security and compliance evidence;
- clean-room manufacturing entrypoint.

Initial reference packs should include:

- repository manufacturing platform;
- enterprise developer platform;
- governed data and lakehouse platform;
- event and integration platform;
- identity and policy enforcement platform;
- AI and agent execution platform;
- process evidence and observability platform;
- regulated release and software supply-chain platform.

### Gate G

```text
packs_without_admitted_primitives=0
packs_without_verifiers=0
packs_without_replay=0
packs_without_operating_model=0
```

## Workstream H — Full equivalence closure

Every legacy capability must have an executable case through an adapter family or an evidence-supported `ARCHIVED` or `REFUSED` case.

Compare all relevant observable surfaces:

- exit code;
- stdout and stderr;
- generated bytes;
- filesystem mutations;
- diagnostic codes;
- protocol messages;
- event ordering;
- receipt structure;
- replay output;
- refusal and recovery behavior.

### Gate H

```text
case_count=capability_count
missing_equivalence_cases=0
equivalence_failures=0
unexplained_differences=0
unknown_dispositions=0
```

## Workstream I — Independent cross-repository verification

External verifiers must validate:

- schemas and ontology closure;
- exact-head agreement;
- source and destination content digests;
- lineage receipts;
- positive witnesses;
- negative falsifiers;
- replay reports;
- dispositions;
- generated projections;
- cross-repository references;
- customer-manufacture lineage.

Neither repository may assign its own standing from a generated matrix or hand-authored declaration.

### Gate I

```text
unassigned_verifiers=0
unknown_standings=0
invalid_receipts=0
stale_heads=0
self_certification_paths=0
```

## Workstream J — Clean-room manufacture and replay

From fresh isolated clones at exact heads:

1. build `ggen`;
2. validate the `ggen-legacy` corpus;
3. manufacture the reference repository architecture from the corpus;
4. verify all generated and runtime surfaces;
5. execute the negative sabotage portfolio;
6. issue receipts;
7. repeat the manufacture;
8. compare semantic, generated, evidence, and receipt trees.

### Gate J

```text
NO_SEMANTIC_CHANGE=true
NO_GENERATED_DRIFT=true
REPLAY_MATCH=true
CROSS_REPOSITORY_LINEAGE_MATCH=true
```

## Workstream K — Fortune-scale reference reconstitution

The bootstrap is incomplete until the foundry corpus manufactures at least one Fortune-scale reference solution architecture that is not merely the original ggen repository.

The reference must exercise:

- multiple organizational domains;
- identity and segregation of duties;
- regional or regulatory constraints;
- data governance;
- integration and eventing;
- observability and process evidence;
- disaster recovery;
- cost and capacity models;
- software supply-chain controls;
- migration from an incumbent estate;
- independent standing and replay.

This reference may be synthetic as an organization model, but its execution paths, receipts, generated artifacts, and verification boundaries must be real.

### Gate K

```text
fortune_scale_reference_manufactured=true
reference_verifiers_pass=true
reference_replay_match=true
reference_solution_admitted=true
```

## Terminal theorem

The entire program reaches `ALIVE` only when:

```text
ggen_kernel_admitted=true
ggen_legacy_corpus_admitted=true
unknown_capabilities=0
unknown_dispositions=0
unknown_standings=0
unassigned_verifiers=0
missing_equivalence_cases=0
equivalence_failures=0
replay_differences=0
cross_repository_receipts_valid=true
fortune_scale_reference_manufactured=true
solution_admission=true
standing=ALIVE
```

Runner congestion, elapsed time, task count, context size, and successful agent return are not terminal conditions.
