# Process Mining Chicago TDD — Van der Aalst Expansion

**Doctrine**: > If the code says it worked but the event log cannot prove a lawful process happened, then it did not work.

## Hostile Assumptions

Assume ALL of the following may be false unless proven otherwise through event-log validation:

* The declared manufacturing pipeline is NOT the real runtime process
* Stages may be skipped or repeated without detection by code-level tests
* Receipts may be emitted outside lawful object lifecycles
* Proof gates may pass despite non-conforming execution
* Release may occur from invalid or incomplete histories
* The system may appear deterministic while logs reveal variant explosion
* Hidden loops, retries, or rework may occur silently
* Temporal violations may be masked by success responses

## Required Validation Work

### 1. Event Log Derivation

OTel traces are NOT proof. They are candidate event evidence that must be:
1. Extracted from runtime observability (Tempo/Jaeger)
2. Converted to OCEL object-centric event logs
3. Validated for required attributes (ocel:activity, ocel:timestamp, ocel:object-id, ocel:object-type)
4. Checked for temporal ordering and lifecycle consistency

### 2. Object-Centric Conformance

Not just case-level replay. Multiple object types must align:
* **artifact objects** — from seed through release
* **receipt objects** — BLAKE3 chain with lawful dependencies
* **proof gate objects** — validation results with causal predecessors
* **benchmark objects** — performance measurements on compiled artifacts
* **release objects** — only from complete, validated histories

Each object type must have:
* Unique object IDs per type
* Consistent object types across events
* Lawful lifecycle transitions (create → start → complete)
* Temporal ordering (no complete before create)

### 3. Process Discovery vs. Declared Model

Use pm4py to discover the ACTUAL process from event logs:
1. Apply multiple discovery algorithms (inductive, heuristics, alpha)
2. Compare discovered process against declared CodeManufactory model
3. Treat model-vs-log mismatch as a FIRST-CLASS DEFECT
4. Report fitness, precision, generalization, simplicity metrics

### 4. Conformance Checking

Replay observed logs against the intended manufacturing model:
* Can the model explain the observed log?
* Where are the deviations, alignments, skipped transitions?
* Are there hidden rework loops or shadow paths?
* Do certain tools/agents create recurring deviations?

### 5. Variant Explosion Detection

If CodeManufactory is supposed to be deterministic-ish:
* Count distinct execution variants in event logs
* Flag excessive variants as process instability
* Investigate why "deterministic" operators produce divergent traces

### 6. Temporal Lawfulness

Using OTel + OCEL, validate time semantics:
* Stages occur in lawful order (no release before validate)
* No impossible overlaps (concurrent terminal states for same artifact)
* Proof gates evaluated after underlying work completes
* No benchmarks on artifacts that were never compiled
* Receipt timestamps consistent with event ordering

### 7. Negative Testing Through Impossible Logs

Generate or inject IMPOSSIBLE event logs to test validation:
* release before validate
* validate before breed
* two concurrent terminal states for one artifact
* receipt emitted for non-existent artifact
* benchmark for object never compiled
* proof gate pass with missing causal predecessor
* orphan MCP tool invocation with no manufacturing case
* same artifact both failed and released without rework

Then REQUIRE pm4py/OCEL-based checks to catch them.

### 8. Object Lifecycle Soundness

Every object must have a lawful lifecycle:
* No orphan objects (created but never completed)
* No broken workflows (missing intermediate steps)
* No duplicate terminal states without rework
* No illegal state transitions

## Integration with Existing Code

Use existing modules:
* `src/ostar/process/mining.py` — trace → event log, discovery, conformance
* `src/ostar/process/invariant_validation.py` — 7 semantic laws + 5 OCEL constraints
* `ontology/queries/ocel/` — SPARQL queries for OCEL validation

## Entry Point: ProcessMiningChicagoTDDValidator

```python
from ostar.process.mining import (
    traces_to_event_log,
    discover_process_model,
    conformance_check,
    analyze_deviations,
    discover_bottlenecks,
    compare_discovery_algorithms,
)
from ostar.process.invariant_validation import (
    InvariantValidator,
    validate_invariants_from_jaeger,
)

# 1. Extract event log from OTel traces
event_log = traces_to_event_log(tempo_url="http://localhost:3200", hours=24)

# 2. Discover actual process from logs
discovered = discover_process_model(event_log, miner="inductive")

# 3. Check conformance against declared BPMN model
conformance = conformance_check(event_log, bpmn_file="path/to/model.bpmn")

# 4. Analyze deviations
deviations = analyze_deviations(event_log, bpmn_file="path/to/model.bpmn")

# 5. Validate OCEL invariants
validator = InvariantValidator(pipeline_id="manufacturing-test")
report = validator.validate_all(
    traces=jaeger_traces,
    receipts=receipts,
    interaction_matrix=interaction_matrix,
    start_time=start,
    end_time=end,
    ocel_events=ocel_events,
)

# 6. Compare multiple discovery algorithms (never trust just one)
comparison = compare_discovery_algorithms(event_log)
```

## Success Criteria

A manufacturing run is VALID only if ALL of the following pass:

1. **Event log completeness**: All required OCEL attributes present
2. **Object lifecycle soundness**: No orphans, no broken workflows
3. **Temporal lawfulness**: All events in lawful order
4. **Conformance fitness**: Discovered model matches declared model (fitness >= 0.9)
5. **Variant control**: Execution variants within expected bounds
6. **Deviation-free**: No skipped stages, hidden loops, or shadow paths
7. **Object-centric consistency**: Artifact, receipt, proof, benchmark, and release objects all align
8. **Semantic invariants**: All 7 semantic laws + 5 OCEL constraints pass

## Coverage Note

The 14-gate manufacturing proof framework covers 4 of 8 process mining success criteria:
- Gate 2 (observability-present) → criterion 1 (event log completeness)
- Gate 10 (process-conformance) → criterion 4 (conformance fitness)
- Gate 9 (cross-system-causality-proven) → criterion 7 (object-centric consistency)
- Gate 5 (owl-consistency) + Gate 11 (variant-explosion) → criterion 8 (semantic invariants)

Remaining criteria (2, 3, 5, 6) require OCEL-derived event logs from real manufacturing runs. Use `scripts/canonical_proof.py` for full process mining validation.

## Failure is Information

When process mining reveals deviations:
* The deviation is NOT a bug in the validator
* The deviation IS a defect in the process or its implementation
* "But the code returns success" is NOT a defense
* Event-log truth trumps architectural diagrams

## References

* Van der Aalst, W. (2016). Process Mining: Data Science in Action.
* OCEL 2.0 standard: https://www.ocel-standard.org/
* pm4py documentation: https://pm4py.fit.fau.de/
* CodeManufactory manufacturing pipeline: `src/ostar/manufacturing/__init__.py`
* Existing Chicago TDD findings: `CHICAGO_TDD_FINDINGS.md`
