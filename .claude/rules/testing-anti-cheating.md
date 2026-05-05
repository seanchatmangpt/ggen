# Testing: Anti-Cheating Mechanism

**Doctrine:** The easiest path to passing tests must be real execution. Faking must be harder than the real thing.

## Core Constraint

Do NOT write tests that:
- Can pass by assertion alone (without evidence)
- Accept mocked evidence (traces, spans, state)
- Fabricate OCEL events, spans, or fixtures
- Validate internal state without external proof
- Assume causality without tracing it
- Use test naming or labeling as evidence

Instead, write tests that:
- Require **real boundary-crossing** (LLM, MCP, observability, process)
- Produce **externalizable evidence** (OTel, files, process logs)
- Prove **causality** (X span caused Y transition)
- Are **unforgeable** (faking is harder than real execution)
- Corroborate across **multiple surfaces** (execution + telemetry + state + process)

## Specific Rules

### 1. No Mocking Observability

**Forbidden:**
```python
@patch("ostar.utils.observability_v2.get_tracer")
def test_something(mock_tracer):
    # This test passes whether or not the code actually traces anything
```

**Why:** The test doesn't prove tracing happened. It only proves the mock was called.

**Required:**
- Use real Tempo/Jaeger
- Validate spans actually exist
- Check span attributes are correct
- Use OCEL derived from real traces

### 2. No Fabricated Fixtures

**Forbidden:**
```python
def _make_spans(n: int):
    """Fake span dicts for testing"""
    return [{"trace_id": "fake", ...} for _ in range(n)]
```

**Why:** Fabricated spans have no causal relationship to actual execution.

**Required:**
- Use `TempoClient.fetch_traces()` to get real spans
- Convert real spans to OCEL via `traces_to_ocel()`
- Never construct event logs by hand

### 3. Evidence Must Be Externalizable

**Forbidden:**
```python
assert my_function_called  # only visible inside the test
```

**Required:**
```python
# Evidence is in OTel, files, or DB
assert "function_name" in tempo_client.get_traces()  # externalizable
assert Path("output.json").exists()  # externalizable
assert db.query("SELECT * FROM log").count() > 0  # externalizable
```

### 4. Causality, Not Assertions

**Forbidden:**
```python
result = await operator.execute(artifact)
assert result.state == ArtifactState.BRED  # what if we just set it to BRED?
```

**Required:**
```python
# Prove causality through traces
result = await operator.execute(artifact)
span = tempo.get_span_by_name("breed-ontology")
assert span.attributes["ostar.artifact.state_before"] == ArtifactState.SEEDED
assert span.attributes["ostar.artifact.state_after"] == ArtifactState.BRED
assert result.state == ArtifactState.BRED  # corroborate with trace
```

### 5. No Test Naming Loopholes

**Forbidden (creates a category to optimize for):**
- `tests/chicago_tdd/` tier
- `@pytest.mark.chicago_tdd` marker
- `# Chicago TDD: this is a real test` comment
- Any naming scheme that lets agents satisfy "the name" instead of "the behavior"

**Why:** Naming creates a loophole. Agents and humans both optimize for satisfying the label, not the principle.

**Required:**
- Tests are validated by **what they prove**, not what they're called
- Enforcement is implicit (test cannot pass without real execution)
- No special naming, no special markers, no tier system

### 6. Multi-Surface Corroboration

A claim is only proven if it appears in **at least 3 of these**:
1. **Execution path** — code actually runs, state changes
2. **Observability** — OTel spans emitted with correct attributes
3. **State** — artifact, receipt, or DB records show change
4. **Process** — OCEL event log records the transition
5. **Causality** — preceding events are linked to this event

**Example: Proving an operator ran**

Not sufficient:
- ✗ Test asserts `result.state == BRED`
- ✗ Mocked tracer was called
- ✗ Artifact field was set

Sufficient:
- ✓ Operator received artifact (execution)
- ✓ OTel span "breed-ontology" exists with correct attributes (observability)
- ✓ Artifact state changed in returned object (state)
- ✓ OCEL log has transition event (process)
- ✓ Receipt links previous artifact hash to new artifact hash (causality)

### 7. Impossible State Injection

Any test that fabricates impossible state **must be rejected by the validator**:

```python
# These tests should FAIL or be rejected:
artifact.receipts = [receipt1, receipt2]
artifact.receipts[1].previous_hash = "wrong"  # tamper with chain
assert check_receipts(artifact)  # should return False

# If this test passes, the validator is broken
```

### 8. Resilience to Agent Laziness

The test is good if:
- ✅ Lazy agent: writes minimal code, test fails (misses real boundary)
- ✅ Cheating agent: stubs the boundary, test fails (no real evidence)
- ✅ Diligent agent: crosses boundary, test passes (real evidence exists)

The test is bad if:
- ❌ Lazy agent: writes trivial mock, test passes
- ❌ Cheating agent: stubs internal state, test passes
- ❌ Real execution is harder than faking it

## Implementation Checklist

Before writing a test, ask:

- [ ] Does this test require **real boundary-crossing** (LLM, MCP, OTel, process)?
- [ ] Would a **mock or stub** make this test pass without real execution?
- [ ] Is evidence **externalizable** (OTel, files, DB, logs)?
- [ ] Does the test prove **causality** (not just assertion)?
- [ ] Is the test **resilient to agent cheating** (faking is harder than real execution)?
- [ ] Does success require **multi-surface corroboration** (at least 3 of the 5)?
- [ ] Could a **lazy agent** pass this without doing the real work?

If any answer is wrong, redesign the test.

## Examples

### ❌ Bad Test (Assertion Without Evidence)

```python
async def test_breed_operator():
    artifact = make_artifact(state=ArtifactState.SEEDED)
    result = await BreedOntologyOperator().execute(artifact)
    assert result.state == ArtifactState.BRED  # could just set this
    assert "namespaces" in result.content  # could just add a key
```

**Why:** Test passes if code just sets the fields. No evidence that breeding happened.

### ✅ Good Test (Causality + Multi-Surface)

```python
async def test_breed_operator():
    artifact = make_artifact(state=ArtifactState.SEEDED)
    
    # Execute (boundary: code runs)
    result = await BreedOntologyOperator().execute(artifact)
    
    # State corroboration
    assert result.state == ArtifactState.BRED
    
    # Observability corroboration (OTel span exists)
    tempo = TempoClient()
    span = tempo.get_span_by_name("breed-ontology")
    assert span is not None
    assert span.attributes["ostar.artifact.state_before"] == "seeded"
    assert span.attributes["ostar.artifact.state_after"] == "bred"
    
    # Causality corroboration (receipt chain)
    new_receipt = Receipt.from_span(span)
    assert new_receipt.previous_hash == artifact.current_hash()
    assert result.receipts[-1].hash == new_receipt.hash
    
    # Process corroboration (OCEL)
    ocel = tempo.traces_to_ocel()
    events = [e for e in ocel if e["activity"] == "breed-ontology"]
    assert len(events) > 0
    assert events[-1]["artifact_state"] == "bred"
```

**Why:** Test requires real execution across 4 surfaces. Faking any one is insufficient. Easier to do the real thing.

---

**Remember:** 

> The test is good if real execution is easier than faking it.

Not: "write more tests"
But: **"make faking harder than the real thing"**

## Legitimate Test Infrastructure

The following test directories are excluded from truth-gate enforcement because they use test infrastructure patterns legitimately:

- `tests/smoke/` — Infrastructure smoke tests using subprocess/OTel mocks for env vars and paths
- `tests/archive/` — Quarantined legacy tests, not part of active test suite
- `src/ostar/testing/` — The anti-mock framework itself

These exclusions are intentional. Tests in these directories may use patterns that would be forbidden in production code.
