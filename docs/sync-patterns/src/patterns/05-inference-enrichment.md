# 5. INFERENCE ENRICHMENT *

*What is implicit must be made explicit before it can be used.*

---

## Context

You have loaded your ontology via **[ONTOLOGY LOADING](04-ontology-loading.md)**. The graph contains facts that were explicitly stated. But often, there are facts that *should* exist—facts that can be derived from what is stated.

For example:
- If a field has `minCardinality > 0`, it is *required*
- If a class has no explicit parent, it inherits from a default
- If two entities share a relationship, a reverse relationship exists

You could state these derived facts explicitly, but that creates redundancy and potential inconsistency. Better to state the rules once and let the system derive the facts.

---

❖ ❖ ❖

**When derived knowledge is implicit, generation logic must re-derive it everywhere. When derived knowledge is explicit, generation logic simply uses it.**

The forces:
- Derived facts should be computed once, not repeatedly
- Derivation rules should be declarative, not embedded in code
- Derived facts should be queryable like explicit facts
- Derivation must complete before generation begins

Without explicit enrichment:
- Each generation rule re-implements derivation logic
- Different rules may derive differently (inconsistency)
- Complex derivations become impossible
- The graph's semantic completeness is unknown

**Therefore:**

**Execute CONSTRUCT queries that derive new facts and materialize them into the graph before any generation rules run. Order the inference rules explicitly. Make the enrichment observable in the audit trail.**

The enrichment should:
- Use SPARQL CONSTRUCT to express derivation rules
- Add derived triples directly to the ontology graph
- Execute rules in a defined order (earlier rules inform later ones)
- Track how many triples each rule adds
- Complete fully before generation begins

---

❖ ❖ ❖

## Connections

This pattern operates on the graph created by **[ONTOLOGY LOADING](04-ontology-loading.md)**.

- **[GENERATION RULES](06-generation-rules.md)** queries the enriched graph
- **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** configures the inference rules
- **[AUDIT TRAIL](14-audit-trail.md)** records inference execution
- **[DETERMINISTIC OUTPUT](13-deterministic-output.md)** requires ordered execution

---

## Implementation

### Manifest Configuration

The `[inference]` section defines enrichment rules:

```toml
[inference]
max_reasoning_timeout_ms = 5000

[[inference.rules]]
name = "infer-required-fields"
description = "Mark fields as required based on cardinality"
order = 1
construct = """
PREFIX domain: <https://example.org/domain#>

CONSTRUCT {
    ?field domain:isRequired true .
}
WHERE {
    ?field domain:minCardinality ?min .
    FILTER(?min > 0)
}
"""

[[inference.rules]]
name = "infer-nullable-fields"
description = "Mark fields as nullable if not required"
order = 2
construct = """
PREFIX domain: <https://example.org/domain#>

CONSTRUCT {
    ?field domain:isNullable true .
}
WHERE {
    ?field a domain:Field .
    FILTER NOT EXISTS { ?field domain:isRequired true }
}
"""

[[inference.rules]]
name = "infer-validation-traits"
description = "Add validation marker to validated entities"
order = 3
when = "ASK { ?x domain:hasValidation ?y }"
construct = """
PREFIX domain: <https://example.org/domain#>

CONSTRUCT {
    ?entity domain:needsValidation true .
}
WHERE {
    ?entity domain:hasValidation ?rule .
}
"""
```

### Rule Execution

```rust
pub fn execute_inference_rules(&mut self) -> Result<Vec<ExecutedRule>> {
    let mut executed = Vec::new();

    // Sort rules by order (deterministic)
    let mut rules: Vec<_> = self.manifest.inference.rules.clone();
    rules.sort_by_key(|r| r.order);

    for rule in rules {
        let result = self.execute_inference_rule(&rule)?;
        executed.push(result);
    }

    self.executed_rules.extend(executed.clone());
    Ok(executed)
}

fn execute_inference_rule(&mut self, rule: &InferenceRule) -> Result<ExecutedRule> {
    let start = Instant::now();

    let graph = self.ontology_graph.as_ref()
        .ok_or_else(|| Error::new("Ontology not loaded"))?;

    // Execute CONSTRUCT and materialize results
    let executor = ConstructExecutor::new(graph);
    let triples_added = executor.execute_and_materialize(&rule.construct)?;

    let duration = start.elapsed();
    let query_hash = format!("{:x}", sha2::Sha256::digest(rule.construct.as_bytes()));

    Ok(ExecutedRule {
        name: rule.name.clone(),
        rule_type: RuleType::Inference,
        triples_added,
        duration_ms: duration.as_millis() as u64,
        query_hash,
    })
}
```

### Materialization

CONSTRUCT queries return new triples. Materialization means **adding those triples to the graph**:

```
Before:  Graph has triples from ontology files
         ↓
CONSTRUCT runs, produces new triples
         ↓
Materialize: Add new triples to graph
         ↓
After:   Graph has original + derived triples
```

This is different from simply querying—the graph is *modified*.

### Ordering

The `order` field is critical:

```toml
[[inference.rules]]
name = "first-rule"
order = 1    # Runs first

[[inference.rules]]
name = "second-rule"
order = 2    # Runs second, can see results of first
```

Lower order values run first. Rules with the same order value have undefined relative order (avoid this for **[DETERMINISTIC OUTPUT](13-deterministic-output.md)**).

### Conditional Execution

The optional `when` field contains a SPARQL ASK query:

```toml
[[inference.rules]]
name = "conditional-rule"
when = "ASK { ?x domain:someProperty ?y }"
construct = "..."
```

If `when` is specified and the ASK query returns `false`, the rule is skipped. This enables:
- Performance optimization (skip irrelevant rules)
- Conditional logic (enrich only when conditions are met)

---

## The Inference Chain

Rules can build on each other:

```
Rule 1: infer-required-fields
   Adds: ?field domain:isRequired true .

Rule 2: infer-nullable-fields (order = 2)
   Query: FILTER NOT EXISTS { ?field domain:isRequired true }
   This sees the results of Rule 1!

Rule 3: infer-api-validation (order = 3)
   Query: ?field domain:isRequired true .
   This sees accumulated results!
```

Each rule enriches the graph for subsequent rules. This is **forward chaining** inference.

---

## Audit Trail

Each executed inference rule is recorded:

```json
{
  "executed_rules": [
    {
      "name": "infer-required-fields",
      "rule_type": "Inference",
      "triples_added": 42,
      "duration_ms": 12,
      "query_hash": "a1b2c3d4..."
    },
    {
      "name": "infer-nullable-fields",
      "rule_type": "Inference",
      "triples_added": 18,
      "duration_ms": 8,
      "query_hash": "e5f6g7h8..."
    }
  ]
}
```

This enables:
- Debugging (which rule added what?)
- Performance analysis (which rule is slow?)
- Reproducibility verification (same hashes = same queries)

---

## The Deeper Pattern

INFERENCE ENRICHMENT is about **making implicit knowledge explicit**.

Domain experts often think in terms of rules:
- "A required field is one with minimum cardinality greater than zero"
- "An entity needs validation if it has any validation rules"

These rules are *implicit* in the domain model. INFERENCE ENRICHMENT makes them *explicit* in the graph.

Once explicit, the knowledge is:
- Queryable (generation rules can SELECT it)
- Cacheable (computed once, used many times)
- Auditable (recorded in the trail)
- Deterministic (same rules produce same facts)

---

## When This Pattern Breaks

INFERENCE ENRICHMENT struggles when:

- Inference rules are complex (performance degrades)
- Inference chains are deep (debugging becomes hard)
- Inference rules conflict (which truth wins?)
- Inference requires external data (not in the graph)

ggen addresses these partially:

- Timeout protection limits execution time
- Ordered execution makes chains predictable
- Last-rule-wins for conflicting assertions
- Only graph-based inference is supported

For very complex inference needs, consider:
- Pre-computing derived facts offline
- Using a dedicated reasoner
- Simplifying the ontology design

The pattern remains: before generation, the graph should contain all facts that generation needs—whether stated or derived.
