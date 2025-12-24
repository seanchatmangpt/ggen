# 14. AUDIT TRAIL **

*What happened, when, and why—recorded for posterity.*

---

## Context

You run `ggen sync`. Files are generated. But:

- Which rules produced which files?
- How long did each stage take?
- What version of the ontology was used?
- Can you prove the output matches the input?

Without answers to these questions, the generation is a black box. You know *that* something happened, but not *what* or *how*.

---

❖ ❖ ❖

**Trust requires evidence. Evidence requires records. Records require an audit trail.**

The forces:
- Debugging requires knowing what ran
- Optimization requires knowing what's slow
- Compliance may require provenance records
- Reproducibility requires capturing all inputs

Without an audit trail:
- Problems are hard to diagnose
- Performance issues are hard to identify
- Compliance cannot be demonstrated
- Reproducing a specific run is impossible

**Therefore:**

**Record all significant events during pipeline execution into an audit trail. Include rule names, durations, input hashes, output hashes, and any decisions made. Store the trail alongside generated output.**

The audit trail should record:
- Pipeline start time and duration
- Each executed rule with timing and hash
- Each generated file with content hash
- Any skipped rules with reasons
- Configuration state at execution time

---

❖ ❖ ❖

## Connections

This pattern captures the history of all other patterns' execution.

- **[DETERMINISTIC OUTPUT](13-deterministic-output.md)** is verified via hashes in the trail
- **[INFERENCE ENRICHMENT](05-inference-enrichment.md)** execution is recorded
- **[GENERATION RULES](06-generation-rules.md)** execution is recorded
- **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** enables audit generation

---

## Implementation

### Enabling the Audit Trail

**Via manifest:**

```toml
[generation]
require_audit_trail = true
```

**Via command line:**

```bash
ggen sync --audit
```

### Output Location

The audit trail is written alongside generated files:

```
src/generated/
├── user.rs
├── order.rs
├── product.rs
├── mod.rs
└── audit.json    ← Audit trail
```

### Audit Trail Structure

```json
{
  "version": "1.0.0",
  "pipeline": {
    "started_at": "2024-01-15T10:30:45.123Z",
    "completed_at": "2024-01-15T10:30:45.178Z",
    "duration_ms": 55,
    "status": "success"
  },
  "manifest": {
    "path": "ggen.toml",
    "hash": "sha256:abc123...",
    "project_name": "my-service",
    "project_version": "1.0.0"
  },
  "ontology": {
    "source": "ontology/domain.ttl",
    "hash": "sha256:def456...",
    "imports": [
      {
        "path": "ontology/common.ttl",
        "hash": "sha256:ghi789..."
      }
    ],
    "triple_count": 1247
  },
  "inference_rules": [
    {
      "name": "infer-required-fields",
      "order": 1,
      "started_at": "2024-01-15T10:30:45.130Z",
      "duration_ms": 8,
      "query_hash": "sha256:jkl012...",
      "triples_added": 42,
      "status": "executed"
    },
    {
      "name": "infer-nullable-fields",
      "order": 2,
      "started_at": "2024-01-15T10:30:45.138Z",
      "duration_ms": 5,
      "query_hash": "sha256:mno345...",
      "triples_added": 18,
      "status": "executed"
    }
  ],
  "generation_rules": [
    {
      "name": "structs",
      "started_at": "2024-01-15T10:30:45.145Z",
      "duration_ms": 12,
      "query_hash": "sha256:pqr678...",
      "template_hash": "sha256:stu901...",
      "files_generated": 4,
      "status": "executed"
    }
  ],
  "generated_files": [
    {
      "path": "src/generated/user.rs",
      "source_rule": "structs",
      "size_bytes": 1247,
      "content_hash": "sha256:vwx234...",
      "action": "created"
    },
    {
      "path": "src/generated/order.rs",
      "source_rule": "structs",
      "size_bytes": 982,
      "content_hash": "sha256:yza567...",
      "action": "updated"
    }
  ],
  "summary": {
    "inference_rules_executed": 2,
    "generation_rules_executed": 1,
    "files_created": 3,
    "files_updated": 1,
    "files_unchanged": 1,
    "total_files": 5
  }
}
```

---

## Audit Trail Uses

### Debugging

"Why is this field missing from the generated struct?"

1. Check `inference_rules` — did the derivation rule run?
2. Check `triples_added` — did it add expected triples?
3. Check `generation_rules` — did the struct rule run?
4. Check `query_hash` — has the query changed?

### Performance Analysis

"Generation is slow. What's taking the time?"

```bash
jq '.inference_rules[] | {name, duration_ms}' audit.json
```

```json
{"name": "infer-required-fields", "duration_ms": 8}
{"name": "infer-complex-validation", "duration_ms": 2847}
{"name": "infer-nullable-fields", "duration_ms": 5}
```

The slow rule is immediately visible.

### Reproducibility Verification

"Is this output from this ontology?"

Compare hashes:

```bash
# Current ontology hash
sha256sum ontology/domain.ttl

# Hash in audit trail
jq '.ontology.hash' audit.json
```

If they match, the output was generated from this ontology.

### Change Detection

"What changed between this run and the last?"

```bash
diff old-audit.json new-audit.json | grep content_hash
```

Changed content hashes indicate changed output.

---

## Recording Implementation

```rust
#[derive(Serialize)]
pub struct AuditTrail {
    pub version: String,
    pub pipeline: PipelineAudit,
    pub manifest: ManifestAudit,
    pub ontology: OntologyAudit,
    pub inference_rules: Vec<RuleAudit>,
    pub generation_rules: Vec<RuleAudit>,
    pub generated_files: Vec<FileAudit>,
    pub summary: SummaryAudit,
}

impl GenerationPipeline {
    fn build_audit_trail(&self) -> AuditTrail {
        AuditTrail {
            version: "1.0.0".to_string(),
            pipeline: self.build_pipeline_audit(),
            manifest: self.build_manifest_audit(),
            ontology: self.build_ontology_audit(),
            inference_rules: self.executed_rules
                .iter()
                .filter(|r| r.rule_type == RuleType::Inference)
                .map(|r| self.build_rule_audit(r))
                .collect(),
            // ... etc
        }
    }
}
```

---

## The Deeper Pattern

AUDIT TRAIL is about **provenance**.

Provenance answers: Where did this come from? How was it made? Can it be reproduced?

In code generation:
- **Source provenance**: Which ontology, which version?
- **Process provenance**: Which rules, in what order?
- **Output provenance**: What files, with what content?

The audit trail captures all three, making the generation process **transparent** and **verifiable**.

---

## Audit vs. Logging

| Aspect | Audit Trail | Logging |
|--------|-------------|---------|
| Purpose | Provenance record | Debugging |
| Format | Structured JSON | Free-form text |
| Retention | Permanent | Often ephemeral |
| Content | Facts about execution | Progress and errors |
| Audience | Auditors, automation | Developers |

Logs help you watch the process. Audits help you trust the output.

---

## When This Pattern Breaks

AUDIT TRAIL struggles when:

- Audit files become large (many generated files)
- Sensitive data appears in audits (credentials, PII)
- Audit writing itself fails

ggen manages this through:

- Summary counts for overview
- Hashes instead of full content
- Best-effort audit writing (failure doesn't stop generation)

For high-volume generation, consider:
- Separate audit files per rule
- Audit database instead of files
- Sampling strategy for large runs

The pattern remains: significant events are recorded, creating a verifiable history.
