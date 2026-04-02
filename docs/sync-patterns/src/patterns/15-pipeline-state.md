<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [15. PIPELINE STATE *](#15-pipeline-state-)
  - [Context](#context)
  - [Connections](#connections)
  - [Implementation](#implementation)
    - [The PipelineState Structure](#the-pipelinestate-structure)
    - [State Progression](#state-progression)
    - [Building State](#building-state)
  - [State Records](#state-records)
    - [ExecutedRule](#executedrule)
    - [GeneratedFile](#generatedfile)
    - [ValidationResult](#validationresult)
  - [Querying State](#querying-state)
  - [State and Dry Run](#state-and-dry-run)
  - [The Deeper Pattern](#the-deeper-pattern)
  - [State Immutability](#state-immutability)
  - [When This Pattern Breaks](#when-this-pattern-breaks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 15. PIPELINE STATE *

*The pipeline is not just a sequence—it is a growing state.*

---

## Context

The generation pipeline executes in stages:

1. Load manifest
2. Load ontology
3. Execute inference rules
4. Execute generation rules
5. Write files

Each stage produces results. Later stages need earlier results. The ontology graph grows as inference adds triples. The list of generated files accumulates. Validation results collect.

This is not a stateless transformation—it is a **stateful process**.

---

❖ ❖ ❖

**Stateful processes require explicit state management. Implicit state leads to confusion and bugs.**

The forces:
- Each stage needs access to previous results
- State must be accessible for debugging
- State must be immutable after stages complete
- The final state must be reportable

Without explicit state:
- Stages communicate via side effects
- Debugging requires mental reconstruction
- Race conditions become possible
- The final report cannot capture intermediate states

**Therefore:**

**Model the pipeline as a progression through explicit states. Each stage receives the previous state and produces the next state. The final state contains all accumulated results. Make state observable and queryable.**

The pipeline state should track:
- The loaded manifest
- The ontology graph (before and after inference)
- Executed rules with their results
- Generated files with their metadata
- Validation outcomes
- Timing information

---

❖ ❖ ❖

## Connections

This pattern underpins **[AUDIT TRAIL](14-audit-trail.md)**, which serializes the final state.

- **[ONTOLOGY LOADING](04-ontology-loading.md)** produces the initial graph state
- **[INFERENCE ENRICHMENT](05-inference-enrichment.md)** modifies and tracks graph state
- **[GENERATION RULES](06-generation-rules.md)** accumulate file state

---

## Implementation

### The PipelineState Structure

```rust
pub struct PipelineState {
    /// Loaded manifest
    pub manifest: GgenManifest,

    /// Domain ontology graph (after all inference)
    pub ontology_graph: Graph,

    /// Code graph (for future use)
    pub code_graph: Graph,

    /// All executed rules (inference and generation)
    pub executed_rules: Vec<ExecutedRule>,

    /// All generated files
    pub generated_files: Vec<GeneratedFile>,

    /// All validation results
    pub validation_results: Vec<ValidationResult>,

    /// Pipeline start time
    pub started_at: Instant,
}
```

### State Progression

```
┌──────────────────────────────────────────────────────────────────┐
│                      PipelineState                                │
├──────────────────────────────────────────────────────────────────┤
│                                                                   │
│  After load_ontology():                                          │
│    ✓ manifest: GgenManifest                                      │
│    ✓ ontology_graph: Graph (initial triples)                     │
│    ○ executed_rules: []                                          │
│    ○ generated_files: []                                         │
│                                                                   │
├──────────────────────────────────────────────────────────────────┤
│                                                                   │
│  After execute_inference_rules():                                 │
│    ✓ manifest: GgenManifest                                      │
│    ✓ ontology_graph: Graph (initial + derived triples)           │
│    ✓ executed_rules: [InferenceRule1, InferenceRule2, ...]       │
│    ○ generated_files: []                                         │
│                                                                   │
├──────────────────────────────────────────────────────────────────┤
│                                                                   │
│  After execute_generation_rules():                                │
│    ✓ manifest: GgenManifest                                      │
│    ✓ ontology_graph: Graph (final)                               │
│    ✓ executed_rules: [Inference..., Generation...]               │
│    ✓ generated_files: [File1, File2, File3, ...]                 │
│                                                                   │
└──────────────────────────────────────────────────────────────────┘
```

### Building State

```rust
impl GenerationPipeline {
    pub fn run(&mut self) -> Result<PipelineState> {
        // 1. Load ontology → State has graph
        self.load_ontology()?;

        // 2. Execute inference → State has rules
        self.execute_inference_rules()?;

        // 3. Execute generation → State has files
        self.execute_generation_rules()?;

        // 4. Build final state
        let state = PipelineState {
            manifest: self.manifest.clone(),
            ontology_graph: self.ontology_graph.take()
                .unwrap_or_else(|| Graph::new().unwrap()),
            code_graph: self.code_graph.take()
                .unwrap_or_else(|| Graph::new().unwrap()),
            executed_rules: self.executed_rules.clone(),
            generated_files: self.generated_files.clone(),
            validation_results: self.validation_results.clone(),
            started_at: self.started_at,
        };

        Ok(state)
    }
}
```

---

## State Records

### ExecutedRule

```rust
#[derive(Debug, Clone, Serialize)]
pub struct ExecutedRule {
    /// Rule name
    pub name: String,

    /// Rule type (inference or generation)
    pub rule_type: RuleType,

    /// Number of triples added (inference) or files generated
    pub triples_added: usize,

    /// Execution duration in milliseconds
    pub duration_ms: u64,

    /// Hash of the query for determinism verification
    pub query_hash: String,
}
```

### GeneratedFile

```rust
#[derive(Debug, Clone, Serialize)]
pub struct GeneratedFile {
    /// Output file path
    pub path: PathBuf,

    /// SHA256 hash of content
    pub content_hash: String,

    /// File size in bytes
    pub size_bytes: usize,

    /// Name of the rule that generated this file
    pub source_rule: String,
}
```

### ValidationResult

```rust
#[derive(Debug, Clone, Serialize)]
pub struct ValidationResult {
    /// Rule name that was checked
    pub rule_name: String,

    /// Whether validation passed
    pub passed: bool,

    /// Optional message
    pub message: Option<String>,

    /// Severity of the validation
    pub severity: ValidationSeverity,
}
```

---

## Querying State

State enables runtime introspection:

```rust
// How many triples did inference add?
let total_triples: usize = state.executed_rules
    .iter()
    .filter(|r| r.rule_type == RuleType::Inference)
    .map(|r| r.triples_added)
    .sum();

// Which rule was slowest?
let slowest = state.executed_rules
    .iter()
    .max_by_key(|r| r.duration_ms);

// Which files came from the 'structs' rule?
let struct_files: Vec<_> = state.generated_files
    .iter()
    .filter(|f| f.source_rule == "structs")
    .collect();
```

---

## State and Dry Run

In dry-run mode, state is built but files are not written:

```rust
pub fn execute_dry_run(&self, manifest: &GgenManifest) -> Result<SyncResult> {
    let mut pipeline = GenerationPipeline::new(manifest.clone(), self.base_path());

    // Run pipeline (builds state internally)
    pipeline.load_ontology()?;
    pipeline.execute_inference_rules()?;

    // Execute generation but capture state, don't write
    pipeline.execute_generation_rules_preview()?;

    // Build result from state
    Ok(self.build_dry_run_result(&pipeline))
}
```

The state exists; only the side effects are suppressed.

---

## The Deeper Pattern

PIPELINE STATE is about **making the process observable**.

A pipeline is not just a function: it is a **journey** through states. Each step transforms the state. The final state contains the history of that journey.

This explicit state enables:
- **Debugging**: Inspect state at any point
- **Auditing**: Serialize state for records
- **Analysis**: Query state for insights
- **Testing**: Assert on state properties

Without explicit state, you only see inputs and outputs. With explicit state, you see the entire transformation.

---

## State Immutability

Once a stage completes, its contribution to state is immutable:

- Inference rules don't remove triples (they add)
- Generated files are recorded once
- Validation results accumulate

This immutability ensures:
- Earlier stages can't be corrupted by later ones
- State can be safely shared
- Parallel execution is possible (future)

---

## When This Pattern Breaks

PIPELINE STATE struggles when:

- State becomes very large (memory pressure)
- State needs to persist across runs (currently in-memory only)
- Parallel stages need to merge state safely

ggen manages this through:

- Keeping state lightweight (hashes, not full content)
- Serializing to audit trail at the end
- Sequential execution (no parallel merge needed yet)

For very large pipelines, consider:
- Streaming state to disk
- Database-backed state storage
- Incremental state updates

The pattern remains: the pipeline's progress is captured in explicit, queryable state.
