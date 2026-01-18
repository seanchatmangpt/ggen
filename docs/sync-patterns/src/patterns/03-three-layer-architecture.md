# 3. THREE-LAYER ARCHITECTURE **

*A building has structure, and the structure has layers.*

---

## Context

You are implementing **[THE SINGLE COMMAND](01-single-command.md)**. The command must:
- Parse arguments and validate input
- Execute complex domain logic
- Handle errors gracefully
- Format output appropriately

Putting all this in one place creates a monolith—hard to test, hard to change, hard to understand. Separating it randomly creates chaos—unclear responsibilities, tangled dependencies.

You need a structure that gives each concern its place.

---

❖ ❖ ❖

**When concerns are mixed, changes ripple unpredictably. When concerns are layered, changes stay contained.**

The forces:
- CLI parsing is about the interface (flags, help text)
- Execution is about orchestration (sequencing, error handling)
- Domain logic is about the work (ontologies, templates, generation)

Mix them and:
- Testing requires mocking the CLI
- Changing output format touches domain logic
- Domain bugs are hidden in orchestration code

Separate them and:
- Each layer can be tested independently
- Changes stay within their layer
- Responsibilities are clear

**Therefore:**

**Structure the implementation in three layers: CLI (input/output), Integration (orchestration), and Domain (pure logic). Each layer depends only on the layer below it.**

```
┌─────────────────────────────────┐
│  Layer 3: CLI                   │  ← Input parsing, output formatting
│  (thin, stateless)              │
├─────────────────────────────────┤
│  Layer 2: Integration           │  ← Orchestration, async execution
│  (SyncExecutor)                 │
├─────────────────────────────────┤
│  Layer 1: Domain                │  ← Pure generation logic
│  (GenerationPipeline)           │
└─────────────────────────────────┘
```

---

❖ ❖ ❖

## Connections

This pattern structures how **[THE SINGLE COMMAND](01-single-command.md)** is implemented.

- **[MANIFEST AS TRUTH](02-manifest-as-truth.md)** is parsed at Layer 3, used at Layer 1
- **[ERROR SIGNALS](12-error-signals.md)** are generated at Layer 1, formatted at Layer 3
- **[AUDIT TRAIL](14-audit-trail.md)** is produced at Layer 1, reported at Layer 2

---

## Implementation

### Layer 3: CLI (`ggen-cli`)

The CLI layer lives in `crates/ggen-cli/src/cmds/sync.rs`:

```rust
#[verb("sync", "root")]
pub fn sync(
    manifest: Option<String>,
    output_dir: Option<String>,
    dry_run: Option<bool>,
    force: Option<bool>,
    // ...more flags
) -> VerbResult<SyncOutput> {
    // Build options from CLI args
    let options = build_sync_options(manifest, output_dir, dry_run, force, ...);

    // Delegate to integration layer
    let result = SyncExecutor::new(options)
        .execute()
        .map_err(|e| NounVerbError::execution_error(e.to_string()))?;

    // Format result for output
    Ok(SyncOutput::from(result))
}
```

**Responsibilities:**
- Parse command-line arguments
- Validate input immediately
- Build options for the integration layer
- Format results for output
- Map errors to exit codes

**Characteristics:**
- Thin (minimal logic)
- Stateless (no side effects beyond calling the integration layer)
- Testable via integration tests

### Layer 2: Integration (`SyncExecutor`)

The integration layer lives in `crates/ggen-core/src/codegen/executor.rs`:

```rust
pub struct SyncExecutor {
    options: SyncOptions,
}

impl SyncExecutor {
    pub fn new(options: SyncOptions) -> Self {
        Self { options }
    }

    pub fn execute(&self) -> Result<SyncResult> {
        // Load and validate manifest
        let manifest = self.load_manifest()?;
        self.validate_manifest(&manifest)?;

        // Handle dry-run early
        if self.options.dry_run {
            return self.execute_dry_run(&manifest);
        }

        // Build and run the domain pipeline
        let mut pipeline = GenerationPipeline::new(manifest, self.base_path());
        let state = pipeline.run()?;

        // Build result from pipeline state
        Ok(self.build_result(state))
    }
}
```

**Responsibilities:**
- Orchestrate the execution flow
- Handle async operations (timeouts, concurrency)
- Coordinate between CLI options and domain logic
- Build result objects from domain state
- Apply flags like `--dry-run` and `--validate-only`

**Characteristics:**
- Async-aware (tokio)
- Error-handling focused
- Orchestrates but does not contain domain logic

### Layer 1: Domain (`GenerationPipeline`)

The domain layer lives in `crates/ggen-core/src/codegen/pipeline.rs`:

```rust
pub struct GenerationPipeline {
    manifest: GgenManifest,
    base_path: PathBuf,
    ontology_graph: Option<Graph>,
    // ...
}

impl GenerationPipeline {
    pub fn run(&mut self) -> Result<PipelineState> {
        // 1. Load ontology
        self.load_ontology()?;

        // 2. Execute inference rules
        self.execute_inference_rules()?;

        // 3. Execute generation rules
        self.execute_generation_rules()?;

        // 4. Build final state
        Ok(self.build_state())
    }
}
```

**Responsibilities:**
- Load and process ontologies
- Execute SPARQL queries
- Render templates
- Write generated files
- Track execution state

**Characteristics:**
- Pure (no CLI or async concerns)
- Testable in isolation
- Contains the "real work"

---

## The Data Flow

```
User Input
    ↓
┌─────────────────────────────────────┐
│ Layer 3: CLI                        │
│   - Parse: "ggen sync --dry-run"    │
│   - Build: SyncOptions              │
└────────────────┬────────────────────┘
                 ↓
┌─────────────────────────────────────┐
│ Layer 2: Integration                │
│   - Load manifest                   │
│   - Check dry-run flag              │
│   - Create pipeline                 │
└────────────────┬────────────────────┘
                 ↓
┌─────────────────────────────────────┐
│ Layer 1: Domain                     │
│   - Load ontology                   │
│   - Execute inference               │
│   - Execute generation              │
│   - Write files                     │
└────────────────┬────────────────────┘
                 ↓
            PipelineState
                 ↓
┌─────────────────────────────────────┐
│ Layer 2: Integration                │
│   - Build SyncResult                │
└────────────────┬────────────────────┘
                 ↓
┌─────────────────────────────────────┐
│ Layer 3: CLI                        │
│   - Format: SyncOutput              │
│   - Exit code: 0                    │
└─────────────────────────────────────┘
                 ↓
             User Output
```

---

## Testing Each Layer

**Layer 3 (CLI)**: Integration tests that invoke the binary

```rust
#[test]
fn test_sync_cli_dry_run() {
    Command::cargo_bin("ggen")
        .arg("sync")
        .arg("--dry-run")
        .assert()
        .success();
}
```

**Layer 2 (Integration)**: Unit tests with mock manifests

```rust
#[test]
fn test_executor_dry_run() {
    let options = SyncOptions { dry_run: true, ..Default::default() };
    let executor = SyncExecutor::new(options);
    let result = executor.execute().unwrap();
    assert_eq!(result.files_synced, 0);  // No files written
}
```

**Layer 1 (Domain)**: Unit tests with in-memory graphs

```rust
#[test]
fn test_pipeline_execute_inference() {
    let manifest = test_manifest();
    let mut pipeline = GenerationPipeline::new(manifest, temp_path());
    pipeline.load_ontology().unwrap();
    let rules = pipeline.execute_inference_rules().unwrap();
    assert_eq!(rules.len(), 2);
}
```

---

## The Deeper Pattern

THREE-LAYER ARCHITECTURE is about **separation of concerns at the architectural level**.

Each layer has:
- Its own language (CLI: flags; Integration: orchestration; Domain: graphs)
- Its own testing strategy
- Its own rate of change

When you change the output format, only Layer 3 changes. When you add a new pipeline stage, only Layer 1 changes. When you add a flag, Layers 3 and 2 change, but Layer 1 remains untouched.

This is not accidental. It is the result of carefully placing each concern in its proper layer.

---

## When This Pattern Breaks

THREE-LAYER ARCHITECTURE struggles when:

- The boundaries become unclear (is this orchestration or domain logic?)
- Performance requires bypassing layers
- The layers become too rigid to accommodate new patterns

ggen manages this by:

- Keeping Layer 3 genuinely thin (it's mostly a function call)
- Allowing Layer 2 to be flexible (it can bypass Layer 1 for dry-run)
- Letting Layer 1 own the complexity (all the real work is here)

If you find logic drifting between layers, ask: "What is this code's primary concern?" Place it in the layer that matches that concern.
