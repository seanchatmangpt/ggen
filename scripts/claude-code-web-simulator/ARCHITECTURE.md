# Claude Code Web Simulator - Architecture

Comprehensive architectural documentation of the Claude Code Web simulation environment.

## System Overview

```
┌──────────────────────────────────────────────────────────────────┐
│                    Claude Code Web Environment                    │
├──────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │ Sandbox Layer (OS-Level Isolation)                          │ │
│  │  ├─ Bubblewrap (Linux) / Seatbelt (macOS)                   │ │
│  │  ├─ Network: Proxy-only, domain whitelist                   │ │
│  │  ├─ Filesystem: Workspace-only, no root access              │ │
│  │  └─ Processes: Child processes inherit restrictions         │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                             ▲                                      │
│                             │                                      │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │ Agent Orchestrator (Multi-Agent Coordination)                │ │
│  │  ├─ Agent Lifecycle Management                              │ │
│  │  │  ├─ Bootstrap (SessionStart hooks)                       │ │
│  │  │  ├─ Initialize (MCP servers, tools)                      │ │
│  │  │  ├─ Execute (Run agent task)                             │ │
│  │  │  └─ Cleanup (Shutdown, memory persist)                   │ │
│  │  ├─ Parallel Execution                                       │ │
│  │  ├─ Dependency Management                                    │ │
│  │  ├─ Memory Synchronization                                   │ │
│  │  ├─ Collision Detection                                      │ │
│  │  └─ Convergence Synthesis                                    │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                             ▲                                      │
│                             │                                      │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │ ggen Pipeline (Deterministic Transformation)                │ │
│  │                                                              │ │
│  │  Input: RDF Ontology + Templates                            │ │
│  │    ▼                                                         │ │
│  │  ┌──────────────────────────────────────────────────────┐   │ │
│  │  │ μ₁ (Normalize)                    [400ms, <5% error] │   │ │
│  │  │ • Parse RDF (Turtle, RDF/XML, N-Triples)            │   │ │
│  │  │ • SHACL shape validation                             │   │ │
│  │  │ • Dependency resolution                              │   │ │
│  │  │ • OWL inference                                      │   │ │
│  │  │ Output: Validated RDF graph (3,847 triples)          │   │ │
│  │  └──────────────────────────────────────────────────────┘   │ │
│  │    ▼                                                         │ │
│  │  ┌──────────────────────────────────────────────────────┐   │ │
│  │  │ μ₂ (Extract)                      [500ms, <3% error] │   │ │
│  │  │ • Execute SPARQL queries (SELECT, CONSTRUCT, ASK)    │   │ │
│  │  │ • Apply inference rules (RDFS, OWL2-RL)              │   │ │
│  │  │ • Extract template context (JSON/YAML)               │   │ │
│  │  │ Output: Structured data for templates (2,156 facts)  │   │ │
│  │  └──────────────────────────────────────────────────────┘   │ │
│  │    ▼                                                         │ │
│  │  ┌──────────────────────────────────────────────────────┐   │ │
│  │  │ μ₃ (Emit)                         [600ms, <2% error] │   │ │
│  │  │ • Tera template rendering (SPARQL-aware)             │   │ │
│  │  │ • Code generation (Rust, TS, Python, Go)             │   │ │
│  │  │ • Multi-file generation (directory structures)        │   │ │
│  │  │ Output: Raw generated artifacts (47 files)           │   │ │
│  │  └──────────────────────────────────────────────────────┘   │ │
│  │    ▼                                                         │ │
│  │  ┌──────────────────────────────────────────────────────┐   │ │
│  │  │ μ₄ (Canonicalize)                 [300ms, 0% error]  │   │ │
│  │  │ • Deterministic formatting (rustfmt, prettier, black)│   │ │
│  │  │ • Syntax validation (compiler checks, linters)       │   │ │
│  │  │ • Content hashing (SHA-256 per file)                 │   │ │
│  │  │ Output: Canonicalized, formatted artifacts           │   │ │
│  │  └──────────────────────────────────────────────────────┘   │ │
│  │    ▼                                                         │ │
│  │  ┌──────────────────────────────────────────────────────┐   │ │
│  │  │ μ₅ (Receipt)                      [200ms, 0% error]  │   │ │
│  │  │ • Cryptographic proof generation                      │   │ │
│  │  │ • Audit trail logging (JSON with provenance)          │   │ │
│  │  │ • Manifest + ontology fingerprinting                  │   │ │
│  │  │ • File-by-file change tracking                        │   │ │
│  │  │ Output: Deterministic receipt (JSON), audit log       │   │ │
│  │  └──────────────────────────────────────────────────────┘   │ │
│  │    ▼                                                         │ │
│  │  Output: Generated code + deterministic receipt              │ │
│  │                                                              │ │
│  │  SLO Targets:                                                │ │
│  │  • Total duration: <5s (1k+ triples)                         │ │
│  │  • Memory: <100MB                                            │ │
│  │  • Determinism: 100% (same input = identical output)         │ │
│  │                                                              │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                             ▲                                      │
│                             │                                      │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │ MCP Proxy (Tool Integration)                                │ │
│  │  ├─ Tool Lookup (200+ available MCP servers)                │ │
│  │  ├─ Domain Whitelist Enforcement                            │ │
│  │  ├─ Timeout Management (10s default, configurable)          │ │
│  │  ├─ Response Formatting (JSON, structured output)           │ │
│  │  ├─ Error Handling (exit codes, retry logic)                │ │
│  │  └─ Performance Optimization (caching, connection pooling)  │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                             ▲                                      │
│                             │                                      │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │ Hook Engine (Event-Driven Coordination)                      │ │
│  │                                                              │ │
│  │  SessionStart Hook (Agent Bootstrap):                        │ │
│  │  ├─ Verify timeout command exists                           │ │
│  │  ├─ Install ggen (cargo install --locked)                   │ │
│  │  ├─ Initialize MCP servers                                  │ │
│  │  ├─ Create isolated workspace                               │ │
│  │  └─ Load agent skills and configuration                     │ │
│  │                                                              │ │
│  │  pre-task Hook (Input Validation):                           │ │
│  │  ├─ Validate specification syntax                           │ │
│  │  ├─ Check file permissions                                  │ │
│  │  ├─ Verify domain whitelisting                              │ │
│  │  └─ Load agent memory context                               │ │
│  │                                                              │ │
│  │  post-tool Hook (Memory Update):                             │ │
│  │  ├─ Record execution metrics                                │ │
│  │  ├─ Update agent memory                                     │ │
│  │  ├─ Persist collision detection data                        │ │
│  │  └─ Trigger convergence if needed                           │ │
│  │                                                              │ │
│  │  post-edit Hook (Quality Validation):                        │ │
│  │  ├─ Validate generated code syntax                          │ │
│  │  ├─ Run linters (cargo make lint)                           │ │
│  │  ├─ Verify SLO compliance                                   │ │
│  │  └─ Update receipt with validation results                  │ │
│  │                                                              │ │
│  │  convergence Hook (Output Synthesis):                        │ │
│  │  ├─ Merge parallel agent outputs                            │ │
│  │  ├─ Apply selection pressure (coverage, invariants)         │ │
│  │  ├─ Minimize redundancy                                     │ │
│  │  └─ Generate unified result                                 │ │
│  │                                                              │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                             ▲                                      │
│                             │                                      │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │ Memory Integration System                                    │ │
│  │                                                              │ │
│  │  Agent Memory (JSON):                                        │ │
│  │  {                                                           │ │
│  │    "agent_id": "validator-1234567890",                       │ │
│  │    "session_id": "sess-abc123",                              │ │
│  │    "status": "executing",                                    │ │
│  │    "operations": [...],                                      │ │
│  │    "memory": {                                               │ │
│  │      "last_ontology_hash": "sha256:...",                     │ │
│  │      "collision_history": [...],                             │ │
│  │      "retry_count": 0                                        │ │
│  │    }                                                          │ │
│  │  }                                                            │ │
│  │                                                              │ │
│  │  Collision Detection:                                        │ │
│  │  ├─ Structural overlap (same files generated)                │ │
│  │  ├─ Semantic overlap (same intent, different code)           │ │
│  │  ├─ Temporal overlap (concurrent modifications)              │ │
│  │  └─ Resolution: Last-write-wins or convergence               │ │
│  │                                                              │ │
│  │  Cross-Agent Synchronization:                                │ │
│  │  ├─ Agent A generates → updates memory                       │ │
│  │  ├─ Agent B reads memory → avoids collision                  │ │
│  │  ├─ Central memory coordinator → consistency                 │ │
│  │  └─ Periodic sync → all agents stay coherent                 │ │
│  │                                                              │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                             ▲                                      │
│                             │                                      │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │ Error Handler & Exit Code System                             │ │
│  │                                                              │ │
│  │  Exit Code Mapping:                                          │ │
│  │  ├─ 0   → Success                                            │ │
│  │  ├─ 1   → General error                                      │ │
│  │  ├─ 2   → Validation failed (SHACL, schema)                  │ │
│  │  ├─ 4   → SPARQL error (query syntax, inference)             │ │
│  │  ├─ 5   → Template error (Tera rendering)                    │ │
│  │  ├─ 126 → Permission denied                                  │ │
│  │  ├─ 127 → Command not found                                  │ │
│  │  └─ 130 → Interrupted (Ctrl+C)                               │ │
│  │                                                              │ │
│  │  Error Detection:                                            │ │
│  │  ├─ Monitor exit codes                                       │ │
│  │  ├─ Parse error messages                                     │ │
│  │  ├─ Extract root cause (via agent memory)                    │ │
│  │  └─ Trigger retry logic (exponential backoff)                │ │
│  │                                                              │ │
│  │  Recovery Procedures:                                        │ │
│  │  ├─ Validation error → Fix ontology, retry μ₁                │ │
│  │  ├─ SPARQL error → Correct query, retry μ₂                   │ │
│  │  ├─ Template error → Fix Tera template, retry μ₃             │ │
│  │  └─ Network error → Retry via proxy, use backoff              │ │
│  │                                                              │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                             ▲                                      │
│                             │                                      │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │ Receipt Generator (Determinism Verification)                │ │
│  │                                                              │ │
│  │  Receipt Structure (JSON):                                   │ │
│  │  {                                                           │ │
│  │    "receipt": {                                              │ │
│  │      "execution_id": "exec-1234567890123456",                │ │
│  │      "agent_id": "agent-identifier",                         │ │
│  │      "timestamp": "2026-01-29T15:30:00Z",                    │ │
│  │      "hashes": {                                             │ │
│  │        "manifest": "sha256:...",                             │ │
│  │        "ontology": "sha256:...",                             │ │
│  │        "templates": "sha256:..."                             │ │
│  │      },                                                       │ │
│  │      "files_generated": 47,                                  │ │
│  │      "files_modified": 12,                                   │ │
│  │      "pipeline_stages": {                                    │ │
│  │        "μ₁_normalize": { "duration_ms": 400 },               │ │
│  │        "μ₂_extract": { "duration_ms": 500 },                 │ │
│  │        "μ₃_emit": { "duration_ms": 600 },                    │ │
│  │        "μ₄_canonicalize": { "duration_ms": 300 },            │ │
│  │        "μ₅_receipt": { "duration_ms": 200 }                  │ │
│  │      },                                                       │ │
│  │      "total_duration_ms": 2000,                              │ │
│  │      "determinism_guarantee": true                           │ │
│  │    }                                                          │ │
│  │  }                                                            │ │
│  │                                                              │ │
│  │  Audit Trail (Log):                                          │ │
│  │  [ISO8601] Agent: ID | Operation: OP | Status: OK | Dur: Xms │ │
│  │                                                              │ │
│  │  Verification:                                               │ │
│  │  • Run identical operation again                             │ │
│  │  • Compare SHA-256 hashes                                    │ │
│  │  • If same → determinism verified ✓                          │ │
│  │  • If different → investigate source (non-deterministic)     │ │
│  │                                                              │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                             ▲                                      │
│                             │                                      │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │ Agent Invocation Patterns                                    │ │
│  │                                                              │ │
│  │  Pattern A: Validation (Dry-Run / Pre-Flight)                │ │
│  │  ┌─ Input: RDF spec, SHACL shapes                            │ │
│  │  ├─ Operation: ggen sync --validate_only true                │ │
│  │  ├─ Output: Validation status (JSON)                         │ │
│  │  ├─ Exit code: 0 (pass) or 2 (fail)                          │ │
│  │  └─ Side effects: None (no files generated)                  │ │
│  │                                                              │ │
│  │  Pattern B: JSON Output (Machine Processing)                 │ │
│  │  ┌─ Input: RDF spec, templates                               │ │
│  │  ├─ Operation: ggen sync --output json                       │ │
│  │  ├─ Output: Structured JSON (parseable by agents)            │ │
│  │  ├─ Exit code: 0 (success) or error code                     │ │
│  │  └─ Side effects: Files generated (can be committed)         │ │
│  │                                                              │ │
│  │  Pattern C: Watch Mode (Continuous)                          │ │
│  │  ┌─ Input: Monitor directory for changes                     │ │
│  │  ├─ Operation: ggen sync --watch true                        │ │
│  │  ├─ Output: Streaming receipts (one per cycle)               │ │
│  │  ├─ Exit code: Continues until interrupted                   │ │
│  │  └─ Side effects: Files regenerated on each change           │ │
│  │                                                              │ │
│  │  Pattern D: Dry-Run (Preview Only)                           │ │
│  │  ┌─ Input: RDF spec, templates                               │ │
│  │  ├─ Operation: ggen sync --dry_run true                      │ │
│  │  ├─ Output: What would be generated (no changes)             │ │
│  │  ├─ Exit code: 0 (success) or error code                     │ │
│  │  └─ Side effects: None (preview only)                        │ │
│  │                                                              │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                                                                   │
└──────────────────────────────────────────────────────────────────┘
```

## Component Interactions

### Startup Sequence

```
┌─────────────────────────────────────────────────────┐
│ 1. User Executes: ./main.sh run-agent validation    │
└──────────────────────┬──────────────────────────────┘
                       ▼
┌─────────────────────────────────────────────────────┐
│ 2. Main Orchestrator                                │
│    - Parse command arguments                        │
│    - Generate unique agent ID                       │
│    - Create agent workspace (/sandboxes/{id}/)      │
└──────────────────────────────┬──────────────────────┘
                               ▼
┌─────────────────────────────────────────────────────┐
│ 3. SessionStart Hook (Bootstrap)                    │
│    - Verify timeout command                         │
│    - Initialize sandbox environment                 │
│    - Configure MCP servers                          │
│    - Set up agent memory context                    │
└──────────────────────────────┬──────────────────────┘
                               ▼
┌─────────────────────────────────────────────────────┐
│ 4. Pre-Task Hook (Input Validation)                 │
│    - Validate specification syntax                  │
│    - Check file permissions                         │
│    - Load agent memory from previous runs           │
│    - Apply collision detection                      │
└──────────────────────────────┬──────────────────────┘
                               ▼
┌─────────────────────────────────────────────────────┐
│ 5. Agent Execution (Main Work)                      │
│    - Invoke ggen sync with appropriate flags        │
│    - Monitor process (timeout enforcement)          │
│    - Capture exit code                              │
│    - Parse output and errors                        │
└──────────────────────────────┬──────────────────────┘
                               ▼
┌─────────────────────────────────────────────────────┐
│ 6. ggen Pipeline (μ₁-μ₅)                            │
│    - μ₁: Normalize RDF ontology                     │
│    - μ₂: Extract via SPARQL                         │
│    - μ₃: Emit code via Tera templates               │
│    - μ₄: Canonicalize output                        │
│    - μ₅: Generate receipt                           │
└──────────────────────────────┬──────────────────────┘
                               ▼
┌─────────────────────────────────────────────────────┐
│ 7. Post-Tool Hook (Memory Update)                   │
│    - Record execution metrics                       │
│    - Update agent memory                            │
│    - Persist collision detection data               │
│    - Sync with central memory coordinator           │
└──────────────────────────────┬──────────────────────┘
                               ▼
┌─────────────────────────────────────────────────────┐
│ 8. Receipt Generation (Determinism Proof)           │
│    - Create SHA-256 hashes                          │
│    - Record timestamps (ISO 8601)                   │
│    - Persist to /receipts/{agent_id}.json           │
│    - Append to audit log                            │
└──────────────────────────────┬──────────────────────┘
                               ▼
┌─────────────────────────────────────────────────────┐
│ 9. Error Handler (Exit Code Propagation)            │
│    - Check exit code from ggen                      │
│    - Map to error type (validation/sparql/template) │
│    - Return appropriate exit code to caller         │
│    - Log error details to audit trail               │
└──────────────────────────────┬──────────────────────┘
                               ▼
┌─────────────────────────────────────────────────────┐
│ 10. Cleanup                                         │
│     - Persist agent memory                          │
│     - Close sandbox                                 │
│     - Release resources                             │
│     - Return success/failure to caller              │
└─────────────────────────────────────────────────────┘
```

### Multi-Agent Workflow Sequence

```
User Request: run-workflow multi-gen --parallel 4
        │
        ▼
┌──────────────────────────────────────┐
│ Agent Orchestrator                   │
│ ├─ Create 4 agent instances          │
│ ├─ Assign unique IDs                 │
│ └─ Schedule parallel execution       │
└──────────────┬───────────────────────┘
               ▼
    ┌──────────────────────┬──────────────────┬──────────────────┐
    ▼                      ▼                  ▼                  ▼
┌────────────┐      ┌────────────┐      ┌────────────┐      ┌────────────┐
│ Agent 1    │      │ Agent 2    │      │ Agent 3    │      │ Agent 4    │
│ (RDF)      │      │ (Template) │      │ (Validate) │      │ (Verify)   │
└────┬───────┘      └────┬───────┘      └────┬───────┘      └────┬───────┘
     │                   │                   │                   │
     ▼                   ▼                   ▼                   ▼
┌────────────┐      ┌────────────┐      ┌────────────┐      ┌────────────┐
│ μ₁-μ₅      │      │ μ₁-μ₅      │      │ μ₁-μ₅      │      │ μ₁-μ₅      │
│ Pipeline   │      │ Pipeline   │      │ Pipeline   │      │ Pipeline   │
│ 2s         │      │ 2s         │      │ 2s         │      │ 2s         │
└────┬───────┘      └────┬───────┘      └────┬───────┘      └────┬───────┘
     │                   │                   │                   │
     ├─ Receipt 1        ├─ Receipt 2        ├─ Receipt 3        ├─ Receipt 4
     ├─ Memory 1         ├─ Memory 2         ├─ Memory 3         ├─ Memory 4
     └─ Audit 1          └─ Audit 2          └─ Audit 3          └─ Audit 4
                   │                   │                   │
                   └──────┬────────────┴───────┬────────────┘
                          ▼
                  ┌────────────────────┐
                  │ Collision Detection│
                  │                    │
                  │ Check for:         │
                  │ • File overlaps    │
                  │ • Semantic overlap │
                  │ • Memory conflicts │
                  │ • Timestamp races   │
                  └────────┬───────────┘
                           ▼
                  ┌────────────────────┐
                  │ Convergence        │
                  │                    │
                  │ Apply pressure:    │
                  │ • Coverage (100%)  │
                  │ • Invariants       │
                  │ • Minimality       │
                  │ • Elegance         │
                  │                    │
                  │ Result: Optimal    │
                  │ combined output    │
                  └────────┬───────────┘
                           ▼
                  ┌────────────────────┐
                  │ Unified Receipt    │
                  │                    │
                  │ • Merge hashes     │
                  │ • Combine files    │
                  │ • Aggregate timing │
                  │ • Final audit log  │
                  └────────────────────┘
```

### Error Recovery Flow

```
ggen sync --validate_only true
        │
        ▼
   [Execution]
        │
    ┌───┴───┐
    ▼       ▼
  Error  Success
    │       │
    ▼       └─► Exit 0 ✓
┌─────────────┐
│ Exit Code 2 │ (Validation Error)
│ or 4        │ (SPARQL Error)
│ or 5        │ (Template Error)
└──────┬──────┘
       ▼
┌─────────────────────┐
│ Error Handler       │
│                     │
│ 1. Parse exit code  │
│ 2. Identify type    │
│ 3. Extract message  │
│ 4. Store in memory  │
└──────┬──────────────┘
       ▼
┌─────────────────────┐
│ Recovery Procedure  │
│                     │
│ Type: Validation    │
│ → Fix ontology      │
│ → Retry μ₁          │
│                     │
│ Type: SPARQL        │
│ → Correct query     │
│ → Retry μ₂          │
│                     │
│ Type: Template      │
│ → Fix Tera template │
│ → Retry μ₃          │
└──────┬──────────────┘
       ▼
┌─────────────────────┐
│ Retry Logic         │
│                     │
│ Attempt 1/3         │
│ Backoff: 0ms        │
│  └─ Success? Exit 0 │
│     Fail? Continue  │
│                     │
│ Attempt 2/3         │
│ Backoff: 1000ms     │
│  └─ Success? Exit 0 │
│     Fail? Continue  │
│                     │
│ Attempt 3/3         │
│ Backoff: 2000ms     │
│  └─ Success? Exit 0 │
│     Fail? Exit 1    │
└─────────────────────┘
```

## Data Flow Diagrams

### Receipt Data Flow

```
Agent Execution
     │
     ├─ Timing Data
     │  ├─ Start: ISO8601 timestamp
     │  ├─ μ₁ duration: 400ms
     │  ├─ μ₂ duration: 500ms
     │  ├─ μ₃ duration: 600ms
     │  ├─ μ₄ duration: 300ms
     │  ├─ μ₅ duration: 200ms
     │  └─ Total: 2000ms
     │
     ├─ Hash Data
     │  ├─ Manifest SHA-256
     │  ├─ Ontology SHA-256
     │  ├─ Templates SHA-256
     │  └─ File hashes (47 files)
     │
     ├─ Metadata
     │  ├─ Execution ID
     │  ├─ Agent ID
     │  ├─ Operation type
     │  ├─ Status (pass/fail)
     │  └─ Error code (if applicable)
     │
     └─ Output
        ├─ JSON Receipt
        │  └─ .ggen/receipts/{agent_id}.json
        │
        ├─ Audit Log Entry
        │  └─ audit-logs/audit.log
        │
        └─ Agent Memory
           └─ agent-memory/{agent_id}.json
```

### Memory Synchronization Flow

```
Agent A Execution           Agent B Execution
     │                            │
     ├─ Lock memory               │
     │  (exclusive access)         │
     │                            │
     ├─ Generate output           │
     │  (47 files)                │
     │                            │
     ├─ Update memory             │
     │  • Generated files list     │
     │  • Content hashes          │
     │  • Timestamp               │
     │                            │
     ├─ Release lock              │
     │  (memory available)         │
     │                            │
     └─► Timestamp: 2026-01-29    └─ Waiting on lock
         T15:30:00Z                   │
                                      ├─ Lock acquired
                                      │
                                      ├─ Read Agent A's memory
                                      │  • Check: Do our outputs overlap?
                                      │  • Decision: Avoid Agent A's files
                                      │
                                      ├─ Generate different output
                                      │  (12 alternative files)
                                      │
                                      ├─ Update memory
                                      │  • Our generated files
                                      │  • No collisions with A
                                      │  • Content hashes
                                      │
                                      └─ Release lock
```

## Performance Characteristics

### Timing Profile (per stage)

```
μ₁ (Normalize)      [████░░░░░░░░░░░░░░░] 400ms (20%)
μ₂ (Extract)        [██████░░░░░░░░░░░░░] 500ms (25%)
μ₃ (Emit)           [███████░░░░░░░░░░░░] 600ms (30%)
μ₄ (Canonicalize)   [██████░░░░░░░░░░░░░] 300ms (15%)
μ₅ (Receipt)        [████░░░░░░░░░░░░░░░] 200ms (10%)
                    ────────────────────────
                    Total:                2000ms (100%)
```

### Scalability Profile (agents)

```
Agents │ Sequential │ Parallel │ Overhead │ Efficiency
───────┼────────────┼──────────┼──────────┼────────────
   1   │    2.0s    │   2.0s   │    0ms   │  100%
   2   │    4.0s    │   2.1s   │  100ms   │   95%
   4   │    8.0s    │   2.3s   │  300ms   │   87%
   8   │   16.0s    │   2.8s   │  800ms   │   82%
  10   │   20.0s    │   3.2s   │ 1200ms   │   77%
```

**Note**: Efficiency decreases due to:
- Memory synchronization overhead
- Collision detection computations
- Convergence synthesis time
- OS scheduling contention

## Docker Container Integration

```
┌──────────────────────────────────────────────────┐
│ Claude Code Web Environment (Sandboxed)          │
├──────────────────────────────────────────────────┤
│                                                  │
│  ┌─ Network Proxy (External to Sandbox)         │
│  │  ├─ Domain Whitelist                         │
│  │  │  ├─ github.com                            │
│  │  │  ├─ crates.io                             │
│  │  │  ├─ npm.js.org                            │
│  │  │  └─ [200+ approved domains]                │
│  │  │                                            │
│  │  ├─ Request Interception                     │
│  │  │  ├─ Check domain against whitelist        │
│  │  │  ├─ Prompt user for new domains           │
│  │  │  └─ Forward approved requests             │
│  │  │                                            │
│  │  └─ Response Processing                      │
│  │     ├─ Parse response                        │
│  │     ├─ Format for agent consumption          │
│  │     └─ Cache results (optional)              │
│  │                                              │
│  └─ Unix Domain Socket                          │
│     (connect from within sandbox)                │
│                                                  │
│  ┌──────────────────────────────────────────┐  │
│  │ Sandbox Environment (Bubblewrap/Seatbelt)│  │
│  │                                           │  │
│  │  ┌─ Agent Process                         │  │
│  │  │  ├─ Isolated workspace                 │  │
│  │  │  │  ├─ /workspace/{agent_id}/          │  │
│  │  │  │  └─ NO access to /home, /root, /sys│  │
│  │  │  │                                      │  │
│  │  │  ├─ Network Access                     │  │
│  │  │  │  └─ Via proxy socket only           │  │
│  │  │  │  └─ Domain restrictions enforced    │  │
│  │  │  │                                      │  │
│  │  │  ├─ Process Restrictions                │  │
│  │  │  │  └─ Child processes inherit          │  │
│  │  │  │  └─ Cannot escape sandbox            │  │
│  │  │  │                                      │  │
│  │  │  └─ Environment Variables               │  │
│  │  │     ├─ GGEN_HOME=/workspace/{id}       │  │
│  │  │     ├─ MCP_PROXY_URL=unix:// socket    │  │
│  │  │     ├─ RUST_BACKTRACE=1                │  │
│  │  │     └─ [other config]                  │  │
│  │  │                                         │  │
│  │  └─ Optional: Docker Container             │  │
│  │     ├─ --dangerously-skip-permissions     │  │
│  │     ├─ Isolated network namespace         │  │
│  │     ├─ Restricted mounts                  │  │
│  │     └─ NO socket access to /var/run/      │  │
│  │        docker.sock (by default)            │  │
│  │                                            │  │
│  └──────────────────────────────────────────┘  │
│                                                  │
└──────────────────────────────────────────────────┘
```

## Security Boundaries

1. **Sandbox Isolation**: OS-level (Bubblewrap/Seatbelt)
2. **Network Restriction**: Domain whitelist via proxy
3. **Filesystem Isolation**: Workspace-only access
4. **Process Isolation**: Child processes inherit restrictions
5. **Docker Security**: No socket access by default

## See Also

- [README.md](README.md) - Full documentation and research findings
- [QUICKSTART.md](QUICKSTART.md) - Getting started guide
- [main.sh](main.sh) - Main orchestrator implementation
- Ggen docs: `/home/user/ggen/docs/installation/CLAUDE_CODE_WEB_GUIDE.md`
