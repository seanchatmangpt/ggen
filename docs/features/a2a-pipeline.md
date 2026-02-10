# A2A-RS μ Pipeline Documentation

## Overview

The A2A-RS integration uses a five-stage pipeline (μ₁ through μ₅) to generate Rust code from RDF ontologies. The `ggen sync` command orchestrates this pipeline with progress reporting for each stage.

## Pipeline Stages

### μ₁: CONSTRUCT (Normalization)

**Purpose**: Normalize RDF ontology from source format to A2A structure

**Input**: 
- `.specify/specs/014-a2a-integration/a2a-ontology.ttl` (raw RDF)
- Various entity definitions with potentially different schemas

**Query**: `crates/ggen-core/queries/a2a/construct-agents.rq`

**Output**: 
- Normalized A2A RDF with `a2a:` prefix
- Unified agent, skill, transport, and task definitions

**Actions**:
1. Load primary ontology from `a2a-ontology.ttl`
2. Execute CONSTRUCT query to transform and normalize
3. Materialize new triples into graph
4. Count triples added

### μ₂: SELECT (Extraction)

**Purpose**: Extract bindings for each module from normalized RDF

**Queries**:
- `extract-agents.rq` → agent bindings
- `extract-messages.rq` → message bindings  
- `extract-tasks.rq` → task bindings
- `extract-transports.rq` → transport bindings
- `extract-skills.rq` → skill bindings

**Output**: SPARQL result bindings (rows with variable bindings)

**Actions**:
1. Execute each SELECT query
2. Collect result bindings
3. Clean RDF serialization syntax (strip brackets, quotes, XSD types)
4. Return as BTreeMap for template rendering

### μ₃: Tera (Code Generation)

**Purpose**: Generate Rust code from templates using extracted bindings

**Templates**: `crates/ggen-core/templates/a2a/*.tera`
- `agent.rs.tera` → `crates/a2a-generated/src/agent.rs`
- `message.rs.tera` → `crates/a2a-generated/src/message.rs`
- `task.rs.tera` → `crates/a2a-generated/src/task.rs`
- `transport.rs.tera` → `crates/a2a-generated/src/transport.rs`
- `skill.rs.tera` → `crates/a2a-generated/src/skill.rs`
- `lib.rs.tera` → `crates/a2a-generated/src/lib.rs`

**Output**: Generated Rust source files

**Actions**:
1. Load Tera template
2. For each SPARQL result row:
   - Build context from bindings
   - Render template with context
   - Expand output path pattern
   - Validate output (not empty, under size limit, no path traversal)
3. Write files atomically with rollback on failure

### μ₄: Canonicalize (Formatting)

**Purpose**: Format and organize generated code

**Actions**:
1. Run `rustfmt` on generated files
2. Organize imports
3. Verify compilation (optional)
4. Apply any code organization rules

**Output**: Formatted, ready-to-compile code

### μ₅: Receipt (Verification)

**Purpose**: Generate cryptographic receipt for reproducibility verification

**Output**: `.ggen/receipts/a2a-{timestamp}.json`

**Contains**:
- SHA256 hashes of all generated files
- Input ontology hash
- Query hashes
- Timestamp
- Execution metadata
- ggen version

## CLI Usage

### Basic A2A Sync

```bash
# Full A2A μ₁-μ₅ pipeline with receipt
ggen sync --audit

# Preview without writing
ggen sync --dry-run

# Custom output directory
ggen sync --output-dir crates/a2a-generated/src/
```

### Stage-Specific Execution

```bash
# Run only μ₃ (template generation)
ggen sync --stage μ₃

# Run only μ₂ (extraction)
ggen sync --stage μ₂
```

### Custom Ontology

```bash
# Override ontology path
ggen sync --ontology .specify/specs/014-a2a-integration/a2a-ontology.ttl --audit
```

## Progress Reporting

When running A2A sync with `--verbose`, progress is reported for each μ stage:

```
[μ₁/5] CONSTRUCT: Normalizing ontology...
       Loaded 847 triples from a2a-ontology.ttl
       +124 triples from construct-agents.rq
[μ₂/5] SELECT: Extracting bindings...
       Agents: 8 bindings
       Messages: 12 bindings
       Tasks: 15 bindings
       Transports: 3 bindings
       Skills: 24 bindings
[μ₃/5] Tera: Generating code...
       agent.rs (2.4 KB)
       message.rs (3.1 KB)
       task.rs (2.8 KB)
       transport.rs (1.2 KB)
       skill.rs (4.5 KB)
       lib.rs (1.8 KB)
[μ₄/5] Canonicalizing: Formatting code...
       Running rustfmt...
       Verifying compilation...
[μ₅/5] Receipt: Generating verification...
       Receipt: .ggen/receipts/a2a-20250208-143022.json
       Ontology hash: a3f2e1b4...
       Total: 6 files, 15.8 KB, 2.34s
```

## Error Handling

Each stage has specific error codes:

| Stage | Error Code | Description |
|-------|------------|-------------|
| μ₁ | E0021 | Ontology parse error |
| μ₁ | E0022 | CONSTRUCT query syntax error |
| μ₂ | E0023 | SELECT query execution error |
| μ₃ | E0024 | Template parse error |
| μ₃ | E0025 | Template render error |
| μ₃ | E0026 | File write error |
| μ₄ | E0027 | Format error |
| μ₅ | E0028 | Receipt generation error |

## Configuration

The A2A pipeline is configured in `ggen.toml`:

```toml
[generation]
rules = [
    # μ₂ queries and μ₃ templates
    { name = "a2a-agents", query = "...", template = "...", output_file = "..." },
    { name = "a2a-messages", query = "...", template = "...", output_file = "..." },
    # ... more rules
]
```

## Integration Points

### SyncOptions (ggen-core/src/codegen/executor.rs)

```rust
pub struct SyncOptions {
    // ... existing fields ...
    
    /// Run specific μ stage only (μ₁, μ₂, μ₃, μ₄, μ₅)
    pub a2a_stage: Option<String>,
    
    /// Override ontology path for A2A generation
    pub ontology_path: Option<PathBuf>,
}
```

### Sync Command (ggen-cli/src/cmds/sync.rs)

The `sync` verb function accepts additional parameters:
- `stage`: Run specific μ stage only
- `ontology`: Override ontology path

## Testing

```bash
# Run A2A integration tests
cargo test --package a2a-generated

# Verify receipt format
cat .ggen/receipts/a2a-*.json | jq '.'
```

## See Also

- `crates/ggen-core/queries/a2a/` - SPARQL queries
- `crates/ggen-core/templates/a2a/` - Tera templates
- `crates/a2a-generated/src/` - Generated output
- `.specify/specs/014-a2a-integration/` - RDF ontology source
