# Drift Detection

Drift detection warns users when ontology changes make generated code stale.

## Overview

The drift detector tracks SHA256 hashes of:
- Main ontology file (`ontology.ttl`)
- Manifest file (`ggen.toml`)
- Imported ontology files
- Inference rules (SPARQL CONSTRUCT queries)

After each successful `ggen sync`, these hashes are stored in `.ggen/sync-state.json`.
Before running sync, the system compares current file hashes against the stored state
and warns users if changes are detected.

## State File Format

`.ggen/sync-state.json`:

```json
{
  "version": "1.0.0",
  "created_at": "2026-01-18T04:15:30.123Z",
  "ontology": {
    "hash": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    "timestamp": "2026-01-18T04:15:30.123Z",
    "size_bytes": 2048
  },
  "manifest": {
    "hash": "a7b9c2d1e4f5g6h7i8j9k0l1m2n3o4p5q6r7s8t9u0v1w2x3y4z5a6b7c8d9e0",
    "timestamp": "2026-01-18T04:15:30.123Z",
    "size_bytes": 512
  },
  "inference_rules": {
    "add_timestamps": "abc123def456...",
    "materialize_derived": "789ghi012jkl..."
  },
  "imports": {
    "schemas/base.ttl": {
      "hash": "def789abc123...",
      "timestamp": "2026-01-18T04:15:30.123Z",
      "size_bytes": 1024
    }
  },
  "files_synced": 12,
  "sync_duration_ms": 1234
}
```

## Usage

### Automatic (Default Behavior)

Drift detection runs automatically on every `ggen sync` command:

```bash
# First sync - creates state file
ggen sync
# Synced 12 files in 1.234s

# Modify ontology
echo "# new triple" >> ontology.ttl

# Next sync - shows drift warning
ggen sync
# ⚠️  Ontology changed since last sync (3 days ago). Run 'ggen sync' to update.
#    - Ontology changed (e3b0c442..a7b9c2d1) since last sync
# Synced 12 files in 1.234s
```

### Checking Drift Programmatically

```rust
use ggen_core::drift::{DriftDetector, DriftStatus};
use std::path::Path;

fn check_ontology_drift() -> ggen_utils::error::Result<()> {
    let detector = DriftDetector::new(Path::new(".ggen"))?;

    let status = detector.check_drift(
        Path::new("ontology.ttl"),
        Path::new("ggen.toml"),
    )?;

    match status {
        DriftStatus::Clean => {
            println!("No drift - code is up to date");
        }
        DriftStatus::Drifted { changes, days_since_sync } => {
            eprintln!("⚠️  Drift detected ({} days since sync)", days_since_sync);
            for change in changes {
                eprintln!("   - {}", change.message);
            }
        }
    }

    Ok(())
}
```

### Saving State After Sync

```rust
use ggen_core::drift::DriftDetector;
use std::path::{Path, PathBuf};

fn save_sync_state() -> ggen_utils::error::Result<()> {
    let detector = DriftDetector::new(Path::new(".ggen"))?;

    detector.save_state_with_details(
        Path::new("ontology.ttl"),
        Path::new("ggen.toml"),
        vec![PathBuf::from("imports/schema.ttl")],
        vec![
            ("add_timestamps".to_string(), "hash123".to_string()),
            ("materialize_derived".to_string(), "hash456".to_string()),
        ],
        12,  // files synced
        1234, // duration in ms
    )?;

    Ok(())
}
```

## Warning Messages

### Ontology Changed

```
⚠️  Ontology changed since last sync (3 days ago). Run 'ggen sync' to update.
   - Ontology changed (e3b0c442..a7b9c2d1) since last sync
```

### Manifest Changed

```
⚠️  Ontology changed since last sync (1 days ago). Run 'ggen sync' to update.
   - Manifest changed (abc12345..def67890) since last sync
```

### Import Changed

```
⚠️  Ontology changed since last sync (5 days ago). Run 'ggen sync' to update.
   - Import 'schemas/base.ttl' changed (111aaa..222bbb) since last sync
```

### No Previous State

```
⚠️  Ontology changed since last sync (0 days ago). Run 'ggen sync' to update.
   - No previous sync state found. Run 'ggen sync' to create baseline.
```

### Missing File

```
⚠️  Ontology changed since last sync (2 days ago). Run 'ggen sync' to update.
   - Missing file 'ontology.ttl'
```

## Performance

Drift detection is designed to be fast and non-blocking:

- **Target**: < 100ms per check
- **Method**: SHA256 hashing with file metadata caching
- **Impact**: Minimal - only hashes files that exist
- **Failures**: Silent - never blocks `ggen sync` execution

## Integration with SyncExecutor

The `SyncExecutor` automatically:

1. **Before sync**: Checks for drift and warns users (non-blocking)
2. **After sync**: Saves updated state with new hashes

```rust
// In SyncExecutor::execute()
self.check_and_warn_drift(base_path);  // Before sync

// ... run sync pipeline ...

self.save_drift_state(base_path, manifest_data, files_synced, duration);  // After sync
```

## Configuration

Drift detection is always enabled but can be suppressed in specific modes:

- **Validate-only mode**: No drift check (not running actual sync)
- **Watch mode**: No drift check (continuous regeneration)
- **Normal mode**: Drift check enabled

## Error Handling

All drift detection errors are handled gracefully:

- **Detector creation fails**: Silent skip
- **State file missing**: Treated as "no state" (not an error)
- **Hash calculation fails**: Silent skip
- **State save fails**: Warning in verbose mode only

**Never fails the sync operation**.

## Testing

Run drift detection tests:

```bash
cargo test --package ggen-core drift
```

Key test scenarios:
- Clean state (no drift)
- Ontology changed
- Manifest changed
- Import changed
- Missing files
- No previous state
- State save/load roundtrip

## Files

- `crates/ggen-core/src/drift/mod.rs` - Module definition and exports
- `crates/ggen-core/src/drift/detector.rs` - Drift detection logic
- `crates/ggen-core/src/drift/sync_state.rs` - State file structure
- `crates/ggen-core/src/codegen/executor.rs` - Integration with SyncExecutor

## Constitutional Requirements

- No `unwrap/expect` in production code
- `Result<T,E>` throughout
- Non-blocking warnings (never fail commands)
- Performance: drift check <100ms
- Silent errors (graceful degradation)

## Future Enhancements

Potential improvements:

1. **File timestamps**: Skip hashing if mtime unchanged
2. **Parallel hashing**: Hash multiple files concurrently
3. **Incremental updates**: Only hash changed files
4. **Configurable warnings**: Allow users to suppress specific warnings
5. **Drift reports**: Generate detailed drift analysis reports
6. **Auto-sync option**: `--auto-sync-on-drift` flag
