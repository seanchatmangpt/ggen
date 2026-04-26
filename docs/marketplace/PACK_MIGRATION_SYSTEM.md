# Pack Migration System - Implementation Guide

## Overview

The pack migration system provides comprehensive version upgrade functionality for managing package version migrations with compatibility checking, upgrade path computation, and rollback support.

## Core Components

### 1. `Migrator` - Main Migration Engine

The `Migrator` struct orchestrates all migration operations:

```rust
pub struct Migrator {
    upgrade_graph: HashMap<PackageVersion, Vec<UpgradeEdge>>,
    rollback_states: HashMap<String, Package>,
}
```

**Key Methods:**
- `new()` - Create empty migrator
- `add_upgrade_edge()` - Add single upgrade transition
- `add_linear_path()` - Define linear upgrade path (v1→v2→v3)
- `compute_upgrade_path()` - Find upgrade path using BFS
- `migrate()` - Execute migration with rollback support

### 2. `UpgradeEdge` - Upgrade Transition

Represents a directed edge in the upgrade graph:

```rust
pub struct UpgradeEdge {
    pub from: PackageVersion,
    pub to: PackageVersion,
    pub is_direct: bool,
}
```

**Usage:**
```rust
let edge = UpgradeEdge::new(v1, v2);  // Direct upgrade
let edge = UpgradeEdge::new(v1, v3).indirect();  // Indirect (requires intermediate)
```

## Upgrade Path Computation

### Linear Upgrade Paths

For simple sequential upgrades (v1 → v2 → v3):

```rust
let mut migrator = Migrator::new();
let v1 = PackageVersion::new("1.0.0")?;
let v2 = PackageVersion::new("2.0.0")?;
let v3 = PackageVersion::new("3.0.0")?;

migrator.add_linear_path(&[v1.clone(), v2.clone(), v3.clone()]);

// Compute path from v1 to v3
let path = migrator.compute_upgrade_path(&v1, &v3)?;
// path = [v1, v2, v3]
```

### Branching Upgrade Paths

For complex upgrade graphs with multiple paths:

```rust
let mut migrator = Migrator::new();

// v1 can upgrade to both v2a and v2b
migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2a.clone()));
migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2b.clone()));

// Query available targets
let targets = migrator.get_upgrade_targets(&v1);
// targets = [v2a, v2b]
```

### Path Finding Algorithm

Uses BFS (Breadth-First Search) to find shortest path:

1. Start from source version
2. Explore all reachable versions level-by-level
3. Return first path to target version
4. Returns error if no path exists

**Complexity:** O(V + E) where V = versions, E = edges

## Migration Execution

### Basic Migration

```rust
let mut migrator = Migrator::new();
migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2.clone()));

let package = Package { /* v1 package */ };

let result = migrator.migrate(package, &v1, &v2)?;
// result.latest_version == v2
```

### Multi-Step Migration

```rust
migrator.add_linear_path(&[v1.clone(), v2.clone(), v3.clone()]);

let result = migrator.migrate(package, &v1, &v3)?;
// Automatically executes: v1→v2, then v2→v3
```

## Rollback Support

The migration system automatically saves state before execution:

```rust
let mut migrator = Migrator::new();
migrator.add_upgrade_edge(UpgradeEdge::new(v1.clone(), v2.clone()));

let package = original_package.clone();
match migrator.migrate(package.clone(), &v1, &v2) {
    Ok(migrated) => {
        // Use migrated package
    }
    Err(e) => {
        // Error includes rollback information
        // Original state is preserved in migrator.rollback_states
    }
}
```

## Compatibility Checking

### Direct Upgrade Check

```rust
if migrator.can_upgrade_directly(&v1, &v2) {
    // Direct upgrade possible
}
```

### Any Path Check

```rust
if migrator.can_upgrade(&v1, &v3) {
    // Path exists (may require intermediate steps)
}
```

### Compatibility Matrix

Generate full upgrade capability matrix:

```rust
let matrix = migrator.get_compatibility_matrix();
// HashMap<PackageVersion, Vec<PackageVersion>>
// For each version, lists all possible upgrade targets

for (from, targets) in &matrix {
    println!("{} can upgrade to {:?}", from, targets);
}
```

## Version Management

### All Registered Versions

```rust
let versions = migrator.get_all_versions();
// Returns sorted list of all versions in graph
```

### Upgrade Targets

```rust
let targets = migrator.get_upgrade_targets(&v1);
// Returns direct upgrade targets from version
```

## Test Coverage

### Unit Tests (in `crates/ggen-marketplace/src/migration.rs`)

- `test_upgrade_edge_creation` - Edge structure
- `test_migrator_creation` - Initialization
- `test_linear_upgrade_path` - Sequential upgrades
- `test_compute_upgrade_path_*` - Path finding algorithms
- `test_direct_upgrade_check` - Compatibility checks
- `test_branching_upgrade_paths` - Graph traversal
- `test_migrate_linear_upgrade` - Migration execution
- `test_migrator_default` - Default construction

### Integration Tests (in `crates/ggen-marketplace/tests/pack_migration_test.rs`)

- `linear_upgrade_path_v1_to_v3` - Multi-step migration
- `direct_upgrade_single_step` - Single step upgrade
- `no_upgrade_path_available` - Error handling
- `branching_upgrade_paths` - Complex graphs
- `migrate_package_to_new_version` - Real package migration
- `migrate_through_multiple_versions` - Multi-step execution
- `compatibility_matrix_generation` - Matrix calculation
- `complex_upgrade_graph` - Real-world scenarios
- `upgrade_preserves_package_metadata` - Metadata preservation

## Real-World Example

```rust
use ggen_marketplace::migration::{Migrator, UpgradeEdge};

// Define upgrade paths for a package
let mut migrator = Migrator::new();

// Linear upgrade path: v1.0.0 → v2.0.0 → v3.0.0
migrator.add_linear_path(&[
    PackageVersion::new("1.0.0")?,
    PackageVersion::new("2.0.0")?,
    PackageVersion::new("3.0.0")?,
]);

// Get a package currently at v1.0.0
let mut package = registry.get_package(&pkg_id).await?;
assert_eq!(package.latest_version, PackageVersion::new("1.0.0")?);

// Compute and execute upgrade to v3.0.0
let upgrade_path = migrator.compute_upgrade_path(
    &package.latest_version,
    &PackageVersion::new("3.0.0")?
)?;
println!("Upgrade path: {:?}", upgrade_path);

// Execute migration
let migrated_package = migrator.migrate(
    package,
    &PackageVersion::new("1.0.0")?,
    &PackageVersion::new("3.0.0")?
)?;

assert_eq!(migrated_package.latest_version, PackageVersion::new("3.0.0")?);
```

## Architecture Decisions

### Why BFS for Path Finding?

1. **Shortest Path** - Finds minimum steps between versions
2. **Optimal Performance** - O(V+E) complexity
3. **Completeness** - Guarantees finding path if one exists
4. **Simplicity** - Easy to understand and maintain

### Why Rollback States?

1. **Safety** - Automatic state preservation on error
2. **Transparency** - Error messages include rollback info
3. **Recoverability** - Users can inspect state before/after

### Why Upgrade Graph as HashMap?

1. **Efficient Lookup** - O(1) edge retrieval
2. **Flexible Structure** - Easy to add/remove edges
3. **Memory Efficient** - Only stores defined transitions

## Extending the System

### Adding Version-Specific Transforms

The `apply_migration_transform()` method can be extended for real version-specific logic:

```rust
fn apply_migration_transform(
    &self, package: &mut Package, from: &PackageVersion, to: &PackageVersion,
) -> Result<()> {
    match (from.as_str(), to.as_str()) {
        ("1.0.0", "2.0.0") => {
            // v1→v2: Add new field with default
            // package.metadata.new_field = default_value;
        }
        ("2.0.0", "3.0.0") => {
            // v2→v3: Rename field, transform structure
            // package.metadata.old_field → package.metadata.new_field
        }
        _ => {}
    }
    Ok(())
}
```

### Custom Path Finding Algorithms

Replace BFS with A* or Dijkstra for weighted paths:

```rust
// Weight edges by compatibility risk
pub struct WeightedUpgradeEdge {
    pub edge: UpgradeEdge,
    pub risk_level: u32,  // 0=safe, 100=breaking
}
```

## Performance Characteristics

| Operation | Time Complexity | Space Complexity |
|-----------|-----------------|------------------|
| `add_upgrade_edge()` | O(1) average | O(1) |
| `compute_upgrade_path()` | O(V + E) | O(V) |
| `migrate()` | O(path_length) | O(package_size) |
| `get_compatibility_matrix()` | O(V × E) | O(V × max_targets) |
| `can_upgrade()` | O(V + E) | O(V) |

## Error Handling

### Path Not Found

```rust
match migrator.compute_upgrade_path(&v1, &v2) {
    Err(e) => {
        // e.message = "No upgrade path found from 1.0.0 to 2.0.0"
    }
    Ok(path) => { /* ... */ }
}
```

### Migration Failure with Rollback

```rust
match migrator.migrate(package, &v1, &v3) {
    Err(e) => {
        // Error message includes rollback status
        // Original state preserved in migrator.rollback_states
    }
    Ok(migrated) => { /* ... */ }
}
```

## Integration with Registry

```rust
pub async fn upgrade_package(
    registry: &RdfRegistry,
    package_id: &PackageId,
    target_version: &PackageVersion,
) -> Result<Package> {
    let mut migrator = build_upgrade_graph();
    let mut package = registry.get_package(package_id).await?;
    
    let upgraded = migrator.migrate(
        package,
        &package.latest_version,
        target_version
    )?;
    
    registry.update_package(&upgraded).await?;
    Ok(upgraded)
}
```

## Testing Checklist

- [ ] Linear upgrade paths compile
- [ ] Branching paths compute correctly
- [ ] BFS path finding is optimal
- [ ] Rollback state is saved
- [ ] Migration updates version
- [ ] Metadata is preserved
- [ ] Error paths are handled
- [ ] Complex graphs work
- [ ] Default migrator is empty
- [ ] All tests pass with real Package objects

## See Also

- `crates/ggen-marketplace/src/migration.rs` - Implementation
- `crates/ggen-marketplace/tests/pack_migration_test.rs` - Integration tests
- `crates/ggen-marketplace/src/models.rs` - Package/Version types
