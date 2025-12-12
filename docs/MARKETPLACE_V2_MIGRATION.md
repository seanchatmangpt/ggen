# ggen-marketplace Migration Guide

## Overview

This guide provides comprehensive instructions for migrating from ggen-marketplace v1 to the RDF-backed v2 system. The migration preserves all package data while enabling advanced semantic search and SPARQL query capabilities.

### Why Migrate to v2?

| Feature | v1 | v2 |
|---------|----|----|
| Data Model | In-memory HashMap | RDF Triplestore (oxigraph) |
| Search | Simple text matching | Semantic SPARQL queries |
| Query Language | None | Full SPARQL 1.1 support |
| Relationships | Flat dependencies | Rich semantic graphs |
| Version History | Limited | Complete RDF fact history |
| Performance | O(n) linear search | Indexed SPARQL queries |
| Extensibility | Fixed schema | Flexible ontology |

### Key Differences

```
v1 Architecture:
+------------------+
| Registry HashMap |  <-- Direct key-value access
+------------------+
        |
        v
+------------------+
|   Package Data   |
+------------------+

v2 Architecture:
+------------------+
|  SPARQL Queries  |  <-- Semantic queries
+------------------+
        |
        v
+------------------+
|  RDF Triplestore |  <-- Oxigraph semantic store
|    (Oxigraph)    |
+------------------+
        |
        v
+------------------+
| Package as RDF   |  <-- Rich relationships
|     Triples      |
+------------------+
```

### Timeline and Effort Estimates

| Phase | Duration | Effort |
|-------|----------|--------|
| Preparation | 1-2 hours | Read docs, backup data |
| Migration | 5-30 minutes | Depends on package count |
| Verification | 30-60 minutes | Test critical workflows |
| Rollback (if needed) | 10 minutes | Restore backup |

---

## Prerequisites

### System Requirements

- Rust 1.70+ (stable toolchain)
- 100MB+ free disk space for RDF store
- ggen-marketplace crate installed

### Dependency Check

```toml
# Cargo.toml
[dependencies]
ggen-marketplace = "2.0"
oxigraph = "0.3"
tokio = { version = "1.0", features = ["full"] }
```

### Pre-Migration Checklist

- [ ] Backup v1 package data
- [ ] Document current package counts
- [ ] Note any custom search configurations
- [ ] Schedule maintenance window (recommended)
- [ ] Test migration on staging environment first

---

## Step-by-Step Migration

### Step 1: Backup v1 Packages

```rust
use ggen_marketplace::prelude::*;

// Export all v1 packages to JSON backup
async fn backup_v1_packages(v1_registry: &Registry) -> Vec<Package> {
    let packages = v1_registry.all_packages().await
        .expect("Failed to retrieve v1 packages");

    // Save to JSON file for safety
    let json = serde_json::to_string_pretty(&packages)
        .expect("Failed to serialize packages");

    std::fs::write("v1_backup.json", json)
        .expect("Failed to write backup file");

    println!("Backed up {} packages to v1_backup.json", packages.len());
    packages
}
```

### Step 2: Initialize v2 RDF Registry

```rust
use ggen_marketplace::{RdfRegistry, migration::MigrationCoordinator};
use std::sync::Arc;

// Create the new RDF-backed registry
fn create_v2_registry() -> Arc<RdfRegistry> {
    let registry = RdfRegistry::new();
    println!("Initialized RDF registry with ontology");
    Arc::new(registry)
}
```

### Step 3: Run Migration

```rust
use ggen_marketplace::migration::{MigrationCoordinator, MigrationReport};

async fn run_migration(
    v1_packages: Vec<Package>,
    v2_registry: Arc<RdfRegistry>,
) -> MigrationReport {
    let coordinator = MigrationCoordinator::new(v2_registry);

    // Full migration (all packages)
    let report = coordinator.migrate_packages(v1_packages).await
        .expect("Migration failed");

    println!("{}", report);

    if report.is_successful() {
        println!("Migration completed successfully!");
    } else {
        println!("Migration completed with {} errors", report.errors.len());
        for error in &report.errors {
            println!("  - {}", error);
        }
    }

    report
}
```

### Step 4: Verify Data Integrity

```rust
use ggen_marketplace::migration::VerificationReport;

async fn verify_migration(
    v1_packages: Vec<Package>,
    coordinator: &MigrationCoordinator,
) -> VerificationReport {
    let report = coordinator.verify_migration(v1_packages).await
        .expect("Verification failed");

    println!("{}", report);

    if report.is_valid() {
        println!("All packages verified successfully!");
    } else {
        println!("Verification found {} mismatches:", report.mismatches.len());
        for mismatch in &report.mismatches {
            println!("  - {}", mismatch);
        }
    }

    report
}
```

### Step 5: Test Critical Workflows

```rust
use ggen_marketplace::{SparqlSearchEngine, Installer};
use ggen_marketplace::traits::AsyncRepository;

async fn test_critical_workflows(registry: Arc<RdfRegistry>) {
    // Test 1: Package retrieval
    let test_pkg_id = PackageId::new("test-package").unwrap();
    match registry.get_package(&test_pkg_id).await {
        Ok(pkg) => println!("Package retrieval: OK - {}", pkg.metadata.name),
        Err(e) => println!("Package retrieval: FAILED - {}", e),
    }

    // Test 2: SPARQL search
    let search = SparqlSearchEngine::new(Arc::clone(&registry.store));
    match search.all_packages() {
        Ok(results) => println!("SPARQL search: OK - {} packages found", results.len()),
        Err(e) => println!("SPARQL search: FAILED - {}", e),
    }

    // Test 3: Version listing
    match registry.list_versions(&test_pkg_id).await {
        Ok(versions) => println!("Version listing: OK - {} versions", versions.len()),
        Err(e) => println!("Version listing: FAILED - {}", e),
    }

    // Test 4: Installer dry-run
    let installer = Installer::new((*registry).clone());
    let manifest = installer
        .create_manifest(vec![test_pkg_id], "/tmp/test".to_string())
        .await;
    match manifest {
        Ok(m) => println!("Installer: OK - {} dependencies resolved", m.dependencies.len()),
        Err(e) => println!("Installer: OK (empty) - {}", e),
    }
}
```

### Step 6: Go Live

```rust
// Complete migration script
async fn complete_migration() -> Result<(), Box<dyn std::error::Error>> {
    // Step 1: Backup
    let v1_registry = Registry::new(1000).await;
    let v1_packages = backup_v1_packages(&v1_registry).await;

    // Step 2: Initialize v2
    let v2_registry = create_v2_registry();

    // Step 3: Migrate
    let coordinator = MigrationCoordinator::new(Arc::clone(&v2_registry));
    let migration_report = run_migration(v1_packages.clone(), Arc::clone(&v2_registry)).await;

    if !migration_report.is_successful() {
        println!("Migration failed! Run rollback procedure.");
        return Err("Migration failed".into());
    }

    // Step 4: Verify
    let verification_report = verify_migration(v1_packages, &coordinator).await;

    if !verification_report.is_valid() {
        println!("Verification failed! Run rollback procedure.");
        return Err("Verification failed".into());
    }

    // Step 5: Test workflows
    test_critical_workflows(Arc::clone(&v2_registry)).await;

    // Step 6: Mark migration complete
    println!("=== MIGRATION COMPLETE ===");
    println!("v2 RDF Registry is now the primary data store");

    Ok(())
}
```

---

## Incremental Migration

For large registries, use incremental migration to minimize downtime:

```rust
async fn incremental_migration(
    v1_packages: Vec<Package>,
    coordinator: &MigrationCoordinator,
) -> MigrationReport {
    // Only migrate new/updated packages (skips existing)
    let report = coordinator.incremental_migrate(v1_packages).await
        .expect("Incremental migration failed");

    println!("Incremental: {} migrated, {} skipped",
        report.migrated_packages,
        report.skipped_packages
    );

    report
}
```

---

## Rollback Procedures

### When to Rollback

- Migration success rate < 95%
- Critical workflows failing
- Performance degradation > 50%
- Data integrity issues detected

### Rollback Execution

```rust
// Restore from JSON backup
async fn rollback_to_v1() -> Result<(), Box<dyn std::error::Error>> {
    // Read backup file
    let backup_json = std::fs::read_to_string("v1_backup.json")
        .expect("Failed to read backup file");

    let packages: Vec<Package> = serde_json::from_str(&backup_json)
        .expect("Failed to parse backup JSON");

    // Re-create v1 registry
    let v1_registry = Registry::new(packages.len() + 100).await;

    // Restore packages
    for package in packages {
        v1_registry.publish(package).await
            .expect("Failed to restore package");
    }

    println!("Rollback complete. v1 registry restored.");
    Ok(())
}
```

### Data Recovery Procedures

```rust
// Recover specific packages from RDF
async fn recover_packages_from_rdf(
    registry: &RdfRegistry,
    package_ids: Vec<PackageId>,
) -> Vec<Package> {
    let mut recovered = Vec::new();

    for id in package_ids {
        match registry.get_package(&id).await {
            Ok(pkg) => recovered.push(pkg),
            Err(e) => println!("Could not recover {}: {}", id, e),
        }
    }

    recovered
}
```

### Verification Checklist

After rollback:

- [ ] All packages accessible
- [ ] Package counts match pre-migration
- [ ] Search functionality works
- [ ] Installation workflows succeed
- [ ] No data corruption

---

## Troubleshooting

### Common Migration Issues

#### Issue: "Package not found" after migration

**Cause**: Package ID normalization differences between v1 and v2.

**Solution**:
```rust
// v2 normalizes all package IDs to lowercase
let normalized_id = PackageId::new(original_id.to_lowercase())?;
```

#### Issue: "SPARQL query failed"

**Cause**: Malformed RDF triples from incomplete migration.

**Solution**:
```rust
// Re-run migration for specific package
let package = v1_registry.get_package(&id).await?;
v2_registry.insert_package_rdf(&package).await?;
```

#### Issue: Version mismatch in verification

**Cause**: Version string normalization.

**Solution**:
```rust
// v2 removes 'v' prefix: "v1.0.0" becomes "1.0.0"
let normalized = PackageVersion::new(version)?;
```

#### Issue: Missing dependencies

**Cause**: Circular dependency references not fully resolved.

**Solution**:
```rust
// Use consistency checker to identify issues
let checker = ConsistencyChecker::new(Arc::clone(&v2_registry));
let report = checker.periodic_check(v1_packages).await?;
for inconsistent in report.inconsistent_packages {
    println!("Fix required: {} - {:?}",
        inconsistent.package_id,
        inconsistent.differences
    );
}
```

### Performance Issues

#### Slow SPARQL Queries

**Solution**: Ensure proper indexing and query optimization:
```rust
// Use specific queries instead of SELECT *
let query = Queries::search_by_name("package-name"); // Optimized
// Not: "SELECT * WHERE { ?s ?p ?o }" // Full scan
```

#### High Memory Usage

**Solution**: Use batch operations:
```rust
// Process in batches of 50
let batch_size = 50;
for chunk in packages.chunks(batch_size) {
    registry.batch_insert_packages(chunk.to_vec()).await?;
}
```

---

## FAQ

### Q: Can I run v1 and v2 simultaneously?

**A**: Yes, during migration you can run both. Use ConsistencyChecker for periodic sync verification.

### Q: Will my existing integrations break?

**A**: The `AsyncRepository` trait is identical. Code using the trait will work unchanged.

### Q: How do I migrate custom search logic?

**A**: Convert to SPARQL queries. See the Search API Guide for examples.

### Q: What happens to package download history?

**A**: Download counts are preserved in the `downloads` RDF property.

### Q: Can I export v2 data back to v1 format?

**A**: Yes, use `registry.all_packages().await` to get Package structs compatible with v1.

---

## Support

### Documentation
- [API Reference](./MARKETPLACE_V2_API.md)
- [Architecture Guide](./MARKETPLACE_V2_ARCHITECTURE.md)
- [SPARQL Query Guide](./GGEN_V3_SPARQL_PATTERNS_AND_OPTIMIZATION.md)

### Contact
- GitHub Issues: [ggen/issues](https://github.com/seanchatmangpt/ggen/issues)
- Label migration issues with `marketplace-v2-migration`

---

## Appendix: Migration Report Format

```rust
/// Migration report structure
#[derive(Debug, Clone)]
pub struct MigrationReport {
    /// Total packages to migrate
    pub total_packages: usize,
    /// Successfully migrated packages
    pub migrated_packages: usize,
    /// Skipped packages (already exist)
    pub skipped_packages: usize,
    /// Migration errors
    pub errors: Vec<String>,
}

impl MigrationReport {
    /// Check if migration was successful (100% migrated, no errors)
    pub fn is_successful(&self) -> bool {
        self.errors.is_empty() && self.migrated_packages == self.total_packages
    }

    /// Get success rate (0.0 - 1.0)
    pub fn success_rate(&self) -> f64 {
        if self.total_packages == 0 { 1.0 }
        else { self.migrated_packages as f64 / self.total_packages as f64 }
    }
}
```

## Version Compatibility

| v1 Version | v2 Version | Migration Support |
|------------|------------|-------------------|
| 1.0.x | 2.0.x | Full |
| 1.1.x | 2.0.x | Full |
| 1.2.x | 2.1.x | Full + Incremental |
