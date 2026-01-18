<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [📝 Test Consolidation: Detailed Pseudocode](#-test-consolidation-detailed-pseudocode)
  - [Module 1: Consolidated Core Tests](#module-1-consolidated-core-tests)
    - [File: `crates/ggen-core/tests/consolidated_core_tests.rs`](#file-cratesggen-coretestsconsolidated_core_testsrs)
  - [Module 2: Consolidated Lifecycle Tests](#module-2-consolidated-lifecycle-tests)
    - [File: `crates/ggen-core/tests/consolidated_lifecycle_tests.rs`](#file-cratesggen-coretestsconsolidated_lifecycle_testsrs)
  - [Module 3: Consolidated Swarm Tests](#module-3-consolidated-swarm-tests)
    - [File: `crates/ggen-core/tests/consolidated_swarm_tests.rs`](#file-cratesggen-coretestsconsolidated_swarm_testsrs)
  - [Module 4: Consolidated Semantic Tests](#module-4-consolidated-semantic-tests)
    - [File: `crates/ggen-core/tests/consolidated_semantic_tests.rs`](#file-cratesggen-coretestsconsolidated_semantic_testsrs)
  - [Summary: 80/20 Test Coverage](#summary-8020-test-coverage)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 📝 Test Consolidation: Detailed Pseudocode

## Module 1: Consolidated Core Tests

### File: `crates/ggen-core/tests/consolidated_core_tests.rs`

```pseudocode
// ================================================================
// UNIT TESTS: Package Validation
// ================================================================

fn test_package_id_validation()
  ARRANGE:
    valid_ids = ["ggen-core", "my-package", "pkg_v2"]
    invalid_ids = ["", "-invalid", "invalid-", "invalid package"]

  ACT & ASSERT:
    FOR EACH valid_id in valid_ids:
      ASSERT PackageId::new(valid_id).is_ok()

    FOR EACH invalid_id in invalid_ids:
      ASSERT PackageId::new(invalid_id).is_err()

fn test_package_version_validation()
  ARRANGE:
    valid_versions = ["1.0.0", "v1.0.0", "1.0.0-alpha", "2.5.3+build"]
    invalid_versions = ["1.0", "abc", "v1.0"]

  ACT & ASSERT:
    FOR EACH valid_version in valid_versions:
      ASSERT PackageVersion::new(valid_version).is_ok()

    FOR EACH invalid_version in invalid_versions:
      ASSERT PackageVersion::new(invalid_version).is_err()

fn test_quality_score_calculation()
  ARRANGE:
    test_cases = [
      (95, true),   // Production ready
      (85, false),  // Needs improvement
      (50, false),  // Not ready
    ]

  ACT & ASSERT:
    FOR EACH (score, expected_ready) in test_cases:
      score_obj = QualityScore::new(score).unwrap()
      ASSERT score_obj.is_production_ready() == expected_ready

fn test_dependency_graph_validation()
  ARRANGE:
    pkg_a = Package::new("pkg-a", "1.0.0")
    pkg_b = Package::new("pkg-b", "1.0.0")
    pkg_c = Package::new("pkg-c", "1.0.0")

    // Create graph: A → B → C (valid)
    graph = DependencyGraph::new()
      .add_edge(pkg_a, pkg_b)
      .add_edge(pkg_b, pkg_c)

  ACT:
    cycles = graph.find_cycles()
    order = graph.topological_sort()

  ASSERT:
    cycles.is_empty()
    order.len() == 3

// ================================================================
// INTEGRATION TESTS: Marketplace Operations
// ================================================================

fn test_package_crud_operations()
  ARRANGE:
    registry = Registry::new()
    pkg = PackageBuilder::new("test-pkg")
      .version("1.0.0")
      .description("Test package")
      .build()

  ACT:
    // Create
    created = registry.insert(pkg.clone()).await

    // Read
    retrieved = registry.get("test-pkg").await

    // Update
    updated = retrieved.with_version("1.1.0")
    registry.update(updated).await

    // Delete
    registry.delete("test-pkg").await

  ASSERT:
    created.is_ok()
    retrieved.id == "test-pkg"
    registry.get("test-pkg").await.is_err()

fn test_dependency_resolution()
  ARRANGE:
    registry = Registry::with_packages([
      ("pkg-a", "1.0.0", []),
      ("pkg-b", "1.0.0", ["pkg-a@1.0"]),
      ("pkg-c", "1.0.0", ["pkg-b@1.0"]),
    ])
    installer = Installer::new(registry)

  ACT:
    resolved = installer.resolve_dependencies("pkg-c", "1.0.0").await

  ASSERT:
    resolved.contains(("pkg-a", "1.0.0"))
    resolved.contains(("pkg-b", "1.0.0"))
    resolved.contains(("pkg-c", "1.0.0"))

fn test_circular_dependency_detection()
  ARRANGE:
    registry = Registry::with_packages([
      ("pkg-a", "1.0.0", ["pkg-b@1.0"]),
      ("pkg-b", "1.0.0", ["pkg-c@1.0"]),
      ("pkg-c", "1.0.0", ["pkg-a@1.0"]),
    ])

  ACT:
    result = Validator::check_dependencies(registry)

  ASSERT:
    result.has_cycle()
    result.cycle_packages == ["pkg-a", "pkg-b", "pkg-c"]

fn test_registry_search_queries()
  ARRANGE:
    registry = Registry::with_packages([
      ("ggen-core", "1.0.0", keywords: ["core", "framework"]),
      ("ggen-cli", "1.0.0", keywords: ["cli", "command"]),
      ("ggen-web", "1.0.0", keywords: ["web", "framework"]),
    ])

  ACT:
    results_core = registry.search("framework")
    results_cli = registry.search("cli")

  ASSERT:
    results_core.len() == 2
    results_cli.len() == 1
    results_cli[0].id == "ggen-cli"

// ================================================================
// SMOKE TESTS: Happy Path
// ================================================================

fn test_happy_path_package_lifecycle()
  ARRANGE:
    registry = Registry::new()
    pkg_builder = PackageBuilder::new("my-pkg")
      .version("1.0.0")
      .description("My Package")

  ACT:
    // Create package
    pkg = pkg_builder.build()
    registry.insert(pkg).await

    // Search for it
    found = registry.search("my-pkg").await

    // Resolve dependencies
    installer = Installer::new(registry)
    resolved = installer.resolve_dependencies("my-pkg", "1.0.0").await

    // Create installation manifest
    manifest = installer.create_manifest(
      vec!["my-pkg"],
      "/usr/local/ggen"
    ).await

  ASSERT:
    found.len() == 1
    resolved.len() == 1
    manifest.dependencies.contains_key("my-pkg")
```

---

## Module 2: Consolidated Lifecycle Tests

### File: `crates/ggen-core/tests/consolidated_lifecycle_tests.rs`

```pseudocode
// ================================================================
// HAPPY PATH: Full Package Lifecycle
// ================================================================

fn test_draft_to_published_transition()
  ARRANGE:
    pkg = Package::new("my-pkg", "1.0.0")
    pkg.state = PackageState::Draft

  ACT:
    // Validate package
    validation = Validator::validate_package(&pkg)

    // Publish package
    pkg.state = PackageState::Published
    registry.insert(pkg).await

    // Verify it's searchable
    found = registry.search("my-pkg").await

  ASSERT:
    validation.is_ok()
    pkg.state == PackageState::Published
    found.len() == 1

fn test_version_upgrade_workflow()
  ARRANGE:
    pkg_v1 = Package::new("my-pkg", "1.0.0")
    pkg_v2 = Package::new("my-pkg", "1.1.0")

    registry.insert(pkg_v1).await

  ACT:
    // Add v1.1.0
    registry.insert(pkg_v2).await

    // Verify both versions exist
    pkg_info = registry.get_all_versions("my-pkg").await

    // Update latest to v1.1.0
    latest = registry.get_latest("my-pkg").await

  ASSERT:
    pkg_info.len() == 2
    latest.version == "1.1.0"

fn test_package_installation_flow()
  ARRANGE:
    registry = Registry::with_packages([
      ("pkg-a", "1.0.0", []),
      ("pkg-b", "1.0.0", ["pkg-a@1.0"]),
    ])
    installer = Installer::new(registry)

  ACT:
    // Create manifest
    manifest = installer.create_manifest(
      vec!["pkg-b"],
      "/tmp/install"
    ).await

    // Validate manifest
    installer.validate_manifest(&manifest).await

    // Dry run
    plan = installer.dry_run(&manifest).await

    // Install
    result = installer.install(manifest).await

  ASSERT:
    manifest.dependencies.len() == 2
    plan.packages.len() == 2
    result.is_ok()

fn test_package_yanking_workflow()
  ARRANGE:
    pkg = Package::new("my-pkg", "1.0.0")
    registry.insert(pkg).await

  ACT:
    // Yank version
    registry.yank("my-pkg", "1.0.0").await

    // Try to install yanked version
    manifest = installer.create_manifest(
      vec!["my-pkg@1.0.0"],
      "/tmp/install"
    ).await

  ASSERT:
    manifest.error contains "yanked"

// ================================================================
// ERROR PATHS: Critical Failures
// ================================================================

fn test_circular_dependency_error_handling()
  ARRANGE:
    registry = Registry::with_packages([
      ("a", "1.0.0", ["b@1.0"]),
      ("b", "1.0.0", ["a@1.0"]),
    ])
    installer = Installer::new(registry)

  ACT:
    result = installer.resolve_dependencies("a", "1.0.0").await

  ASSERT:
    result.is_err()
    result.error contains "circular"

fn test_version_conflict_resolution()
  ARRANGE:
    registry = Registry::with_packages([
      ("a", "1.0.0", ["c@1.0"]),
      ("b", "1.0.0", ["c@2.0"]),
      ("c", "1.0.0", []),
      ("c", "2.0.0", []),
    ])

  ACT:
    manifest = installer.create_manifest(
      vec!["a", "b"],
      "/tmp/install"
    ).await

  ASSERT:
    manifest.error contains "conflict"

fn test_broken_package_handling()
  ARRANGE:
    // Package with missing dependency
    broken_pkg = PackageBuilder::new("broken")
      .version("1.0.0")
      .add_dependency("nonexistent@1.0")
      .build()

    registry.insert(broken_pkg).await

  ACT:
    result = installer.resolve_dependencies("broken", "1.0.0").await

  ASSERT:
    result.is_err()
    result.error contains "not found"

fn test_installation_rollback_on_failure()
  ARRANGE:
    registry = Registry::with_packages([
      ("good", "1.0.0", []),
      ("bad", "1.0.0", []),  // Will fail during install
    ])
    installer = Installer::new(registry)

  ACT:
    manifest = installer.create_manifest(
      vec!["good", "bad"],
      "/tmp/install"
    ).await

    // Mock install failure on "bad"
    result = installer.install(manifest).await

  ASSERT:
    result.is_err()
    installation_path is empty
```

---

## Module 3: Consolidated Swarm Tests

### File: `crates/ggen-core/tests/consolidated_swarm_tests.rs`

```pseudocode
// ================================================================
// CONSENSUS: Leader Election & State Agreement
// ================================================================

fn test_leader_election()
  ARRANGE:
    hive = HiveCoordinator::new(3)  // 3 nodes
    nodes = hive.spawn_nodes(3)

  ACT:
    // Wait for election
    leader = hive.wait_for_leader(Duration::from_secs(5)).await

    // Verify majority has leader
    election_results = nodes.map(|n| n.current_leader())

  ASSERT:
    leader.is_some()
    election_results.iter().all(|l| l == leader)

fn test_consensus_state_agreement()
  ARRANGE:
    hive = HiveCoordinator::new(3)
    hive.spawn_nodes(3)

  ACT:
    // Propose state change
    proposal = StateChange::set_registry_version("2.0.0")
    hive.propose(proposal).await

    // Wait for consensus
    result = hive.wait_for_consensus(Duration::from_secs(5)).await

  ASSERT:
    result.is_committed()

// ================================================================
// FAILURES: Recovery Scenarios
// ================================================================

fn test_node_failure_recovery()
  ARRANGE:
    hive = HiveCoordinator::new(3)
    hive.spawn_nodes(3)
    hive.wait_for_leader(Duration::from_secs(5)).await

  ACT:
    // Stop 1 node
    hive.stop_node(0).await

    // System should still operate (2/3 quorum)
    proposal = StateChange::increment_version()
    result = hive.propose(proposal).await

    // Wait for recovery
    recovered = hive.wait_for_node_recovery(0, Duration::from_secs(10)).await

  ASSERT:
    result.is_ok()
    recovered.is_ok()

fn test_network_partition_recovery()
  ARRANGE:
    hive = HiveCoordinator::new(5)
    hive.spawn_nodes(5)

  ACT:
    // Partition: 3 vs 2 nodes
    hive.partition_nodes([0,1,2], [3,4]).await

    // Majority (3) should continue
    proposal_majority = StateChange::set_value("key", "value")
    result_majority = hive.nodes([0,1,2]).propose(proposal_majority).await

    // Minority (2) should block
    proposal_minority = StateChange::set_value("other", "data")
    result_minority = hive.nodes([3,4]).propose(proposal_minority).await

    // Heal partition
    hive.heal_partition().await

  ASSERT:
    result_majority.is_ok()
    result_minority.is_err()  // Blocked (no quorum)

// ================================================================
// SECURITY: Byzantine Tolerance
// ================================================================

fn test_byzantine_node_tolerance()
  ARRANGE:
    hive = HiveCoordinator::new(5)
    hive.spawn_nodes(5)

    // Mark node 0 as byzantine (sends conflicting messages)
    hive.nodes[0].set_byzantine()

  ACT:
    // Propose change
    proposal = StateChange::set_registry_version("2.0.0")
    results = hive.propose(proposal).await

  ASSERT:
    // Majority (4) should agree
    results.committed_count >= 3
    hive.state.registry_version == "2.0.0"

fn test_signature_verification()
  ARRANGE:
    (public_key, private_key) = generate_ed25519_keypair()
    data = b"important message"

  ACT:
    signature = sign(data, &private_key)
    valid = verify(data, &signature, &public_key)

  ASSERT:
    valid == true
    verify(b"tampered", &signature, &public_key) == false
```

---

## Module 4: Consolidated Semantic Tests

### File: `crates/ggen-core/tests/consolidated_semantic_tests.rs`

```pseudocode
// ================================================================
// ONTOLOGY: Schema & Validation
// ================================================================

fn test_ontology_namespace_definitions()
  ARRANGE:
    ontology = OntologyDef::new()

  ACT:
    // Define namespaces
    ontology.define_namespace("ggen", "http://ggen.dev/")
    ontology.define_namespace("foaf", "http://xmlns.com/foaf/")

    namespaces = ontology.get_namespaces()

  ASSERT:
    namespaces.contains("ggen")
    namespaces.contains("foaf")

fn test_class_hierarchy()
  ARRANGE:
    ontology = OntologyDef::new()

  ACT:
    ontology.define_class("Package")
    ontology.define_class("Release", subclass_of: "Package")
    ontology.define_class("Version", subclass_of: "Release")

    hierarchy = ontology.get_class_hierarchy()

  ASSERT:
    hierarchy["Release"].parent == "Package"
    hierarchy["Version"].parent == "Release"

fn test_property_constraints()
  ARRANGE:
    ontology = OntologyDef::new()

  ACT:
    ontology.define_property(
      "hasVersion",
      domain: "Package",
      range: "Version",
      cardinality: "ONE_OR_MORE"
    )

    constraints = ontology.get_property("hasVersion")

  ASSERT:
    constraints.domain == "Package"
    constraints.range == "Version"

// ================================================================
// RDF OPERATIONS: Triple Management
// ================================================================

fn test_triple_insertion()
  ARRANGE:
    store = RdfStore::new()

  ACT:
    store.insert(Triple {
      subject: "ggen:pkg-core",
      predicate: "rdf:type",
      object: "ggen:Package",
    })

    store.insert(Triple {
      subject: "ggen:pkg-core",
      predicate: "ggen:version",
      object: "1.0.0",
    })

    triples = store.get_triples("ggen:pkg-core", None, None)

  ASSERT:
    triples.len() == 2

fn test_triple_deletion()
  ARRANGE:
    store = RdfStore::with_triples([
      ("ggen:pkg", "rdf:type", "ggen:Package"),
      ("ggen:pkg", "ggen:version", "1.0.0"),
    ])

  ACT:
    store.delete(Triple {
      subject: "ggen:pkg",
      predicate: "ggen:version",
      object: "1.0.0",
    })

    remaining = store.get_triples("ggen:pkg", None, None)

  ASSERT:
    remaining.len() == 1

fn test_sparql_query_execution()
  ARRANGE:
    store = RdfStore::with_packages([
      Package {
        id: "ggen-core",
        version: "1.0.0",
        published: true,
      },
      Package {
        id: "ggen-cli",
        version: "1.0.0",
        published: true,
      },
    ])

  ACT:
    query = """
      SELECT ?pkg ?version WHERE {
        ?pkg rdf:type ggen:Package .
        ?pkg ggen:version ?version .
        ?pkg ggen:published true .
      }
    """
    results = store.sparql(query)

  ASSERT:
    results.len() == 2

// ================================================================
// GRAPH CONSISTENCY: Data Integrity
// ================================================================

fn test_graph_consistency_validation()
  ARRANGE:
    store = RdfStore::new()

    // Add circular dependency
    store.insert(("a", "depends", "b"))
    store.insert(("b", "depends", "c"))
    store.insert(("c", "depends", "a"))

  ACT:
    consistency = store.validate_consistency()

  ASSERT:
    consistency.has_cycles == true
    consistency.cycles contains ["a", "b", "c"]
```

---

## Summary: 80/20 Test Coverage

```
Consolidated Modules:
├── consolidated_core_tests.rs (350 lines)
│   ├── Package validation (3 tests)
│   ├── Marketplace operations (4 tests)
│   └── Happy path smoke tests (2 tests)
│
├── consolidated_lifecycle_tests.rs (350 lines)
│   ├── Happy path (4 tests)
│   └── Error paths (5 tests)
│
├── consolidated_swarm_tests.rs (300 lines)
│   ├── Consensus (2 tests)
│   ├── Failure recovery (2 tests)
│   └── Security (2 tests)
│
└── consolidated_semantic_tests.rs (300 lines)
    ├── Ontology (3 tests)
    ├── RDF operations (3 tests)
    └── Graph consistency (1 test)

Total: ~1,300 lines, 29 critical tests
Coverage: 80%+ of critical paths
Execution: <60 seconds
```

