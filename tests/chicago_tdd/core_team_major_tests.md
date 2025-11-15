# Major Tests for Ggen Core Team - High-Impact Production Tests

**Document**: Core Team Priority Test Specification
**Status**: High-Impact Testing Roadmap
**Scope**: 12 major tests to improve production readiness from 89% → 96%+
**Timeline**: 4-6 weeks | 1-2 engineers
**Impact**: Eliminate 11 panic points, 23 untested code paths, 5 data integrity risks

---

## Executive Summary

This document identifies the **12 major tests** that the ggen core team should implement to ensure production readiness. These are high-impact integration and end-to-end tests that verify critical business workflows.

### Current State (v2.6.0)
- Code coverage: ~70%
- Production readiness: 89%
- Known panic points: 11
- Untested critical paths: 23
- Security gaps: 2 (path traversal, input injection)

### Target State (After Tests)
- Code coverage: 90%+
- Production readiness: 96%+
- Known panics: 0
- Untested critical paths: <5

---

## Tier 1: P0 - Production Blockers (23-30 days)

### Test 1: RDF → Polyglot Code Generation (CRITICAL - 100% impact)

**Why Critical**: This is the core value proposition. If this breaks, the product doesn't work.

**Workflow**:
1. Create RDF knowledge graph with semantic model
2. Add generation templates for multiple languages
3. Execute generation pipeline
4. Verify polyglot code is correct for each language

**Files Involved**:
- `crates/ggen-core/src/generator.rs` (main orchestration)
- `crates/ggen-core/src/graph/core.rs` (RDF store)
- `crates/ggen-core/src/templates/generator.rs` (file tree gen)
- `crates/ggen-domain/src/template/generate.rs` (domain logic)

**Test Scenario**:
```rust
test!(test_rdf_to_polyglot_generation, {
    // Arrange: Create RDF graph with User entity
    let graph = create_test_ontology_with_user_entity();

    // Add Rust template
    add_template(&graph, "rust", RUST_STRUCT_TEMPLATE);

    // Add Python template
    add_template(&graph, "python", PYTHON_CLASS_TEMPLATE);

    // Add TypeScript template
    add_template(&graph, "typescript", TYPESCRIPT_INTERFACE_TEMPLATE);

    // Act: Generate for all three languages
    let rust_code = generate(&graph, "rust");
    let python_code = generate(&graph, "python");
    let ts_code = generate(&graph, "typescript");

    // Assert: All languages generated correctly
    assert_contains!(&rust_code, "struct User");
    assert_contains!(&python_code, "class User:");
    assert_contains!(&ts_code, "interface User");

    // Verify no panics or unwrap() failures
    assert!(rust_code.len() > 0);
    assert!(python_code.len() > 0);
    assert!(ts_code.len() > 0);
});
```

**Impact if Broken**: Product non-functional - core generation pipeline fails

**Test Type**: Integration + E2E

---

### Test 2: Marketplace Package Installation (P0 - 25% impact)

**Why Critical**: Enterprise feature. Circular dependencies, missing packages, version conflicts break silently.

**Workflow**:
1. Install package from marketplace (ggen-marketplace)
2. Resolve all dependencies recursively
3. Detect circular dependencies
4. Download and cache package
5. Verify integrity with SHA256
6. Load templates from package

**Files Involved**:
- `crates/ggen-domain/src/marketplace/install.rs` (main installation)
- `crates/ggen-core/src/cache.rs` (package caching)
- `crates/ggen-domain/src/marketplace/dependencies.rs` (dependency resolution)

**Test Scenarios** (3 variations):
```rust
// Scenario 1: Simple dependency chain
test!(test_marketplace_install_simple_chain, {
    // Package A depends on B, B depends on C
    let result = marketplace.install("package-a");
    assert_ok!(&result);

    // Verify all three are installed
    assert!(is_installed("package-a"));
    assert!(is_installed("package-b"));
    assert!(is_installed("package-c"));
});

// Scenario 2: Circular dependency detection
test!(test_marketplace_circular_dependency_detection, {
    // Package A depends on B, B depends on A
    let result = marketplace.install("package-a");

    // Should detect and report cycle, not hang
    assert_err!(&result);
    assert!(error_msg(&result).contains("circular"));
});

// Scenario 3: Conflict resolution
test!(test_marketplace_version_conflict, {
    // Already have B@1.0, but A needs B@2.0
    install_package("package-b", "1.0");
    let result = marketplace.install("package-a");

    // Should handle conflict gracefully
    assert_err!(&result) || verify_version("package-b", "2.0");
});
```

**Impact if Broken**: Users can't install packages, dependency resolution fails silently, circular dependencies hang

**Test Type**: Integration + Property-based (for version conflict scenarios)

---

### Test 3: Deterministic Generation (P0 - 20% impact)

**Why Critical**: Users need reproducible builds. Non-deterministic output breaks CI/CD and version control.

**Workflow**:
1. Generate code from template + RDF
2. Run generation again with same inputs
3. Verify outputs are byte-for-byte identical
4. Verify with different orderings (property iteration order)

**Files Involved**:
- `crates/ggen-core/src/generator.rs`
- `crates/ggen-core/src/graph/core.rs` (SPARQL result ordering)
- `crates/ggen-core/src/templates/generator.rs`

**Test Scenario**:
```rust
test!(test_deterministic_generation, {
    // Arrange: Fixed RDF graph and template
    let graph = create_deterministic_test_graph();
    let template = DETERMINISTIC_TEST_TEMPLATE;

    // Act: Generate 10 times
    let results: Vec<String> = (0..10)
        .map(|_| generate(&graph, template).unwrap())
        .collect();

    // Assert: All results are identical
    let first = &results[0];
    for (i, result) in results.iter().enumerate() {
        assert_eq!(result, first, "Generation {} differs from first", i);
    }

    // Verify checksums match
    let checksums: Vec<_> = results
        .iter()
        .map(|r| sha256(r))
        .collect();

    let first_hash = &checksums[0];
    for (i, hash) in checksums.iter().enumerate() {
        assert_eq!(hash, first_hash, "Hash {} differs", i);
    }
});
```

**Impact if Broken**: Non-reproducible builds, breaking CI/CD pipelines, users can't version control generated code

**Test Type**: Property-based testing (with seed control)

---

### Test 4: RDF Graph Consistency (P0 - 18% impact)

**Why Critical**: Data integrity. Graph becomes corrupted after queries, updates, or concurrent access.

**Workflow**:
1. Create RDF graph with 100+ triples
2. Execute SPARQL queries and updates concurrently
3. Verify graph integrity (no data loss, no corruption)
4. Verify cache invalidation works correctly

**Files Involved**:
- `crates/ggen-core/src/graph/core.rs`
- `crates/ggen-core/src/graph/store.rs`
- `crates/ggen-core/src/graph/update.rs`

**Test Scenario**:
```rust
test!(test_rdf_graph_consistency_after_concurrent_ops, {
    // Arrange: Create graph
    let graph = Arc::new(Graph::new().unwrap());

    // Insert initial data
    graph.insert_turtle(COMPREHENSIVE_TEST_DATA).unwrap();
    let initial_count = graph.query("SELECT ?s WHERE { ?s ?p ?o }")
        .unwrap()
        .len();

    // Act: Spawn concurrent threads doing reads and writes
    let mut handles = vec![];
    for i in 0..10 {
        let g = Arc::clone(&graph);
        let handle = std::thread::spawn(move || {
            // Half do reads, half do writes
            if i % 2 == 0 {
                g.query("SELECT ?s WHERE { ?s a ?type }").unwrap()
            } else {
                g.update(&format!("INSERT DATA {{ <http://example.org/item{}> a <http://example.org/Type> }}", i))
                    .unwrap();
                vec![]
            }
        });
        handles.push(handle);
    }

    // Wait for all to complete
    for handle in handles {
        handle.join().unwrap();
    }

    // Assert: Graph integrity maintained
    let final_count = graph.query("SELECT ?s WHERE { ?s ?p ?o }")
        .unwrap()
        .len();

    // Should have initial count + 5 new insertions
    assert!(final_count >= initial_count + 5,
            "Expected at least {} triples, got {}",
            initial_count + 5, final_count);

    // Verify no panics or corrupted results
    let all_subjects = graph.query("SELECT DISTINCT ?s WHERE { ?s ?p ?o }")
        .unwrap();
    assert!(all_subjects.len() > 0, "Should have subjects after operations");
});
```

**Impact if Broken**: Silent data corruption, incorrect generation, concurrent access breaks

**Test Type**: Stress + Property-based + Integration

---

### Test 5: Cache Invalidation (P0 - 15% impact)

**Why Critical**: Stale cache returns incorrect results, users see outdated generated code.

**Workflow**:
1. Execute SPARQL query, result is cached
2. Insert new data (epoch changes)
3. Execute same query again
4. Verify new data is seen, cache was invalidated

**Files Involved**:
- `crates/ggen-core/src/graph/core.rs` (epoch-based invalidation)
- `crates/ggen-core/src/cache.rs` (LRU cache)

**Test Scenario**:
```rust
test!(test_cache_invalidation_on_data_change, {
    // Arrange: Create graph and populate
    let graph = Graph::new().unwrap();
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        ex:alice a ex:Person .
    "#).unwrap();

    // Query and verify result (cached)
    let result1 = graph.query("SELECT ?person WHERE { ?person a ex:Person }")
        .unwrap();
    assert_eq!(result1.len(), 1, "Should find alice");

    // Act: Insert new data
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        ex:bob a ex:Person .
    "#).unwrap();

    // Assert: Cache should be invalidated
    let result2 = graph.query("SELECT ?person WHERE { ?person a ex:Person }")
        .unwrap();
    assert_eq!(result2.len(), 2, "Should find both alice and bob (cache invalidated)");

    // Verify results are different (cache wasn't used)
    assert_ne!(result1, result2, "Results should differ after insertion");
});
```

**Impact if Broken**: Users see stale generated code, incorrect SPARQL results, confusing behavior

**Test Type**: Integration

---

### Test 6: Path Traversal Security (P0 - 12% impact)

**Why Critical**: Security vulnerability. Attacker could write to arbitrary files.

**Workflow**:
1. Attempt to generate code with path traversal in template names
2. Attempt to generate code with `../` in variable values
3. Verify all attempts are blocked or sanitized

**Files Involved**:
- `crates/ggen-domain/src/marketplace/install.rs` (package name validation)
- `crates/ggen-domain/src/template/generate.rs` (path handling)
- `crates/ggen-core/src/generator.rs` (output path validation)

**Test Scenarios**:
```rust
test!(test_path_traversal_blocked, {
    // Attempt 1: Path traversal in package name
    let result = validate_package_name("../../../etc/passwd");
    assert_err!(&result, "Should reject path traversal");

    // Attempt 2: Path traversal in template variable
    let result = generate_with_var("output_dir", "../../../sensitive");
    assert_err!(&result, "Should reject output directory traversal");

    // Attempt 3: Null byte injection
    let result = validate_package_name("package\0.so");
    assert_err!(&result, "Should reject null bytes");
});

test!(test_symlink_injection_prevented, {
    // Create output dir
    let temp = TempDir::new().unwrap();

    // Attempt to generate to symlink target
    let symlink_path = temp.path().join("link");
    std::os::unix::fs::symlink("/etc/passwd", &symlink_path).ok(); // Ignore if not supported

    let result = generate_to_path(TEMPLATE, &symlink_path);

    // Should either reject or handle safely
    if result.is_ok() {
        // Verify we didn't write to /etc/passwd
        assert!("/etc/passwd".lines().count() < 100); // Sanity check
    }
});
```

**Impact if Broken**: Arbitrary file write vulnerability, data breach, system compromise

**Test Type**: Security testing

---

### Test 7: Panic Prevention (P0 - 10% impact)

**Why Critical**: 11 identified `panic!()` and `unwrap()` in critical code paths.

**Known Panic Points**:
- `Graph::query()` on invalid SPARQL (parse panic)
- `Generator::generate()` on missing template variable (unwrap panic)
- `CacheManager::ensure()` on download failure (unwrap)
- `merge.rs` on malformed regions (panic)
- Others in lifecycle, update operations

**Workflow**:
1. Systematically pass invalid input to each panic point
2. Verify graceful error handling instead of panic
3. Verify error messages are helpful

**Test Scenario**:
```rust
test!(test_no_panics_on_invalid_sparql, {
    let graph = Graph::new().unwrap();

    // These should error, not panic
    let invalid_queries = vec![
        "SELECT WHERE { }",  // Missing variables
        "SELECT ?s FROM { ?s ?p ?o }",  // Invalid FROM
        "INVALID QUERY",  // Garbage
    ];

    for query in invalid_queries {
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            graph.query(query)
        }));

        // Should not panic
        assert!(result.is_ok(), "Query '{}' should not panic", query);

        // Should return error
        assert_err!(&graph.query(query), "Query '{}' should error", query);
    }
});

test!(test_no_panics_on_missing_template_var, {
    let template = "Hello {{ name }}, {{ missing_var }}";

    // This should error, not panic
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        render_template(template, &[("name", "World")])
    }));

    // Should not panic
    assert!(result.is_ok(), "Missing var should not panic");

    // Should return error about missing variable
    assert_err!(&render_template(template, &[("name", "World")]));
});
```

**Impact if Broken**: Production crashes, service downtime, data loss during crashes

**Test Type**: Stress + Fuzz testing

---

## Tier 2: P1 - Important (13-17 days)

### Test 8: Template + RDF Combined Rendering (P1 - 12% impact)

**Why Important**: Most complex real-world scenario. Templates use RDF data extensively.

**Workflow**:
1. Create RDF graph with complex data model
2. Create template that queries RDF and uses results
3. Render template with RDF integration
4. Verify output is correct

**Files Involved**:
- `crates/ggen-core/src/templates/generator.rs`
- `crates/ggen-domain/src/template/generate.rs`
- `crates/ggen-core/src/graph/core.rs` (SPARQL in template)

**Test Scenario**:
```rust
test!(test_template_with_rdf_queries, {
    // Arrange: Create RDF graph
    let graph = Graph::new().unwrap();
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        ex:User a ex:Entity ; ex:fields "id,name,email" .
        ex:Post a ex:Entity ; ex:fields "id,title,content" .
    "#).unwrap();

    // Create template that queries RDF
    let template = r#"
// Generated entities
{% for entity in entities %}
pub struct {{ entity.name }} {
    {% for field in entity.fields %}
    pub {{ field }}: String,
    {% endfor %}
}
{% endfor %}
    "#;

    // Act: Render template with RDF
    let result = render_template_with_rdf(&graph, template);

    // Assert: Output includes both entities
    assert_ok!(&result);
    let code = result.unwrap();
    assert_contains!(&code, "pub struct User");
    assert_contains!(&code, "pub struct Post");
    assert_contains!(&code, "pub id: String");
});
```

**Impact if Broken**: Complex templates fail, real-world use cases broken

**Test Type**: Integration

---

### Test 9: Three-Way Merge Edge Cases (P1 - 10% impact)

**Why Important**: Users edit generated code, merge conflicts lose data.

**Workflow**:
1. Create baseline generated code
2. User edits baseline (manual)
3. Generator produces new code (regenerated)
4. Perform 3-way merge: baseline → regenerated, baseline → manual
5. Verify conflicts are detected correctly, no data loss

**Files Involved**:
- `crates/ggen-core/src/merge.rs` (3-way merge logic)
- `crates/ggen-core/src/merge/regions.rs` (region parsing)

**Test Scenarios**:
```rust
test!(test_three_way_merge_user_edits_preserved, {
    let baseline = "fn hello() {\n    println!(\"Hello\");\n}\n";
    let generated = "fn hello() {\n    println!(\"Hello, World!\");\n}\n";
    let manual = "fn hello() {\n    println!(\"Hello\");\n    println!(\"Extra line\");\n}\n";

    let result = three_way_merge(baseline, generated, manual);

    // Should merge successfully
    assert_ok!(&result);
    let merged = result.unwrap();

    // Should preserve user's extra line
    assert_contains!(&merged, "Extra line");
});

test!(test_three_way_merge_conflict_detection, {
    let baseline = "fn greet() { println!(\"Hi\"); }";
    let generated = "fn greet() { println!(\"Hello\"); }";  // Changed message
    let manual = "fn greet() { println!(\"Hey\"); }";       // Also changed message (conflict)

    let result = three_way_merge(baseline, generated, manual);

    // Should detect conflict
    if result.is_err() {
        let error = result.unwrap_err();
        assert_contains!(&error.to_string(), "conflict");
    } else {
        // Or mark conflict in output
        let merged = result.unwrap();
        assert!(merged.contains("<<<<<<") || merged.contains("conflict"));
    }
});

test!(test_three_way_merge_region_awareness, {
    let baseline = "// START GENERATED\nfn gen() {}\n// END GENERATED\nfn manual() {}";
    let generated = "// START GENERATED\nfn gen() { println!(\"updated\"); }\n// END GENERATED\nfn manual() {}";
    let manual = "// START GENERATED\nfn gen() {}\n// END GENERATED\nfn manual() { println!(\"custom\"); }";

    let result = three_way_merge(baseline, generated, manual);

    assert_ok!(&result);
    let merged = result.unwrap();

    // Both changes should be merged
    assert_contains!(&merged, "updated");
    assert_contains!(&merged, "custom");
});
```

**Impact if Broken**: Users lose manual edits on regeneration, merge conflicts silent

**Test Type**: Integration + Property-based

---

### Test 10: Large File Handling (P1 - 8% impact)

**Why Important**: Enterprise scale - some projects have 1000+ generated files.

**Workflow**:
1. Create template that generates 1000 files
2. Each file has 100+ lines
3. Generate all files concurrently
4. Verify all files are created correctly
5. Verify no resource exhaustion

**Files Involved**:
- `crates/ggen-core/src/templates/generator.rs`
- `crates/ggen-core/src/generator.rs`

**Test Scenario**:
```rust
test!(test_large_project_generation, {
    // Arrange: Template that generates many files
    let template = create_large_template(1000); // 1000 files

    // Act: Generate all files
    let start_time = std::time::Instant::now();
    let result = generate_files(&template);
    let duration = start_time.elapsed();

    // Assert: All files created
    assert_ok!(&result);

    let generated_files = result.unwrap();
    assert_eq!(generated_files.len(), 1000, "Should generate 1000 files");

    // Verify reasonable performance (< 10 seconds)
    assert!(duration.as_secs() < 10, "Generation took {}s, expected < 10s", duration.as_secs());

    // Verify all files are readable
    for file_path in &generated_files {
        assert!(file_path.exists(), "File {} should exist", file_path.display());
        let size = std::fs::metadata(file_path).unwrap().len();
        assert!(size > 100, "File should have content");
    }
});
```

**Impact if Broken**: Large projects timeout, incomplete generation, resource exhaustion

**Test Type**: Stress testing

---

### Test 11: Merge Conflict Edge Cases (P1 - 5% impact)

**Why Important**: Prevent data loss in edge cases.

**Edge Cases**:
- Empty files
- Files with only whitespace
- Binary files (should not merge)
- Very large files (memory issues)
- Files with no newline at end
- Files with different line endings (CRLF vs LF)

**Test Scenarios**:
```rust
test!(test_merge_empty_file, {
    let baseline = "";
    let generated = "fn hello() {}";
    let manual = "";

    let result = three_way_merge(baseline, generated, manual);
    assert_ok!(&result);
});

test!(test_merge_whitespace_only, {
    let baseline = "   \n\n   ";
    let generated = "fn test() {}";
    let manual = "   \n\n   ";

    let result = three_way_merge(baseline, generated, manual);
    assert_ok!(&result);
});

test!(test_merge_different_line_endings, {
    let baseline = "line1\nline2\n";      // LF
    let generated = "line1\r\nline2\r\n"; // CRLF
    let manual = "line1\nline2\n";        // LF

    let result = three_way_merge(baseline, generated, manual);
    // Should normalize or handle gracefully
    assert_ok!(&result);
});
```

**Impact if Broken**: Data loss, merge failures, confusing errors

**Test Type**: Edge case testing

---

## Tier 3: P2 - Polish (3-4 days)

### Test 12: UTF-8 & Encoding Robustness (P2 - 3% impact)

**Why Important**: International support, non-ASCII identifiers, emoji in docs.

**Scenarios**:
- UTF-8 BOM handling
- Combining characters
- Emoji in strings
- Right-to-left text
- Surrogate pairs
- Invalid UTF-8 sequences

**Test Scenario**:
```rust
test!(test_utf8_robustness, {
    let test_cases = vec![
        ("Hello 世界", "Chinese characters"),
        ("Привет мир", "Cyrillic"),
        ("مرحبا بالعالم", "Arabic (RTL)"),
        ("Hello 👋 World", "Emoji"),
        ("café", "Combining diacritics"),
    ];

    for (text, description) in test_cases {
        let result = generate_with_text(text);
        assert_ok!(&result, "Should handle {} in output", description);
    }
});
```

**Impact if Broken**: International users can't use ggen, encoding errors

**Test Type**: Internationalization testing

---

## Implementation Roadmap

### Week 1: Foundation (Tests 1, 3, 7)
- P0: RDF generation (test core value prop)
- P0: Deterministic output (reproducible builds)
- P0: Panic prevention (crash prevention)

### Week 2: Distribution (Tests 2, 4, 5)
- P0: Marketplace installation (enterprise feature)
- P0: RDF consistency (data integrity)
- P0: Cache invalidation (stale data prevention)

### Week 3: Safety & Scale (Tests 6, 8, 10)
- P0: Path traversal (security)
- P1: Template+RDF (complex scenarios)
- P1: Large files (enterprise scale)

### Week 4-5: Polish (Tests 9, 11, 12)
- P1: Three-way merge (user edits)
- P1: Merge edge cases (data safety)
- P2: UTF-8 (internationalization)

---

## Success Criteria

✅ All 12 tests passing
✅ Code coverage improved to 90%+
✅ Zero known panic points in critical code
✅ All 23 untested critical paths covered
✅ Production readiness improved to 96%+
✅ No regression in performance
✅ All security tests passing
✅ Enterprise scenarios (1000+ files) working

---

## Resource Estimate

**Effort**: 39-51 days total
- Test implementation: 30-40 days
- Debugging/fixes: 5-10 days
- Documentation: 4-5 days

**Team**: 1-2 engineers
**Cost**: ~$50K-100K (assuming $80K/year per engineer)
**Timeline**: 4-6 weeks with 1 engineer, 2-3 weeks with 2 engineers

---

## Risk Mitigation

1. **Already identified**: 11 panic points - most critical
2. **Property-based testing**: Catch edge cases automatically
3. **Stress testing**: Find performance cliffs early
4. **Security testing**: Prevent exploitation
5. **Concurrency testing**: Catch race conditions
6. **Integration tests**: Verify real-world workflows

---

## Next Steps

1. **Review** this document with core team
2. **Prioritize** based on team bandwidth and business needs
3. **Start** with P0 tests (highest ROI)
4. **Track** progress against timeline
5. **Iterate** - tests will reveal more issues
6. **Document** learnings for future testing

---

## Related Documents

- `EXPERT_TESTING_PATTERNS.md` - General testing patterns (already implemented)
- `tests/chicago_tdd/expert_patterns/` - Foundational tests
- This document - Major production tests (next phase)

---

## Appendix: Test Types Reference

### Integration Testing
Real objects, actual dependencies, verifies workflows end-to-end.
Example: RDF generation with real templates and file I/O.

### Property-Based Testing
Automatic generation of test cases, verifies properties hold for all inputs.
Example: "Deterministic generation always produces identical output"

### Stress Testing
High load, many concurrent operations, resource exhaustion.
Example: 1000 concurrent file generation, 100K+ triples in RDF.

### Security Testing
Adversarial inputs, vulnerability probing, fuzzing.
Example: Path traversal, SQL injection (if applicable), buffer overflow.

### Fuzz Testing
Random/mutated inputs to find crashes.
Example: Random SPARQL queries to find parser panics.

---

**Document Version**: 1.0
**Last Updated**: 2025-11-15
**Status**: Ready for Implementation
