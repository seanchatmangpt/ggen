//! Concurrent access tests for `Graph`.
//!
//! Validates that `Graph` (Arc<Store> + Arc<Mutex<LruCache>> + Arc<AtomicU64>)
//! is safe for concurrent reads and writes across multiple threads.

use ggen_core::graph::{CachedResult, Graph};
use std::sync::{Arc, Barrier};
use std::thread;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Create a graph pre-loaded with `count` triples of the form:
/// `ex:item0 ex:value "0" .`, `ex:item1 ex:value "1" .`, ...
fn make_graph_with_triples(count: usize) -> Graph {
    let graph = Graph::new().expect("Graph::new");
    let mut turtle = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..count {
        turtle.push_str(&format!("ex:item{i} ex:value \"{i}\" .\n"));
    }
    graph
        .insert_turtle(&turtle)
        .expect("insert_turtle in make_graph_with_triples");
    graph
}

/// SPARQL that counts all triples (returns a single-row, single-column result).
const COUNT_QUERY: &str = "SELECT (COUNT(*) AS ?n) WHERE { ?s ?p ?o }";

/// SPARQL that selects all items that have an `ex:value` predicate.
const ALL_ITEMS_QUERY: &str =
    "SELECT ?item WHERE { ?item <http://example.org/value> ?v }";

/// SPARQL that selects all items that have an `ex:name` predicate.
const ALL_NAMED_QUERY: &str =
    "SELECT ?item WHERE { ?item <http://example.org/name> ?v }";

/// Extract the number of solution rows.
fn row_count(result: &CachedResult) -> usize {
    match result {
        CachedResult::Solutions(rows) => rows.len(),
        _ => 0,
    }
}

/// Extract the integer value from a single-row COUNT query result.
///
/// Oxigraph serializes `COUNT(*)` as something like
/// `"3"^^<http://www.w3.org/2001/XMLSchema#integer>`.
/// We parse out the leading digits from the string representation.
fn extract_count(result: &CachedResult) -> usize {
    match result {
        CachedResult::Solutions(rows) => rows
            .first()
            .and_then(|r| r.get("n"))
            .and_then(|v| {
                v.chars()
                    .take_while(|c| c.is_ascii_digit())
                    .collect::<String>()
                    .parse::<usize>()
                    .ok()
            })
            .unwrap_or(0),
        _ => 0,
    }
}

// ---------------------------------------------------------------------------
// Test 1: Clone shares underlying store
// ---------------------------------------------------------------------------

#[test]
fn test_clone_shares_underlying_store() {
    let graph = make_graph_with_triples(3);
    let clone = graph.clone();

    assert_eq!(graph.len(), clone.len());
    assert!(!graph.is_empty());

    // Insert via the clone using Turtle (insert_quad only accepts IRIs, not literals).
    clone
        .insert_turtle(
            r#"@prefix ex: <http://example.org/> .
               ex:extra ex:value "extra" ."#,
        )
        .expect("insert_turtle on clone");

    assert_eq!(graph.len(), clone.len());
    assert_eq!(graph.len(), 4);
}

// ---------------------------------------------------------------------------
// Test 2: Concurrent reads do not panic
// ---------------------------------------------------------------------------

#[test]
fn test_concurrent_reads_do_not_panic() {
    let graph = Arc::new(make_graph_with_triples(5));
    let barrier = Arc::new(Barrier::new(4));
    let mut handles = Vec::new();

    for i in 0..4 {
        let g = Arc::clone(&graph);
        let b = Arc::clone(&barrier);
        handles.push(thread::spawn(move || {
            b.wait();
            let result = g
                .query_cached(ALL_ITEMS_QUERY)
                .expect("query_cached in thread");
            // Each thread should see 5 items.
            assert_eq!(
                row_count(&result),
                5,
                "thread {i}: expected 5 rows, got {}",
                row_count(&result)
            );
        }));
    }

    for h in handles {
        h.join().expect("thread should not panic");
    }
}

// ---------------------------------------------------------------------------
// Test 3: Concurrent writes are visible across clones
// ---------------------------------------------------------------------------

#[test]
fn test_concurrent_writes_visible_across_clones() {
    let graph = Arc::new(Graph::new().expect("Graph::new"));
    let barrier = Arc::new(Barrier::new(2));

    // Thread A: insert after barrier.
    let writer = {
        let g = Arc::clone(&graph);
        let b = Arc::clone(&barrier);
        thread::spawn(move || {
            b.wait();
            g.insert_turtle(
                r#"@prefix ex: <http://example.org/> .
                   ex:alice ex:name "Alice" ."#,
            )
            .expect("insert_turtle in writer");
        })
    };

    // Thread B: query after barrier (and a small sleep to let A proceed).
    let reader = {
        let g = Arc::clone(&graph);
        let b = Arc::clone(&barrier);
        thread::spawn(move || {
            b.wait();
            // Give the writer a moment to insert.
            thread::sleep(std::time::Duration::from_millis(50));
            let result = g
                .query_cached(ALL_NAMED_QUERY)
                .expect("query_cached in reader");
            assert_eq!(
                row_count(&result),
                1,
                "reader should see the triple inserted by writer"
            );
        })
    };

    writer.join().expect("writer should not panic");
    reader.join().expect("reader should not panic");
}

// ---------------------------------------------------------------------------
// Test 4: Concurrent read + write does not deadlock
// ---------------------------------------------------------------------------

#[test]
fn test_concurrent_read_write_does_not_deadlock() {
    let graph = Arc::new(make_graph_with_triples(2));
    const ITERATIONS: usize = 50;

    let writer = {
        let g = Arc::clone(&graph);
        thread::spawn(move || {
            for i in 0..ITERATIONS {
                let turtle = format!(
                    r#"@prefix ex: <http://example.org/> .
                       ex:w{i} ex:value "{i}" ."#
                );
                g.insert_turtle(&turtle)
                    .expect("insert_turtle in writer loop");
            }
        })
    };

    let reader = {
        let g = Arc::clone(&graph);
        thread::spawn(move || {
            for _ in 0..ITERATIONS {
                let result = g
                    .query_cached(COUNT_QUERY)
                    .expect("query_cached in reader loop");
                // Just ensure we got a result (deadlock would hang here).
                let _n = extract_count(&result);
            }
        })
    };

    writer
        .join()
        .expect("writer thread panicked or deadlocked");
    reader
        .join()
        .expect("reader thread panicked or deadlocked");
}

// ---------------------------------------------------------------------------
// Test 5: Graph len() is consistent after concurrent inserts
// ---------------------------------------------------------------------------

#[test]
fn test_len_consistent_after_concurrent_inserts() {
    let graph = Arc::new(Graph::new().expect("Graph::new"));
    let thread_count = 8;
    let barrier = Arc::new(Barrier::new(thread_count));
    let mut handles = Vec::new();

    for i in 0..thread_count {
        let g = Arc::clone(&graph);
        let b = Arc::clone(&barrier);
        handles.push(thread::spawn(move || {
            b.wait();
            let turtle = format!(
                r#"@prefix ex: <http://example.org/> .
                   ex:t{i} ex:value "{i}" ."#
            );
            g.insert_turtle(&turtle)
                .expect("insert_turtle in concurrent len test");
        }));
    }

    for h in handles {
        h.join().expect("thread should not panic");
    }

    assert_eq!(
        graph.len(),
        thread_count,
        "expected {thread_count} triples after concurrent inserts, got {}",
        graph.len()
    );
}

// ---------------------------------------------------------------------------
// Test 6: Cache invalidation after concurrent insert
// ---------------------------------------------------------------------------

#[test]
fn test_cache_invalidation_after_concurrent_insert() {
    let graph = Graph::new().expect("Graph::new");

    // Insert initial data.
    graph
        .insert_turtle(
            r#"@prefix ex: <http://example.org/> .
               ex:alice ex:value "Alice" ."#,
        )
        .expect("initial insert");

    // Query (populates cache).
    let r1 = graph
        .query_cached(ALL_ITEMS_QUERY)
        .expect("first query");
    assert_eq!(
        row_count(&r1),
        1,
        "initial query should return 1 row"
    );

    // Insert more data (bumps epoch, invalidating cache).
    graph
        .insert_turtle(
            r#"@prefix ex: <http://example.org/> .
               ex:bob ex:value "Bob" ."#,
        )
        .expect("second insert");

    // Query again -- should see both triples.
    let r2 = graph
        .query_cached(ALL_ITEMS_QUERY)
        .expect("second query");
    assert_eq!(
        row_count(&r2),
        2,
        "after insert, query should return 2 rows (cache invalidated)"
    );
}

// ---------------------------------------------------------------------------
// Test 7: Multiple independent graphs do not interfere
// ---------------------------------------------------------------------------

#[test]
fn test_independent_graphs_do_not_interfere() {
    let graph_a = Graph::new().expect("Graph::new A");
    let graph_b = Graph::new().expect("Graph::new B");

    graph_a
        .insert_turtle(
            r#"@prefix ex: <http://example.org/> .
               ex:alpha ex:value "1" ."#,
        )
        .expect("insert into A");

    graph_b
        .insert_turtle(
            r#"@prefix ex: <http://example.org/> .
               ex:beta ex:value "2" ."#,
        )
        .expect("insert into B");

    assert_eq!(
        graph_a.len(),
        1,
        "graph_a should have exactly 1 triple"
    );
    assert_eq!(
        graph_b.len(),
        1,
        "graph_b should have exactly 1 triple"
    );

    let result_a = graph_a
        .query_cached("SELECT ?item WHERE { ?item <http://example.org/value> \"1\" }")
        .expect("query A");
    assert_eq!(row_count(&result_a), 1);

    let result_b = graph_b
        .query_cached("SELECT ?item WHERE { ?item <http://example.org/value> \"2\" }")
        .expect("query B");
    assert_eq!(row_count(&result_b), 1);
}

// ---------------------------------------------------------------------------
// Test 8: Concurrent query_cached with same query returns consistent results
// ---------------------------------------------------------------------------

#[test]
fn test_concurrent_identical_query_cached_returns_same_result() {
    let graph = Arc::new(make_graph_with_triples(3));
    let barrier = Arc::new(Barrier::new(4));
    let mut handles = Vec::new();

    for i in 0..4 {
        let g = Arc::clone(&graph);
        let b = Arc::clone(&barrier);
        handles.push(thread::spawn(move || {
            b.wait();
            let result = g
                .query_cached(ALL_ITEMS_QUERY)
                .expect("query_cached in identical query test");
            let count = row_count(&result);
            assert_eq!(count, 3, "thread {i}: expected 3 triples, got {count}");
        }));
    }

    for h in handles {
        h.join().expect("thread should not panic");
    }
}
