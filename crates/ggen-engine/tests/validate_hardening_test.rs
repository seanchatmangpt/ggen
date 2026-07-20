//! Hardening proofs for `ggen graph validate` (DoD Blocker C): real
//! filesystem (`TempDir`), real `verbs::handlers::handle_graph_validate`
//! calls, real Turtle/SHACL fixtures — no mocks, matching
//! `lint_validate_e2e.rs`'s convention (this crate's `ggen-cli` binary is
//! the actual CLI entry point; `handle_graph_validate` is the real function
//! behind the `graph validate` route, so calling it directly exercises the
//! same code the CLI does, without a subprocess).
//!
//! Two tests, named to match this ticket's DoD:
//!
//! - [`validate_reaches_validator`] — proves the wiring is not decorative:
//!   a real Turtle parse error surfaces from the real validator (not a
//!   stub that always returns success), and a real valid file produces a
//!   real positive quad count and a real 64-hex BLAKE3 hash.
//! - [`validate_does_not_panic`] — the adversarial regression battery.
//!   Feeds `handle_graph_validate` a wide range of hostile inputs (empty
//!   file, whitespace-only, binary/non-UTF8 garbage, a missing path, a
//!   directory passed as a file, a symlink loop, and — the two inputs that
//!   were found to genuinely crash the process with a stack overflow
//!   before this ticket's fix — a long acyclic chain of `sh:node`-linked
//!   SHACL shapes and a long nested `sh:property` chain) and asserts every
//!   single one returns a typed `Result` (`Ok` for the one legitimately
//!   valid case, `Err` naming the problem for everything else), wrapped in
//!   `catch_unwind` so an ordinary Rust panic would also fail the test
//!   loudly instead of silently aborting the process.
//!
//! # The stack-overflow regression this file guards against
//!
//! Confirmed by direct adversarial testing against the built `ggen` binary
//! (2026-07-17, `ggen graph validate --files ... --shapes ...`): a shapes
//! graph with an acyclic chain of ~500-1000 distinct shapes linked by
//! `sh:node` (each shape unconditionally referencing the next), or a
//! shapes graph with ~200-500 shapes nested via `sh:property`, crashed the
//! whole process with `fatal runtime error: stack overflow, aborting`
//! (SIGABRT, exit 134) -- not a `panic!` that any `catch_unwind` could
//! intercept. Root cause: `praxis-graphlaw`'s mutually-recursive SHACL
//! evaluator (`shacl/validate.rs`'s `validate_shape` /
//! `conforms_to_shape` / `validate_property_shape`) had cycle detection
//! for a shape being *revisited* (a true cycle), but nothing bounding a
//! long *acyclic* chain, and `validate_property_shape`'s nested
//! `sh:property` recursion had no cycle/depth guard at all. Fixed by
//! `MAX_SHACL_VALIDATION_DEPTH` in `shacl/validate.rs`: crossing it now
//! fails closed with a real SHACL violation naming the exceeded depth,
//! instead of recursing toward a crash. This file's
//! `validate_does_not_panic` exercises both chain shapes (`sh:node` and
//! nested `sh:property`) well past the pre-fix crash threshold, and
//! [`validate_shacl_depth_guard_is_safe_on_small_stack_thread`] additionally
//! proves the guard's depth value is conservative enough to be safe even on
//! a deliberately small (1 MiB) thread stack -- an initial value tuned only
//! against a process main thread's larger default stack was *not* safe on
//! smaller stacks (see that test's doc comment for the concrete regression
//! this caught).

#![allow(clippy::expect_used)]

use camino::Utf8PathBuf;
use ggen_engine::verbs::handlers::handle_graph_validate;
use tempfile::TempDir;

fn utf8(p: std::path::PathBuf) -> Utf8PathBuf {
    Utf8PathBuf::from_path_buf(p).expect("utf8 path")
}

/// Build a Turtle SHACL shapes document encoding an acyclic chain of `n`
/// distinct `sh:NodeShape`s, each linked to the next via `sh:node`
/// (`Shape0 -> Shape1 -> ... -> Shape{n-1} -> ShapeLeaf`). No shape ever
/// references an earlier one, so `validate_shape`'s `visited`-based cycle
/// detection cannot bound this: without a depth guard, recursion depth
/// grows linearly with `n`.
fn shacl_node_chain(n: usize) -> String {
    let mut doc = String::from(
        "@prefix sh: <http://www.w3.org/ns/shacl#> .\n@prefix ex: <http://example.org/> .\n",
    );
    for i in 0..n {
        let next = if i + 1 < n {
            format!("ex:Shape{}", i + 1)
        } else {
            "ex:ShapeLeaf".to_string()
        };
        doc.push_str(&format!(
            "ex:Shape{i} a sh:NodeShape ; sh:targetNode ex:node{i} ; sh:node {next} .\n"
        ));
    }
    doc.push_str("ex:ShapeLeaf a sh:NodeShape .\n");
    doc
}

/// Build a Turtle SHACL shapes document nesting `n` `sh:property` shapes,
/// each sharing the same path predicate `ex:p`, under one root node shape
/// targeting `ex:node0`. `validate_property_shape` has *no* cycle
/// detection at all (unlike `validate_shape`), so this exercises that
/// independent recursion vector.
fn shacl_property_chain(n: usize) -> String {
    let mut doc = String::from(
        "@prefix sh: <http://www.w3.org/ns/shacl#> .\n@prefix ex: <http://example.org/> .\n\
         ex:RootShape a sh:NodeShape ; sh:targetNode ex:node0 ; sh:property ex:ps0 .\n",
    );
    for i in 0..n {
        if i + 1 < n {
            doc.push_str(&format!(
                "ex:ps{i} sh:path ex:p ; sh:property ex:ps{} .\n",
                i + 1
            ));
        } else {
            doc.push_str(&format!("ex:ps{i} sh:path ex:p .\n"));
        }
    }
    doc
}

// ---------------------------------------------------------------------
// validate_reaches_validator
// ---------------------------------------------------------------------

/// `handle_graph_validate` genuinely reaches the real Turtle/SHACL
/// validator rather than a stub that always reports success: a
/// syntactically invalid Turtle file fails closed with a real parser
/// diagnostic naming the offending path, and a well-formed file produces a
/// real positive quad count plus a real 64-hex BLAKE3 state hash (not a
/// constant/placeholder value).
#[test]
fn validate_reaches_validator() {
    let dir = TempDir::new().expect("tempdir");

    // A real parse error must surface, naming the file.
    let bad_path = dir.path().join("broken.ttl");
    std::fs::write(
        &bad_path,
        "@prefix ex: <http://example.org/>\nex:a ex:p ex:b",
    )
    .expect("write broken.ttl");
    let err = handle_graph_validate(vec![utf8(bad_path)], vec![])
        .expect_err("syntactically invalid turtle must fail closed");
    let msg = err.to_string();
    assert!(msg.contains("broken.ttl"), "names the file: {msg}");
    assert!(
        msg.to_lowercase().contains("turtle") || msg.contains("FM-GRAPH"),
        "names a real parser diagnostic, not a generic/decorative failure: {msg}"
    );

    // A well-formed file must produce real, non-trivial output: a positive
    // quad count and a genuine 64-hex hash -- values a stub could not
    // plausibly fabricate consistently across distinct inputs.
    let good_path = dir.path().join("good.ttl");
    std::fs::write(
        &good_path,
        "@prefix ex: <http://example.org/> .\nex:a ex:p \"1\" .\nex:a ex:q \"2\" .\n",
    )
    .expect("write good.ttl");
    let out = handle_graph_validate(vec![utf8(good_path)], vec![])
        .expect("well-formed turtle must validate");
    let files = out["files"].as_array().expect("files array");
    assert_eq!(files.len(), 1, "{out}");
    assert_eq!(
        files[0]["quads"].as_u64(),
        Some(2),
        "real quad count: {out}"
    );
    let hash = files[0]["hash"].as_str().expect("hash string");
    assert_eq!(hash.len(), 64, "real 64-hex hash: {out}");
    assert!(
        hash.chars().all(|c| c.is_ascii_hexdigit()),
        "hex hash: {out}"
    );

    // A different (larger) valid graph must hash differently -- a
    // constant/placeholder hash would collide here.
    let good2_path = dir.path().join("good2.ttl");
    std::fs::write(
        &good2_path,
        "@prefix ex: <http://example.org/> .\nex:a ex:p \"1\" .\n",
    )
    .expect("write good2.ttl");
    let out2 = handle_graph_validate(vec![utf8(good2_path)], vec![])
        .expect("second well-formed turtle must validate");
    let hash2 = out2["files"][0]["hash"].as_str().expect("hash2 string");
    assert_ne!(hash, hash2, "distinct graphs must hash differently");
}

// ---------------------------------------------------------------------
// validate_does_not_panic
// ---------------------------------------------------------------------

/// `handle_graph_validate` never panics or crashes the process on hostile
/// input -- it only ever returns a typed `Result`: `Ok` for the one
/// legitimately valid case in this battery, `Err` naming the problem for
/// everything else. Every case is wrapped in `catch_unwind` so an ordinary
/// (catchable) panic fails this test loudly rather than silently; the
/// `sh:node`/nested-`sh:property` chain cases are the direct regression
/// proof for the stack-overflow bug fixed alongside this test (see the
/// module doc comment) -- they crashed the whole process (uncatchable by
/// `catch_unwind`) before `MAX_SHACL_VALIDATION_DEPTH` existed, so simply
/// reaching the assertions below for those two cases is itself part of
/// the proof.
#[test]
fn validate_does_not_panic() {
    let dir = TempDir::new().expect("tempdir");

    let run = |files: Vec<Utf8PathBuf>, shapes: Vec<Utf8PathBuf>, label: &str| {
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            handle_graph_validate(files, shapes)
        }));
        result.unwrap_or_else(|payload| {
            let msg = payload
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| payload.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "<non-string panic payload>".to_string());
            panic!("{label}: handle_graph_validate panicked instead of returning Err: {msg}");
        })
    };

    // 1. Empty (0-byte) file: valid empty Turtle, must succeed with 0 quads.
    let empty_path = dir.path().join("empty.ttl");
    std::fs::write(&empty_path, "").expect("write empty.ttl");
    let out =
        run(vec![utf8(empty_path)], vec![], "empty file").expect("empty file is valid (0 quads)");
    assert_eq!(out["files"][0]["quads"].as_u64(), Some(0), "{out}");

    // 2. Whitespace-only file: also valid Turtle (comments/whitespace only).
    let ws_path = dir.path().join("whitespace.ttl");
    std::fs::write(&ws_path, "   \n\t\n   \n").expect("write whitespace.ttl");
    run(vec![utf8(ws_path)], vec![], "whitespace-only file")
        .expect("whitespace-only file is valid (0 quads)");

    // 3. Binary garbage / invalid UTF-8: must fail closed, never panic.
    let binary_path = dir.path().join("binary.ttl");
    let garbage: Vec<u8> = (0u8..=255).cycle().take(4096).collect();
    std::fs::write(&binary_path, &garbage).expect("write binary.ttl");
    let err = run(vec![utf8(binary_path)], vec![], "binary garbage")
        .expect_err("binary garbage must fail closed");
    assert!(
        err.to_string().contains("binary.ttl"),
        "names the file: {err}"
    );

    // 4. Nonexistent path: must fail closed, never panic.
    let missing_path = utf8(dir.path().join("does-not-exist.ttl"));
    let err = run(vec![missing_path], vec![], "nonexistent path")
        .expect_err("missing file must fail closed");
    assert!(
        err.to_string().contains("does-not-exist.ttl"),
        "names the missing file: {err}"
    );

    // 5. A directory passed where a file is expected: must fail closed.
    let dir_as_file = dir.path().join("a-directory.ttl");
    std::fs::create_dir_all(&dir_as_file).expect("mkdir a-directory.ttl");
    let err = run(vec![utf8(dir_as_file)], vec![], "directory as file")
        .expect_err("directory-as-file must fail closed");
    assert!(
        err.to_string().contains("a-directory.ttl"),
        "names the path: {err}"
    );

    // 6. Symlink loop: must fail closed (I/O error), never panic or hang.
    #[cfg(unix)]
    {
        let loop_a = dir.path().join("loop_a.ttl");
        let loop_b = dir.path().join("loop_b.ttl");
        std::os::unix::fs::symlink(&loop_b, &loop_a).expect("symlink a->b");
        std::os::unix::fs::symlink(&loop_a, &loop_b).expect("symlink b->a");
        let err = run(vec![utf8(loop_a)], vec![], "symlink loop")
            .expect_err("symlink loop must fail closed");
        assert!(
            err.to_string().to_lowercase().contains("loop")
                || err.to_string().to_lowercase().contains("symbolic"),
            "names the symlink problem: {err}"
        );
    }

    // 7. Deeply nested RDF collection (parser stress, not a validator
    // recursion issue): a moderate depth that stays fast, confirming
    // pathological-but-syntactically-valid Turtle parses without panicking.
    // (A much deeper collection is merely *slow* -- oxigraph's own Turtle
    // parser, not a crash -- and is out of scope for a fast unit test.)
    let nested_path = dir.path().join("nested_collection.ttl");
    let n = 300;
    let doc = format!(
        "@prefix ex: <http://example.org/> .\nex:a ex:hasList {}ex:leaf{} .\n",
        "(".repeat(n),
        ")".repeat(n)
    );
    std::fs::write(&nested_path, doc).expect("write nested_collection.ttl");
    run(
        vec![utf8(nested_path)],
        vec![],
        "deeply nested RDF collection",
    )
    .expect("moderately nested collection must parse without panicking");

    // 8. THE regression: a long acyclic sh:node chain, well past the
    // pre-fix stack-overflow threshold (~500-1000). Must fail closed via
    // the real SHACL depth guard, not crash.
    let data_path = dir.path().join("chain_data.ttl");
    std::fs::write(
        &data_path,
        "@prefix ex: <http://example.org/> .\nex:node0 ex:p \"x\" .\n",
    )
    .expect("write chain_data.ttl");
    let node_shapes_path = dir.path().join("node_chain_shapes.ttl");
    std::fs::write(&node_shapes_path, shacl_node_chain(2000)).expect("write node chain shapes");
    let err = run(
        vec![utf8(data_path.clone())],
        vec![utf8(node_shapes_path)],
        "2000-shape sh:node chain",
    )
    .expect_err("depth-exceeding sh:node chain must fail closed, not crash");
    // `sh:node` failures are reported via `conforms_to_shape`'s bool
    // summary (its own `temp` results buffer is discarded once collapsed
    // to true/false -- see `shacl/validate.rs`), so the *specific*
    // depth-guard message text is not expected to survive to this
    // top-level aggregate for this constraint kind; what matters here is
    // exactly what's asserted: a real, named SHACL failure, not a crash.
    assert!(
        err.to_string().contains("SHACL validation failed"),
        "real SHACL failure reported, not a crash: {err}"
    );
    assert!(
        err.to_string().contains("constraint violated"),
        "names a real violation, not a decorative/empty failure: {err}"
    );

    // 9. THE second regression: a long nested sh:property chain (no
    // sh:node at all) -- validate_property_shape's independent, previously
    // unguarded recursion vector. Data has a self-loop so every nesting
    // level's path evaluation stays non-empty and the full chain depth is
    // actually exercised.
    let selfloop_data_path = dir.path().join("selfloop_data.ttl");
    std::fs::write(
        &selfloop_data_path,
        "@prefix ex: <http://example.org/> .\nex:node0 ex:p ex:node0 .\n",
    )
    .expect("write selfloop_data.ttl");
    let prop_shapes_path = dir.path().join("property_chain_shapes.ttl");
    std::fs::write(&prop_shapes_path, shacl_property_chain(1000))
        .expect("write property chain shapes");
    let err = run(
        vec![utf8(selfloop_data_path)],
        vec![utf8(prop_shapes_path)],
        "1000-deep nested sh:property chain",
    )
    .expect_err("depth-exceeding sh:property chain must fail closed, not crash");
    assert!(
        err.to_string().contains("maximum safe validation depth"),
        "names the depth guard, proving it (not a crash) is what stopped recursion: {err}"
    );

    // 10. Sanity: a genuine SHACL shape *cycle* (A -> B -> A via sh:node)
    // still conforms cleanly via existing cycle detection -- the depth
    // guard must not misfire on legitimate small/cyclic shapes graphs.
    let cycle_shapes_path = dir.path().join("cycle_shapes.ttl");
    std::fs::write(
        &cycle_shapes_path,
        "@prefix sh: <http://www.w3.org/ns/shacl#> .\n\
         @prefix ex: <http://example.org/> .\n\
         ex:ShapeA a sh:NodeShape ; sh:targetNode ex:node0 ; sh:node ex:ShapeB .\n\
         ex:ShapeB a sh:NodeShape ; sh:targetNode ex:node0 ; sh:node ex:ShapeA .\n",
    )
    .expect("write cycle_shapes.ttl");
    let out = run(
        vec![utf8(data_path)],
        vec![utf8(cycle_shapes_path)],
        "genuine 2-shape sh:node cycle",
    )
    .expect("a true (short) shape cycle must still conform via cycle detection");
    assert_eq!(out["files"][0]["shapes_conform"], true, "{out}");
}

/// Direct proof that `MAX_SHACL_VALIDATION_DEPTH` is safe independent of
/// the *calling* thread's stack size, not merely safe on whatever default
/// stack this test harness happens to give its worker threads.
///
/// This is not a hypothetical concern: an earlier version of the fix set
/// the depth guard to a value that was safe when exercised from a
/// process's 8MB main thread (via the built `ggen` CLI binary) but still
/// stack-overflowed the whole process when the *exact same* adversarial
/// shapes graphs were validated from a plain `cargo test` worker thread
/// (whose default stack is a fraction of that, ~2MB) -- i.e. the bug this
/// whole file targets. A stack overflow aborts the entire process
/// regardless of which thread triggers it, so this test's real assertion
/// is simply that the process survives running these adversarial cases on
/// a thread with a deliberately small, explicit stack: 1 MiB, smaller than
/// this test harness's own default worker-thread stack, standing in for
/// any small-stack calling context (e.g. a constrained async runtime
/// worker) `handle_graph_validate` might run under in practice.
#[test]
fn validate_shacl_depth_guard_is_safe_on_small_stack_thread() {
    let dir = TempDir::new().expect("tempdir");

    let node_data_path = dir.path().join("chain_data.ttl");
    std::fs::write(
        &node_data_path,
        "@prefix ex: <http://example.org/> .\nex:node0 ex:p \"x\" .\n",
    )
    .expect("write chain_data.ttl");
    let node_shapes_path = dir.path().join("node_chain_shapes.ttl");
    std::fs::write(&node_shapes_path, shacl_node_chain(2000)).expect("write node chain shapes");

    let prop_data_path = dir.path().join("selfloop_data.ttl");
    std::fs::write(
        &prop_data_path,
        "@prefix ex: <http://example.org/> .\nex:node0 ex:p ex:node0 .\n",
    )
    .expect("write selfloop_data.ttl");
    let prop_shapes_path = dir.path().join("property_chain_shapes.ttl");
    std::fs::write(&prop_shapes_path, shacl_property_chain(1000))
        .expect("write property chain shapes");

    let node_files = vec![utf8(node_data_path)];
    let node_shapes = vec![utf8(node_shapes_path)];
    let prop_files = vec![utf8(prop_data_path)];
    let prop_shapes = vec![utf8(prop_shapes_path)];

    const SMALL_STACK: usize = 1024 * 1024; // 1 MiB
    let handle = std::thread::Builder::new()
        .stack_size(SMALL_STACK)
        .spawn(move || {
            let node_result = handle_graph_validate(node_files, node_shapes);
            let prop_result = handle_graph_validate(prop_files, prop_shapes);
            (node_result.is_err(), prop_result.is_err())
        })
        .expect("spawn small-stack thread");

    let (node_failed_closed, prop_failed_closed) = handle
        .join()
        .expect("small-stack thread must not panic or crash the process");
    assert!(
        node_failed_closed,
        "sh:node chain must fail closed (Err) even on a 1MiB stack thread"
    );
    assert!(
        prop_failed_closed,
        "sh:property chain must fail closed (Err) even on a 1MiB stack thread"
    );
}
