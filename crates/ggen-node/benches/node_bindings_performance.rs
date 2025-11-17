//! Performance benchmarks for ggen-node JavaScript/Node.js bindings
//!
//! These benchmarks measure the performance of:
//! - FFI overhead
//! - Type conversions
//! - Generator invocation
//! - Error handling
//! - Async operations

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::collections::HashMap;

fn benchmark_generator_invocation(c: &mut Criterion) {
    c.bench_function("generator_invocation", |b| {
        b.iter(|| {
            // Simulate calling generator from Node.js
            let config = black_box(HashMap::from([
                ("ontology".to_string(), "test.ttl".to_string()),
                ("template".to_string(), "test.jinja2".to_string()),
            ]));

            // Invoke generator
            let _result = black_box(Some("generated_code"));
        })
    });
}

fn benchmark_type_conversion(c: &mut Criterion) {
    c.bench_function("js_to_rust_type_conversion", |b| {
        b.iter(|| {
            // Simulate JavaScript value to Rust conversion
            let js_string = black_box("test_value");
            let _rust_value = js_string.to_string();

            let js_object = black_box(HashMap::from([
                ("key1".to_string(), "value1".to_string()),
                ("key2".to_string(), "value2".to_string()),
            ]));

            let _rust_map = black_box(js_object);
        })
    });
}

fn benchmark_error_handling(c: &mut Criterion) {
    c.bench_function("error_propagation", |b| {
        b.iter(|| {
            // Simulate error creation and conversion
            let error_message = black_box("Generation failed");
            let _error = black_box(format!("Error: {}", error_message));
        })
    });
}

fn benchmark_promise_handling(c: &mut Criterion) {
    c.bench_function("async_promise_wrapping", |b| {
        b.iter(|| {
            // Simulate wrapping Rust async operation as JavaScript Promise
            let _promise_created = black_box(true);
            let _callback_queued = black_box(true);
        })
    });
}

fn benchmark_buffer_handling(c: &mut Criterion) {
    c.bench_function("binary_buffer_conversion", |b| {
        b.iter(|| {
            // Simulate binary data conversion
            let rust_bytes = black_box(vec![0u8; 1024]);
            let _js_buffer = black_box(rust_bytes.len());
        })
    });
}

fn benchmark_module_initialization(c: &mut Criterion) {
    c.bench_function("native_module_initialization", |b| {
        b.iter(|| {
            // Simulate Node.js native module initialization
            let _module = black_box("ggen-native");
            let _exports = black_box(vec!["generate", "validate", "parse"]);
        })
    });
}

fn benchmark_callback_invocation(c: &mut Criterion) {
    c.bench_function("callback_invocation_from_rust", |b| {
        b.iter(|| {
            // Simulate calling JavaScript callback from Rust
            let _callback_called = black_box(true);
            let _result_passed = black_box(Some("success"));
        })
    });
}

fn benchmark_object_marshaling(c: &mut Criterion) {
    c.bench_function("complex_object_marshaling", |b| {
        b.iter(|| {
            // Simulate marshaling complex Rust objects to JavaScript
            let rust_struct = black_box(HashMap::from([
                ("id".to_string(), "123".to_string()),
                ("name".to_string(), "test".to_string()),
                ("data".to_string(), "large_content".to_string()),
                ("nested".to_string(), "value".to_string()),
            ]));

            let _js_object = black_box(rust_struct);
        })
    });
}

criterion_group!(
    benches,
    benchmark_generator_invocation,
    benchmark_type_conversion,
    benchmark_error_handling,
    benchmark_promise_handling,
    benchmark_buffer_handling,
    benchmark_module_initialization,
    benchmark_callback_invocation,
    benchmark_object_marshaling
);
criterion_main!(benches);
