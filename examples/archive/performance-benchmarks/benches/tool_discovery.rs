//! Benchmark tool discovery

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::collections::HashMap;

fn tool_discovery_benchmark(c: &mut Criterion) {
    c.bench_function("discover_4_tools", |b| {
        b.iter(|| {
            let mut tools = HashMap::new();
            tools.insert("search_web".to_string(), "search");
            tools.insert("get_time".to_string(), "time");
            tools.insert("store_data".to_string(), "store");
            tools.insert("retrieve_data".to_string(), "retrieve");
            black_box(tools);
        })
    });

    c.bench_function("search_tools_by_name", |b| {
        let mut tools = HashMap::new();
        tools.insert("search_web".to_string(), "search");
        tools.insert("get_time".to_string(), "time");
        tools.insert("store_data".to_string(), "store");
        tools.insert("retrieve_data".to_string(), "retrieve");
        
        b.iter(|| {
            let _found = tools.get("search_web");
        })
    });
}

criterion_group!(benches, tool_discovery_benchmark);
criterion_main!(benches);
