use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_test_opt::{TestId, TestValueScorer};
use std::collections::HashMap;

fn build_dataset(size: usize) -> HashMap<TestId, (u32, u32, usize, u64, Vec<String>)> {
    let mut data = HashMap::with_capacity(size);
    for i in 0..size {
        let id = TestId::new(format!("test_{i}")).expect("valid test id");
        let fail_count = ((i % 7) as u32) + 1;
        let runs = 40 + (i as u32 % 30);
        let lines = 80 + (i * 5);
        let exec_time_ms = 50 + (i as u64 * 3);
        let critical_paths = match i % 5 {
            0 => vec!["crates/ggen-rdf/src/parser.rs".to_string()],
            1 => vec!["crates/ggen-config/src/ggen.toml".to_string()],
            2 => vec!["crates/ggen-core/src/template.rs".to_string()],
            3 => vec!["crates/ggen-cli/src/cmds/workflow.rs".to_string()],
            _ => vec!["crates/ggen-utils/src/lib.rs".to_string()],
        };
        data.insert(id, (fail_count, runs, lines, exec_time_ms, critical_paths));
    }
    data
}

fn bench_value_scoring(c: &mut Criterion) {
    let scorer = TestValueScorer::new();
    let dataset = build_dataset(200);
    let total_codebase_lines = 200_000;
    let max_exec_time_ms = 2_000;
    let budget_ms = 1_000;

    c.bench_function("score_tests_200", |b| {
        b.iter(|| {
            let scores = scorer.score_tests(
                black_box(&dataset),
                total_codebase_lines,
                max_exec_time_ms,
                budget_ms,
            );
            black_box(scores);
        })
    });
}

criterion_group!(benches, bench_value_scoring);
criterion_main!(benches);
