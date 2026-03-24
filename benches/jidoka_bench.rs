use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_jidoka::gate::{CompilerGate, LintGate, TestGate};
use ggen_jidoka::{AndonSignal, ProductionLine};

fn bench_andon_signal_checks(c: &mut Criterion) {
    let mut group = c.benchmark_group("andon_signal_checks");

    group.bench_function("should_stop", |b| {
        let signal = AndonSignal::Red;
        b.iter(|| {
            black_box(signal.should_stop());
        });
    });

    group.bench_function("is_warning", |b| {
        let signal = AndonSignal::Yellow;
        b.iter(|| {
            black_box(signal.is_warning());
        });
    });

    group.bench_function("is_green", |b| {
        let signal = AndonSignal::Green;
        b.iter(|| {
            black_box(signal.is_green());
        });
    });

    group.finish();
}

fn bench_andon_signal_ordering(c: &mut Criterion) {
    c.bench_function("andon_signal_comparison", |b| {
        b.iter(|| {
            black_box(AndonSignal::Green < AndonSignal::Yellow);
            black_box(AndonSignal::Yellow < AndonSignal::Red);
        });
    });
}

fn bench_gate_creation(c: &mut Criterion) {
    let mut group = c.benchmark_group("gate_creation");

    group.bench_function("compiler_gate", |b| {
        b.iter(|| {
            black_box(CompilerGate::new("/home/user/ggen"));
        });
    });

    group.bench_function("test_gate", |b| {
        b.iter(|| {
            black_box(TestGate::new("/home/user/ggen"));
        });
    });

    group.bench_function("lint_gate", |b| {
        b.iter(|| {
            black_box(LintGate::new("/home/user/ggen"));
        });
    });

    group.finish();
}

fn bench_output_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("output_parsing");

    let compiler_outputs = vec![
        ("clean", "   Compiling ggen v6.0.0\n    Finished dev [unoptimized + debuginfo]"),
        ("warning", "warning: unused variable `x`\n --> src/main.rs:5:9"),
        ("error", "error[E0425]: cannot find value `y` in this scope\n --> src/main.rs:10:5"),
    ];

    for (name, output) in compiler_outputs {
        group.bench_with_input(BenchmarkId::new("compiler", name), output, |b, output| {
            b.iter(|| {
                // Simulate parsing logic from CompilerGate
                use regex::Regex;
                let error_pattern = Regex::new(r"error(\[E\d+\])?:").unwrap();
                let warning_pattern = Regex::new(r"warning:").unwrap();

                let signal = if error_pattern.is_match(output) {
                    AndonSignal::Red
                } else if warning_pattern.is_match(output) {
                    AndonSignal::Yellow
                } else {
                    AndonSignal::Green
                };
                black_box(signal);
            });
        });
    }

    group.finish();
}

fn bench_production_line_creation(c: &mut Criterion) {
    c.bench_function("production_line_new", |b| {
        b.iter(|| {
            black_box(ProductionLine::new());
        });
    });
}

fn bench_signal_display(c: &mut Criterion) {
    let mut group = c.benchmark_group("signal_display");

    for signal in [AndonSignal::Green, AndonSignal::Yellow, AndonSignal::Red] {
        group.bench_with_input(
            BenchmarkId::from_parameter(format!("{:?}", signal)),
            &signal,
            |b, signal| {
                b.iter(|| {
                    black_box(format!("{}", signal));
                });
            },
        );
    }

    group.finish();
}

fn bench_signal_serialization(c: &mut Criterion) {
    let mut group = c.benchmark_group("signal_serialization");

    let signals = vec![
        AndonSignal::Green,
        AndonSignal::Yellow,
        AndonSignal::Red,
    ];

    group.bench_function("serialize", |b| {
        b.iter(|| {
            for signal in &signals {
                black_box(serde_json::to_string(signal).unwrap());
            }
        });
    });

    group.bench_function("deserialize", |b| {
        let json_signals: Vec<String> = signals
            .iter()
            .map(|s| serde_json::to_string(s).unwrap())
            .collect();

        b.iter(|| {
            for json in &json_signals {
                black_box(serde_json::from_str::<AndonSignal>(json).unwrap());
            }
        });
    });

    group.finish();
}

fn bench_batch_signal_checks(c: &mut Criterion) {
    let mut group = c.benchmark_group("batch_signal_checks");

    for count in [10, 100, 1000].iter() {
        let signals: Vec<AndonSignal> = (0..*count)
            .map(|i| match i % 3 {
                0 => AndonSignal::Green,
                1 => AndonSignal::Yellow,
                _ => AndonSignal::Red,
            })
            .collect();

        group.throughput(Throughput::Elements(*count));
        group.bench_with_input(BenchmarkId::from_parameter(count), &signals, |b, signals| {
            b.iter(|| {
                let red_count = signals.iter().filter(|s| s.should_stop()).count();
                let yellow_count = signals.iter().filter(|s| s.is_warning()).count();
                let green_count = signals.iter().filter(|s| s.is_green()).count();
                black_box((red_count, yellow_count, green_count));
            });
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_andon_signal_checks,
    bench_andon_signal_ordering,
    bench_gate_creation,
    bench_output_parsing,
    bench_production_line_creation,
    bench_signal_display,
    bench_signal_serialization,
    bench_batch_signal_checks,
);
criterion_main!(benches);
