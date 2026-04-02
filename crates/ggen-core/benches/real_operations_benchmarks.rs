use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_core::*;
use std::collections::BTreeMap;
use std::path::PathBuf;
use tempfile::TempDir;

fn bench_pipeline_creation(c: &mut Criterion) {
    c.bench_function("pipeline_new", |b| {
        b.iter(|| {
            let _pipeline = Pipeline::new().unwrap();
        })
    });
}

fn bench_lockfile_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("lockfile_operations");

    group.bench_function("lockfile_create", |b| {
        b.iter_batched(
            || TempDir::new().unwrap(),
            |temp_dir| {
                let manager = lockfile::LockfileManager::new(temp_dir.path());
                let _ = manager.create();
            },
            criterion::BatchSize::SmallInput,
        )
    });

    group.bench_function("lockfile_add_entry", |b| {
        b.iter_batched(
            || {
                let temp_dir = TempDir::new().unwrap();
                let manager = lockfile::LockfileManager::new(temp_dir.path());
                manager.create().ok();
                (temp_dir, manager)
            },
            |(_, manager)| {
                let _ = manager.upsert(
                    black_box("io.ggen.test"),
                    black_box("1.0.0"),
                    black_box("abc123"),
                    black_box("https://example.com"),
                );
            },
            criterion::BatchSize::SmallInput,
        )
    });

    group.finish();
}

fn bench_tera_rendering(c: &mut Criterion) {
    let mut group = c.benchmark_group("tera_rendering");

    let mut tera = tera::Tera::default();
    tera.add_raw_template("simple", "Hello {{ name }}!").ok();
    tera.add_raw_template(
        "complex",
        "{% for item in items %}\n  - {{ item }}\n{% endfor %}",
    )
    .ok();

    group.bench_function("render_simple_template", |b| {
        b.iter(|| {
            let mut ctx = tera::Context::new();
            ctx.insert("name", black_box("World"));
            let _ = tera.render("simple", &ctx);
        })
    });

    group.bench_function("render_with_loop", |b| {
        b.iter(|| {
            let mut ctx = tera::Context::new();
            ctx.insert("items", black_box(&vec!["a", "b", "c", "d", "e"]));
            let _ = tera.render("complex", &ctx);
        })
    });

    group.finish();
}

fn bench_bmap_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("bmap_operations");

    group.bench_function("bmap_insert_100", |b| {
        b.iter(|| {
            let mut map = BTreeMap::new();
            for i in 0..100 {
                map.insert(format!("key_{}", i), format!("value_{}", i));
            }
        })
    });

    group.bench_function("bmap_lookup_100", |b| {
        b.iter_batched(
            || {
                let mut map = BTreeMap::new();
                for i in 0..100 {
                    map.insert(format!("key_{}", i), format!("value_{}", i));
                }
                map
            },
            |map| {
                for i in 0..100 {
                    let _ = map.get(&format!("key_{}", i));
                }
            },
            criterion::BatchSize::SmallInput,
        )
    });

    group.bench_function("bmap_iteration_100", |b| {
        b.iter_batched(
            || {
                let mut map = BTreeMap::new();
                for i in 0..100 {
                    map.insert(format!("key_{}", i), format!("value_{}", i));
                }
                map
            },
            |map| {
                let _count = map.iter().count();
            },
            criterion::BatchSize::SmallInput,
        )
    });

    group.finish();
}

fn bench_string_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("string_operations");

    group.bench_function("string_format_simple", |b| {
        b.iter(|| {
            let _s = format!("value_{}", black_box(42));
        })
    });

    group.bench_function("string_format_with_path", |b| {
        b.iter(|| {
            let _s = format!("output/generated_{}.rs", black_box(42));
        })
    });

    group.bench_function("string_replace_simple", |b| {
        let template = "Hello {{ name }}, welcome to {{ place }}!";
        b.iter(|| {
            let result = template
                .replace("{{ name }}", black_box("Alice"))
                .replace("{{ place }}", black_box("ggen"));
            let _ = result;
        })
    });

    group.bench_function("string_clone_1kb", |b| {
        let s = "x".repeat(1024);
        b.iter(|| {
            let _cloned = s.clone();
        })
    });

    group.finish();
}

fn bench_path_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("path_operations");

    group.bench_function("path_join", |b| {
        let base = PathBuf::from("/home/user/project");
        b.iter(|| {
            let _path = base.join("src").join("main.rs");
        })
    });

    group.bench_function("path_canonicalize", |b| {
        b.iter_batched(
            || {
                let temp = TempDir::new().unwrap();
                temp.path().to_path_buf()
            },
            |path| {
                let _ = std::fs::canonicalize(&path);
            },
            criterion::BatchSize::SmallInput,
        )
    });

    group.bench_function("path_extension_check", |b| {
        let paths = vec![
            PathBuf::from("file.rs"),
            PathBuf::from("file.txt"),
            PathBuf::from("file.toml"),
        ];
        b.iter(|| {
            for path in &paths {
                let _ = path.extension();
            }
        })
    });

    group.finish();
}

fn bench_vec_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("vec_operations");

    group.bench_function("vec_push_100", |b| {
        b.iter(|| {
            let mut vec = Vec::new();
            for i in 0..100 {
                vec.push(i);
            }
        })
    });

    group.bench_function("vec_collect_100", |b| {
        b.iter(|| {
            let _vec: Vec<i32> = (0..100).collect();
        })
    });

    group.bench_function("vec_iter_sum", |b| {
        let vec: Vec<i32> = (0..100).collect();
        b.iter(|| {
            let _sum: i32 = vec.iter().sum();
        })
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_pipeline_creation,
    bench_lockfile_operations,
    bench_tera_rendering,
    bench_bmap_operations,
    bench_string_operations,
    bench_path_operations,
    bench_vec_operations,
);
criterion_main!(benches);
