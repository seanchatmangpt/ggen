use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_backpressure::token::{TokenPool, WIPToken};
use tokio::runtime::Runtime;

fn bench_token_pool_creation(c: &mut Criterion) {
    c.bench_function("token_pool_creation", |b| {
        b.iter(|| {
            black_box(TokenPool::new(100));
        });
    });
}

fn bench_token_acquire_release(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let pool = TokenPool::new(100);

    c.bench_function("token_acquire_release", |b| {
        b.iter(|| {
            rt.block_on(async {
                let token = pool.acquire().await.unwrap();
                black_box(token);
            });
        });
    });
}

fn bench_token_try_acquire(c: &mut Criterion) {
    let pool = TokenPool::new(100);

    c.bench_function("token_try_acquire_success", |b| {
        b.iter(|| {
            let token = pool.try_acquire().unwrap();
            black_box(token);
        });
    });
}

fn bench_token_try_acquire_no_capacity(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let pool = TokenPool::new(1);
    let _held_token = rt.block_on(pool.acquire()).unwrap();

    c.bench_function("token_try_acquire_no_capacity", |b| {
        b.iter(|| {
            let token = pool.try_acquire().unwrap();
            black_box(token);
        });
    });
}

fn bench_pool_metrics(c: &mut Criterion) {
    let mut group = c.benchmark_group("pool_metrics");
    let pool = TokenPool::new(100);

    group.bench_function("capacity", |b| {
        b.iter(|| {
            black_box(pool.capacity());
        });
    });

    group.bench_function("available", |b| {
        b.iter(|| {
            black_box(pool.available());
        });
    });

    group.bench_function("in_flight", |b| {
        b.iter(|| {
            black_box(pool.in_flight());
        });
    });

    group.bench_function("utilization", |b| {
        b.iter(|| {
            black_box(pool.utilization());
        });
    });

    group.finish();
}

fn bench_token_metadata_access(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let pool = TokenPool::new(1);
    let token = rt.block_on(pool.acquire()).unwrap();

    let mut group = c.benchmark_group("token_metadata");

    group.bench_function("metadata_access", |b| {
        b.iter(|| {
            black_box(token.metadata());
        });
    });

    group.bench_function("elapsed", |b| {
        b.iter(|| {
            black_box(token.elapsed());
        });
    });

    group.finish();
}

fn bench_concurrent_acquire(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_acquire");

    for threads in [2, 4, 8, 16].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(threads),
            threads,
            |b, &threads| {
                let rt = Runtime::new().unwrap();
                let pool = TokenPool::new(100);

                b.iter(|| {
                    rt.block_on(async {
                        let mut handles = vec![];

                        for _ in 0..threads {
                            let pool = pool.clone();
                            let handle = tokio::spawn(async move {
                                let token = pool.acquire().await.unwrap();
                                black_box(token);
                            });
                            handles.push(handle);
                        }

                        for handle in handles {
                            handle.await.unwrap();
                        }
                    });
                });
            },
        );
    }

    group.finish();
}

fn bench_token_contention(c: &mut Criterion) {
    let mut group = c.benchmark_group("token_contention");

    for capacity in [1, 10, 100].iter() {
        group.throughput(Throughput::Elements((*capacity as u64) * 10));
        group.bench_with_input(
            BenchmarkId::new("high_contention", capacity),
            capacity,
            |b, &capacity| {
                let rt = Runtime::new().unwrap();
                let pool = TokenPool::new(capacity);

                b.iter(|| {
                    rt.block_on(async {
                        let mut handles = vec![];

                        // Spawn 10x capacity tasks to create contention
                        for _ in 0..(capacity * 10) {
                            let pool = pool.clone();
                            let handle = tokio::spawn(async move {
                                let token = pool.acquire().await.unwrap();
                                black_box(token);
                            });
                            handles.push(handle);
                        }

                        for handle in handles {
                            handle.await.unwrap();
                        }
                    });
                });
            },
        );
    }

    group.finish();
}

fn bench_admission_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("admission_throughput");

    for count in [100, 1000, 10000].iter() {
        let rt = Runtime::new().unwrap();
        let pool = TokenPool::new(*count);

        group.throughput(Throughput::Elements(*count as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(count),
            count,
            |b, &count| {
                b.iter(|| {
                    rt.block_on(async {
                        let mut tokens = vec![];
                        for _ in 0..count {
                            let token = pool.acquire().await.unwrap();
                            tokens.push(token);
                        }
                        black_box(tokens);
                    });
                });
            },
        );
    }

    group.finish();
}

fn bench_try_acquire_batch(c: &mut Criterion) {
    let mut group = c.benchmark_group("try_acquire_batch");

    for capacity in [10, 100, 1000].iter() {
        let pool = TokenPool::new(*capacity);

        group.throughput(Throughput::Elements(*capacity as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(capacity),
            capacity,
            |b, &capacity| {
                b.iter(|| {
                    let mut tokens = vec![];
                    for _ in 0..capacity {
                        if let Some(token) = pool.try_acquire().unwrap() {
                            tokens.push(token);
                        }
                    }
                    black_box(tokens);
                });
            },
        );
    }

    group.finish();
}

fn bench_utilization_tracking(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let pool = TokenPool::new(100);

    // Acquire 50% capacity
    let _tokens: Vec<WIPToken> = rt
        .block_on(async {
            let mut tokens = vec![];
            for _ in 0..50 {
                tokens.push(pool.acquire().await.unwrap());
            }
            tokens
        });

    c.bench_function("utilization_at_50_percent", |b| {
        b.iter(|| {
            black_box(pool.utilization());
            black_box(pool.in_flight());
            black_box(pool.available());
        });
    });
}

criterion_group!(
    benches,
    bench_token_pool_creation,
    bench_token_acquire_release,
    bench_token_try_acquire,
    bench_token_try_acquire_no_capacity,
    bench_pool_metrics,
    bench_token_metadata_access,
    bench_concurrent_acquire,
    bench_token_contention,
    bench_admission_throughput,
    bench_try_acquire_batch,
    bench_utilization_tracking,
);
criterion_main!(benches);
