//! Domain Swarm Primitives Performance Benchmarks
//!
//! Benchmarks low-level coordination primitives:
//! 1. Lock-free snapshot reads
//! 2. Proposal aggregation (commutative & conditional)
//! 3. Priority scheduling
//! 4. Conflict detection

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_domain::swarm_coordination::{
    CommutativeProposal, ConditionalProposal, ConflictFree, LatencyTier, Priority,
    PriorityScheduler, ProposalAggregator, ResourceCost, ScheduledTask, SchedulingHint,
    SnapshotCell,
};

/// Benchmark lock-free snapshot reads
fn bench_snapshot_reads(c: &mut Criterion) {
    let mut group = c.benchmark_group("snapshot_reads");

    for num_readers in [10, 50, 100, 500, 1000].iter() {
        group.throughput(Throughput::Elements(*num_readers as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_readers),
            num_readers,
            |b, &num_readers| {
                let cell = SnapshotCell::new("snapshot-1");

                b.iter(|| {
                    let start = std::time::Instant::now();

                    for _ in 0..num_readers {
                        let snapshot = cell.current();
                        black_box(snapshot.version);
                    }

                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark snapshot staging and commit
fn bench_snapshot_writes(c: &mut Criterion) {
    let mut group = c.benchmark_group("snapshot_writes");

    for iterations in [10, 50, 100, 200].iter() {
        group.throughput(Throughput::Elements(*iterations as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(iterations),
            iterations,
            |b, &iterations| {
                b.iter(|| {
                    let mut cell = SnapshotCell::new("snapshot-1");
                    let start = std::time::Instant::now();

                    for i in 0..iterations {
                        cell.stage_next(format!("snapshot-{}", i));
                        cell.commit_staged().unwrap();
                    }

                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark commutative proposal aggregation
fn bench_commutative_aggregation(c: &mut Criterion) {
    let mut group = c.benchmark_group("commutative_aggregation");

    for num_proposals in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*num_proposals as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_proposals),
            num_proposals,
            |b, &num_proposals| {
                let proposals: Vec<_> = (0..num_proposals)
                    .map(|i| CommutativeProposal {
                        id: format!("p{}", i),
                        action: format!("action{}", i),
                        priority: if i % 2 == 0 {
                            Priority::High
                        } else {
                            Priority::Normal
                        },
                    })
                    .collect();

                b.iter(|| {
                    let start = std::time::Instant::now();
                    let _result = ProposalAggregator::aggregate_commutative(proposals.clone());
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark conditional proposal aggregation with conflicts
fn bench_conditional_aggregation(c: &mut Criterion) {
    let mut group = c.benchmark_group("conditional_aggregation");

    for num_proposals in [10, 50, 100, 200].iter() {
        group.throughput(Throughput::Elements(*num_proposals as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_proposals),
            num_proposals,
            |b, &num_proposals| {
                // Create proposals with varying conflict rates
                let proposals: Vec<_> = (0..num_proposals)
                    .map(|i| ConditionalProposal {
                        id: format!("p{}", i),
                        action: format!("action{}", i),
                        resource: format!("resource-{}", i % 10), // 10% conflict rate
                        priority: Priority::Normal,
                    })
                    .collect();

                b.iter(|| {
                    let start = std::time::Instant::now();
                    let _result = ProposalAggregator::aggregate_conditional(proposals.clone());
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark priority scheduler enqueue/dequeue
fn bench_priority_scheduling(c: &mut Criterion) {
    let mut group = c.benchmark_group("priority_scheduling");

    for num_tasks in [10, 50, 100, 500, 1000].iter() {
        group.throughput(Throughput::Elements(*num_tasks as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_tasks),
            num_tasks,
            |b, &num_tasks| {
                b.iter(|| {
                    let mut scheduler = PriorityScheduler::new();
                    let start = std::time::Instant::now();

                    // Enqueue tasks with mixed priorities
                    for i in 0..num_tasks {
                        let priority = match i % 4 {
                            0 => Priority::Critical,
                            1 => Priority::High,
                            2 => Priority::Normal,
                            _ => Priority::Low,
                        };

                        scheduler.enqueue(ScheduledTask::new(
                            format!("task-{}", i),
                            SchedulingHint::new(priority, LatencyTier::Hot, ResourceCost::Light),
                            "test task",
                        ));
                    }

                    // Dequeue all tasks
                    while let Some(_task) = scheduler.dequeue(false) {
                        black_box(_task);
                    }

                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark scheduler queue depth calculation
fn bench_scheduler_queue_depth(c: &mut Criterion) {
    let mut group = c.benchmark_group("scheduler_queue_depth");

    for queue_size in [100, 500, 1000, 5000].iter() {
        group.throughput(Throughput::Elements(*queue_size as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(queue_size),
            queue_size,
            |b, &queue_size| {
                let mut scheduler = PriorityScheduler::new();

                // Pre-fill scheduler
                for i in 0..queue_size {
                    scheduler.enqueue(ScheduledTask::new(
                        format!("task-{}", i),
                        SchedulingHint::new(
                            Priority::Normal,
                            LatencyTier::Warm,
                            ResourceCost::Moderate,
                        ),
                        "test task",
                    ));
                }

                b.iter(|| {
                    let start = std::time::Instant::now();
                    let depth = scheduler.queue_depth();
                    black_box(depth);
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark conflict detection in proposals
fn bench_conflict_detection(c: &mut Criterion) {
    let mut group = c.benchmark_group("conflict_detection");

    for num_proposals in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*num_proposals as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_proposals),
            num_proposals,
            |b, &num_proposals| {
                // Create proposals with high conflict rate
                let proposals: Vec<_> = (0..num_proposals)
                    .map(|i| ConditionalProposal {
                        id: format!("p{}", i),
                        action: "write".to_string(),
                        resource: format!("resource-{}", i % 3), // High conflict rate
                        priority: Priority::High,
                    })
                    .collect();

                b.iter(|| {
                    let start = std::time::Instant::now();

                    // Check all pairs for conflicts
                    let mut conflicts = 0;
                    for i in 0..proposals.len() {
                        for j in (i + 1)..proposals.len() {
                            if !proposals[i].is_commutative_with(&proposals[j]) {
                                conflicts += 1;
                            }
                        }
                    }

                    black_box(conflicts);
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark proposal merging performance
fn bench_proposal_merging(c: &mut Criterion) {
    let mut group = c.benchmark_group("proposal_merging");

    for merge_size in [2, 5, 10, 20].iter() {
        group.throughput(Throughput::Elements(*merge_size as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(merge_size),
            merge_size,
            |b, &merge_size| {
                let proposals: Vec<_> = (0..merge_size)
                    .map(|i| CommutativeProposal {
                        id: format!("p{}", i),
                        action: format!("action{}", i),
                        priority: Priority::Normal,
                    })
                    .collect();

                b.iter(|| {
                    let start = std::time::Instant::now();

                    // Sequential merging
                    let mut result = proposals[0].clone();
                    for proposal in &proposals[1..] {
                        result = result.try_merge(proposal).unwrap();
                    }

                    black_box(result);
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark scheduling hint sort key calculation
fn bench_scheduling_hint_sorting(c: &mut Criterion) {
    let mut group = c.benchmark_group("scheduling_hint_sorting");

    for num_hints in [100, 500, 1000, 5000].iter() {
        group.throughput(Throughput::Elements(*num_hints as u64));

        group.bench_with_input(
            BenchmarkId::from_parameter(num_hints),
            num_hints,
            |b, &num_hints| {
                let hints: Vec<_> = (0..num_hints)
                    .map(|i| {
                        let priority = match i % 4 {
                            0 => Priority::Critical,
                            1 => Priority::High,
                            2 => Priority::Normal,
                            _ => Priority::Low,
                        };
                        SchedulingHint::new(priority, LatencyTier::Warm, ResourceCost::Moderate)
                    })
                    .collect();

                b.iter(|| {
                    let start = std::time::Instant::now();

                    let mut keys: Vec<_> = hints.iter().map(|h| h.sort_key()).collect();
                    keys.sort_by(|a, b| b.cmp(a)); // Descending

                    black_box(keys);
                    black_box(start.elapsed())
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_snapshot_reads,
    bench_snapshot_writes,
    bench_commutative_aggregation,
    bench_conditional_aggregation,
    bench_priority_scheduling,
    bench_scheduler_queue_depth,
    bench_conflict_detection,
    bench_proposal_merging,
    bench_scheduling_hint_sorting,
);

criterion_main!(benches);
