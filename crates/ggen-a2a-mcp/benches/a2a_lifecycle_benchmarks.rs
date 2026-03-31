//! A2A Task Lifecycle Benchmarks
//!
//! Criterion benchmarks covering the full A2A task lifecycle:
//! - Task creation, retrieval, status transitions, cancellation
//! - Task listing and pagination
//! - Message sending and history retrieval
//! - Batch task creation throughput
//! - History length bounds enforcement
//!
//! Uses the real domain types from `a2a_generated` (Task, TaskStatus, Message, etc.)
//! and a lightweight async TaskStore that mirrors the `A2aLlmClient` storage pattern
//! (HashMap<String, TaskContext> behind tokio::sync::RwLock).

use std::collections::HashMap;
use std::hint::black_box;
use std::sync::Arc;

use a2a_generated::{
    message::{Message, MessageType},
    task::{Task, TaskPriority, TaskStatus},
};
use criterion::{criterion_group, criterion_main, Criterion};
use tokio::sync::RwLock;

// ---------------------------------------------------------------------------
// Lightweight in-memory task store (mirrors A2aLlmClient::active_tasks)
// ---------------------------------------------------------------------------

/// Per-task execution context, mirroring `client::TaskContext`.
#[allow(dead_code)]
#[derive(Debug, Clone)]
struct TaskContext {
    task_id: String,
    created_at: std::time::Instant,
    status: LifecycleStatus,
    retry_count: usize,
    messages: Vec<Message>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
enum LifecycleStatus {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
}

/// In-memory async task store backed by HashMap + RwLock.
struct InMemoryTaskStore {
    tasks: RwLock<HashMap<String, TaskContext>>,
}

impl InMemoryTaskStore {
    fn new() -> Self {
        Self {
            tasks: RwLock::new(HashMap::new()),
        }
    }

    async fn create_task(&self, task_id: String) {
        let ctx = TaskContext {
            task_id: task_id.clone(),
            created_at: std::time::Instant::now(),
            status: LifecycleStatus::Pending,
            retry_count: 0,
            messages: Vec::new(),
        };
        self.tasks.write().await.insert(task_id, ctx);
    }

    async fn get_task(&self, task_id: &str) -> Option<TaskContext> {
        self.tasks.read().await.get(task_id).cloned()
    }

    async fn update_status(&self, task_id: &str, status: LifecycleStatus) -> bool {
        let mut tasks = self.tasks.write().await;
        if let Some(ctx) = tasks.get_mut(task_id) {
            ctx.status = status;
            true
        } else {
            false
        }
    }

    async fn cancel_task(&self, task_id: &str) -> bool {
        self.update_status(task_id, LifecycleStatus::Cancelled)
            .await
    }

    async fn list_tasks(&self) -> Vec<TaskContext> {
        self.tasks.read().await.values().cloned().collect()
    }

    async fn list_tasks_limited(&self, limit: usize) -> Vec<TaskContext> {
        self.tasks
            .read()
            .await
            .values()
            .take(limit)
            .cloned()
            .collect()
    }

    async fn send_message(&self, task_id: &str, message: Message) -> bool {
        let mut tasks = self.tasks.write().await;
        if let Some(ctx) = tasks.get_mut(task_id) {
            ctx.messages.push(message);
            true
        } else {
            false
        }
    }

    async fn get_message_history(&self, task_id: &str, limit: Option<usize>) -> Vec<Message> {
        let tasks = self.tasks.read().await;
        if let Some(ctx) = tasks.get(task_id) {
            match limit {
                Some(n) => ctx.messages.iter().rev().take(n).cloned().collect(),
                None => ctx.messages.clone(),
            }
        } else {
            Vec::new()
        }
    }

    async fn enforce_history_length(&self, task_id: &str, max_length: usize) -> usize {
        let mut tasks = self.tasks.write().await;
        if let Some(ctx) = tasks.get_mut(task_id) {
            if ctx.messages.len() > max_length {
                let overflow = ctx.messages.len() - max_length;
                ctx.messages.drain(..overflow);
                overflow
            } else {
                0
            }
        } else {
            0
        }
    }

    async fn task_count(&self) -> usize {
        self.tasks.read().await.len()
    }
}

// ---------------------------------------------------------------------------
// Helper: create domain values (from a2a_generated)
// ---------------------------------------------------------------------------

/// Create a domain Task value (from a2a_generated::task).
fn make_task(id: &str) -> Task {
    Task::new(
        id.to_string(),
        format!("Benchmark Task {}", id),
        "benchmark".to_string(),
        serde_json::json!({"input": "benchmark"}),
    )
}

/// Create a domain Message value (from a2a_generated::message).
fn make_message(id: &str, task_id: &str) -> Message {
    Message::new(
        id.to_string(),
        MessageType::TaskRequest,
        "bench-agent".to_string(),
        Some("target-agent".to_string()),
        serde_json::json!({"task_id": task_id, "content": "benchmark payload"}),
    )
}

// ---------------------------------------------------------------------------
// Benchmarks (async task lifecycle)
// ---------------------------------------------------------------------------

/// (a) bench_task_create -- Create a task in InMemoryTaskStore, measure insertion
fn bench_task_create(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    c.bench_function("a2a_lifecycle/task_create", |b| {
        b.to_async(&rt).iter(|| {
            let store = Arc::new(InMemoryTaskStore::new());
            async move {
                let id = uuid::Uuid::new_v4().to_string();
                store.create_task(id).await;
                black_box(&store);
            }
        });
    });
}

/// (b) bench_task_get -- Retrieve task by ID (O(1) HashMap lookup)
fn bench_task_get(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let store = Arc::new(InMemoryTaskStore::new());
    rt.block_on(async {
        store.create_task("lookup-target".to_string()).await;
    });

    c.bench_function("a2a_lifecycle/task_get", |b| {
        let store = Arc::clone(&store);
        b.to_async(&rt).iter(|| {
            let store = Arc::clone(&store);
            async move {
                let result = store.get_task("lookup-target").await;
                black_box(result);
            }
        });
    });
}

/// (c) bench_task_update_status -- Transition task: Pending -> Running -> Completed
fn bench_task_update_status(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    c.bench_function("a2a_lifecycle/task_update_status", |b| {
        b.to_async(&rt).iter(|| {
            let store = Arc::new(InMemoryTaskStore::new());
            async move {
                let id = uuid::Uuid::new_v4().to_string();
                store.create_task(id.clone()).await;
                store.update_status(&id, LifecycleStatus::Running).await;
                store.update_status(&id, LifecycleStatus::Completed).await;
                black_box(store.get_task(&id).await);
            }
        });
    });
}

/// (d) bench_task_cancel -- Cancel a Running task
fn bench_task_cancel(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    c.bench_function("a2a_lifecycle/task_cancel", |b| {
        b.to_async(&rt).iter(|| {
            let store = Arc::new(InMemoryTaskStore::new());
            async move {
                let id = uuid::Uuid::new_v4().to_string();
                store.create_task(id.clone()).await;
                store.update_status(&id, LifecycleStatus::Running).await;
                let cancelled = store.cancel_task(&id).await;
                black_box(cancelled);
            }
        });
    });
}

/// (e) bench_task_list -- List all tasks (pre-populate 100 tasks inside iter)
fn bench_task_list(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    c.bench_function("a2a_lifecycle/task_list", |b| {
        b.to_async(&rt).iter(|| {
            let store = Arc::new(InMemoryTaskStore::new());
            async move {
                for _ in 0..100u32 {
                    let id = uuid::Uuid::new_v4().to_string();
                    store.create_task(id).await;
                }
                let all = store.list_tasks().await;
                black_box(all);
            }
        });
    });
}

/// (f) bench_task_list_pagination -- List tasks with limit, measure pagination
fn bench_task_list_pagination(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let mut group = c.benchmark_group("a2a_lifecycle/task_list_pagination");

    for limit in [10usize, 25, 50].iter() {
        group.bench_with_input(
            criterion::BenchmarkId::from_parameter(limit),
            limit,
            |b, &limit| {
                b.to_async(&rt).iter(|| {
                    let store = Arc::new(InMemoryTaskStore::new());
                    async move {
                        for _ in 0..100u32 {
                            let id = uuid::Uuid::new_v4().to_string();
                            store.create_task(id).await;
                        }
                        let page = store.list_tasks_limited(limit).await;
                        black_box(page);
                    }
                });
            },
        );
    }

    group.finish();
}

/// (g) bench_message_send -- Send message to existing task
fn bench_message_send(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    c.bench_function("a2a_lifecycle/message_send", |b| {
        b.to_async(&rt).iter(|| {
            let store = Arc::new(InMemoryTaskStore::new());
            async move {
                let task_id = uuid::Uuid::new_v4().to_string();
                store.create_task(task_id.clone()).await;

                let msg_id = uuid::Uuid::new_v4().to_string();
                let msg = make_message(&msg_id, &task_id);
                let sent = store.send_message(&task_id, msg).await;
                black_box(sent);
            }
        });
    });
}

/// (h) bench_message_history -- Retrieve message history (pre-populate messages)
fn bench_message_history(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let mut group = c.benchmark_group("a2a_lifecycle/message_history");

    for msg_count in [10usize, 50].iter() {
        group.bench_with_input(
            criterion::BenchmarkId::from_parameter(msg_count),
            msg_count,
            |b, &msg_count| {
                b.to_async(&rt).iter(|| {
                    let store = Arc::new(InMemoryTaskStore::new());
                    async move {
                        let task_id = uuid::Uuid::new_v4().to_string();
                        store.create_task(task_id.clone()).await;
                        for i in 0..msg_count {
                            let msg = make_message(&format!("hist-msg-{}", i), &task_id);
                            store.send_message(&task_id, msg).await;
                        }
                        let history = store.get_message_history(&task_id, None).await;
                        black_box(history);
                    }
                });
            },
        );
    }

    group.finish();
}

/// (i) bench_batch_create_tasks -- Create 100 tasks in batch, measure throughput
fn bench_batch_create_tasks(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    c.bench_function("a2a_lifecycle/batch_create_tasks", |b| {
        b.to_async(&rt).iter(|| {
            let store = Arc::new(InMemoryTaskStore::new());
            async move {
                for _ in 0..100u32 {
                    let id = uuid::Uuid::new_v4().to_string();
                    store.create_task(id).await;
                }
                let count = store.task_count().await;
                black_box(count);
            }
        });
    });
}

/// (j) bench_task_history_length_bounds -- Measure history_length parameter enforcement
fn bench_task_history_length_bounds(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    c.bench_function("a2a_lifecycle/task_history_length_bounds", |b| {
        b.to_async(&rt).iter(|| {
            let store = Arc::new(InMemoryTaskStore::new());
            async move {
                let task_id = uuid::Uuid::new_v4().to_string();
                store.create_task(task_id.clone()).await;

                // Pre-populate with 200 messages
                for i in 0..200u32 {
                    let msg = make_message(&format!("bound-msg-{}", i), &task_id);
                    store.send_message(&task_id, msg).await;
                }

                // Enforce max history length of 50 -- should evict 150
                let evicted = store.enforce_history_length(&task_id, 50).await;
                black_box(evicted);
                assert_eq!(evicted, 150);
            }
        });
    });
}

// ---------------------------------------------------------------------------
// Domain-type-only benchmarks (pure struct construction, no async)
// ---------------------------------------------------------------------------

/// Benchmark a2a_generated::task::Task construction (sync, no async overhead)
fn bench_domain_task_construction(c: &mut Criterion) {
    c.bench_function("a2a_lifecycle/domain_task_construction", |b| {
        b.iter(|| {
            let id = uuid::Uuid::new_v4().to_string();
            black_box(make_task(&id));
        });
    });
}

/// Benchmark a2a_generated::message::Message construction (sync, no async overhead)
fn bench_domain_message_construction(c: &mut Criterion) {
    c.bench_function("a2a_lifecycle/domain_message_construction", |b| {
        b.iter(|| {
            let msg_id = uuid::Uuid::new_v4().to_string();
            black_box(make_message(&msg_id, "task-123"));
        });
    });
}

/// Benchmark Task with full builder chain (with_status, with_dependency, with_priority)
fn bench_domain_task_builder_chain(c: &mut Criterion) {
    c.bench_function("a2a_lifecycle/domain_task_builder_chain", |b| {
        b.iter(|| {
            let task = make_task("builder-test")
                .with_status(TaskStatus::Ready)
                .with_dependency("dep-1".to_string())
                .with_dependency("dep-2".to_string())
                .with_priority(TaskPriority::High)
                .with_timeout(std::time::Duration::from_secs(600))
                .with_metadata("bench".to_string(), "true".to_string());
            black_box(task);
        });
    });
}

/// Benchmark JSON payload roundtrip (serde_json::Value -- used inside Message/Task)
fn bench_json_payload_roundtrip(c: &mut Criterion) {
    let payload = serde_json::json!({
        "task_id": "bench-task",
        "content": "benchmark payload data",
        "nested": {"key": "value", "numbers": [1, 2, 3, 4, 5]},
        "metadata": {"source": "agent-1", "target": "agent-2", "priority": "high"}
    });
    c.bench_function("a2a_lifecycle/json_payload_roundtrip", |b| {
        b.iter(|| {
            let serialized = serde_json::to_string(&payload).unwrap();
            let deserialized: serde_json::Value = serde_json::from_str(&serialized).unwrap();
            black_box(deserialized);
        });
    });
}

criterion_group!(
    benches,
    bench_task_create,
    bench_task_get,
    bench_task_update_status,
    bench_task_cancel,
    bench_task_list,
    bench_task_list_pagination,
    bench_message_send,
    bench_message_history,
    bench_batch_create_tasks,
    bench_task_history_length_bounds,
    bench_domain_task_construction,
    bench_domain_message_construction,
    bench_domain_task_builder_chain,
    bench_json_payload_roundtrip,
);
criterion_main!(benches);
