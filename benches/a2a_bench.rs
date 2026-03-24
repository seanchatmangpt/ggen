use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_a2a::{
    Artifact, ArtifactContent, ArtifactType, StateTransition, Task, TaskState, TaskStateMachine,
};

fn bench_task_creation(c: &mut Criterion) {
    c.bench_function("task_creation", |b| {
        b.iter(|| {
            black_box(Task::new("Test task".to_string(), "agent-1".to_string()));
        });
    });
}

fn bench_task_builder(c: &mut Criterion) {
    c.bench_function("task_builder_full", |b| {
        b.iter(|| {
            let task = Task::new("Complex task".to_string(), "agent-1".to_string())
                .with_description("Detailed description".to_string())
                .with_assignment("agent-2".to_string())
                .with_metadata("priority".to_string(), "high".to_string())
                .with_metadata("team".to_string(), "backend".to_string());

            black_box(task);
        });
    });
}

fn bench_state_transition_validation(c: &mut Criterion) {
    let mut group = c.benchmark_group("state_transition_validation");

    let transitions = vec![
        (TaskState::Created, TaskState::Running, true),
        (TaskState::Created, TaskState::Failed, true),
        (TaskState::Running, TaskState::Blocked, true),
        (TaskState::Running, TaskState::Completed, true),
        (TaskState::Blocked, TaskState::Running, true),
        (TaskState::Created, TaskState::Blocked, false),
        (TaskState::Completed, TaskState::Running, false),
    ];

    for (from, to, valid) in transitions {
        group.bench_function(format!("{:?}_to_{:?}", from, to), |b| {
            b.iter(|| {
                let result = TaskStateMachine::is_valid_transition(from, to);
                black_box(result);
                assert_eq!(result, valid);
            });
        });
    }

    group.finish();
}

fn bench_state_transition_apply(c: &mut Criterion) {
    let mut group = c.benchmark_group("state_transition_apply");

    group.bench_function("created_to_running", |b| {
        b.iter(|| {
            let mut task = Task::new("Test".to_string(), "agent-1".to_string())
                .with_assignment("agent-1".to_string());
            let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
            black_box(TaskStateMachine::transition(&mut task, transition).unwrap());
        });
    });

    group.bench_function("running_to_completed", |b| {
        b.iter(|| {
            let mut task = Task::new("Test".to_string(), "agent-1".to_string())
                .with_assignment("agent-1".to_string());

            // First transition to Running
            let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
            TaskStateMachine::transition(&mut task, transition).unwrap();

            // Then to Completed
            let transition = StateTransition::new(TaskState::Completed, "agent-1".to_string());
            black_box(TaskStateMachine::transition(&mut task, transition).unwrap());
        });
    });

    group.bench_function("running_to_failed_with_reason", |b| {
        b.iter(|| {
            let mut task = Task::new("Test".to_string(), "agent-1".to_string())
                .with_assignment("agent-1".to_string());

            let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
            TaskStateMachine::transition(&mut task, transition).unwrap();

            let transition = StateTransition::new(TaskState::Failed, "agent-1".to_string())
                .with_reason("Connection timeout".to_string());
            black_box(TaskStateMachine::transition(&mut task, transition).unwrap());
        });
    });

    group.finish();
}

fn bench_possible_transitions(c: &mut Criterion) {
    let mut group = c.benchmark_group("possible_transitions");

    for state in [
        TaskState::Created,
        TaskState::Running,
        TaskState::Blocked,
        TaskState::Completed,
        TaskState::Failed,
    ] {
        group.bench_function(format!("{:?}", state), |b| {
            b.iter(|| {
                black_box(TaskStateMachine::possible_transitions(state));
            });
        });
    }

    group.finish();
}

fn bench_task_state_checks(c: &mut Criterion) {
    let mut group = c.benchmark_group("task_state_checks");

    let task = Task::new("Test".to_string(), "agent-1".to_string());

    group.bench_function("is_terminal", |b| {
        b.iter(|| {
            black_box(task.is_terminal());
        });
    });

    group.bench_function("is_active", |b| {
        b.iter(|| {
            black_box(task.is_active());
        });
    });

    group.finish();
}

fn bench_artifact_creation(c: &mut Criterion) {
    let mut group = c.benchmark_group("artifact_creation");

    group.bench_function("text_artifact", |b| {
        b.iter(|| {
            black_box(Artifact::new(
                "test_artifact".to_string(),
                ArtifactType::Output,
                ArtifactContent::Text("Sample text content".to_string()),
            ));
        });
    });

    group.bench_function("binary_artifact", |b| {
        b.iter(|| {
            black_box(Artifact::new(
                "binary_artifact".to_string(),
                ArtifactType::Output,
                ArtifactContent::Binary(vec![1, 2, 3, 4, 5]),
            ));
        });
    });

    group.finish();
}

fn bench_task_serialization(c: &mut Criterion) {
    let task = Task::new("Serialization test".to_string(), "agent-1".to_string())
        .with_description("Test description".to_string())
        .with_assignment("agent-2".to_string());

    let mut group = c.benchmark_group("task_serialization");

    group.bench_function("serialize", |b| {
        b.iter(|| {
            black_box(serde_json::to_string(&task).unwrap());
        });
    });

    let json = serde_json::to_string(&task).unwrap();
    group.bench_function("deserialize", |b| {
        b.iter(|| {
            black_box(serde_json::from_str::<Task>(&json).unwrap());
        });
    });

    group.finish();
}

fn bench_task_lifecycle(c: &mut Criterion) {
    c.bench_function("complete_task_lifecycle", |b| {
        b.iter(|| {
            let mut task = Task::new("Lifecycle test".to_string(), "agent-1".to_string())
                .with_assignment("agent-1".to_string());

            // Created -> Running
            let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
            TaskStateMachine::transition(&mut task, transition).unwrap();

            // Running -> Completed
            let transition = StateTransition::new(TaskState::Completed, "agent-1".to_string());
            TaskStateMachine::transition(&mut task, transition).unwrap();

            black_box(task);
        });
    });
}

fn bench_batch_state_transitions(c: &mut Criterion) {
    let mut group = c.benchmark_group("batch_state_transitions");

    for count in [10, 100, 1000].iter() {
        group.throughput(Throughput::Elements(*count));
        group.bench_with_input(
            BenchmarkId::from_parameter(count),
            count,
            |b, &count| {
                b.iter(|| {
                    for i in 0..count {
                        let mut task = Task::new(
                            format!("Task {}", i),
                            format!("agent-{}", i),
                        )
                        .with_assignment(format!("agent-{}", i));

                        let transition = StateTransition::new(
                            TaskState::Running,
                            format!("agent-{}", i),
                        );
                        TaskStateMachine::transition(&mut task, transition).unwrap();

                        let transition = StateTransition::new(
                            TaskState::Completed,
                            format!("agent-{}", i),
                        );
                        TaskStateMachine::transition(&mut task, transition).unwrap();

                        black_box(task);
                    }
                });
            },
        );
    }

    group.finish();
}

fn bench_task_duration_calculation(c: &mut Criterion) {
    let mut task = Task::new("Test".to_string(), "agent-1".to_string())
        .with_assignment("agent-1".to_string());

    // Complete the task
    let transition = StateTransition::new(TaskState::Running, "agent-1".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();

    let transition = StateTransition::new(TaskState::Completed, "agent-1".to_string());
    TaskStateMachine::transition(&mut task, transition).unwrap();

    c.bench_function("task_duration", |b| {
        b.iter(|| {
            black_box(task.duration());
        });
    });
}

criterion_group!(
    benches,
    bench_task_creation,
    bench_task_builder,
    bench_state_transition_validation,
    bench_state_transition_apply,
    bench_possible_transitions,
    bench_task_state_checks,
    bench_artifact_creation,
    bench_task_serialization,
    bench_task_lifecycle,
    bench_batch_state_transitions,
    bench_task_duration_calculation,
);
criterion_main!(benches);
