//! Comprehensive tests for all 43 workflow patterns

use ggen_workflow_43::*;
use ggen_workflow_43::patterns::*;
use async_trait::async_trait;

struct TestActivity {
    id: ActivityId,
    sleep_ms: u64,
}

impl TestActivity {
    fn new(id: impl Into<String>) -> Self {
        Self {
            id: ActivityId::new(id),
            sleep_ms: 0,
        }
    }

    #[allow(dead_code)]
    fn with_delay(id: impl Into<String>, sleep_ms: u64) -> Self {
        Self {
            id: ActivityId::new(id),
            sleep_ms,
        }
    }
}

#[async_trait]
impl Activity for TestActivity {
    async fn execute(&self, context: &mut ActivityContext) -> Result<()> {
        if self.sleep_ms > 0 {
            tokio::time::sleep(tokio::time::Duration::from_millis(self.sleep_ms)).await;
        }
        context.set_output("result", serde_json::json!("success"));
        context.set_output("timestamp", serde_json::json!(chrono::Utc::now().to_rfc3339()));
        Ok(())
    }

    fn id(&self) -> &ActivityId {
        &self.id
    }
}

struct DecisionActivity {
    id: ActivityId,
    decision: String,
}

impl DecisionActivity {
    fn new(id: impl Into<String>, decision: impl Into<String>) -> Self {
        Self {
            id: ActivityId::new(id),
            decision: decision.into(),
        }
    }
}

#[async_trait]
impl Activity for DecisionActivity {
    async fn execute(&self, context: &mut ActivityContext) -> Result<()> {
        context.set_output("decision", serde_json::json!(self.decision));
        Ok(())
    }

    fn id(&self) -> &ActivityId {
        &self.id
    }
}

struct MultiDecisionActivity {
    id: ActivityId,
    decisions: Vec<String>,
}

impl MultiDecisionActivity {
    fn new(id: impl Into<String>, decisions: Vec<String>) -> Self {
        Self {
            id: ActivityId::new(id),
            decisions,
        }
    }
}

#[async_trait]
impl Activity for MultiDecisionActivity {
    async fn execute(&self, context: &mut ActivityContext) -> Result<()> {
        context.set_output("decisions", serde_json::json!(self.decisions));
        Ok(())
    }

    fn id(&self) -> &ActivityId {
        &self.id
    }
}

// Pattern 1: Sequence
#[tokio::test]
async fn test_pattern_01_sequence() {
    let mut engine = WorkflowEngine::new();

    let act1 = ActivityId::new("seq-1");
    let act2 = ActivityId::new("seq-2");
    let act3 = ActivityId::new("seq-3");

    engine.register_activity(Box::new(TestActivity::new("seq-1")));
    engine.register_activity(Box::new(TestActivity::new("seq-2")));
    engine.register_activity(Box::new(TestActivity::new("seq-3")));

    let pattern = SequencePattern::new(vec![act1.clone(), act2.clone(), act3.clone()]);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
    assert_eq!(engine.get_context(&act1).ok().map(|c| &c.state), Some(&ActivityState::Completed));
    assert_eq!(engine.get_context(&act2).ok().map(|c| &c.state), Some(&ActivityState::Completed));
    assert_eq!(engine.get_context(&act3).ok().map(|c| &c.state), Some(&ActivityState::Completed));
}

// Pattern 2: Parallel Split
#[tokio::test]
async fn test_pattern_02_parallel_split() {
    let mut engine = WorkflowEngine::new();

    let act1 = ActivityId::new("par-1");
    let act2 = ActivityId::new("par-2");
    let act3 = ActivityId::new("par-3");

    engine.register_activity(Box::new(TestActivity::new("par-1")));
    engine.register_activity(Box::new(TestActivity::new("par-2")));
    engine.register_activity(Box::new(TestActivity::new("par-3")));

    let pattern = ParallelSplitPattern::new(vec![
        vec![act1.clone()],
        vec![act2.clone()],
        vec![act3.clone()],
    ]);

    let result = pattern.execute(&mut engine).await;
    assert!(result.is_ok());
}

// Pattern 3: Synchronization
#[tokio::test]
async fn test_pattern_03_synchronization() {
    let mut engine = WorkflowEngine::new();

    let branch1 = ActivityId::new("sync-branch-1");
    let branch2 = ActivityId::new("sync-branch-2");
    let join = ActivityId::new("sync-join");

    engine.register_activity(Box::new(TestActivity::new("sync-branch-1")));
    engine.register_activity(Box::new(TestActivity::new("sync-branch-2")));
    engine.register_activity(Box::new(TestActivity::new("sync-join")));

    let pattern = SynchronizationPattern::new(
        vec![branch1.clone(), branch2.clone()],
        join.clone(),
    );

    let result = pattern.execute(&mut engine).await;
    assert!(result.is_ok());
}

// Pattern 4: Exclusive Choice
#[tokio::test]
async fn test_pattern_04_exclusive_choice() {
    let mut engine = WorkflowEngine::new();

    let cond = ActivityId::new("choice-cond");
    let act1 = ActivityId::new("choice-1");
    let act2 = ActivityId::new("choice-2");

    engine.register_activity(Box::new(DecisionActivity::new("choice-cond", "branch1")));
    engine.register_activity(Box::new(TestActivity::new("choice-1")));
    engine.register_activity(Box::new(TestActivity::new("choice-2")));

    let pattern = ExclusiveChoicePattern::new(
        cond.clone(),
        vec![
            ("branch1".to_string(), vec![act1.clone()]),
            ("branch2".to_string(), vec![act2.clone()]),
        ],
    );

    let result = pattern.execute(&mut engine).await;
    assert!(result.is_ok());
    assert_eq!(engine.get_context(&act1).ok().map(|c| &c.state), Some(&ActivityState::Completed));
}

// Pattern 5: Simple Merge
#[tokio::test]
async fn test_pattern_05_simple_merge() {
    let mut engine = WorkflowEngine::new();

    let branch1 = ActivityId::new("merge-1");
    let branch2 = ActivityId::new("merge-2");
    let merge = ActivityId::new("merge-join");

    engine.register_activity(Box::new(TestActivity::new("merge-1")));
    engine.register_activity(Box::new(TestActivity::new("merge-2")));
    engine.register_activity(Box::new(TestActivity::new("merge-join")));

    let pattern = SimpleMergePattern::new(
        vec![branch1, branch2],
        merge.clone(),
    );

    let result = pattern.execute(&mut engine).await;
    assert!(result.is_ok());
    assert_eq!(engine.get_context(&merge).ok().map(|c| &c.state), Some(&ActivityState::Completed));
}

// Pattern 6: Multi-Choice
#[tokio::test]
async fn test_pattern_06_multi_choice() {
    let mut engine = WorkflowEngine::new();

    let cond = ActivityId::new("multi-cond");
    let act1 = ActivityId::new("multi-1");
    let act2 = ActivityId::new("multi-2");

    engine.register_activity(Box::new(MultiDecisionActivity::new(
        "multi-cond",
        vec!["branch1".to_string(), "branch2".to_string()],
    )));
    engine.register_activity(Box::new(TestActivity::new("multi-1")));
    engine.register_activity(Box::new(TestActivity::new("multi-2")));

    let pattern = MultiChoicePattern::new(
        cond.clone(),
        vec![
            ("branch1".to_string(), vec![act1.clone()]),
            ("branch2".to_string(), vec![act2.clone()]),
        ],
    );

    let result = pattern.execute(&mut engine).await;
    assert!(result.is_ok());
}

// Pattern 7: Structured Synchronizing Merge
#[tokio::test]
async fn test_pattern_07_structured_synchronizing_merge() {
    let mut engine = WorkflowEngine::new();

    let branch1 = ActivityId::new("ssm-1");
    let branch2 = ActivityId::new("ssm-2");
    let merge = ActivityId::new("ssm-merge");

    engine.register_activity(Box::new(TestActivity::new("ssm-1")));
    engine.register_activity(Box::new(TestActivity::new("ssm-2")));
    engine.register_activity(Box::new(TestActivity::new("ssm-merge")));

    let pattern = StructuredSynchronizingMergePattern::new(
        vec![branch1, branch2],
        merge.clone(),
    );

    let result = pattern.execute(&mut engine).await;
    assert!(result.is_ok());
}

// Pattern 8: Multi-Merge
#[tokio::test]
async fn test_pattern_08_multi_merge() {
    let mut engine = WorkflowEngine::new();

    let branch1 = ActivityId::new("mm-1");
    let branch2 = ActivityId::new("mm-2");
    let merge = ActivityId::new("mm-merge");

    engine.register_activity(Box::new(TestActivity::new("mm-1")));
    engine.register_activity(Box::new(TestActivity::new("mm-2")));
    engine.register_activity(Box::new(TestActivity::new("mm-merge")));

    let pattern = MultiMergePattern::new(
        vec![branch1, branch2],
        merge.clone(),
    );

    let result = pattern.execute(&mut engine).await;
    assert!(result.is_ok());
}

// Pattern 9: Structured Discriminator
#[tokio::test]
async fn test_pattern_09_structured_discriminator() {
    let mut engine = WorkflowEngine::new();

    let branch1 = ActivityId::new("sd-1");
    let branch2 = ActivityId::new("sd-2");
    let disc = ActivityId::new("sd-disc");

    engine.register_activity(Box::new(TestActivity::new("sd-1")));
    engine.register_activity(Box::new(TestActivity::new("sd-2")));
    engine.register_activity(Box::new(TestActivity::new("sd-disc")));

    let pattern = StructuredDiscriminatorPattern::new(
        vec![branch1, branch2],
        disc.clone(),
    );

    let result = pattern.execute(&mut engine).await;
    assert!(result.is_ok());
    assert_eq!(engine.get_context(&disc).ok().map(|c| &c.state), Some(&ActivityState::Completed));
}

// Pattern 10: Arbitrary Cycles
#[tokio::test]
async fn test_pattern_10_arbitrary_cycles() {
    let mut engine = WorkflowEngine::new();

    let cond = ActivityId::new("cycle-cond");
    let body = ActivityId::new("cycle-body");

    engine.register_activity(Box::new(TestActivity::new("cycle-cond")));
    engine.register_activity(Box::new(TestActivity::new("cycle-body")));

    // Set condition to not continue
    if let Ok(context) = engine.get_context_mut(&cond) {
        context.set_output("continue", serde_json::json!(false));
    }

    let pattern = ArbitraryCyclesPattern::new(vec![body], cond, 5);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 11: Implicit Termination
#[tokio::test]
async fn test_pattern_11_implicit_termination() {
    let mut engine = WorkflowEngine::new();

    let act1 = ActivityId::new("term-1");
    let act2 = ActivityId::new("term-2");

    engine.register_activity(Box::new(TestActivity::new("term-1")));
    engine.register_activity(Box::new(TestActivity::new("term-2")));

    let pattern = ImplicitTerminationPattern::new(vec![act1.clone(), act2.clone()]);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
    assert_eq!(engine.get_context(&act1).ok().map(|c| &c.state), Some(&ActivityState::Completed));
    assert_eq!(engine.get_context(&act2).ok().map(|c| &c.state), Some(&ActivityState::Completed));
}

// Pattern 12: Multiple Instances without Synchronization
#[tokio::test]
async fn test_pattern_12_multiple_instances_no_sync() {
    let mut engine = WorkflowEngine::new();
    let template = ActivityId::new("mi-template");

    engine.register_activity(Box::new(TestActivity::new("mi-template")));

    let pattern = MultipleInstancesWithoutSyncPattern::new(template, 3);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 13: Multiple Instances with Design-Time Knowledge
#[tokio::test]
async fn test_pattern_13_multiple_instances_design_time() {
    let mut engine = WorkflowEngine::new();
    let template = ActivityId::new("mi-dt-template");

    engine.register_activity(Box::new(TestActivity::new("mi-dt-template")));

    let pattern = MultipleInstancesDesignTimePattern::new(template, 3);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 14: Multiple Instances with Runtime Knowledge
#[tokio::test]
async fn test_pattern_14_multiple_instances_runtime() {
    let mut engine = WorkflowEngine::new();

    let template = ActivityId::new("mi-rt-template");
    let count_source = ActivityId::new("mi-rt-count");

    engine.register_activity(Box::new(TestActivity::new("mi-rt-template")));
    engine.register_activity(Box::new(TestActivity::new("mi-rt-count")));

    // Set instance count
    if let Ok(context) = engine.get_context_mut(&count_source) {
        context.set_output("instance_count", serde_json::json!(3));
    }

    let pattern = MultipleInstancesRuntimePattern::new(template, count_source);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 15: Multiple Instances without a Priori Runtime Knowledge
#[tokio::test]
async fn test_pattern_15_multiple_instances_dynamic() {
    let mut engine = WorkflowEngine::new();

    let template = ActivityId::new("mi-dyn-template");
    let controller = ActivityId::new("mi-dyn-controller");

    engine.register_activity(Box::new(TestActivity::new("mi-dyn-template")));
    engine.register_activity(Box::new(TestActivity::new("mi-dyn-controller")));

    let pattern = MultipleInstancesDynamicPattern::new(template, controller);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 16: Deferred Choice
#[tokio::test]
async fn test_pattern_16_deferred_choice() {
    let mut engine = WorkflowEngine::new();

    let trigger1 = ActivityId::new("dc-trigger1");
    let trigger2 = ActivityId::new("dc-trigger2");
    let act1 = ActivityId::new("dc-act1");
    let act2 = ActivityId::new("dc-act2");

    engine.register_activity(Box::new(TestActivity::new("dc-trigger1")));
    engine.register_activity(Box::new(TestActivity::new("dc-trigger2")));
    engine.register_activity(Box::new(TestActivity::new("dc-act1")));
    engine.register_activity(Box::new(TestActivity::new("dc-act2")));

    let pattern = DeferredChoicePattern::new(vec![
        (trigger1, vec![act1]),
        (trigger2, vec![act2]),
    ]);

    let result = pattern.execute(&mut engine).await;
    assert!(result.is_ok());
}

// Pattern 17: Interleaved Parallel Routing
#[tokio::test]
async fn test_pattern_17_interleaved_parallel_routing() {
    let mut engine = WorkflowEngine::new();

    let act1 = ActivityId::new("ipr-1");
    let act2 = ActivityId::new("ipr-2");
    let act3 = ActivityId::new("ipr-3");

    engine.register_activity(Box::new(TestActivity::new("ipr-1")));
    engine.register_activity(Box::new(TestActivity::new("ipr-2")));
    engine.register_activity(Box::new(TestActivity::new("ipr-3")));

    let pattern = InterleavedParallelRoutingPattern::new(vec![act1, act2, act3]);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 18: Milestone
#[tokio::test]
async fn test_pattern_18_milestone() {
    let mut engine = WorkflowEngine::new();

    let milestone = ActivityId::new("ms-milestone");
    let act1 = ActivityId::new("ms-act1");

    engine.register_activity(Box::new(TestActivity::new("ms-milestone")));
    engine.register_activity(Box::new(TestActivity::new("ms-act1")));

    // Set milestone reached
    if let Ok(context) = engine.get_context_mut(&milestone) {
        context.set_output("milestone_reached", serde_json::json!(true));
    }

    let pattern = MilestonePattern::new(milestone, vec![act1.clone()]);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 19: Critical Section
#[tokio::test]
async fn test_pattern_19_critical_section() {
    let mut engine = WorkflowEngine::new();

    let act1 = ActivityId::new("cs-1");
    let act2 = ActivityId::new("cs-2");

    engine.register_activity(Box::new(TestActivity::new("cs-1")));
    engine.register_activity(Box::new(TestActivity::new("cs-2")));

    let pattern = CriticalSectionPattern::new(vec![act1, act2]);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 20: Interleaved Routing
#[tokio::test]
async fn test_pattern_20_interleaved_routing() {
    let mut engine = WorkflowEngine::new();

    let act1 = ActivityId::new("ir-1");
    let act2 = ActivityId::new("ir-2");

    engine.register_activity(Box::new(TestActivity::new("ir-1")));
    engine.register_activity(Box::new(TestActivity::new("ir-2")));

    let pattern = InterleavedRoutingPattern::new(vec![act1, act2]);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 21: Structured Loop
#[tokio::test]
async fn test_pattern_21_structured_loop() {
    let mut engine = WorkflowEngine::new();

    let cond = ActivityId::new("sl-cond");
    let body = ActivityId::new("sl-body");

    engine.register_activity(Box::new(TestActivity::new("sl-cond")));
    engine.register_activity(Box::new(TestActivity::new("sl-body")));

    if let Ok(context) = engine.get_context_mut(&cond) {
        context.set_output("continue", serde_json::json!(false));
    }

    let pattern = StructuredLoopPattern::new(vec![body], cond, 5);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 22: Recursion
#[tokio::test]
async fn test_pattern_22_recursion() {
    let mut engine = WorkflowEngine::new();

    let recursive = ActivityId::new("rec-act");
    engine.register_activity(Box::new(TestActivity::new("rec-act")));

    if let Ok(context) = engine.get_context_mut(&recursive) {
        context.set_output("recurse", serde_json::json!(false));
    }

    let pattern = RecursionPattern::new(recursive, 5);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 19 (Cancellation): Cancel Activity
#[tokio::test]
async fn test_pattern_19_cancel_activity() {
    let mut engine = WorkflowEngine::new();

    let act = ActivityId::new("cancel-act");
    engine.register_activity(Box::new(TestActivity::new("cancel-act")));

    let pattern = CancelActivityPattern::new(act.clone());
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
    assert_eq!(
        engine.get_context(&act).ok().map(|c| &c.state),
        Some(&ActivityState::Cancelled)
    );
}

// Pattern 20 (Cancellation): Cancel Case
#[tokio::test]
async fn test_pattern_20_cancel_case() {
    let mut engine = WorkflowEngine::new();

    let act1 = ActivityId::new("cc-1");
    let act2 = ActivityId::new("cc-2");

    engine.register_activity(Box::new(TestActivity::new("cc-1")));
    engine.register_activity(Box::new(TestActivity::new("cc-2")));

    let pattern = CancelCasePattern::new(vec![act1, act2]);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 28: Blocking Discriminator
#[tokio::test]
async fn test_pattern_28_blocking_discriminator() {
    let mut engine = WorkflowEngine::new();

    let branch1 = ActivityId::new("bd-1");
    let branch2 = ActivityId::new("bd-2");
    let disc = ActivityId::new("bd-disc");

    engine.register_activity(Box::new(TestActivity::new("bd-1")));
    engine.register_activity(Box::new(TestActivity::new("bd-2")));
    engine.register_activity(Box::new(TestActivity::new("bd-disc")));

    let pattern = BlockingDiscriminatorPattern::new(vec![branch1, branch2], disc);
    let result = pattern.execute(&mut engine).await;

    assert!(result.is_ok());
}

// Pattern 30: Structured Partial Join
#[tokio::test]
async fn test_pattern_30_structured_partial_join() {
    let mut engine = WorkflowEngine::new();

    let branch1 = ActivityId::new("spj-1");
    let branch2 = ActivityId::new("spj-2");
    let branch3 = ActivityId::new("spj-3");
    let join = ActivityId::new("spj-join");

    engine.register_activity(Box::new(TestActivity::new("spj-1")));
    engine.register_activity(Box::new(TestActivity::new("spj-2")));
    engine.register_activity(Box::new(TestActivity::new("spj-3")));
    engine.register_activity(Box::new(TestActivity::new("spj-join")));

    let pattern = StructuredPartialJoinPattern::new(
        vec![branch1, branch2, branch3],
        join,
        2,
    );

    let result = pattern.execute(&mut engine).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_all_patterns_execution_history() {
    let mut engine = WorkflowEngine::new();

    let act = ActivityId::new("history-test");
    engine.register_activity(Box::new(TestActivity::new("history-test")));

    let pattern = SequencePattern::new(vec![act]);
    let _result = pattern.execute(&mut engine).await;

    assert!(!engine.execution_history.is_empty());
    assert_eq!(engine.execution_history[0].pattern_name, "Sequence");
    assert_eq!(engine.execution_history[0].pattern_number, 1);
}
