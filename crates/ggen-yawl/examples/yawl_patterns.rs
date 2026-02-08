//! YAWL Workflow Pattern Examples
//!
//! This example demonstrates the 20 YAWL workflow patterns and how they
//! are generated from FIBO ontology structures.

use ggen_yawl::{
    template::{FlowContext, TaskContext, TemplateContext, VariableContext},
    YawlXmlGenerator,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example 1: Sequence (WP1) - Entity Lifecycle
    println!("=== Pattern 1: Sequence (WP1) - Entity Lifecycle ===\n");
    let sequence_pattern = sequence_pattern();
    println!("{}", sequence_pattern);
    println!();

    // Example 2: Parallel Split (WP2) - Multiple Obligations
    println!("=== Pattern 2: Parallel Split (WP2) - Multiple Obligations ===\n");
    let parallel_pattern = parallel_split_pattern();
    println!("{}", parallel_pattern);
    println!();

    // Example 3: Synchronization (WP3) - Join Parallel
    println!("=== Pattern 3: Synchronization (WP3) - Join Parallel ===\n");
    let sync_pattern = synchronization_pattern();
    println!("{}", sync_pattern);
    println!();

    // Example 4: Exclusive Choice (WP4) - Conditional Requirements
    println!("=== Pattern 4: Exclusive Choice (WP4) - Conditional ===\n");
    let exclusive_pattern = exclusive_choice_pattern();
    println!("{}", exclusive_pattern);
    println!();

    // Example 5: Simple Merge (WP5) - Default Path
    println!("=== Pattern 5: Simple Merge (WP5) - Default Path ===\n");
    let merge_pattern = simple_merge_pattern();
    println!("{}", merge_pattern);
    println!();

    // Example 6: Multi-Choice (WP6) - Alternative Paths
    println!("=== Pattern 6: Multi-Choice (WP6) - Alternative Paths ===\n");
    let multichoice_pattern = multi_choice_pattern();
    println!("{}", multichoice_pattern);
    println!();

    // Example 12: Multiple Instance (WP12) - Collection Iteration
    println!("=== Pattern 12: Multiple Instance (WP12) - Collection ===\n");
    let multi_instance_pattern = multiple_instance_pattern();
    println!("{}", multi_instance_pattern);
    println!();

    // Example 15: State-Based Choice (WP15) - State Machine
    println!("=== Pattern 15: State-Based Choice (WP15) - State Machine ===\n");
    let state_pattern = state_based_choice_pattern();
    println!("{}", state_pattern);
    println!();

    Ok(())
}

/// Pattern 1: Sequence (WP1)
///
/// Maps FIBO entity lifecycle (Created->Active->Closed) to YAWL sequence.
fn sequence_pattern() -> String {
    let context = TemplateContext {
        workflow_name: "EntityLifecycle".to_string(),
        description: "Entity lifecycle workflow with sequential states".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "create".to_string(),
                name: "Create Entity".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "activate".to_string(),
                name: "Activate Entity".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "close".to_string(),
                name: "Close Entity".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "create".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "create".to_string(),
                target: "activate".to_string(),
                condition: Some("/status = 'created'".to_string()),
                predicate: Some("status == 'created'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "activate".to_string(),
                target: "close".to_string(),
                condition: Some("/status = 'active'".to_string()),
                predicate: Some("status == 'active'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "close".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![VariableContext {
            name: "status".to_string(),
            var_type: "string".to_string(),
            default: Some("created".to_string()),
            scope: "workflow".to_string(),
        }],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}

/// Pattern 2: Parallel Split (WP2)
///
/// Maps FIBO multiple obligations (all must complete) to AND split.
fn parallel_split_pattern() -> String {
    let context = TemplateContext {
        workflow_name: "ObligationFulfillment".to_string(),
        description: "Multiple obligations must be fulfilled in parallel".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "obligation1".to_string(),
                name: "Credit Check".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "obligation2".to_string(),
                name: "Identity Verification".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "obligation3".to_string(),
                name: "Background Check".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "complete".to_string(),
                name: "All Obligations Met".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "obligation1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "input".to_string(),
                target: "obligation2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "input".to_string(),
                target: "obligation3".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "obligation1".to_string(),
                target: "complete".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "obligation2".to_string(),
                target: "complete".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "obligation3".to_string(),
                target: "complete".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "complete".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}

/// Pattern 3: Synchronization (WP3)
///
/// Waits for all parallel branches to complete before continuing.
fn synchronization_pattern() -> String {
    let context = TemplateContext {
        workflow_name: "Synchronization".to_string(),
        description: "Synchronize parallel branches before continuation".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "branch1".to_string(),
                name: "Branch 1".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "branch2".to_string(),
                name: "Branch 2".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "sync".to_string(),
                name: "Synchronization Point".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "continue".to_string(),
                name: "Continue After Sync".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "branch1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "input".to_string(),
                target: "branch2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "branch1".to_string(),
                target: "sync".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "branch2".to_string(),
                target: "sync".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "sync".to_string(),
                target: "continue".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "continue".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}

/// Pattern 4: Exclusive Choice (WP4)
///
/// Choose one branch based on condition (XOR split).
fn exclusive_choice_pattern() -> String {
    let context = TemplateContext {
        workflow_name: "ConditionalApproval".to_string(),
        description: "Conditional routing based on approval status".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "review".to_string(),
                name: "Review Application".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "approve".to_string(),
                name: "Approve".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "reject".to_string(),
                name: "Reject".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "manual".to_string(),
                name: "Manual Review".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "review".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "review".to_string(),
                target: "approve".to_string(),
                condition: Some("/approved = true".to_string()),
                predicate: Some("approved == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "review".to_string(),
                target: "reject".to_string(),
                condition: Some("/approved = false AND /score < 50".to_string()),
                predicate: Some("approved == false && score < 50".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "review".to_string(),
                target: "manual".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "approve".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "reject".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "manual".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![
            VariableContext {
                name: "approved".to_string(),
                var_type: "boolean".to_string(),
                default: Some("false".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "score".to_string(),
                var_type: "integer".to_string(),
                default: Some("0".to_string()),
                scope: "workflow".to_string(),
            },
        ],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}

/// Pattern 5: Simple Merge (WP5)
///
/// Merge exclusive branches without synchronization.
fn simple_merge_pattern() -> String {
    let context = TemplateContext {
        workflow_name: "SimpleMerge".to_string(),
        description: "Merge exclusive choice branches".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "start".to_string(),
                name: "Start".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "option_a".to_string(),
                name: "Option A".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "option_b".to_string(),
                name: "Option B".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "merge".to_string(),
                name: "Merge Point".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "end".to_string(),
                name: "End".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "start".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "start".to_string(),
                target: "option_a".to_string(),
                condition: Some("/choice = 'A'".to_string()),
                predicate: Some("choice == 'A'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "start".to_string(),
                target: "option_b".to_string(),
                condition: Some("/choice = 'B'".to_string()),
                predicate: Some("choice == 'B'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "option_a".to_string(),
                target: "merge".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "option_b".to_string(),
                target: "merge".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "merge".to_string(),
                target: "end".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "end".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![VariableContext {
            name: "choice".to_string(),
            var_type: "string".to_string(),
            default: Some("A".to_string()),
            scope: "workflow".to_string(),
        }],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}

/// Pattern 6: Multi-Choice (WP6)
///
/// Choose multiple branches simultaneously (OR split).
fn multi_choice_pattern() -> String {
    let context = TemplateContext {
        workflow_name: "MultiChoice".to_string(),
        description: "Select multiple execution paths based on conditions".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "start".to_string(),
                name: "Start".to_string(),
                split_type: "OR".to_string(),
                join_type: "OR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "task1".to_string(),
                name: "Task 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "task2".to_string(),
                name: "Task 2".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "task3".to_string(),
                name: "Task 3".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "end".to_string(),
                name: "End".to_string(),
                split_type: "OR".to_string(),
                join_type: "OR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "start".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "start".to_string(),
                target: "task1".to_string(),
                condition: Some("/flag1 = true".to_string()),
                predicate: Some("flag1 == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "start".to_string(),
                target: "task2".to_string(),
                condition: Some("/flag2 = true".to_string()),
                predicate: Some("flag2 == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "start".to_string(),
                target: "task3".to_string(),
                condition: Some("/flag3 = true".to_string()),
                predicate: Some("flag3 == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "task1".to_string(),
                target: "end".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "task2".to_string(),
                target: "end".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "task3".to_string(),
                target: "end".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "end".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![
            VariableContext {
                name: "flag1".to_string(),
                var_type: "boolean".to_string(),
                default: Some("false".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "flag2".to_string(),
                var_type: "boolean".to_string(),
                default: Some("false".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "flag3".to_string(),
                var_type: "boolean".to_string(),
                default: Some("false".to_string()),
                scope: "workflow".to_string(),
            },
        ],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}

/// Pattern 12: Multiple Instance (WP12)
///
/// Create multiple task instances for collection iteration.
fn multiple_instance_pattern() -> String {
    let context = TemplateContext {
        workflow_name: "MultipleInstance".to_string(),
        description: "Process collection items in parallel instances".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "prepare".to_string(),
                name: "Prepare Collection".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "process_item".to_string(),
                name: "Process Item".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: Some("process_item_decomp".to_string()),
            },
            TaskContext {
                id: "complete".to_string(),
                name: "All Items Processed".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "prepare".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "prepare".to_string(),
                target: "process_item".to_string(),
                condition: Some("COUNT(/items) > 0".to_string()),
                predicate: Some("items.length() > 0".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "process_item".to_string(),
                target: "complete".to_string(),
                condition: Some("/all_processed = true".to_string()),
                predicate: Some("all_processed == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "complete".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![
            VariableContext {
                name: "items".to_string(),
                var_type: "collection".to_string(),
                default: None,
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "all_processed".to_string(),
                var_type: "boolean".to_string(),
                default: Some("false".to_string()),
                scope: "workflow".to_string(),
            },
        ],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}

/// Pattern 15: State-Based Choice (WP15)
///
/// Route based on entity state (FIBO state machine pattern).
fn state_based_choice_pattern() -> String {
    let context = TemplateContext {
        workflow_name: "StateMachine".to_string(),
        description: "State-based routing using FIBO lifecycle states".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "determine_state".to_string(),
                name: "Determine State".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "handle_pending".to_string(),
                name: "Handle Pending".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "handle_active".to_string(),
                name: "Handle Active".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "handle_suspended".to_string(),
                name: "Handle Suspended".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "handle_closed".to_string(),
                name: "Handle Closed".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "final".to_string(),
                name: "Final State".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "determine_state".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "determine_state".to_string(),
                target: "handle_pending".to_string(),
                condition: Some("/entity_state = 'Pending'".to_string()),
                predicate: Some("entity_state == 'Pending'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "determine_state".to_string(),
                target: "handle_active".to_string(),
                condition: Some("/entity_state = 'Active'".to_string()),
                predicate: Some("entity_state == 'Active'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "determine_state".to_string(),
                target: "handle_suspended".to_string(),
                condition: Some("/entity_state = 'Suspended'".to_string()),
                predicate: Some("entity_state == 'Suspended'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "determine_state".to_string(),
                target: "handle_closed".to_string(),
                condition: Some("/entity_state = 'Closed'".to_string()),
                predicate: Some("entity_state == 'Closed'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "handle_pending".to_string(),
                target: "final".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "handle_active".to_string(),
                target: "final".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "handle_suspended".to_string(),
                target: "final".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "handle_closed".to_string(),
                target: "final".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "final".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![VariableContext {
            name: "entity_state".to_string(),
            var_type: "string".to_string(),
            default: Some("Pending".to_string()),
            scope: "workflow".to_string(),
        }],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}
