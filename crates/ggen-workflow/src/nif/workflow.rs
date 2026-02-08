//! Workflow NIF bindings
//!
//! This module provides Erlang NIF bindings for workflow pattern execution.

use crate::error::WorkflowResult;
use crate::patterns::{Choice, Parallel, Sequence, Sync, WorkflowContext, WorkflowPattern};

/// Create a new workflow context (returns workflow_id as JSON string)
#[rustler::nif]
fn workflow_context_create() -> String {
    let context = WorkflowContext::default();
    serde_json::json!({
        "workflow_id": context.metadata.workflow_id,
        "timestamp": context.metadata.timestamp.to_rfc3339(),
    })
    .to_string()
}

/// Add input to workflow context
#[rustler::nif]
fn workflow_context_add_input(context_json: String, key: String, value_json: String) -> String {
    let result: WorkflowResult<String> = (|| {
        // Parse existing context
        let mut context: WorkflowContext = serde_json::from_str(&context_json).unwrap_or_default();

        // Parse value as JSON
        let json_value: serde_json::Value = serde_json::from_str(&value_json)
            .unwrap_or_else(|_| serde_json::Value::String(value_json));

        context.input.insert(key, json_value);

        // Return updated context
        Ok(serde_json::to_string(&context)?)
    })();

    match result {
        Ok(s) => s,
        Err(e) => format!("{{\"error\": \"{}\"}}", e),
    }
}

/// Execute a sequence workflow
#[rustler::nif]
fn execute_sequence(context_json: String, steps: Vec<String>) -> String {
    let result: WorkflowResult<String> = (|| {
        let mut context: WorkflowContext = serde_json::from_str(&context_json)?;
        let sequence = Sequence { steps };
        sequence.execute(&mut context)?;
        Ok(serde_json::to_string(&context)?)
    })();

    match result {
        Ok(s) => s,
        Err(e) => format!("{{\"error\": \"{}\"}}", e),
    }
}

/// Execute a parallel workflow
#[rustler::nif]
fn execute_parallel(context_json: String, steps: Vec<String>) -> String {
    let result: WorkflowResult<String> = (|| {
        let mut context: WorkflowContext = serde_json::from_str(&context_json)?;
        let parallel = Parallel {
            steps,
            sync_config: Default::default(),
        };
        parallel.execute(&mut context)?;
        Ok(serde_json::to_string(&context)?)
    })();

    match result {
        Ok(s) => s,
        Err(e) => format!("{{\"error\": \"{}\"}}", e),
    }
}

/// Execute a choice workflow
#[rustler::nif]
fn execute_choice(
    context_json: String, condition: String, true_branch: Vec<String>, false_branch: Vec<String>,
) -> String {
    let result: WorkflowResult<String> = (|| {
        let mut context: WorkflowContext = serde_json::from_str(&context_json)?;

        let mut branches = std::collections::HashMap::new();
        branches.insert(
            "true".to_string(),
            Box::new(Sequence { steps: true_branch }) as Box<dyn crate::patterns::WorkflowPattern>,
        );
        branches.insert(
            "false".to_string(),
            Box::new(Sequence {
                steps: false_branch,
            }) as Box<dyn crate::patterns::WorkflowPattern>,
        );

        let choice = Choice {
            condition,
            branches,
        };
        choice.execute(&mut context)?;

        Ok(serde_json::to_string(&context)?)
    })();

    match result {
        Ok(s) => s,
        Err(e) => format!("{{\"error\": \"{}\"}}", e),
    }
}

/// Execute a sync workflow
#[rustler::nif]
fn execute_sync(context_json: String, steps: Vec<String>) -> String {
    let result: WorkflowResult<String> = (|| {
        let mut context: WorkflowContext = serde_json::from_str(&context_json)?;

        let sync = Sync {
            steps,
            sync_config: Default::default(),
        };

        sync.execute(&mut context)?;

        Ok(serde_json::to_string(&context)?)
    })();

    match result {
        Ok(s) => s,
        Err(e) => format!("{{\"error\": \"{}\"}}", e),
    }
}

/// Get workflow context metadata
#[rustler::nif]
fn workflow_context_metadata(context_json: String) -> String {
    if let Ok(context) = serde_json::from_str::<WorkflowContext>(&context_json) {
        serde_json::json!({
            "workflow_id": context.metadata.workflow_id,
            "timestamp": context.metadata.timestamp.to_rfc3339(),
            "trace_count": context.metadata.trace.len()
        })
        .to_string()
    } else {
        "{\"error\": \"invalid context\"}".to_string()
    }
}

/// Get workflow context output
#[rustler::nif]
fn workflow_context_output(context_json: String) -> String {
    if let Ok(context) = serde_json::from_str::<WorkflowContext>(&context_json) {
        serde_json::to_string(&context.output).unwrap_or_else(|_| "{}".to_string())
    } else {
        "{\"error\": \"invalid context\"}".to_string()
    }
}
