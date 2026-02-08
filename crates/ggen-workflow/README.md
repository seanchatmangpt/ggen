# ggen-workflow

Workflow orchestration engine for ggen, providing YAWL pattern support and Erlang NIF integration for high-performance workflow execution.

## Quick Start

```rust
use ggen_workflow::{WorkflowEngine, Yawlpattern, NifIntegration};
use ggen_core::rdf::TripleStore;

// Initialize the workflow engine
let engine = WorkflowEngine::new();
let store = TripleStore::new();

// Create a YAWL workflow
let yawl_workflow = Yawlpattern::from_ttl(workflow_spec)?;
let workflow_id = engine.register_workflow(yawl_workflow)?;

// Execute the workflow
let result = engine.execute_workflow(
    workflow_id,
    &store,
    ExecutionContext::default()
)?;
```

## Features

- **YAWL Pattern Support**: Full implementation of Yet Another Workflow Language patterns
- **NIF Integration**: High-performance Erlang NIF bindings for workflow execution
- **RDF-Based**: Workflows defined in RDF/TTL format
- **Deterministic Execution**: Cryptographic receipts for reproducible results
- **Parallel Processing**: Concurrent task execution with dependency resolution

## API Documentation

### WorkflowEngine

The main workflow orchestration engine.

```rust
pub struct WorkflowEngine {
    workflows: HashMap<String, Workflow>,
    state_machine: StateMachine,
    executor: TaskExecutor,
}

impl WorkflowEngine {
    /// Create a new workflow engine
    pub fn new() -> Self

    /// Register a workflow from YAWL pattern
    pub fn register_workflow(&mut self, workflow: Yawlpattern) -> Result<String>

    /// Execute a workflow with given context
    pub fn execute_workflow(
        &self,
        workflow_id: &str,
        store: &TripleStore,
        context: ExecutionContext
    ) -> Result<WorkflowResult>

    /// Get workflow status
    pub fn get_workflow_status(&self, workflow_id: &str) -> WorkflowStatus

    /// Cancel a running workflow
    pub fn cancel_workflow(&mut self, workflow_id: &str) -> Result<()>
}
```

### Yawlpattern

YAWL pattern implementation for workflow definition.

```rust
pub struct Yawlpattern {
    pub id: String,
    pub name: String,
    pub description: String,
    pub net: WorkflowNet,
    pub input_parameters: Vec<Parameter>,
    pub output_parameters: Vec<Parameter>,
}

impl Yawlpattern {
    /// Create from TTL specification
    pub fn from_ttl(ttl_content: &str) -> Result<Self>

    /// Validate workflow structure
    pub fn validate(&self) -> Result<()>

    /// Get workflow execution dependencies
    pub fn get_dependencies(&self) -> Vec<String>

    /// Serialize to TTL
    pub fn to_ttl(&self) -> Result<String>
}
```

### NifIntegration

Erlang NIF bindings for high-performance workflow execution.

```rust
pub struct NifIntegration {
    engine: Arc<Mutex<WorkflowEngine>>,
    runtime: Runtime,
}

impl NifIntegration {
    /// Create new NIF integration
    pub fn new(engine: WorkflowEngine) -> Self

    /// Execute workflow from Erlang
    pub fn execute_workflow(
        &self,
        workflow_id: &str,
        context: NifContext
    ) -> NifResult

    /// Get workflow status for monitoring
    pub fn get_status(&self, workflow_id: &str) -> NifStatus

    /// Handle workflow events
    pub fn handle_event(&self, event: NifEvent) -> Result<()>
}
```

## YAWL Pattern Integration

### Basic YAWL Workflow

```turtle
@prefix yawl: <http://www.yawlfoundation.org/yawlti#> .
@prefix ex: <http://example.org/> .

ex:approval-workflow a yawl:Workflow ;
    rdfs:label "Document Approval" ;
    yawl:inputParameters [
        a yawl:Parameter ;
        rdfs:label "Document" ;
        yawl:dataType ex:Document ;
    ] ;
    yawl:outputParameters [
        a yawl:Parameter ;
        rdfs:label "Approval Result" ;
        yawl:dataType ex:ApprovalResult ;
    ] ;
    yawl:hasWorkflowNet [
        a yawl:WorkflowNet ;
        yawl:hasPlaces [
            a yawl:StartPlace ;
            rdfs:label "start" ;
        ], [
            a yawl:Task ;
            rdfs:label "review-document" ;
            yawl:taskImplementation ex:DocumentReview ;
        ], [
            a yawl:Task ;
            rdfs:label "approve-document" ;
            yawl:taskImplementation ex:DocumentApproval ;
        ], [
            a yawl:EndPlace ;
            rdfs:label "end" ;
        ] ;
        yawl:hasFlows [
            a yawl:Flow ;
            yawl:source "start" ;
            yawl:target "review-document" ;
        ], [
            a yawl:Flow ;
            yawl:source "review-document" ;
            yawl:target "approve-document" ;
        ], [
            a yawl:Flow ;
            yawl:source "approve-document" ;
            yawl:target "end" ;
        ] ;
    ] .
```

### Complex YAWL with XOR/SOR

```turtle
ex:complex-workflow a yawl:Workflow ;
    rdfs:label "Complex Business Process" ;
    yawl:hasWorkflowNet [
        a yawl:WorkflowNet ;
        yawl:hasPlaces [
            a yawl:StartPlace ;
            rdfs:label "start" ;
        ], [
            a yawl:Task ;
            rdfs:label "validate-input" ;
        ], [
            a yawl:Gateway ;
            rdfs:label "xor-gateway" ;
            yawl:gatewayType yawl:XOR ;
        ], [
            a yawl:Task ;
            rdfs:label "process-high-priority" ;
        ], [
            a yawl:Task ;
            rdfs:label "process-low-priority" ;
        ], [
            a yawl:Gateway ;
            rdfs:label "sor-gateway" ;
            yawl:gatewayType yawl:SOR ;
        ], [
            a yawl:Task ;
            rdfs:label "aggregate-results" ;
        ], [
            a yawl:EndPlace ;
            rdfs:label "end" ;
        ] ;
        yawl:hasFlows [
            # Validation gateway
            a yawl:Flow ;
            yawl:source "validate-input" ;
            yawl:target "xor-gateway" ;
        ], [
            # XOR branches
            a yawl:Flow ;
            yawl:source "xor-gateway" ;
            yawl:target "process-high-priority" ;
            yawl:condition "priority = 'high'" ;
        }, [
            a yawl:Flow ;
            yawl:source "xor-gateway" ;
            yawl:target "process-low-priority" ;
            yawl:condition "priority = 'low'" ;
        ], [
            # SOR convergence
            a yawl:Flow ;
            yawl:source "process-high-priority" ;
            yawl:target "sor-gateway" ;
        }, [
            a yawl:Flow ;
            yawl:source "process-low-priority" ;
            yawl:target "sor-gateway" ;
        ], [
            # Final processing
            a yawl:Flow ;
            yawl:source "sor-gateway" ;
            yawl:target "aggregate-results" ;
        }, [
            a yawl:Flow ;
            yawl:source "aggregate-results" ;
            yawl:target "end" ;
        ] ;
    ] .
```

## NIF Usage Examples

### Basic Workflow Execution

```erlang
% Load the NIF module
ggen_workflow_nif:start().

% Define workflow context
Context = #{document => #{id => "doc123", content => "Important document"}},
WorkflowId = "approval-workflow".

% Execute workflow
Result = ggen_workflow_nif:execute_workflow(WorkflowId, Context),
case Result of
    {ok, WorkflowResult} ->
        io:format("Workflow completed: ~p~n", [WorkflowResult]);
    {error, Reason} ->
        io:format("Workflow failed: ~p~n", [Reason])
end.
```

### Workflow Monitoring

```erlang
% Start workflow execution and monitor
{ok, Ref} = ggen_workflow_nif:start_workflow(
    "approval-workflow",
    #{document => #{id => "doc123"}}
).

% Monitor workflow progress
receive
    {workflow_progress, Ref, Progress} ->
        io:format("Progress: ~p%~n", [Progress]),
        case Progress of
            100 -> io:format("Workflow completed~n");
            _ -> continue_monitoring
        end;
    {workflow_complete, Ref, Result} ->
        io:format("Completed: ~p~n", [Result]);
    {workflow_error, Ref, Error} ->
        io:format("Error: ~p~n", [Error])
after 30000 ->
    io:format("Timeout waiting for workflow completion~n")
end.
```

### Parallel Execution

```erlang
% Define multiple workflows to execute in parallel
Workflows = [
    {"workflow1", #{param1 => "value1"}},
    {"workflow2", #{param2 => "value2"}},
    {"workflow3", #{param3 => "value3"}}
].

% Execute all workflows concurrently
Results = lists:map(fun({Id, Params}) ->
    {ok, Ref} = ggen_workflow_nif:start_workflow(Id, Params),
    Ref
end, Workflows).

% Wait for all workflows to complete
FinalResults = lists:map(fun(Ref) ->
    receive
        {workflow_complete, Ref, Result} -> {ok, Result};
        {workflow_error, Ref, Error} -> {error, Error}
    after 30000 ->
        {timeout, Ref}
    end
end, Results).
```

## Common Workflows

### Document Approval Workflow

```rust
use ggen_workflow::{WorkflowEngine, Yawlpattern};

pub fn create_approval_workflow() -> Result<Yawlpattern> {
    let ttl = r#"
    @prefix yawl: <http://www.yawlfoundation.org/yawlti#> .
    @prefix ex: <http://example.org/> .

    ex:approval-workflow a yawl:Workflow ;
        rdfs:label "Document Approval" ;
        yawl:inputParameters [
            a yawl:Parameter ;
            rdfs:label "Document" ;
            yawl:dataType ex:Document ;
        ] ;
        yawl:outputParameters [
            a yawl:Parameter ;
            rdfs:label "Decision" ;
            yawl:dataType ex:Decision ;
        ] ;
        yawl:hasWorkflowNet [
            a yawl:WorkflowNet ;
            yawl:hasPlaces [
                a yawl:StartPlace ;
                rdfs:label "start" ;
            ], [
                a yawl:Task ;
                rdfs:label "review-document" ;
                yawl:taskImplementation ex:DocumentReview ;
            ], [
                a yawl:Gateway ;
                rdfs:label "decision-gateway" ;
                yawl:gatewayType yawl:OR ;
            ], [
                a yawl:Task ;
                rdfs:label "approve" ;
                yawl:taskImplementation ex:DocumentApproval ;
            ], [
                a yawl:Task ;
                rdfs:label "reject" ;
                yawl:taskImplementation ex:DocumentRejection ;
            ], [
                a yawl:EndPlace ;
                rdfs:label "end" ;
            ] ;
            yawl:hasFlows [
                a yawl:Flow ;
                yawl:source "start" ;
                yawl:target "review-document" ;
            ], [
                a yawl:Flow ;
                yawl:source "review-document" ;
                yawl:target "decision-gateway" ;
            ], [
                a yawl:Flow ;
                yawl:source "decision-gateway" ;
                yawl:target "approve" ;
                yawl:condition "approval = 'true'" ;
            ], [
                a yawl:Flow ;
                yawl:source "decision-gateway" ;
                yawl:target "reject" ;
                yawl:condition "approval = 'false'" ;
            ], [
                a yawl:Flow ;
                yawl:source "approve" ;
                yawl:target "end" ;
            ], [
                a yawl:Flow ;
                yawl:source "reject" ;
                yawl:target "end" ;
            ] ;
        ] .
    "#;

    Yawlpattern::from_ttl(ttl)
}
```

### Data Processing Pipeline

```rust
use ggen_workflow::{WorkflowEngine, ExecutionContext};

pub fn create_data_pipeline() -> Result<Workflow> {
    let mut engine = WorkflowEngine::new();

    // Define sequential data processing workflow
    let workflow_ttl = r#"
    @prefix yawl: <http://www.yawlfoundation.org/yawlti#> .
    @prefix ex: <http://example.org/> .

    ex:data-pipeline a yawl:Workflow ;
        rdfs:label "Data Processing Pipeline" ;
        yawl:inputParameters [
            a yawl:Parameter ;
            rdfs:label "RawData" ;
            yawl:dataType ex:RawData ;
        ] ;
        yawl:outputParameters [
            a yawl:Parameter ;
            rdfs:label "ProcessedData" ;
            yawl:dataType ex:ProcessedData ;
        ] ;
        yawl:hasWorkflowNet [
            a yawl:WorkflowNet ;
            yawl:hasPlaces [
                a yawl:StartPlace ;
                rdfs:label "start" ;
            ], [
                a yawl:Task ;
                rdfs:label "validate-data" ;
                yawl:taskImplementation ex:DataValidation ;
            ], [
                a yawl:Task ;
                rdfs:label "transform-data" ;
                yawl:taskImplementation ex:DataTransformation ;
            ], [
                a yawl:Task ;
                rdfs:label "analyze-data" ;
                yawl:taskImplementation ex:DataAnalysis ;
            ], [
                a yawl:Task ;
                rdfs:label "persist-results" ;
                yawl:taskImplementation ex:DataPersistence ;
            ], [
                a yawl:EndPlace ;
                rdfs:label "end" ;
            ] ;
            yawl:hasFlows [
                a yawl:Flow ;
                yawl:source "start" ;
                yawl:target "validate-data" ;
            ], [
                a yawl:Flow ;
                yawl:source "validate-data" ;
                yawl:target "transform-data" ;
            ], [
                a yawl:Flow ;
                yawl:source "transform-data" ;
                yawl:target "analyze-data" ;
            ], [
                a yawl:Flow ;
                yawl:source "analyze-data" ;
                yawl:target "persist-results" ;
            ], [
                a yawl:Flow ;
                yawl:source "persist-results" ;
                yawl:target "end" ;
            ] ;
        ] .
    "#;

    let workflow = Yawlpattern::from_ttl(workflow_ttl)?;
    engine.register_workflow(workflow)
}
```

## Configuration

### Engine Configuration

```rust
use ggen_workflow::{WorkflowEngine, Config};

let config = Config {
    max_concurrent_workflows: 10,
    task_timeout_ms: 30000,
    retry_attempts: 3,
    enable_monitoring: true,
};

let engine = WorkflowEngine::with_config(config);
```

### NIF Configuration

```erlang
% Configure NIF options
Opts = [
    {max_concurrent, 10},
    {task_timeout, 30000},
    {retry_attempts, 3},
    {monitoring, true}
],

ggen_workflow_nif:configure(Opts).
```

## Error Handling

```rust
use ggen_workflow::{WorkflowError, WorkflowResult};

match workflow_engine.execute_workflow(&workflow_id, &store, context) {
    WorkflowResult::Success(result) => {
        println!("Workflow completed successfully: {:?}", result);
    }
    WorkflowResult::Failure(error) => {
        match error {
            WorkflowError::WorkflowNotFound(id) => {
                println!("Workflow not found: {}", id);
            }
            WorkflowError::ExecutionError(msg) => {
                println!("Execution error: {}", msg);
            }
            WorkflowError::Timeout => {
                println!("Workflow timed out");
            }
        }
    }
}
```

## Monitoring and Observability

```rust
use ggen_workflow::{WorkflowMonitor, Event};

// Set up monitoring
let monitor = WorkflowMonitor::new();

// Subscribe to workflow events
monitor.subscribe(|event: Event| {
    match event {
        Event::WorkflowStarted(id) => {
            println!("Workflow started: {}", id);
        }
        Event::TaskCompleted(task_id, result) => {
            println!("Task completed: {} with result: {:?}", task_id, result);
        }
        Event::WorkflowCompleted(id, result) => {
            println!("Workflow completed: {} with result: {:?}", id, result);
        }
        Event::WorkflowError(id, error) => {
            println!("Workflow error: {} - {}", id, error);
        }
    }
});

// Add monitor to engine
engine.add_monitor(monitor);
```

## Performance Considerations

1. **Concurrent Execution**: The workflow engine supports concurrent task execution for better performance
2. **NIF Integration**: Erlang NIFs provide zero-cost interoperability for high-performance workflows
3. **Memory Management**: Proper cleanup of workflow state to prevent memory leaks
4. **Caching**: Workflow definitions are cached to avoid repeated parsing

## Testing

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use ggen_core::rdf::TripleStore;

    #[test]
    fn test_workflow_execution() {
        let engine = WorkflowEngine::new();
        let store = TripleStore::new();

        let workflow = create_approval_workflow().unwrap();
        let workflow_id = engine.register_workflow(workflow).unwrap();

        let context = ExecutionContext::default();
        let result = engine.execute_workflow(&workflow_id, &store, context);

        assert!(result.is_ok());
    }
}
```

## Contributing

1. Follow the ggen project's coding standards
2. Include comprehensive tests for new functionality
3. Update documentation for new features
4. Ensure all Andon signals are cleared before committing

## License

This crate is part of the ggen project and follows the same license terms.