<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen-workflow Architecture Design](#ggen-workflow-architecture-design)
  - [Overview](#overview)
  - [Core Architecture](#core-architecture)
    - [System Components](#system-components)
    - [Stage Implementations](#stage-implementations)
    - [Pattern Execution Engine](#pattern-execution-engine)
    - [Erlang NIF Integration](#erlang-nif-integration)
    - [Integration with Existing System](#integration-with-existing-system)
    - [Performance Optimizations](#performance-optimizations)
    - [Error Handling and Recovery](#error-handling-and-recovery)
    - [Usage Examples](#usage-examples)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen-workflow Architecture Design

## Overview

This document details the architecture design for the `ggen-workflow` crate, which implements the five-stage transformation pipeline (A = μ(O)) with Erlang NIF bindings for deterministic workflow execution based on RDF ontology specifications.

## Core Architecture

### System Components

```rust
/// Workflow Engine Core - Central coordinator for all workflow operations
pub struct WorkflowEngine {
    /// Ontology store for RDF data
    ontology: OntologyStore,
    /// Five-stage transformation pipeline
    pipeline: FiveStagePipeline,
    /// Pattern execution engine
    pattern_executor: PatternExecutor,
    /// Receipt generator for cryptographic proofs
    receipt_generator: ReceiptGenerator,
    /// Metrics collector for performance monitoring
    metrics: WorkflowMetrics,
}

/// Five-Stage Transformation Pipeline (μ₁-μ₅)
pub struct FiveStagePipeline {
    /// μ₁ (Normalize): RDF validation and SHACL shapes
    normalizer: Stage<μ1, Normalizer>,
    /// μ₂ (Extract): SPARQL queries and OWL inference
    extractor: Stage<μ2, Extractor>,
    /// μ₃ (Emit): Tera template rendering and code generation
    emitter: Stage<μ3, Emitter>,
    /// μ₄ (Canonicalize): Deterministic formatting and content hashing
    canonicalizer: Stage<μ4, Canonicalizer>,
    /// μ₅ (Receipt): Cryptographic proof generation and audit trail
    receipt_stage: Stage<μ5, ReceiptGenerator>,
}

/// Generic Stage Implementation
pub struct Stage<μ, T> {
    /// Stage identifier
    name: &'static str,
    /// Stage implementation
    executor: T,
    /// Input cache
    input_cache: LruCache<StageInput, StageOutput>,
    /// Metrics for this stage
    metrics: StageMetrics,
    /// Stage configuration
    config: StageConfig,
}

/// Workflow Pattern Types
pub enum WorkflowPattern {
    /// Sequential pattern: steps executed in order
    Sequence(SequencePattern),
    /// Parallel pattern: steps executed concurrently
    Parallel(ParallelPattern),
    /// Choice pattern: conditional execution based on evaluation
    Choice(ChoicePattern),
    /// Sync pattern: barrier synchronization for concurrent streams
    Sync(SyncPattern),
}

/// Workflow Context for Execution
#[derive(Debug, Clone)]
pub struct WorkflowContext {
    /// Current ontology reference
    pub ontology: OntologyRef,
    /// Variable store for execution state
    pub variables: VariableStore,
    /// Execution metadata
    pub metadata: ExecutionMetadata,
    /// Execution trace for debugging
    pub trace: ExecutionTrace,
    /// Parent workflow context (for nested workflows)
    pub parent_context: Option<WorkflowContextRef>,
    /// Child workflow contexts
    pub child_contexts: Vec<WorkflowContextRef>,
}
```

### Stage Implementations

```rust
/// μ₁ (Normalize) Stage - RDF Validation and SHACL Shapes
pub struct Normalizer {
    /// SHACL validator
    shacl_validator: SHACLValidator,
    /// RDF normalization engine
    normalizer: RDFNormalizer,
    /// Dependency resolver
    dependency_resolver: DependencyResolver,
    /// Schema validator
    schema_validator: SchemaValidator,
}

impl Normalizer {
    pub fn normalize(&mut self, graph: &RDFGraph) -> Result<NormalizedGraph, WorkflowError> {
        // Validate SHACL constraints
        let validation_result = self.shacl_validator.validate(graph)?;

        if !validation_result.is_valid() {
            return Err(WorkflowError::SHACLValidationFailed(validation_result));
        }

        // Normalize RDF graph
        let normalized = self.normalizer.normalize(graph)?;

        // Resolve dependencies
        let resolved = self.dependency_resolver.resolve(&normalized)?;

        Ok(resolved)
    }
}

/// μ₂ (Extract) Stage - SPARQL Queries and OWL Inference
pub struct Extractor {
    /// SPARQL query engine
    sparql_engine: SPARQLEngine,
    /// OWL reasoner
    owl_reasoner: OWLReasoner,
    /// Rule executor
    rule_executor: RuleExecutor,
    /// Query cache
    query_cache: QueryCache,
}

impl Extractor {
    pub fn extract(&mut self, graph: &RDFGraph, queries: &[SPARQLQuery]) -> Result<ExtractionResult, WorkflowError> {
        let mut results = ExtractionResult::new();

        for query in queries {
            // Check cache first
            if let Some(cached_result) = self.query_cache.get(query) {
                results.add_result(query.clone(), cached_result);
                continue;
            }

            // Execute SPARQL query
            let query_result = self.sparql_engine.execute(graph, query)?;

            // Apply OWL inference
            let inferred_result = self.owl_reasoner.infer(&query_result)?;

            // Execute rules
            let rule_result = self.rule_executor.execute(&inferred_result)?;

            // Cache result
            let final_result = rule_result;
            self.query_cache.put(query.clone(), final_result.clone());
            results.add_result(query.clone(), final_result);
        }

        Ok(results)
    }
}

/// μ₃ (Emit) Stage - Tera Template Rendering and Code Generation
pub struct Emitter {
    /// Tera template engine
    tera: Tera,
    /// Template resolver
    template_resolver: TemplateResolver,
    /// Code generator
    code_generator: CodeGenerator,
    /// File writer
    file_writer: FileWriter,
}

impl Emitter {
    pub fn emit(&mut self, context: &WorkflowContext, templates: &[Template]) -> Result<EmissionResult, WorkflowError> {
        let mut emission_result = EmissionResult::new();

        for template in templates {
            // Resolve template
            let resolved_template = self.template_resolver.resolve(template)?;

            // Create Tera context
            let mut tera_context = self.create_tera_context(context)?;

            // Render template
            let rendered = self.tera.render_str(&resolved_template.content, &tera_context)?;

            // Generate code if needed
            let generated_code = self.code_generator.generate(rendered, template)?;

            // Write to file
            let file_path = self.file_writer.write(&generated_code, template)?;

            emission_result.add_output(template.name.clone(), file_path);
        }

        Ok(emission_result)
    }
}

/// μ₄ (Canonicalize) Stage - Deterministic Formatting and Content Hashing
pub struct Canonicalizer {
    /// Content hasher
    hasher: ContentHasher,
    /// Format normalizer
    format_normalizer: FormatNormalizer,
    /// Sorter for deterministic ordering
    sorter: Sorter,
    /// Merkle tree builder
    merkle_builder: MerkleBuilder,
}

impl Canonicalizer {
    pub fn canonicalize(&mut self, inputs: &[CanonicalizationInput]) -> Result<CanonicalizedResult, WorkflowError> {
        // Sort inputs deterministically
        let sorted_inputs = self.sorter.sort(inputs);

        // Normalize format
        let normalized = self.format_normalizer.normalize(&sorted_inputs)?;

        // Calculate content hash
        let content_hash = self.hasher.hash(&normalized)?;

        // Build Merkle tree
        let merkle_tree = self.merkle_builder.build(&normalized)?;

        Ok(CanonicalizedResult {
            content_hash,
            merkle_tree,
            normalized_content: normalized,
        })
    }
}

/// μ₅ (Receipt) Stage - Cryptographic Proof Generation and Audit Trail
pub struct ReceiptGenerator {
    /// Digital signature engine
    signature_engine: SignatureEngine,
    /// Hash engine
    hash_engine: HashEngine,
    /// Receipt store
    receipt_store: ReceiptStore,
    /// Audit logger
    audit_logger: AuditLogger,
}

impl ReceiptGenerator {
    pub fn generate_receipt(&mut self, execution: &ExecutionTrace) -> Result<WorkflowReceipt, WorkflowError> {
        // Generate hash of execution trace
        let trace_hash = self.hash_engine.hash(execution)?;

        // Generate digital signature
        let signature = self.signature_engine.sign(&trace_hash)?;

        // Create receipt
        let receipt = WorkflowReceipt {
            receipt_id: ReceiptId::new(),
            trace_hash,
            signature,
            metadata: execution.metadata.clone(),
            timestamp: current_timestamp(),
        };

        // Store receipt
        self.receipt_store.store(&receipt)?;

        // Log audit event
        self.audit_logger.log_receipt_generated(&receipt)?;

        Ok(receipt)
    }
}
```

### Pattern Execution Engine

```rust
/// Pattern Executor - Executes workflow patterns
pub struct PatternExecutor {
    /// Sequential pattern executor
    sequence_executor: Box<dyn SequenceExecutor>,
    /// Parallel pattern executor
    parallel_executor: Box<dyn ParallelExecutor>,
    /// Choice pattern executor
    choice_executor: Box<dyn ChoiceExecutor>,
    /// Sync pattern executor
    sync_executor: Box<dyn SyncExecutor>,
    /// Pattern cache for performance
    pattern_cache: PatternCache,
}

/// Sequence Pattern Implementation
pub struct SequencePattern {
    /// Steps to execute in order
    pub steps: Vec<WorkflowStep>,
    /// Step dependencies
    pub dependencies: StepDependencies,
    /// Error handling strategy
    pub error_handling: ErrorHandlingStrategy,
}

impl SequencePattern {
    pub fn execute(&self, context: &WorkflowContext) -> Result<PatternResult, WorkflowError> {
        let mut results = Vec::new();

        for step in &self.steps {
            // Check dependencies
            if !self.dependencies.are_satisfied(step, &results) {
                return Err(WorkflowError::DependencyNotSatisfied(step.id.clone()));
            }

            // Execute step
            let result = step.execute(context)?;
            results.push(result);

            // Handle errors if any
            if let Some(error) = self.error_handling.handle_error(&result) {
                return Err(error);
            }
        }

        Ok(PatternResult::Sequential(results))
    }
}

/// Parallel Pattern Implementation
pub struct ParallelPattern {
    /// Steps to execute in parallel
    pub steps: Vec<WorkflowStep>,
    /// Concurrency limit
    pub concurrency_limit: Option<usize>,
    /// Synchronization strategy
    pub sync_strategy: SyncStrategy,
}

impl ParallelPattern {
    pub fn execute(&self, context: &WorkflowContext) -> Result<PatternResult, WorkflowError> {
        let mut execution = ParallelExecution::new(self.concurrency_limit);

        // Submit all steps for execution
        for step in &self.steps {
            execution.submit(step.clone(), context.clone())?;
        }

        // Wait for completion
        let results = execution.wait_for_completion()?;

        // Apply synchronization strategy
        let synchronized = self.sync_strategy.synchronize(results)?;

        Ok(PatternResult::Parallel(synchronized))
    }
}

/// Choice Pattern Implementation
pub struct ChoicePattern {
    /// Branches for conditional execution
    pub branches: Vec<ChoiceBranch>,
    /// Evaluation strategy
    pub evaluation: EvaluationStrategy,
    /// Default branch (optional)
    pub default_branch: Option<ChoiceBranch>,
}

impl ChoicePattern {
    pub fn execute(&self, context: &WorkflowContext) -> Result<PatternResult, WorkflowError> {
        // Evaluate branches
        let mut selected_branches = Vec::new();

        for branch in &self.branches {
            if self.evaluation.should_execute(branch, context) {
                selected_branches.push(branch.clone());
            }
        }

        // Execute selected branches
        let mut results = Vec::new();
        for branch in selected_branches {
            let result = branch.execute(context)?;
            results.push(result);
        }

        // Execute default branch if no branches selected
        let final_results = if results.is_empty() {
            if let Some(default) = &self.default_branch {
                let result = default.execute(context)?;
                vec![result]
            } else {
                // No default branch and no selected branches
                return Err(WorkflowError::NoBranchSelected);
            }
        } else {
            results
        };

        Ok(PatternResult::Choice(final_results))
    }
}

/// Sync Pattern Implementation
pub struct SyncPattern {
    /// Concurrent streams to synchronize
    pub streams: Vec<ConcurrentStream>,
    /// Synchronization configuration
    pub sync_config: SyncConfig,
    /// Timeout configuration
    pub timeout: Option<Duration>,
}

impl SyncPattern {
    pub fn execute(&self, context: &WorkflowContext) -> Result<PatternResult, WorkflowError> {
        // Create synchronization barrier
        let barrier = SynchronizationBarrier::new(self.streams.len(), self.timeout);

        // Start concurrent streams
        let mut stream_results = Vec::new();
        for stream in &self.streams {
            let result = stream.execute(context)?;
            stream_results.push(result);

            // Wait at barrier
            barrier.wait()?;
        }

        // Apply sync configuration
        let synchronized = self.sync_config.apply(stream_results)?;

        Ok(PatternResult::Sync(synchronized))
    }
}
```

### Erlang NIF Integration

```rust
/// Erlang NIF Binding for Workflow Patterns
#[derive(NifStruct)]
#[module = "Workflow"]
pub struct WorkflowNif {
    /// Workflow identifier
    id: WorkflowId,
    /// Pattern type
    pattern: PatternType,
    /// Configuration
    config: WorkflowConfig,
    /// Current state
    state: WorkflowState,
}

/// NIF Functions for Workflow Execution
#[rust_nif]
impl WorkflowNif {
    /// Create a new workflow
    fn new(id: &str, pattern: PatternType, config: WorkflowConfig) -> Self {
        Self {
            id: WorkflowId::from(id),
            pattern,
            config,
            state: WorkflowState::Created,
        }
    }

    /// Start workflow execution
    fn start(&mut self, context: WorkflowContext) -> atom() {
        match self.pattern.execute(&context) {
            Ok(_) => {
                self.state = WorkflowState::Completed;
                "ok"
            }
            Err(e) => {
                self.state = WorkflowState::Failed(e.to_string());
                "error"
            }
        }
    }

    /// Get workflow status
    fn status(&self) -> atom() {
        match self.state {
            WorkflowState::Created => "created",
            WorkflowState::Running => "running",
            WorkflowState::Completed => "completed",
            WorkflowState::Failed(_) => "failed",
        }
    }
}

/// Pattern Types for Erlang Integration
#[derive(NifStruct)]
#[module = "Pattern"]
pub struct PatternNif {
    /// Pattern type
    pub type_: atom(),
    /// Pattern configuration
    pub config: PatternConfig,
}

/// Pattern Configuration for Erlang
#[derive(NifStruct)]
#[module = "PatternConfig"]
pub struct PatternConfig {
    /// Steps or branches
    pub items: Vec<ItemConfig>,
    /// Configuration options
    pub options: BTreeMap<String, Term>,
}
```

### Integration with Existing System

```rust
/// Integration with ggen-core Pipeline
impl WorkflowEngine {
    pub fn from_pipeline(pipeline: &Pipeline) -> Self {
        // Reuse existing RDF graph and SPARQL capabilities
        Self {
            ontology: OntologyStore::from_graph(pipeline.graph.clone()),
            pipeline: FiveStagePipeline::new(),
            pattern_executor: PatternExecutor::new(),
            receipt_generator: ReceiptGenerator::new(),
            metrics: WorkflowMetrics::new(),
        }
    }
}

/// Integration with ggen-agent System
impl WorkflowEngine {
    pub fn create_agent_workflow(&mut self, agent_config: AgentConfig) -> Result<AgentWorkflow, WorkflowError> {
        // Create workflow context from agent configuration
        let context = WorkflowContext::new_from_agent(&agent_config)?;

        // Create workflow pattern based on agent capabilities
        let pattern = self.create_pattern_for_agent(&agent_config)?;

        // Create and configure workflow
        let workflow = AgentWorkflow {
            id: WorkflowId::new(&agent_config.id),
            pattern,
            context,
            created_at: current_timestamp(),
        };

        Ok(workflow)
    }
}
```

### Performance Optimizations

```rust
/// Hot-Path Optimizations
#[repr(align(64))]
pub struct HotPathCache {
    /// Pattern cache for frequently used patterns
    pattern_cache: LruCache<PatternId, Pattern>,
    /// RDF store cache
    rdf_store: RDFStore,
    /// SPARQL result cache
    sparql_cache: LruCache<SparqlQuery, QueryResults>,
    /// Template cache
    template_cache: LruCache<TemplateId, Template>,
}

/// Zero-Cost Pattern Matching
pub fn execute_pattern<P: WorkflowPattern>(
    pattern: &P,
    context: &WorkflowContext,
) -> Result<PatternResult, WorkflowError> {
    // Compile-time pattern resolution
    match pattern {
        WorkflowPattern::Sequence(seq) => execute_sequence(seq, context),
        WorkflowPattern::Parallel(par) => execute_parallel(par, context),
        WorkflowPattern::Choice(cho) => execute_choice(cho, context),
        WorkflowPattern::Sync(sync) => execute_sync(sync, context),
    }
}

/// Parallel Execution with Rayon
pub struct ParallelExecution {
    executor: ThreadPool,
    tasks: Vec<JoinHandle<WorkflowResult>>,
    results: Vec<WorkflowResult>,
}

impl ParallelExecution {
    pub fn new(concurrency_limit: Option<usize>) -> Self {
        let executor = match concurrency_limit {
            Some(limit) => ThreadPool::new(limit),
            None => ThreadPool::new_parallel(),
        };

        Self {
            executor,
            tasks: Vec::new(),
            results: Vec::new(),
        }
    }

    pub fn submit(&mut self, step: WorkflowStep, context: WorkflowContext) -> Result<(), WorkflowError> {
        let task = self.executor.spawn(move || step.execute(&context));
        self.tasks.push(task);
        Ok(())
    }

    pub fn wait_for_completion(&mut self) -> Result<Vec<WorkflowResult>, WorkflowError> {
        while let Some(task) = self.tasks.pop() {
            let result = task.join().map_err(WorkflowError::TaskFailed)?;
            self.results.push(result);
        }
        Ok(self.results.clone())
    }
}
```

### Error Handling and Recovery

```rust
/// Workflow Error Types
#[derive(Debug)]
pub enum WorkflowError {
    /// Validation error
    ValidationError(String),
    /// Execution error
    ExecutionError(String),
    /// Timeout error
    TimeoutError,
    /// Dependency error
    DependencyError(String),
    /// Resource error
    ResourceError(String),
    /// System error
    SystemError(String),
}

/// Recovery Strategy
pub enum RecoveryStrategy {
    /// Retry with exponential backoff
    Retry(RetryConfig),
    /// Fallback to alternative
    Fallback(FallbackAction),
    /// Circuit breaker
    CircuitBreaker(CircuitBreakerConfig),
    /// Manual intervention
    ManualIntervention,
}

/// Retry Configuration
pub struct RetryConfig {
    pub max_attempts: usize,
    pub base_delay_ms: u64,
    pub max_delay_ms: u64,
    pub backoff_multiplier: f64,
    pub jitter: bool,
}

/// Fallback Action
pub enum FallbackAction {
    UseDefault,
    UseCachedValue,
    ExecuteAlternative,
    ReturnError,
}
```

### Usage Examples

```rust
/// Example: Creating and executing a workflow
#[tokio::main]
async fn example() -> Result<(), WorkflowError> {
    // Create workflow engine
    let mut engine = WorkflowEngine::new();

    // Create workflow context
    let context = WorkflowContext {
        ontology: OntologyRef::new("example.org"),
        variables: VariableStore::new(),
        metadata: ExecutionMetadata::new(),
        trace: ExecutionTrace::new(),
        parent_context: None,
        child_contexts: Vec::new(),
    };

    // Create workflow pattern
    let pattern = WorkflowPattern::Sequence(SequencePattern {
        steps: vec![
            WorkflowStep::new("step1", Box::new(Step1 {})),
            WorkflowStep::new("step2", Box::new(Step2 {})),
            WorkflowStep::new("step3", Box::new(Step3 {})),
        ],
        dependencies: StepDependencies::new(),
        error_handling: ErrorHandlingStrategy::Continue,
    });

    // Execute workflow
    let result = engine.execute_pattern(&pattern, &context)?;

    // Generate receipt
    let receipt = engine.generate_receipt(&result.trace)?;

    println!("Workflow completed with receipt: {}", receipt.receipt_id);
    Ok(())
}

/// Example: Parallel workflow execution
async fn parallel_example() -> Result<(), WorkflowError> {
    let mut engine = WorkflowEngine::new();

    let context = WorkflowContext::new();

    let pattern = WorkflowPattern::Parallel(ParallelPattern {
        steps: vec![
            WorkflowStep::new("step1", Box::new(ParallelStep1 {})),
            WorkflowStep::new("step2", Box::new(ParallelStep2 {})),
            WorkflowStep::new("step3", Box::new(ParallelStep3 {})),
        ],
        concurrency_limit: Some(2),
        sync_strategy: SyncStrategy::WaitAll,
    });

    let result = engine.execute_pattern(&pattern, &context)?;
    println!("Parallel workflow completed with {} results", result.len());

    Ok(())
}
```

## Conclusion

The `ggen-workflow` architecture provides a robust, type-safe, and performant framework for executing deterministic workflows based on RDF ontology specifications. It implements the five-stage transformation pipeline with support for various workflow patterns and provides seamless integration with existing ggen components. The architecture ensures compile-time safety through comprehensive error handling and provides advanced features like cryptographic receipts and performance optimizations for enterprise deployment.