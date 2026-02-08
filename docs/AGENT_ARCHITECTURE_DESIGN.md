# ggen-agent Architecture Design

## Overview

This document details the architecture design for the `ggen-agent` crate, which serves as the event bridge connecting A2A (Agent-to-Agent) protocol agents with YAWL workflows in the ggen ecosystem.

## Core Architecture

### System Components

```rust
/// Agent System Core - Central coordinator for all agent operations
pub struct AgentSystem {
    /// Registry of all active agents
    registry: AgentRegistry,
    /// Bridge between A2A protocol and YAWL workflows
    bridge: AgentBridge,
    /// MCP (Model Context Protocol) integration hub
    mcp_hub: McpHub,
    /// Workflow coordinator for agent lifecycle
    workflow_coordinator: WorkflowCoordinator,
    /// Event bus for agent communication
    event_bus: EventBus,
    /// Security framework for agent operations
    security: SecurityFramework,
    /// Observability and monitoring
    observability: Observability,
}

/// Agent State Machine with Type-Level Guarantees
#[derive(Debug, Clone, PartialEq)]
pub enum AgentState {
    Initializing,  // Agent being created
    Ready,        // Agent ready for work
    Executing,    // Agent running workflow
    Waiting,      // Agent waiting for input
    Completed,    // Agent completed workflow
    Error(String), // Agent in error state
}

/// Agent Capabilities and Configuration
#[derive(Debug, Clone)]
pub struct AgentCapabilities {
    /// Workflow patterns the agent can execute
    pub patterns: Vec<WorkflowPattern>,
    /// Communication protocols supported
    pub protocols: Vec<ProtocolType>,
    /// Performance throughput specifications
    pub throughput: ThroughputSpec,
    /// Security profile for the agent
    pub security: SecurityProfile,
    /// Resource requirements
    pub resources: ResourceSpec,
}

/// Agent Identity and Metadata
#[derive(Debug, Clone)]
pub struct Agent {
    pub id: AgentId,
    pub name: String,
    pub version: String,
    pub state: AgentState,
    pub capabilities: AgentCapabilities,
    pub workflow_context: WorkflowContext,
    pub created_at: u64,
    pub last_active: u64,
}
```

### A2A Protocol Integration

```rust
/// A2A Protocol Stack - Complete agent communication framework
pub struct A2AProtocolStack {
    /// Transport layer for agent communication
    transport: A2ATransport,
    /// Messaging system for inter-agent communication
    messaging: A2AMessaging,
    /// Discovery service for agent location
    discovery: A2ADiscovery,
    /// Security layer for authentication and encryption
    security: A2ASecurity,
    /// Protocol version management
    version_manager: ProtocolVersionManager,
}

/// Agent-to-Event Bridge - Connects A2A agents to ggen workflows
pub struct AgentBridge {
    /// A2A protocol stack implementation
    a2a_stack: A2AProtocolStack,
    /// YAWL workflow engine integration
    yawl_engine: YAWLEngine,
    /// Event format converter
    event_converter: EventConverter,
    /// Agent state manager
    state_manager: AgentStateManager,
    /// Workflow mapping engine
    workflow_mapper: WorkflowMapper,
}

/// A2A Transport Layer
pub enum A2ATransport {
    /// Direct WebSocket connections
    WebSocket(WebSocketTransport),
    /// Message broker based
    MessageBroker(MessageBrokerTransport),
    /// HTTP/HTTPS based
    HTTP(HTTPTransport),
    /// Custom transport implementation
    Custom(Box<dyn Transport>),
}

/// A2A Messaging System
pub struct A2AMessaging {
    /// Message queue for agent communication
    message_queue: MessageQueue,
    /// Message routing and dispatch
    router: MessageRouter,
    /// Message serialization/deserialization
    codec: MessageCodec,
    /// Message priority system
    priority_system: PrioritySystem,
}
```

### Type-Level Agent State Machine

```rust
/// Compile-time state machine for agents
pub struct Agent<S: AgentState> {
    /// Agent identifier
    id: AgentId,
    /// State phantom data for type safety
    state: PhantomData<S>,
    /// Agent capabilities
    capabilities: AgentCapabilities,
    /// Workflow execution context
    workflow_context: WorkflowContext,
    /// Security context
    security_context: SecurityContext,
}

/// Type-safe agent creation
impl Agent<Initializing> {
    pub fn new(id: AgentId, config: AgentConfig) -> Result<Agent<Ready>, AgentError> {
        // Validate configuration
        if !config.is_valid() {
            return Err(AgentError::InvalidConfiguration);
        }

        // Initialize security context
        let security_context = SecurityContext::new(&config.security)?;

        // Create workflow context
        let workflow_context = WorkflowContext::new(&config.workflow)?;

        Ok(Agent {
            id,
            state: PhantomData,
            capabilities: config.capabilities,
            workflow_context,
            security_context,
        })
    }
}

/// Type-safe state transitions
impl Agent<Ready> {
    pub fn start_workflow(self, workflow_id: WorkflowId) -> Result<Agent<Executing>, AgentError> {
        // Validate workflow compatibility
        if !self.capabilities.supports_workflow(&workflow_id) {
            return Err(AgentError::WorkflowNotSupported);
        }

        // Start workflow execution
        let workflow_context = self.workflow_context.start(workflow_id)?;

        Ok(Agent {
            id: self.id,
            state: PhantomData,
            capabilities: self.capabilities,
            workflow_context,
            security_context: self.security_context,
        })
    }
}

impl Agent<Executing> {
    pub fn wait_for_input(self) -> Agent<Waiting> {
        // Transition to waiting state
        Agent {
            id: self.id,
            state: PhantomData,
            capabilities: self.capabilities,
            workflow_context: self.workflow_context,
            security_context: self.security_context,
        }
    }

    pub fn complete(self, result: WorkflowResult) -> Result<Agent<Completed>, AgentError> {
        // Validate completion conditions
        if !self.workflow_context.can_complete() {
            return Err(AgentError::PrematureCompletion);
        }

        // Finalize workflow
        let workflow_context = self.workflow_context.complete(result)?;

        Ok(Agent {
            id: self.id,
            state: PhantomData,
            capabilities: self.capabilities,
            workflow_context,
            security_context: self.security_context,
        })
    }
}
```

### Workflow Integration

```rust
/// Workflow Coordinator - Manages agent workflow lifecycle
pub struct WorkflowCoordinator {
    /// Active workflows
    workflows: BTreeMap<WorkflowId, ActiveWorkflow>,
    /// Workflow templates
    templates: WorkflowTemplateStore,
    /// Workflow execution engine
    executor: WorkflowExecutor,
    /// Workflow result collector
    result_collector: ResultCollector,
    /// Workflow metrics
    metrics: WorkflowMetrics,
}

/// Active Workflow Instance
pub struct ActiveWorkflow {
    pub id: WorkflowId,
    pub agent_id: AgentId,
    pub template: WorkflowTemplate,
    pub state: WorkflowState,
    pub context: WorkflowContext,
    pub created_at: u64,
    pub started_at: Option<u64>,
    pub completed_at: Option<u64>,
}

/// Workflow State Machine
#[derive(Debug, Clone, PartialEq)]
pub enum WorkflowState {
    Pending,
    Running,
    Waiting,
    Completed,
    Failed(WorkflowError),
    Cancelled,
}

/// Workflow Context for Agent Execution
#[derive(Debug, Clone)]
pub struct WorkflowContext {
    pub variables: VariableStore,
    pub metadata: ExecutionMetadata,
    pub trace: ExecutionTrace,
    pub parent_context: Option<WorkflowContextRef>,
    pub child_contexts: Vec<WorkflowContextRef>,
}

impl WorkflowContext {
    pub fn new(template: &WorkflowTemplate) -> Result<Self, WorkflowError> {
        let mut variables = VariableStore::new();
        variables.initialize_from_template(template)?;

        Ok(Self {
            variables,
            metadata: ExecutionMetadata::new(),
            trace: ExecutionTrace::new(),
            parent_context: None,
            child_contexts: Vec::new(),
        })
    }

    pub fn start(&mut self, workflow_id: WorkflowId) -> Result<(), WorkflowError> {
        self.metadata.start_time = Some(current_timestamp());
        self.metadata.workflow_id = Some(workflow_id);
        Ok(())
    }

    pub fn complete(&mut self, result: WorkflowResult) -> Result<Self, WorkflowError> {
        self.metadata.end_time = Some(current_timestamp());
        self.metadata.result = Some(result);
        Ok(self.clone())
    }
}
```

### Event Bus System

```rust
/// Event Bus for Agent Communication
pub struct EventBus {
    /// Topic-based message routing
    topics: BTreeMap<Topic, TopicQueue>,
    /// Event type registry
    event_registry: EventTypeRegistry,
    /// Event serializer
    serializer: EventSerializer,
    /// Event router
    router: EventRouter,
    /// Dead letter queue
    dlq: DeadLetterQueue,
}

/// Event Types for Agent Communication
#[derive(Debug, Clone)]
pub enum AgentEvent {
    /// Agent created
    AgentCreated { agent_id: AgentId, config: AgentConfig },
    /// Agent started
    AgentStarted { agent_id: AgentId, workflow_id: WorkflowId },
    /// Agent completed
    AgentCompleted { agent_id: AgentId, result: WorkflowResult },
    /// Agent error
    AgentError { agent_id: AgentId, error: AgentError },
    /// Workflow event
    WorkflowEvent { workflow_id: WorkflowId, event: WorkflowEvent },
    /// System event
    SystemEvent { event: SystemEvent },
}

/// Event Router for Message Distribution
pub struct EventRouter {
    /// Subscription management
    subscriptions: BTreeMap<Topic, Vec<Subscription>>,
    /// Event filters
    filters: Vec<EventFilter>,
    /// Event transformers
    transformers: Vec<EventTransformer>,
    /// Metrics collector
    metrics: EventMetrics,
}

/// Event Subscription
pub struct Subscription {
    pub id: SubscriptionId,
    pub topic: Topic,
    pub filter: Option<EventFilter>,
    pub handler: EventHandler,
    pub created_at: u64,
}
```

### Security Framework

```rust
/// Security Framework for Agent Operations
pub struct SecurityFramework {
    /// Authentication engine
    authentication: AuthEngine,
    /// Authorization engine
    authorization: AuthZEngine,
    /// Encryption engine
    encryption: EncryptionEngine,
    /// Audit engine
    audit: AuditEngine,
    /// Certificate management
    certs: CertificateManager,
}

/// Agent Security Profile
#[derive(Debug, Clone)]
pub struct SecurityProfile {
    /// Authentication method
    pub auth_method: AuthMethod,
    /// Authorization roles
    pub roles: Vec<String>,
    /// Encryption requirements
    pub encryption: EncryptionSpec,
    /// Certificate requirements
    pub certificates: CertSpec,
    /// Security constraints
    pub constraints: SecurityConstraints,
}

/// Authentication Methods
pub enum AuthMethod {
    JWT { issuer: String, audience: String },
    OAuth2 { provider: String, scopes: Vec<String> },
    APIKey { key_id: String, key_value: String },
    MutualTLS { certs: CertSpec },
}

/// Authorization Engine
pub struct AuthZEngine {
    /// Policy store
    policy_store: PolicyStore,
    /// Role-based access control
    rbac: RBACSystem,
    /// Attribute-based access control
    abac: ABACSystem,
    /// Context-aware access control
    context_access: ContextAccessControl,
}

/// Authorization Result
pub struct AuthZResult {
    pub allowed: bool,
    pub reason: Option<String>,
    pub constraints: Vec<String>,
    pub audit_log: AuditLog,
}
```

### Observability and Monitoring

```rust
/// Observability Framework
pub struct Observability {
    /// OpenTelemetry tracer
    tracer: Tracer,
    /// Metrics collector
    meter: Meter,
    /// Logs collector
    logger: Logger,
    /// Trace context propagation
    propagator: TextMapPropagator,
    /// Alert manager
    alert_manager: AlertManager,
}

/// Agent Metrics
#[derive(Debug, Clone)]
pub struct AgentMetrics {
    /// Agent lifecycle metrics
    pub lifecycle: LifecycleMetrics,
    /// Performance metrics
    pub performance: PerformanceMetrics,
    /// Resource usage metrics
    pub resources: ResourceMetrics,
    /// Workflow metrics
    pub workflow: WorkflowMetrics,
    /// Error metrics
    pub errors: ErrorMetrics,
}

/// Performance Metrics
#[derive(Debug, Clone)]
pub struct PerformanceMetrics {
    /// Average response time
    pub avg_response_time_ms: f64,
    /// Requests per second
    pub requests_per_second: f64,
    /// Error rate
    pub error_rate: f64,
    /// Throughput
    pub throughput: u64,
    /// Latency percentiles
    pub percentiles: PercentileMetrics,
}

/// Alert Manager
pub struct AlertManager {
    /// Alert rules
    rules: Vec<AlertRule>,
    /// Notification channels
    channels: Vec<NotificationChannel>,
    /// Alert history
    history: AlertHistory,
    /// Current active alerts
    active_alerts: BTreeMap<AlertId, Alert>,
}

/// Alert Rule
pub struct AlertRule {
    pub id: AlertId,
    pub name: String,
    pub condition: AlertCondition,
    pub severity: AlertSeverity,
    pub channels: Vec<NotificationChannelId>,
    pub enabled: bool,
}
```

### Integration with Five-Stage Pipeline

The `ggen-agent` crate integrates with the existing five-stage transformation pipeline as follows:

#### μ₁ (Normalize) Stage Integration
- Agent specifications validated against RDF schema
- Security profiles normalized to standard format
- Configuration parameters validated and defaulted

#### μ₂ (Extract) Stage Integration
- SPARQL queries extract agent relationships and dependencies
- Agent capabilities discovered from ontology
- Workflow templates retrieved from RDF store

#### μ₃ (Emit) Stage Integration
- Tera templates generate agent configurations
- Workflow specifications generated from templates
- Event handlers and routing logic generated

#### μ₄ (Canonicalize) Stage Integration
- Agent state canonicalized to standard format
- Event messages standardized
- Workflow results normalized

#### μ₅ (Receipt) Stage Integration
- Cryptographic receipts generated for agent execution
- Audit trail created for agent lifecycle
- Provenance information stored for generated artifacts

### Error Handling

```rust
/// Agent Error Types
#[derive(Debug)]
pub enum AgentError {
    /// Invalid configuration
    InvalidConfiguration(String),
    /// Authentication failed
    AuthenticationFailed,
    /// Authorization denied
    AuthorizationDenied(String),
    /// Workflow execution failed
    WorkflowExecutionFailed(WorkflowError),
    /// Communication error
    CommunicationError(String),
    /// Resource unavailable
    ResourceUnavailable(String),
    /// State transition invalid
    InvalidStateTransition(AgentState, AgentState),
    /// System error
    SystemError(String),
}

/// Result Type for Agent Operations
pub type AgentResult<T> = Result<T, AgentError>;

/// Error Recovery Strategies
pub enum RecoveryStrategy {
    /// Retry operation
    Retry(RetryConfig),
    /// Circuit breaker
    CircuitBreaker(CircuitBreakerConfig),
    /// Fallback to alternative
    Fallback(FallbackAction),
    /// Manual intervention required
    ManualIntervention,
}
```

### Performance Optimizations

```rust
/// Hot-Path Cache for Agent Operations
#[repr(align(64))]
pub struct HotPathCache {
    /// Agent state cache
    agent_states: LruCache<AgentId, AgentState>,
    /// Workflow template cache
    workflow_templates: LruCache<WorkflowId, WorkflowTemplate>,
    /// Event handler cache
    event_handlers: LruCache<EventType, EventHandler>,
    /// SPARQL result cache
    sparql_cache: LruCache<SparqlQuery, QueryResults>,
}

/// Zero-Cost Agent Scheduler
pub struct AgentScheduler {
    /// Fixed-size array for performance
    agents: ArrayVec<AgentHandle, 100>,
    /// Schedule table for agent execution
    schedule: ScheduleTable,
    /// Priority queue for agent scheduling
    priority_queue: PriorityQueue<AgentId, Priority>,
    /// Load balancer
    load_balancer: LoadBalancer,
}

/// Schedule Table for Agent Execution
pub struct ScheduleTable {
    /// Time slots for agent execution
    time_slots: Vec<TimeSlot>,
    /// Agent assignments
    assignments: BTreeMap<AgentId, TimeSlotId>,
    /// Load metrics
    load_metrics: LoadMetrics,
    /// Scheduling algorithm
    algorithm: SchedulingAlgorithm,
}
```

### Usage Examples

```rust
/// Example: Creating and starting an agent
#[tokio::main]
async fn example() -> AgentResult<()> {
    // Create agent configuration
    let config = AgentConfig {
        id: AgentId::new("example-agent"),
        name: "Example Agent".to_string(),
        version: "1.0.0".to_string(),
        capabilities: AgentCapabilities {
            patterns: vec![WorkflowPattern::Sequential],
            protocols: vec![ProtocolType::HTTP, ProtocolType::WebSocket],
            throughput: ThroughputSpec {
                max_concurrent: 10,
                requests_per_second: 100,
            },
            security: SecurityProfile::default(),
        },
        workflow: WorkflowTemplate::default(),
    };

    // Create agent with type safety
    let agent = Agent::<Initializing>::new(config.id.clone(), config)?;

    // Start workflow
    let agent = agent.start_workflow(WorkflowId::new("example-workflow"))?;

    // Wait for completion
    match agent.state {
        AgentState::Executing => {
            // Continue execution...
        }
        _ => return Err(AgentError::InvalidStateTransition),
    }

    Ok(())
}

/// Example: Event handling
async fn event_handling_example() -> AgentResult<()> {
    // Create event bus
    let event_bus = EventBus::new();

    // Subscribe to events
    let subscription = event_bus.subscribe("agent.lifecycle".to_string(), |event| {
        async move {
            match event {
                AgentEvent::AgentStarted { agent_id, workflow_id } => {
                    println!("Agent {} started workflow {}", agent_id, workflow_id);
                }
                _ => {}
            }
        }
    });

    // Publish event
    let event = AgentEvent::AgentStarted {
        agent_id: AgentId::new("test-agent"),
        workflow_id: WorkflowId::new("test-workflow"),
    };

    event_bus.publish("agent.lifecycle".to_string(), event).await?;

    Ok(())
}
```

## Conclusion

The `ggen-agent` architecture provides a type-safe, secure, and observable framework for agent-based workflow execution. It seamlessly integrates with the existing ggen five-stage pipeline while providing advanced capabilities for agent coordination, event handling, and observability. The architecture ensures compile-time safety through type-level state machines and provides comprehensive monitoring and security features for enterprise deployment.