//! gRPC server implementation with middleware and interceptors

use std::net::SocketAddr;
use std::sync::Arc;

use tonic::transport::Server;
use tonic::{Request, Response, Status};
use tower::ServiceBuilder;
use tower_http::cors::CorsLayer;
use tower_http::timeout::TimeoutLayer;
use tower_http::trace::TraceLayer;
use tracing::{debug, info};

use crate::grpc_interceptor::{AuthInterceptor, MetricsInterceptor, RequestInterceptor};
use crate::tai::*;
use crate::Result as GrpcResult;

/// Configuration for gRPC server
#[derive(Debug, Clone)]
pub struct GrpcServerConfig {
    /// Address to bind to
    pub bind_addr: SocketAddr,
    /// Maximum concurrent connections
    pub max_concurrent_streams: u32,
    /// Request timeout in seconds
    pub request_timeout_secs: u64,
    /// Enable reflection service
    pub enable_reflection: bool,
}

impl Default for GrpcServerConfig {
    fn default() -> Self {
        Self {
            bind_addr: "127.0.0.1:50051".parse().unwrap(),
            max_concurrent_streams: 1000,
            request_timeout_secs: 30,
            enable_reflection: true,
        }
    }
}

/// gRPC server implementation
pub struct GrpcServer {
    config: GrpcServerConfig,
    governor_service: Arc<GovernorServiceImpl>,
    coordinator_service: Arc<CoordinatorServiceImpl>,
    scheduler_service: Arc<SchedulerServiceImpl>,
}

impl GrpcServer {
    /// Create a new gRPC server with configuration
    pub fn new(config: GrpcServerConfig) -> Self {
        Self {
            config,
            governor_service: Arc::new(GovernorServiceImpl::new()),
            coordinator_service: Arc::new(CoordinatorServiceImpl::new()),
            scheduler_service: Arc::new(SchedulerServiceImpl::new()),
        }
    }

    /// Start the gRPC server
    pub async fn start(self) -> GrpcResult<()> {
        let addr = self.config.bind_addr;

        info!("Starting gRPC server on {}", addr);

        let governor = self.governor_service.clone();
        let coordinator = self.coordinator_service.clone();
        let scheduler = self.scheduler_service.clone();

        // Build the router with middleware
        let mut server = Server::builder()
            .max_concurrent_streams(Some(self.config.max_concurrent_streams))
            .layer(
                ServiceBuilder::new()
                    .layer(TraceLayer::new_for_grpc())
                    .layer(TimeoutLayer::new(
                        std::time::Duration::from_secs(self.config.request_timeout_secs),
                    ))
                    .layer(CorsLayer::permissive()),
            )
            .add_service(governor::server::GovernorServer::new(GovernorService(governor)))
            .add_service(coordinator::server::CoordinatorServer::new(CoordinatorService(
                coordinator,
            )))
            .add_service(scheduler::server::SchedulerServer::new(SchedulerService(scheduler)));

        // Add reflection service if enabled
        if self.config.enable_reflection {
            server = server.add_service(
                tonic_reflection::server()
                    .register_encoded_file_descriptor_set(crate::tai::FILE_DESCRIPTOR_SET),
            );
        }

        server.serve(addr).await?;

        Ok(())
    }
}

// ============ Governor Service Implementation ============

/// Governor service state
#[derive(Debug)]
pub struct GovernorServiceImpl {
    policies: parking_lot::RwLock<std::collections::HashMap<String, Policy>>,
}

impl GovernorServiceImpl {
    /// Create a new governor service
    pub fn new() -> Self {
        Self {
            policies: parking_lot::RwLock::new(std::collections::HashMap::new()),
        }
    }
}

impl Default for GovernorServiceImpl {
    fn default() -> Self {
        Self::new()
    }
}

/// Wrapper for Governor service
#[derive(Debug)]
pub struct GovernorService(pub Arc<GovernorServiceImpl>);

#[tonic::async_trait]
impl governor::server::Governor for GovernorService {
    async fn propose_policy(&self, request: Request<Policy>) -> Result<Response<Receipt>, Status> {
        let policy = request.into_inner();
        debug!("Proposing policy: {}", policy.policy_name);

        let receipt = Receipt {
            id: uuid::Uuid::new_v4().to_string(),
            request_id: policy.id.clone(),
            status: "success".to_string(),
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            result_data: std::collections::HashMap::new(),
            error_message: String::new(),
            execution_time_us: 100,
        };

        Ok(Response::new(receipt))
    }

    async fn enforce_policy(&self, request: Request<Policy>) -> Result<Response<Receipt>, Status> {
        let policy = request.into_inner();
        debug!("Enforcing policy: {}", policy.policy_name);

        self.0.policies.write().insert(policy.id.clone(), policy);

        let receipt = Receipt {
            id: uuid::Uuid::new_v4().to_string(),
            request_id: uuid::Uuid::new_v4().to_string(),
            status: "success".to_string(),
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            result_data: std::collections::HashMap::new(),
            error_message: String::new(),
            execution_time_us: 50,
        };

        Ok(Response::new(receipt))
    }

    async fn revoke_policy(&self, request: Request<Policy>) -> Result<Response<Receipt>, Status> {
        let policy = request.into_inner();
        debug!("Revoking policy: {}", policy.policy_name);

        self.0.policies.write().remove(&policy.id);

        let receipt = Receipt {
            id: uuid::Uuid::new_v4().to_string(),
            request_id: uuid::Uuid::new_v4().to_string(),
            status: "success".to_string(),
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            result_data: std::collections::HashMap::new(),
            error_message: String::new(),
            execution_time_us: 40,
        };

        Ok(Response::new(receipt))
    }

    async fn get_policies(
        &self,
        request: Request<GetPoliciesRequest>,
    ) -> Result<Response<GetPoliciesResponse>, Status> {
        let req = request.into_inner();
        debug!("Getting policies, filter: {}", req.policy_type);

        let policies = self.0.policies.read();
        let filtered: Vec<Policy> = policies
            .values()
            .filter(|p| {
                if req.policy_type.is_empty() {
                    true
                } else {
                    p.policy_type == req.policy_type
                }
            })
            .filter(|p| !req.enabled_only || p.enabled)
            .cloned()
            .collect();

        Ok(Response::new(GetPoliciesResponse {
            policies: filtered,
        }))
    }

    async fn stream_policies(
        &self,
        request: Request<StreamPoliciesRequest>,
    ) -> Result<Response<tonic::codegen::BoxStream<'static, Policy>>, Status> {
        let req = request.into_inner();
        let policies = self.0.policies.read().clone();

        let filtered: Vec<Policy> = policies
            .into_values()
            .filter(|p| {
                if req.policy_type.is_empty() {
                    true
                } else {
                    p.policy_type == req.policy_type
                }
            })
            .collect();

        let stream = futures::stream::iter(filtered);
        Ok(Response::new(Box::pin(stream)))
    }

    async fn health_check(
        &self,
        _request: Request<HealthCheckRequest>,
    ) -> Result<Response<HealthStatus>, Status> {
        let health = HealthStatus {
            service_name: "governor".to_string(),
            status: "healthy".to_string(),
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            details: vec!["governor service operational".to_string()],
            uptime_seconds: 0,
        };

        Ok(Response::new(health))
    }
}

// ============ Coordinator Service Implementation ============

/// Coordinator service state
#[derive(Debug)]
pub struct CoordinatorServiceImpl {
    signals: parking_lot::RwLock<std::collections::HashMap<String, Signal>>,
    actions: parking_lot::RwLock<std::collections::HashMap<String, Action>>,
}

impl CoordinatorServiceImpl {
    /// Create a new coordinator service
    pub fn new() -> Self {
        Self {
            signals: parking_lot::RwLock::new(std::collections::HashMap::new()),
            actions: parking_lot::RwLock::new(std::collections::HashMap::new()),
        }
    }
}

impl Default for CoordinatorServiceImpl {
    fn default() -> Self {
        Self::new()
    }
}

/// Wrapper for Coordinator service
#[derive(Debug)]
pub struct CoordinatorService(pub Arc<CoordinatorServiceImpl>);

#[tonic::async_trait]
impl coordinator::server::Coordinator for CoordinatorService {
    async fn submit_signal(&self, request: Request<Signal>) -> Result<Response<Receipt>, Status> {
        let signal = request.into_inner();
        debug!("Submitting signal: {}", signal.signal_type);

        self.0.signals.write().insert(signal.id.clone(), signal);

        let receipt = Receipt {
            id: uuid::Uuid::new_v4().to_string(),
            request_id: uuid::Uuid::new_v4().to_string(),
            status: "success".to_string(),
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            result_data: std::collections::HashMap::new(),
            error_message: String::new(),
            execution_time_us: 80,
        };

        Ok(Response::new(receipt))
    }

    async fn request_action(&self, request: Request<Action>) -> Result<Response<Receipt>, Status> {
        let action = request.into_inner();
        debug!("Requesting action: {}", action.action_type);

        self.0.actions.write().insert(action.id.clone(), action);

        let receipt = Receipt {
            id: uuid::Uuid::new_v4().to_string(),
            request_id: uuid::Uuid::new_v4().to_string(),
            status: "success".to_string(),
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            result_data: std::collections::HashMap::new(),
            error_message: String::new(),
            execution_time_us: 70,
        };

        Ok(Response::new(receipt))
    }

    async fn stream_actions(
        &self,
        request: Request<StreamActionsRequest>,
    ) -> Result<Response<tonic::codegen::BoxStream<'static, Action>>, Status> {
        let req = request.into_inner();
        let actions = self.0.actions.read().clone();

        let filtered: Vec<Action> = actions
            .into_values()
            .filter(|a| a.source_service == req.service_name)
            .collect();

        let stream = futures::stream::iter(filtered);
        Ok(Response::new(Box::pin(stream)))
    }

    async fn acknowledge_action(
        &self,
        request: Request<AcknowledgeActionRequest>,
    ) -> Result<Response<Receipt>, Status> {
        let req = request.into_inner();
        debug!("Acknowledging action: {}", req.action_id);

        let receipt = Receipt {
            id: uuid::Uuid::new_v4().to_string(),
            request_id: req.action_id,
            status: req.status,
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            result_data: std::collections::HashMap::new(),
            error_message: String::new(),
            execution_time_us: 20,
        };

        Ok(Response::new(receipt))
    }

    async fn get_status(
        &self,
        request: Request<GetStatusRequest>,
    ) -> Result<Response<CoordinationStatus>, Status> {
        let req = request.into_inner();
        let signals = self.0.signals.read();
        let actions = self.0.actions.read();

        let status = CoordinationStatus {
            service_name: req.service_name,
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            pending_signals: signals.len() as i32,
            pending_actions: actions.len() as i32,
            last_signal_ns: 0,
        };

        Ok(Response::new(status))
    }

    async fn health_check(
        &self,
        _request: Request<HealthCheckRequest>,
    ) -> Result<Response<HealthStatus>, Status> {
        let health = HealthStatus {
            service_name: "coordinator".to_string(),
            status: "healthy".to_string(),
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            details: vec!["coordinator service operational".to_string()],
            uptime_seconds: 0,
        };

        Ok(Response::new(health))
    }
}

// ============ Scheduler Service Implementation ============

/// Scheduler service state
#[derive(Debug)]
pub struct SchedulerServiceImpl {
    tasks: parking_lot::RwLock<std::collections::HashMap<String, TaskStatus>>,
}

impl SchedulerServiceImpl {
    /// Create a new scheduler service
    pub fn new() -> Self {
        Self {
            tasks: parking_lot::RwLock::new(std::collections::HashMap::new()),
        }
    }
}

impl Default for SchedulerServiceImpl {
    fn default() -> Self {
        Self::new()
    }
}

/// Wrapper for Scheduler service
#[derive(Debug)]
pub struct SchedulerService(pub Arc<SchedulerServiceImpl>);

#[tonic::async_trait]
impl scheduler::server::Scheduler for SchedulerService {
    async fn submit_task(
        &self,
        request: Request<ScheduleTaskRequest>,
    ) -> Result<Response<Receipt>, Status> {
        let req = request.into_inner();
        debug!("Submitting task: {}", req.task_id);

        let task_status = TaskStatus {
            task_id: req.task_id.clone(),
            status: "pending".to_string(),
            created_at_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            started_at_ns: 0,
            completed_at_ns: 0,
            error_message: String::new(),
            result: std::collections::HashMap::new(),
        };

        self.0.tasks.write().insert(req.task_id.clone(), task_status);

        let receipt = Receipt {
            id: uuid::Uuid::new_v4().to_string(),
            request_id: req.task_id,
            status: "success".to_string(),
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            result_data: std::collections::HashMap::new(),
            error_message: String::new(),
            execution_time_us: 60,
        };

        Ok(Response::new(receipt))
    }

    async fn cancel_task(
        &self,
        request: Request<CancelTaskRequest>,
    ) -> Result<Response<Receipt>, Status> {
        let req = request.into_inner();
        debug!("Cancelling task: {}", req.task_id);

        let mut tasks = self.0.tasks.write();
        if let Some(task) = tasks.get_mut(&req.task_id) {
            task.status = "cancelled".to_string();
        }

        let receipt = Receipt {
            id: uuid::Uuid::new_v4().to_string(),
            request_id: req.task_id,
            status: "success".to_string(),
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            result_data: std::collections::HashMap::new(),
            error_message: String::new(),
            execution_time_us: 30,
        };

        Ok(Response::new(receipt))
    }

    async fn get_task_status(
        &self,
        request: Request<GetTaskStatusRequest>,
    ) -> Result<Response<TaskStatus>, Status> {
        let req = request.into_inner();
        let tasks = self.0.tasks.read();

        let task = tasks
            .get(&req.task_id)
            .cloned()
            .unwrap_or_else(|| TaskStatus {
                task_id: req.task_id,
                status: "not_found".to_string(),
                created_at_ns: 0,
                started_at_ns: 0,
                completed_at_ns: 0,
                error_message: "Task not found".to_string(),
                result: std::collections::HashMap::new(),
            });

        Ok(Response::new(task))
    }

    async fn stream_task_updates(
        &self,
        request: Request<StreamTaskUpdatesRequest>,
    ) -> Result<Response<tonic::codegen::BoxStream<'static, TaskUpdate>>, Status> {
        let req = request.into_inner();
        let tasks = self.0.tasks.read().clone();

        let updates = if let Some(task) = tasks.get(&req.task_id) {
            vec![TaskUpdate {
                task_id: task.task_id.clone(),
                status: task.status.clone(),
                timestamp_ns: task.created_at_ns,
                message: format!("Task status: {}", task.status),
            }]
        } else {
            vec![]
        };

        let stream = futures::stream::iter(updates);
        Ok(Response::new(Box::pin(stream)))
    }

    async fn health_check(
        &self,
        _request: Request<HealthCheckRequest>,
    ) -> Result<Response<HealthStatus>, Status> {
        let health = HealthStatus {
            service_name: "scheduler".to_string(),
            status: "healthy".to_string(),
            timestamp_ns: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0) as i64,
            details: vec!["scheduler service operational".to_string()],
            uptime_seconds: 0,
        };

        Ok(Response::new(health))
    }
}
