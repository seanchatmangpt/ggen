//! gRPC client with resilience patterns

use std::sync::Arc;

use tonic::transport::Channel;
use tracing::{debug, info};

use crate::resilience::{
    CircuitBreaker, CircuitBreakerConfig, LoadBalancer, LoadBalancingStrategy, RetryConfig,
    RetryStrategy, TimeoutConfig,
};
use crate::tai::*;
use crate::Result as GrpcResult;

/// Configuration for gRPC client
#[derive(Debug, Clone)]
pub struct GrpcClientConfig {
    /// Server addresses to connect to
    pub server_addresses: Vec<String>,
    /// Retry configuration
    pub retry_config: RetryConfig,
    /// Circuit breaker configuration
    pub circuit_breaker_config: CircuitBreakerConfig,
    /// Timeout configuration
    pub timeout_config: TimeoutConfig,
    /// Load balancing strategy
    pub load_balancing_strategy: LoadBalancingStrategy,
    /// Maximum number of connection retries
    pub max_connection_retries: u32,
}

impl Default for GrpcClientConfig {
    fn default() -> Self {
        Self {
            server_addresses: vec!["127.0.0.1:50051".to_string()],
            retry_config: RetryConfig::default(),
            circuit_breaker_config: CircuitBreakerConfig::default(),
            timeout_config: TimeoutConfig::default(),
            load_balancing_strategy: LoadBalancingStrategy::RoundRobin,
            max_connection_retries: 3,
        }
    }
}

/// Client state for a single service
#[derive(Debug, Clone)]
struct ServiceClientState {
    /// Channel for communication
    channel: Option<Channel>,
    /// Circuit breaker for fault tolerance
    circuit_breaker: CircuitBreaker,
    /// Service address
    address: String,
}

/// Governor service client
pub struct GovernorClient {
    state: Arc<parking_lot::RwLock<ServiceClientState>>,
    config: GrpcClientConfig,
}

impl GovernorClient {
    /// Create a new Governor client
    pub async fn new(config: GrpcClientConfig) -> GrpcResult<Self> {
        let address = config.server_addresses[0].clone();
        let channel = Self::create_channel(&address).await?;

        let state = ServiceClientState {
            channel: Some(channel),
            circuit_breaker: CircuitBreaker::new(config.circuit_breaker_config.clone()),
            address,
        };

        Ok(Self {
            state: Arc::new(parking_lot::RwLock::new(state)),
            config,
        })
    }

    /// Create a channel to a server
    async fn create_channel(address: &str) -> GrpcResult<Channel> {
        let uri = format!("http://{}", address);
        Channel::from_shared(uri)?
            .connect()
            .await
            .map_err(|e| crate::GrpcError::ConnectionError(e.to_string()))
    }

    /// Get or create a channel
    async fn get_channel(&self) -> GrpcResult<Channel> {
        let mut state = self.state.write();

        // Check if circuit is open
        state.circuit_breaker.check_recovery().await;
        state.circuit_breaker.is_call_allowed()?;

        // If channel exists and is valid, use it
        if let Some(channel) = &state.channel {
            return Ok(channel.clone());
        }

        // Reconnect
        let channel = Self::create_channel(&state.address).await?;
        state.channel = Some(channel.clone());
        Ok(channel)
    }

    /// Execute a request with retry and circuit breaker logic
    async fn execute_request<F, T>(&self, mut f: F) -> GrpcResult<T>
    where
        F: FnMut(Channel) -> futures::future::BoxFuture<'static, GrpcResult<T>>,
    {
        let retry_strategy = RetryStrategy::new(self.config.retry_config.clone());

        retry_strategy
            .execute(|| {
                Box::pin(async {
                    let channel = self.get_channel().await?;

                    match f(channel).await {
                        Ok(result) => {
                            self.state.read().circuit_breaker.record_success();
                            Ok(result)
                        }
                        Err(err) => {
                            self.state.read().circuit_breaker.record_failure();
                            Err(err)
                        }
                    }
                })
            })
            .await
    }

    /// Propose a policy
    pub async fn propose_policy(&self, policy: Policy) -> GrpcResult<Receipt> {
        debug!("Proposing policy: {}", policy.policy_name);

        self.execute_request(|channel| {
            Box::pin(async move {
                let mut client = governor::client::GovernorClient::new(channel);
                let request = tonic::Request::new(policy.clone());

                client
                    .propose_policy(request)
                    .await
                    .map(|response| response.into_inner())
                    .map_err(|e| crate::GrpcError::from(e))
            })
        })
        .await
    }

    /// Enforce a policy
    pub async fn enforce_policy(&self, policy: Policy) -> GrpcResult<Receipt> {
        debug!("Enforcing policy: {}", policy.policy_name);

        self.execute_request(|channel| {
            Box::pin(async move {
                let mut client = governor::client::GovernorClient::new(channel);
                let request = tonic::Request::new(policy.clone());

                client
                    .enforce_policy(request)
                    .await
                    .map(|response| response.into_inner())
                    .map_err(|e| crate::GrpcError::from(e))
            })
        })
        .await
    }

    /// Get policies
    pub async fn get_policies(&self, filter: String) -> GrpcResult<Vec<Policy>> {
        debug!("Getting policies with filter: {}", filter);

        self.execute_request(|channel| {
            Box::pin(async move {
                let mut client = governor::client::GovernorClient::new(channel);
                let request = tonic::Request::new(GetPoliciesRequest {
                    policy_type: filter.clone(),
                    enabled_only: false,
                });

                client
                    .get_policies(request)
                    .await
                    .map(|response| response.into_inner().policies)
                    .map_err(|e| crate::GrpcError::from(e))
            })
        })
        .await
    }
}

/// Coordinator service client
pub struct CoordinatorClient {
    state: Arc<parking_lot::RwLock<ServiceClientState>>,
    config: GrpcClientConfig,
}

impl CoordinatorClient {
    /// Create a new Coordinator client
    pub async fn new(config: GrpcClientConfig) -> GrpcResult<Self> {
        let address = config.server_addresses[0].clone();
        let channel = GovernorClient::create_channel(&address).await?;

        let state = ServiceClientState {
            channel: Some(channel),
            circuit_breaker: CircuitBreaker::new(config.circuit_breaker_config.clone()),
            address,
        };

        Ok(Self {
            state: Arc::new(parking_lot::RwLock::new(state)),
            config,
        })
    }

    /// Get or create a channel
    async fn get_channel(&self) -> GrpcResult<Channel> {
        let mut state = self.state.write();

        state.circuit_breaker.check_recovery().await;
        state.circuit_breaker.is_call_allowed()?;

        if let Some(channel) = &state.channel {
            return Ok(channel.clone());
        }

        let channel = GovernorClient::create_channel(&state.address).await?;
        state.channel = Some(channel.clone());
        Ok(channel)
    }

    /// Execute a request with retry and circuit breaker logic
    async fn execute_request<F, T>(&self, mut f: F) -> GrpcResult<T>
    where
        F: FnMut(Channel) -> futures::future::BoxFuture<'static, GrpcResult<T>>,
    {
        let retry_strategy = RetryStrategy::new(self.config.retry_config.clone());

        retry_strategy
            .execute(|| {
                Box::pin(async {
                    let channel = self.get_channel().await?;

                    match f(channel).await {
                        Ok(result) => {
                            self.state.read().circuit_breaker.record_success();
                            Ok(result)
                        }
                        Err(err) => {
                            self.state.read().circuit_breaker.record_failure();
                            Err(err)
                        }
                    }
                })
            })
            .await
    }

    /// Submit a signal
    pub async fn submit_signal(&self, signal: Signal) -> GrpcResult<Receipt> {
        debug!("Submitting signal: {}", signal.signal_type);

        self.execute_request(|channel| {
            Box::pin(async move {
                let mut client = coordinator::client::CoordinatorClient::new(channel);
                let request = tonic::Request::new(signal.clone());

                client
                    .submit_signal(request)
                    .await
                    .map(|response| response.into_inner())
                    .map_err(|e| crate::GrpcError::from(e))
            })
        })
        .await
    }

    /// Request an action
    pub async fn request_action(&self, action: Action) -> GrpcResult<Receipt> {
        debug!("Requesting action: {}", action.action_type);

        self.execute_request(|channel| {
            Box::pin(async move {
                let mut client = coordinator::client::CoordinatorClient::new(channel);
                let request = tonic::Request::new(action.clone());

                client
                    .request_action(request)
                    .await
                    .map(|response| response.into_inner())
                    .map_err(|e| crate::GrpcError::from(e))
            })
        })
        .await
    }

    /// Get status
    pub async fn get_status(&self, service_name: String) -> GrpcResult<CoordinationStatus> {
        debug!("Getting status for: {}", service_name);

        self.execute_request(|channel| {
            Box::pin(async move {
                let mut client = coordinator::client::CoordinatorClient::new(channel);
                let request = tonic::Request::new(GetStatusRequest {
                    service_name: service_name.clone(),
                });

                client
                    .get_status(request)
                    .await
                    .map(|response| response.into_inner())
                    .map_err(|e| crate::GrpcError::from(e))
            })
        })
        .await
    }
}

/// Scheduler service client
pub struct SchedulerClient {
    state: Arc<parking_lot::RwLock<ServiceClientState>>,
    config: GrpcClientConfig,
}

impl SchedulerClient {
    /// Create a new Scheduler client
    pub async fn new(config: GrpcClientConfig) -> GrpcResult<Self> {
        let address = config.server_addresses[0].clone();
        let channel = GovernorClient::create_channel(&address).await?;

        let state = ServiceClientState {
            channel: Some(channel),
            circuit_breaker: CircuitBreaker::new(config.circuit_breaker_config.clone()),
            address,
        };

        Ok(Self {
            state: Arc::new(parking_lot::RwLock::new(state)),
            config,
        })
    }

    /// Get or create a channel
    async fn get_channel(&self) -> GrpcResult<Channel> {
        let mut state = self.state.write();

        state.circuit_breaker.check_recovery().await;
        state.circuit_breaker.is_call_allowed()?;

        if let Some(channel) = &state.channel {
            return Ok(channel.clone());
        }

        let channel = GovernorClient::create_channel(&state.address).await?;
        state.channel = Some(channel.clone());
        Ok(channel)
    }

    /// Execute a request with retry and circuit breaker logic
    async fn execute_request<F, T>(&self, mut f: F) -> GrpcResult<T>
    where
        F: FnMut(Channel) -> futures::future::BoxFuture<'static, GrpcResult<T>>,
    {
        let retry_strategy = RetryStrategy::new(self.config.retry_config.clone());

        retry_strategy
            .execute(|| {
                Box::pin(async {
                    let channel = self.get_channel().await?;

                    match f(channel).await {
                        Ok(result) => {
                            self.state.read().circuit_breaker.record_success();
                            Ok(result)
                        }
                        Err(err) => {
                            self.state.read().circuit_breaker.record_failure();
                            Err(err)
                        }
                    }
                })
            })
            .await
    }

    /// Submit a task
    pub async fn submit_task(&self, request: ScheduleTaskRequest) -> GrpcResult<Receipt> {
        debug!("Submitting task: {}", request.task_id);

        self.execute_request(|channel| {
            Box::pin(async move {
                let mut client = scheduler::client::SchedulerClient::new(channel);
                let request_obj = tonic::Request::new(request.clone());

                client
                    .submit_task(request_obj)
                    .await
                    .map(|response| response.into_inner())
                    .map_err(|e| crate::GrpcError::from(e))
            })
        })
        .await
    }

    /// Get task status
    pub async fn get_task_status(&self, task_id: String) -> GrpcResult<TaskStatus> {
        debug!("Getting task status: {}", task_id);

        self.execute_request(|channel| {
            Box::pin(async move {
                let mut client = scheduler::client::SchedulerClient::new(channel);
                let request = tonic::Request::new(GetTaskStatusRequest {
                    task_id: task_id.clone(),
                });

                client
                    .get_task_status(request)
                    .await
                    .map(|response| response.into_inner())
                    .map_err(|e| crate::GrpcError::from(e))
            })
        })
        .await
    }
}

/// Client pool for managing multiple client instances
pub struct GrpcClient {
    governor: GovernorClient,
    coordinator: CoordinatorClient,
    scheduler: SchedulerClient,
}

impl GrpcClient {
    /// Create a new client pool
    pub async fn new(config: GrpcClientConfig) -> GrpcResult<Self> {
        info!("Creating gRPC client pool");

        let governor = GovernorClient::new(config.clone()).await?;
        let coordinator = CoordinatorClient::new(config.clone()).await?;
        let scheduler = SchedulerClient::new(config).await?;

        Ok(Self {
            governor,
            coordinator,
            scheduler,
        })
    }

    /// Get the governor client
    pub fn governor(&self) -> &GovernorClient {
        &self.governor
    }

    /// Get the coordinator client
    pub fn coordinator(&self) -> &CoordinatorClient {
        &self.coordinator
    }

    /// Get the scheduler client
    pub fn scheduler(&self) -> &SchedulerClient {
        &self.scheduler
    }
}
