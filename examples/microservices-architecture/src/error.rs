use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum MicroserviceError {
    #[error("Service not found: {0}")]
    ServiceNotFound(String),

    #[error("Registration failed: {0}")]
    RegistrationFailed(String),

    #[error("Health check failed: {0}")]
    HealthCheckFailed(String),

    #[error("Load balancing failed: {0}")]
    LoadBalancingFailed(String),

    #[error("Recovery failed: {0}")]
    RecoveryFailed(String),

    #[error("Tool execution failed: {0}")]
    ToolExecutionFailed(String),
}

pub type Result<T> = std::result::Result<T, MicroserviceError>;
