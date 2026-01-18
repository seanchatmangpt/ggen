// Common middleware pattern for request processing
// Demonstrates composable middleware architecture

use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::time::Instant;

/// Request context passed through middleware chain
#[derive(Debug, Clone)]
pub struct RequestContext {
    pub request_id: String,
    pub user_id: Option<String>,
    pub start_time: Instant,
    pub metadata: std::collections::HashMap<String, String>,
}

impl RequestContext {
    pub fn new(request_id: String) -> Self {
        Self {
            request_id,
            user_id: None,
            start_time: Instant::now(),
            metadata: std::collections::HashMap::new(),
        }
    }

    pub fn with_user(mut self, user_id: String) -> Self {
        self.user_id = Some(user_id);
        self
    }

    pub fn elapsed(&self) -> std::time::Duration {
        self.start_time.elapsed()
    }
}

/// Middleware result type
pub type MiddlewareResult<T> = Result<T, MiddlewareError>;

/// Middleware error types
#[derive(Debug)]
pub enum MiddlewareError {
    AuthenticationRequired,
    AuthorizationFailed,
    RateLimitExceeded,
    ValidationFailed(String),
    InternalError(String),
}

impl std::fmt::Display for MiddlewareError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AuthenticationRequired => write!(f, "Authentication required"),
            Self::AuthorizationFailed => write!(f, "Authorization failed"),
            Self::RateLimitExceeded => write!(f, "Rate limit exceeded"),
            Self::ValidationFailed(msg) => write!(f, "Validation failed: {}", msg),
            Self::InternalError(msg) => write!(f, "Internal error: {}", msg),
        }
    }
}

impl std::error::Error for MiddlewareError {}

/// Middleware trait for composable request processing
pub trait Middleware: Send + Sync {
    fn handle<'a>(
        &'a self,
        ctx: RequestContext,
        next: Box<dyn FnOnce(RequestContext) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>> + Send + 'a>,
    ) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>>;
}

/// Logging middleware - logs request/response
pub struct LoggingMiddleware {
    logger: Arc<dyn Logger>,
}

impl LoggingMiddleware {
    pub fn new(logger: Arc<dyn Logger>) -> Self {
        Self { logger }
    }
}

impl Middleware for LoggingMiddleware {
    fn handle<'a>(
        &'a self,
        ctx: RequestContext,
        next: Box<dyn FnOnce(RequestContext) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>> + Send + 'a>,
    ) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>> {
        let logger = self.logger.clone();

        Box::pin(async move {
            logger.log(&format!("Request started: {}", ctx.request_id));

            let result = next(ctx.clone()).await;

            logger.log(&format!(
                "Request completed: {} ({:?})",
                ctx.request_id,
                ctx.elapsed()
            ));

            result
        })
    }
}

/// Authentication middleware - verifies user identity
pub struct AuthenticationMiddleware {
    token_validator: Arc<dyn TokenValidator>,
}

impl AuthenticationMiddleware {
    pub fn new(token_validator: Arc<dyn TokenValidator>) -> Self {
        Self { token_validator }
    }
}

impl Middleware for AuthenticationMiddleware {
    fn handle<'a>(
        &'a self,
        mut ctx: RequestContext,
        next: Box<dyn FnOnce(RequestContext) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>> + Send + 'a>,
    ) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>> {
        let validator = self.token_validator.clone();

        Box::pin(async move {
            // Extract and validate token
            let token = ctx.metadata.get("authorization")
                .ok_or(MiddlewareError::AuthenticationRequired)?;

            let user_id = validator.validate(token)
                .await
                .map_err(|_| MiddlewareError::AuthenticationRequired)?;

            ctx.user_id = Some(user_id);

            next(ctx).await
        })
    }
}

/// Rate limiting middleware - prevents abuse
pub struct RateLimitMiddleware {
    limiter: Arc<dyn RateLimiter>,
}

impl RateLimitMiddleware {
    pub fn new(limiter: Arc<dyn RateLimiter>) -> Self {
        Self { limiter }
    }
}

impl Middleware for RateLimitMiddleware {
    fn handle<'a>(
        &'a self,
        ctx: RequestContext,
        next: Box<dyn FnOnce(RequestContext) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>> + Send + 'a>,
    ) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>> {
        let limiter = self.limiter.clone();

        Box::pin(async move {
            let user_id = ctx.user_id.as_deref().unwrap_or("anonymous");

            if !limiter.check_limit(user_id).await {
                return Err(MiddlewareError::RateLimitExceeded);
            }

            next(ctx).await
        })
    }
}

/// Request validation middleware - validates input
pub struct ValidationMiddleware {
    validator: Arc<dyn RequestValidator>,
}

impl ValidationMiddleware {
    pub fn new(validator: Arc<dyn RequestValidator>) -> Self {
        Self { validator }
    }
}

impl Middleware for ValidationMiddleware {
    fn handle<'a>(
        &'a self,
        ctx: RequestContext,
        next: Box<dyn FnOnce(RequestContext) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>> + Send + 'a>,
    ) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>> {
        let validator = self.validator.clone();

        Box::pin(async move {
            validator.validate(&ctx)
                .await
                .map_err(|e| MiddlewareError::ValidationFailed(e))?;

            next(ctx).await
        })
    }
}

/// Middleware chain builder for composing multiple middleware
pub struct MiddlewareChain {
    middleware: Vec<Arc<dyn Middleware>>,
}

impl MiddlewareChain {
    pub fn new() -> Self {
        Self {
            middleware: Vec::new(),
        }
    }

    pub fn add<M: Middleware + 'static>(mut self, middleware: M) -> Self {
        self.middleware.push(Arc::new(middleware));
        self
    }

    pub async fn execute(&self, ctx: RequestContext) -> MiddlewareResult<()> {
        self.execute_recursive(ctx, 0).await
    }

    fn execute_recursive<'a>(
        &'a self,
        ctx: RequestContext,
        index: usize,
    ) -> Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send + 'a>> {
        Box::pin(async move {
            if index >= self.middleware.len() {
                // End of chain
                return Ok(());
            }

            let middleware = self.middleware[index].clone();
            let next = Box::new(move |ctx: RequestContext| {
                self.execute_recursive(ctx, index + 1)
            });

            middleware.handle(ctx, next).await
        })
    }
}

impl Default for MiddlewareChain {
    fn default() -> Self {
        Self::new()
    }
}

// Traits for dependency injection

pub trait Logger: Send + Sync {
    fn log(&self, message: &str);
}

pub trait TokenValidator: Send + Sync {
    fn validate(&self, token: &str) -> Pin<Box<dyn Future<Output = Result<String, String>> + Send + '_>>;
}

pub trait RateLimiter: Send + Sync {
    fn check_limit(&self, user_id: &str) -> Pin<Box<dyn Future<Output = bool> + Send + '_>>;
}

pub trait RequestValidator: Send + Sync {
    fn validate(&self, ctx: &RequestContext) -> Pin<Box<dyn Future<Output = Result<(), String>> + Send + '_>>;
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockLogger;
    impl Logger for MockLogger {
        fn log(&self, _message: &str) {}
    }

    struct MockValidator;
    impl TokenValidator for MockValidator {
        fn validate(&self, _token: &str) -> Pin<Box<dyn Future<Output = Result<String, String>> + Send + '_>> {
            Box::pin(async { Ok("user123".to_string()) })
        }
    }

    #[tokio::test]
    async fn test_logging_middleware() {
        let logger = Arc::new(MockLogger);
        let middleware = LoggingMiddleware::new(logger);
        let ctx = RequestContext::new("test-123".to_string());

        let next = Box::new(|_ctx: RequestContext| {
            Box::pin(async { Ok(()) }) as Pin<Box<dyn Future<Output = MiddlewareResult<()>> + Send>>
        });

        let result = middleware.handle(ctx, next).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_middleware_chain() {
        let logger = Arc::new(MockLogger);
        let chain = MiddlewareChain::new()
            .add(LoggingMiddleware::new(logger));

        let ctx = RequestContext::new("test-456".to_string());
        let result = chain.execute(ctx).await;
        assert!(result.is_ok());
    }
}
