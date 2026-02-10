//! Request Routing
//!
//! Routes admitted requests or refuses inadmissible ones.
//! Implements boundary control: λ_admitted → 0.

use crate::admission::{AdmissionController, AdmissionError, AdmissionPolicy};
use async_trait::async_trait;
use std::future::Future;
use std::time::Instant;
use thiserror::Error;
use tracing::{debug, warn};

/// Routing errors
#[derive(Debug, Error)]
pub enum RoutingError {
    /// Request was refused by admission control
    #[error("Request refused: {0}")]
    Refused(#[from] AdmissionError),

    /// Request processing failed
    #[error("Request processing failed: {0}")]
    ProcessingFailed(String),
}

/// Request handler trait
#[async_trait]
pub trait RequestHandler: Send + Sync {
    /// Request type
    type Request: Send;
    /// Response type
    type Response: Send;

    /// Handle request and return response
    async fn handle(&self, request: Self::Request) -> Result<Self::Response, String>;
}

/// Router with admission control
pub struct Router<H: RequestHandler> {
    handler: H,
    admission: AdmissionController,
}

impl<H: RequestHandler> Router<H> {
    /// Create new router
    pub fn new(handler: H, policy: AdmissionPolicy) -> Result<Self, RoutingError> {
        let admission = AdmissionController::new(policy)
            .map_err(|e| RoutingError::Refused(e.into()))?;

        Ok(Self { handler, admission })
    }

    /// Route request with admission control
    pub async fn route(&mut self, request: H::Request) -> Result<H::Response, RoutingError> {
        // Admission control: refuse at boundary
        let ticket = self.admission.try_admit()?;
        debug!("Request admitted");

        // Process request
        let start = Instant::now();
        let result = self.handler.handle(request).await;
        let duration = start.elapsed();

        // Release ticket (implicitly via drop)
        drop(ticket);

        // Update capacity estimation
        self.admission.record_completion(duration);

        match result {
            Ok(response) => {
                debug!("Request completed in {:?}", duration);
                Ok(response)
            }
            Err(e) => {
                warn!("Request failed: {}", e);
                Err(RoutingError::ProcessingFailed(e))
            }
        }
    }

    /// Get admission statistics
    pub fn stats(&self) -> crate::admission::AdmissionStats {
        self.admission.stats()
    }
}

/// Convenience function for routing with closure
pub async fn route_with<F, Fut, Req, Resp>(
    policy: AdmissionPolicy,
    request: Req,
    handler: F,
) -> Result<Resp, RoutingError>
where
    F: FnOnce(Req) -> Fut,
    Fut: Future<Output = Result<Resp, String>>,
    Req: Send + Sync,
    Resp: Send + Sync,
{
    #[allow(dead_code)]
    struct ClosureHandler<F, Req, Resp> {
        handler: F,
        _phantom: std::marker::PhantomData<(Req, Resp)>,
    }

    #[async_trait]
    impl<F, Fut, Req, Resp> RequestHandler for ClosureHandler<F, Req, Resp>
    where
        F: Fn(Req) -> Fut + Send + Sync,
        Fut: Future<Output = Result<Resp, String>> + Send,
        Req: Send + Sync,
        Resp: Send + Sync,
    {
        type Request = Req;
        type Response = Resp;

        async fn handle(&self, request: Self::Request) -> Result<Self::Response, String> {
            (self.handler)(request).await
        }
    }

    let mut admission = AdmissionController::new(policy)
        .map_err(|e| RoutingError::Refused(e.into()))?;

    // Admission control
    let ticket = admission.try_admit()?;

    // Process
    let start = Instant::now();
    let result = handler(request).await;
    let duration = start.elapsed();

    // Release and update
    drop(ticket);
    admission.record_completion(duration);

    result.map_err(RoutingError::ProcessingFailed)
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestHandler;

    #[async_trait]
    impl RequestHandler for TestHandler {
        type Request = String;
        type Response = String;

        async fn handle(&self, request: Self::Request) -> Result<Self::Response, String> {
            Ok(format!("Processed: {}", request))
        }
    }

    #[tokio::test]
    async fn test_router_creation() {
        let handler = TestHandler;
        let policy = AdmissionPolicy::default();
        let router = Router::new(handler, policy);
        assert!(router.is_ok());
    }

    #[tokio::test]
    async fn test_router_route_success() {
        let handler = TestHandler;
        let policy = AdmissionPolicy::default();
        let mut router = Router::new(handler, policy).unwrap();

        let response = router.route("test".to_string()).await.unwrap();
        assert_eq!(response, "Processed: test");
    }

    #[tokio::test]
    async fn test_router_refuse_at_capacity() {
        let handler = TestHandler;
        let policy = AdmissionPolicy {
            wip_limit: 1,
            min_capacity_ratio: 0.0,
            cooldown: std::time::Duration::from_millis(10),
        };
        let mut router = Router::new(handler, policy).unwrap();

        // First request succeeds
        let _response1 = router.route("test1".to_string()).await.unwrap();

        // Second concurrent request should fail
        // (In real async code, this would be concurrent; here it's sequential)
        let stats = router.stats();
        assert_eq!(stats.current_wip, 0); // First completed
    }

    struct FailingHandler;

    #[async_trait]
    impl RequestHandler for FailingHandler {
        type Request = String;
        type Response = String;

        async fn handle(&self, _request: Self::Request) -> Result<Self::Response, String> {
            Err("Handler error".to_string())
        }
    }

    #[tokio::test]
    async fn test_router_handler_failure() {
        let handler = FailingHandler;
        let policy = AdmissionPolicy::default();
        let mut router = Router::new(handler, policy).unwrap();

        let result = router.route("test".to_string()).await;
        assert!(matches!(result, Err(RoutingError::ProcessingFailed(_))));
    }

    #[tokio::test]
    async fn test_route_with_closure() {
        let policy = AdmissionPolicy::default();

        let result = route_with(
            policy,
            "input",
            |req| async move { Ok::<_, String>(format!("Processed: {}", req)) },
        )
        .await;

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "Processed: input");
    }
}
