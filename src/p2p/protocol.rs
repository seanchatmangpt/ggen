//! Request-Response Protocol Implementation

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::SystemTime;

use crate::p2p::types::{Package, Query, SearchResult};

/// Package protocol for request-response communication
#[derive(Debug)]
pub struct PackageProtocol {
    pending_requests: HashMap<String, PendingRequest>,
    request_handlers: HashMap<RequestType, Box<dyn RequestHandler>>,
}

/// Request-response message types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RequestType {
    GetPackage,
    SearchPackages,
    GetMetadata,
    PublishPackage,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    pub id: String,
    pub request_type: RequestType,
    pub payload: RequestPayload,
    pub timestamp: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RequestPayload {
    GetPackage { package_id: String },
    SearchPackages { query: Query },
    GetMetadata { package_id: String },
    PublishPackage { package: Package },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response {
    pub request_id: String,
    pub status: ResponseStatus,
    pub payload: ResponsePayload,
    pub timestamp: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ResponseStatus {
    Success,
    Error(String),
    NotFound,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResponsePayload {
    Package(Package),
    SearchResults(Vec<SearchResult>),
    Metadata(PackageMetadata),
    Success,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageMetadata {
    pub package_id: String,
    pub size: u64,
    pub content_hash: String,
    pub providers: Vec<String>,
}

#[derive(Debug, Clone)]
struct PendingRequest {
    request: Request,
    sent_at: SystemTime,
    peer_id: String,
}

/// Trait for handling requests
trait RequestHandler: Send + Sync {
    fn handle(&self, request: &Request) -> Result<Response>;
}

impl PackageProtocol {
    pub fn new() -> Self {
        Self {
            pending_requests: HashMap::new(),
            request_handlers: HashMap::new(),
        }
    }

    /// Send a request
    pub fn send_request(&mut self, request: Request, peer_id: String) -> Result<String> {
        let request_id = request.id.clone();

        let pending = PendingRequest {
            request,
            sent_at: SystemTime::now(),
            peer_id,
        };

        self.pending_requests.insert(request_id.clone(), pending);
        Ok(request_id)
    }

    /// Handle incoming request
    pub fn handle_request(&self, request: &Request) -> Result<Response> {
        // In a real implementation, this would route to appropriate handlers
        match &request.request_type {
            RequestType::GetPackage => {
                if let RequestPayload::GetPackage { package_id } = &request.payload {
                    // Simulate package retrieval
                    Ok(Response {
                        request_id: request.id.clone(),
                        status: ResponseStatus::NotFound,
                        payload: ResponsePayload::Success,
                        timestamp: chrono::Utc::now().timestamp(),
                    })
                } else {
                    Err(anyhow!("Invalid payload for GetPackage"))
                }
            }
            RequestType::SearchPackages => {
                if let RequestPayload::SearchPackages { query } = &request.payload {
                    // Simulate search
                    Ok(Response {
                        request_id: request.id.clone(),
                        status: ResponseStatus::Success,
                        payload: ResponsePayload::SearchResults(Vec::new()),
                        timestamp: chrono::Utc::now().timestamp(),
                    })
                } else {
                    Err(anyhow!("Invalid payload for SearchPackages"))
                }
            }
            _ => Err(anyhow!("Unsupported request type")),
        }
    }

    /// Handle incoming response
    pub fn handle_response(&mut self, response: Response) -> Result<()> {
        if let Some(_pending) = self.pending_requests.remove(&response.request_id) {
            // Process response
            Ok(())
        } else {
            Err(anyhow!("No pending request found for response"))
        }
    }

    /// Create a GetPackage request
    pub fn create_get_package_request(package_id: String) -> Request {
        Request {
            id: uuid::Uuid::new_v4().to_string(),
            request_type: RequestType::GetPackage,
            payload: RequestPayload::GetPackage { package_id },
            timestamp: chrono::Utc::now().timestamp(),
        }
    }

    /// Create a SearchPackages request
    pub fn create_search_request(query: Query) -> Request {
        Request {
            id: uuid::Uuid::new_v4().to_string(),
            request_type: RequestType::SearchPackages,
            payload: RequestPayload::SearchPackages { query },
            timestamp: chrono::Utc::now().timestamp(),
        }
    }

    /// Create a PublishPackage request
    pub fn create_publish_request(package: Package) -> Request {
        Request {
            id: uuid::Uuid::new_v4().to_string(),
            request_type: RequestType::PublishPackage,
            payload: RequestPayload::PublishPackage { package },
            timestamp: chrono::Utc::now().timestamp(),
        }
    }

    /// Get pending request count
    pub fn pending_count(&self) -> usize {
        self.pending_requests.len()
    }

    /// Cleanup old pending requests
    pub fn cleanup_pending(&mut self, max_age_secs: u64) {
        let now = SystemTime::now();
        self.pending_requests.retain(|_, pending| {
            now.duration_since(pending.sent_at)
                .unwrap_or(std::time::Duration::from_secs(0))
                .as_secs()
                < max_age_secs
        });
    }
}

impl Default for PackageProtocol {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_get_package_request() {
        let request = PackageProtocol::create_get_package_request("package-1".to_string());

        assert_eq!(request.request_type, RequestType::GetPackage);
        assert!(matches!(request.payload, RequestPayload::GetPackage { .. }));
    }

    #[test]
    fn test_create_search_request() {
        let query = Query::new(vec!["rust".to_string()]);
        let request = PackageProtocol::create_search_request(query);

        assert_eq!(request.request_type, RequestType::SearchPackages);
    }

    #[test]
    fn test_send_request() {
        let mut protocol = PackageProtocol::new();
        let request = PackageProtocol::create_get_package_request("package-1".to_string());
        let request_id = request.id.clone();

        let result = protocol.send_request(request, "peer1".to_string());
        assert!(result.is_ok());
        assert_eq!(protocol.pending_count(), 1);
    }

    #[test]
    fn test_handle_request() {
        let protocol = PackageProtocol::new();
        let request = PackageProtocol::create_get_package_request("package-1".to_string());

        let response = protocol.handle_request(&request);
        assert!(response.is_ok());
    }

    #[test]
    fn test_cleanup_pending() {
        let mut protocol = PackageProtocol::new();
        let request = PackageProtocol::create_get_package_request("package-1".to_string());

        protocol.send_request(request, "peer1".to_string()).unwrap();
        assert_eq!(protocol.pending_count(), 1);

        // Cleanup with 0 second max age (removes all)
        protocol.cleanup_pending(0);
        assert_eq!(protocol.pending_count(), 0);
    }
}
