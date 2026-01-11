// REST API handler with error handling and middleware
// Demonstrates common patterns for web API implementations

use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// API response wrapper with consistent structure
#[derive(Debug, Serialize)]
#[serde(tag = "status", rename_all = "lowercase")]
pub enum ApiResponse<T> {
    Success {
        data: T,
        #[serde(skip_serializing_if = "Option::is_none")]
        message: Option<String>,
    },
    Error {
        error: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        details: Option<serde_json::Value>,
    },
}

impl<T> ApiResponse<T> {
    pub fn success(data: T) -> Self {
        Self::Success {
            data,
            message: None,
        }
    }

    pub fn success_with_message(data: T, message: String) -> Self {
        Self::Success {
            data,
            message: Some(message),
        }
    }

    pub fn error(error: String) -> Self {
        Self::Error {
            error,
            details: None,
        }
    }

    pub fn error_with_details(error: String, details: serde_json::Value) -> Self {
        Self::Error {
            error,
            details: Some(details),
        }
    }
}

/// API error types with HTTP status codes
#[derive(Debug)]
pub enum ApiError {
    BadRequest(String),
    Unauthorized(String),
    Forbidden(String),
    NotFound(String),
    Conflict(String),
    InternalServerError(String),
    ServiceUnavailable(String),
}

impl ApiError {
    pub fn status_code(&self) -> u16 {
        match self {
            Self::BadRequest(_) => 400,
            Self::Unauthorized(_) => 401,
            Self::Forbidden(_) => 403,
            Self::NotFound(_) => 404,
            Self::Conflict(_) => 409,
            Self::InternalServerError(_) => 500,
            Self::ServiceUnavailable(_) => 503,
        }
    }

    pub fn message(&self) -> &str {
        match self {
            Self::BadRequest(msg)
            | Self::Unauthorized(msg)
            | Self::Forbidden(msg)
            | Self::NotFound(msg)
            | Self::Conflict(msg)
            | Self::InternalServerError(msg)
            | Self::ServiceUnavailable(msg) => msg,
        }
    }
}

impl std::fmt::Display for ApiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl std::error::Error for ApiError {}

/// Pagination parameters for list endpoints
#[derive(Debug, Deserialize)]
pub struct PaginationParams {
    #[serde(default = "default_page")]
    pub page: u32,

    #[serde(default = "default_page_size")]
    pub page_size: u32,

    #[serde(default)]
    pub sort_by: Option<String>,

    #[serde(default)]
    pub sort_order: Option<SortOrder>,
}

fn default_page() -> u32 { 1 }
fn default_page_size() -> u32 { 20 }

impl PaginationParams {
    pub fn offset(&self) -> u32 {
        (self.page - 1) * self.page_size
    }

    pub fn validate(&self) -> Result<(), ApiError> {
        if self.page == 0 {
            return Err(ApiError::BadRequest("Page must be at least 1".to_string()));
        }
        if self.page_size == 0 || self.page_size > 100 {
            return Err(ApiError::BadRequest("Page size must be between 1 and 100".to_string()));
        }
        Ok(())
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SortOrder {
    Asc,
    Desc,
}

/// Paginated response wrapper
#[derive(Debug, Serialize)]
pub struct PaginatedResponse<T> {
    pub items: Vec<T>,
    pub total: u32,
    pub page: u32,
    pub page_size: u32,
    pub total_pages: u32,
}

impl<T> PaginatedResponse<T> {
    pub fn new(items: Vec<T>, total: u32, page: u32, page_size: u32) -> Self {
        let total_pages = (total + page_size - 1) / page_size;
        Self {
            items,
            total,
            page,
            page_size,
            total_pages,
        }
    }
}

/// Example API handler with all common patterns
pub struct ApiHandler<S> {
    state: Arc<S>,
}

impl<S> ApiHandler<S> {
    pub fn new(state: Arc<S>) -> Self {
        Self { state }
    }

    /// Handle GET request with query parameters
    pub async fn handle_get(
        &self,
        id: i64,
    ) -> Result<ApiResponse<serde_json::Value>, ApiError> {
        // Validate input
        if id <= 0 {
            return Err(ApiError::BadRequest("Invalid ID".to_string()));
        }

        // Fetch data (example)
        let data = self.fetch_by_id(id).await?;

        Ok(ApiResponse::success(data))
    }

    /// Handle POST request with body validation
    pub async fn handle_post(
        &self,
        body: serde_json::Value,
    ) -> Result<ApiResponse<serde_json::Value>, ApiError> {
        // Validate request body
        self.validate_create_request(&body)?;

        // Create resource
        let created = self.create_resource(body).await?;

        Ok(ApiResponse::success_with_message(
            created,
            "Resource created successfully".to_string(),
        ))
    }

    /// Handle PUT request with update
    pub async fn handle_put(
        &self,
        id: i64,
        body: serde_json::Value,
    ) -> Result<ApiResponse<serde_json::Value>, ApiError> {
        // Validate input
        if id <= 0 {
            return Err(ApiError::BadRequest("Invalid ID".to_string()));
        }

        // Check if resource exists
        self.fetch_by_id(id).await?;

        // Update resource
        let updated = self.update_resource(id, body).await?;

        Ok(ApiResponse::success_with_message(
            updated,
            "Resource updated successfully".to_string(),
        ))
    }

    /// Handle DELETE request
    pub async fn handle_delete(
        &self,
        id: i64,
    ) -> Result<ApiResponse<()>, ApiError> {
        // Validate input
        if id <= 0 {
            return Err(ApiError::BadRequest("Invalid ID".to_string()));
        }

        // Check if resource exists
        self.fetch_by_id(id).await?;

        // Delete resource
        self.delete_resource(id).await?;

        Ok(ApiResponse::success_with_message(
            (),
            "Resource deleted successfully".to_string(),
        ))
    }

    /// Handle LIST request with pagination
    pub async fn handle_list(
        &self,
        params: PaginationParams,
    ) -> Result<ApiResponse<PaginatedResponse<serde_json::Value>>, ApiError> {
        // Validate pagination parameters
        params.validate()?;

        // Fetch paginated data
        let (items, total) = self.fetch_paginated(&params).await?;

        let response = PaginatedResponse::new(
            items,
            total,
            params.page,
            params.page_size,
        );

        Ok(ApiResponse::success(response))
    }

    // Helper methods (would be implemented with actual data access)

    async fn fetch_by_id(&self, id: i64) -> Result<serde_json::Value, ApiError> {
        // Simulate data fetch
        Ok(serde_json::json!({ "id": id, "data": "example" }))
    }

    async fn create_resource(&self, _body: serde_json::Value) -> Result<serde_json::Value, ApiError> {
        Ok(serde_json::json!({ "id": 1, "created": true }))
    }

    async fn update_resource(&self, id: i64, _body: serde_json::Value) -> Result<serde_json::Value, ApiError> {
        Ok(serde_json::json!({ "id": id, "updated": true }))
    }

    async fn delete_resource(&self, _id: i64) -> Result<(), ApiError> {
        Ok(())
    }

    async fn fetch_paginated(
        &self,
        params: &PaginationParams,
    ) -> Result<(Vec<serde_json::Value>, u32), ApiError> {
        let items = vec![
            serde_json::json!({ "id": 1 }),
            serde_json::json!({ "id": 2 }),
        ];
        Ok((items, 2))
    }

    fn validate_create_request(&self, _body: &serde_json::Value) -> Result<(), ApiError> {
        // Validation logic
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pagination_params() {
        let params = PaginationParams {
            page: 2,
            page_size: 20,
            sort_by: None,
            sort_order: None,
        };

        assert_eq!(params.offset(), 20);
        assert!(params.validate().is_ok());
    }

    #[test]
    fn test_pagination_validation() {
        let params = PaginationParams {
            page: 0,
            page_size: 20,
            sort_by: None,
            sort_order: None,
        };

        assert!(params.validate().is_err());
    }

    #[test]
    fn test_api_response() {
        let response = ApiResponse::success("test");
        match response {
            ApiResponse::Success { data, .. } => assert_eq!(data, "test"),
            _ => panic!("Expected success response"),
        }
    }
}
