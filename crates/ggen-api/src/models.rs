//! API request and response models

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use validator::Validate;

/// User registration request
#[derive(Debug, Clone, Serialize, Deserialize, Validate)]
pub struct RegisterRequest {
    #[validate(email)]
    pub email: String,
    #[validate(length(min = 6))]
    pub password: String,
    #[validate(length(min = 1))]
    pub username: String,
}

/// Login request
#[derive(Debug, Clone, Serialize, Deserialize, Validate)]
pub struct LoginRequest {
    pub email: String,
    #[validate(length(min = 6))]
    pub password: String,
}

/// Login response with JWT token
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoginResponse {
    pub token: String,
    pub user_id: String,
    pub username: String,
    pub email: String,
    pub tier: String,
    pub expires_in_secs: u64,
}

/// Marketplace package search request
#[derive(Debug, Clone, Serialize, Deserialize, Validate)]
pub struct SearchRequest {
    pub query: String,
    #[validate(range(min = 1, max = 100))]
    pub limit: Option<u32>,
    pub offset: Option<u32>,
    pub category: Option<String>,
    pub sort_by: Option<String>, // "relevance", "downloads", "recent", "rating"
}

/// Package search result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageResult {
    pub id: String,
    pub name: String,
    pub version: String,
    pub author: String,
    pub description: String,
    pub downloads: u64,
    pub price: Option<f64>,
    pub license: String,
    pub rating: f32,
}

/// Package purchase request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PurchaseRequest {
    pub package_id: String,
    pub version: String,
}

/// Purchase response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PurchaseResponse {
    pub transaction_id: String,
    pub package_id: String,
    pub price: f64,
    pub status: String, // "pending", "completed", "failed"
    pub payment_url: Option<String>,
}

/// API key creation request
#[derive(Debug, Clone, Serialize, Deserialize, Validate)]
pub struct CreateApiKeyRequest {
    #[validate(length(min = 1))]
    pub name: String,
    pub expires_in_days: Option<u32>,
}

/// API key response (only returned on creation)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiKeyResponse {
    pub id: String,
    pub key: String, // Hashed in database, only shown once
    pub name: String,
    pub created_at: DateTime<Utc>,
    pub expires_at: Option<DateTime<Utc>>,
}

/// Usage tracking request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UsageRequest {
    pub operation: String, // "api_call", "marketplace_search", "installation", etc.
    pub cost_credits: u32,
}

/// Usage statistics response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UsageStatsResponse {
    pub user_id: String,
    pub tier: String,
    pub period_start: DateTime<Utc>,
    pub period_end: DateTime<Utc>,
    pub api_calls_used: u64,
    pub api_calls_limit: u64,
    pub templates_used: u64,
    pub templates_limit: u64,
    pub total_credits_used: u64,
    pub total_credits_available: u64,
}

/// Invoice response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvoiceResponse {
    pub invoice_id: String,
    pub user_id: String,
    pub amount: f64,
    pub currency: String,
    pub status: String, // "paid", "pending", "failed"
    pub issued_at: DateTime<Utc>,
    pub due_at: DateTime<Utc>,
    pub items: Vec<InvoiceItem>,
}

/// Invoice line item
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvoiceItem {
    pub description: String,
    pub quantity: u32,
    pub unit_price: f64,
    pub total: f64,
}

/// Subscription upgrade/downgrade request
#[derive(Debug, Clone, Serialize, Deserialize, Validate)]
pub struct UpgradeRequest {
    pub new_tier: String, // "free", "pro", "enterprise"
}

/// Health check response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthResponse {
    pub status: String,
    pub timestamp: DateTime<Utc>,
    pub version: String,
}

/// Generic paginated response wrapper
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PaginatedResponse<T> {
    pub data: Vec<T>,
    pub total: u64,
    pub limit: u32,
    pub offset: u32,
}

/// Error response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorResponse {
    pub error: String,
    pub status: u16,
    pub timestamp: DateTime<Utc>,
}
