// Multi-Tenant SaaS Middleware - Rust Implementation with Axum
// Handles tenant identification, isolation, and request routing

use axum::{
    extract::{Extension, Request},
    http::{HeaderMap, StatusCode},
    middleware::Next,
    response::Response,
};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Clone, Debug)]
pub struct TenantContext {
    pub tenant_id: String,
    pub tenant_slug: String,
    pub isolation_strategy: IsolationStrategy,
    pub subscription_tier: SubscriptionTier,
    pub feature_flags: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum IsolationStrategy {
    Schema,
    Database,
    RowLevel,
    Hybrid,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SubscriptionTier {
    Free,
    Basic,
    Pro,
    Enterprise,
}

pub struct TenantResolver {
    tenant_cache: Arc<RwLock<std::collections::HashMap<String, TenantContext>>>,
}

impl TenantResolver {
    pub fn new() -> Self {
        Self {
            tenant_cache: Arc::new(RwLock::new(std::collections::HashMap::new())),
        }
    }

    /// Resolve tenant from subdomain or custom domain
    pub async fn resolve_from_host(&self, host: &str) -> Option<TenantContext> {
        // Extract subdomain (e.g., "acme" from "acme.saas.com")
        let parts: Vec<&str> = host.split('.').collect();
        if parts.len() >= 2 {
            let tenant_slug = parts[0].to_string();

            // Check cache first
            let cache = self.tenant_cache.read().await;
            if let Some(tenant) = cache.get(&tenant_slug) {
                return Some(tenant.clone());
            }
            drop(cache);

            // Load from database (mocked for example)
            let tenant_ctx = self.load_tenant_from_db(&tenant_slug).await?;

            // Update cache
            let mut cache = self.tenant_cache.write().await;
            cache.insert(tenant_slug.clone(), tenant_ctx.clone());

            Some(tenant_ctx)
        } else {
            None
        }
    }

    /// Resolve tenant from header (X-Tenant-ID)
    pub async fn resolve_from_header(&self, headers: &HeaderMap) -> Option<TenantContext> {
        if let Some(tenant_id) = headers.get("X-Tenant-ID") {
            if let Ok(id) = tenant_id.to_str() {
                return self.load_tenant_from_db(id).await;
            }
        }
        None
    }

    /// Mock database lookup (replace with actual DB query using SPARQL)
    async fn load_tenant_from_db(&self, tenant_slug: &str) -> Option<TenantContext> {
        // TODO: Execute SPARQL query to fetch tenant details
        // For now, return mock data
        Some(TenantContext {
            tenant_id: format!("tenant_{}", tenant_slug),
            tenant_slug: tenant_slug.to_string(),
            isolation_strategy: IsolationStrategy::Schema,
            subscription_tier: SubscriptionTier::Pro,
            feature_flags: vec!["api_v2".to_string(), "analytics".to_string()],
        })
    }
}

/// Axum middleware for tenant identification
pub async fn tenant_middleware(
    headers: HeaderMap,
    Extension(resolver): Extension<Arc<TenantResolver>>,
    mut request: Request,
    next: Next,
) -> Result<Response, StatusCode> {
    // Try to resolve tenant from Host header
    let tenant_ctx = if let Some(host) = headers.get("Host") {
        if let Ok(host_str) = host.to_str() {
            resolver.resolve_from_host(host_str).await
        } else {
            None
        }
    } else {
        None
    };

    // Fallback to X-Tenant-ID header
    let tenant_ctx = tenant_ctx.or_else(|| {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current()
                .block_on(resolver.resolve_from_header(&headers))
        })
    });

    // Reject request if tenant not found
    let tenant_ctx = tenant_ctx.ok_or(StatusCode::BAD_REQUEST)?;

    // Add tenant context to request extensions
    request.extensions_mut().insert(tenant_ctx);

    Ok(next.run(request).await)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_tenant_resolution_from_subdomain() {
        let resolver = TenantResolver::new();
        let tenant = resolver.resolve_from_host("acme.saas.com").await;

        assert!(tenant.is_some());
        let tenant = tenant.unwrap();
        assert_eq!(tenant.tenant_slug, "acme");
        assert_eq!(tenant.isolation_strategy, IsolationStrategy::Schema);
    }

    #[tokio::test]
    async fn test_tenant_cache() {
        let resolver = TenantResolver::new();

        // First lookup (cache miss)
        let tenant1 = resolver.resolve_from_host("test.saas.com").await.unwrap();

        // Second lookup (cache hit)
        let tenant2 = resolver.resolve_from_host("test.saas.com").await.unwrap();

        assert_eq!(tenant1.tenant_id, tenant2.tenant_id);
    }
}
