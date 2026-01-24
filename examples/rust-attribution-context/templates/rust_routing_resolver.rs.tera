//! Route Resolution with Zero-Allocation Caching
//!
//! Generated from ontology/routing.ttl
//! DO NOT EDIT - regenerate via: ggen sync

use dashmap::DashMap;
use std::sync::Arc;
use uuid::Uuid;
use chrono::{DateTime, Utc};

/// Route resolver with lock-free cache
#[derive(Clone)]
pub struct RouteResolver {
    /// Zero-clone cache: Arc references avoid allocations on cache hits
    cache: Arc<DashMap<String, Arc<AffiliateLinkRoute>>>,
}

/// Affiliate link route entity
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AffiliateLinkRoute {
    pub id: Uuid,
    pub source_path: String,
    pub destination_url: String,
    pub affiliate_id: Uuid,
    pub tracking_params: Option<String>,
    pub created_at: DateTime<Utc>,
    pub active: bool,
}

/// Route resolution errors
#[derive(Debug, thiserror::Error)]
pub enum RouteError {
    #[error("Route not found: {slug}")]
    NotFound { slug: String },

    #[error("Route inactive: {slug}")]
    Inactive { slug: String },

    #[error("Invalid route slug: {slug}")]
    InvalidSlug { slug: String },

    #[error("Cache error: {0}")]
    CacheError(String),
}

impl RouteResolver {
    /// Create new resolver with empty cache
    pub fn new() -> Self {
        Self {
            cache: Arc::new(DashMap::new()),
        }
    }

    /// Warm cache from route list (idempotent)
    pub fn warm_cache(&self, routes: Vec<AffiliateLinkRoute>) -> Result<usize, RouteError> {
        let mut count = 0;
        for route in routes {
            if route.active {
                self.cache.insert(route.source_path.clone(), Arc::new(route));
                count += 1;
            }
        }
        Ok(count)
    }

    /// Resolve route by slug (zero allocations on cache hit)
    ///
    /// # Performance
    /// - Cache hit: O(1) with zero allocations (Arc clone is pointer copy)
    /// - Cache miss: O(1) with error allocation
    pub fn resolve(&self, slug: &str) -> Result<Arc<AffiliateLinkRoute>, RouteError> {
        // Validate slug format (prevent injection)
        if !Self::is_valid_slug(slug) {
            return Err(RouteError::InvalidSlug {
                slug: slug.to_string(),
            });
        }

        // Cache lookup (zero-alloc on hit)
        match self.cache.get(slug) {
            Some(route) => {
                if route.active {
                    // Arc::clone is just pointer increment (zero heap alloc)
                    Ok(Arc::clone(route.value()))
                } else {
                    Err(RouteError::Inactive {
                        slug: slug.to_string(),
                    })
                }
            }
            None => Err(RouteError::NotFound {
                slug: slug.to_string(),
            }),
        }
    }

    /// Validate slug format (alphanumeric + hyphens, 1-64 chars)
    fn is_valid_slug(slug: &str) -> bool {
        slug.len() >= 1
            && slug.len() <= 64
            && slug.chars().all(|c| c.is_ascii_alphanumeric() || c == '-')
    }

    /// Append tracking params to destination URL
    pub fn build_redirect_url(
        route: &AffiliateLinkRoute,
        click_id: Uuid,
    ) -> Result<String, RouteError> {
        let mut url = route.destination_url.clone();

        // Add click_id tracking param
        let separator = if url.contains('?') { "&" } else { "?" };
        url.push_str(&format!("{}click_id={}", separator, click_id));

        // Append additional tracking params from route config
        if let Some(ref params) = route.tracking_params {
            url.push_str(&format!("&{}", params));
        }

        Ok(url)
    }

    /// Cache statistics for monitoring
    pub fn cache_stats(&self) -> CacheStats {
        CacheStats {
            entries: self.cache.len(),
            capacity: self.cache.capacity(),
        }
    }
}

impl Default for RouteResolver {
    fn default() -> Self {
        Self::new()
    }
}

/// Cache statistics
#[derive(Debug, Clone, serde::Serialize)]
pub struct CacheStats {
    pub entries: usize,
    pub capacity: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_route_resolution_cache_hit() {
        let resolver = RouteResolver::new();
        let route = create_test_route("promo-123");
        resolver.warm_cache(vec![route.clone()]).expect("warm cache");

        let result = resolver.resolve("promo-123").expect("resolve");
        assert_eq!(result.source_path, "promo-123");
    }

    #[test]
    fn test_route_not_found() {
        let resolver = RouteResolver::new();
        let result = resolver.resolve("nonexistent");
        assert!(matches!(result, Err(RouteError::NotFound { .. })));
    }

    #[test]
    fn test_invalid_slug() {
        let resolver = RouteResolver::new();
        let result = resolver.resolve("invalid/../path");
        assert!(matches!(result, Err(RouteError::InvalidSlug { .. })));
    }

    #[test]
    fn test_inactive_route() {
        let resolver = RouteResolver::new();
        let mut route = create_test_route("inactive");
        route.active = false;
        resolver.warm_cache(vec![route]).expect("warm cache");

        let result = resolver.resolve("inactive");
        assert!(matches!(result, Err(RouteError::Inactive { .. })));
    }

    fn create_test_route(slug: &str) -> AffiliateLinkRoute {
        AffiliateLinkRoute {
            id: Uuid::now_v7(),
            source_path: slug.to_string(),
            destination_url: "https://example.com/offer".to_string(),
            affiliate_id: Uuid::now_v7(),
            tracking_params: None,
            created_at: Utc::now(),
            active: true,
        }
    }
}
