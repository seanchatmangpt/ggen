//! Affiliate link routing and resolution

use super::{InvalidRouteSlug, RouteSlug};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Affiliate route configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AffiliateRoute {
    /// Route ID
    pub id: Uuid,
    /// Route slug
    pub slug: RouteSlug,
    /// Target URL
    pub target_url: String,
    /// Affiliate ID
    pub affiliate_id: Uuid,
    /// Active status
    pub active: bool,
}

impl AffiliateRoute {
    /// Create a new affiliate route
    pub fn new(slug: RouteSlug, target_url: String, affiliate_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            slug,
            target_url,
            affiliate_id,
            active: true,
        }
    }

    /// Deactivate route
    pub fn deactivate(&mut self) {
        self.active = false;
    }
}

/// Route resolver - deterministic route resolution
pub struct RouteResolver {
    routes: HashMap<String, AffiliateRoute>,
}

impl RouteResolver {
    /// Create a new route resolver
    pub fn new() -> Self {
        Self {
            routes: HashMap::new(),
        }
    }

    /// Add a route
    pub fn add_route(&mut self, route: AffiliateRoute) -> Result<(), RouteError> {
        let slug_str = route.slug.as_str().to_string();
        if self.routes.contains_key(&slug_str) {
            return Err(RouteError::DuplicateSlug);
        }
        self.routes.insert(slug_str, route);
        Ok(())
    }

    /// Resolve a route slug to target URL
    pub fn resolve(&self, slug: &RouteSlug) -> Result<&str, RouteError> {
        self.routes
            .get(slug.as_str())
            .filter(|route| route.active)
            .map(|route| route.target_url.as_str())
            .ok_or(RouteError::NotFound)
    }

    /// Get route by slug
    pub fn get_route(&self, slug: &RouteSlug) -> Option<&AffiliateRoute> {
        self.routes.get(slug.as_str())
    }

    /// List all active routes
    pub fn list_active_routes(&self) -> Vec<&AffiliateRoute> {
        self.routes.values().filter(|route| route.active).collect()
    }
}

impl Default for RouteResolver {
    fn default() -> Self {
        Self::new()
    }
}

/// Route resolution errors
#[derive(Debug, Clone, thiserror::Error)]
pub enum RouteError {
    #[error("Route not found")]
    NotFound,
    #[error("Duplicate route slug")]
    DuplicateSlug,
}
