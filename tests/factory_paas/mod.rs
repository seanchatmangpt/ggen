//! FactoryPaaS Test Module
//!
//! Comprehensive Chicago TDD test suite for FactoryPaaS covering:
//! - Integration tests (end-to-end workflows)
//! - Property tests (determinism and correctness)
//! - Load tests (performance under load)

pub mod integration_tests;
pub mod load_tests;
pub mod property_tests;

use ggen_saas::factory_paas::*;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Test database using testcontainers
#[cfg(test)]
pub struct TestDatabase {
    _container: testcontainers::Container<'static, testcontainers_modules::postgres::Postgres>,
    pub connection_string: String,
}

#[cfg(test)]
impl TestDatabase {
    /// Create a new test database with PostgreSQL
    pub async fn new() -> Result<Self, Box<dyn std::error::Error>> {
        use testcontainers::{clients::Cli, RunnableImage};
        use testcontainers_modules::postgres::Postgres;

        let docker = Cli::default();
        let postgres = Postgres::default();
        let image = RunnableImage::from(postgres);

        let container = docker.run(image);
        let port = container.get_host_port_ipv4(5432);

        let connection_string =
            format!("postgresql://postgres:postgres@localhost:{}/postgres", port);

        Ok(Self {
            _container: container,
            connection_string,
        })
    }
}

/// Shared test context with real collaborators
#[cfg(test)]
#[derive(Clone)]
pub struct TestContext {
    pub route_resolver: Arc<RwLock<affiliate::RouteResolver>>,
    pub click_tracker: Arc<RwLock<click_tracking::ClickTracker>>,
    pub content_pipeline: Arc<RwLock<content::ContentPipeline>>,
    pub revenue_attribution: Arc<RwLock<revenue::RevenueAttribution>>,
    pub subscription_manager: Arc<RwLock<subscription::SubscriptionManager>>,
}

#[cfg(test)]
impl TestContext {
    /// Create a new test context with real objects
    pub fn new() -> Self {
        Self {
            route_resolver: Arc::new(RwLock::new(affiliate::RouteResolver::new())),
            click_tracker: Arc::new(RwLock::new(click_tracking::ClickTracker::new())),
            content_pipeline: Arc::new(RwLock::new(content::ContentPipeline::new())),
            revenue_attribution: Arc::new(RwLock::new(revenue::RevenueAttribution::new())),
            subscription_manager: Arc::new(RwLock::new(subscription::SubscriptionManager::new())),
        }
    }

    /// Setup sample affiliate routes for testing
    pub async fn setup_sample_routes(&self) -> Result<Vec<RouteSlug>, Box<dyn std::error::Error>> {
        let mut resolver = self.route_resolver.write().await;

        let slugs = vec![
            RouteSlug::new("tech-gadgets-2024".to_string())?,
            RouteSlug::new("fitness-equipment".to_string())?,
            RouteSlug::new("software-deals".to_string())?,
        ];

        for slug in &slugs {
            let route = affiliate::AffiliateRoute::new(
                slug.clone(),
                format!("https://example.com/affiliate/{}", slug),
                Uuid::new_v4(),
            );
            resolver.add_route(route)?;
        }

        Ok(slugs)
    }

    /// Create a test subscription
    pub async fn create_test_subscription(&self, tier: SubscriptionTier) -> Uuid {
        let user_id = Uuid::new_v4();
        let mut manager = self.subscription_manager.write().await;
        manager.create_subscription(user_id, tier)
    }
}

#[cfg(test)]
impl Default for TestContext {
    fn default() -> Self {
        Self::new()
    }
}
