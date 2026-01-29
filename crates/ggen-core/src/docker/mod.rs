//! Docker Compose generation from RDF ontologies
//!
//! This module provides functionality to generate docker-compose.yml files
//! from RDF service definitions. It extracts service configurations using
//! SPARQL queries and renders them into production-ready Docker Compose files.
//!
//! ## Features
//!
//! - **RDF-driven configuration**: Define services in RDF, generate Docker Compose
//! - **Health checks**: Automatic health check configuration for all services
//! - **Networks and volumes**: Support for Docker networks and persistent volumes
//! - **Environment variables**: Secure environment variable management
//! - **Multi-service orchestration**: Redis, PostgreSQL, monitoring services
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_core::docker::{DockerComposeGenerator, ServiceConfig};
//! use ggen_core::Graph;
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let graph = Graph::new()?;
//! graph.insert_turtle(r#"
//!     @prefix svc: <http://example.org/service#> .
//!     @prefix docker: <http://ggen.io/docker#> .
//!
//!     svc:redis a docker:Service ;
//!         docker:image "redis:7-alpine" ;
//!         docker:port "6379" ;
//!         docker:healthCheck "redis-cli ping" .
//! "#)?;
//!
//! let generator = DockerComposeGenerator::new(&graph)?;
//! let compose_yaml = generator.generate().await?;
//! # Ok(())
//! # }
//! ```

pub mod compose;
pub mod service_config;

pub use compose::DockerComposeGenerator;
pub use service_config::{HealthCheck, ServiceConfig, ServicePort};
