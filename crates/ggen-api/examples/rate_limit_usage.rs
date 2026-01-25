//! Example: Rate limiting middleware usage
//!
//! Demonstrates how to integrate production-ready rate limiting
//! with per-IP and per-API-key limits, Redis backend, and graceful degradation.

use axum::{extract::State, routing::get, Router};
use ggen_api::middleware::{rate_limit_middleware, RateLimitBackend, RateLimitConfig, RateLimiter};
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;
use tracing_subscriber;

async fn health_check() -> &'static str {
    "OK"
}

async fn api_endpoint() -> &'static str {
    "API response"
}

/// Example 1: In-memory rate limiting (single instance)
async fn example_in_memory() -> Result<(), Box<dyn std::error::Error>> {
    // Configure in-memory rate limiter
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 20,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };

    let limiter = Arc::new(RateLimiter::new(config).await?);

    // Create router with rate limiting middleware
    let app = Router::new()
        .route("/api/data", get(api_endpoint))
        .layer(axum::middleware::from_fn_with_state(
            limiter.clone(),
            rate_limit_middleware,
        ))
        .with_state(limiter);

    // Serve (in production)
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

/// Example 2: Redis-backed rate limiting (distributed)
async fn example_redis() -> Result<(), Box<dyn std::error::Error>> {
    // Configure Redis-backed rate limiter for distributed deployments
    let config = RateLimitConfig {
        requests_per_second: 100,
        burst_size: 200,
        backend: RateLimitBackend::Redis {
            url: "redis://localhost:6379".to_string(),
        },
        ttl: Duration::from_secs(60),
    };

    let limiter = Arc::new(RateLimiter::new(config).await?);

    let app = Router::new()
        .route("/api/data", get(api_endpoint))
        .layer(axum::middleware::from_fn_with_state(
            limiter.clone(),
            rate_limit_middleware,
        ))
        .with_state(limiter);

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

/// Example 3: Custom limits per route
async fn example_multi_tier() -> Result<(), Box<dyn std::error::Error>> {
    // Free tier: 10 req/s
    let free_config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 15,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let free_limiter = Arc::new(RateLimiter::new(free_config).await?);

    // Premium tier: 100 req/s
    let premium_config = RateLimitConfig {
        requests_per_second: 100,
        burst_size: 150,
        backend: RateLimitBackend::Redis {
            url: "redis://localhost:6379".to_string(),
        },
        ttl: Duration::from_secs(60),
    };
    let premium_limiter = Arc::new(RateLimiter::new(premium_config).await?);

    // Create separate routers for different tiers
    let free_routes = Router::new()
        .route("/api/free/data", get(api_endpoint))
        .layer(axum::middleware::from_fn_with_state(
            free_limiter.clone(),
            rate_limit_middleware,
        ))
        .with_state(free_limiter);

    let premium_routes = Router::new()
        .route("/api/premium/data", get(api_endpoint))
        .layer(axum::middleware::from_fn_with_state(
            premium_limiter.clone(),
            rate_limit_middleware,
        ))
        .with_state(premium_limiter);

    // Combine routers
    let app = Router::new()
        .route("/health", get(health_check))
        .merge(free_routes)
        .merge(premium_routes);

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

/// Example 4: Programmatic rate limit checking (without middleware)
async fn example_programmatic() -> Result<(), Box<dyn std::error::Error>> {
    let config = RateLimitConfig::default();
    let limiter = RateLimiter::new(config).await?;

    // Check limit for API key
    let api_key = "user-api-key-123";
    match limiter.check_limit(&format!("api:{}", api_key)).await {
        Ok(()) => {
            println!("Request allowed");
            // Process request
        }
        Err(e) => {
            println!("Rate limited: {}", e);
            // Return 429 response
        }
    }

    // Check limit for IP address
    let ip = "192.168.1.100";
    match limiter.check_limit(&format!("ip:{}", ip)).await {
        Ok(()) => {
            println!("Request allowed");
        }
        Err(e) => {
            println!("Rate limited: {}", e);
        }
    }

    Ok(())
}

/// Example 5: Testing graceful degradation
async fn example_graceful_degradation() -> Result<(), Box<dyn std::error::Error>> {
    // Configure with Redis that might be unavailable
    let config = RateLimitConfig {
        requests_per_second: 50,
        burst_size: 100,
        backend: RateLimitBackend::Redis {
            url: "redis://unreachable-host:6379".to_string(),
        },
        ttl: Duration::from_secs(60),
    };

    // This will fail to connect to Redis
    let limiter = match RateLimiter::new(config.clone()).await {
        Ok(limiter) => Arc::new(limiter),
        Err(e) => {
            eprintln!("Redis connection failed: {}, falling back to in-memory", e);

            // Fallback to in-memory configuration
            let fallback_config = RateLimitConfig {
                backend: RateLimitBackend::InMemory,
                ..config
            };
            Arc::new(RateLimiter::new(fallback_config).await?)
        }
    };

    // The limiter will automatically use in-memory fallback if Redis fails
    let app = Router::new()
        .route("/api/data", get(api_endpoint))
        .layer(axum::middleware::from_fn_with_state(
            limiter.clone(),
            rate_limit_middleware,
        ))
        .with_state(limiter);

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    println!("Rate Limiter Examples");
    println!("====================");
    println!();
    println!("Run individual examples:");
    println!("  cargo run --example rate_limit_usage");
    println!();
    println!("Available patterns:");
    println!("  1. In-memory (single instance)");
    println!("  2. Redis-backed (distributed)");
    println!("  3. Multi-tier (free vs premium)");
    println!("  4. Programmatic checking");
    println!("  5. Graceful degradation");

    // Run in-memory example by default
    example_in_memory().await?;

    Ok(())
}
