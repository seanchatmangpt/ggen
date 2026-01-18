use actix_web::{get, web, HttpResponse, Responder};
use chrono::Utc;
use prometheus::{Encoder, TextEncoder};

use crate::models::HealthResponse;

pub fn configure(cfg: &mut web::ServiceConfig) {
    cfg.service(health_check).service(metrics);
}

/// Health check endpoint
#[utoipa::path(
    get,
    path = "/api/v1/health",
    tag = "health",
    responses(
        (status = 200, description = "Service is healthy", body = HealthResponse)
    )
)]
#[get("/health")]
async fn health_check() -> impl Responder {
    HttpResponse::Ok().json(HealthResponse {
        status: "healthy".to_string(),
        version: env!("CARGO_PKG_VERSION").to_string(),
        timestamp: Utc::now(),
    })
}

/// Prometheus metrics endpoint
#[utoipa::path(
    get,
    path = "/api/v1/metrics",
    tag = "health",
    responses(
        (status = 200, description = "Prometheus metrics")
    )
)]
#[get("/metrics")]
async fn metrics() -> impl Responder {
    let encoder = TextEncoder::new();
    let metric_families = prometheus::gather();
    let mut buffer = vec![];

    if let Err(e) = encoder.encode(&metric_families, &mut buffer) {
        return HttpResponse::InternalServerError()
            .body(format!("Failed to encode metrics: {}", e));
    }

    HttpResponse::Ok()
        .content_type("text/plain; version=0.0.4")
        .body(buffer)
}
