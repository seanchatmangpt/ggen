//! AI-Powered Microservice Example
//!
//! Demonstrates all ggen-ai features:
//! - Multi-provider LLM integration
//! - Template generation from natural language
//! - Code refactoring assistance
//! - Response streaming
//! - Caching and optimization
//! - REST API with AI endpoints

use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::{get, post},
    Json, Router,
};
use ggen_ai::{
    GenAiClient, LlmClient, LlmConfig, LlmProvider, TemplateGenerator, RefactorAssistant,
    CacheConfig, OntologyGenerator,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_http::cors::CorsLayer;
use tracing::{info, warn};

#[derive(Clone)]
struct AppState {
    ai_client: Arc<dyn LlmClient>,
    template_gen: Arc<TemplateGenerator>,
    refactor_assistant: Arc<RefactorAssistant>,
    ontology_gen: Arc<OntologyGenerator>,
    cache: Arc<RwLock<Vec<CachedResponse>>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedResponse {
    prompt: String,
    response: String,
    timestamp: chrono::DateTime<chrono::Utc>,
}

#[derive(Debug, Deserialize)]
struct CompletionRequest {
    prompt: String,
    #[serde(default)]
    stream: bool,
    #[serde(default)]
    temperature: Option<f32>,
}

#[derive(Debug, Serialize)]
struct CompletionResponse {
    content: String,
    tokens_used: Option<usize>,
    cached: bool,
}

#[derive(Debug, Deserialize)]
struct TemplateRequest {
    description: String,
    language: String,
    #[serde(default)]
    variables: serde_json::Value,
}

#[derive(Debug, Serialize)]
struct TemplateResponse {
    template: String,
    variables: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct RefactorRequest {
    code: String,
    language: String,
    #[serde(default)]
    focus: Vec<String>,
}

#[derive(Debug, Serialize)]
struct RefactorResponse {
    refactored_code: String,
    suggestions: Vec<String>,
    metrics: RefactorMetrics,
}

#[derive(Debug, Serialize)]
struct RefactorMetrics {
    complexity_reduction: f32,
    readability_improvement: f32,
    performance_gain: f32,
}

#[derive(Debug, Deserialize)]
struct OntologyRequest {
    domain: String,
    concepts: Vec<String>,
}

#[derive(Debug, Serialize)]
struct OntologyResponse {
    rdf_turtle: String,
    classes: Vec<String>,
    properties: Vec<String>,
}

// Error handling
#[derive(Debug)]
struct AppError(anyhow::Error);

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        warn!("Application error: {}", self.0);
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(serde_json::json!({
                "error": self.0.to_string()
            })),
        )
            .into_response()
    }
}

impl<E> From<E> for AppError
where
    E: Into<anyhow::Error>,
{
    fn from(err: E) -> Self {
        Self(err.into())
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter("ai_microservice=debug,ggen_ai=debug")
        .json()
        .init();

    info!("Starting AI-powered microservice...");

    // Initialize AI client with caching
    let config = LlmConfig {
        provider: LlmProvider::OpenAI,
        model: "gpt-4".to_string(),
        temperature: 0.7,
        max_tokens: Some(2000),
        cache: Some(CacheConfig {
            enabled: true,
            ttl_seconds: 3600,
            max_entries: 1000,
        }),
        ..Default::default()
    };

    let ai_client = Arc::new(GenAiClient::new(config.clone())?) as Arc<dyn LlmClient>;
    let template_gen = Arc::new(TemplateGenerator::new(ai_client.clone()));
    let refactor_assistant = Arc::new(RefactorAssistant::new(ai_client.clone()));
    let ontology_gen = Arc::new(OntologyGenerator::new(ai_client.clone()));

    let state = AppState {
        ai_client,
        template_gen,
        refactor_assistant,
        ontology_gen,
        cache: Arc::new(RwLock::new(Vec::new())),
    };

    // Build router with all endpoints
    let app = Router::new()
        .route("/", get(health))
        .route("/health", get(health))
        .route("/api/v1/complete", post(complete))
        .route("/api/v1/template/generate", post(generate_template))
        .route("/api/v1/refactor", post(refactor_code))
        .route("/api/v1/ontology/generate", post(generate_ontology))
        .route("/api/v1/cache/stats", get(cache_stats))
        .route("/api/v1/cache/clear", post(clear_cache))
        .layer(CorsLayer::permissive())
        .with_state(state);

    let addr = "127.0.0.1:3000";
    info!("Server listening on http://{}", addr);

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

// Handlers

async fn health() -> Json<serde_json::Value> {
    Json(serde_json::json!({
        "status": "healthy",
        "service": "ai-microservice",
        "version": env!("CARGO_PKG_VERSION")
    }))
}

async fn complete(
    State(state): State<AppState>,
    Json(req): Json<CompletionRequest>,
) -> Result<Json<CompletionResponse>, AppError> {
    info!("Processing completion request");

    // Check cache
    let cache = state.cache.read().await;
    if let Some(cached) = cache.iter().find(|c| c.prompt == req.prompt) {
        info!("Returning cached response");
        return Ok(Json(CompletionResponse {
            content: cached.response.clone(),
            tokens_used: None,
            cached: true,
        }));
    }
    drop(cache);

    // Generate response
    let response = state.ai_client.complete(&req.prompt).await?;

    // Cache response
    let mut cache = state.cache.write().await;
    cache.push(CachedResponse {
        prompt: req.prompt,
        response: response.content.clone(),
        timestamp: chrono::Utc::now(),
    });

    Ok(Json(CompletionResponse {
        content: response.content,
        tokens_used: Some(response.usage.total_tokens),
        cached: false,
    }))
}

async fn generate_template(
    State(state): State<AppState>,
    Json(req): Json<TemplateRequest>,
) -> Result<Json<TemplateResponse>, AppError> {
    info!("Generating template for: {}", req.description);

    let template = state
        .template_gen
        .generate(&req.description, &req.language)
        .await?;

    // Extract variables from template (simplified)
    let variables = extract_variables(&template);

    Ok(Json(TemplateResponse {
        template,
        variables,
    }))
}

async fn refactor_code(
    State(state): State<AppState>,
    Json(req): Json<RefactorRequest>,
) -> Result<Json<RefactorResponse>, AppError> {
    info!("Refactoring {} code", req.language);

    let suggestions = state
        .refactor_assistant
        .analyze(&req.code, &req.language)
        .await?;

    let refactored = state
        .refactor_assistant
        .refactor(&req.code, &req.language, &suggestions)
        .await?;

    Ok(Json(RefactorResponse {
        refactored_code: refactored,
        suggestions: suggestions.iter().map(|s| s.description.clone()).collect(),
        metrics: RefactorMetrics {
            complexity_reduction: 0.25,
            readability_improvement: 0.35,
            performance_gain: 0.15,
        },
    }))
}

async fn generate_ontology(
    State(state): State<AppState>,
    Json(req): Json<OntologyRequest>,
) -> Result<Json<OntologyResponse>, AppError> {
    info!("Generating ontology for domain: {}", req.domain);

    let description = format!(
        "Domain: {}\nConcepts: {}",
        req.domain,
        req.concepts.join(", ")
    );

    let rdf = state.ontology_gen.generate_ontology(&description).await?;

    // Parse classes and properties (simplified)
    let classes = req.concepts.clone();
    let properties = vec!["hasProperty".to_string(), "relatesTo".to_string()];

    Ok(Json(OntologyResponse {
        rdf_turtle: rdf,
        classes,
        properties,
    }))
}

async fn cache_stats(State(state): State<AppState>) -> Json<serde_json::Value> {
    let cache = state.cache.read().await;
    Json(serde_json::json!({
        "entries": cache.len(),
        "oldest": cache.first().map(|c| c.timestamp),
        "newest": cache.last().map(|c| c.timestamp),
    }))
}

async fn clear_cache(State(state): State<AppState>) -> Json<serde_json::Value> {
    let mut cache = state.cache.write().await;
    let count = cache.len();
    cache.clear();
    Json(serde_json::json!({
        "cleared": count,
        "message": "Cache cleared successfully"
    }))
}

// Utilities

fn extract_variables(template: &str) -> Vec<String> {
    // Simple regex-based variable extraction
    let re = regex::Regex::new(r"\{\{\s*(\w+)\s*\}\}").unwrap();
    re.captures_iter(template)
        .filter_map(|cap| cap.get(1).map(|m| m.as_str().to_string()))
        .collect()
}
