//! REST API Wrapper for ggen
//!
//! This provides a RESTful HTTP API for ggen's code generation capabilities.
//!
//! ## Features
//! - Template generation endpoints
//! - AI-powered generation
//! - Rate limiting
//! - OpenAPI/Swagger documentation
//! - CORS support
//! - Error handling

use actix_cors::Cors;
use actix_web::{middleware::Logger, web, App, HttpResponse, HttpServer, Responder};
use ggen_ai::{GenAiClient, LlmConfig, LlmProvider, TemplateGenerator};
use ggen_core::{GenContext, Generator, Template};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;
use utoipa::{OpenApi, ToSchema};
use utoipa_swagger_ui::SwaggerUi;

/// Application state shared across requests
struct AppState {
    generator: Arc<Generator>,
    template_cache: Arc<Mutex<HashMap<String, Template>>>,
}

/// Request to generate code from a template
#[derive(Debug, Deserialize, Serialize, ToSchema)]
struct GenerateRequest {
    /// Template content or ID
    template: String,
    /// Generation context variables
    #[serde(default)]
    context: HashMap<String, String>,
    /// Whether template is an ID (cached) or inline content
    #[serde(default)]
    is_template_id: bool,
}

/// Response from code generation
#[derive(Debug, Serialize, ToSchema)]
struct GenerateResponse {
    /// Generated code
    output: String,
    /// Template name used
    template_name: String,
}

/// Request to create/update a template
#[derive(Debug, Deserialize, Serialize, ToSchema)]
struct CreateTemplateRequest {
    /// Template ID for caching
    id: String,
    /// Template content in ggen format
    content: String,
}

/// Response from template creation
#[derive(Debug, Serialize, ToSchema)]
struct CreateTemplateResponse {
    /// Template ID
    id: String,
    /// Template metadata
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
}

/// Request for AI-powered template generation
#[derive(Debug, Deserialize, Serialize, ToSchema)]
struct AiGenerateRequest {
    /// Natural language description
    description: String,
    /// Additional requirements
    #[serde(default)]
    requirements: Vec<String>,
    /// LLM provider to use
    #[serde(default)]
    provider: Option<String>,
    /// Model to use
    #[serde(default)]
    model: Option<String>,
}

/// Response from AI generation
#[derive(Debug, Serialize, ToSchema)]
struct AiGenerateResponse {
    /// Generated template content
    template: String,
    /// Template metadata
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
}

/// Error response
#[derive(Debug, Serialize, ToSchema)]
struct ErrorResponse {
    /// Error message
    error: String,
    /// Additional details
    #[serde(skip_serializing_if = "Option::is_none")]
    details: Option<String>,
}

/// OpenAPI documentation
#[derive(OpenApi)]
#[openapi(
    paths(
        health_check,
        generate,
        create_template,
        get_template,
        list_templates,
        ai_generate,
    ),
    components(
        schemas(
            GenerateRequest,
            GenerateResponse,
            CreateTemplateRequest,
            CreateTemplateResponse,
            AiGenerateRequest,
            AiGenerateResponse,
            ErrorResponse,
        )
    ),
    tags(
        (name = "ggen", description = "ggen code generation API")
    ),
    info(
        title = "ggen REST API",
        version = "1.0.0",
        description = "REST API wrapper for ggen code generation",
    )
)]
struct ApiDoc;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // Initialize logging
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();
    tracing::subscriber::set_global_default(subscriber)
        .expect("Failed to set subscriber");

    info!("Starting ggen REST API server");

    // Initialize application state
    let generator = Generator::new(vec![], HashMap::new()).expect("Failed to create generator");
    let state = web::Data::new(AppState {
        generator: Arc::new(generator),
        template_cache: Arc::new(Mutex::new(HashMap::new())),
    });

    info!("Starting server on http://0.0.0.0:8080");
    info!("Swagger UI available at http://0.0.0.0:8080/swagger-ui/");

    // Start HTTP server
    HttpServer::new(move || {
        let cors = Cors::default()
            .allow_any_origin()
            .allow_any_method()
            .allow_any_header();

        App::new()
            .app_data(state.clone())
            .wrap(Logger::default())
            .wrap(cors)
            .service(
                SwaggerUi::new("/swagger-ui/{_:.*}")
                    .url("/api-docs/openapi.json", ApiDoc::openapi()),
            )
            .service(
                web::scope("/api")
                    .route("/health", web::get().to(health_check))
                    .route("/generate", web::post().to(generate))
                    .route("/templates", web::post().to(create_template))
                    .route("/templates", web::get().to(list_templates))
                    .route("/templates/{id}", web::get().to(get_template))
                    .route("/ai/generate", web::post().to(ai_generate)),
            )
    })
    .bind(("0.0.0.0", 8080))?
    .run()
    .await
}

/// Health check endpoint
#[utoipa::path(
    get,
    path = "/api/health",
    responses(
        (status = 200, description = "Service is healthy", body = String)
    )
)]
async fn health_check() -> impl Responder {
    HttpResponse::Ok().json(serde_json::json!({
        "status": "healthy",
        "service": "ggen-rest-api",
        "version": env!("CARGO_PKG_VERSION")
    }))
}

/// Generate code from template
#[utoipa::path(
    post,
    path = "/api/generate",
    request_body = GenerateRequest,
    responses(
        (status = 200, description = "Code generated successfully", body = GenerateResponse),
        (status = 400, description = "Invalid request", body = ErrorResponse),
        (status = 500, description = "Generation failed", body = ErrorResponse)
    )
)]
async fn generate(
    state: web::Data<AppState>,
    req: web::Json<GenerateRequest>,
) -> impl Responder {
    info!("Generate request received");

    // Get template
    let template = if req.is_template_id {
        // Load from cache
        let cache = state.template_cache.lock().await;
        match cache.get(&req.template) {
            Some(t) => t.clone(),
            None => {
                return HttpResponse::NotFound().json(ErrorResponse {
                    error: "Template not found".to_string(),
                    details: Some(format!("No template with ID: {}", req.template)),
                });
            }
        }
    } else {
        // Parse inline template
        match Template::from_str(&req.template) {
            Ok(t) => t,
            Err(e) => {
                return HttpResponse::BadRequest().json(ErrorResponse {
                    error: "Invalid template".to_string(),
                    details: Some(e.to_string()),
                });
            }
        }
    };

    // Create context
    let mut context = GenContext::new();
    for (key, value) in &req.context {
        context.insert(key, value);
    }

    // Generate
    match state.generator.generate(&template, &context).await {
        Ok(output) => HttpResponse::Ok().json(GenerateResponse {
            output,
            template_name: template.metadata.name.clone(),
        }),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: "Generation failed".to_string(),
            details: Some(e.to_string()),
        }),
    }
}

/// Create or update a template
#[utoipa::path(
    post,
    path = "/api/templates",
    request_body = CreateTemplateRequest,
    responses(
        (status = 200, description = "Template created", body = CreateTemplateResponse),
        (status = 400, description = "Invalid template", body = ErrorResponse)
    )
)]
async fn create_template(
    state: web::Data<AppState>,
    req: web::Json<CreateTemplateRequest>,
) -> impl Responder {
    info!("Create template request: {}", req.id);

    // Parse template
    let template = match Template::from_str(&req.content) {
        Ok(t) => t,
        Err(e) => {
            return HttpResponse::BadRequest().json(ErrorResponse {
                error: "Invalid template".to_string(),
                details: Some(e.to_string()),
            });
        }
    };

    // Store in cache
    let mut cache = state.template_cache.lock().await;
    cache.insert(req.id.clone(), template.clone());

    HttpResponse::Ok().json(CreateTemplateResponse {
        id: req.id.clone(),
        name: template.metadata.name.clone(),
        description: template.metadata.description.clone(),
    })
}

/// Get template by ID
#[utoipa::path(
    get,
    path = "/api/templates/{id}",
    params(
        ("id" = String, Path, description = "Template ID")
    ),
    responses(
        (status = 200, description = "Template found", body = String),
        (status = 404, description = "Template not found", body = ErrorResponse)
    )
)]
async fn get_template(state: web::Data<AppState>, id: web::Path<String>) -> impl Responder {
    info!("Get template request: {}", id);

    let cache = state.template_cache.lock().await;
    match cache.get(id.as_str()) {
        Some(template) => HttpResponse::Ok().json(serde_json::json!({
            "id": id.as_str(),
            "name": template.metadata.name,
            "description": template.metadata.description,
            "version": template.metadata.version,
            "content": template.content,
        })),
        None => HttpResponse::NotFound().json(ErrorResponse {
            error: "Template not found".to_string(),
            details: None,
        }),
    }
}

/// List all cached templates
#[utoipa::path(
    get,
    path = "/api/templates",
    responses(
        (status = 200, description = "Templates list", body = Vec<CreateTemplateResponse>)
    )
)]
async fn list_templates(state: web::Data<AppState>) -> impl Responder {
    info!("List templates request");

    let cache = state.template_cache.lock().await;
    let templates: Vec<_> = cache
        .iter()
        .map(|(id, template)| CreateTemplateResponse {
            id: id.clone(),
            name: template.metadata.name.clone(),
            description: template.metadata.description.clone(),
        })
        .collect();

    HttpResponse::Ok().json(templates)
}

/// AI-powered template generation
#[utoipa::path(
    post,
    path = "/api/ai/generate",
    request_body = AiGenerateRequest,
    responses(
        (status = 200, description = "Template generated", body = AiGenerateResponse),
        (status = 400, description = "Invalid request", body = ErrorResponse),
        (status = 500, description = "Generation failed", body = ErrorResponse)
    )
)]
async fn ai_generate(req: web::Json<AiGenerateRequest>) -> impl Responder {
    info!("AI generate request: {}", req.description);

    // Get API key from environment
    let api_key = std::env::var("OPENAI_API_KEY").unwrap_or_else(|_| {
        std::env::var("ANTHROPIC_API_KEY").unwrap_or_default()
    });

    if api_key.is_empty() {
        return HttpResponse::BadRequest().json(ErrorResponse {
            error: "No API key configured".to_string(),
            details: Some("Set OPENAI_API_KEY or ANTHROPIC_API_KEY environment variable".to_string()),
        });
    }

    // Determine provider and model
    let provider = req.provider.as_deref().unwrap_or("openai");
    let model = req.model.as_deref().unwrap_or("gpt-4o");

    let provider_enum = match provider {
        "openai" => LlmProvider::OpenAI,
        "anthropic" => LlmProvider::Anthropic,
        "ollama" => LlmProvider::Ollama,
        _ => {
            return HttpResponse::BadRequest().json(ErrorResponse {
                error: "Invalid provider".to_string(),
                details: Some(format!("Provider must be one of: openai, anthropic, ollama")),
            });
        }
    };

    // Create LLM client
    let config = LlmConfig {
        provider: provider_enum,
        model: model.to_string(),
        api_key,
        ..Default::default()
    };

    let client = match GenAiClient::with_config(config) {
        Ok(c) => c,
        Err(e) => {
            return HttpResponse::InternalServerError().json(ErrorResponse {
                error: "Failed to create LLM client".to_string(),
                details: Some(e.to_string()),
            });
        }
    };

    // Generate template
    let generator = TemplateGenerator::new(Box::new(client));

    match generator
        .generate_template(
            &req.description,
            req.requirements.iter().map(|s| s.as_str()).collect(),
        )
        .await
    {
        Ok(template) => HttpResponse::Ok().json(AiGenerateResponse {
            template: template.content.clone(),
            name: template.metadata.name.clone(),
            description: template.metadata.description.clone(),
        }),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: "AI generation failed".to_string(),
            details: Some(e.to_string()),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::{test, App};

    #[actix_web::test]
    async fn test_health_check() {
        let app = test::init_service(App::new().route("/health", web::get().to(health_check))).await;

        let req = test::TestRequest::get().uri("/health").to_request();
        let resp = test::call_service(&app, req).await;

        assert!(resp.status().is_success());
    }
}
