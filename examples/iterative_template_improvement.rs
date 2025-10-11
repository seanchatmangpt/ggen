//! Example demonstrating iterative template improvement with AI validation
//!
//! This example shows how ggen-ai can validate and iteratively improve generated templates
//! to ensure they meet quality standards and best practices.

use ggen_ai::providers::MockClient;
use ggen_ai::{TemplateGenerator, TemplateValidator, ValidationResult};
use ggen_core::Template;
use std::fs;
use std::path::Path;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 ggen-ai Iterative Template Improvement Demo");
    println!("==============================================");

    // Initialize mock client for testing
    let client = Box::new(MockClient::new(vec![
        // First iteration - basic template
        r#"---
to: "user_service.rs"
vars:
  name: "UserService"
  description: "A basic user service"
---

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct User {
    pub id: u32,
    pub name: String,
    pub email: String,
}

pub struct {{ name }} {
    // Service implementation
}

impl {{ name }} {
    pub fn new() -> Self {
        Self {}
    }
}
"#
        .to_string(),
        // Second iteration - improved template with better structure
        r#"---
to: "user_service.rs"
vars:
  name: "UserService"
  description: "A comprehensive user service with CRUD operations"
  language: "rust"
  framework: "axum"
---

use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::Json,
    routing::{get, post, put, delete},
    Router,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: u32,
    pub name: String,
    pub email: String,
    pub created_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateUserRequest {
    pub name: String,
    pub email: String,
}

#[derive(Debug, Clone)]
pub struct {{ name }}Service {
    // Database connection would go here
}

impl {{ name }}Service {
    pub fn new() -> Self {
        Self {}
    }

    pub async fn create_user(&self, req: CreateUserRequest) -> Result<User, String> {
        Ok(User {
            id: 1,
            name: req.name,
            email: req.email,
            created_at: Utc::now(),
        })
    }

    pub async fn get_user(&self, id: u32) -> Result<Option<User>, String> {
        Ok(Some(User {
            id,
            name: "Example User".to_string(),
            email: "user@example.com".to_string(),
            created_at: Utc::now(),
        }))
    }

    pub async fn update_user(&self, id: u32, req: CreateUserRequest) -> Result<User, String> {
        Ok(User {
            id,
            name: req.name,
            email: req.email,
            created_at: Utc::now(),
        })
    }

    pub async fn delete_user(&self, id: u32) -> Result<(), String> {
        Ok(())
    }
}

// REST API handlers
async fn create_user_handler(
    State(service): State<Arc<{{ name }}Service>>,
    Json(payload): Json<CreateUserRequest>,
) -> Result<Json<User>, StatusCode> {
    match service.create_user(payload).await {
        Ok(user) => Ok(Json(user)),
        Err(_) => Err(StatusCode::INTERNAL_SERVER_ERROR),
    }
}

async fn get_user_handler(
    State(service): State<Arc<{{ name }}Service>>,
    Path(id): Path<u32>,
) -> Result<Json<User>, StatusCode> {
    match service.get_user(id).await {
        Ok(Some(user)) => Ok(Json(user)),
        Ok(None) => Err(StatusCode::NOT_FOUND),
        Err(_) => Err(StatusCode::INTERNAL_SERVER_ERROR),
    }
}

pub fn create_user_router() -> Router {
    let service = Arc::new({{ name }}Service::new());

    Router::new()
        .route("/users", post(create_user_handler))
        .route("/users/:id", get(get_user_handler))
        .with_state(service)
}
"#
        .to_string(),
    ]));

    let generator = TemplateGenerator::new(client);

    // Step 1: Generate initial template
    println!("\n📝 Step 1: Generating initial template...");
    let initial_template = generator
        .generate_template(
            "Generate a Rust service for user management with basic CRUD operations",
            vec![
                "Include REST API endpoints",
                "Use proper error handling",
                "Add database integration points",
            ],
        )
        .await?;

    println!("✅ Initial template generated");

    // Step 2: Validate the template
    println!("\n🔍 Step 2: Validating template quality...");
    let validator = TemplateValidator::new();
    let validation_result = validator.validate_template(&initial_template).await?;

    println!("📊 Validation Results:");
    println!("  Valid: {}", validation_result.is_valid);
    println!("  Quality Score: {:.2}", validation_result.quality_score);
    println!("  Issues Found: {}", validation_result.issues.len());

    for issue in &validation_result.issues {
        let severity = match issue.severity {
            ggen_ai::generators::validator::Severity::Error => "❌ ERROR",
            ggen_ai::generators::validator::Severity::Warning => "⚠️  WARNING",
            ggen_ai::generators::validator::Severity::Info => "ℹ️  INFO",
        };
        println!("  {}: {}", severity, issue.description);
    }

    println!("\n💡 Suggestions for improvement:");
    for suggestion in &validation_result.suggestions {
        println!("  • {}", suggestion);
    }

    // Step 3: Generate improved template with validation
    println!("\n🔄 Step 3: Generating improved template with validation...");
    let improved_template = generator.generate_template_with_validation(
        "Generate a comprehensive Rust user service with proper validation, error handling, and REST API",
        vec![
            "Include full CRUD operations",
            "Add proper input validation",
            "Use async/await patterns",
            "Include database integration points",
            "Add comprehensive error handling",
            "Follow Rust best practices"
        ],
        2 // Max 2 iterations
    ).await?;

    println!("✅ Improved template generated");

    // Step 4: Validate the improved template
    println!("\n🔍 Step 4: Validating improved template...");
    let improved_validation = validator.validate_template(&improved_template).await?;

    println!("📊 Improved Validation Results:");
    println!("  Valid: {}", improved_validation.is_valid);
    println!("  Quality Score: {:.2}", improved_validation.quality_score);
    println!(
        "  Improvement: +{:.2} points",
        improved_validation.quality_score - validation_result.quality_score
    );

    // Step 5: Save the final template
    println!("\n💾 Step 5: Saving final template...");

    let demo_dir = Path::new("iterative_demo_output");
    if !demo_dir.exists() {
        fs::create_dir_all(demo_dir)?;
    }

    let template_path = demo_dir.join("improved_user_service.tmpl");
    let template_content = format!(
        "{}\n---\n{}",
        improved_template.front, improved_template.body
    );
    fs::write(&template_path, template_content)?;

    println!("✅ Template saved to: {}", template_path.display());
    println!("\n📋 Template preview:");
    println!(
        "{}",
        &improved_template.body[..200.min(improved_template.body.len())]
    );
    println!("...");

    // Step 6: Demonstrate metrics
    println!("\n📊 Step 6: Quality Metrics Analysis");
    println!("==================================");

    println!("📈 Quality Metrics:");
    println!(
        "  Completeness: {:.2}",
        improved_validation.metrics.completeness
    );
    println!(
        "  Correctness: {:.2}",
        improved_validation.metrics.correctness
    );
    println!(
        "  Maintainability: {:.2}",
        improved_validation.metrics.maintainability
    );
    println!(
        "  Performance: {:.2}",
        improved_validation.metrics.performance
    );
    println!("  Security: {:.2}", improved_validation.metrics.security);
    println!(
        "  Readability: {:.2}",
        improved_validation.metrics.readability
    );

    println!("\n✅ Demo completed successfully!");
    println!("\n🎯 Key Features Demonstrated:");
    println!("  • Template validation with detailed feedback");
    println!("  • Quality assessment across multiple dimensions");
    println!("  • Iterative improvement with AI feedback");
    println!("  • Comprehensive error detection and suggestions");
    println!("  • Best practices enforcement");

    Ok(())
}
