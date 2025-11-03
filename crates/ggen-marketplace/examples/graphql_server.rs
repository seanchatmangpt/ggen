//! GraphQL API server example using Axum
//!
//! This example demonstrates how to create a production-ready GraphQL server
//! for the ggen-marketplace using async-graphql and Axum.
//!
//! Run with:
//! ```
//! cargo run --example graphql_server --features graphql-server
//! ```
//!
//! Then visit http://localhost:8000/graphql for the GraphQL playground

use async_graphql::http::{playground_source, GraphQLPlaygroundConfig};
use async_graphql_axum::{GraphQLRequest, GraphQLResponse};
use axum::{
    extract::State,
    response::{Html, IntoResponse},
    routing::{get, post},
    Router,
};
use ggen_marketplace::{backend::LocalRegistry, graphql::create_schema, storage::MemoryStore};
use std::sync::Arc;
use tower_http::cors::{Any, CorsLayer};

/// Application state containing the GraphQL schema
struct AppState {
    schema: ggen_marketplace::graphql::MarketplaceSchema,
}

/// GraphQL query handler
async fn graphql_handler(
    State(state): State<Arc<AppState>>, req: GraphQLRequest,
) -> GraphQLResponse {
    state.schema.execute(req.into_inner()).await.into()
}

/// GraphQL playground handler (development only)
async fn graphql_playground() -> impl IntoResponse {
    Html(playground_source(GraphQLPlaygroundConfig::new("/graphql")))
}

/// Health check endpoint
async fn health_check() -> impl IntoResponse {
    "OK"
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("🚀 Initializing ggen-marketplace GraphQL server...");

    // Create storage and registry
    let store = Arc::new(MemoryStore::new());
    let registry = Arc::new(LocalRegistry::new(store.clone()));

    // Seed with example data (optional)
    seed_example_data(registry.clone()).await?;

    // Create GraphQL schema
    let schema = create_schema(
        registry.clone() as Arc<dyn ggen_marketplace::traits::Registry>,
        store.clone() as Arc<dyn ggen_marketplace::traits::PackageStore>,
    );

    // Create application state
    let state = Arc::new(AppState { schema });

    // Configure CORS
    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods(Any)
        .allow_headers(Any);

    // Build router
    let app = Router::new()
        .route("/graphql", post(graphql_handler))
        .route("/graphql", get(graphql_playground))
        .route("/health", get(health_check))
        .layer(cors)
        .with_state(state);

    // Bind and serve
    let addr = "0.0.0.0:8000";
    println!("✅ Server ready!");
    println!("📊 GraphQL Playground: http://localhost:8000/graphql");
    println!("🏥 Health check: http://localhost:8000/health");
    println!();
    println!("Example queries:");
    println!("  - Search: query {{ search(query: \"web\") {{ id name version }} }}");
    println!(
        "  - Package: query {{ package(namespace: \"rust\", name: \"example\") {{ id title }} }}"
    );
    println!();

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

/// Seed the registry with example packages
async fn seed_example_data(registry: Arc<LocalRegistry<MemoryStore>>) -> anyhow::Result<()> {
    use ggen_marketplace::models::{
        Category, ContentId, HashAlgorithm, Package, PackageId, PackageMetadata, PackageStats,
        Version,
    };

    println!("📦 Seeding example packages...");

    let packages = vec![
        (
            ("rust", "web-framework"),
            "1.0.0",
            "Rust Web Framework",
            "A modern web framework for Rust",
            Category::WebFramework,
        ),
        (
            ("rust", "cli-builder"),
            "2.5.1",
            "CLI Builder",
            "Build beautiful command-line interfaces",
            Category::CLI,
        ),
        (
            ("python", "data-tools"),
            "0.8.0",
            "Data Analysis Tools",
            "Comprehensive data science utilities",
            Category::DataScience,
        ),
        (
            ("js", "react-components"),
            "3.2.0",
            "React Components Library",
            "Reusable React UI components",
            Category::WebFramework,
        ),
    ];

    for (id_parts, version_str, title, description, category) in packages {
        let id = PackageId::new(id_parts.0, id_parts.1);
        let version = Version::parse(version_str)?;
        let content_id = ContentId::new(format!("example_{}", id_parts.1), HashAlgorithm::Sha256);

        let mut metadata = PackageMetadata::default();
        metadata.title = title.to_string();
        metadata.description = description.to_string();
        metadata.license = "MIT".to_string();
        metadata.tags = vec!["example".to_string()];
        metadata.categories = vec![category];

        let package = Package {
            id,
            version,
            metadata,
            content_id,
            dependencies: vec![],
            stats: PackageStats::default(),
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        };

        registry.publish(package).await?;
    }

    println!("✅ Seeded {} example packages", packages.len());
    Ok(())
}
