use actix_cors::Cors;
use actix_web::{middleware, web, App, HttpServer};
use anyhow::Result;
use sqlx::postgres::PgPoolOptions;
use std::sync::Arc;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

mod db;
mod middleware as custom_middleware;
mod models;
mod routes;

use utoipa::OpenApi;
use utoipa_swagger_ui::SwaggerUi;

/// Application state shared across handlers
pub struct AppState {
    pub db: sqlx::PgPool,
    pub redis: redis::aio::ConnectionManager,
    pub jwt_secret: String,
}

#[derive(OpenApi)]
#[openapi(
    paths(
        routes::health::health_check,
        routes::health::metrics,
        routes::users::get_users,
        routes::users::get_user,
        routes::users::create_user,
        routes::users::update_user,
        routes::users::delete_user,
        routes::auth::login,
        routes::auth::register,
    ),
    components(
        schemas(
            models::User,
            models::CreateUserRequest,
            models::UpdateUserRequest,
            models::LoginRequest,
            models::RegisterRequest,
            models::AuthResponse,
            models::ErrorResponse,
        )
    ),
    tags(
        (name = "health", description = "Health and metrics endpoints"),
        (name = "users", description = "User management endpoints"),
        (name = "auth", description = "Authentication endpoints")
    )
)]
struct ApiDoc;

#[actix_web::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "async_web_service=debug,actix_web=info".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    // Load environment variables
    dotenv::dotenv().ok();

    // Database connection
    let database_url = std::env::var("DATABASE_URL")
        .expect("DATABASE_URL must be set");

    let db_pool = PgPoolOptions::new()
        .max_connections(5)
        .connect(&database_url)
        .await?;

    // Run migrations
    sqlx::migrate!("./migrations")
        .run(&db_pool)
        .await?;

    // Redis connection
    let redis_url = std::env::var("REDIS_URL")
        .unwrap_or_else(|_| "redis://127.0.0.1:6379".to_string());

    let redis_client = redis::Client::open(redis_url)?;
    let redis_conn = redis::aio::ConnectionManager::new(redis_client).await?;

    // JWT secret
    let jwt_secret = std::env::var("JWT_SECRET")
        .unwrap_or_else(|_| "your-secret-key-change-in-production".to_string());

    // Create shared state
    let app_state = Arc::new(AppState {
        db: db_pool,
        redis: redis_conn,
        jwt_secret,
    });

    let bind_address = std::env::var("BIND_ADDRESS")
        .unwrap_or_else(|_| "127.0.0.1:8080".to_string());

    tracing::info!("Starting server on {}", bind_address);

    // Start HTTP server
    HttpServer::new(move || {
        let cors = Cors::default()
            .allow_any_origin()
            .allow_any_method()
            .allow_any_header()
            .max_age(3600);

        App::new()
            .app_data(web::Data::new(app_state.clone()))
            .wrap(middleware::Logger::default())
            .wrap(middleware::Compress::default())
            .wrap(cors)
            .wrap(tracing_actix_web::TracingLogger::default())
            .configure(routes::configure)
            .service(
                SwaggerUi::new("/swagger-ui/{_:.*}")
                    .url("/api-docs/openapi.json", ApiDoc::openapi()),
            )
    })
    .bind(bind_address)?
    .run()
    .await?;

    Ok(())
}
