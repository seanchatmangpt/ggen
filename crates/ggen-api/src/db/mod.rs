//! Database layer for user, subscription, and payment operations

pub mod models;
pub mod schema;
pub mod repository;

pub use models::*;
pub use repository::*;

use sqlx::sqlite::{SqlitePool, SqlitePoolOptions};
use std::path::Path;

/// Initialize SQLite database
pub async fn init_db(database_url: &str) -> Result<SqlitePool, sqlx::Error> {
    let pool = SqlitePoolOptions::new()
        .max_connections(5)
        .connect(database_url)
        .await?;

    // Run migrations (schema)
    schema::init(&pool).await?;

    Ok(pool)
}
