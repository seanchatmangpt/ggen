pub mod item_repo;

pub use item_repo::SqliteItemRepository;
use sqlx::SqlitePool;

/// Create the SQLite pool and run pending migrations.
pub async fn init(database_url: &str) -> anyhow::Result<SqlitePool> {
    let pool = SqlitePool::connect(database_url).await?;
    sqlx::migrate!("./migrations").run(&pool).await?;
    Ok(pool)
}
