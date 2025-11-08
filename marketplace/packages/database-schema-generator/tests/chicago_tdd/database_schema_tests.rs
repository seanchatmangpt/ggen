// ============================================================================
// Chicago TDD Test Suite for Database Schema Generator
// Real database testing with testcontainers
// ============================================================================

use testcontainers::{clients::Cli, images::postgres::Postgres, Container};
use sqlx::{PgPool, Row};
use std::time::Instant;

// ----------------------------------------------------------------------------
// Test 1: Schema Creation - PostgreSQL
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_postgresql_schema_creation() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    // Execute schema creation DDL
    let schema_ddl = include_str!("../../templates/postgresql/test_schema.sql");
    sqlx::query(schema_ddl).execute(&pool).await.unwrap();

    // Verify table creation
    let row = sqlx::query("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'public'")
        .fetch_one(&pool)
        .await
        .unwrap();

    let table_count: i64 = row.get(0);
    assert!(table_count > 0, "Schema should create at least one table");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 2: Primary Key Constraints
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_primary_key_constraints() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    // Create test table with primary key
    sqlx::query(r#"
        CREATE TABLE users (
            id SERIAL PRIMARY KEY,
            email VARCHAR(255) NOT NULL UNIQUE,
            username VARCHAR(100) NOT NULL
        );
    "#).execute(&pool).await.unwrap();

    // Insert valid record
    sqlx::query("INSERT INTO users (email, username) VALUES ($1, $2)")
        .bind("test@example.com")
        .bind("testuser")
        .execute(&pool)
        .await
        .unwrap();

    // Verify primary key auto-increment
    let row = sqlx::query("SELECT id FROM users WHERE email = $1")
        .bind("test@example.com")
        .fetch_one(&pool)
        .await
        .unwrap();

    let user_id: i32 = row.get(0);
    assert_eq!(user_id, 1, "First record should have ID 1");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 3: Foreign Key Relationships
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_foreign_key_relationships() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    // Create parent and child tables
    sqlx::query(r#"
        CREATE TABLE categories (
            id SERIAL PRIMARY KEY,
            name VARCHAR(100) NOT NULL
        );

        CREATE TABLE products (
            id SERIAL PRIMARY KEY,
            name VARCHAR(255) NOT NULL,
            category_id INTEGER NOT NULL,
            CONSTRAINT fk_category FOREIGN KEY (category_id)
                REFERENCES categories(id) ON DELETE CASCADE
        );
    "#).execute(&pool).await.unwrap();

    // Insert parent record
    let category_result = sqlx::query("INSERT INTO categories (name) VALUES ($1) RETURNING id")
        .bind("Electronics")
        .fetch_one(&pool)
        .await
        .unwrap();

    let category_id: i32 = category_result.get(0);

    // Insert child record
    sqlx::query("INSERT INTO products (name, category_id) VALUES ($1, $2)")
        .bind("Laptop")
        .bind(category_id)
        .execute(&pool)
        .await
        .unwrap();

    // Test CASCADE delete
    sqlx::query("DELETE FROM categories WHERE id = $1")
        .bind(category_id)
        .execute(&pool)
        .await
        .unwrap();

    // Verify child record was deleted
    let count: i64 = sqlx::query_scalar("SELECT COUNT(*) FROM products")
        .fetch_one(&pool)
        .await
        .unwrap();

    assert_eq!(count, 0, "Child records should be cascade deleted");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 4: Unique Constraints
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_unique_constraints() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    sqlx::query(r#"
        CREATE TABLE emails (
            id SERIAL PRIMARY KEY,
            email VARCHAR(255) NOT NULL UNIQUE
        );
    "#).execute(&pool).await.unwrap();

    // Insert first record
    sqlx::query("INSERT INTO emails (email) VALUES ($1)")
        .bind("unique@example.com")
        .execute(&pool)
        .await
        .unwrap();

    // Attempt duplicate insert (should fail)
    let result = sqlx::query("INSERT INTO emails (email) VALUES ($1)")
        .bind("unique@example.com")
        .execute(&pool)
        .await;

    assert!(result.is_err(), "Duplicate email should violate unique constraint");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 5: Check Constraints
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_check_constraints() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    sqlx::query(r#"
        CREATE TABLE products (
            id SERIAL PRIMARY KEY,
            name VARCHAR(255) NOT NULL,
            price DECIMAL(10, 2) NOT NULL,
            CONSTRAINT check_price_positive CHECK (price > 0)
        );
    "#).execute(&pool).await.unwrap();

    // Valid insert
    sqlx::query("INSERT INTO products (name, price) VALUES ($1, $2)")
        .bind("Widget")
        .bind(19.99)
        .execute(&pool)
        .await
        .unwrap();

    // Invalid insert (negative price)
    let result = sqlx::query("INSERT INTO products (name, price) VALUES ($1, $2)")
        .bind("Gadget")
        .bind(-5.00)
        .execute(&pool)
        .await;

    assert!(result.is_err(), "Negative price should violate check constraint");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 6: B-tree Index Performance
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_btree_index_performance() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    // Create table with index
    sqlx::query(r#"
        CREATE TABLE search_data (
            id SERIAL PRIMARY KEY,
            search_term VARCHAR(255) NOT NULL
        );

        CREATE INDEX idx_search_term ON search_data USING btree (search_term);
    "#).execute(&pool).await.unwrap();

    // Insert 10,000 records
    for i in 0..10_000 {
        sqlx::query("INSERT INTO search_data (search_term) VALUES ($1)")
            .bind(format!("term_{}", i))
            .execute(&pool)
            .await
            .unwrap();
    }

    // Measure query performance with index
    let start = Instant::now();
    let _row = sqlx::query("SELECT * FROM search_data WHERE search_term = $1")
        .bind("term_5000")
        .fetch_one(&pool)
        .await
        .unwrap();
    let duration = start.elapsed();

    assert!(duration.as_millis() < 10, "Indexed query should be fast (<10ms)");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 7: GIN Index for JSONB
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_gin_index_jsonb() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    // Create table with JSONB column and GIN index
    sqlx::query(r#"
        CREATE TABLE documents (
            id SERIAL PRIMARY KEY,
            data JSONB NOT NULL
        );

        CREATE INDEX idx_documents_data ON documents USING gin (data);
    "#).execute(&pool).await.unwrap();

    // Insert JSON documents
    sqlx::query("INSERT INTO documents (data) VALUES ($1)")
        .bind(r#"{"name": "John", "age": 30, "tags": ["developer", "rust"]}"#)
        .execute(&pool)
        .await
        .unwrap();

    sqlx::query("INSERT INTO documents (data) VALUES ($1)")
        .bind(r#"{"name": "Jane", "age": 25, "tags": ["designer", "ui"]}"#)
        .execute(&pool)
        .await
        .unwrap();

    // Query using JSONB operators
    let row = sqlx::query("SELECT data FROM documents WHERE data @> $1")
        .bind(r#"{"tags": ["rust"]}"#)
        .fetch_one(&pool)
        .await
        .unwrap();

    let json_data: serde_json::Value = row.get(0);
    assert_eq!(json_data["name"], "John", "Should find document with 'rust' tag");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 8: Migration Up/Down
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_migration_up_down() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    // Create migration tracking table
    sqlx::query(r#"
        CREATE TABLE schema_migrations (
            version VARCHAR(255) PRIMARY KEY,
            name VARCHAR(255) NOT NULL,
            applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
    "#).execute(&pool).await.unwrap();

    // Migration UP: Create users table
    sqlx::query(r#"
        CREATE TABLE users (
            id SERIAL PRIMARY KEY,
            username VARCHAR(100) NOT NULL
        );

        INSERT INTO schema_migrations (version, name) VALUES ('0001', 'create_users_table');
    "#).execute(&pool).await.unwrap();

    // Verify table exists
    let exists: bool = sqlx::query_scalar(
        "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'users')"
    )
    .fetch_one(&pool)
    .await
    .unwrap();

    assert!(exists, "Users table should exist after migration up");

    // Migration DOWN: Drop users table
    sqlx::query(r#"
        DROP TABLE users;
        DELETE FROM schema_migrations WHERE version = '0001';
    "#).execute(&pool).await.unwrap();

    // Verify table is dropped
    let exists: bool = sqlx::query_scalar(
        "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'users')"
    )
    .fetch_one(&pool)
    .await
    .unwrap();

    assert!(!exists, "Users table should not exist after migration down");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 9: Trigger Execution
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_trigger_execution() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    // Create table with updated_at trigger
    sqlx::query(r#"
        CREATE TABLE posts (
            id SERIAL PRIMARY KEY,
            title VARCHAR(255) NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );

        CREATE OR REPLACE FUNCTION update_updated_at()
        RETURNS TRIGGER AS $$
        BEGIN
            NEW.updated_at = CURRENT_TIMESTAMP;
            RETURN NEW;
        END;
        $$ LANGUAGE plpgsql;

        CREATE TRIGGER trigger_posts_updated_at
            BEFORE UPDATE ON posts
            FOR EACH ROW
            EXECUTE FUNCTION update_updated_at();
    "#).execute(&pool).await.unwrap();

    // Insert record
    let result = sqlx::query("INSERT INTO posts (title) VALUES ($1) RETURNING created_at")
        .bind("Test Post")
        .fetch_one(&pool)
        .await
        .unwrap();

    let created_at: chrono::NaiveDateTime = result.get(0);

    // Wait 1 second
    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;

    // Update record
    sqlx::query("UPDATE posts SET title = $1 WHERE title = $2")
        .bind("Updated Post")
        .bind("Test Post")
        .execute(&pool)
        .await
        .unwrap();

    // Verify updated_at changed
    let row = sqlx::query("SELECT updated_at FROM posts WHERE title = $1")
        .bind("Updated Post")
        .fetch_one(&pool)
        .await
        .unwrap();

    let updated_at: chrono::NaiveDateTime = row.get(0);
    assert!(updated_at > created_at, "Trigger should update updated_at timestamp");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 10: Full-text Search Index
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_fulltext_search_index() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    // Create table with full-text search
    sqlx::query(r#"
        CREATE TABLE articles (
            id SERIAL PRIMARY KEY,
            title VARCHAR(255) NOT NULL,
            content TEXT NOT NULL,
            search_vector tsvector
        );

        CREATE INDEX idx_articles_search ON articles USING gin (search_vector);

        CREATE OR REPLACE FUNCTION update_search_vector()
        RETURNS TRIGGER AS $$
        BEGIN
            NEW.search_vector = to_tsvector('english', NEW.title || ' ' || NEW.content);
            RETURN NEW;
        END;
        $$ LANGUAGE plpgsql;

        CREATE TRIGGER trigger_articles_search
            BEFORE INSERT OR UPDATE ON articles
            FOR EACH ROW
            EXECUTE FUNCTION update_search_vector();
    "#).execute(&pool).await.unwrap();

    // Insert articles
    sqlx::query("INSERT INTO articles (title, content) VALUES ($1, $2)")
        .bind("Rust Programming")
        .bind("Rust is a systems programming language focused on safety and performance.")
        .execute(&pool)
        .await
        .unwrap();

    sqlx::query("INSERT INTO articles (title, content) VALUES ($1, $2)")
        .bind("Database Design")
        .bind("Good database design is crucial for application performance.")
        .execute(&pool)
        .await
        .unwrap();

    // Search for "rust"
    let row = sqlx::query("SELECT title FROM articles WHERE search_vector @@ to_tsquery('english', 'rust')")
        .fetch_one(&pool)
        .await
        .unwrap();

    let title: String = row.get(0);
    assert_eq!(title, "Rust Programming", "Full-text search should find 'Rust Programming'");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 11: Transaction Rollback
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_transaction_rollback() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    sqlx::query("CREATE TABLE accounts (id SERIAL PRIMARY KEY, balance DECIMAL(10, 2))")
        .execute(&pool)
        .await
        .unwrap();

    // Start transaction
    let mut tx = pool.begin().await.unwrap();

    sqlx::query("INSERT INTO accounts (balance) VALUES ($1)")
        .bind(100.00)
        .execute(&mut *tx)
        .await
        .unwrap();

    // Rollback transaction
    tx.rollback().await.unwrap();

    // Verify no records inserted
    let count: i64 = sqlx::query_scalar("SELECT COUNT(*) FROM accounts")
        .fetch_one(&pool)
        .await
        .unwrap();

    assert_eq!(count, 0, "Rolled back transaction should not insert records");

    pool.close().await;
}

// ----------------------------------------------------------------------------
// Test 12: Composite Index
// ----------------------------------------------------------------------------

#[tokio::test]
async fn test_composite_index() {
    let docker = Cli::default();
    let postgres = docker.run(Postgres::default());
    let connection_string = format!(
        "postgres://postgres:postgres@localhost:{}/test_db",
        postgres.get_host_port_ipv4(5432)
    );

    let pool = PgPool::connect(&connection_string).await.unwrap();

    sqlx::query(r#"
        CREATE TABLE orders (
            id SERIAL PRIMARY KEY,
            user_id INTEGER NOT NULL,
            status VARCHAR(50) NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );

        CREATE INDEX idx_orders_user_status ON orders (user_id, status);
    "#).execute(&pool).await.unwrap();

    // Insert test data
    for i in 1..=100 {
        sqlx::query("INSERT INTO orders (user_id, status) VALUES ($1, $2)")
            .bind(i % 10)
            .bind(if i % 2 == 0 { "completed" } else { "pending" })
            .execute(&pool)
            .await
            .unwrap();
    }

    // Query using composite index
    let start = Instant::now();
    let count: i64 = sqlx::query_scalar(
        "SELECT COUNT(*) FROM orders WHERE user_id = $1 AND status = $2"
    )
    .bind(5)
    .bind("completed")
    .fetch_one(&pool)
    .await
    .unwrap();

    let duration = start.elapsed();

    assert!(count > 0, "Should find orders for user_id 5 with status 'completed'");
    assert!(duration.as_millis() < 5, "Composite index query should be fast (<5ms)");

    pool.close().await;
}
