//! Database repository layer for all operations

use chrono::Utc;
use sqlx::SqlitePool;
use uuid::Uuid;

use crate::error::{ApiError, ApiResult};

use super::models::*;

/// User repository
pub struct UserRepository;

impl UserRepository {
    pub async fn create(
        pool: &SqlitePool,
        email: &str,
        username: &str,
        password_hash: &str,
    ) -> ApiResult<User> {
        let user_id = Uuid::new_v4().to_string();
        let now = Utc::now().to_rfc3339();

        sqlx::query(
            r#"
            INSERT INTO users (id, email, username, password_hash, tier, created_at, updated_at)
            VALUES (?, ?, ?, ?, 'free', ?, ?)
            "#,
        )
        .bind(&user_id)
        .bind(email)
        .bind(username)
        .bind(password_hash)
        .bind(&now)
        .bind(&now)
        .execute(pool)
        .await
        .map_err(|e| ApiError::BadRequest(format!("Failed to create user: {}", e)))?;

        Self::get_by_id(pool, &user_id).await
    }

    pub async fn get_by_email(pool: &SqlitePool, email: &str) -> ApiResult<Option<User>> {
        sqlx::query_as::<_, User>("SELECT * FROM users WHERE email = ?")
            .bind(email)
            .fetch_optional(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Database error: {}", e)))
    }

    pub async fn get_by_id(pool: &SqlitePool, user_id: &str) -> ApiResult<User> {
        sqlx::query_as::<_, User>("SELECT * FROM users WHERE id = ?")
            .bind(user_id)
            .fetch_one(pool)
            .await
            .map_err(|_| ApiError::NotFound("User not found".to_string()))
    }

    pub async fn update_tier(
        pool: &SqlitePool,
        user_id: &str,
        tier: &str,
    ) -> ApiResult<User> {
        let now = Utc::now().to_rfc3339();
        sqlx::query("UPDATE users SET tier = ?, updated_at = ? WHERE id = ?")
            .bind(tier)
            .bind(&now)
            .bind(user_id)
            .execute(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Failed to update tier: {}", e)))?;

        Self::get_by_id(pool, user_id).await
    }

    pub async fn set_stripe_customer(
        pool: &SqlitePool,
        user_id: &str,
        stripe_customer_id: &str,
    ) -> ApiResult<()> {
        sqlx::query("UPDATE users SET stripe_customer_id = ? WHERE id = ?")
            .bind(stripe_customer_id)
            .bind(user_id)
            .execute(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Database error: {}", e)))?;

        Ok(())
    }
}

/// API Key repository
pub struct ApiKeyRepository;

impl ApiKeyRepository {
    pub async fn create(
        pool: &SqlitePool,
        user_id: &str,
        key_hash: &str,
        name: &str,
        expires_at: Option<&str>,
    ) -> ApiResult<ApiKey> {
        let key_id = Uuid::new_v4().to_string();
        let now = Utc::now().to_rfc3339();

        sqlx::query(
            r#"
            INSERT INTO api_keys (id, user_id, key_hash, name, expires_at, created_at)
            VALUES (?, ?, ?, ?, ?, ?)
            "#,
        )
        .bind(&key_id)
        .bind(user_id)
        .bind(key_hash)
        .bind(name)
        .bind(expires_at)
        .bind(&now)
        .execute(pool)
        .await
        .map_err(|e| ApiError::BadRequest(format!("Failed to create API key: {}", e)))?;

        Self::get_by_id(pool, &key_id).await
    }

    pub async fn get_by_id(pool: &SqlitePool, key_id: &str) -> ApiResult<ApiKey> {
        sqlx::query_as::<_, ApiKey>("SELECT * FROM api_keys WHERE id = ?")
            .bind(key_id)
            .fetch_one(pool)
            .await
            .map_err(|_| ApiError::NotFound("API key not found".to_string()))
    }

    pub async fn get_by_hash(pool: &SqlitePool, key_hash: &str) -> ApiResult<Option<ApiKey>> {
        sqlx::query_as::<_, ApiKey>("SELECT * FROM api_keys WHERE key_hash = ? AND active = 1")
            .bind(key_hash)
            .fetch_optional(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Database error: {}", e)))
    }

    pub async fn list_by_user(pool: &SqlitePool, user_id: &str) -> ApiResult<Vec<ApiKey>> {
        sqlx::query_as::<_, ApiKey>("SELECT * FROM api_keys WHERE user_id = ? ORDER BY created_at DESC")
            .bind(user_id)
            .fetch_all(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Database error: {}", e)))
    }

    pub async fn revoke(pool: &SqlitePool, key_id: &str) -> ApiResult<()> {
        sqlx::query("UPDATE api_keys SET active = 0 WHERE id = ?")
            .bind(key_id)
            .execute(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Failed to revoke key: {}", e)))?;

        Ok(())
    }

    pub async fn record_usage(pool: &SqlitePool, key_id: &str) -> ApiResult<()> {
        let now = Utc::now().to_rfc3339();
        sqlx::query("UPDATE api_keys SET last_used = ? WHERE id = ?")
            .bind(&now)
            .bind(key_id)
            .execute(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Database error: {}", e)))?;

        Ok(())
    }
}

/// Subscription repository
pub struct SubscriptionRepository;

impl SubscriptionRepository {
    pub async fn create(
        pool: &SqlitePool,
        user_id: &str,
        tier: &str,
    ) -> ApiResult<Subscription> {
        let sub_id = Uuid::new_v4().to_string();
        let now = Utc::now();
        let period_end = (now + chrono::Duration::days(30)).to_rfc3339();
        let now_str = now.to_rfc3339();

        sqlx::query(
            r#"
            INSERT INTO subscriptions (id, user_id, tier, status, current_period_start, current_period_end, created_at)
            VALUES (?, ?, ?, 'active', ?, ?, ?)
            "#,
        )
        .bind(&sub_id)
        .bind(user_id)
        .bind(tier)
        .bind(&now_str)
        .bind(&period_end)
        .bind(&now_str)
        .execute(pool)
        .await
        .map_err(|e| ApiError::BadRequest(format!("Failed to create subscription: {}", e)))?;

        Self::get_by_user_id(pool, user_id).await
    }

    pub async fn get_by_user_id(pool: &SqlitePool, user_id: &str) -> ApiResult<Subscription> {
        sqlx::query_as::<_, Subscription>("SELECT * FROM subscriptions WHERE user_id = ?")
            .bind(user_id)
            .fetch_one(pool)
            .await
            .map_err(|_| ApiError::NotFound("Subscription not found".to_string()))
    }

    pub async fn update_tier(
        pool: &SqlitePool,
        user_id: &str,
        tier: &str,
    ) -> ApiResult<Subscription> {
        sqlx::query("UPDATE subscriptions SET tier = ? WHERE user_id = ?")
            .bind(tier)
            .bind(user_id)
            .execute(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Failed to update subscription: {}", e)))?;

        Self::get_by_user_id(pool, user_id).await
    }

    pub async fn set_stripe_id(
        pool: &SqlitePool,
        user_id: &str,
        stripe_sub_id: &str,
    ) -> ApiResult<()> {
        sqlx::query("UPDATE subscriptions SET stripe_subscription_id = ? WHERE user_id = ?")
            .bind(stripe_sub_id)
            .bind(user_id)
            .execute(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Database error: {}", e)))?;

        Ok(())
    }
}

/// Payment repository
pub struct PaymentRepository;

impl PaymentRepository {
    pub async fn create(
        pool: &SqlitePool,
        user_id: &str,
        amount_cents: i64,
        description: &str,
    ) -> ApiResult<Payment> {
        let payment_id = Uuid::new_v4().to_string();
        let now = Utc::now().to_rfc3339();

        sqlx::query(
            r#"
            INSERT INTO payments (id, user_id, amount_cents, status, description, created_at)
            VALUES (?, ?, ?, 'pending', ?, ?)
            "#,
        )
        .bind(&payment_id)
        .bind(user_id)
        .bind(amount_cents)
        .bind(description)
        .bind(&now)
        .execute(pool)
        .await
        .map_err(|e| ApiError::BadRequest(format!("Failed to create payment: {}", e)))?;

        Self::get_by_id(pool, &payment_id).await
    }

    pub async fn get_by_id(pool: &SqlitePool, payment_id: &str) -> ApiResult<Payment> {
        sqlx::query_as::<_, Payment>("SELECT * FROM payments WHERE id = ?")
            .bind(payment_id)
            .fetch_one(pool)
            .await
            .map_err(|_| ApiError::NotFound("Payment not found".to_string()))
    }

    pub async fn update_status(
        pool: &SqlitePool,
        payment_id: &str,
        status: &str,
        stripe_id: Option<&str>,
    ) -> ApiResult<()> {
        sqlx::query("UPDATE payments SET status = ?, stripe_payment_id = ? WHERE id = ?")
            .bind(status)
            .bind(stripe_id)
            .bind(payment_id)
            .execute(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Failed to update payment: {}", e)))?;

        Ok(())
    }

    pub async fn list_by_user(pool: &SqlitePool, user_id: &str) -> ApiResult<Vec<Payment>> {
        sqlx::query_as::<_, Payment>(
            "SELECT * FROM payments WHERE user_id = ? ORDER BY created_at DESC LIMIT 50",
        )
        .bind(user_id)
        .fetch_all(pool)
        .await
        .map_err(|e| ApiError::InternalError(format!("Database error: {}", e)))
    }
}

/// Usage event repository
pub struct UsageRepository;

impl UsageRepository {
    pub async fn record(
        pool: &SqlitePool,
        user_id: &str,
        operation: &str,
        cost: i64,
    ) -> ApiResult<()> {
        let event_id = Uuid::new_v4().to_string();
        let now = Utc::now().to_rfc3339();

        sqlx::query(
            r#"
            INSERT INTO usage_events (id, user_id, operation, cost, created_at)
            VALUES (?, ?, ?, ?, ?)
            "#,
        )
        .bind(&event_id)
        .bind(user_id)
        .bind(operation)
        .bind(cost)
        .bind(&now)
        .execute(pool)
        .await
        .map_err(|e| ApiError::InternalError(format!("Failed to record usage: {}", e)))?;

        Ok(())
    }

    pub async fn get_month_usage(pool: &SqlitePool, user_id: &str) -> ApiResult<u64> {
        let thirty_days_ago = (Utc::now() - chrono::Duration::days(30)).to_rfc3339();

        let result: (i64,) = sqlx::query_as(
            "SELECT COUNT(*) as count FROM usage_events WHERE user_id = ? AND created_at > ?",
        )
        .bind(user_id)
        .bind(&thirty_days_ago)
        .fetch_one(pool)
        .await
        .map_err(|e| ApiError::InternalError(format!("Database error: {}", e)))?;

        Ok(result.0 as u64)
    }
}

/// Webhook event repository
pub struct WebhookRepository;

impl WebhookRepository {
    pub async fn create(
        pool: &SqlitePool,
        stripe_event_id: &str,
        event_type: &str,
        data: &str,
    ) -> ApiResult<WebhookEvent> {
        let webhook_id = Uuid::new_v4().to_string();
        let now = Utc::now().to_rfc3339();

        sqlx::query(
            r#"
            INSERT INTO webhook_events (id, stripe_event_id, event_type, data, status, created_at)
            VALUES (?, ?, ?, ?, 'pending', ?)
            "#,
        )
        .bind(&webhook_id)
        .bind(stripe_event_id)
        .bind(event_type)
        .bind(data)
        .bind(&now)
        .execute(pool)
        .await
        .map_err(|e| ApiError::BadRequest(format!("Failed to create webhook event: {}", e)))?;

        Self::get_by_id(pool, &webhook_id).await
    }

    pub async fn get_by_id(pool: &SqlitePool, webhook_id: &str) -> ApiResult<WebhookEvent> {
        sqlx::query_as::<_, WebhookEvent>("SELECT * FROM webhook_events WHERE id = ?")
            .bind(webhook_id)
            .fetch_one(pool)
            .await
            .map_err(|_| ApiError::NotFound("Webhook event not found".to_string()))
    }

    pub async fn mark_processed(pool: &SqlitePool, webhook_id: &str) -> ApiResult<()> {
        sqlx::query("UPDATE webhook_events SET status = 'processed' WHERE id = ?")
            .bind(webhook_id)
            .execute(pool)
            .await
            .map_err(|e| ApiError::InternalError(format!("Database error: {}", e)))?;

        Ok(())
    }

    pub async fn get_pending(pool: &SqlitePool) -> ApiResult<Vec<WebhookEvent>> {
        sqlx::query_as::<_, WebhookEvent>(
            "SELECT * FROM webhook_events WHERE status = 'pending' ORDER BY created_at ASC LIMIT 100",
        )
        .fetch_all(pool)
        .await
        .map_err(|e| ApiError::InternalError(format!("Database error: {}", e)))
    }
}
