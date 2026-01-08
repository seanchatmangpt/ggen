<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [80/20 Implementation Checklist: 6 Weeks to Revenue](#8020-implementation-checklist-6-weeks-to-revenue)
  - [Week 1-2: Database & Repository (Status: 70% Done)](#week-1-2-database--repository-status-70-done)
    - [✅ Completed](#-completed)
    - [⏳ To Complete](#-to-complete)
    - [Commands](#commands)
  - [Week 2-3: Wire Auth Handlers (Status: 40% Done)](#week-2-3-wire-auth-handlers-status-40-done)
    - [Current State](#current-state)
    - [Step-by-Step](#step-by-step)
    - [Test](#test)
  - [Week 3: Integrate Handlers with Database (Status: 30% Done)](#week-3-integrate-handlers-with-database-status-30-done)
    - [Handlers to Update](#handlers-to-update)
    - [Pattern](#pattern)
    - [Test](#test-1)
  - [Week 3-5: Payment Integration (Status: 10% Done)](#week-3-5-payment-integration-status-10-done)
    - [Prerequisites](#prerequisites)
    - [Implementation](#implementation)
    - [Test](#test-2)
  - [Week 4: React Dashboard (Status: 0% Done)](#week-4-react-dashboard-status-0-done)
    - [Setup](#setup)
    - [Key Pages to Create](#key-pages-to-create)
    - [Template (Dashboard)](#template-dashboard)
  - [Week 5-6: Monitoring & Production (Status: 0% Done)](#week-5-6-monitoring--production-status-0-done)
    - [Monitoring Setup](#monitoring-setup)
    - [Production Checklist](#production-checklist)
  - [Validation Points](#validation-points)
    - [After Each Week](#after-each-week)
  - [Quick Reference: API Patterns](#quick-reference-api-patterns)
    - [All handlers follow this pattern:](#all-handlers-follow-this-pattern)
  - [Environment Variables](#environment-variables)
  - [Success Criteria](#success-criteria)
  - [Revenue at Launch](#revenue-at-launch)
  - [Troubleshooting](#troubleshooting)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 80/20 Implementation Checklist: 6 Weeks to Revenue

**Follow this checklist to activate all 7 revenue streams in production.**

---

## Week 1-2: Database & Repository (Status: 70% Done)

### ✅ Completed
- [x] Database schema (7 tables)
- [x] SQLite initialization
- [x] User repository
- [x] API key repository
- [x] Subscription repository
- [x] Payment repository
- [x] Usage repository
- [x] Webhook repository

### ⏳ To Complete
- [ ] Update `Cargo.toml` with sqlx and bcrypt dependencies
- [ ] Update `src/lib.rs` to expose `db` module
- [ ] Update `src/state.rs` to add `pool: Arc<SqlitePool>`
- [ ] Create `.env.example` with DATABASE_URL
- [ ] Test: `cargo test --lib ggen_api`

### Commands
```bash
# Install sqlx CLI (optional, for migrations)
cargo install sqlx-cli --no-default-features --features sqlite

# Create database
sqlx database create

# Run migrations (if using separate migrations folder)
sqlx migrate run
```

---

## Week 2-3: Wire Auth Handlers (Status: 40% Done)

### Current State
- API handlers exist but don't use database
- Repository layer exists but not called
- JWT manager exists but not in AppState

### Step-by-Step

**Step 1: Update AppState** (`src/state.rs`)
```rust
use sqlx::SqlitePool;
use ggen_auth::JwtManager;
use ggen_payments::StripeClient;
use ggen_saas::QuotaManager;
use std::sync::Arc;

#[derive(Clone)]
pub struct AppState {
    // Add these fields:
    pub pool: Arc<SqlitePool>,
    pub jwt_manager: Arc<JwtManager>,
    pub stripe_client: Arc<StripeClient>,
    pub quota_manager: Arc<QuotaManager>,
    // Keep existing fields
    pub marketplace: Arc<Marketplace>,
    pub config: Arc<ApiConfig>,
    pub telemetry: Arc<TelemetryConfig>,
}
```

**Step 2: Implement register handler** (`src/handlers/auth.rs`)
```rust
use bcrypt::{hash, verify};
use crate::db::{UserRepository, SubscriptionRepository};

pub async fn register(
    State(state): State<AppState>,
    Json(req): Json<RegisterRequest>,
) -> ApiResult<(StatusCode, Json<LoginResponse>)> {
    // Validate
    req.validate()?;

    // Check if exists
    if UserRepository::get_by_email(&state.pool, &req.email).await?.is_some() {
        return Err(ApiError::Conflict("User already exists".to_string()));
    }

    // Hash password
    let password_hash = hash(&req.password, 12)?;

    // Create user
    let user = UserRepository::create(
        &state.pool,
        &req.email,
        &req.username,
        &password_hash,
    ).await?;

    // Create free tier subscription
    SubscriptionRepository::create(&state.pool, &user.id, "free").await?;

    // Generate token
    let token = state.jwt_manager.generate_token(&user.id, &user.email, "free")?;

    Ok((
        StatusCode::CREATED,
        Json(LoginResponse {
            token,
            user_id: user.id,
            username: user.username,
            email: user.email,
            tier: "free".to_string(),
            expires_in_secs: 86400 * 30,
        }),
    ))
}
```

**Step 3: Implement login handler** (`src/handlers/auth.rs`)
```rust
pub async fn login(
    State(state): State<AppState>,
    Json(req): Json<LoginRequest>,
) -> ApiResult<Json<LoginResponse>> {
    // Validate
    req.validate()?;

    // Get user
    let user = UserRepository::get_by_email(&state.pool, &req.email).await?
        .ok_or_else(|| ApiError::Unauthorized("Invalid credentials".to_string()))?;

    // Verify password
    if !verify(&req.password, &user.password_hash)? {
        return Err(ApiError::Unauthorized("Invalid credentials".to_string()));
    }

    // Generate token
    let token = state.jwt_manager.generate_token(&user.id, &user.email, &user.tier)?;

    Ok(Json(LoginResponse {
        token,
        user_id: user.id,
        username: user.username,
        email: user.email,
        tier: user.tier,
        expires_in_secs: 86400 * 30,
    }))
}
```

**Step 4: Create main.rs** (`ggen-api-server/src/main.rs`)
```rust
use ggen_api::{init_api, init_db, ApiConfig, AppState};
use ggen_auth::JwtManager;
use ggen_payments::{StripeClient, StripeConfig};
use ggen_saas::QuotaManager;
use sqlx::sqlite::SqlitePool;
use std::sync::Arc;
use tokio::net::TcpListener;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Load env
    dotenvy::dotenv().ok();
    tracing_subscriber::fmt::init();

    // Config
    let config = ApiConfig {
        host: std::env::var("API_HOST").unwrap_or_else(|_| "127.0.0.1".to_string()),
        port: std::env::var("API_PORT")
            .unwrap_or_else(|_| "3000".to_string())
            .parse()?,
        jwt_secret: std::env::var("JWT_SECRET")?,
        stripe_key: std::env::var("STRIPE_API_KEY").ok(),
        base_url: std::env::var("BASE_URL").unwrap_or_else(|_| "http://localhost:3000".to_string()),
        ..Default::default()
    };

    // Database
    let database_url = std::env::var("DATABASE_URL").unwrap_or_else(|_| "sqlite:ggen.db".to_string());
    let pool = init_db(&database_url).await?;

    // Services
    let jwt_manager = JwtManager::new(config.jwt_secret.clone(), 86400 * 30);
    let stripe_config = StripeConfig {
        api_key: config.stripe_key.clone().unwrap_or_default(),
        webhook_secret: std::env::var("STRIPE_WEBHOOK_SECRET").ok(),
    };
    let stripe_client = StripeClient::new(stripe_config);
    let quota_manager = QuotaManager::new(300);

    // State
    let state = AppState {
        pool: Arc::new(pool),
        config: Arc::new(config.clone()),
        jwt_manager: Arc::new(jwt_manager),
        stripe_client: Arc::new(stripe_client),
        quota_manager: Arc::new(quota_manager),
        marketplace: Arc::new(ggen_marketplace::Marketplace::new()),
        telemetry: Arc::new(ggen_core::telemetry::TelemetryConfig::default()),
    };

    // Router
    let app = init_api(state).await;

    // Server
    let listener = TcpListener::bind(format!("{}:{}", config.host, config.port)).await?;
    tracing::info!("Server listening on {}:{}", config.host, config.port);

    axum::serve(listener, app).await?;
    Ok(())
}
```

### Test
```bash
# Create database
sqlite3 ggen.db < schema.sql

# Run server
RUST_LOG=debug DATABASE_URL="sqlite:ggen.db" JWT_SECRET="test-secret" cargo run

# Test registration
curl -X POST http://localhost:3000/api/v1/auth/register \
  -H "Content-Type: application/json" \
  -d '{"email":"user@example.com","username":"testuser","password":"Password123"}'

# Test login
curl -X POST http://localhost:3000/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email":"user@example.com","password":"Password123"}'
```

---

## Week 3: Integrate Handlers with Database (Status: 30% Done)

### Handlers to Update
- [ ] `billing.rs` - record_usage, get_usage_stats
- [ ] `marketplace.rs` - purchase_package with quota check
- [ ] `auth.rs` - API key create/list/revoke

### Pattern
```rust
// Every handler needs:
// 1. Get user from JWT (from middleware)
// 2. Check quota (if applicable)
// 3. Query database
// 4. Record usage
// 5. Return response
```

### Test
```bash
cargo test --lib ggen_api::handlers
```

---

## Week 3-5: Payment Integration (Status: 10% Done)

### Prerequisites
- [ ] Stripe account created (https://dashboard.stripe.com)
- [ ] Test API keys obtained
- [ ] Webhook endpoint configured

### Implementation

**Step 1: Add Stripe webhook handler**
```rust
pub async fn handle_stripe_webhook(
    State(state): State<AppState>,
    headers: HeaderMap,
    body: String,
) -> ApiResult<StatusCode> {
    // Get signature
    let signature = headers
        .get("stripe-signature")
        .and_then(|h| h.to_str().ok())
        .ok_or_else(|| ApiError::Unauthorized("Missing signature".to_string()))?;

    // Verify
    state.stripe_client.verify_webhook(&body, signature)?;

    // Store event
    let event: serde_json::Value = serde_json::from_str(&body)?;
    WebhookRepository::create(
        &state.pool,
        event["id"].as_str().unwrap_or_default(),
        event["type"].as_str().unwrap_or_default(),
        &body,
    ).await?;

    // Process based on type
    match event["type"].as_str().unwrap_or("") {
        "payment_intent.succeeded" => {
            // Update payment status
            PaymentRepository::update_status(
                &state.pool,
                &payment_id,
                "succeeded",
                Some(stripe_id),
            ).await?;
        }
        _ => {}
    }

    Ok(StatusCode::OK)
}
```

**Step 2: Implement purchase flow**
```rust
pub async fn purchase_package(
    State(state): State<AppState>,
    headers: HeaderMap,
    Json(req): Json<PurchaseRequest>,
) -> ApiResult<(StatusCode, Json<PurchaseResponse>)> {
    // Extract user
    let user = extract_user_from_headers(&headers)?;

    // Get package
    let package = state.marketplace.get_package(&req.package_id).await?;

    // Check quota
    let limits = state.quota_manager.get_limits(&user.id, "marketplace_templates").await?;
    if limits.is_exceeded() {
        return Err(ApiError::QuotaExceeded {
            resource: "templates".to_string(),
            used: limits.used,
            limit: limits.limit,
        });
    }

    // Create payment
    let payment = PaymentRepository::create(
        &state.pool,
        &user.id,
        (package.price * 100.0) as i64,
        &format!("Package: {}", package.name),
    ).await?;

    // Create Stripe payment intent
    let payment_intent = state.stripe_client.create_payment_intent(
        &user.id,
        (package.price * 100.0) as i64,
        "usd",
        &format!("Package: {}", package.name),
    ).await?;

    // Record usage
    state.quota_manager.record_usage(&user.id, "template_install", 1).await?;

    Ok((
        StatusCode::CREATED,
        Json(PurchaseResponse {
            transaction_id: payment.id,
            package_id: req.package_id,
            price: package.price,
            status: "pending".to_string(),
            payment_url: payment_intent.client_secret.map(|s| {
                format!("{}/checkout/{}", state.config.base_url, s)
            }),
        }),
    ))
}
```

### Test
```bash
# Set Stripe keys
export STRIPE_API_KEY="sk_test_..."
export STRIPE_WEBHOOK_SECRET="whsec_..."

# Test payment creation
curl -X POST http://localhost:3000/api/v1/marketplace/packages/test/purchase \
  -H "Authorization: Bearer <TOKEN>" \
  -H "Content-Type: application/json" \
  -d '{"package_id":"test","version":"1.0.0"}'

# Test webhook (using Stripe CLI)
stripe listen --forward-to localhost:3000/api/v1/webhooks/stripe
stripe trigger payment_intent.succeeded
```

---

## Week 4: React Dashboard (Status: 0% Done)

### Setup
```bash
npm create vite@latest ggen-ui -- --template react-ts
cd ggen-ui && npm install
npm install -D tailwindcss postcss autoprefixer
npx tailwindcss init -p
```

### Key Pages to Create
- [ ] `src/pages/LoginPage.tsx`
- [ ] `src/pages/DashboardPage.tsx`
- [ ] `src/pages/ApiKeysPage.tsx`
- [ ] `src/pages/BillingPage.tsx`

### Template (Dashboard)
```tsx
export function DashboardPage() {
  const [user, setUser] = useState(null);
  const [usage, setUsage] = useState(null);

  useEffect(() => {
    fetchUserData();
    fetchUsageData();
  }, []);

  return (
    <div className="p-8">
      <h1>Welcome, {user?.username}</h1>
      <div className="grid grid-cols-2 gap-4">
        <Card title="API Calls" value={`${usage?.api_calls_used} / ${usage?.api_calls_limit}`} />
        <Card title="Templates" value={`${usage?.templates_used} / ${usage?.templates_limit}`} />
      </div>
    </div>
  );
}
```

---

## Week 5-6: Monitoring & Production (Status: 0% Done)

### Monitoring Setup
- [ ] Enable Prometheus metrics endpoint
- [ ] Setup Grafana dashboard
- [ ] Configure alerts for errors/quota exceeded
- [ ] Add logging to all handlers

### Production Checklist
- [ ] Switch to PostgreSQL
- [ ] Add Redis for caching
- [ ] Setup SSL certificates
- [ ] Configure CDN (Cloudflare)
- [ ] Add email notifications
- [ ] Setup backups
- [ ] Enable audit logging

---

## Validation Points

### After Each Week
```bash
# Week 1-2
sqlite3 ggen.db ".tables"  # Should see all 7 tables

# Week 2-3
curl http://localhost:3000/api/v1/health  # Should return 200

# Week 3
curl -X POST http://localhost:3000/api/v1/auth/register \
  -H "Content-Type: application/json" \
  -d '{"email":"test@example.com","username":"test","password":"Password123"}'

# Week 3-5
curl http://localhost:3000/api/v1/webhooks/stripe  # Stripe webhooks working

# Week 4
npm run dev  # Dashboard loads at localhost:5173

# Week 5-6
curl http://localhost:3000/metrics  # Prometheus metrics available
```

---

## Quick Reference: API Patterns

### All handlers follow this pattern:
```rust
pub async fn <handler_name>(
    State(state): State<AppState>,          // App state with DB, services
    headers: HeaderMap,                     // For JWT extraction
    Json(req): Json<RequestType>,           // Request body
) -> ApiResult<ResponseType> {              // Returns ApiResult
    // 1. Validate request
    req.validate()?;

    // 2. Extract user from JWT
    let user = extract_user_from_headers(&headers)?;

    // 3. Check quota (if applicable)
    state.quota_manager.check_quota(&user.id, "operation", 1).await?;

    // 4. Database operations
    let result = Repository::create(&state.pool, ...args).await?;

    // 5. Stripe operations (if payment)
    let stripe_result = state.stripe_client.create_payment_intent(...).await?;

    // 6. Record usage
    UsageRepository::record(&state.pool, &user.id, "operation", cost).await?;
    state.quota_manager.record_usage(&user.id, "operation", 1).await?;

    // 7. Return response
    Ok(Json(ResponseType { ...result }))
}
```

---

## Environment Variables

Create `.env`:
```bash
# API
API_HOST=127.0.0.1
API_PORT=3000
JWT_SECRET=your-secret-key-change-in-prod
BASE_URL=http://localhost:3000

# Database
DATABASE_URL=sqlite:ggen.db

# Stripe
STRIPE_API_KEY=sk_test_...
STRIPE_WEBHOOK_SECRET=whsec_...

# Logging
RUST_LOG=debug

# UI
VITE_API_URL=http://localhost:3000/api/v1
```

---

## Success Criteria

By end of Week 6:
- [ ] Database has 5+ test users
- [ ] User can register and login
- [ ] API keys can be created and revoked
- [ ] Quota enforcement is working
- [ ] Payments are being processed
- [ ] Dashboard shows usage stats
- [ ] Metrics are collecting
- [ ] Webhooks are being processed
- [ ] 0 unwrap()/expect() in production code
- [ ] All tests pass: `cargo test`

---

## Revenue at Launch

Expected metrics:
- **Users**: 100+ registered (if beta)
- **Conversions**: 5-10% free→pro
- **Revenue**: $100-200 MRR (small user base)
- **Cost**: $0 (SQLite, self-hosted)
- **Margin**: ~95% (hosting only)

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| `database locked` | Ensure single instance, use WAL mode |
| `JWT token invalid` | Check JWT_SECRET matches between sign/verify |
| `Stripe webhook failed` | Verify STRIPE_WEBHOOK_SECRET is correct |
| `Quota exceeded but should work` | Clear quota cache: `redis-cli FLUSHDB` |
| `API returns 500` | Check logs: `RUST_LOG=debug cargo run` |

---

**When you complete this checklist, all 7 revenue streams will be operational and generating revenue.**
