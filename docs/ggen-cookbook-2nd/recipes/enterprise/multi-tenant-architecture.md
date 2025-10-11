# Multi-Tenant Architecture Generator

## Context

Enterprise SaaS applications need to serve multiple tenants (customers) while maintaining data isolation, performance, and security. Traditional approaches either duplicate infrastructure or create complex shared systems that compromise isolation.

## Problem

**How can we generate secure, scalable multi-tenant applications that maintain tenant isolation while optimizing resource usage?**

## Forces

- **Data Isolation**: Each tenant's data must be completely isolated from others
- **Performance**: System should scale horizontally with tenant growth
- **Security**: Tenants must not access each other's data or resources
- **Cost Efficiency**: Shared infrastructure should reduce costs without compromising isolation
- **Compliance**: Must meet regulatory requirements for data sovereignty and privacy
- **Operational Complexity**: Management overhead should be minimized

## Solution

**Generate multi-tenant architectures using tenant-aware templates with database sharding, API routing, and tenant-specific configurations.**

## Implementation

### Tenant-Aware Service Template

```yaml
---
name: "Multi-Tenant Service"
description: "Generate a multi-tenant service with tenant isolation and routing"
variables:
  - name: service_name
    description: "Name of the service"
    type: string
    required: true
  - name: tenant_id_field
    description: "Field name for tenant identification"
    type: string
    default: "tenant_id"
  - name: database_strategy
    description: "Database isolation strategy"
    type: enum
    values: ["separate_databases", "schema_per_tenant", "shared_schema"]
    default: "schema_per_tenant"
  - name: api_versioning
    description: "API versioning strategy"
    type: enum
    values: ["url_path", "header", "query_param"]
    default: "url_path"
dependencies:
  - service: "tenant-registry"
    type: "depends_on"
  - service: "auth-service"
    type: "depends_on"
---
# {{service_name}}/src/tenant_middleware.rs
use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    Error, HttpMessage,
};
use futures_util::future::{ok, LocalBoxFuture, Ready};
use std::{
    future::{ready, Ready as StdReady},
    rc::Rc,
};

pub struct TenantMiddleware<S> {
    service: Rc<S>,
}

impl<S> TenantMiddleware<S> {
    pub fn new(service: S) -> Self {
        Self {
            service: Rc::new(service),
        }
    }
}

impl<S, B> Transform<S, ServiceRequest> for TenantMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Transform = TenantMiddlewareService<S>;
    type InitError = ();
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(TenantMiddlewareService {
            service: Rc::new(service),
        }))
    }
}

pub struct TenantMiddlewareService<S> {
    service: Rc<S>,
}

impl<S, B> Service<ServiceRequest> for TenantMiddlewareService<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, mut req: ServiceRequest) -> Self::Future {
        // Extract tenant ID from request
        let tenant_id = match self.extract_tenant_id(&req) {
            Ok(id) => id,
            Err(e) => {
                return Box::pin(async move {
                    Err(actix_web::error::ErrorBadRequest(e))
                });
            }
        };

        // Add tenant context to request extensions
        req.extensions_mut().insert(TenantContext { tenant_id });

        let fut = self.service.call(req);

        Box::pin(async move {
            let res = fut.await?;
            Ok(res)
        })
    }
}

impl<S> TenantMiddlewareService<S> {
    fn extract_tenant_id(&self, req: &ServiceRequest) -> Result<String, String> {
        // Extract tenant ID from header, URL path, or query parameter
        if let Some(tenant_header) = req.headers().get("X-Tenant-ID") {
            if let Ok(tenant_str) = tenant_header.to_str() {
                return Ok(tenant_str.to_string());
            }
        }

        // Try URL path extraction
        if let Some(path) = req.match_info().get("tenant_id") {
            return Ok(path.to_string());
        }

        // Try query parameter
        if let Some(query) = req.query_string() {
            if let Some(tenant_param) = self.extract_tenant_from_query(query) {
                return Ok(tenant_param);
            }
        }

        Err("No tenant ID found in request".to_string())
    }

    fn extract_tenant_from_query(&self, query: &str) -> Option<String> {
        for pair in query.split('&') {
            let mut parts = pair.split('=');
            if let (Some(key), Some(value)) = (parts.next(), parts.next()) {
                if key == "{{tenant_id_field}}" {
                    return Some(value.to_string());
                }
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct TenantContext {
    pub tenant_id: String,
}

impl TenantContext {
    pub fn tenant_id(&self) -> &str {
        &self.tenant_id
    }
}
---
# {{service_name}}/src/database/tenant_manager.rs
use sqlx::{Pool, Postgres};
use std::collections::HashMap;

pub struct TenantDatabaseManager {
    pools: HashMap<String, Pool<Postgres>>,
    default_pool: Pool<Postgres>,
}

impl TenantDatabaseManager {
    pub async fn new(database_url: &str) -> Result<Self, sqlx::Error> {
        let default_pool = Pool::<Postgres>::connect(database_url).await?;

        Ok(Self {
            pools: HashMap::new(),
            default_pool,
        })
    }

    pub async fn get_tenant_pool(&mut self, tenant_id: &str) -> Result<&Pool<Postgres>, sqlx::Error> {
        if !self.pools.contains_key(tenant_id) {
            // Create tenant-specific database connection
            let tenant_db_url = self.build_tenant_db_url(tenant_id);
            let pool = Pool::<Postgres>::connect(&tenant_db_url).await?;
            self.pools.insert(tenant_id.to_string(), pool);
        }

        Ok(self.pools.get(tenant_id).unwrap())
    }

    fn build_tenant_db_url(&self, tenant_id: &str) -> String {
        match self.database_strategy.as_str() {
            "separate_databases" => {
                format!("postgresql://user:pass@localhost/{}_db", tenant_id)
            }
            "schema_per_tenant" => {
                format!("postgresql://user:pass@localhost/main_db?search_path={}", tenant_id)
            }
            "shared_schema" => {
                "postgresql://user:pass@localhost/main_db".to_string()
            }
            _ => "postgresql://user:pass@localhost/main_db".to_string(),
        }
    }

    pub async fn execute_tenant_query<T>(
        &self,
        tenant_id: &str,
        query: &str,
        bind_values: Vec<T>,
    ) -> Result<Vec<sqlx::postgres::PgRow>, sqlx::Error>
    where
        T: sqlx::Type<Postgres> + sqlx::Encode<'_, Postgres>,
    {
        let pool = self.get_tenant_pool(tenant_id).await?;

        // Add tenant_id filter to all queries
        let tenant_aware_query = self.add_tenant_filter(query, tenant_id);

        sqlx::query(&tenant_aware_query)
            .bind(tenant_id)
            .fetch_all(pool)
            .await
    }

    fn add_tenant_filter(&self, query: &str, tenant_id: &str) -> String {
        // Automatically add tenant_id WHERE clause if not present
        if !query.to_lowercase().contains("where") {
            format!("{} WHERE {} = $1", query, self.tenant_id_field)
        } else {
            query.to_string()
        }
    }
}
---
# {{service_name}}/src/api/tenant_routes.rs
use actix_web::{web, HttpResponse, Result};
use serde::{Deserialize, Serialize};

use super::handlers::*;

pub fn configure_tenant_routes(cfg: &mut web::ServiceConfig) {
    cfg.service(
        web::scope("/tenants/{tenant_id}")
            .route("", web::get().to(get_tenant_data))
            .route("", web::post().to(create_tenant_data))
            .route("", web::put().to(update_tenant_data))
            .route("", web::delete().to(delete_tenant_data))
    );
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TenantDataRequest {
    pub data: serde_json::Value,
    pub metadata: Option<HashMap<String, String>>,
}

async fn get_tenant_data(
    tenant_id: web::Path<String>,
    tenant_context: web::Data<TenantContext>,
) -> Result<HttpResponse> {
    // Verify tenant access
    if tenant_context.tenant_id() != tenant_id.as_str() {
        return Ok(HttpResponse::Forbidden().finish());
    }

    // Fetch tenant-specific data
    let data = fetch_tenant_data(&tenant_id).await?;

    Ok(HttpResponse::Ok().json(data))
}

async fn create_tenant_data(
    tenant_id: web::Path<String>,
    req: web::Json<TenantDataRequest>,
    tenant_context: web::Data<TenantContext>,
) -> Result<HttpResponse> {
    // Verify tenant access
    if tenant_context.tenant_id() != tenant_id.as_str() {
        return Ok(HttpResponse::Forbidden().finish());
    }

    // Create data for specific tenant
    let result = create_tenant_data(&tenant_id, &req.0).await?;

    Ok(HttpResponse::Created().json(result))
}

async fn update_tenant_data(
    tenant_id: web::Path<String>,
    req: web::Json<TenantDataRequest>,
    tenant_context: web::Data<TenantContext>,
) -> Result<HttpResponse> {
    // Verify tenant access
    if tenant_context.tenant_id() != tenant_id.as_str() {
        return Ok(HttpResponse::Forbidden().finish());
    }

    // Update tenant-specific data
    let result = update_tenant_data(&tenant_id, &req.0).await?;

    Ok(HttpResponse::Ok().json(result))
}

async fn delete_tenant_data(
    tenant_id: web::Path<String>,
    tenant_context: web::Data<TenantContext>,
) -> Result<HttpResponse> {
    // Verify tenant access
    if tenant_context.tenant_id() != tenant_id.as_str() {
        return Ok(HttpResponse::Forbidden().finish());
    }

    // Delete tenant-specific data
    delete_tenant_data(&tenant_id).await?;

    Ok(HttpResponse::NoContent().finish())
}
---
# {{service_name}}/src/auth/tenant_auth.rs
use jsonwebtoken::{decode, encode, Header, Algorithm, Validation, EncodingKey, DecodingKey};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct TenantClaims {
    pub sub: String,        // Subject (user ID)
    pub tenant_id: String,  // Tenant the user belongs to
    pub roles: Vec<String>, // User roles within tenant
    pub exp: usize,         // Expiration time
}

pub struct TenantAuthenticator {
    encoding_key: EncodingKey,
    decoding_key: DecodingKey,
}

impl TenantAuthenticator {
    pub fn new(secret: &str) -> Self {
        Self {
            encoding_key: EncodingKey::from_secret(secret.as_ref()),
            decoding_key: DecodingKey::from_secret(secret.as_ref()),
        }
    }

    pub fn generate_token(&self, user_id: &str, tenant_id: &str, roles: &[String]) -> Result<String, jsonwebtoken::errors::Error> {
        let claims = TenantClaims {
            sub: user_id.to_string(),
            tenant_id: tenant_id.to_string(),
            roles: roles.to_vec(),
            exp: (chrono::Utc::now() + chrono::Duration::hours(24)).timestamp() as usize,
        };

        encode(&Header::default(), &claims, &self.encoding_key)
    }

    pub fn validate_token(&self, token: &str) -> Result<TenantClaims, jsonwebtoken::errors::Error> {
        let validation = Validation::new(Algorithm::HS256);
        let token_data = decode::<TenantClaims>(token, &self.decoding_key, &validation)?;

        Ok(token_data.claims)
    }

    pub fn extract_tenant_from_token(&self, token: &str) -> Result<String, anyhow::Error> {
        let claims = self.validate_token(token)?;
        Ok(claims.tenant_id)
    }
}
---
# {{service_name}}/tests/tenant_isolation_test.rs
#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::{test, web, App};

    #[actix_rt::test]
    async fn test_tenant_isolation() {
        // Setup two tenants
        let tenant1_id = "tenant_1";
        let tenant2_id = "tenant_2";

        // Create test data for each tenant
        create_tenant_data(tenant1_id, &test_data()).await.unwrap();
        create_tenant_data(tenant2_id, &test_data()).await.unwrap();

        // Verify tenant1 cannot access tenant2 data
        let tenant1_data = get_tenant_data(tenant1_id).await.unwrap();
        let tenant2_data = get_tenant_data(tenant2_id).await.unwrap();

        // Data should be identical in structure but separate
        assert_eq!(tenant1_data.len(), tenant2_data.len());
        // But should be different instances in the database

        // Test API access with tenant context
        let app = test::init_service(
            App::new()
                .wrap(TenantMiddleware::new())
                .route("/tenants/{tenant_id}", web::get().to(get_tenant_data))
        ).await;

        // Test tenant1 access
        let req = test::TestRequest::get()
            .uri(&format!("/tenants/{}", tenant1_id))
            .insert_header(("X-Tenant-ID", tenant1_id))
            .to_request();

        let resp = test::call_service(&app, req).await;
        assert!(resp.status().is_success());

        // Test tenant2 access
        let req = test::TestRequest::get()
            .uri(&format!("/tenants/{}", tenant2_id))
            .insert_header(("X-Tenant-ID", tenant2_id))
            .to_request();

        let resp = test::call_service(&app, req).await;
        assert!(resp.status().is_success());
    }

    #[actix_rt::test]
    async fn test_tenant_cross_access_denied() {
        // Test that tenant1 cannot access tenant2 data
        let app = test::init_service(
            App::new()
                .wrap(TenantMiddleware::new())
                .route("/tenants/{tenant_id}", web::get().to(get_tenant_data))
        ).await;

        // Tenant1 tries to access tenant2 data
        let req = test::TestRequest::get()
            .uri("/tenants/tenant_2")
            .insert_header(("X-Tenant-ID", "tenant_1"))  // Wrong tenant ID
            .to_request();

        let resp = test::call_service(&app, req).await;
        assert_eq!(resp.status(), actix_web::http::StatusCode::FORBIDDEN);
    }
}
```

### Tenant Registry Template

```yaml
---
name: "Tenant Registry Service"
description: "Generate a service registry for managing tenants and their configurations"
variables:
  - name: registry_name
    description: "Name of the registry service"
    type: string
    default: "tenant-registry"
  - name: storage_backend
    description: "Storage backend for tenant data"
    type: enum
    values: ["postgres", "mongodb", "redis"]
    default: "postgres"
---
# {{registry_name}}/src/models/tenant.rs
use serde::{Deserialize, Serialize};
use sqlx::FromRow;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct Tenant {
    pub id: Uuid,
    pub name: String,
    pub domain: String,
    pub plan: SubscriptionPlan,
    pub status: TenantStatus,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,
    pub settings: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SubscriptionPlan {
    Free,
    Starter,
    Professional,
    Enterprise,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TenantStatus {
    Active,
    Suspended,
    Deactivated,
}
---
# {{registry_name}}/src/api/tenant_management.rs
use actix_web::{web, HttpResponse, Result};
use serde::{Deserialize, Serialize};

use super::models::*;

pub async fn create_tenant(
    req: web::Json<CreateTenantRequest>,
    tenant_manager: web::Data<TenantManager>,
) -> Result<HttpResponse> {
    let tenant = tenant_manager.create_tenant(req.0).await?;

    Ok(HttpResponse::Created().json(tenant))
}

pub async fn get_tenant(
    tenant_id: web::Path<Uuid>,
    tenant_manager: web::Data<TenantManager>,
) -> Result<HttpResponse> {
    let tenant = tenant_manager.get_tenant(*tenant_id).await?;

    Ok(HttpResponse::Ok().json(tenant))
}

pub async fn update_tenant(
    tenant_id: web::Path<Uuid>,
    req: web::Json<UpdateTenantRequest>,
    tenant_manager: web::Data<TenantManager>,
) -> Result<HttpResponse> {
    let tenant = tenant_manager.update_tenant(*tenant_id, req.0).await?;

    Ok(HttpResponse::Ok().json(tenant))
}

pub async fn list_tenants(
    query: web::Query<ListTenantsQuery>,
    tenant_manager: web::Data<TenantManager>,
) -> Result<HttpResponse> {
    let tenants = tenant_manager.list_tenants(query.0).await?;

    Ok(HttpResponse::Ok().json(tenants))
}
```

## Usage

1. **Generate tenant-aware service**:
   ```bash
   ggen generate multi-tenant-service.tmpl \
     --set service_name=user-management \
     --set tenant_id_field=tenant_id \
     --set database_strategy=schema_per_tenant \
     --output ./services/user-management
   ```

2. **Generate tenant registry**:
   ```bash
   ggen generate tenant-registry.tmpl \
     --set registry_name=tenant-registry \
     --set storage_backend=postgres \
     --output ./services/tenant-registry
   ```

3. **Deploy multi-tenant ecosystem**:
   ```bash
   docker-compose -f docker-compose.tenant.yml up -d
   ```

## Verification

1. **Test tenant isolation**:
   ```bash
   # Create two tenants
   curl -X POST http://tenant-registry:8080/tenants \
     -H "Content-Type: application/json" \
     -d '{"name": "Tenant A", "domain": "tenant-a.com"}'

   curl -X POST http://tenant-registry:8080/tenants \
     -H "Content-Type: application/json" \
     -d '{"name": "Tenant B", "domain": "tenant-b.com"}'

   # Verify tenants are isolated
   curl http://user-management:8081/tenants/tenant-a-id/users \
     -H "X-Tenant-ID: tenant-a-id"
   ```

2. **Test API routing**:
   ```bash
   # Should work for correct tenant
   curl http://user-management:8081/tenants/tenant-a-id/users \
     -H "X-Tenant-ID: tenant-a-id"

   # Should fail for wrong tenant
   curl http://user-management:8081/tenants/tenant-a-id/users \
     -H "X-Tenant-ID: tenant-b-id"  # Returns 403
   ```

3. **Test database isolation**:
   ```bash
   # Verify tenant data is properly isolated
   psql -h postgres -U user -d tenant_a_db -c "SELECT COUNT(*) FROM users;"
   psql -h postgres -U user -d tenant_b_db -c "SELECT COUNT(*) FROM users;"
   ```

## Benefits

- **🔒 Data Isolation**: Complete tenant data separation at database level
- **🚀 Scalability**: Horizontal scaling with tenant-aware load balancing
- **🔧 Maintainability**: Consistent patterns across all tenant services
- **📊 Monitoring**: Tenant-specific metrics and alerting
- **⚡ Performance**: Optimized queries with tenant context
- **🛡️ Security**: Multi-layer tenant authentication and authorization

## Next Steps

- **Tenant Migration**: Tools for moving tenants between plans or databases
- **Tenant Analytics**: Built-in analytics and usage tracking per tenant
- **Tenant Customization**: Allow tenants to customize their service instances
- **Multi-Region Support**: Support for tenants in different geographic regions
- **Tenant Backup/Restore**: Automated backup and restore for tenant data
