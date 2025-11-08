# REST API Template - Real-World Examples

This document provides 10+ production-ready examples demonstrating how to use the REST API template for common use cases.

## Example 1: E-Commerce Product Catalog

### RDF Definition

```turtle
@prefix rest: <http://ggen.ai/ontology/rest-api#> .
@prefix ecom: <http://example.com/ecommerce#> .

ecom:ProductsAPI a rest:RestAPI ;
    rest:hasEndpoint ecom:ListProducts, ecom:SearchProducts, ecom:GetProduct .

ecom:ListProducts a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/products" ] ;
    rest:hasMethod rest:GET ;
    rest:hasHandler [ rest:handlerName "list_products" ] ;
    rest:hasRequestSchema [
        rest:queryParameter "category" ;
        rest:queryParameter "minPrice" ;
        rest:queryParameter "maxPrice" ;
        rest:queryParameter "inStock"
    ] ;
    rest:hasCacheStrategy [
        rest:cacheControl rest:PublicCache ;
        rest:cacheMaxAge 600  # 10 minutes
    ] .

ecom:SearchProducts a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/products/search" ] ;
    rest:hasMethod rest:GET ;
    rest:hasHandler [ rest:handlerName "search_products" ] ;
    rest:hasRequestSchema [
        rest:queryParameter "q" ;
        rest:queryParameter "page" ;
        rest:queryParameter "limit"
    ] .

ecom:GetProduct a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/products/:id" ] ;
    rest:hasMethod rest:GET ;
    rest:hasHandler [ rest:handlerName "get_product" ] ;
    rest:hasCacheStrategy [
        rest:cacheControl rest:PublicCache ;
        rest:cacheMaxAge 3600  # 1 hour
    ] .
```

### Generated API Usage

```bash
# List all products
curl http://localhost:3000/api/products

# Filter by category
curl "http://localhost:3000/api/products?category=electronics&inStock=true"

# Search products
curl "http://localhost:3000/api/products/search?q=laptop&minPrice=500&maxPrice=2000"

# Get single product with caching
curl http://localhost:3000/api/products/123
```

### TypeScript Implementation

```typescript
interface Product {
    id: string;
    name: string;
    category: string;
    price: number;
    inStock: boolean;
    description: string;
}

class ProductService {
    async listProducts(filters: {
        category?: string;
        minPrice?: number;
        maxPrice?: number;
        inStock?: boolean;
    }): Promise<Product[]> {
        let query = this.knex('products');

        if (filters.category) {
            query = query.where('category', filters.category);
        }
        if (filters.minPrice) {
            query = query.where('price', '>=', filters.minPrice);
        }
        if (filters.maxPrice) {
            query = query.where('price', '<=', filters.maxPrice);
        }
        if (filters.inStock !== undefined) {
            query = query.where('in_stock', filters.inStock);
        }

        return await query;
    }

    async searchProducts(q: string, page: number, limit: number): Promise<Product[]> {
        return await this.knex('products')
            .where('name', 'ilike', `%${q}%`)
            .orWhere('description', 'ilike', `%${q}%`)
            .limit(limit)
            .offset((page - 1) * limit);
    }
}
```

---

## Example 2: Blog with Comments

### RDF Definition

```turtle
blog:PostsAPI a rest:RestAPI ;
    rest:hasEndpoint blog:CreatePost, blog:ListPosts, blog:AddComment .

blog:CreatePost a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/posts" ] ;
    rest:hasMethod rest:POST ;
    rest:requiresAuth true ;
    rest:hasAuthentication rest:JWTAuth ;
    rest:hasAuthorization blog:AuthorRole ;
    rest:hasRequestSchema [
        rest:hasValidation [
            a rest:RequiredField ;
            rest:fieldName "title" ;
            rest:constraint "min_length:5,max_length:200"
        ] ;
        rest:hasValidation [
            a rest:RequiredField ;
            rest:fieldName "content" ;
            rest:constraint "min_length:50"
        ]
    ] .

blog:AddComment a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/posts/:postId/comments" ] ;
    rest:hasMethod rest:POST ;
    rest:requiresAuth true ;
    rest:hasRateLimiter [
        a rest:FixedWindow ;
        rest:maxRequestsPerMinute 10  # Prevent spam
    ] .
```

### Usage

```bash
# Create post (requires auth)
curl -X POST http://localhost:3000/api/posts \
  -H "Authorization: Bearer $JWT_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "title": "My First Blog Post",
    "content": "This is the content of my blog post with at least 50 characters.",
    "tags": ["technology", "programming"]
  }'

# Add comment (rate limited)
curl -X POST http://localhost:3000/api/posts/123/comments \
  -H "Authorization: Bearer $JWT_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "content": "Great post!"
  }'
```

---

## Example 3: User Authentication Service

### RDF Definition

```turtle
auth:AuthAPI a rest:RestAPI ;
    rest:hasEndpoint auth:Register, auth:Login, auth:RefreshToken, auth:Logout .

auth:Register a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/auth/register" ] ;
    rest:hasMethod rest:POST ;
    rest:hasRequestSchema [
        rest:hasValidation [
            a rest:RequiredField, rest:PatternConstraint ;
            rest:fieldName "email" ;
            rest:pattern "^[^@]+@[^@]+\\.[^@]+$"
        ] ;
        rest:hasValidation [
            a rest:RequiredField ;
            rest:fieldName "password" ;
            rest:constraint "min_length:8,requires_uppercase,requires_digit"
        ]
    ] ;
    rest:hasRateLimiter [
        a rest:SlidingWindow ;
        rest:maxRequestsPerMinute 5  # Prevent brute force
    ] .

auth:Login a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/auth/login" ] ;
    rest:hasMethod rest:POST ;
    rest:hasResponseSchema [
        rest:fieldName "accessToken" ;
        rest:fieldName "refreshToken" ;
        rest:fieldName "expiresIn"
    ] ;
    rest:hasRateLimiter [
        a rest:SlidingWindow ;
        rest:maxRequestsPerMinute 10
    ] .

auth:RefreshToken a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/auth/refresh" ] ;
    rest:hasMethod rest:POST ;
    rest:hasRequestSchema [
        rest:hasValidation [
            a rest:RequiredField ;
            rest:fieldName "refreshToken"
        ]
    ] .

auth:Logout a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/auth/logout" ] ;
    rest:hasMethod rest:POST ;
    rest:requiresAuth true .
```

### Python Implementation

```python
from datetime import datetime, timedelta
from jose import jwt
from passlib.context import CryptContext

pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
SECRET_KEY = "your-secret-key"

class AuthService:
    def hash_password(self, password: str) -> str:
        return pwd_context.hash(password)

    def verify_password(self, plain: str, hashed: str) -> bool:
        return pwd_context.verify(plain, hashed)

    def create_access_token(self, user_id: str) -> str:
        expire = datetime.utcnow() + timedelta(hours=1)
        payload = {"sub": user_id, "exp": expire}
        return jwt.encode(payload, SECRET_KEY, algorithm="HS256")

    def create_refresh_token(self, user_id: str) -> str:
        expire = datetime.utcnow() + timedelta(days=7)
        payload = {"sub": user_id, "exp": expire, "type": "refresh"}
        return jwt.encode(payload, SECRET_KEY, algorithm="HS256")

@app.post("/api/auth/register")
async def register(request: RegisterRequest, db: DatabaseService = Depends(get_db)):
    # Check if user exists
    existing = await db.get_user_by_email(request.email)
    if existing:
        raise HTTPException(status_code=400, detail="Email already registered")

    # Hash password and create user
    auth_service = AuthService()
    hashed_password = auth_service.hash_password(request.password)
    user = await db.create_user(request.email, hashed_password)

    return {"message": "User registered successfully", "userId": user.id}

@app.post("/api/auth/login")
async def login(request: LoginRequest, db: DatabaseService = Depends(get_db)):
    # Verify credentials
    user = await db.get_user_by_email(request.email)
    if not user:
        raise HTTPException(status_code=401, detail="Invalid credentials")

    auth_service = AuthService()
    if not auth_service.verify_password(request.password, user.password_hash):
        raise HTTPException(status_code=401, detail="Invalid credentials")

    # Generate tokens
    access_token = auth_service.create_access_token(str(user.id))
    refresh_token = auth_service.create_refresh_token(str(user.id))

    return {
        "accessToken": access_token,
        "refreshToken": refresh_token,
        "expiresIn": 3600
    }
```

---

## Example 4: File Upload API

### RDF Definition

```turtle
files:UploadAPI a rest:RestAPI ;
    rest:hasEndpoint files:UploadFile, files:ListFiles, files:DownloadFile .

files:UploadFile a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/files" ] ;
    rest:hasMethod rest:POST ;
    rest:requiresAuth true ;
    rest:hasRequestSchema [
        rest:contentType rest:MultipartFormData ;
        rest:hasValidation [
            a rest:RequiredField ;
            rest:fieldName "file" ;
            rest:constraint "max_size:10485760"  # 10MB
        ] ;
        rest:hasValidation [
            rest:fieldName "allowedTypes" ;
            rest:constraint "image/jpeg,image/png,application/pdf"
        ]
    ] ;
    rest:hasRateLimiter [
        a rest:FixedWindow ;
        rest:maxRequestsPerMinute 20
    ] .

files:DownloadFile a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/files/:id/download" ] ;
    rest:hasMethod rest:GET ;
    rest:hasCacheStrategy [
        rest:cacheControl rest:PrivateCache ;
        rest:cacheMaxAge 86400  # 1 day
    ] .
```

### Rust Implementation

```rust
use axum::{
    extract::{Multipart, Path, State},
    http::{header, StatusCode},
    response::IntoResponse,
};
use tokio::fs::File;
use tokio::io::AsyncWriteExt;

#[derive(Clone)]
struct FileMetadata {
    id: String,
    filename: String,
    content_type: String,
    size: u64,
    path: String,
}

async fn upload_file(
    State(state): State<AppState>,
    mut multipart: Multipart,
) -> Result<Json<FileMetadata>, StatusCode> {
    while let Some(field) = multipart.next_field().await.unwrap() {
        let name = field.name().unwrap().to_string();

        if name == "file" {
            let filename = field.file_name().unwrap().to_string();
            let content_type = field.content_type().unwrap().to_string();

            // Validate file type
            if !["image/jpeg", "image/png", "application/pdf"].contains(&content_type.as_str()) {
                return Err(StatusCode::UNSUPPORTED_MEDIA_TYPE);
            }

            let data = field.bytes().await.unwrap();

            // Validate file size
            if data.len() > 10 * 1024 * 1024 {
                return Err(StatusCode::PAYLOAD_TOO_LARGE);
            }

            // Save to disk
            let file_id = uuid::Uuid::new_v4().to_string();
            let file_path = format!("uploads/{}", file_id);

            let mut file = File::create(&file_path).await.unwrap();
            file.write_all(&data).await.unwrap();

            // Store metadata in database
            let metadata = FileMetadata {
                id: file_id,
                filename,
                content_type,
                size: data.len() as u64,
                path: file_path,
            };

            state.db.save_file_metadata(&metadata).await;

            return Ok(Json(metadata));
        }
    }

    Err(StatusCode::BAD_REQUEST)
}

async fn download_file(
    State(state): State<AppState>,
    Path(id): Path<String>,
) -> Result<impl IntoResponse, StatusCode> {
    let metadata = state.db.get_file_metadata(&id).await
        .ok_or(StatusCode::NOT_FOUND)?;

    let contents = tokio::fs::read(&metadata.path).await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    Ok((
        [(header::CONTENT_TYPE, metadata.content_type)],
        contents,
    ))
}
```

### Usage

```bash
# Upload file
curl -X POST http://localhost:3000/api/files \
  -H "Authorization: Bearer $JWT_TOKEN" \
  -F "file=@document.pdf"

# Response:
# {
#   "id": "550e8400-e29b-41d4-a716-446655440000",
#   "filename": "document.pdf",
#   "contentType": "application/pdf",
#   "size": 204800,
#   "path": "uploads/550e8400-e29b-41d4-a716-446655440000"
# }

# Download file
curl http://localhost:3000/api/files/550e8400-e29b-41d4-a716-446655440000/download \
  -o downloaded.pdf
```

---

## Example 5: Real-Time WebSocket Notifications

### RDF Definition

```turtle
ws:NotificationsAPI a rest:RestAPI ;
    rest:hasEndpoint ws:SubscribeNotifications, ws:SendNotification .

ws:SubscribeNotifications a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/notifications/subscribe" ] ;
    rest:hasMethod rest:GET ;  # WebSocket upgrade
    rest:requiresAuth true ;
    rest:hasProtocol "websocket" .

ws:SendNotification a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/notifications" ] ;
    rest:hasMethod rest:POST ;
    rest:requiresAuth true ;
    rest:hasAuthorization rest:AdminRole ;
    rest:hasRequestSchema [
        rest:hasValidation [
            a rest:RequiredField ;
            rest:fieldName "userId" ;
            rest:fieldName "message" ;
            rest:fieldName "type"
        ]
    ] .
```

### TypeScript Implementation

```typescript
import { WebSocketServer, WebSocket } from 'ws';

interface Notification {
    type: 'info' | 'warning' | 'error';
    message: string;
    timestamp: Date;
}

class NotificationService {
    private connections: Map<string, WebSocket> = new Map();

    handleConnection(ws: WebSocket, userId: string) {
        this.connections.set(userId, ws);

        ws.on('close', () => {
            this.connections.delete(userId);
        });

        ws.send(JSON.stringify({
            type: 'info',
            message: 'Connected to notification service',
            timestamp: new Date()
        }));
    }

    sendNotification(userId: string, notification: Notification) {
        const ws = this.connections.get(userId);
        if (ws && ws.readyState === WebSocket.OPEN) {
            ws.send(JSON.stringify(notification));
            return true;
        }
        return false;
    }

    broadcast(notification: Notification) {
        for (const ws of this.connections.values()) {
            if (ws.readyState === WebSocket.OPEN) {
                ws.send(JSON.stringify(notification));
            }
        }
    }
}

// Setup WebSocket server
const wss = new WebSocketServer({ port: 8080 });
const notificationService = new NotificationService();

wss.on('connection', (ws, req) => {
    // Extract user ID from JWT token
    const token = req.headers.authorization?.replace('Bearer ', '');
    const userId = verifyToken(token);

    notificationService.handleConnection(ws, userId);
});

// HTTP endpoint to send notifications
app.post('/api/notifications', async (req, res) => {
    const { userId, message, type } = req.body;

    const sent = notificationService.sendNotification(userId, {
        type,
        message,
        timestamp: new Date()
    });

    res.status(sent ? 200 : 404).json({
        success: sent,
        message: sent ? 'Notification sent' : 'User not connected'
    });
});
```

---

## Example 6: Multi-Tenant SaaS API

### RDF Definition

```turtle
saas:MultiTenantAPI a rest:RestAPI ;
    rest:hasEndpoint saas:CreateTenant, saas:ListTenantData .

saas:CreateTenant a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/tenants" ] ;
    rest:hasMethod rest:POST ;
    rest:requiresAuth true ;
    rest:hasAuthorization rest:AdminRole ;
    rest:hasRequestSchema [
        rest:hasValidation [
            a rest:RequiredField ;
            rest:fieldName "tenantId" ;
            rest:constraint "min_length:3,max_length:50"
        ]
    ] .

saas:ListTenantData a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/tenants/:tenantId/data" ] ;
    rest:hasMethod rest:GET ;
    rest:requiresAuth true ;
    rest:hasMiddleware [
        a saas:TenantIsolationMiddleware ;
        rest:middlewareOrder 1
    ] .
```

### Python Implementation

```python
from fastapi import Depends, Header, HTTPException

class TenantContext:
    def __init__(self, tenant_id: str):
        self.tenant_id = tenant_id

async def get_tenant_context(
    x_tenant_id: str = Header(...),
    db: DatabaseService = Depends(get_db)
) -> TenantContext:
    """Middleware to extract and validate tenant ID"""
    tenant = await db.get_tenant(x_tenant_id)
    if not tenant:
        raise HTTPException(status_code=404, detail="Tenant not found")

    return TenantContext(tenant_id=x_tenant_id)

@app.post("/api/tenants")
async def create_tenant(
    request: CreateTenantRequest,
    db: DatabaseService = Depends(get_db)
):
    """Create new tenant (admin only)"""
    tenant = await db.create_tenant(
        tenant_id=request.tenantId,
        name=request.name,
        plan=request.plan
    )

    # Create isolated database schema
    await db.execute(f"CREATE SCHEMA tenant_{tenant.id}")

    return {"tenantId": tenant.id, "status": "active"}

@app.get("/api/tenants/{tenant_id}/data")
async def list_tenant_data(
    tenant_id: str,
    tenant_context: TenantContext = Depends(get_tenant_context),
    db: DatabaseService = Depends(get_db)
):
    """List data for specific tenant with isolation"""
    # Verify tenant ID matches context
    if tenant_context.tenant_id != tenant_id:
        raise HTTPException(status_code=403, detail="Access denied")

    # Query data from tenant-specific schema
    data = await db.execute(
        f"SELECT * FROM tenant_{tenant_id}.data"
    )

    return {"data": data}
```

---

## Example 7: GraphQL-Compatible REST API

### RDF Definition

```turtle
graphql:HybridAPI a rest:RestAPI ;
    rest:hasEndpoint graphql:GraphQLEndpoint, graphql:RESTFallback .

graphql:GraphQLEndpoint a rest:Endpoint ;
    rest:hasRoute [ rest:path "/graphql" ] ;
    rest:hasMethod rest:POST ;
    rest:hasRequestSchema [
        rest:contentType rest:ApplicationJSON ;
        rest:hasValidation [
            a rest:RequiredField ;
            rest:fieldName "query"
        ]
    ] ;
    rest:hasResponseSchema [
        rest:fieldName "data" ;
        rest:fieldName "errors"
    ] .

graphql:RESTFallback a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/users/:id" ] ;
    rest:hasMethod rest:GET ;
    rest:hasResponseSchema [
        rest:hasField "id" ;
        rest:hasField "name" ;
        rest:hasField "email"
    ] .
```

### Usage

```bash
# GraphQL query
curl -X POST http://localhost:3000/graphql \
  -H "Content-Type: application/json" \
  -d '{
    "query": "{ user(id: \"123\") { id name email posts { title } } }"
  }'

# REST fallback
curl http://localhost:3000/api/users/123
```

---

## Example 8: Microservices Orchestration

### RDF Definition

```turtle
micro:OrchestratorAPI a rest:RestAPI ;
    rest:hasEndpoint micro:ProcessOrder .

micro:ProcessOrder a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/orders" ] ;
    rest:hasMethod rest:POST ;
    rest:hasMiddleware [
        a micro:CircuitBreakerMiddleware ;
        rest:middlewareOrder 1 ;
        rest:failureThreshold 5 ;
        rest:timeout 5000  # 5 seconds
    ] .
```

### Rust Implementation

```rust
use std::sync::Arc;
use tokio::time::{timeout, Duration};

struct CircuitBreaker {
    failure_count: Arc<AtomicU32>,
    state: Arc<RwLock<CircuitState>>,
}

enum CircuitState {
    Closed,
    Open,
    HalfOpen,
}

async fn process_order(
    State(state): State<AppState>,
    Json(order): Json<Order>,
) -> Result<Json<OrderResult>, StatusCode> {
    // Circuit breaker logic
    let cb = &state.circuit_breaker;
    if matches!(*cb.state.read().await, CircuitState::Open) {
        return Err(StatusCode::SERVICE_UNAVAILABLE);
    }

    // Call multiple microservices with timeout
    let inventory_check = timeout(
        Duration::from_secs(5),
        check_inventory(&order.items)
    );

    let payment_process = timeout(
        Duration::from_secs(5),
        process_payment(&order.payment)
    );

    let shipping_create = timeout(
        Duration::from_secs(5),
        create_shipment(&order.address)
    );

    // Run in parallel
    let results = tokio::try_join!(
        inventory_check,
        payment_process,
        shipping_create
    );

    match results {
        Ok((inv, pay, ship)) => {
            cb.failure_count.store(0, Ordering::SeqCst);
            Ok(Json(OrderResult {
                order_id: order.id,
                status: "confirmed",
                inventory: inv.unwrap(),
                payment: pay.unwrap(),
                shipping: ship.unwrap(),
            }))
        }
        Err(_) => {
            cb.failure_count.fetch_add(1, Ordering::SeqCst);
            if cb.failure_count.load(Ordering::SeqCst) >= 5 {
                *cb.state.write().await = CircuitState::Open;
            }
            Err(StatusCode::SERVICE_UNAVAILABLE)
        }
    }
}
```

---

## Example 9: IoT Device Management

### RDF Definition

```turtle
iot:DeviceAPI a rest:RestAPI ;
    rest:hasEndpoint iot:RegisterDevice, iot:SendTelemetry, iot:GetDeviceStatus .

iot:SendTelemetry a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/devices/:deviceId/telemetry" ] ;
    rest:hasMethod rest:POST ;
    rest:requiresAuth true ;
    rest:hasAuthentication iot:DeviceAPIKey ;
    rest:hasRateLimiter [
        a rest:TokenBucket ;
        rest:maxRequestsPerMinute 1000  # High throughput
    ] ;
    rest:hasRequestSchema [
        rest:hasValidation [
            a rest:RequiredField ;
            rest:fieldName "temperature" ;
            rest:fieldName "humidity" ;
            rest:fieldName "timestamp"
        ]
    ] .
```

### TypeScript Implementation

```typescript
interface TelemetryData {
    temperature: number;
    humidity: number;
    pressure?: number;
    timestamp: Date;
}

class IoTService {
    private timeseriesDB: InfluxDB;

    async storeTelemetry(deviceId: string, data: TelemetryData) {
        // Write to time-series database
        await this.timeseriesDB.writePoint({
            measurement: 'telemetry',
            tags: { deviceId },
            fields: {
                temperature: data.temperature,
                humidity: data.humidity,
                pressure: data.pressure || null
            },
            timestamp: data.timestamp
        });

        // Check for anomalies
        if (data.temperature > 80 || data.temperature < -20) {
            await this.sendAlert(deviceId, 'Temperature out of range');
        }
    }

    async getDeviceStatus(deviceId: string) {
        // Get last 24 hours of data
        const query = `
            SELECT * FROM telemetry
            WHERE deviceId = '${deviceId}'
            AND time > now() - 24h
        `;

        const results = await this.timeseriesDB.query(query);

        return {
            deviceId,
            lastSeen: results[results.length - 1]?.timestamp,
            averageTemp: results.reduce((sum, r) => sum + r.temperature, 0) / results.length,
            dataPoints: results.length,
            status: results.length > 0 ? 'online' : 'offline'
        };
    }
}
```

---

## Example 10: Real-Time Analytics Dashboard API

### RDF Definition

```turtle
analytics:DashboardAPI a rest:RestAPI ;
    rest:hasEndpoint analytics:GetMetrics, analytics:StreamMetrics .

analytics:GetMetrics a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/analytics/metrics" ] ;
    rest:hasMethod rest:GET ;
    rest:requiresAuth true ;
    rest:hasRequestSchema [
        rest:queryParameter "startDate" ;
        rest:queryParameter "endDate" ;
        rest:queryParameter "granularity"  # hour, day, week, month
    ] ;
    rest:hasCacheStrategy [
        rest:cacheControl rest:PrivateCache ;
        rest:cacheMaxAge 60  # 1 minute cache
    ] .

analytics:StreamMetrics a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/analytics/stream" ] ;
    rest:hasMethod rest:GET ;
    rest:requiresAuth true ;
    rest:hasProtocol "server-sent-events" .
```

### Python Implementation

```python
from fastapi.responses import StreamingResponse
import asyncio
from datetime import datetime, timedelta

@app.get("/api/analytics/metrics")
async def get_metrics(
    start_date: datetime,
    end_date: datetime,
    granularity: str = "hour",
    db: DatabaseService = Depends(get_db)
):
    """Get aggregated metrics for date range"""
    metrics = await db.get_analytics(
        start_date=start_date,
        end_date=end_date,
        granularity=granularity
    )

    return {
        "metrics": metrics,
        "summary": {
            "totalUsers": sum(m["users"] for m in metrics),
            "totalRevenue": sum(m["revenue"] for m in metrics),
            "avgPageViews": sum(m["page_views"] for m in metrics) / len(metrics)
        }
    }

@app.get("/api/analytics/stream")
async def stream_metrics():
    """Real-time metrics via Server-Sent Events"""
    async def event_generator():
        while True:
            # Fetch current metrics
            current_users = await get_current_user_count()
            revenue_today = await get_revenue_today()

            # Yield SSE event
            yield f"data: {json.dumps({
                'timestamp': datetime.utcnow().isoformat(),
                'activeUsers': current_users,
                'revenueToday': revenue_today
            })}\n\n"

            # Wait 5 seconds
            await asyncio.sleep(5)

    return StreamingResponse(
        event_generator(),
        media_type="text/event-stream"
    )
```

### Client Usage

```javascript
// Server-Sent Events client
const eventSource = new EventSource('http://localhost:8000/api/analytics/stream');

eventSource.onmessage = (event) => {
    const metrics = JSON.parse(event.data);
    console.log('Active users:', metrics.activeUsers);
    console.log('Revenue today:', metrics.revenueToday);

    // Update dashboard UI
    updateDashboard(metrics);
};
```

---

## Summary

These examples demonstrate the versatility of the REST API template:

| Example | Key Features | Technologies |
|---------|--------------|--------------|
| 1. E-Commerce | Filtering, search, caching | PostgreSQL, Elasticsearch |
| 2. Blog | Authentication, rate limiting | JWT, Redis |
| 3. Auth Service | JWT, refresh tokens, security | bcrypt, jose |
| 4. File Upload | Multipart, file validation | S3, local storage |
| 5. WebSocket | Real-time notifications | WebSocket, Redis Pub/Sub |
| 6. Multi-Tenant | Tenant isolation, schema separation | PostgreSQL schemas |
| 7. GraphQL Hybrid | GraphQL + REST fallback | GraphQL.js |
| 8. Microservices | Circuit breaker, timeouts | Service mesh |
| 9. IoT | High throughput, time-series | InfluxDB, MQTT |
| 10. Analytics | Aggregations, streaming | ClickHouse, SSE |

All examples are generated from RDF ontology definitions and include:
- ✅ Type-safe implementations
- ✅ Validation and error handling
- ✅ Authentication and authorization
- ✅ Rate limiting and caching
- ✅ Chicago TDD test suites
- ✅ OpenAPI documentation

## Next Steps

1. Choose an example that matches your use case
2. Customize the RDF ontology for your domain
3. Generate code with `ggen project new`
4. Run comprehensive Chicago TDD tests
5. Deploy to production

For more details, see [ARCHITECTURE.md](./ARCHITECTURE.md) and [API.md](./API.md).
