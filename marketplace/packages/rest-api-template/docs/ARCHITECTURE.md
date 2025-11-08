# REST API Template - Architecture Documentation

## Overview

This package demonstrates RDF-driven code generation for REST APIs across multiple programming languages. The architecture follows a semantic-first approach where all API behavior is defined in RDF ontology, then transformed into executable code via SPARQL queries.

## RDF Ontology Design

### Core Abstraction Layers

```
┌─────────────────────────────────────────────┐
│         REST API Ontology (TTL)             │
│  - HTTP semantics (methods, status codes)   │
│  - Request/response schemas                 │
│  - Authentication & authorization           │
│  - Middleware composition                   │
│  - CRUD operations                          │
└─────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────┐
│       SPARQL Query Templates (RQ)           │
│  - Extract endpoints and routes             │
│  - Generate handlers by HTTP method         │
│  - Compose middleware chains                │
│  - Build OpenAPI specifications             │
└─────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────┐
│     Language-Specific Code Generation       │
│  Rust:       Axum + Tower middleware        │
│  TypeScript: Express.js + validators        │
│  Python:     FastAPI + Pydantic             │
└─────────────────────────────────────────────┘
```

### Ontology Structure (250+ lines)

**1. Core Classes:**

- `rest:RestAPI` - Top-level API definition
- `rest:Endpoint` - Individual API endpoint
- `rest:Route` - URL pattern with path parameters
- `rest:Handler` - Business logic processor
- `rest:Middleware` - Request/response interceptor

**2. HTTP Semantics:**

- `rest:HTTPMethod` - GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS
- `rest:StatusCode` - 200, 201, 204, 400, 401, 403, 404, 422, 429, 500, 503
- `rest:ContentType` - JSON, XML, plain text, multipart form data

**3. Schema Definitions:**

- `rest:Schema` - Base schema class
- `rest:RequestSchema` - Validates incoming requests
- `rest:ResponseSchema` - Defines response structure
- `rest:ValidationRule` - Constraints (required, type, range, pattern)

**4. Security:**

- `rest:AuthenticationScheme` - JWT, OAuth2, API key, Basic auth
- `rest:Role` - Admin, User, Guest roles
- `rest:Permission` - Read, write, delete, manage permissions

**5. Quality of Service:**

- `rest:RateLimiter` - Token bucket, fixed window, sliding window
- `rest:CacheStrategy` - Public, private, no-cache directives
- `rest:ErrorHandler` - Centralized error processing

### Property Graph Example

```turtle
:UsersAPI a rest:RestAPI ;
    rest:hasEndpoint :ListUsers, :CreateUser, :GetUser, :UpdateUser, :DeleteUser .

:ListUsers a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/users" ] ;
    rest:hasMethod rest:GET ;
    rest:hasHandler [ rest:handlerName "list_users" ] ;
    rest:hasResponseSchema :UsersListSchema ;
    rest:hasCacheStrategy [
        rest:cacheControl rest:PublicCache ;
        rest:cacheMaxAge 300
    ] ;
    rest:hasRateLimiter [
        a rest:TokenBucket ;
        rest:maxRequestsPerMinute 100
    ] .

:CreateUser a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/users" ] ;
    rest:hasMethod rest:POST ;
    rest:hasHandler [ rest:handlerName "create_user" ] ;
    rest:hasRequestSchema :CreateUserSchema ;
    rest:hasResponseSchema :UserSchema ;
    rest:requiresAuth true ;
    rest:hasAuthentication rest:JWTAuth ;
    rest:hasAuthorization rest:UserRole .

:CreateUserSchema a rest:RequestSchema ;
    rest:hasValidation [
        a rest:RequiredField ;
        rest:fieldName "username" ;
        rest:constraint "min_length:3,max_length:30"
    ] ;
    rest:hasValidation [
        a rest:RequiredField, rest:PatternConstraint ;
        rest:fieldName "email" ;
        rest:pattern "^[^@]+@[^@]+\\.[^@]+$"
    ] .
```

## SPARQL Query Templates

### Query 1: Extract All Endpoints

Retrieves complete endpoint configuration including path, method, handlers, and middleware.

**Use Case:** Router configuration generation

**Output:** Route table with full metadata

### Query 2: Generate Route Handlers

Extracts handlers grouped by HTTP method with request/response schemas.

**Use Case:** Handler function generation with type signatures

**Output:** Type-safe handler definitions

### Query 3: Extract Validation Rules

Retrieves all validation constraints for request schemas.

**Use Case:** Input validation middleware generation

**Output:** Validation rule sets (required, type, range, pattern)

### Query 4: Middleware Composition Chain

Builds ordered middleware pipeline per endpoint.

**Use Case:** Middleware stack configuration

**Output:** Ordered middleware sequence

### Query 5: OpenAPI Specification

Generates OpenAPI 3.0 components from RDF definitions.

**Use Case:** API documentation generation (Swagger/Redoc)

**Output:** OpenAPI YAML/JSON specification

### Query 13: Complete Router Configuration

Comprehensive query combining routes, handlers, middleware, auth, validation, and rate limiting.

**Use Case:** Full application bootstrap

**Output:** Complete router configuration

## Code Generation Flow

### 1. Parse RDF Ontology

```rust
// GGEN core functionality (simplified)
fn parse_ontology(ttl_path: &str) -> Graph {
    let content = fs::read_to_string(ttl_path)?;
    Graph::parse(content, "turtle")?
}
```

### 2. Execute SPARQL Queries

```rust
fn execute_sparql(graph: &Graph, query: &str) -> QueryResults {
    let prepared = graph.prepare_query(query)?;
    prepared.exec()?
}
```

### 3. Transform Results to AST

```rust
fn build_ast(results: QueryResults, language: Language) -> AST {
    match language {
        Language::Rust => RustASTBuilder::from_results(results),
        Language::TypeScript => TypeScriptASTBuilder::from_results(results),
        Language::Python => PythonASTBuilder::from_results(results),
    }
}
```

### 4. Generate Code

```rust
fn generate_code(ast: AST, output_dir: &Path) {
    for module in ast.modules() {
        let code = module.render();
        fs::write(output_dir.join(module.filename()), code)?;
    }
}
```

## Multi-Language Patterns

### Request Handler Pattern

**Rust (Axum):**
```rust
pub async fn create_user(
    State(state): State<AppState>,
    Json(req): Json<CreateUserRequest>,
) -> Result<(StatusCode, Json<User>), StatusCode> {
    // Validation
    if req.username.is_empty() || !req.email.contains('@') {
        return Err(StatusCode::UNPROCESSABLE_ENTITY);
    }

    // Business logic
    let user = state.db.create_user(req.username, req.email).await;

    Ok((StatusCode::CREATED, Json(user)))
}
```

**TypeScript (Express):**
```typescript
async createUser(req: Request<{}, {}, CreateUserRequest>, res: Response) {
    try {
        const user = await this.db.createUser(req.body);
        res.status(201).json({ success: true, data: user });
    } catch (error) {
        res.status(500).json({ success: false, error: 'Failed to create user' });
    }
}
```

**Python (FastAPI):**
```python
@app.post("/api/users", response_model=User, status_code=status.HTTP_201_CREATED)
async def create_user(
    request: CreateUserRequest,
    db: DatabaseService = Depends(get_db),
):
    """Create a new user"""
    user = await db.create_user(request)
    return user
```

### Validation Pattern

**Rust:** Type system + custom validators
```rust
if req.username.len() < 3 || req.username.len() > 30 {
    return Err(StatusCode::UNPROCESSABLE_ENTITY);
}
```

**TypeScript:** express-validator
```typescript
body('username')
    .isString()
    .trim()
    .isLength({ min: 3, max: 30 })
    .withMessage('Username must be 3-30 characters')
```

**Python:** Pydantic
```python
class CreateUserRequest(BaseModel):
    username: str = Field(..., min_length=3, max_length=30)
    email: EmailStr
```

### Middleware Pattern

**Rust:** Tower layers
```rust
Router::new()
    .route("/api/users", post(create_user))
    .layer(ServiceBuilder::new()
        .layer(TraceLayer::new_for_http())
        .layer(CorsLayer::permissive()))
```

**TypeScript:** Express middleware
```typescript
app.use(helmet());
app.use(cors());
app.use(limiter);
app.use(express.json());
```

**Python:** FastAPI middleware
```python
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
```

## Database Abstraction

### Interface Design

All generated APIs use a common database interface pattern:

```
┌─────────────────────────────────────┐
│      Database Service Interface      │
│  - get_users(page, limit)           │
│  - get_user_by_id(id)               │
│  - create_user(data)                │
│  - update_user(id, data)            │
│  - delete_user(id)                  │
└─────────────────────────────────────┘
           ↓                ↓
    ┌──────────┐     ┌──────────┐
    │ Template │     │ Production│
    │ (Memory) │     │ (SQL Pool)│
    └──────────┘     └──────────┘
```

### Template Implementation (In-Memory)

**Purpose:** Zero setup, instant testing

**Rust:**
```rust
pub struct DatabasePool {
    users: Arc<RwLock<Vec<User>>>,
}
```

**TypeScript:**
```typescript
class DatabaseService {
    private users: Map<string, User> = new Map();
}
```

**Python:**
```python
class DatabaseService:
    def __init__(self):
        self.users: Dict[UUID, User] = {}
```

### Production Implementation (SQL)

**Rust (sqlx):**
```rust
pub struct DatabasePool {
    pool: sqlx::PgPool,
}

impl DatabasePool {
    pub async fn create_user(&self, username: String, email: String) -> User {
        sqlx::query_as!(
            User,
            "INSERT INTO users (username, email) VALUES ($1, $2) RETURNING *",
            username,
            email
        )
        .fetch_one(&self.pool)
        .await
        .unwrap()
    }
}
```

**TypeScript (Knex):**
```typescript
class DatabaseService {
    constructor(private knex: Knex) {}

    async createUser(data: CreateUserRequest): Promise<User> {
        const [user] = await this.knex('users')
            .insert(data)
            .returning('*');
        return user;
    }
}
```

**Python (SQLAlchemy):**
```python
class DatabaseService:
    def __init__(self, session: AsyncSession):
        self.session = session

    async def create_user(self, data: CreateUserRequest) -> User:
        user = User(**data.dict())
        self.session.add(user)
        await self.session.commit()
        return user
```

## Testing Strategy (Chicago TDD)

### Philosophy

**London TDD (Mockist):** Mock all dependencies, test in isolation

**Chicago TDD (Classicist):** Use real dependencies, test integration

### Why Chicago TDD for REST APIs?

1. **Real HTTP Testing:** Actual TCP connections, not mocked responses
2. **Database Integration:** Testcontainers for real PostgreSQL
3. **End-to-End Validation:** Full request/response cycle
4. **Performance Metrics:** Measure actual throughput
5. **Security Testing:** Real attack simulations

### Test Architecture

```
Test Harness
    ↓
Real HTTP Server (Axum/Express/FastAPI)
    ↓
Real Database (Testcontainers PostgreSQL)
    ↓
Assertions on HTTP responses
```

### Test Categories

**1. CRUD Operations (5 tests)**
- Create, read, update, delete success cases
- 404 not found handling

**2. Validation (3 tests)**
- Required field validation
- Type constraint validation
- Pattern validation (email format)

**3. Performance Benchmarks (2 tests)**
- User creation: >100 req/sec
- User retrieval: >500 req/sec

**4. Security (2 tests)**
- SQL injection prevention (parameterized queries)
- XSS attack handling (input sanitization)

**5. Edge Cases (6 tests)**
- Concurrent requests (20 simultaneous)
- Unicode characters in usernames
- Very long usernames (100+ chars)
- Empty request bodies
- Malformed JSON

### Performance Thresholds

```rust
let requests_per_sec = iterations as f64 / duration.as_secs_f64();
assert!(requests_per_sec > 100.0, "Performance below threshold");
```

**Requirements:**
- POST /users: >100 req/sec
- GET /users: >500 req/sec
- GET /users/:id: >1000 req/sec
- All tests pass in <2 seconds total

## OpenAPI Integration

### Automatic Documentation Generation

**From RDF:**
```turtle
:ListUsers
    rdfs:label "List all users" ;
    rdfs:comment "Returns paginated list of users" ;
    rest:hasRequestSchema :PaginationSchema ;
    rest:hasResponseSchema :UsersListSchema .
```

**To OpenAPI:**
```yaml
/api/users:
  get:
    summary: List all users
    description: Returns paginated list of users
    parameters:
      - name: page
        in: query
        schema:
          type: integer
          minimum: 1
      - name: limit
        in: query
        schema:
          type: integer
          minimum: 1
          maximum: 100
    responses:
      '200':
        description: Success
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/UsersList'
```

### Interactive Documentation

**FastAPI (Python):** Built-in Swagger UI at `/docs`

**Express (TypeScript):** Add swagger-ui-express middleware

**Axum (Rust):** Use utoipa crate

## Deployment Patterns

### Containerization

**Dockerfile (Multi-stage):**
```dockerfile
FROM rust:1.75 as builder
WORKDIR /app
COPY . .
RUN cargo build --release

FROM debian:bookworm-slim
COPY --from=builder /app/target/release/api /usr/local/bin/
EXPOSE 3000
CMD ["api"]
```

**Size:** 50MB (Rust), 200MB (Node.js), 150MB (Python)

### Kubernetes Deployment

**Horizontal Pod Autoscaler:**
```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: rest-api-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: rest-api
  minReplicas: 2
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
```

### Serverless Adaptation

**AWS Lambda Handler (Node.js):**
```typescript
import serverlessExpress from '@vendia/serverless-express';
import { createApp } from './server';

const app = createApp(new DatabaseService());
export const handler = serverlessExpress({ app });
```

**Cold Start:** <500ms (Rust), <1s (Node.js), <2s (Python)

## Performance Optimization

### 1. Connection Pooling

**Before:** 1 connection per request
**After:** Reuse pool of 10-20 connections
**Improvement:** 5x throughput increase

### 2. Prepared Statements

**Before:** Parse SQL on every request
**After:** Prepare once, execute many times
**Improvement:** 2x query speedup

### 3. Response Caching

**Before:** Query database on every GET
**After:** Cache results for 5 minutes
**Improvement:** 10x faster reads

**Implementation:**
```rust
async fn list_users_cached(cache: &Cache) -> Vec<User> {
    cache.get_or_insert("users", Duration::from_secs(300), || {
        db.get_users().await
    }).await
}
```

### 4. Compression

**Before:** 100KB JSON response
**After:** 20KB gzipped response
**Improvement:** 5x bandwidth reduction

**Middleware:**
```typescript
app.use(compression({
    threshold: 1024,  // Compress responses >1KB
    level: 6          // Balanced compression
}));
```

### 5. Database Indexing

**Before:** Full table scan on user lookup
**After:** B-tree index on `id` and `email`
**Improvement:** 100x query speedup

**Migration:**
```sql
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_users_created_at ON users(created_at DESC);
```

## Security Best Practices

### 1. HTTPS Enforcement

```rust
app.layer(RequireHttpsLayer::new());
```

### 2. CORS Configuration

**Development:**
```typescript
app.use(cors({ origin: '*' }));
```

**Production:**
```typescript
app.use(cors({
    origin: ['https://yourdomain.com'],
    credentials: true,
    maxAge: 86400
}));
```

### 3. Rate Limiting

**Token Bucket Algorithm:**
```typescript
const limiter = rateLimit({
    windowMs: 60 * 1000,     // 1 minute
    max: 100,                 // 100 requests
    standardHeaders: true,
    legacyHeaders: false,
});
```

### 4. Input Validation

**Defense in Depth:**
1. Schema validation (Pydantic/Zod)
2. Type checking (TypeScript/Rust)
3. SQL parameterization
4. Output escaping

### 5. Secret Management

**Environment Variables:**
```bash
export DATABASE_URL="postgresql://user:pass@localhost/db"
export JWT_SECRET="your-secret-key"
export API_KEY="your-api-key"
```

**Never commit secrets to version control!**

## Extensibility

### Adding Custom Middleware

**1. Define in RDF:**
```turtle
:LoggingMiddleware a rest:Middleware ;
    rest:middlewareOrder 1 ;
    rdfs:label "Request Logging" .
```

**2. Generate Code:**
```rust
async fn logging_middleware(
    req: Request,
    next: Next,
) -> Response {
    let start = Instant::now();
    let method = req.method().clone();
    let uri = req.uri().clone();

    let response = next.run(req).await;

    println!("{} {} - {}ms", method, uri, start.elapsed().as_millis());
    response
}
```

### Adding Custom Validation

**1. Define in RDF:**
```turtle
:CustomPasswordRule a rest:ValidationRule ;
    rest:fieldName "password" ;
    rest:constraint "min_length:8,requires_uppercase,requires_digit" .
```

**2. Generate Validator:**
```python
@validator('password')
def validate_password(cls, v):
    if len(v) < 8:
        raise ValueError('Password must be at least 8 characters')
    if not any(c.isupper() for c in v):
        raise ValueError('Password must contain uppercase letter')
    if not any(c.isdigit() for c in v):
        raise ValueError('Password must contain digit')
    return v
```

## Conclusion

This architecture demonstrates how RDF ontologies can drive code generation across multiple languages while maintaining:

- **Consistency:** Same API behavior in Rust/TypeScript/Python
- **Type Safety:** Leveraging each language's type system
- **Performance:** Production-grade implementations
- **Testing:** Comprehensive Chicago TDD test suites
- **Documentation:** Automatic OpenAPI generation
- **Security:** Built-in best practices
- **Extensibility:** Easy customization via RDF

The 80/20 principle applies: this template covers 80% of REST API use cases with 20% of the code typically required.
