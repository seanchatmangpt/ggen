# REST API Template - 5-Minute Quick Start

Get a production-ready REST API running in 5 minutes.

## Prerequisites

Choose your language:

**Rust:**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo --version  # Should be 1.70+
```

**TypeScript:**
```bash
node --version  # Should be 18+
npm --version   # Should be 9+
```

**Python:**
```bash
python --version  # Should be 3.10+
pip --version
```

## Step 1: Install GGEN (30 seconds)

```bash
cargo install ggen
ggen --version
```

## Step 2: Create Your API (60 seconds)

### Option A: Rust (Axum)

```bash
ggen marketplace install rest-api-template
ggen project new my-rust-api --template rest-api-template --language rust
cd my-rust-api

# Install dependencies
cargo build

# Run server
cargo run
# 🚀 Server running on http://127.0.0.1:3000
```

### Option B: TypeScript (Express)

```bash
ggen marketplace install rest-api-template
ggen project new my-ts-api --template rest-api-template --language typescript
cd my-ts-api

# Install dependencies
npm install

# Run server
npm run dev
# 🚀 Server running on http://localhost:3000
```

### Option C: Python (FastAPI)

```bash
ggen marketplace install rest-api-template
ggen project new my-py-api --template rest-api-template --language python
cd my-py-api

# Install dependencies
pip install -r requirements.txt

# Run server
python main.py
# 🚀 Server running on http://127.0.0.1:8000
# 📚 API docs at http://127.0.0.1:8000/docs
```

## Step 3: Test Your API (90 seconds)

Open a new terminal and run these commands:

### Health Check

```bash
curl http://localhost:3000/health
```

**Expected Response:**
```json
{
  "status": "healthy",
  "timestamp": "2025-11-08T12:00:00Z"
}
```

### Create a User

```bash
curl -X POST http://localhost:3000/api/users \
  -H "Content-Type: application/json" \
  -d '{
    "username": "johndoe",
    "email": "john@example.com"
  }'
```

**Expected Response:**
```json
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "username": "johndoe",
    "email": "john@example.com",
    "createdAt": "2025-11-08T12:00:00Z",
    "updatedAt": "2025-11-08T12:00:00Z"
  }
}
```

### List Users

```bash
curl http://localhost:3000/api/users
```

**Expected Response:**
```json
{
  "success": true,
  "data": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "username": "johndoe",
      "email": "john@example.com",
      "createdAt": "2025-11-08T12:00:00Z",
      "updatedAt": "2025-11-08T12:00:00Z"
    }
  ],
  "meta": {
    "page": 1,
    "limit": 10,
    "total": 1
  }
}
```

### Get User by ID

```bash
# Replace with actual ID from previous response
curl http://localhost:3000/api/users/550e8400-e29b-41d4-a716-446655440000
```

### Update User

```bash
curl -X PUT http://localhost:3000/api/users/550e8400-e29b-41d4-a716-446655440000 \
  -H "Content-Type: application/json" \
  -d '{
    "username": "janedoe"
  }'
```

### Delete User

```bash
curl -X DELETE http://localhost:3000/api/users/550e8400-e29b-41d4-a716-446655440000
```

**Expected Response:** `204 No Content` (empty response)

## Step 4: Run Tests (60 seconds)

**Rust:**
```bash
cargo test
# Running 18+ tests...
# test result: ok. 18 passed; 0 failed
```

**TypeScript:**
```bash
npm test
# PASS  tests/integration.test.ts
# ✓ All tests passed (18 tests)
```

**Python:**
```bash
pytest
# ==================== 18 passed in 2.5s ====================
```

## Step 5: View Documentation (30 seconds)

**Python (FastAPI) only:**

Open your browser to:
- **Swagger UI:** http://localhost:8000/docs
- **ReDoc:** http://localhost:8000/redoc

**All languages:**
- View `docs/README.md` for complete documentation
- View `docs/EXAMPLES.md` for 10+ real-world examples
- View `docs/API.md` for API reference

## What You Just Built

✅ **Production-Ready REST API** with:
- CRUD operations (Create, Read, Update, Delete)
- Input validation (username 3-30 chars, valid email)
- Error handling (400, 404, 422, 500 status codes)
- Pagination (page & limit query params)
- Type safety (Rust types, TypeScript interfaces, Pydantic models)
- Health check endpoint
- 18+ integration tests with 100% pass rate

✅ **Built-In Features:**
- Rate limiting (100 req/min)
- CORS support
- Security headers
- JSON serialization
- Async/await throughout
- Database abstraction (in-memory template)

## Next Steps

### 1. Add a Database

**PostgreSQL (Rust):**
```rust
// Replace in-memory store with sqlx
pub struct DatabasePool {
    pool: sqlx::PgPool,
}
```

**PostgreSQL (TypeScript):**
```typescript
// Replace in-memory store with Knex.js
import knex from 'knex';
const db = knex({
    client: 'postgresql',
    connection: process.env.DATABASE_URL
});
```

**PostgreSQL (Python):**
```python
# Replace in-memory store with SQLAlchemy
from sqlalchemy.ext.asyncio import create_async_engine
engine = create_async_engine(os.getenv('DATABASE_URL'))
```

### 2. Add Authentication

Edit `ontology/rest-api.ttl`:

```turtle
:CreateUser
    rest:requiresAuth true ;
    rest:hasAuthentication rest:JWTAuth ;
    rest:hasAuthorization rest:UserRole .
```

Regenerate code:

```bash
ggen project regenerate
```

### 3. Add More Endpoints

Copy the user endpoint pattern for posts, comments, etc:

```turtle
:PostsAPI a rest:RestAPI ;
    rest:hasEndpoint :ListPosts, :CreatePost, :GetPost .

:CreatePost a rest:Endpoint ;
    rest:hasRoute [ rest:path "/api/posts" ] ;
    rest:hasMethod rest:POST ;
    rest:hasHandler [ rest:handlerName "create_post" ] .
```

### 4. Deploy to Production

**Docker:**
```bash
docker build -t my-api .
docker run -p 3000:3000 my-api
```

**Kubernetes:**
```bash
kubectl apply -f k8s/deployment.yaml
```

**Serverless (AWS Lambda):**
```bash
npm install @vendia/serverless-express
# Deploy handler from generated code
```

## Common Issues

### Port Already in Use

```bash
# Change port in .env or command line
PORT=8080 cargo run  # Rust
PORT=8080 npm run dev  # TypeScript
PORT=8080 python main.py  # Python
```

### Missing Dependencies

**Rust:**
```bash
cargo clean
cargo build
```

**TypeScript:**
```bash
rm -rf node_modules package-lock.json
npm install
```

**Python:**
```bash
pip install -r requirements.txt --force-reinstall
```

### Tests Failing

Make sure server is NOT running while tests execute (tests start their own server).

## Performance Tips

1. **Enable connection pooling** (5x improvement)
2. **Add response caching** (10x for reads)
3. **Use prepared statements** (2x speedup)
4. **Enable compression** (5x bandwidth reduction)

## Security Checklist

- [ ] Use HTTPS in production
- [ ] Configure CORS origins (no `*`)
- [ ] Rotate JWT signing keys
- [ ] Enable rate limiting
- [ ] Use environment variables for secrets
- [ ] Enable request logging

## Resources

- **Full Documentation:** `docs/README.md`
- **Architecture Deep-Dive:** `docs/ARCHITECTURE.md`
- **10+ Real-World Examples:** `docs/EXAMPLES.md`
- **API Reference:** `docs/API.md`
- **GitHub Issues:** https://github.com/ggen/marketplace/issues
- **Discord Community:** https://discord.gg/ggen

## Congratulations! 🎉

You now have a production-ready REST API that:
- Handles thousands of requests per second
- Validates all inputs
- Returns proper HTTP status codes
- Includes comprehensive tests
- Is ready to deploy

**Total Time:** ~5 minutes from zero to running API

---

**Generated with GGEN** - Semantic code generation from RDF ontologies
