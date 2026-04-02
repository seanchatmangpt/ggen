# REST API Template - API Reference

Complete API reference for the generated REST endpoints.

## Base URL

```
http://localhost:3000
```

## Authentication

All authenticated endpoints require a Bearer token in the Authorization header:

```http
Authorization: Bearer <your-jwt-token>
```

## Rate Limiting

| Endpoint | Limit |
|----------|-------|
| `/api/users` (GET) | 100 req/min |
| `/api/users` (POST) | 20 req/min |
| `/api/users/:id` (PUT) | 30 req/min |
| `/api/users/:id` (DELETE) | 50 req/min |

Rate limit headers are included in responses:

```http
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1640000000
```

## Endpoints

### Health Check

**GET** `/health`

Check API health status.

**Response:** `200 OK`

```json
{
  "status": "healthy",
  "timestamp": "2025-11-08T12:00:00Z"
}
```

---

### List Users

**GET** `/api/users`

Retrieve paginated list of users.

**Query Parameters:**

| Name | Type | Required | Default | Description |
|------|------|----------|---------|-------------|
| `page` | integer | No | 1 | Page number (min: 1) |
| `limit` | integer | No | 10 | Items per page (min: 1, max: 100) |

**Response:** `200 OK`

```json
{
  "success": true,
  "data": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "username": "johndoe",
      "email": "john@example.com",
      "createdAt": "2025-11-08T10:00:00Z",
      "updatedAt": "2025-11-08T10:00:00Z"
    }
  ],
  "meta": {
    "page": 1,
    "limit": 10,
    "total": 42
  }
}
```

**Cache Headers:**

```http
Cache-Control: public, max-age=300
```

**Example:**

```bash
curl http://localhost:3000/api/users?page=2&limit=20
```

---

### Get User

**GET** `/api/users/:id`

Retrieve single user by ID.

**Path Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `id` | string (UUID) | Yes | User ID |

**Response:** `200 OK`

```json
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "username": "johndoe",
    "email": "john@example.com",
    "createdAt": "2025-11-08T10:00:00Z",
    "updatedAt": "2025-11-08T10:00:00Z"
  }
}
```

**Error Responses:**

| Status | Description |
|--------|-------------|
| `400 Bad Request` | Invalid UUID format |
| `404 Not Found` | User does not exist |

**Example:**

```bash
curl http://localhost:3000/api/users/550e8400-e29b-41d4-a716-446655440000
```

---

### Create User

**POST** `/api/users`

Create a new user.

**Request Body:**

```json
{
  "username": "johndoe",
  "email": "john@example.com"
}
```

**Validation Rules:**

| Field | Type | Required | Constraints |
|-------|------|----------|-------------|
| `username` | string | Yes | 3-30 characters |
| `email` | string | Yes | Valid email format |

**Response:** `201 Created`

```json
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "username": "johndoe",
    "email": "john@example.com",
    "createdAt": "2025-11-08T10:00:00Z",
    "updatedAt": "2025-11-08T10:00:00Z"
  }
}
```

**Error Responses:**

| Status | Description | Example |
|--------|-------------|---------|
| `400 Bad Request` | Missing required fields | `{"success": false, "error": "username is required"}` |
| `422 Unprocessable Entity` | Validation failed | `{"success": false, "error": "Invalid email format"}` |
| `429 Too Many Requests` | Rate limit exceeded | `{"success": false, "error": "Too many requests"}` |

**Example:**

```bash
curl -X POST http://localhost:3000/api/users \
  -H "Content-Type: application/json" \
  -d '{
    "username": "johndoe",
    "email": "john@example.com"
  }'
```

---

### Update User

**PUT** `/api/users/:id`

Update an existing user.

**Path Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `id` | string (UUID) | Yes | User ID |

**Request Body:**

```json
{
  "username": "janedoe",
  "email": "jane@example.com"
}
```

**Validation Rules:**

| Field | Type | Required | Constraints |
|-------|------|----------|-------------|
| `username` | string | No | 3-30 characters |
| `email` | string | No | Valid email format |

**Response:** `200 OK`

```json
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "username": "janedoe",
    "email": "jane@example.com",
    "createdAt": "2025-11-08T10:00:00Z",
    "updatedAt": "2025-11-08T12:00:00Z"
  }
}
```

**Error Responses:**

| Status | Description |
|--------|-------------|
| `404 Not Found` | User does not exist |
| `422 Unprocessable Entity` | Validation failed |

**Example:**

```bash
curl -X PUT http://localhost:3000/api/users/550e8400-e29b-41d4-a716-446655440000 \
  -H "Content-Type: application/json" \
  -d '{
    "username": "janedoe"
  }'
```

---

### Delete User

**DELETE** `/api/users/:id`

Delete a user.

**Path Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `id` | string (UUID) | Yes | User ID |

**Response:** `204 No Content`

(Empty response body)

**Error Responses:**

| Status | Description |
|--------|-------------|
| `404 Not Found` | User does not exist |

**Example:**

```bash
curl -X DELETE http://localhost:3000/api/users/550e8400-e29b-41d4-a716-446655440000
```

---

## Error Response Format

All errors follow this format:

```json
{
  "success": false,
  "error": "Error message describing what went wrong"
}
```

### Error Status Codes

| Code | Name | Description |
|------|------|-------------|
| `400` | Bad Request | Invalid request format or missing required fields |
| `401` | Unauthorized | Missing or invalid authentication token |
| `403` | Forbidden | Insufficient permissions for this action |
| `404` | Not Found | Resource does not exist |
| `422` | Unprocessable Entity | Validation failed on request data |
| `429` | Too Many Requests | Rate limit exceeded |
| `500` | Internal Server Error | Unexpected server error |
| `503` | Service Unavailable | Service is temporarily unavailable |

---

## Data Types

### User

```typescript
interface User {
  id: string;              // UUID v4
  username: string;        // 3-30 characters
  email: string;           // Valid email format
  createdAt: string;       // ISO 8601 timestamp
  updatedAt: string;       // ISO 8601 timestamp
}
```

### CreateUserRequest

```typescript
interface CreateUserRequest {
  username: string;        // 3-30 characters, required
  email: string;           // Valid email format, required
}
```

### UpdateUserRequest

```typescript
interface UpdateUserRequest {
  username?: string;       // 3-30 characters, optional
  email?: string;          // Valid email format, optional
}
```

### ApiResponse

```typescript
interface ApiResponse<T> {
  success: boolean;
  data?: T;
  error?: string;
  meta?: {
    page?: number;
    limit?: number;
    total?: number;
  };
}
```

---

## OpenAPI Specification

The complete OpenAPI 3.0 specification is available at:

```
GET /openapi.json
GET /openapi.yaml
```

Interactive documentation:

- **Swagger UI:** `http://localhost:3000/docs` (Python/FastAPI only)
- **ReDoc:** `http://localhost:3000/redoc` (Python/FastAPI only)

---

## Code Examples

### JavaScript/TypeScript

```typescript
// List users
const response = await fetch('http://localhost:3000/api/users?page=1&limit=10');
const { data, meta } = await response.json();

// Create user
const newUser = await fetch('http://localhost:3000/api/users', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    username: 'johndoe',
    email: 'john@example.com'
  })
});
const user = await newUser.json();

// Update user
await fetch(`http://localhost:3000/api/users/${user.data.id}`, {
  method: 'PUT',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    username: 'janedoe'
  })
});

// Delete user
await fetch(`http://localhost:3000/api/users/${user.data.id}`, {
  method: 'DELETE'
});
```

### Python

```python
import requests

# List users
response = requests.get('http://localhost:3000/api/users', params={
    'page': 1,
    'limit': 10
})
data = response.json()

# Create user
response = requests.post('http://localhost:3000/api/users', json={
    'username': 'johndoe',
    'email': 'john@example.com'
})
user = response.json()

# Update user
requests.put(f"http://localhost:3000/api/users/{user['data']['id']}", json={
    'username': 'janedoe'
})

# Delete user
requests.delete(f"http://localhost:3000/api/users/{user['data']['id']}")
```

### Rust

```rust
use reqwest;
use serde::{Deserialize, Serialize};

#[derive(Serialize)]
struct CreateUserRequest {
    username: String,
    email: String,
}

#[derive(Deserialize)]
struct User {
    id: String,
    username: String,
    email: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = reqwest::Client::new();

    // Create user
    let new_user = CreateUserRequest {
        username: "johndoe".to_string(),
        email: "john@example.com".to_string(),
    };

    let response = client
        .post("http://localhost:3000/api/users")
        .json(&new_user)
        .send()
        .await?;

    let user: User = response.json().await?;
    println!("Created user: {}", user.id);

    // List users
    let users: Vec<User> = client
        .get("http://localhost:3000/api/users")
        .query(&[("page", "1"), ("limit", "10")])
        .send()
        .await?
        .json()
        .await?;

    println!("Found {} users", users.len());

    Ok(())
}
```

### cURL

```bash
# List users
curl http://localhost:3000/api/users?page=1&limit=10

# Create user
curl -X POST http://localhost:3000/api/users \
  -H "Content-Type: application/json" \
  -d '{"username": "johndoe", "email": "john@example.com"}'

# Get user
curl http://localhost:3000/api/users/550e8400-e29b-41d4-a716-446655440000

# Update user
curl -X PUT http://localhost:3000/api/users/550e8400-e29b-41d4-a716-446655440000 \
  -H "Content-Type: application/json" \
  -d '{"username": "janedoe"}'

# Delete user
curl -X DELETE http://localhost:3000/api/users/550e8400-e29b-41d4-a716-446655440000
```

---

## Testing

### Unit Tests

```bash
# Rust
cargo test

# TypeScript
npm test

# Python
pytest
```

### Integration Tests

```bash
# Start server in test mode
export TEST_MODE=true
cargo run

# Run integration tests
cargo test --test integration_tests
```

### Load Testing

```bash
# Using Apache Bench
ab -n 1000 -c 10 http://localhost:3000/api/users

# Using wrk
wrk -t12 -c400 -d30s http://localhost:3000/api/users
```

---

## Versioning

API versioning via URL path:

```
/api/v1/users
/api/v2/users
```

Or via Accept header:

```http
Accept: application/vnd.myapi.v1+json
```

---

## Support

- **Documentation**: [README.md](./README.md)
- **Architecture**: [ARCHITECTURE.md](./ARCHITECTURE.md)
- **Examples**: [EXAMPLES.md](./EXAMPLES.md)
- **Issues**: https://github.com/ggen/marketplace/issues
