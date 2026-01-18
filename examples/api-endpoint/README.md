# API Endpoint Example

A production-ready REST API built with **Axum** and **Tokio**, demonstrating specification-first development using RDF ontologies.

## Overview

This example showcases:
- **RDF-Driven API Design**: User model and endpoints defined in `ontology/api-spec.ttl`
- **Production Code Patterns**: Proper error handling, validation, and thread-safe state management
- **Complete Testing**: Unit tests (7) and integration tests (10)
- **Quality Standards**: 0 clippy warnings, full test coverage

## Architecture

```
┌─────────────────────────────────────────────────────┐
│              REST API (Axum)                        │
│                                                     │
│  GET    /users           → list_users()             │
│  POST   /users           → create_user()            │
│  GET    /users/:id       → get_user()               │
│  DELETE /users/:id       → delete_user()            │
│                                                     │
│              ↓                                      │
│                                                     │
│  ┌─────────────────────────────────────────────┐   │
│  │     InMemoryUserStore (Thread-Safe)        │   │
│  │  Arc<RwLock<HashMap<Uuid, User>>>          │   │
│  │                                             │   │
│  │  - Create (with duplicate email check)     │   │
│  │  - Read (by ID)                            │   │
│  │  - List (all users)                        │   │
│  │  - Delete (by ID)                          │   │
│  └─────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────┘
```

## File Structure

```
api-endpoint/
├── Cargo.toml                 # Dependencies: axum, tokio, serde, uuid, tower
├── make.toml                  # Lifecycle configuration (check, test, lint, run)
├── README.md                  # This file
│
├── ontology/
│   └── api-spec.ttl          # RDF specification for User API
│
├── src/
│   ├── main.rs              # HTTP handlers and server setup
│   ├── error.rs             # Custom error types with proper HTTP responses
│   └── store.rs             # In-memory user store with Arc<RwLock>
│
└── tests/
    └── integration_tests.rs  # 10 integration tests for API structure
```

## Quick Start

### 1. Start the Server

```bash
cd examples/api-endpoint
cargo make run
```

Server runs on `http://127.0.0.1:3000`

### 2. Test the API

#### List all users
```bash
curl http://localhost:3000/users
# Response: []
```

#### Create a user
```bash
curl -X POST http://localhost:3000/users \
  -H "Content-Type: application/json" \
  -d '{
    "name": "John Doe",
    "email": "john@example.com"
  }'
# Response: {
#   "id": "550e8400-e29b-41d4-a716-446655440000",
#   "name": "John Doe",
#   "email": "john@example.com",
#   "active": true
# }
```

#### Get a specific user
```bash
curl http://localhost:3000/users/550e8400-e29b-41d4-a716-446655440000
# Returns the user with that UUID
```

#### Delete a user
```bash
curl -X DELETE http://localhost:3000/users/550e8400-e29b-41d4-a716-446655440000
# Response: 204 No Content
```

## Key Features

### Error Handling

All endpoints return proper HTTP status codes:

| Scenario | Status | Response |
|----------|--------|----------|
| User not found | 404 | `{"error": "User not found"}` |
| Invalid UUID | 400 | `{"error": "Invalid user ID format"}` |
| Duplicate email | 409 | `{"error": "Email already exists"}` |
| Invalid name | 400 | `{"error": "Name must be between 1 and 100 characters"}` |
| Invalid email | 400 | `{"error": "Invalid email format"}` |

### Validation

**Name**: 1-100 characters
**Email**: Must contain `@` and be at least 5 characters
**Duplicates**: Email must be unique across all users

### Thread Safety

User store uses `Arc<RwLock<HashMap<Uuid, User>>>` for thread-safe concurrent access:
- Multiple readers can access user data simultaneously
- Writes are serialized (create, delete operations)
- No race conditions possible

## Testing

### Run All Tests
```bash
cargo make test
# Output: test result: ok. 17 passed
```

### Unit Tests (7)
- Model serialization
- Validation rules (name, email)
- Store operations

### Integration Tests (10)
- HTTP response structure
- JSON serialization
- Status codes for all endpoints
- Error cases

### Code Quality
```bash
cargo make lint
# Output: 0 warnings
```

## Development

### Format Code
```bash
cargo make fmt
```

### Fast Check (< 5s)
```bash
cargo make check
```

### Full Validation
```bash
cargo make pre-commit
# Runs: fmt, lint, test
```

## Specification

The RDF ontology (`ontology/api-spec.ttl`) defines:

- **User Model**: id (uuid), name (string), email (string), active (bool)
- **Endpoints**:
  - `ListUsersEndpoint`: GET /users
  - `CreateUserEndpoint`: POST /users
  - `GetUserEndpoint`: GET /users/{id}
  - `DeleteUserEndpoint`: DELETE /users/{id}

Example RDF snippet:
```ttl
ex:UserModel a ex:Model ;
    rdfs:label "User Model" ;
    ex:hasField ex:UserIdField, ex:UserNameField, ex:UserEmailField, ex:UserActiveField .

ex:UserIdField a ex:Field ;
    ex:fieldName "id" ;
    ex:fieldType "uuid" ;
    ex:fieldRequired true .
```

All generated code is deterministic: same RDF + templates = always same output.

## Dependencies

| Crate | Purpose |
|-------|---------|
| `tokio` | Async runtime |
| `axum` | Web framework |
| `serde` | Serialization/deserialization |
| `uuid` | UUID generation and parsing |
| `thiserror` | Error type definition |
| `tower` | HTTP middleware |
| `tower-http` | HTTP utilities (CORS, tracing) |
| `tracing` | Structured logging |

## Patterns Demonstrated

1. **Error Handling**: `Result<T, E>` with custom `ApiError` type
2. **Concurrency**: `Arc<RwLock<T>>` for thread-safe state
3. **Validation**: Input validation before processing
4. **API Design**: RESTful endpoints with proper HTTP methods
5. **Testing**: Unit + integration tests with good coverage
6. **Determinism**: All values come from RDF specification

## Next Steps

See `../FINAL_STATUS.md` for the full reimplementation roadmap and Wave 2 completion strategy.

## Quality Metrics

✅ `cargo build` - Compiles without errors
✅ `cargo make check` - < 5 seconds
✅ `cargo make test` - 17/17 tests PASS
✅ `cargo make lint` - 0 warnings
✅ Type-safe - No unwrap/expect in production code
✅ Documentation - This README + code comments
