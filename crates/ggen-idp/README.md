# 🔐 ggen-idp: Innovative Identity Provider System

A production-grade Identity Provider (IDP) system for the BPMN.js Marketplace, featuring innovative BPMN workflow-based authentication flows, advanced Relation-Based Access Control (ReBAC), full OAuth2/OIDC compliance, and comprehensive audit logging.

## 🚀 Key Features

### 1. **BPMN Workflow-Based Authentication**
- Authentication flows defined as BPMN workflows
- Versioned, composable, auditable auth sequences
- Pre-built flows: Login, MFA, Password Reset, OAuth2 Callback
- Customizable per organization
- CEL expression language for flow transitions

```rust
// Define login flow as BPMN workflow
let flow = standard_login_flow(org_id);
let executor = FlowExecutor::new(flow);
let result = executor.execute(flow_context).await;
```

### 2. **Relation-Based Access Control (ReBAC)**
Inspired by Google Zanzibar, implementing:
- Relation tuples: (User, Relation, Resource)
- Transitive relations and role hierarchies
- Constraint evaluation (owner-only, org-only, approval-required)
- CEL-based permission evaluation
- Resource-aware access decisions

```rust
// Check if user can perform action
let allowed = rbac_engine.has_permission(
    user_id,
    org_id,
    "pack",    // resource
    "publish"  // action
).await?;
```

### 3. **Multi-Tenant Architecture**
- Complete organizational isolation
- Per-org customization of auth flows
- Organization-scoped roles and permissions
- Audit logs per organization

### 4. **OAuth2 & OpenID Connect (OIDC)**
Full provider implementation:
- Authorization Code flow with PKCE
- Implicit flow for SPAs
- Client Credentials flow
- Token refresh and revocation
- OpenID Connect Discovery (.well-known/openid-configuration)
- UserInfo endpoint
- JWKS (JSON Web Key Set) support

```bash
# OIDC Discovery
curl http://localhost:8000/oauth/.well-known/openid-configuration

# Authorize
curl "http://localhost:8000/oauth/authorize?client_id=...&redirect_uri=...&scope=openid"

# Token exchange
curl -X POST http://localhost:8000/oauth/token \
  -d "grant_type=authorization_code&code=...&client_id=..."
```

### 5. **Session Management**
- Token lifecycle management
- Token refresh with rotation
- Session revocation and invalidation
- Logout all devices
- Session tracking per user
- IP address and user agent logging

```rust
// Create session
let session = session_manager
    .create_user_session(user_id, ip_address, user_agent)
    .await?;

// Validate token
session_manager.validate_token(&token).await?;

// Refresh token
let new_token = session_manager
    .refresh_access_token(session_id)
    .await?;
```

### 6. **Comprehensive Audit Logging**
- All authentication events logged
- Action tracking: login, logout, permission changes, role assignments
- Failed access attempts
- Resource modifications
- User tracking with IP and user agent
- Compliance-ready audit trails

```rust
// Log login attempt
audit_logger.log_login(
    user_id,
    org_id,
    Some(ip_address),
    Some(user_agent),
    success
).await?;

// Log pack publication
audit_logger.log_pack_publish(user_id, org_id, "pack-123", "1.0.0").await?;
```

### 7. **MFA/2FA Support**
- TOTP (Time-based One-Time Password)
- WebAuthn / FIDO2
- Email verification
- SMS verification
- Backup codes
- MFA enforcement per organization

### 8. **RDF-Backed Identity Graph**
- Semantic identity model using RDF
- SPARQL queries for complex relationships
- Identity onthology support
- Integration with oxigraph RDF store

---

## 🏗️ Architecture

### System Layers

```
┌─────────────────────────────────────┐
│  HTTP API Layer (Actix-web)         │
│  /auth, /rbac, /oauth, /audit       │
└─────────────────────────────────────┘
               ↓
┌─────────────────────────────────────┐
│  Core Services Layer                │
│  ┌──────────────────────────────┐  │
│  │ AuthService                  │  │
│  │ - Register, Login, Password  │  │
│  ├──────────────────────────────┤  │
│  │ RbacEngine                   │  │
│  │ - Permission checks, Roles   │  │
│  ├──────────────────────────────┤  │
│  │ SessionManager               │  │
│  │ - Token lifecycle            │  │
│  ├──────────────────────────────┤  │
│  │ AuditLogger                  │  │
│  │ - Event logging              │  │
│  ├──────────────────────────────┤  │
│  │ OAuth2Provider               │  │
│  │ - OIDC endpoints             │  │
│  └──────────────────────────────┘  │
└─────────────────────────────────────┘
               ↓
┌─────────────────────────────────────┐
│  Workflow Engine Layer              │
│  ┌──────────────────────────────┐  │
│  │ FlowExecutor                 │  │
│  │ - BPMN workflow execution    │  │
│  │ - Step-by-step orchestration │  │
│  ├──────────────────────────────┤  │
│  │ CelEvaluator                 │  │
│  │ - CEL expression evaluation  │  │
│  └──────────────────────────────┘  │
└─────────────────────────────────────┘
               ↓
┌─────────────────────────────────────┐
│  Storage Layer                      │
│  ┌──────────────────────────────┐  │
│  │ RDF Store (Oxigraph)         │  │
│  │ - Identity graph             │  │
│  ├──────────────────────────────┤  │
│  │ PostgreSQL                   │  │
│  │ - Sessions, Audit logs       │  │
│  └──────────────────────────────┘  │
└─────────────────────────────────────┘
```

### Module Structure

```
crates/ggen-idp/src/
├── lib.rs                    # Main library entry
├── models.rs                 # Data models (User, Role, Session, etc.)
├── auth.rs                   # Authentication service
├── rbac/
│   ├── mod.rs               # RBAC engine
│   ├── rebac.rs             # Relation-based access control
│   ├── evaluator.rs         # CEL expression evaluator
│   └── store.rs             # RBAC data persistence
├── oauth2.rs                # OAuth2 and OIDC provider
├── sessions.rs              # Session and token management
├── audit.rs                 # Audit logging
├── workflows/
│   ├── mod.rs               # Workflow orchestration
│   └── auth_flows.rs        # BPMN auth flow definitions
├── handlers.rs              # HTTP request handlers
├── errors.rs                # Error types and responses
└── bin/
    └── main.rs              # CLI server executable
```

---

## 📦 Getting Started

### Installation

Add to `Cargo.toml`:

```toml
[dependencies]
ggen-idp = { path = "crates/ggen-idp", features = ["full"] }
```

### Quick Start

#### 1. Start the IDP Server

```bash
cd crates/ggen-idp
GGEN_IDP_HOST=localhost \
GGEN_IDP_PORT=8000 \
GGEN_IDP_JWT_SECRET="your-secret-key" \
cargo run --bin ggen-idp-server
```

#### 2. Health Check

```bash
curl http://localhost:8000/health
```

#### 3. Register a User

```bash
curl -X POST http://localhost:8000/auth/550e8400-e29b-41d4-a716-446655440000/register \
  -H "Content-Type: application/json" \
  -d '{
    "username": "john_doe",
    "email": "john@example.com",
    "password": "SecurePass123!",
    "display_name": "John Doe"
  }'
```

#### 4. Login

```bash
curl -X POST http://localhost:8000/auth/550e8400-e29b-41d4-a716-446655440000/login \
  -H "Content-Type: application/json" \
  -d '{
    "username": "john_doe",
    "password": "SecurePass123!"
  }'

# Response:
{
  "access_token": "eyJ0eXAiOiJKV1QiLCJhbGc...",
  "refresh_token": "base64-encoded-refresh-token",
  "expires_in": 3600,
  "token_type": "Bearer"
}
```

#### 5. Check Permissions

```bash
curl -X POST http://localhost:8000/rbac/check/550e8400-e29b-41d4-a716-446655440000 \
  -H "Content-Type: application/json" \
  -d '{
    "resource": "pack",
    "action": "publish"
  }'

# Response:
{
  "allowed": true
}
```

---

## 🔐 Security Features

### Password Security
- Minimum 12 characters
- Requires uppercase, lowercase, digit, and special character
- Bcrypt hashing with cost factor 12
- Password reset via email token

### Token Security
- JWT with HS256 signature
- Configurable token expiration (default 1 hour)
- Refresh token rotation
- Token revocation support
- PKCE for OAuth2

### Audit & Compliance
- Complete audit trail
- Failed login attempt tracking
- Suspicious activity detection
- IP address and user agent logging
- GDPR-friendly data retention

### Transport Security
- HTTPS enforcement (recommended for production)
- CORS support
- CSRF protection via state parameter
- Secure cookie handling

---

## 📊 Data Models

### User
```rust
pub struct User {
    pub id: Uuid,
    pub username: String,
    pub email: String,
    pub password_hash: String,
    pub display_name: Option<String>,
    pub avatar_url: Option<String>,
    pub is_active: bool,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub organization_id: Uuid,
}
```

### Role with ReBAC
```rust
pub struct Role {
    pub id: Uuid,
    pub name: String,
    pub description: Option<String>,
    pub organization_id: Uuid,
    pub permissions: Vec<Permission>,
    pub parent_roles: Vec<Uuid>,  // Role hierarchy
    pub created_at: DateTime<Utc>,
}

pub struct Permission {
    pub resource: String,      // "pack", "organization"
    pub action: String,        // "read", "write", "publish"
    pub constraints: Option<PermissionConstraint>,
}
```

### Session & Token
```rust
pub struct Session {
    pub id: Uuid,
    pub user_id: Uuid,
    pub access_token: String,
    pub refresh_token: String,
    pub expires_at: DateTime<Utc>,
    pub created_at: DateTime<Utc>,
    pub ip_address: Option<String>,
    pub user_agent: Option<String>,
    pub is_revoked: bool,
}
```

---

## 🧪 Testing

Run the test suite:

```bash
cargo test -p ggen-idp
```

Run with logging:

```bash
RUST_LOG=debug cargo test -p ggen-idp -- --nocapture
```

Key test areas:
- RBAC permission evaluation
- Session lifecycle
- OAuth2 flows
- Auth workflow execution
- CEL expression evaluation
- Audit logging

---

## 📈 Performance Considerations

### Optimization Strategies
- In-memory session store for high throughput
- Async all-the-way with Tokio
- RDF store caching for common queries
- JWT stateless tokens (no DB lookup)
- Parallel permission checks

### Scaling Recommendations
- Database connection pooling (sqlx)
- Redis for distributed session store
- RDF store replication
- Load balancing across IDP instances
- CDN for JWKS/discovery endpoint

---

## 🔄 Integration with ggen Marketplace

### Marketplace Integration Points

1. **Pack Publishing**
   - Authenticate publisher via IDP
   - Check authorization (has publish permission)
   - Log publish action

2. **Pack Installation**
   - Authenticate user
   - Check pack installation permission
   - Log installation attempt

3. **Team Management**
   - Create organizations in IDP
   - Assign roles to team members
   - Manage team permissions

4. **Audit Compliance**
   - All marketplace actions audited through IDP
   - Trace all modifications to packs
   - Generate compliance reports

### API Integration Example

```rust
// In marketplace service
use ggen_idp::{RbacEngine, AuditLogger};

// Check if user can publish pack
let can_publish = rbac_engine.has_permission(
    user_id,
    org_id,
    "pack",
    "publish"
).await?;

if !can_publish {
    return Err("Permission denied");
}

// Log the publication
audit_logger.log_pack_publish(
    user_id,
    org_id,
    pack_id,
    version
).await?;
```

---

## 🗂️ Configuration

### Environment Variables

```env
# Server
GGEN_IDP_HOST=localhost
GGEN_IDP_PORT=8000

# Security
GGEN_IDP_JWT_SECRET=your-secret-key-min-32-chars
GGEN_IDP_JWT_EXPIRY=3600
GGEN_IDP_REFRESH_TOKEN_EXPIRY=604800

# Database
DATABASE_URL=postgres://user:password@localhost/ggen_idp

# RDF Store
GGEN_IDP_RDF_STORE=./data/rdf

# OAuth2
GGEN_IDP_OAUTH_BASE=http://localhost:3000

# Logging
RUST_LOG=debug
```

### Configuration File (Optional)

Create `idp.config.toml`:

```toml
[server]
host = "localhost"
port = 8000

[security]
jwt_expiry_secs = 3600
refresh_token_expiry_secs = 604800

[database]
url = "postgres://localhost/ggen_idp"

[oauth2]
redirect_base_url = "http://localhost:3000"
```

---

## 🚨 Production Deployment

### Pre-Deployment Checklist

- [ ] Set strong JWT_SECRET (min 32 characters)
- [ ] Configure HTTPS/TLS
- [ ] Set up PostgreSQL database
- [ ] Configure RDF store (production-grade Oxigraph or Virtuoso)
- [ ] Set up Redis for session store
- [ ] Enable CORS for authorized domains
- [ ] Configure rate limiting
- [ ] Set up monitoring and alerting
- [ ] Configure log aggregation (ELK, etc.)
- [ ] Back up audit logs regularly
- [ ] Implement GDPR compliance
- [ ] Security audit and pen testing

### Docker Deployment

```dockerfile
FROM rust:latest as builder
WORKDIR /app
COPY . .
RUN cargo build --release --bin ggen-idp-server

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libpq5
COPY --from=builder /app/target/release/ggen-idp-server /usr/local/bin/
EXPOSE 8000
CMD ["ggen-idp-server"]
```

```bash
docker build -t ggen-idp .
docker run -p 8000:8000 \
  -e GGEN_IDP_JWT_SECRET="..." \
  -e DATABASE_URL="..." \
  ggen-idp
```

---

## 📝 API Documentation

### Authentication Endpoints

- `POST /auth/{org_id}/register` - Register new user
- `POST /auth/{org_id}/login` - Authenticate user
- `POST /auth/{org_id}/refresh` - Refresh access token
- `POST /auth/{org_id}/logout/{session_id}` - Logout
- `POST /auth/{org_id}/change-password/{user_id}` - Change password

### RBAC Endpoints

- `POST /rbac/check/{user_id}` - Check permission
- `POST /rbac/assign/{user_id}/{org_id}` - Assign role
- `POST /rbac/revoke/{user_id}/{role_id}` - Revoke role
- `GET /rbac/roles/{user_id}/{org_id}` - List user roles

### OAuth2 / OIDC Endpoints

- `GET /oauth/authorize` - Authorization endpoint
- `POST /oauth/token` - Token endpoint
- `GET /oauth/.well-known/openid-configuration` - OIDC Discovery
- `GET /oauth/userinfo` - UserInfo endpoint
- `GET /oauth/jwks` - JSON Web Key Set

### Audit Endpoints

- `GET /audit/org/{org_id}` - Get organization audit logs
- `GET /audit/user/{user_id}` - Get user audit logs

---

## 🤝 Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new features
4. Ensure all tests pass
5. Submit a pull request

---

## 📄 License

MIT License - See LICENSE file for details

---

## 🆘 Support & Community

- **Issues**: GitHub Issues
- **Discussions**: GitHub Discussions
- **Documentation**: Full API docs at `/docs`
- **Email**: support@ggen.dev

---

## 🚀 Roadmap

### Phase 1 (Current)
- ✅ Core IDP system
- ✅ RBAC with ReBAC
- ✅ OAuth2/OIDC provider
- ✅ Session management
- ✅ Audit logging

### Phase 2 (Q2 2024)
- [ ] Next.js Admin Portal
- [ ] MFA/2FA implementation
- [ ] SAML support
- [ ] LDAP integration
- [ ] Kubernetes operator

### Phase 3 (Q3 2024)
- [ ] Advanced analytics
- [ ] Machine learning-based anomaly detection
- [ ] Policy-as-Code governance
- [ ] Advanced delegation
- [ ] Risk-based authentication

### Phase 4 (Q4 2024)
- [ ] Blockchain-based identity verification
- [ ] Zero-knowledge proofs
- [ ] Decentralized identity (W3C DID)
- [ ] Advanced federation

---

**Built with ❤️ for the ggen BPMN.js Marketplace ecosystem**
