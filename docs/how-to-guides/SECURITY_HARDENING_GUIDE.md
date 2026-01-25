<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Security Hardening Guide for Generated Code](#security-hardening-guide-for-generated-code)
  - [Code Generation Security](#code-generation-security)
    - [Principle 1: Validate All Inputs](#principle-1-validate-all-inputs)
    - [Principle 2: Use Type System](#principle-2-use-type-system)
    - [Principle 3: Escape Output](#principle-3-escape-output)
  - [API Security](#api-security)
    - [1. Authentication & Authorization](#1-authentication--authorization)
    - [2. Rate Limiting](#2-rate-limiting)
    - [3. CORS Configuration](#3-cors-configuration)
    - [4. HTTPS Enforcement](#4-https-enforcement)
  - [Database Security](#database-security)
    - [1. Encrypted Connections](#1-encrypted-connections)
    - [2. Secret Management](#2-secret-management)
    - [3. Row-Level Security (RLS)](#3-row-level-security-rls)
    - [4. Prepared Statements](#4-prepared-statements)
  - [ORM Security](#orm-security)
    - [Safe Query Building](#safe-query-building)
    - [Relationship Traversal](#relationship-traversal)
  - [RDF/Ontology Security](#rdfontology-security)
    - [1. Schema Validation](#1-schema-validation)
    - [2. Ontology Integrity](#2-ontology-integrity)
  - [Dependency Security](#dependency-security)
    - [Version Pinning](#version-pinning)
    - [Regular Audits](#regular-audits)
  - [Secrets Management](#secrets-management)
    - [CI/CD Secrets](#cicd-secrets)
    - [.env Files](#env-files)
  - [Logging & Monitoring](#logging--monitoring)
    - [Security Logging](#security-logging)
    - [Sensitive Data Masking](#sensitive-data-masking)
  - [Testing Security](#testing-security)
    - [Security Tests](#security-tests)
  - [Deployment Security Checklist](#deployment-security-checklist)
  - [References](#references)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Security Hardening Guide for Generated Code

> Best practices for securing generated APIs, databases, and infrastructure

## Code Generation Security

### Principle 1: Validate All Inputs

```rust
// ❌ WRONG: Direct SQL from SPARQL
let sparql = user_input;  // XSS/injection risk
let results = graph.query(&sparql)?;

// ✅ CORRECT: Parameterized queries
let query = "SELECT ?name WHERE { ?user :name ?name . FILTER (?user = ?id) }";
let results = graph.query_with_params(query, &[("id", user_id)])?;
```

### Principle 2: Use Type System

```typescript
// ❌ WRONG: String-based validation
function validateEmail(email: string): boolean {
  return email.includes("@");  // Weak!
}

// ✅ CORRECT: Type-safe validation
import { z } from "zod";

const EmailSchema = z.string().email();
const email = EmailSchema.parse(userInput);  // Throws on invalid
```

### Principle 3: Escape Output

```tera
{# ❌ WRONG: Unescaped HTML #}
{{ user_content }}

{# ✅ CORRECT: Auto-escaped (Tera default) #}
{{ user_content | escape }}

{# For JSON output #}
{{ user_content | json }}
```

## API Security

### 1. Authentication & Authorization

```typescript
// Generated Express route with auth middleware
router.post("/api/posts", authenticateUser, (req, res) => {
  // Verify user owns resource
  const userId = req.user.id;

  if (req.body.authorId !== userId) {
    return res.status(403).json({ error: "Forbidden" });
  }

  // Proceed with operation
});
```

### 2. Rate Limiting

```python
# Generated FastAPI with rate limiting
from slowapi import Limiter
from slowapi.util import get_remote_address

limiter = Limiter(key_func=get_remote_address)

@app.get("/api/posts")
@limiter.limit("100/minute")
async def list_posts(request: Request):
    """Rate-limited endpoint"""
    pass
```

### 3. CORS Configuration

```javascript
// Restrict cross-origin requests
app.use(cors({
  origin: process.env.ALLOWED_ORIGINS?.split(','),
  credentials: true,
  methods: ['GET', 'POST', 'PUT', 'DELETE'],
  allowedHeaders: ['Content-Type', 'Authorization'],
  maxAge: 86400
}));
```

### 4. HTTPS Enforcement

```javascript
// Enforce HTTPS in production
app.use((req, res, next) => {
  if (process.env.NODE_ENV === 'production' && !req.secure) {
    return res.redirect('https://' + req.get('host') + req.url);
  }
  next();
});
```

## Database Security

### 1. Encrypted Connections

```typescript
// PostgreSQL with SSL
const connectionString = `postgresql://user:pass@host:5432/db?sslmode=require`;
```

### 2. Secret Management

```bash
# ❌ WRONG: Hardcoded secrets
export DATABASE_PASSWORD="my-secret"

# ✅ CORRECT: Environment variables
export DATABASE_PASSWORD=$(aws secretsmanager get-secret-value --secret-id db-password)
```

### 3. Row-Level Security (RLS)

```sql
-- Restrict users to their own data
ALTER TABLE posts ENABLE ROW LEVEL SECURITY;

CREATE POLICY user_isolation ON posts
  USING (author_id = current_user_id());

CREATE POLICY user_isolation ON comments
  USING (author_id = current_user_id());
```

### 4. Prepared Statements

```python
# ✅ SQLAlchemy automatically uses parameterized queries
from sqlalchemy import select

query = select(User).where(User.id == user_id)
user = db.session.execute(query).scalar_one()
```

## ORM Security

### Safe Query Building

```typescript
// ✅ Type-safe Drizzle ORM
import { eq } from "drizzle-orm";

const user = await db
  .select()
  .from(users)
  .where(eq(users.id, userId));  // Parameterized
```

### Relationship Traversal

```python
# ❌ WRONG: N+1 query vulnerability
for user in users:
    posts = user.posts  # Lazy load (multiple queries!)

# ✅ CORRECT: Eager load relationships
users = (
    db.query(User)
    .options(joinedload(User.posts))
    .all()
)
```

## RDF/Ontology Security

### 1. Schema Validation

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .

:UserShape a sh:NodeShape ;
  sh:targetClass :User ;
  sh:property [
    sh:path :email ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
    sh:pattern "^[^@]+@[^@]+\\.[^@]+$"  # Email format
  ] ;
  sh:property [
    sh:path :password ;
    sh:minLength 12 ;  # Minimum password length
    sh:minCount 1 ;
  ] .
```

### 2. Ontology Integrity

```rust
// Validate ontology before generation
let validator = SparqlValidator::new();
let result = validator.validate(&graph, &shapes)?;

if !result.passed {
  eprintln!("Ontology validation failed:");
  for violation in &result.violations {
    eprintln!("  {}: {}", violation.focus_node, violation.message);
  }
  std::process::exit(1);
}
```

## Dependency Security

### Version Pinning

```json
{
  "dependencies": {
    "express": "4.18.2",
    "pydantic": "==2.5.0",
    "@types/node": "20.10.0"
  }
}
```

### Regular Audits

```bash
# JavaScript
npm audit
npm update

# Python
pip install pip-audit
pip-audit

# Rust
cargo audit
```

## Secrets Management

### CI/CD Secrets

```yaml
# GitHub Actions
env:
  DATABASE_PASSWORD: ${{ secrets.DB_PASSWORD }}
  API_KEY: ${{ secrets.API_KEY }}
  JWT_SECRET: ${{ secrets.JWT_SECRET }}
```

### .env Files

```bash
# .gitignore
.env
.env.local
.env.*.local
```

```python
# Load from environment
import os
from dotenv import load_dotenv

load_dotenv()

DATABASE_URL = os.getenv("DATABASE_URL")
if not DATABASE_URL:
    raise ValueError("DATABASE_URL not set")
```

## Logging & Monitoring

### Security Logging

```typescript
// Log authentication attempts
app.use((req, res, next) => {
  console.log({
    timestamp: new Date().toISOString(),
    method: req.method,
    path: req.path,
    user: req.user?.id || 'anonymous',
    ip: req.ip,
    status: res.statusCode
  });
  next();
});
```

### Sensitive Data Masking

```python
import logging

# Don't log passwords, tokens, etc.
class SensitiveDataFilter(logging.Filter):
    def filter(self, record):
        record.msg = str(record.msg).replace(
            os.getenv('DATABASE_PASSWORD', ''),
            '***'
        )
        return True

logging.getLogger().addFilter(SensitiveDataFilter())
```

## Testing Security

### Security Tests

```javascript
describe('Security', () => {
  it('should reject SQL injection', async () => {
    const result = await request(app)
      .get("/api/users")
      .query({ id: "1; DROP TABLE users;" });

    expect(result.status).toBe(400);
  });

  it('should enforce authentication', async () => {
    const result = await request(app)
      .post("/api/posts")
      .send({ title: "Hacked" });

    expect(result.status).toBe(401);
  });

  it('should check authorization', async () => {
    const result = await request(app)
      .put("/api/posts/other-users-post")
      .set('Authorization', `Bearer ${token}`);

    expect(result.status).toBe(403);
  });
});
```

## Deployment Security Checklist

```yaml
security-checklist:
  authentication:
    - [ ] All endpoints require auth except /health
    - [ ] JWT tokens use strong secrets
    - [ ] Tokens have short expiration (15min-1hr)

  data-protection:
    - [ ] Database connections use TLS
    - [ ] Passwords hashed with bcrypt/argon2
    - [ ] Secrets stored in secrets manager
    - [ ] PII encrypted at rest

  api-security:
    - [ ] CORS properly configured
    - [ ] Rate limiting enabled
    - [ ] Input validation on all endpoints
    - [ ] HTTPS enforced in production

  code:
    - [ ] No hardcoded credentials
    - [ ] Dependencies up-to-date
    - [ ] No sql/command injection vulnerabilities
    - [ ] OWASP top 10 checklist passed

  operations:
    - [ ] Security logging enabled
    - [ ] Monitoring/alerting configured
    - [ ] Regular security audits scheduled
    - [ ] Incident response plan documented
```

## References

- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [CWE Top 25](https://cwe.mitre.org/top25/)
- [Node.js Security Best Practices](https://nodejs.org/en/docs/guides/security/)
- [FastAPI Security](https://fastapi.tiangolo.com/tutorial/security/)
- [PostgreSQL Security](https://www.postgresql.org/docs/current/sql-syntax.html)

## See Also

- [Troubleshooting Guide](../troubleshooting/TROUBLESHOOTING_GUIDE.md)
- [GITHUB_ACTIONS_CICD_EXAMPLE.md](./GITHUB_ACTIONS_CICD_EXAMPLE.md) - Secure pipelines
