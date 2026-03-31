# Authentication & Authorization Architecture - erlmcp

## Overview

erlmcp implements a multi-layered authentication and authorization framework using GCP Identity and Access Management (IAM), Cloud Run identity tokens, and Firestore-based tenant isolation. This document describes the architecture, authentication flows, and authorization policies.

## Architecture Layers

```
┌──────────────────────────────────────────────────────┐
│ External Services (API Clients, Billing Systems)     │
└──────────────────┬───────────────────────────────────┘
                   │ Identity Token
                   ▼
┌──────────────────────────────────────────────────────┐
│ Cloud Run Invocation Layer                           │
│ - Service-to-service authentication                  │
│ - Identity token validation                          │
│ - Rate limiting & quota enforcement                  │
└──────────────────┬───────────────────────────────────┘
                   │ Cloud Identity Token
                   ▼
┌──────────────────────────────────────────────────────┐
│ TAIEA Service (erlmcp)                               │
│ - Extract tenant context from token                  │
│ - Apply authorization policies                       │
│ - Enforce tenant isolation                           │
└──────────────────┬───────────────────────────────────┘
                   │ Firestore Partition Key
                   ▼
┌──────────────────────────────────────────────────────┐
│ Data Access Layer (Firestore)                        │
│ - Tenant-scoped document access                      │
│ - Audit logging for all operations                   │
│ - Encryption at rest & in transit                    │
└──────────────────────────────────────────────────────┘
```

## Authentication Methods

### 1. Service-to-Service Authentication

**Used by**: API servers, billing systems, internal microservices

**Flow**:
```
1. Service account requests access token via GCP IAM
   POST /iamcredentials.googleapis.com/v1/projects/-/serviceAccounts/{SA}/generateAccessToken

2. Service account receives Bearer token (lifetime: 1 hour)

3. Service invokes Cloud Run with Authorization header
   curl -H "Authorization: Bearer $TOKEN" https://taiea.run.app/api/endpoint

4. Cloud Run verifies token signature and audience (aud: https://taiea.run.app)

5. Cloud Run extracts service account email from token claims
   "email": "taiea-api-client@project-id.iam.gserviceaccount.com"
```

**Token Structure**:
```json
{
  "iss": "https://accounts.google.com",
  "sub": "1234567890",
  "aud": "https://taiea.run.app",
  "iat": 1516239022,
  "exp": 1516242622,
  "email": "taiea-api-client@project-id.iam.gserviceaccount.com",
  "email_verified": true
}
```

**Expiration**: 1 hour (standard Google access tokens)

### 2. Cloud Run Identity Token Authentication

**Used by**: External callers with Cloud Run service account credentials

**Flow**:
```
1. Obtain service account key file (taiea-api-client-key.json)

2. Request identity token via GCP IAM
   gcloud auth application-default print-access-token

3. Invoke Cloud Run service
   curl -H "Authorization: Bearer $IDENTITY_TOKEN" https://taiea.run.app/api/endpoint

4. Cloud Run validates token signature and audience

5. Extract caller identity from token sub claim
```

**Key Difference**: Identity tokens (instead of access tokens) are used directly, not exchanged for API tokens.

### 3. Application-Level API Key Authentication (Future Phase 2)

**Planned for**: External customer API access
- API keys stored in Secret Manager
- Tenant-specific API keys with scoped permissions
- Key rotation policies (90-day expiration)
- Rate limiting per API key

## Authorization Policies

### 1. Cloud Run Invoker Role

**Who**: Service accounts, users, systems allowed to invoke the service

**Roles**:
- `roles/run.invoker` - Can invoke TAIEA Cloud Run service
- `roles/run.admin` - Can manage Cloud Run deployments and traffic

**Assignment**:
```hcl
# API client can invoke service
google_cloud_run_service_iam_member "api_client_invoker"

# Billing service can invoke service
google_cloud_run_service_iam_member "billing_service_invoker"

# Admin can manage service
google_cloud_run_service_iam_member "admin_invoker"
```

### 2. Firestore Data Access Control

**Roles**:
- `roles/datastore.user` - Read/write Firestore documents
- `roles/datastore.admin` - Manage databases, indexes, backups
- `roles/datastore.importExportAdmin` - Import/export data

**Tenant Isolation**: Enforced via Firestore partitions
- Each tenant has a collection root: `/tenants/{tenant_id}`
- Service accounts can only access their assigned tenant's data
- Query-level filtering applies tenant partition key automatically
- Firestore security rules prevent cross-tenant access

**Example Firestore Structure**:
```
/tenants/{tenant_id}/
  /ledgers/{ledger_id}/
    /entries/{entry_id}
  /receipts/{receipt_id}
  /settings/
```

### 3. Custom Roles for Tenant Isolation

**tenant-firestore-reader**:
- `datastore.databases.get`
- `datastore.entities.get`
- `datastore.entities.list`
- `datastore.indexes.get`
- `datastore.indexes.list`

**tenant-firestore-writer**:
- `datastore.databases.get`
- `datastore.entities.create`
- `datastore.entities.delete`
- `datastore.entities.get`
- `datastore.entities.list`
- `datastore.entities.update`
- `datastore.indexes.get`
- `datastore.indexes.list`

### 4. Logging & Monitoring Access

**Roles**:
- `roles/logging.logWriter` - Write application logs (INFO, WARN, ERROR)
- `roles/logging.viewer` - View logs (read-only)
- `roles/monitoring.metricWriter` - Write custom metrics
- `roles/cloudtrace.agent` - Write trace data

## Service Accounts

### TAIEA Cloud Run Service Account (taiea-sa)
- **Purpose**: Runs the TAIEA Cloud Run service
- **Permissions**:
  - Firestore: datastore.user (read/write)
  - Logging: logging.logWriter
  - Monitoring: monitoring.metricWriter
  - Cloud Trace: cloudtrace.agent
  - Pub/Sub: pubsub.subscriber, pubsub.publisher (optional)
- **Scope**: Only application-internal operations

### Admin Service Account (taiea-admin)
- **Purpose**: Infrastructure management and deployments
- **Permissions**:
  - Firestore: datastore.admin
  - Cloud Run: run.admin
  - IAM: serviceAccountUser, serviceAccountTokenCreator
  - Logging: logging.admin
  - Monitoring: monitoring.admin
- **Usage**: CI/CD pipelines, Terraform deployments, infrastructure changes
- **Security**: Key stored in CI/CD secret management, rotated quarterly

### API Client Service Account (taiea-api-client)
- **Purpose**: External API client invoking TAIEA service
- **Permissions**:
  - Cloud Run: run.invoker
  - Firestore: datastore.user (tenant-scoped)
  - Logging: logging.logWriter
- **Usage**: API gateway, billing system, partner integrations
- **Security**: Key stored securely, never committed to version control

### Billing Service Account (taiea-billing)
- **Purpose**: Billing operations and ledger management
- **Permissions**:
  - Cloud Run: run.invoker
  - Firestore: datastore.user
  - BigQuery: bigquery.dataEditor (if enabled)
- **Usage**: Billing system invoking TAIEA receipt processing
- **Security**: Dedicated service account for audit trail separation

## Tenant Isolation Strategy

### Firestore Partition Keys

Every Firestore document includes a tenant partition key:

```
/tenants/{tenant_id}/
  ledger_id: "..."
  company_name: "..."
  billing_email: "..."
  created_at: "..."
  // All subordinate collections
  /ledgers/{ledger_id}/
    /entries/{entry_id}
  /receipts/{receipt_id}
```

### Query Filtering

All Firestore queries automatically filter by tenant ID:

```rust
// Pseudocode: Extracted from Cloud Run token
let tenant_id = extract_tenant_from_token(request);

// Query automatically filtered
db.collection("tenants")
  .document(&tenant_id)
  .collection("ledgers")
  .get()
```

### Security Rules (Firestore)

```json
{
  "rules": {
    "tenants": {
      "{tenant_id}": {
        "allow read, write": "if request.auth.claims.tenant_id == tenant_id"
      }
    }
  }
}
```

## API Request Flow with Authentication

### Step 1: External Service Requests Token

```bash
# Using gcloud CLI
gcloud auth application-default print-access-token

# Or using OAuth 2.0 service account flow
curl -X POST https://iamcredentials.googleapis.com/v1/projects/-/serviceAccounts/taiea-api-client@project.iam.gserviceaccount.com:generateAccessToken \
  -H "Authorization: Bearer $ROOT_TOKEN" \
  -d '{"lifetime": "3600s"}'
```

### Step 2: External Service Invokes TAIEA

```bash
TOKEN=$(gcloud auth application-default print-access-token)

curl -X POST https://taiea.run.app/api/ledgers \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "ledger_id": "ledger-1",
    "entries": [...]
  }'
```

### Step 3: Cloud Run Validates Token

```
1. Extract Bearer token from Authorization header
2. Verify token signature using Google's public keys
3. Validate aud claim matches Cloud Run service URL
4. Check token expiration (exp < current_time)
5. Extract service account email from token
6. Verify service account has roles/run.invoker IAM binding
```

### Step 4: TAIEA Application Extracts Tenant Context

```rust
// From Cloud Run environment variable (set by platform)
let service_account_email = std::env::var("GOOGLE_SERVICE_ACCOUNT_EMAIL")?;

// From request context (injected by Cloud Run)
let request_identity = extract_cloud_identity(&request)?;

// Determine tenant based on service account
// API Client -> Maps to specific tenant(s)
let tenant_id = get_tenant_for_service_account(&service_account_email)?;

// Apply tenant isolation in data queries
query_firestore(&format!("tenants/{}/ledgers", tenant_id))
```

### Step 5: Authorization Check

```rust
// Verify service account has required roles
let permissions = list_service_account_iam_bindings(&service_account_email)?;

// Check if invoker role is present
if !permissions.contains("roles/run.invoker") {
  return Err(AuthorizationError::NotAuthorized);
}

// Verify Firestore access
if !permissions.contains("roles/datastore.user") {
  return Err(AuthorizationError::DataAccessDenied);
}

// Proceed with request processing
process_request(&request, &tenant_id)
```

### Step 6: Firestore Data Access

```rust
// All operations scoped to tenant
let entries = db
  .collection("tenants")
  .document(&tenant_id)
  .collection("ledgers")
  .document(&ledger_id)
  .collection("entries")
  .get()
  .await?;

// Write operations also scoped
db.collection("tenants")
  .document(&tenant_id)
  .collection("receipts")
  .add(receipt_data)
  .await?;
```

## Future: OAuth 2.0 / OIDC Integration (Phase 2)

### Customer Authentication

**Flow**:
```
1. Customer navigates to erlmcp console
2. OAuth 2.0 Authorization Code Flow initiated
3. Redirects to GCP Cloud Identity / Google Sign-In
4. Customer authenticates with email/password or federated identity
5. Redirects back to erlmcp with authorization code
6. erlmcp exchanges code for ID token and access token
7. ID token contains customer identity and tenant claim
```

### Customer Authorization

**Token Claims**:
```json
{
  "iss": "https://accounts.google.com",
  "sub": "customer-user-id",
  "aud": "erlmcp-web-app-client-id",
  "iat": 1516239022,
  "exp": 1516242622,
  "email": "customer@example.com",
  "email_verified": true,
  "custom:tenant_id": "customer-tenant-123",
  "custom:roles": ["admin", "viewer"]
}
```

**Role-Based Access Control (RBAC)**:
- Admin: Full access to tenant settings, billing, users
- Editor: Can create/modify receipts and ledgers
- Viewer: Read-only access to receipts and reports
- Auditor: Read-only access to audit logs

## Audit Logging

### Cloud Audit Logs

All IAM and data access operations logged to Cloud Audit Logs:

```
{
  "protoPayload": {
    "serviceName": "datastore.googleapis.com",
    "methodName": "google.datastore.v1.Datastore.Lookup",
    "resourceName": "projects/taiea/databases/(default)/documents/tenants/tenant-1/receipts/receipt-123",
    "request": {...},
    "response": {...}
  },
  "insertId": "...",
  "severity": "DEFAULT",
  "sourceIP": "...",
  "userAgent": "...",
  "timestamp": "2024-01-26T10:30:00Z"
}
```

### Application-Level Audit Trail

```rust
pub struct AuditLog {
  pub timestamp: DateTime<Utc>,
  pub tenant_id: String,
  pub service_account: String,
  pub operation: String,
  pub resource: String,
  pub result: String, // "SUCCESS" or "FAILURE"
  pub details: serde_json::Value,
}

// Write to dedicated audit collection
db.collection("tenants")
  .document(&tenant_id)
  .collection("audit_logs")
  .add(audit_log)
  .await?;
```

## Security Best Practices

### 1. Principle of Least Privilege

- Each service account has minimum required permissions
- Separate accounts for different roles (admin, api-client, billing)
- No root/owner service accounts used in applications
- Regular permission audits via IAM audit tool

### 2. Key Management

- Service account keys stored in encrypted secret management
- Keys rotated every 90 days
- Separate keys for development, staging, production
- Keys never committed to version control
- Keys logged when created/rotated

### 3. Network Security

- Cloud Run enforces HTTPS only (TLS 1.2+)
- VPC Service Controls restrict data exfiltration (enterprise)
- Private IP Google Cloud connector for Firestore (optional)
- Cloud Armor DDoS protection (optional)

### 4. Token Security

- Access tokens short-lived (1 hour max)
- Identity tokens validated on every request
- Token audience claim strictly validated
- Token expiration checked immediately

### 5. Data Isolation

- Firestore partitions by tenant
- Security rules prevent cross-tenant access
- Encryption at rest with Google-managed keys (default) or customer-managed keys (CMK)
- Encryption in transit with TLS 1.2+

### 6. Audit & Monitoring

- All API calls logged to Cloud Audit Logs
- Access anomalies detected via Cloud Monitoring
- Failed authentication attempts logged and alerted
- Quarterly audit trail review

## Troubleshooting Authentication Issues

### 403 Unauthorized: Service Account Not Authorized

**Cause**: Service account lacks `roles/run.invoker` IAM binding

**Fix**:
```bash
gcloud iam service-accounts add-iam-policy-binding \
  taiea-api-client@project-id.iam.gserviceaccount.com \
  --member="serviceAccount:taiea-api-client@project-id.iam.gserviceaccount.com" \
  --role="roles/run.invoker"
```

### 401 Unauthorized: Invalid Token

**Cause**: Token expired, invalid signature, or wrong audience

**Fix**:
- Request fresh token (max age 1 hour)
- Verify token audience matches Cloud Run URL
- Verify token signer is Google's public key
- Check service account key validity

### 403 Forbidden: Firestore Access Denied

**Cause**: Service account lacks `roles/datastore.user` permission

**Fix**:
```bash
gcloud projects add-iam-policy-binding project-id \
  --member="serviceAccount:taiea-api-client@project-id.iam.gserviceaccount.com" \
  --role="roles/datastore.user"
```

### Tenant Isolation Not Working

**Cause**: Tenant ID not extracted from request context

**Fix**:
- Verify Cloud Run automatically injects `X-Goog-IAM-Authority-Selector` header
- Confirm application extracts tenant ID from service account email mapping
- Check Firestore security rules enable tenant filtering

## Deployment Checklist

- [ ] Create service accounts (taiea, taiea-admin, taiea-api-client, taiea-billing)
- [ ] Grant required IAM roles to service accounts
- [ ] Create custom roles for tenant isolation
- [ ] Generate and secure service account keys
- [ ] Configure Firestore security rules for tenant access
- [ ] Enable Cloud Audit Logs for data access
- [ ] Set up Cloud Monitoring for authentication failures
- [ ] Test authentication flows for all service accounts
- [ ] Document tenant-to-service-account mappings
- [ ] Schedule quarterly key rotation
- [ ] Review audit logs for unauthorized access attempts
