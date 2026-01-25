# TAI Security Architecture

Comprehensive security design for TAI system.

## Security Layers

```
Layer 1: Network Security
  ├── TLS 1.3 encryption
  ├── mTLS for service-to-service
  └── Network policies (firewall rules)

Layer 2: Authentication
  ├── OAuth2 for external users
  ├── mTLS certificates for services
  └── API key validation

Layer 3: Authorization
  ├── RBAC (Role-Based Access Control)
  ├── Fine-grained IAM policies
  └── Resource-level permissions

Layer 4: Data Protection
  ├── Encryption at rest (Cloud KMS)
  ├── Encryption in transit (TLS 1.3)
  └── Field-level encryption (sensitive data)

Layer 5: Audit & Compliance
  ├── Immutable audit logs
  ├── Event sourcing
  └── Compliance monitoring
```

## Transport Security (TLS 1.3)

### External Connections (Client → API Gateway)

```yaml
apiVersion: networking.istio.io/v1beta1
kind: Gateway
metadata:
  name: tai-gateway
spec:
  selector:
    istio: ingressgateway
  servers:
  - port:
      number: 443
      name: https
      protocol: HTTPS
    tls:
      mode: SIMPLE
      minProtocolVersion: TLSV1_3  # TLS 1.3 only
      maxProtocolVersion: TLSV1_3
      credentialName: tai-tls-cert
      cipherSuites:
      - TLS_AES_256_GCM_SHA384       # Strongest first
      - TLS_CHACHA20_POLY1305_SHA256
      - TLS_AES_128_GCM_SHA256
    hosts:
    - "api.tai.example.com"
```

### Service-to-Service (mTLS)

```yaml
# Enable mTLS for entire namespace
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: default
  namespace: tai-system
spec:
  mtls:
    mode: STRICT  # Only accept mTLS
```

**Certificate Details:**
- Issuer: Istio (automated)
- Validity: 24 hours
- Key rotation: <1 second disruption
- CN: `service-name.namespace.svc.cluster.local`
- SAN: `spiffe://cluster.local/ns/namespace/sa/service-account`

## Authentication

### External Users (OAuth2)

```rust
pub async fn authenticate_user(token: &str) -> Result<User> {
    // Validate token with OAuth provider
    let client = reqwest::Client::new();
    let response = client
        .post("https://oauth.example.com/verify")
        .bearer_auth(token)
        .send()
        .await?;

    let claims: Claims = response.json().await?;

    // Check token expiration
    if claims.exp < SystemTime::now()
        .duration_since(UNIX_EPOCH)?
        .as_secs()
    {
        return Err("Token expired".into());
    }

    Ok(User {
        id: claims.sub,
        role: claims.role,
        email: claims.email,
    })
}
```

### Service-to-Service (mTLS)

Automatic via Istio:
1. Service CA issues certificate to pod
2. Certificate mounted via ServiceAccount token
3. Pod authenticates with certificate
4. No code changes needed

## Authorization (RBAC)

### Role Definitions

```rust
pub enum Role {
    Admin,              // Full access
    Operator,          // Read/write policies, submit signals
    Viewer,            // Read-only access
    ServiceAccount,    // Pod-to-pod (internal)
}

pub struct Permission {
    resource: String,   // "policy", "signal", "action"
    action: String,    // "create", "read", "update", "delete"
    scope: String,     // "own", "team", "all"
}
```

### Policy-Based Access Control

```yaml
# Istio AuthorizationPolicy
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: tai-authz
  namespace: tai-system
spec:
  selector:
    matchLabels:
      app: governor
  action: ALLOW
  rules:
  # Admin can do everything
  - from:
    - source:
        principals:
        - "cluster.local/ns/tai-system/sa/admin-service"
    to:
    - operation:
        methods: ["*"]
        paths: ["*"]

  # Operators can propose and query policies
  - from:
    - source:
        principals:
        - "cluster.local/ns/tai-system/sa/operator-service"
    to:
    - operation:
        methods: ["POST"]
        paths: ["/tai.Governor/ProposePolicy"]
    - operation:
        methods: ["GET"]
        paths: ["/tai.Governor/GetPolicies"]

  # Viewers can only read
  - from:
    - source:
        principals:
        - "cluster.local/ns/tai-system/sa/viewer-service"
    to:
    - operation:
        methods: ["GET"]
        paths: ["/tai.Governor/GetPolicies"]
```

## Secret Management

### Vault Integration

All secrets managed by Vault:

```
Application needs secret
  ↓
Vault Agent Injector (Kubernetes)
  ↓ (authenticates with Kubernetes ServiceAccount)
Vault Server
  ↓ (encrypts with Cloud KMS)
Cloud KMS
  ↓ (store in HSM)
Secret returned to pod
```

**Secrets Managed:**
- Database credentials (Firestore service account)
- API keys (third-party integrations)
- TLS certificates and keys
- OAuth client secrets
- Encryption keys

### Credential Rotation

```hcl
# Firestore service account rotation (30 days)
path "database/static-roles/firestore-tai" {
  database        = "firestore"
  username        = "tai-app@ggen-project.iam.gserviceaccount.com"
  rotation_period = "720h"  # 30 days
}

# API key rotation (90 days)
path "generic/data/tai/api-keys" {
  rotation_period = "2160h"  # 90 days
}

# TLS certificate rotation (90 days)
path "pki/issue/tai-services" {
  ttl = "2160h"  # 90 days
}
```

## Data Encryption

### Encryption at Rest

**Firestore:**
```yaml
apiVersion: firestore.googleapis.com/v1
kind: FirestoreDatabase
metadata:
  name: tai
spec:
  encryptionConfig:
    kmsKeyName: projects/ggen-project/locations/us/keyRings/firestore/cryptoKeys/default
```

**Redis:**
```yaml
# Enable encryption at rest
redis:
  auth: true                    # Password protected
  tls:
    enabled: true               # TLS for connections
    certFile: /etc/redis/certs/cert.pem
    keyFile: /etc/redis/certs/key.pem
```

### Encryption in Transit

All traffic encrypted with TLS 1.3:
- External traffic: Client → API Gateway (TLS 1.3)
- Service traffic: Service A → Service B (mTLS)
- Database traffic: Service → Firestore (TLS 1.3)
- Cache traffic: Service → Redis (TLS)

### Field-Level Encryption (Sensitive Data)

```rust
pub struct Policy {
    pub id: String,
    pub policy_type: String,
    #[serde(serialize_with = "encrypt", deserialize_with = "decrypt")]
    pub secret_rules: String,  // Encrypted field
}

fn encrypt<S>(value: &str, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let kms = CloudKmsClient::new();
    let encrypted = kms.encrypt("tai-secret-key", value.as_bytes())
        .map_err(serde::ser::Error::custom)?;
    serializer.serialize_bytes(&encrypted)
}

fn decrypt<'de, D>(deserializer: D) -> Result<String, D::Error>
where
    D: Deserializer<'de>,
{
    let bytes = Vec::<u8>::deserialize(deserializer)?;
    let kms = CloudKmsClient::new();
    let plaintext = kms.decrypt("tai-secret-key", &bytes)
        .map_err(serde::de::Error::custom)?;
    Ok(String::from_utf8(plaintext)
        .map_err(serde::de::Error::custom)?)
}
```

## Audit Logging

All actions logged with full context:

```rust
pub async fn log_action(
    user: &str,
    action: &str,
    resource: &str,
    result: &str,
    ip_address: &str,
) {
    let audit_event = AuditEvent {
        event_id: Uuid::new_v4().to_string(),
        timestamp: SystemTime::now(),
        user_id: user.to_string(),
        action: action.to_string(),
        resource: resource.to_string(),
        result: result.to_string(),
        ip_address: ip_address.to_string(),
        user_agent: request.user_agent().unwrap_or_default().to_string(),
    };

    // Store in immutable audit log
    firestore
        .collection("audit_logs")
        .add(serde_json::to_value(&audit_event)?)
        .await?;
}
```

**Audit Log Fields:**
- event_id: Unique identifier
- timestamp: ISO 8601 with nanosecond precision
- user_id: Who performed the action
- action: ProposePolicy, EnforcePolicy, etc.
- resource: Policy ID, signal ID, etc.
- result: "success", "denied", "error"
- ip_address: Source IP
- user_agent: Client information

**Retention:** 7 years (GDPR requirement)

## Compliance

### GDPR Compliance

**Right to be Forgotten:**
```rust
pub async fn delete_user_data(user_id: &str) -> Result<()> {
    // Delete user's own policies
    firestore
        .collection("policies")
        .where_eq("owner", user_id)
        .delete()
        .await?;

    // Delete audit logs mentioning user (with reason in audit)
    firestore
        .collection("audit_logs")
        .where_eq("user_id", user_id)
        .delete()
        .await?;

    // Log deletion for compliance
    log_action(
        "system",
        "delete_user_data",
        user_id,
        "GDPR right to be forgotten",
        "0.0.0.0",
    ).await;

    Ok(())
}
```

**Data Retention:**
- Operational data: Deleted after 2 years
- Audit logs: Retained 7 years (legal requirement)
- Backups: Deleted after retention period

### SOC 2 Compliance

**Control Framework:**
- Access control (IAM, RBAC)
- Encryption (data at rest and in transit)
- Audit logging (immutable logs)
- Change management (policy-driven)
- Incident response (alerting)
- Business continuity (multi-region failover)

## Security Scanning

### Container Image Scanning

```bash
# Enable vulnerability scanning
gcloud container images scan IMAGE_URL

# Automated scanning on push
gcloud container images describe IMAGE_URL \
  --show-package-vulnerability
```

### Code Scanning (SAST)

```bash
# Cargo audit
cargo audit

# Clippy with security lints
cargo clippy -- -D clippy::all -D clippy::pedantic
```

### Dependency Management

```rust
// Cargo.toml with security auditing
[dependencies]
tokio = "1.47"          # Async runtime
tonic = "0.12"          # gRPC framework
serde = "1.0"           # Serialization
# All dependencies audited for vulnerabilities
```

## Incident Response

### Security Alerts

```yaml
- name: UnauthorizedAccessAttempt
  expr: |
    increase(tai_authz_denied_total[5m]) > 10
  annotations:
    severity: critical
    action: "Review recent denials in audit log"

- name: HighErrorRate
  expr: |
    rate(tai_errors_total[5m]) > 0.05
  annotations:
    severity: warning
    action: "Check error logs for root cause"

- name: RateLimitExceeded
  expr: |
    rate(tai_ratelimit_exceeded_total[5m]) > 100
  annotations:
    severity: warning
    action: "Check for DDoS attack"
```

### Incident Playbook

1. **Detection:** Alert fires
2. **Investigation:** Check audit logs and metrics
3. **Containment:** Revoke tokens, block IPs if needed
4. **Recovery:** Deploy fix or rollback
5. **Post-incident:** Root cause analysis

## References
- OWASP Top 10: https://owasp.org/www-project-top-ten/
- NIST Cybersecurity Framework: https://www.nist.gov/cyberframework
- Google Cloud Security: https://cloud.google.com/security
- Istio Security: https://istio.io/latest/docs/concepts/security/
