# ADR-006: Secret Management with Vault and Cloud KMS

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Securely managing database passwords, API keys, certificates
**Deciders:** Security Infrastructure Team

## Problem Statement

TAI system requires secure management of:
- Database credentials (Firestore service accounts)
- API keys (external integrations)
- TLS certificates and keys
- Encryption keys
- OAuth secrets

Requirements:
- Automatic rotation
- Audit trail
- Fine-grained access control
- Encryption at rest and in transit
- Zero-knowledge architecture (application never sees raw secrets)

## Decision

**Multi-tier secret management:**
1. **HashiCorp Vault** (secret broker)
2. **Google Cloud KMS** (root key encryption)
3. **Kubernetes Secrets** (Vault-injected, encrypted)
4. **Sealed Secrets** for GitOps workflow

## Rationale

### Vault Strengths
- Dynamic secret generation (credentials created on-demand)
- Automatic rotation (change DB password every 30 days)
- Audit logging (who accessed what secret when)
- Encryption (Vault encrypts at-rest with Cloud KMS)
- Identity-based access (pods authenticate via JWT)
- Multi-auth methods (Kubernetes SA, JWT, OIDC)

### Cloud KMS Benefits
- HSM-backed key management
- Google-managed key rotation
- FIPS 140-2 compliance
- Audit logging in Cloud Audit Logs
- No keys leave Google infrastructure

### Kubernetes Secrets
- Automatically injected by Vault
- Never stored in etcd unencrypted (encryption key in KMS)
- Mounted as environment variables
- Pod RBAC controls access

## Architecture

```
Application Container
    ↓ (needs secret)
Vault Agent Injector
    ↓ (auto-inject at pod launch)
Kubernetes Secret (vault-managed)
    ↓ (encrypted with)
Google Cloud KMS
    ↓ (key stored in)
HSM (Hardware Security Module)
```

## Implementation

### Vault Configuration

```hcl
# Vault server configuration
storage "google" {
  bucket     = "tai-vault-backend"
  ha_enabled = "true"
}

seal "gcpckms" {
  project     = "ggen-project"
  region      = "us-central1"
  key_ring    = "vault-key-ring"
  crypto_key  = "vault-seal-key"
}

# Enable secret engines
path "secret/data/tai/*" {
  capabilities = ["read", "list"]
}

# Database secret engine
path "database/config/firestore" {
  capabilities = ["read", "update"]
}

path "database/roles/tai-app" {
  capabilities = ["read"]
}

# PKI (certificate management)
path "pki/issue/tai-services" {
  capabilities = ["create", "read"]
}
```

### Database Credential Rotation

```hcl
# Firestore service account rotation (30 days)
path "database/static-roles/firestore-tai" {
  database        = "firestore"
  username        = "terraform-sa@ggen-project.iam.gserviceaccount.com"
  rotation_period = "720h"  # 30 days
}

# Access policy
path "database/static-creds/firestore-tai" {
  capabilities = ["read"]
}
```

### Vault Agent Injector (Kubernetes)

```yaml
# Install Vault Agent Injector via Helm
apiVersion: v1
kind: Pod
metadata:
  annotations:
    vault.hashicorp.com/agent-inject: "true"
    vault.hashicorp.com/role: "tai-app"
    vault.hashicorp.com/agent-inject-secret-db: "secret/data/tai/firestore-credentials"
    vault.hashicorp.com/agent-inject-template-db: |
      {{ with secret "secret/data/tai/firestore-credentials" }}
      export FIRESTORE_CREDENTIALS={{ .Data.data.key_json }}
      {{ end }}
    vault.hashicorp.com/agent-inject-secret-api-key: "secret/data/tai/external-api-keys"
spec:
  containers:
  - name: governor
    image: gcr.io/ggen-project/governor
    env:
    # Vault injects these at pod launch
    - name: VAULT_ADDR
      value: "http://vault.vault.svc.cluster.local:8200"
    - name: VAULT_ROLE
      value: "tai-app"
    volumeMounts:
    - name: vault-token
      mountPath: /vault/secrets
  serviceAccountName: governor
```

### Kubernetes Authentiation (OIDC)

```hcl
# Kubernetes auth method
path "auth/kubernetes/config" {
  capabilities = ["create", "read", "update"]
}

# Kubernetes role (pod authenticates with its ServiceAccount)
path "auth/kubernetes/role/tai-app" {
  bound_service_account_names     = ["governor", "coordinator", "scheduler"]
  bound_service_account_namespaces = ["tai-system"]
  policies                         = ["tai-app-policy"]
  ttl                              = "24h"
}
```

### Pod Authentication Example

```rust
// Kubernetes auth: Pod authenticates using its ServiceAccount JWT
pub async fn authenticate_to_vault() -> Result<VaultClient> {
    // Read ServiceAccount JWT from pod
    let jwt = std::fs::read_to_string(
        "/var/run/secrets/kubernetes.io/serviceaccount/token"
    )?;

    let client = reqwest::Client::new();
    let response = client
        .post("http://vault:8200/v1/auth/kubernetes/login")
        .json(&json!({
            "jwt": jwt,
            "role": "tai-app"
        }))
        .send()
        .await?;

    let token = response.json::<TokenResponse>().await?.auth.client_token;

    Ok(VaultClient::new(
        "http://vault:8200",
        &token
    ))
}

// Fetch secret from Vault
pub async fn get_firestore_credentials(
    vault: &VaultClient
) -> Result<FirestoreCredentials> {
    let response = vault
        .read("secret/data/tai/firestore-credentials")
        .await?;

    Ok(response.data.data.into())
}

// Automatic credential refresh
pub async fn start_credential_refresh(vault: VaultClient) {
    let mut interval = tokio::time::interval(Duration::from_secs(3600)); // 1 hour

    loop {
        interval.tick().await;

        if let Ok(creds) = get_firestore_credentials(&vault).await {
            UPDATE_FIRESTORE_CREDS.store(creds);
        }
    }
}
```

### Secret Rotation Policies

```yaml
# Rotate API keys every 90 days
apiVersion: v1
kind: ConfigMap
metadata:
  name: vault-policies
data:
  rotation-policy.hcl: |
    # API Key rotation
    path "secret/data/tai/api-keys" {
      capabilities = ["read", "create", "update"]
    }

    # Database password rotation (automated by Vault)
    path "database/static-roles/firestore" {
      capabilities = ["read"]
    }

    # Certificate renewal (PKI)
    path "pki/issue/tai-services" {
      capabilities = ["create", "read"]
    }
```

### Sealed Secrets for GitOps

For non-sensitive secrets in Git:

```bash
# Encrypt secret with sealing key
echo -n 'my-db-password' | kubectl create secret generic \
  db-secret --dry-run=client --from-file=/dev/stdin \
  -o yaml | kubeseal -f -

# Result (safe to commit):
apiVersion: bitnami.com/v1alpha1
kind: SealedSecret
metadata:
  name: db-secret
spec:
  encryptedData:
    password: AgBXh4n+8K3J...
  template:
    metadata:
      name: db-secret
    type: Opaque
```

## Audit Trail

All secret access logged in Vault:

```json
{
  "timestamp": "2026-01-25T10:30:00Z",
  "request_path": "secret/data/tai/firestore-credentials",
  "request_operation": "read",
  "auth": {
    "client_token": "s.xxxxxxxxxxxx",
    "policies": ["tai-app-policy"],
    "metadata": {
      "role": "tai-app",
      "service_account": "governor",
      "namespace": "tai-system"
    }
  },
  "response": {
    "status": "200",
    "data_keys": ["key_json", "project_id"]
  }
}
```

## Secret Inventory

| Secret | Rotation | Store | Access |
|--------|----------|-------|--------|
| Firestore Service Account | 30d | Vault | Pods via Agent |
| External API Keys | 90d | Vault | Pods via Agent |
| TLS Certificates | 90d | Vault PKI | Istio auto-renew |
| Database Passwords | 30d | Vault | App env vars |
| JWT Signing Keys | Never (rotate in app) | Vault | Governor service |
| OAuth Client Secrets | 180d | Vault | Auth service |

## Consequences

### Positive
- Zero-knowledge: app never sees unencrypted secrets at rest
- Automatic rotation reduces breach impact
- Full audit trail for compliance
- Fine-grained access control
- Encryption with HSM-backed keys

### Negative
- Operational complexity (Vault management)
- Network latency for secret fetch
- Additional infrastructure (Vault HA cluster)
- Learning curve for team
- Cost of Cloud KMS

## High Availability

Vault setup:
- 3-node HA cluster (Raft backend)
- Replicated in 2 regions
- Automatic failover
- Cloud KMS as encryption layer

## Monitoring

- Vault seal status alerts
- Secret rotation success/failure
- Token expiration warnings
- Unauthorized access attempts
- Key rotation completion

## References
- [HashiCorp Vault Documentation](https://www.vaultproject.io/docs)
- [Vault Kubernetes Auth](https://www.vaultproject.io/docs/auth/kubernetes)
- [Google Cloud KMS](https://cloud.google.com/kms)
- [Sealed Secrets](https://github.com/bitnami-labs/sealed-secrets)
