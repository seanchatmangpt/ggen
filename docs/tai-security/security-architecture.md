<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TAI Security: Production-Grade Security Infrastructure](#tai-security-production-grade-security-infrastructure)
  - [Overview](#overview)
  - [Architecture](#architecture)
    - [Five-Layer Security Model](#five-layer-security-model)
  - [Module Reference](#module-reference)
    - [1. Vault Client (`vault_client.rs`)](#1-vault-client-vault_clientrs)
    - [2. Cloud KMS Client (`cloud_kms.rs`)](#2-cloud-kms-client-cloud_kmsrs)
    - [3. mTLS Manager (`mtls.rs`)](#3-mtls-manager-mtlsrs)
    - [4. Secret Rotation Manager (`secret_rotation.rs`)](#4-secret-rotation-manager-secret_rotationrs)
    - [5. Encryption Manager (`encryption.rs`)](#5-encryption-manager-encryptionrs)
  - [Security Principles](#security-principles)
    - [1. Defense in Depth](#1-defense-in-depth)
    - [2. Least Privilege](#2-least-privilege)
    - [3. Secrets Never in Code](#3-secrets-never-in-code)
    - [4. Automatic Rotation](#4-automatic-rotation)
    - [5. Observable](#5-observable)
  - [Deployment Guide](#deployment-guide)
    - [1. Initialize Vault](#1-initialize-vault)
    - [2. Configure Cloud KMS](#2-configure-cloud-kms)
    - [3. Configure mTLS](#3-configure-mtls)
    - [4. Application Configuration](#4-application-configuration)
  - [Best Practices](#best-practices)
    - [1. Key Management](#1-key-management)
    - [2. Secret Management](#2-secret-management)
    - [3. TLS/mTLS](#3-tlsmtls)
    - [4. Audit Logging](#4-audit-logging)
    - [5. Incident Response](#5-incident-response)
  - [Threat Model](#threat-model)
    - [Addressed Threats](#addressed-threats)
    - [Assumed Trust Boundaries](#assumed-trust-boundaries)
  - [Performance Characteristics](#performance-characteristics)
    - [Latency SLOs](#latency-slos)
    - [Throughput](#throughput)
  - [Compliance](#compliance)
  - [Integration Examples](#integration-examples)
    - [Database Credentials Rotation](#database-credentials-rotation)
    - [Encryption at Rest](#encryption-at-rest)
    - [Service-to-Service Authentication](#service-to-service-authentication)
  - [Troubleshooting](#troubleshooting)
    - [Common Issues](#common-issues)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TAI Security: Production-Grade Security Infrastructure

**Version**: 0.1.0
**Release Date**: January 2026
**Status**: Production-Ready

## Overview

TAI Security provides comprehensive, production-grade security infrastructure for ggen, implementing enterprise security patterns including HashiCorp Vault integration, Google Cloud KMS, mTLS, automatic secret rotation, and encryption at rest/in transit.

## Architecture

### Five-Layer Security Model

```
┌─────────────────────────────────────┐
│    Application Layer                 │
│  (Encrypted Data + Secrets)          │
├─────────────────────────────────────┤
│    Identity & Access Control Layer   │
│  (mTLS, OAuth2, JWT, API Keys)      │
├─────────────────────────────────────┤
│    Secret Management Layer           │
│  (Vault, Cloud KMS, Rotation)       │
├─────────────────────────────────────┤
│    Cryptography Layer               │
│  (AES-256-GCM, ChaCha20, HMAC)      │
├─────────────────────────────────────┤
│    Transport Layer                   │
│  (TLS 1.3, Certificate Pinning)     │
└─────────────────────────────────────┘
```

## Module Reference

### 1. Vault Client (`vault_client.rs`)

**Purpose**: Integration with HashiCorp Vault for dynamic secret generation and management

**Key Features**:
- Dynamic secret generation (database credentials, API keys)
- Secret versioning and history tracking
- Automatic lease renewal with configurable thresholds
- Encryption-as-a-Service via Transit engine
- Multiple authentication methods (Kubernetes, JWT, OAuth2)
- In-memory secret caching with cache statistics

**Example Usage**:

```rust
use tai_security::vault_client::{VaultClient, VaultConfig};
use std::time::Duration;

let config = VaultConfig {
    url: "https://vault.example.com".to_string(),
    token: "s.xxxxxxxxxxxxxxxxxxxx".to_string(),
    timeout: Duration::from_secs(10),
    namespace: None,
    tls_verify: true,
    ca_cert: None,
    renewal_threshold: 0.75, // Renew when 75% of TTL has passed
};

let client = VaultClient::new(config).await?;

// Retrieve static secret
let secret = client.get_secret("database/credentials").await?;

// Generate dynamic database credential
let db_cred = client.generate_database_credential("mysql-role").await?;

// Encrypt data with Transit engine
let encrypted = client.encrypt("transit/mysql", plaintext).await?;

// Decrypt data
let decrypted = client.decrypt("transit/mysql", &ciphertext).await?;
```

**Data Structures**:
- `Secret`: Generic KV secret with lease information
- `DatabaseCredential`: Database connection credentials with TTL
- `ApiKeySecret`: API key with scopes and expiration
- `VaultConfig`: Configuration for Vault connection

### 2. Cloud KMS Client (`cloud_kms.rs`)

**Purpose**: Integration with Google Cloud KMS for key management

**Key Features**:
- Key Ring and Cryptographic Key management
- Encryption/decryption operations (AES-256, RSA)
- Automatic key rotation with scheduled policies
- IAM-based access control
- Comprehensive audit logging
- Key versioning and lifecycle management

**Example Usage**:

```rust
use tai_security::cloud_kms::{CloudKmsClient, KmsConfig};
use std::time::Duration;

let config = KmsConfig {
    project_id: "my-project".to_string(),
    location: "us-central1".to_string(),
    credentials_path: Some("/path/to/credentials.json".to_string()),
    timeout: Duration::from_secs(30),
    audit_logging: true,
};

let client = CloudKmsClient::new(config).await?;

// Create key ring and key
let ring = client.create_key_ring("production").await?;
let key = client.create_key("production", "app-key", "AES_256_GCM").await?;

// Encrypt/decrypt
let encrypted = client.encrypt("production", "app-key", plaintext).await?;
let decrypted = client.decrypt("production", "app-key", &encrypted).await?;

// Rotate key
client.rotate_key("production", "app-key").await?;

// Get audit log
let audit_log = client.get_audit_log().await?;
```

**Data Structures**:
- `KeyRing`: Container for cryptographic keys
- `CryptographicKey`: Individual key with versioning
- `KeyVersion`: Versioned key with lifecycle status
- `EncryptedData`: Ciphertext with metadata
- `IamPolicy`: Access control bindings

### 3. mTLS Manager (`mtls.rs`)

**Purpose**: Manage mTLS certificates with automatic renewal and pinning

**Key Features**:
- Certificate provisioning from Vault, Let's Encrypt, or Google CAS
- Automatic renewal before expiry (configurable threshold)
- Certificate chain validation
- Certificate pinning for enhanced security
- Expiry alerts with multiple threshold levels (90, 60, 30, 14, 7, 1 days)
- Multiple certificate provider support

**Example Usage**:

```rust
use tai_security::mtls::{MtlsManager, RenewalConfig, CertificateProvider};
use std::path::Path;

let renewal_config = RenewalConfig {
    renew_before_days: 30,
    provider: CertificateProvider::Vault,
    provider_config: Default::default(),
    auto_renewal: true,
    check_interval: 24, // Check every 24 hours
};

let manager = MtlsManager::new(renewal_config);

// Load certificate from file
let cert = manager.load_certificate_from_file(Path::new("/path/to/cert.pem")).await?;

// Check expiry and get alerts
let alerts = manager.check_expiry().await?;
for alert in alerts {
    println!("ALERT: {} expires in {} days", alert.common_name, alert.days_until_expiry);
}

// Pin certificate for verification
let pin_hash = manager.pin_certificate(&cert.id).await?;

// Validate chain
let is_valid = manager.validate_certificate_chain(&cert.id).await?;
```

**Data Structures**:
- `Certificate`: Certificate metadata and PEM data
- `CertificateState`: Current state (Valid, Expired, ExpiringS soon, Revoked)
- `RenewalConfig`: Policy for certificate renewal
- `ExpiryAlert`: Alert when certificate approaches expiry
- `CertificateProvider`: Provider (Vault, LetsEncrypt, GoogleCas, SelfSigned, Manual)

### 4. Secret Rotation Manager (`secret_rotation.rs`)

**Purpose**: Automatic secret rotation with audit trails and rollback capability

**Key Features**:
- Register secrets for rotation with interval
- Atomic rotation with version tracking
- Graceful fallback with old secret in grace period
- Rollback to previous version if new secret fails
- Complete audit trail of all rotations
- Per-secret rotation policies

**Example Usage**:

```rust
use tai_security::secret_rotation::{SecretRotationManager, SecretType};

let manager = SecretRotationManager::new();

// Register secret for rotation
manager.register_secret(
    "db-password".to_string(),
    SecretType::DatabaseCredential,
    90, // Rotate every 90 days
).await?;

// Perform rotation
let new_version = manager.rotate_secret(
    "db-password",
    "new_password_hash".to_string(),
    "Scheduled rotation".to_string(),
).await?;

// Get history
let history = manager.get_rotation_history("db-password").await?;

// Rollback if needed
manager.rollback_secret("db-password", 1, "v2 caused issues".to_string()).await?;
```

**Data Structures**:
- `SecretType`: Type of secret (DatabaseCredential, ApiKey, Token, Certificate, etc.)
- `RotationPolicy`: Policy for automatic rotation
- `SecretVersion`: Individual version with status and lifecycle
- `VersionStatus`: Status (Pending, Active, GracePeriod, Deactivated, Failed)
- `RotationEvent`: Audit trail entry for rotation

### 5. Encryption Manager (`encryption.rs`)

**Purpose**: Encryption at rest with key derivation and HMAC

**Key Features**:
- AES-256-GCM and ChaCha20-Poly1305 symmetric encryption
- PBKDF2 and Argon2 key derivation
- HMAC-SHA256 for message authentication
- Password hashing with SHA-256
- Key versioning and rotation
- Secure key material storage (zeros after use)

**Example Usage**:

```rust
use tai_security::encryption::{EncryptionManager, EncryptionAlgorithm, KeyDerivationFunction};

let manager = EncryptionManager::new(
    EncryptionAlgorithm::Aes256Gcm,
    KeyDerivationFunction::Pbkdf2,
);

// Generate key
let key = manager.generate_key("app-key".to_string())?;

// Encrypt data
let plaintext = b"sensitive data";
let encrypted = manager.encrypt_aes256(plaintext, &key)?;

// Decrypt data
let decrypted = manager.decrypt_aes256(&encrypted, &key)?;
assert_eq!(decrypted, plaintext);

// Derive key from password
let salt = b"random-salt";
let key = manager.derive_key_from_password("user-key".to_string(), "password", salt)?;

// HMAC
let tag = manager.hmac_sha256(data, key.key_material())?;
assert!(manager.verify_hmac_sha256(data, key.key_material(), &tag)?);

// Password hashing
let hash = manager.hash_password("user-password")?;
```

**Data Structures**:
- `EncryptionAlgorithm`: Algorithm (AES256Gcm, ChaCha20Poly1305, RSA)
- `KeyDerivationFunction`: KDF (Pbkdf2, Argon2id, Scrypt)
- `EncryptionKey`: Key with ID, material, and lifecycle
- `EncryptedContent`: Ciphertext with IV, tag, AAD

## Security Principles

### 1. Defense in Depth

Multiple layers of security ensure that compromise at one layer doesn't expose the entire system:

- **Transport Layer**: TLS 1.3 with certificate pinning
- **Access Control**: OAuth2, JWT, API keys with RBAC
- **Secret Management**: Vault/KMS for secrets, never in code
- **Data Encryption**: AES-256-GCM for sensitive data at rest
- **Audit Trail**: Complete logging of all security operations

### 2. Least Privilege

- Secrets have minimal permissions, rotated frequently
- Services authenticate with service accounts (not shared credentials)
- Key access controlled via IAM with role-based permissions
- Every operation logged with actor, action, resource, outcome

### 3. Secrets Never in Code

- Configuration loaded from Vault/KMS at runtime
- Database credentials dynamically generated
- API keys rotated automatically
- Source code contains no hardcoded secrets

### 4. Automatic Rotation

- Database credentials rotate automatically (configurable interval)
- API keys rotate on schedule
- Certificates renewed before expiry
- Old secrets kept in grace period for graceful transition

### 5. Observable

- All operations logged with timestamps, actors, outcomes
- Audit trail maintained for compliance (HIPAA, SOC2, etc.)
- Metrics tracked: key rotations, failed operations, latency
- Alerts for security events (failed auth, expiry approaching)

## Deployment Guide

### 1. Initialize Vault

```bash
# Start Vault
vault server -dev

# Authenticate
export VAULT_ADDR='http://127.0.0.1:8200'
export VAULT_TOKEN='s.xxx'

# Enable database secrets engine
vault secrets enable database

# Configure database connection
vault write database/config/my-mysql \
  plugin_name=mysql-database-plugin \
  allowed_roles="readonly" \
  connection_url="{{username}}:{{password}}@tcp(localhost:3306)/" \
  username="vaultadmin" \
  password="vaultadminpassword"

# Create database role
vault write database/roles/readonly \
  db_name=my-mysql \
  creation_statements="CREATE USER '{{name}}'@'%' IDENTIFIED BY '{{password}}'; GRANT SELECT ON *.* TO '{{name}}'@'%';" \
  default_ttl="1h" \
  max_ttl="24h"
```

### 2. Configure Cloud KMS

```bash
# Create key ring
gcloud kms keyrings create production --location us-central1

# Create key
gcloud kms keys create app-key \
  --location us-central1 \
  --keyring production \
  --purpose encryption-decryption \
  --rotation-period 365d \
  --next-rotation-time 2026-01-25T00:00:00Z

# Grant service account access
gcloud kms keys add-iam-policy-binding app-key \
  --location us-central1 \
  --keyring production \
  --member serviceAccount:app@project.iam.gserviceaccount.com \
  --role roles/cloudkms.cryptoKeyEncrypterDecrypter
```

### 3. Configure mTLS

```bash
# Generate CA
openssl genrsa -out ca.key 4096
openssl req -new -x509 -days 365 -key ca.key -out ca.crt

# Generate server certificate
openssl req -new -key server.key -out server.csr
openssl x509 -req -days 365 -in server.csr -CA ca.crt -CAkey ca.key -out server.crt

# Or use Let's Encrypt
certbot certonly --standalone -d app.example.com
```

### 4. Application Configuration

```toml
[security]
# Vault configuration
vault_url = "https://vault.example.com"
vault_token = "${VAULT_TOKEN}"  # Set as environment variable
vault_namespace = "my-app"
vault_tls_verify = true

# Cloud KMS configuration
gcp_project = "my-project"
gcp_location = "us-central1"
gcp_credentials = "${GCP_CREDENTIALS}"  # JSON key file

# mTLS configuration
cert_path = "/etc/certs/app.crt"
key_path = "/etc/certs/app.key"
ca_path = "/etc/certs/ca.crt"
renew_before_days = 30

# Secret rotation
db_rotation_interval = 90  # days
api_key_rotation_interval = 30  # days
```

## Best Practices

### 1. Key Management

- Generate keys in HSM (Hardware Security Module)
- Never export private keys
- Use separate keys for different purposes
- Rotate keys automatically (annual minimum)
- Archive old keys for decryption of historical data

### 2. Secret Management

- Use Vault for all secrets
- Never hardcode secrets in config files
- Rotate database credentials frequently
- Use short-lived credentials (minutes to hours)
- Implement grace period for credential rotation

### 3. TLS/mTLS

- Use TLS 1.3 minimum (disable older versions)
- Certificate pinning for critical connections
- Verify entire certificate chain
- Monitor certificate expiry (alert at 90, 60, 30, 14, 7, 1 days)
- Use wildcard certs sparingly (security risk)

### 4. Audit Logging

- Log all cryptographic operations
- Include actor (service account, user)
- Include timestamp, request ID for tracing
- Protect audit logs (immutable storage)
- Archive and analyze for anomalies

### 5. Incident Response

- Have procedure for credential compromise
- Rotate all affected credentials immediately
- Review audit logs to identify impact
- Notify affected services
- Update incident timeline

## Threat Model

### Addressed Threats

1. **Credential Theft**: Secrets in Vault/KMS, never in code
2. **Man-in-the-Middle**: mTLS + certificate pinning
3. **Unauthorized Access**: IAM + RBAC + audit logging
4. **Data Breach**: Encryption at rest + encryption in transit
5. **Key Compromise**: Key rotation, separate keys per purpose
6. **Long-Lived Secrets**: Automatic rotation, short TTLs

### Assumed Trust Boundaries

- Google Cloud Platform infrastructure is secure
- Vault is deployed securely (HA cluster, encrypted storage)
- TLS infrastructure is trustworthy
- Service accounts are isolated

## Performance Characteristics

### Latency SLOs

- Vault get_secret: <100ms (with caching)
- Cloud KMS encrypt/decrypt: <500ms
- mTLS handshake: <200ms (with session resumption)
- Secret rotation: <2s (atomic operation)

### Throughput

- Encryption: ~500MB/s (AES-NI optimized)
- Hashing: ~1GB/s (SHA-256 with simd)
- Vault requests: ~1000/s (with connection pooling)
- KMS requests: ~100/s (API rate limits)

## Compliance

Supports compliance requirements for:

- **HIPAA**: Encryption at rest/transit, audit logging, access control
- **PCI-DSS**: Encryption, key rotation, access logging
- **SOC2**: Audit trails, monitoring, incident response
- **GDPR**: Data protection, encryption, audit trails
- **ISO 27001**: Information security management

## Integration Examples

### Database Credentials Rotation

```rust
// Every 90 days
let manager = SecretRotationManager::new();
manager.register_secret("db-password", SecretType::DatabaseCredential, 90).await?;

// Automatic:
// 1. Generate new credential via Vault
// 2. Test connection with new credential
// 3. Activate new credential (atomic)
// 4. Keep old credential for 24 hours (grace period)
// 5. Revoke old credential
// 6. Log rotation event
```

### Encryption at Rest

```rust
// Encrypt sensitive data before storing
let manager = EncryptionManager::new(
    EncryptionAlgorithm::Aes256Gcm,
    KeyDerivationFunction::Pbkdf2,
);

let key = manager.generate_key("db-encryption-key".to_string())?;
let encrypted = manager.encrypt_aes256(sensitive_data, &key)?;

// Store encrypted data in database
// Key stored in Cloud KMS (never in database)
```

### Service-to-Service Authentication

```rust
// Service A calls Service B via mTLS
let config = RenewalConfig {
    renew_before_days: 30,
    provider: CertificateProvider::Vault,
    provider_config: Default::default(),
    auto_renewal: true,
    check_interval: 24,
};

let manager = MtlsManager::new(config);
let cert = manager.load_certificate_from_file(Path::new("/etc/certs/app.crt")).await?;

// TLS client uses cert for authentication
// Server verifies cert against CA and pinned hashes
```

## Troubleshooting

### Common Issues

**Vault Connection Failed**
- Check Vault is running: `vault status`
- Verify token is valid: `vault token lookup`
- Check TLS certificates: `curl https://vault.example.com/v1/sys/health`

**Cloud KMS Permission Denied**
- Verify service account has roles: `gcloud projects get-iam-policy PROJECT_ID`
- Check key IAM policy: `gcloud kms keys get-iam-policy KEY_ID --location LOCATION --keyring KEYRING`

**Certificate Expiry Alerts Not Working**
- Check alert thresholds are set correctly
- Verify monitoring system is collecting metrics
- Check certificate is loaded and expiry date is parsed

**Secret Rotation Failing**
- Check provider (Vault/KMS) is accessible
- Verify service account has permissions
- Review audit logs for error details
- Test with manual rotation first

## References

- [HashiCorp Vault Documentation](https://www.vaultproject.io/docs)
- [Google Cloud KMS Documentation](https://cloud.google.com/docs/authentication/production)
- [NIST Cryptographic Standards](https://csrc.nist.gov/publications)
- [OWASP Cryptographic Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Cryptographic_Storage_Cheat_Sheet.html)

---

**Last Updated**: January 25, 2026
**Maintainer**: TAI Security Team
