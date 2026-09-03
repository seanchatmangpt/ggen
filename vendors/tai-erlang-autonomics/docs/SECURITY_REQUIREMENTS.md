# TAI Erlang Autonomics: Security Requirements & Configuration

**Document Version:** 1.0
**Date:** January 25, 2026
**Classification:** INTERNAL - CONFIDENTIAL

---

## Executive Summary

This document outlines the complete security requirements for deploying TAI Erlang Autonomics to production GCP environments. All requirements are categorized by priority level and implementation status.

---

## 1. Network Security Requirements

### 1.1 TLS/HTTPS Enforcement

**Requirement ID:** NET-001
**Priority:** CRITICAL
**Status:** NOT IMPLEMENTED

#### Requirements:
- All HTTP endpoints must use TLS 1.2 or higher
- Certificate must be valid and issued by trusted CA
- Certificate pinning recommended for GCP services
- Strong cipher suites only (no weak/null ciphers)
- HSTS header with `max-age=31536000`

#### Implementation:
```erlang
%% config/prod.sys.config
{tls_enabled, true},
{tls_cert_file, {env, "TLS_CERT_FILE"}},
{tls_key_file, {env, "TLS_KEY_FILE"}},
{tls_versions, ['tlsv1.2', 'tlsv1.3']},
{tls_ciphers, [
    {ecdhe_ecdsa, aes_256_gcm, null, sha384},
    {ecdhe_rsa, aes_256_gcm, null, sha384},
    {ecdhe_ecdsa, aes_128_gcm, null, sha256},
    {ecdhe_rsa, aes_128_gcm, null, sha256}
]},
```

#### Verification:
```bash
# Verify TLS enforcement
curl -I https://tai-autonomics-service/health
# Should return 200 OK (not 301 redirect)

# Verify certificate
openssl s_client -connect tai-autonomics-service:443 -tls1_2
# Should show valid certificate chain

# Verify HSTS header
curl -I https://tai-autonomics-service/health | grep strict-transport-security
# Should output: strict-transport-security: max-age=31536000; includeSubDomains; preload
```

---

### 1.2 Network Isolation (VPC)

**Requirement ID:** NET-002
**Priority:** HIGH
**Status:** NOT IMPLEMENTED

#### Requirements:
- Cloud Run service must run within VPC
- Private IP only for internal communication
- Cloud Armor protection for DDoS
- WAF rules for common attacks

#### Implementation (Terraform):
```hcl
# Network
resource "google_compute_network" "tai" {
  name                    = "tai-autonomics-network"
  auto_create_subnetworks = false
  routing_mode            = "REGIONAL"
}

resource "google_compute_subnetwork" "tai" {
  name          = "tai-autonomics-subnet"
  ip_cidr_range = "10.0.0.0/24"
  region        = var.region
  network       = google_compute_network.tai.id

  private_ip_google_access = true
}

# VPC Connector
resource "google_vpc_access_connector" "tai" {
  name          = "tai-autonomics-connector"
  ip_cidr_range = "10.8.0.0/28"
  network       = google_compute_network.tai.name
  region        = var.region
  machine_type  = "f1-micro"
}

# Cloud Armor
resource "google_compute_security_policy" "tai_armor" {
  name = "tai-autonomics-armor"

  # Rate limiting rule
  rules {
    action   = "rate_based_ban"
    priority = 100
    match {
      versioned_expr = "CEL"
      cel_options {
        recommend_rules = true
      }
    }
    rate_limit_options {
      conform_action = "allow"
      exceed_action  = "deny(429)"
      rate_limit_threshold {
        count        = 100
        interval_sec = 60
      }
      ban_durationSec = 600
    }
  }

  # Allow rules
  rules {
    action   = "allow"
    priority = 1000
  }
}

# Cloud Run with VPC
resource "google_cloud_run_service" "tai_autonomics" {
  # ... existing configuration ...

  template {
    metadata {
      annotations = {
        "run.googleapis.com/vpc-access-connector"  = google_vpc_access_connector.tai.id
        "run.googleapis.com/vpc-access-egress"     = "all-traffic"
      }
    }
  }
}
```

---

### 1.3 Port & Firewall Rules

**Requirement ID:** NET-003
**Priority:** HIGH

#### Requirements:
- Only HTTPS (443) exposed externally
- HTTP (80) for redirect only (optional)
- No SSH access to production instances
- Firewall rules based on source IP whitelist

#### Implementation (Terraform):
```hcl
# Firewall rules
resource "google_compute_firewall" "tai_https_only" {
  name    = "tai-autonomics-https-only"
  network = google_compute_network.tai.name

  allow {
    protocol = "tcp"
    ports    = ["443"]
  }

  source_ranges = ["0.0.0.0/0"]  # Cloud Armor will rate limit
}

resource "google_compute_firewall" "tai_internal_only" {
  name    = "tai-autonomics-internal"
  network = google_compute_network.tai.name

  allow {
    protocol = "tcp"
    ports    = ["8080"]  # Internal communication
  }

  source_ranges = ["10.0.0.0/8"]  # Internal VPC only
}

resource "google_compute_firewall" "tai_deny_ssh" {
  name    = "tai-autonomics-deny-ssh"
  network = google_compute_network.tai.name

  deny {
    protocol = "tcp"
    ports    = ["22"]
  }

  source_ranges = ["0.0.0.0/0"]
}
```

---

## 2. Authentication & Authorization Requirements

### 2.1 Service Account Identity

**Requirement ID:** AUTH-001
**Priority:** CRITICAL
**Status:** NOT IMPLEMENTED

#### Requirements:
- All service-to-service calls must use OAuth 2.0 bearer tokens
- Service account email must be validated
- Workload Identity for non-GCP services
- Token expiration must be enforced (max 1 hour)

#### Implementation:
```erlang
%% Enforce OAuth 2.0 for all GCP API calls
{service_account_validation, true},
{oauth_token_expiration_seconds, 3600},
{oauth_clock_skew_seconds, 30},
{trusted_service_accounts, [
    <<"tai-autonomics@project.iam.gserviceaccount.com">>,
    <<"ci-cd@project.iam.gserviceaccount.com">>
]},
```

---

### 2.2 JWT Bearer Token Authentication

**Requirement ID:** AUTH-002
**Priority:** CRITICAL
**Status:** PARTIAL

#### Requirements:
- All HTTP endpoints require JWT bearer token
- Token must be issued by trusted issuer
- Token must include required claims:
  - `exp` - expiration time
  - `iat` - issued at time
  - `iss` - issuer
  - `aud` - audience
  - `sub` - subject/user ID
- Token verification must be cryptographically validated

#### Implementation:
```erlang
{jwt_enabled, true},
{jwt_verification_required, true},  % MANDATORY
{jwt_public_key, {env, "JWT_PUBLIC_KEY"}},
{jwt_min_algorithm, <<"RS256">>},
{jwt_required_claims, [<<"exp">>, <<"iat">>, <<"iss">>, <<"aud">>, <<"sub">>]},
{jwt_clock_skew_seconds, 30},
{jwt_max_age_seconds, 3600},
{trusted_jwt_issuers, [
    <<"https://accounts.google.com">>,
    <<"https://identity.googleapis.com">>
]},
{valid_jwt_audiences, [
    <<"tai-autonomics">>,
    <<"https://tai-autonomics.example.com">>
]},
```

---

### 2.3 Role-Based Access Control (RBAC)

**Requirement ID:** AUTH-003
**Priority:** HIGH
**Status:** NOT IMPLEMENTED

#### Requirements:
- All actions must be authorized by role
- Minimum 4 roles: admin, operator, viewer, system
- Admin: all actions
- Operator: grant, revoke, suspend entitlements
- Viewer: read-only access
- System: internal only, bypasses checks

#### Implementation:
```erlang
{rbac_enabled, true},
{roles, [
    {admin, [
        grant_entitlement, revoke_entitlement, suspend_entitlement,
        delete_entitlement, view_entitlements, manage_users
    ]},
    {operator, [
        grant_entitlement, revoke_entitlement, suspend_entitlement,
        view_entitlements
    ]},
    {viewer, [
        view_entitlements
    ]},
    {system, [
        all  % Bypass all checks
    ]}
]},
```

---

### 2.4 Tenant Isolation

**Requirement ID:** AUTH-004
**Priority:** CRITICAL

#### Requirements:
- Multi-tenant: users can only access their tenant
- System accounts can bypass tenant check
- All queries must be filtered by tenant ID
- Cross-tenant access must be explicitly denied

#### Implementation:
```erlang
{multi_tenant_enabled, true},
{tenant_isolation_enforced, true},
{system_account_roles, [system]},  % Can bypass tenant check
```

---

## 3. Input Validation & Injection Prevention

### 3.1 Request Size Limits

**Requirement ID:** INPUT-001
**Priority:** HIGH
**Status:** PARTIAL

#### Requirements:
- Max HTTP body size: 1 MB
- Max Firestore document size: 1 MB
- Max nested JSON depth: 10 levels
- Max field length: varies by field

#### Implementation:
```erlang
{max_http_body_size, 1048576},  % 1 MB
{max_json_depth, 10},
{max_field_length, #{
    <<"tenant_id">> => 128,
    <<"entitlement_id">> => 128,
    <<"action">> => 64,
    <<"reason">> => 1024
}},
```

---

### 3.2 Data Format Validation

**Requirement ID:** INPUT-002
**Priority:** CRITICAL

#### Requirements:
- UUID fields must be valid UUID format
- Action field must be whitelisted
- All binary fields must be valid UTF-8
- No null bytes allowed
- No control characters allowed

#### Implementation:
```erlang
{uuid_validation_enabled, true},
{action_whitelist, [<<"grant">>, <<"revoke">>, <<"suspend">>]},
{binary_validation_enabled, true},
{reject_null_bytes, true},
{reject_control_characters, true},
```

---

### 3.3 SQL/NoSQL Injection Prevention

**Requirement ID:** INPUT-003
**Priority:** CRITICAL

#### Requirements:
- All inputs must be parameterized (no string concatenation)
- No Firestore query injection possible
- Reject SQL-like patterns in fields
- Reject common injection payloads

#### Implementation:
```erlang
{injection_detection_enabled, true},
{reject_sql_patterns, true},
{reject_ldap_patterns, true},
{reject_xpath_patterns, true},
```

---

## 4. Cryptography & Hashing

### 4.1 Hash Algorithm Standards

**Requirement ID:** CRYPTO-001
**Priority:** CRITICAL

#### Requirements:
- Use SHA-256 for hashing (minimum)
- No MD5 or SHA1 allowed
- No weak hashing algorithms
- Hash chain must include sequence numbers

#### Implementation:
```erlang
{hash_algorithm, sha256},  % Only SHA-256
{min_hash_bits, 256},
{hash_chain_include_sequence, true},
{hash_chain_include_timestamp, true},
```

---

### 4.2 Cryptographic Signing

**Requirement ID:** CRYPTO-002
**Priority:** CRITICAL
**Status:** NOT IMPLEMENTED

#### Requirements:
- All receipts must be HMAC-signed
- Use HMAC-SHA256
- Signature verification must be mandatory
- Signing key must be rotated regularly

#### Implementation:
```erlang
{receipt_signing_enabled, true},
{signing_algorithm, hmac_sha256},
{signing_key, {env, "RECEIPT_SIGNING_KEY"}},
{signing_key_rotation_days, 30},
```

---

### 4.3 Encryption at Rest

**Requirement ID:** CRYPTO-003
**Priority:** HIGH
**Status:** PARTIAL

#### Requirements:
- Firestore documents must be encrypted
- Use AES-256-GCM
- Encryption keys must be rotated regularly
- Sensitive fields encrypted before transmission

#### Implementation:
```erlang
{encryption_enabled, true},
{encryption_algorithm, aes_256_gcm},
{encryption_key_size, 256},
{encryption_key, {env, "ENCRYPTION_KEY"}},
{encryption_key_rotation_days, 30},
{encrypted_fields, [
    <<"tenant_id">>,
    <<"entitlement_id">>,
    <<"customer_id">>
]},
```

---

## 5. Secrets Management

### 5.1 Secret Storage

**Requirement ID:** SECRET-001
**Priority:** CRITICAL
**Status:** NOT IMPLEMENTED

#### Requirements:
- No hardcoded secrets in code
- No secrets in configuration files
- Use Google Secret Manager for all secrets
- Use environment variables for configuration

#### Implementation:
```erlang
{secrets_backend, gcp_secret_manager},
{secret_manager_project_id, {env, "GCP_PROJECT_ID"}},
{secret_manager_enabled, true},

% Never hardcode secrets:
% WRONG: {api_key, "sk_prod_123"},
% RIGHT: {api_key, {env, "API_KEY"}},

{jwt_public_key, {env, "JWT_PUBLIC_KEY"}},
{jwt_signing_key, {env, "JWT_SIGNING_KEY"}},
{firestore_encryption_key, {env, "FIRESTORE_ENCRYPTION_KEY"}},
{receipt_signing_key, {env, "RECEIPT_SIGNING_KEY"}},
{field_encryption_key, {env, "FIELD_ENCRYPTION_KEY"}},
```

---

### 5.2 Secret Rotation

**Requirement ID:** SECRET-002
**Priority:** HIGH
**Status:** NOT IMPLEMENTED

#### Requirements:
- Automatic secret rotation every 30 days
- Rotation must not disrupt service
- Both old and new secrets valid during transition
- Audit trail for all rotations

#### Implementation:
```erlang
{secret_rotation_enabled, true},
{secret_rotation_interval_days, 30},
{secret_rotation_buffer_days, 2},  % Time to accept both old/new
{secret_rotation_audit_log, true},
```

---

## 6. Audit & Compliance

### 6.1 Audit Logging

**Requirement ID:** AUDIT-001
**Priority:** CRITICAL
**Status:** PARTIAL

#### Requirements:
- All authorization decisions logged
- All data access logged
- All modifications logged
- Audit logs immutable (append-only)
- Retention: minimum 90 days

#### Implementation:
```erlang
{audit_logging_enabled, true},
{audit_backend, firestore},  % Append-only store
{audit_collection, <<"audit_logs">>},
{audit_retention_days, 90},
{audit_events, [
    authorization_attempt,
    authorization_success,
    authorization_failure,
    data_access,
    data_modification,
    secret_rotation,
    configuration_change
]},
```

---

### 6.2 GDPR Compliance

**Requirement ID:** AUDIT-002
**Priority:** HIGH
**Status:** NOT IMPLEMENTED

#### Requirements:
- Log all personal data access
- Log all personal data modifications
- Support GDPR erasure requests
- Support data portability requests
- Demonstrate lawful basis for processing

#### Implementation:
```erlang
{gdpr_compliance_enabled, true},
{personal_data_logging_enabled, true},
{gdpr_erasure_enabled, true},
{gdpr_data_portability_enabled, true},
{data_retention_days, 90},
{lawful_basis, contract},  % contract | consent | legitimate_interest
```

---

## 7. GCP Security Integration

### 7.1 Service Account IAM Permissions

**Requirement ID:** GCP-001
**Priority:** CRITICAL
**Status:** PARTIAL

#### Requirements:
- Minimum permissions only (least privilege)
- No project-level viewer role
- No editor or admin roles
- Custom role with specific permissions only

#### Terraform Implementation:
```hcl
# Custom minimal role
resource "google_project_iam_custom_role" "tai_minimal" {
  role_id = "tai_autonomics_minimal"
  title   = "TAI Autonomics - Minimal Role"
  permissions = [
    # Firestore
    "datastore.databases.get",
    "datastore.entities.create",
    "datastore.entities.get",
    "datastore.entities.list",
    "datastore.entities.update",
    "datastore.entities.delete",  # For GDPR erasure only

    # Pub/Sub
    "pubsub.subscriptions.get",
    "pubsub.subscriptions.pull",
    "pubsub.subscriptions.consume",
    "pubsub.subscriptions.update",

    # Logging
    "logging.logEntries.create",

    # Metrics
    "monitoring.timeSeries.create",

    # Tracing
    "cloudtrace.traces.patch",

    # No secretmanager.secretAccessor - handled via Workload Identity
  ]
}

resource "google_project_iam_member" "tai_custom_role" {
  project = var.project_id
  role    = google_project_iam_custom_role.tai_minimal.id
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

# No grant of viewer/editor/admin roles!
# NEVER: roles/viewer, roles/editor, roles/admin
```

---

### 7.2 Workload Identity

**Requirement ID:** GCP-002
**Priority:** HIGH
**Status:** NOT IMPLEMENTED

#### Requirements:
- Use Workload Identity instead of service account keys
- Bind Cloud Run service to Kubernetes service account
- No long-lived service account keys

#### Terraform Implementation:
```hcl
# Workload Identity setup
resource "google_iam_workload_identity_pool" "tai" {
  location              = "global"
  workload_identity_pool_id = "tai-autonomics"
  display_name          = "TAI Autonomics Workload Identity Pool"
}

resource "google_iam_workload_identity_pool_provider" "cloud_run" {
  workload_identity_pool_id          = google_iam_workload_identity_pool.tai.workload_identity_pool_id
  workload_identity_pool_provider_id = "cloud-run"
  display_name                       = "Cloud Run Provider"

  attribute_mapping = {
    "google.subject"       = "assertion.sub"
    "attribute.service_account" = "assertion.service_account"
  }

  oidc {
    issuer_uri = "https://accounts.google.com"
  }
}

# Bind service account to Workload Identity
resource "google_service_account_iam_member" "tai_workload_identity" {
  service_account_id = google_service_account.tai_autonomics.name
  role               = "roles/iam.workloadIdentityUser"
  member             = "principalSet://goog/subject/cloud-run-${google_cloud_run_service.tai_autonomics.name}"
}

# NO service account keys!
# NEVER create: google_service_account_key
```

---

### 7.3 Secret Manager Integration

**Requirement ID:** GCP-003
**Priority:** HIGH
**Status:** NOT IMPLEMENTED

#### Requirements:
- All secrets stored in Secret Manager
- Runtime access via metadata server
- No secrets in environment variables at startup
- Automatic rotation configured

#### Terraform Implementation:
```hcl
# Create secrets in Secret Manager
resource "google_secret_manager_secret" "jwt_public_key" {
  secret_id = "jwt-public-key"
  labels = {
    service = "tai-autonomics"
  }
}

resource "google_secret_manager_secret_version" "jwt_public_key_v1" {
  secret      = google_secret_manager_secret.jwt_public_key.id
  secret_data = file("${path.module}/secrets/jwt_public_key.pem")
}

resource "google_secret_manager_secret_iam_member" "jwt_public_key_access" {
  secret_id = google_secret_manager_secret.jwt_public_key.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

# Automatic rotation
resource "google_secret_manager_secret" "encryption_key" {
  secret_id = "encryption-key"
  rotation {
    rotation_period    = "2592000s"  # 30 days
    next_rotation_time = timeadd(timestamp(), "30d")
  }
}
```

---

### 7.4 Cloud Run Security Configuration

**Requirement ID:** GCP-004
**Priority:** CRITICAL

#### Requirements:
- Authenticated access only (no public access)
- Secure environment variables
- Resource limits enforced
- Health checks configured

#### Terraform Implementation:
```hcl
# Cloud Run with security hardening
resource "google_cloud_run_service" "tai_autonomics" {
  name     = "tai-autonomics"
  location = var.region

  template {
    spec {
      service_account_name = google_service_account.tai_autonomics.email

      containers {
        image = "${var.region}-docker.pkg.dev/${var.project_id}/tai-autonomics/tai-autonomics:${var.image_tag}"

        # Secure environment variables from Secret Manager
        env {
          name  = "JWT_PUBLIC_KEY"
          value_from {
            secret_key_ref {
              name = google_secret_manager_secret.jwt_public_key.secret_id
              key  = "latest"
            }
          }
        }

        # Resource limits for DoS protection
        resources {
          limits = {
            cpu    = "2"
            memory = "2Gi"
          }
        }

        # Security context
        security_context {
          run_as_non_root = true
          allow_privilege_escalation = false
          read_only_root_filesystem = true
        }
      }
    }

    metadata {
      annotations = {
        "run.googleapis.com/vpc-access-connector"  = google_vpc_access_connector.tai.id
        "run.googleapis.com/vpc-access-egress"     = "all-traffic"
        "run.googleapis.com/execution-environment" = "gen2"
      }
    }
  }

  # Authenticated access ONLY
  # NO public access (no google_cloud_run_service_iam_member for allUsers)

  depends_on = [google_project_service.required_apis]
}

# Authenticated IAM only
resource "google_cloud_run_service_iam_member" "authenticated" {
  service  = google_cloud_run_service.tai_autonomics.name
  location = google_cloud_run_service.tai_autonomics.location
  role     = "roles/run.invoker"
  member   = "allAuthenticatedUsers"
}

# Specific service account access
resource "google_cloud_run_service_iam_member" "service_account" {
  service  = google_cloud_run_service.tai_autonomics.name
  location = google_cloud_run_service.tai_autonomics.location
  role     = "roles/run.invoker"
  member   = "serviceAccount:${google_service_account.ci_cd.email}"
}
```

---

## 8. Monitoring & Alerting

### 8.1 Security Event Monitoring

**Requirement ID:** MONITOR-001
**Priority:** HIGH
**Status:** PARTIAL

#### Requirements:
- Alert on failed authentication attempts
- Alert on authorization failures
- Alert on rate limit exceeded
- Alert on signature verification failures

#### Implementation:
```erlang
{monitoring_enabled, true},
{alert_on_auth_failure, true},
{alert_on_authz_failure, true},
{alert_on_rate_limit, true},
{alert_on_signature_failure, true},
{alert_notification_channels, [
    "projects/PROJECT_ID/notificationChannels/1234567890"
]},
```

---

## 9. Deployment Security Checklist

Before deploying to production, verify:

```markdown
## Pre-Deployment Security Verification

### Authentication & Authorization
- [ ] JWT validation enabled and tested
- [ ] RBAC configured with all roles
- [ ] Service account validation working
- [ ] Tenant isolation verified
- [ ] Cross-tenant access blocked

### Encryption & Cryptography
- [ ] TLS 1.2+ enforced
- [ ] Valid certificate installed
- [ ] HSTS header present
- [ ] Encryption at rest enabled
- [ ] Encryption keys rotated
- [ ] Receipt signing enabled

### Input Validation
- [ ] Size limits enforced
- [ ] Format validation working
- [ ] Injection attacks prevented
- [ ] Null byte rejection working
- [ ] Field length validation working

### Secrets Management
- [ ] No hardcoded secrets in code
- [ ] All secrets in Secret Manager
- [ ] Secret rotation configured
- [ ] Environment variables set
- [ ] Service account permissions minimal

### Audit & Logging
- [ ] Audit logging enabled
- [ ] GDPR compliance configured
- [ ] Data retention policies set
- [ ] Sensitive data not logged
- [ ] Logs immutable (append-only)

### GCP Integration
- [ ] Cloud Run authenticated access only
- [ ] VPC configured
- [ ] Cloud Armor rules active
- [ ] Service account least-privilege
- [ ] Firewall rules restrictive
- [ ] Secret Manager access configured
- [ ] Monitoring/alerting enabled

### Testing
- [ ] Security tests passing
- [ ] Load tests passing
- [ ] Integration tests passing
- [ ] Penetration testing completed
- [ ] Compliance verification completed
```

---

## 10. Security Configuration Summary

### Production Configuration Template

```erlang
%% config/prod.sys.config - Complete security configuration

[
  {tai_autonomics, [
    %% ============================================================
    %% TLS/HTTPS
    %% ============================================================
    {tls_enabled, true},
    {tls_cert_file, {env, "TLS_CERT_FILE"}},
    {tls_key_file, {env, "TLS_KEY_FILE"}},
    {tls_versions, ['tlsv1.2', 'tlsv1.3']},

    %% ============================================================
    %% Authentication
    %% ============================================================
    {jwt_enabled, true},
    {jwt_verification_required, true},
    {jwt_public_key, {env, "JWT_PUBLIC_KEY"}},
    {jwt_min_algorithm, <<"RS256">>},
    {jwt_clock_skew_seconds, 30},
    {jwt_max_age_seconds, 3600},
    {trusted_jwt_issuers, [
        <<"https://accounts.google.com">>,
        <<"https://identity.googleapis.com">>
    ]},
    {valid_jwt_audiences, [<<"tai-autonomics">>]},

    %% Service Account Validation
    {service_account_validation, true},
    {oauth_token_expiration_seconds, 3600},
    {trusted_service_accounts, [
        <<"tai-autonomics@PROJECT_ID.iam.gserviceaccount.com">>
    ]},

    %% ============================================================
    %% Authorization
    %% ============================================================
    {rbac_enabled, true},
    {multi_tenant_enabled, true},
    {tenant_isolation_enforced, true},

    %% ============================================================
    %% Input Validation
    %% ============================================================
    {max_http_body_size, 1048576},
    {max_json_depth, 10},
    {uuid_validation_enabled, true},
    {action_whitelist, [<<"grant">>, <<"revoke">>, <<"suspend">>]},
    {injection_detection_enabled, true},

    %% ============================================================
    %% Cryptography
    %% ============================================================
    {hash_algorithm, sha256},
    {receipt_signing_enabled, true},
    {signing_algorithm, hmac_sha256},
    {signing_key, {env, "RECEIPT_SIGNING_KEY"}},
    {encryption_enabled, true},
    {encryption_algorithm, aes_256_gcm},
    {encryption_key, {env, "ENCRYPTION_KEY"}},
    {encryption_key_rotation_days, 30},

    %% ============================================================
    %% Secrets Management
    %% ============================================================
    {secrets_backend, gcp_secret_manager},
    {secret_manager_enabled, true},
    {secret_rotation_enabled, true},
    {secret_rotation_interval_days, 30},

    %% ============================================================
    %% Audit & Compliance
    %% ============================================================
    {audit_logging_enabled, true},
    {audit_backend, firestore},
    {audit_retention_days, 90},
    {gdpr_compliance_enabled, true},
    {personal_data_logging_enabled, true},
    {gdpr_erasure_enabled, true},

    %% ============================================================
    %% GCP Integration
    %% ============================================================
    {gcp_secret_manager_enabled, true},
    {gcp_project_id, {env, "GCP_PROJECT_ID"}},
    {firestore_encryption_enabled, true},
    {cloud_armor_enabled, true},

    %% ============================================================
    %% Monitoring & Alerting
    %% ============================================================
    {monitoring_enabled, true},
    {alert_on_auth_failure, true},
    {alert_on_authz_failure, true},
    {alert_on_rate_limit, true}
  ]}
].
```

---

## Conclusion

All security requirements must be implemented and verified before production deployment. Regular security audits and penetration testing are essential for maintaining security posture.

**Key Principles:**
1. **Zero Trust:** Verify all access
2. **Least Privilege:** Minimum permissions only
3. **Defense in Depth:** Multiple security layers
4. **Audit Everything:** Complete audit trail
5. **Automate:** Security testing in CI/CD

---

**Version:** 1.0
**Last Updated:** January 25, 2026
**Next Review:** July 25, 2026
