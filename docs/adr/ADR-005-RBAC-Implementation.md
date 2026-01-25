<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ADR-005: Role-Based Access Control (RBAC) Implementation](#adr-005-role-based-access-control-rbac-implementation)
  - [Problem Statement](#problem-statement)
  - [Decision](#decision)
  - [Rationale](#rationale)
    - [Layer 1: Kubernetes RBAC](#layer-1-kubernetes-rbac)
    - [Layer 2: Istio AuthorizationPolicy](#layer-2-istio-authorizationpolicy)
    - [Layer 3: Application RBAC](#layer-3-application-rbac)
  - [Implementation](#implementation)
    - [Layer 1: Kubernetes ServiceAccounts](#layer-1-kubernetes-serviceaccounts)
    - [Layer 2: Istio AuthorizationPolicy](#layer-2-istio-authorizationpolicy-1)
    - [Layer 3: Application-Level RBAC](#layer-3-application-level-rbac)
    - [gRPC Service Implementation with Authorization](#grpc-service-implementation-with-authorization)
  - [External Client Authentication](#external-client-authentication)
    - [OAuth2 / JWT for External Clients](#oauth2--jwt-for-external-clients)
  - [Audit Trail](#audit-trail)
  - [Consequences](#consequences)
    - [Positive](#positive)
    - [Negative](#negative)
  - [Monitoring](#monitoring)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ADR-005: Role-Based Access Control (RBAC) Implementation

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Implementing authorization for service access and resource operations
**Deciders:** Security Architecture Team

## Problem Statement

TAI system needs:
- Service authentication (who you are)
- Authorization (what you can do)
- Role hierarchy (admin > operator > viewer)
- Fine-grained permissions (policy read/write, signal submission)
- Audit trail (who did what when)
- Token-based access from external clients

## Decision

**Three-layer authorization:**
1. **Kubernetes ServiceAccount RBAC** (pod-to-pod)
2. **Istio AuthorizationPolicy** (service-to-service mTLS)
3. **Application-level RBAC** (resource and action permissions)

Integration with Cloud IAM for external clients.

## Rationale

### Layer 1: Kubernetes RBAC
- Pod identity via ServiceAccount
- Network policies enforce which pods can talk
- Automatic certificate injection via mTLS

### Layer 2: Istio AuthorizationPolicy
- Enforces at network boundary
- Based on certificate principals
- Deny by default, allow explicit

### Layer 3: Application RBAC
- Coarse permissions at Kubernetes layer
- Fine-grained business rules in application
- Audit logging per action

## Implementation

### Layer 1: Kubernetes ServiceAccounts

```yaml
# Create ServiceAccounts for each service
apiVersion: v1
kind: ServiceAccount
metadata:
  name: governor
  namespace: tai-system
---
apiVersion: v1
kind: ServiceAccount
metadata:
  name: coordinator
  namespace: tai-system
---
apiVersion: v1
kind: ServiceAccount
metadata:
  name: scheduler
  namespace: tai-system
```

### Layer 2: Istio AuthorizationPolicy

```yaml
# Default deny all traffic (zero-trust)
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: default-deny
  namespace: tai-system
spec:
  {} # Empty spec denies everything

---
# Governor → (Coordinator, Scheduler)
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: coordinator-from-governor
  namespace: tai-system
spec:
  selector:
    matchLabels:
      app: coordinator
  rules:
  - from:
    - source:
        principals:
        - cluster.local/ns/tai-system/sa/governor
    to:
    - operation:
        methods: ["POST", "GET"]
        paths:
        - "/tai.Coordinator/*"

---
# Scheduler → (Coordinator)
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: coordinator-from-scheduler
  namespace: tai-system
spec:
  selector:
    matchLabels:
      app: coordinator
  rules:
  - from:
    - source:
        principals:
        - cluster.local/ns/tai-system/sa/scheduler
    to:
    - operation:
        methods: ["POST"]
        paths:
        - "/tai.Coordinator/RequestAction"

---
# External clients → Ingress (validated at gateway)
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: ingress-authz
  namespace: istio-system
spec:
  selector:
    matchLabels:
      app: ingressgateway
  rules:
  - from:
    - source:
        principals:
        - cluster.local/ns/tai-system/sa/external-client
    to:
    - operation:
        paths:
        - "/tai.Governor/*"
```

### Layer 3: Application-Level RBAC

```rust
// Role definition
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Role {
    Admin,
    Operator,
    Viewer,
    ServiceAccount(String),  // For pod-to-pod
}

// Permission definition
#[derive(Clone, Debug)]
pub struct Permission {
    pub resource: String,    // "policy", "signal", "action"
    pub action: String,      // "read", "write", "delete", "admin"
    pub scope: String,       // "own", "team", "all"
}

// Role-permission mapping
pub struct RolePermissions {
    permissions: HashMap<Role, Vec<Permission>>,
}

impl RolePermissions {
    pub fn new() -> Self {
        let mut perms = HashMap::new();

        // Admin: all permissions
        perms.insert(Role::Admin, vec![
            Permission { resource: "policy".into(), action: "write".into(), scope: "all".into() },
            Permission { resource: "policy".into(), action: "delete".into(), scope: "all".into() },
            Permission { resource: "signal".into(), action: "write".into(), scope: "all".into() },
            Permission { resource: "action".into(), action: "write".into(), scope: "all".into() },
            Permission { resource: "*".into(), action: "admin".into(), scope: "all".into() },
        ]);

        // Operator: read/write except delete
        perms.insert(Role::Operator, vec![
            Permission { resource: "policy".into(), action: "read".into(), scope: "all".into() },
            Permission { resource: "policy".into(), action: "write".into(), scope: "all".into() },
            Permission { resource: "signal".into(), action: "read".into(), scope: "all".into() },
            Permission { resource: "signal".into(), action: "write".into(), scope: "team".into() },
            Permission { resource: "action".into(), action: "read".into(), scope: "all".into() },
        ]);

        // Viewer: read-only
        perms.insert(Role::Viewer, vec![
            Permission { resource: "policy".into(), action: "read".into(), scope: "all".into() },
            Permission { resource: "signal".into(), action: "read".into(), scope: "own".into() },
            Permission { resource: "action".into(), action: "read".into(), scope: "own".into() },
        ]);

        RolePermissions { permissions: perms }
    }

    pub fn can_perform(
        &self,
        role: &Role,
        resource: &str,
        action: &str,
        scope: &str,
    ) -> bool {
        if let Some(perms) = self.permissions.get(role) {
            perms.iter().any(|p| {
                (p.resource == "*" || p.resource == resource) &&
                p.action == action &&
                (p.scope == "all" || p.scope == scope)
            })
        } else {
            false
        }
    }
}

// Authorization middleware
pub async fn authorize_request(
    req: &Request,
    resource: &str,
    action: &str,
) -> Result<(), AuthorizationError> {
    let role = extract_role_from_cert(req)?;
    let permissions = RolePermissions::new();

    if !permissions.can_perform(&role, resource, action, "all") {
        return Err(AuthorizationError::PermissionDenied {
            role: format!("{:?}", role),
            resource: resource.to_string(),
            action: action.to_string(),
        });
    }

    Ok(())
}

// Extract role from mTLS certificate
fn extract_role_from_cert(req: &Request) -> Result<Role, AuthorizationError> {
    let cert = req.extensions()
        .get::<PeerCertificate>()
        .ok_or(AuthorizationError::NoCertificate)?;

    // Extract subject alternative name (SAN) which contains service account
    let san = cert.subject_alt_names()
        .and_then(|names| names.get(0))
        .ok_or(AuthorizationError::NoServiceIdentity)?;

    // Parse "cluster.local/ns/tai-system/sa/governor"
    match parse_service_account(san) {
        "governor" => Ok(Role::ServiceAccount("governor".into())),
        "coordinator" => Ok(Role::ServiceAccount("coordinator".into())),
        "scheduler" => Ok(Role::ServiceAccount("scheduler".into())),
        sa => Err(AuthorizationError::UnknownServiceAccount(sa.to_string())),
    }
}

// Audit logging
pub async fn audit_log(
    user: &str,
    role: &Role,
    resource: &str,
    action: &str,
    result: &str,
) {
    let log = AuditEvent {
        timestamp: SystemTime::now(),
        user: user.to_string(),
        role: format!("{:?}", role),
        resource: resource.to_string(),
        action: action.to_string(),
        result: result.to_string(),
    };

    // Store in audit log (Firestore or audit sink)
    store_audit_event(log).await.ok();
}
```

### gRPC Service Implementation with Authorization

```rust
use tonic::{Request, Response, Status};
use tai::governor_server::Governor;

pub struct GovernorService;

#[tonic::async_trait]
impl Governor for GovernorService {
    async fn propose_policy(
        &self,
        request: Request<Policy>,
    ) -> Result<Response<Receipt>, Status> {
        // Extract caller identity
        let cert = request.extensions()
            .get::<PeerCertificate>()
            .ok_or_else(|| Status::unauthenticated("No certificate"))?;

        let role = extract_role_from_cert_tonic(cert)?;

        // Authorize action
        authorize_request_tonic(&request, "policy", "write", &role)?;

        // Audit log
        audit_log_request(&request, "propose_policy", &role).await;

        // Process request
        let policy = request.into_inner();
        let receipt = self.process_policy(policy).await?;

        Ok(Response::new(receipt))
    }

    async fn enforce_policy(
        &self,
        request: Request<Policy>,
    ) -> Result<Response<Receipt>, Status> {
        let cert = request.extensions()
            .get::<PeerCertificate>()
            .ok_or_else(|| Status::unauthenticated("No certificate"))?;

        let role = extract_role_from_cert_tonic(cert)?;

        // Only admin can enforce
        if !matches!(role, Role::Admin) {
            return Err(Status::permission_denied(
                "Only admin can enforce policies"
            ));
        }

        audit_log_request(&request, "enforce_policy", &role).await;

        let policy = request.into_inner();
        let receipt = self.enforce_policy_impl(policy).await?;

        Ok(Response::new(receipt))
    }
}
```

## External Client Authentication

### OAuth2 / JWT for External Clients

```rust
// Validate JWT token from external client
pub async fn validate_token(token: &str) -> Result<Claims, TokenError> {
    let decoded = decode::<Claims>(
        token,
        &DecodingKey::from_secret(b"secret"),
        &Validation::default(),
    )?;

    // Check expiration
    if decoded.claims.exp < SystemTime::now()
        .duration_since(UNIX_EPOCH)?
        .as_secs()
    {
        return Err(TokenError::Expired);
    }

    Ok(decoded.claims)
}

#[derive(Serialize, Deserialize)]
pub struct Claims {
    pub sub: String,        // Subject (user ID)
    pub role: String,       // Role: admin, operator, viewer
    pub exp: u64,          // Expiration time
    pub iat: u64,          // Issued at
    pub scope: String,     // Resource scope
}
```

## Audit Trail

```yaml
# Audit logs stored in Firestore
/audit_logs
  {logId}
    - timestamp (timestamp)
    - user (string)
    - role (string)
    - resource (string)
    - action (string)
    - result (string): "success", "denied"
    - reason (string): denial reason
    - ip_address (string)
    - user_agent (string)
```

## Consequences

### Positive
- Zero-trust architecture
- Defense in depth (multiple authorization layers)
- Audit trail for compliance
- Least privilege enforcement
- Service identity automatic (no passwords)

### Negative
- Operational complexity
- Debugging authorization issues difficult
- Certificate lifecycle management
- Role hierarchy maintenance
- Token rotation for external clients

## Monitoring

- RBAC denials per service pair
- Token validation failures
- Audit log completeness
- Permission consistency checks

## References
- [Kubernetes RBAC](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)
- [Istio Authorization](https://istio.io/latest/docs/concepts/security/#authorization)
- [Tonic gRPC Framework](https://github.com/hyperium/tonic)
