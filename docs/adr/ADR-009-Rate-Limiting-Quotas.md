<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ADR-009: Rate Limiting and Quota Implementation](#adr-009-rate-limiting-and-quota-implementation)
  - [Problem Statement](#problem-statement)
  - [Decision](#decision)
  - [Rationale](#rationale)
    - [Token Bucket Algorithm](#token-bucket-algorithm)
    - [Redis Backing](#redis-backing)
  - [Implementation](#implementation)
    - [API Gateway (Envoy) Rate Limiting](#api-gateway-envoy-rate-limiting)
    - [Application-Level Rate Limiting (Rust)](#application-level-rate-limiting-rust)
    - [Quota Configuration](#quota-configuration)
    - [HTTP Headers for Rate Limiting](#http-headers-for-rate-limiting)
    - [Monitoring and Alerts](#monitoring-and-alerts)
    - [Alert Rules](#alert-rules)
  - [Multi-Tenant Quotas](#multi-tenant-quotas)
  - [Graceful Degradation](#graceful-degradation)
  - [Consequences](#consequences)
    - [Positive](#positive)
    - [Negative](#negative)
  - [Cost Impact](#cost-impact)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ADR-009: Rate Limiting and Quota Implementation

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Protecting services from overload and enforcing fair resource usage
**Deciders:** Platform Architecture Team

## Problem Statement

TAI services need protection from:
- Malicious or accidental request floods
- Unfair resource consumption
- Cascading failures from spikes
- Cost runaway

Need: Per-service, per-client, per-operation rate limits with graceful degradation.

## Decision

**Three-tier rate limiting:**
1. **API Gateway (Envoy):** Global rate limiting per IP/API key
2. **Service mesh (Istio):** Per-destination rate limiting
3. **Application:** Per-operation budget for fine-grained control

Quotas tracked in Redis, enforced by token bucket algorithm.

## Rationale

### Token Bucket Algorithm
- Allows bursts (token accumulation)
- Fair distribution over time
- Simple, efficient implementation
- Industry standard (used by AWS, Google)

### Redis Backing
- Distributed across services
- Atomic operations
- Fast queries (<1ms)
- Shared quota state

## Implementation

### API Gateway (Envoy) Rate Limiting

```yaml
# Envoy configuration for rate limiting
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: tai-api
spec:
  hosts:
  - "api.tai.example.com"
  http:
  - match:
    - uri:
        prefix: "/tai.Governor"
    rateLimit:
      actions:
      - remoteAddress: {}  # Limit by IP
      - headerMatch:
          headerName: "x-api-key"
    route:
    - destination:
        host: governor
        port:
          number: 50051

---
# Global rate limiting (EnvoyFilter)
apiVersion: networking.istio.io/v1alpha3
kind: EnvoyFilter
metadata:
  name: global-ratelimit
spec:
  workloadSelector:
    labels:
      app: ingressgateway
  configPatches:
  - applyTo: HTTP_FILTER
    match:
      context: SIDECAR_INBOUND
      listener:
        filterChain:
          filter:
            name: "envoy.filters.network.http_connection_manager"
            subFilter:
              name: "envoy.filters.http.router"
    patch:
      operation: INSERT_BEFORE
      value:
        name: envoy.filters.http.local_ratelimit
        typedConfig:
          "@type": type.googleapis.com/udpa.type.v1.TypedStruct
          type_url: type.googleapis.com/envoy.extensions.filters.http.local_ratelimit.v3.LocalRateLimit
          value:
            stat_prefix: http_local_rate_limiter
            token_bucket:
              max_tokens: 1000
              tokens_per_fill: 1000
              fill_interval: 1s
            filter_enabled:
              runtime_key: local_rate_limit_enabled
              default_value:
                numerator: 100
                denominator: HUNDRED
            filter_enforced:
              runtime_key: local_rate_limit_enforced
              default_value:
                numerator: 100
                denominator: HUNDRED
            response_headers_to_add:
            - append: false
              header:
                key: x-local-rate-limit
                value: "true"
```

### Application-Level Rate Limiting (Rust)

```rust
use redis::Commands;
use std::sync::Arc;

pub struct RateLimiter {
    redis: redis::Connection,
}

pub struct RateLimit {
    pub requests_per_second: u32,
    pub burst_size: u32,
}

impl RateLimiter {
    pub fn new(redis_url: &str) -> Result<Self> {
        let client = redis::Client::open(redis_url)?;
        let connection = client.get_connection()?;
        Ok(Self { redis: connection })
    }

    // Token bucket algorithm
    pub fn allow_request(
        &mut self,
        client_id: &str,
        operation: &str,
        limit: &RateLimit,
    ) -> Result<bool> {
        let key = format!("ratelimit:{}:{}", client_id, operation);
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)?
            .as_secs();

        // Get current bucket state
        let bucket: Option<(f64, u64)> = self.redis
            .get(&format!("{}:state", key))
            .unwrap_or((limit.burst_size as f64, now));

        let (mut tokens, last_refill) = bucket;
        let elapsed = now - last_refill;

        // Refill tokens
        tokens = (tokens
            + (elapsed as f64) * (limit.requests_per_second as f64))
            .min(limit.burst_size as f64);

        // Check if request allowed
        if tokens >= 1.0 {
            tokens -= 1.0;
            self.redis.set_ex(
                &format!("{}:state", key),
                format!("{},{}", tokens, now),
                3600,
            )?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // Get remaining quota
    pub fn get_quota(&mut self, client_id: &str, operation: &str) -> Result<RemainingQuota> {
        let key = format!("ratelimit:{}:{}", client_id, operation);
        let bucket: Option<String> = self.redis.get(&format!("{}:state", key))?;

        match bucket {
            Some(state) => {
                let parts: Vec<&str> = state.split(',').collect();
                Ok(RemainingQuota {
                    tokens_remaining: parts[0].parse()?,
                    reset_at: parts[1].parse::<u64>()?  + 3600,
                })
            }
            None => {
                Ok(RemainingQuota {
                    tokens_remaining: 1000.0,
                    reset_at: (SystemTime::now()
                        .duration_since(UNIX_EPOCH)?
                        .as_secs() + 3600),
                })
            }
        }
    }
}

pub struct RemainingQuota {
    pub tokens_remaining: f64,
    pub reset_at: u64,
}

// gRPC middleware with rate limiting
#[tonic::async_trait]
impl Governor for GovernorService {
    #[tracing::instrument(skip(self, request))]
    async fn propose_policy(
        &self,
        request: Request<Policy>,
    ) -> Result<Response<Receipt>, Status> {
        let client_id = extract_client_id(&request)?;

        // Check rate limit
        let mut limiter = self.rate_limiter.lock().await;
        let limit = RateLimit {
            requests_per_second: 100,
            burst_size: 200,
        };

        if !limiter.allow_request(&client_id, "propose_policy", &limit)? {
            // Add rate limit headers
            let mut response_headers = tonic::metadata::MetadataMap::new();
            response_headers.insert(
                "x-ratelimit-remaining",
                format!("{}", 0).parse().unwrap(),
            );
            response_headers.insert(
                "x-ratelimit-reset",
                format!("{}", limiter.get_quota(&client_id, "propose_policy")?.reset_at)
                    .parse()
                    .unwrap(),
            );

            return Err(Status::resource_exhausted(
                "Rate limit exceeded"
            ));
        }

        // Proceed with request
        let policy = request.into_inner();
        let receipt = self.process_policy(policy).await?;

        Ok(Response::new(receipt))
    }
}

// Extract client ID from request
fn extract_client_id(request: &Request<Policy>) -> Result<String, Status> {
    // Try API key first
    if let Some(api_key) = request
        .metadata()
        .get("x-api-key")
        .and_then(|v| v.to_str().ok())
    {
        return Ok(format!("apikey:{}", api_key));
    }

    // Fall back to mTLS certificate
    if let Some(cert) = request.extensions().get::<PeerCertificate>() {
        let san = cert.subject_alt_names()
            .and_then(|names| names.get(0))
            .ok_or_else(|| Status::unauthenticated("No certificate"))?;
        return Ok(format!("service:{}", san));
    }

    Err(Status::unauthenticated("No client identity"))
}
```

### Quota Configuration

```rust
pub fn get_rate_limits() -> HashMap<&'static str, RateLimit> {
    let mut limits = HashMap::new();

    // Public API limits (external clients)
    limits.insert("api_key:propose_policy", RateLimit {
        requests_per_second: 100,
        burst_size: 200,
    });

    limits.insert("api_key:get_policies", RateLimit {
        requests_per_second: 500,
        burst_size: 1000,
    });

    // Service-to-service limits (internal)
    limits.insert("service:coordinator:propose_policy", RateLimit {
        requests_per_second: 5000,
        burst_size: 10000,
    });

    limits.insert("service:scheduler:submit_task", RateLimit {
        requests_per_second: 1000,
        burst_size: 2000,
    });

    limits
}
```

### HTTP Headers for Rate Limiting

```
Response headers:
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 42
X-RateLimit-Reset: 1642000000

429 Too Many Requests:
Retry-After: 30
```

### Monitoring and Alerts

```prometheus
# Rate limit hits per operation
tai_ratelimit_exceeded_total{operation="propose_policy",client_type="api_key"} 45

# Current quota usage
tai_ratelimit_tokens_remaining{operation="propose_policy",client="api_key:abc123"} 42.5

# Quota refill rate
tai_ratelimit_tokens_per_second{operation="propose_policy"} 100
```

### Alert Rules

```yaml
- name: HighRateLimitHitRate
  expr: |
    rate(tai_ratelimit_exceeded_total[5m]) > 10
  for: 5m
  severity: warning
  annotations:
    summary: "High rate limit hit rate"

- name: ClientExhaustedQuota
  expr: |
    tai_ratelimit_tokens_remaining < 10
  for: 1m
  severity: info
  annotations:
    summary: "Client approaching quota limit"
```

## Multi-Tenant Quotas

```rust
pub struct TenantQuota {
    pub tenant_id: String,
    pub operations_per_minute: u32,
    pub storage_gb: u32,
    pub concurrent_connections: u32,
}

pub fn get_tenant_limits(tenant_id: &str) -> TenantQuota {
    match tenant_id {
        "tier:free" => TenantQuota {
            operations_per_minute: 600,
            storage_gb: 1,
            concurrent_connections: 10,
        },
        "tier:pro" => TenantQuota {
            operations_per_minute: 60000,
            storage_gb: 100,
            concurrent_connections: 1000,
        },
        "tier:enterprise" => TenantQuota {
            operations_per_minute: 1000000,
            storage_gb: 10000,
            concurrent_connections: 10000,
        },
        _ => TenantQuota {
            operations_per_minute: 0,
            storage_gb: 0,
            concurrent_connections: 0,
        },
    }
}
```

## Graceful Degradation

When limits approached:
1. Return 429 with Retry-After
2. Client backs off exponentially
3. Optional: queue requests (5 min timeout)
4. Shed load for non-critical operations
5. Prioritize health checks

## Consequences

### Positive
- Prevents resource exhaustion
- Fair usage across clients
- Cost control (predictable)
- Graceful degradation
- Protects against DDoS

### Negative
- Legitimate spikes get throttled
- Configuration tuning needed
- Client retry logic required
- Adds operational complexity
- Redis dependency

## Cost Impact

- Redis: ~$50/month (for quota state)
- Operational overhead: minimal (<1% CPU)

## References
- [Token Bucket Algorithm](https://en.wikipedia.org/wiki/Token_bucket)
- [Envoy Rate Limiting](https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/local_rate_limit_filter)
- [Redis Rate Limiting](https://redis.io/docs/manual/client-side-caching/)
