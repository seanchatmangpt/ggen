# Configuration Guide

Complete reference for TAI Erlang Autonomics configuration options.

## Overview

Configuration is managed through:
1. **Environment Variables**: Runtime overrides
2. **sys.config**: Application configuration
3. **vm.args**: Erlang VM arguments
4. **Docker**: Container environment
5. **Terraform**: GCP deployment variables

---

## Environment Variables

### HTTP Server

| Variable | Default | Description |
|----------|---------|-------------|
| `PORT` | `8080` | HTTP server listening port |
| `BIND_ADDRESS` | `0.0.0.0` | HTTP server bind address |
| `REQUEST_TIMEOUT_MS` | `30000` | HTTP request timeout (milliseconds) |
| `GRACEFUL_SHUTDOWN_MS` | `30000` | Graceful shutdown timeout |

### GCP Configuration

| Variable | Default | Required | Description |
|----------|---------|----------|-------------|
| `GCP_PROJECT_ID` | - | Yes (prod) | GCP project ID |
| `GCP_REGION` | `us-central1` | No | GCP region |
| `GOOGLE_APPLICATION_CREDENTIALS` | - | No (auth) | Path to service account JSON |

### Pub/Sub Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `PUBSUB_SUBSCRIPTION` | `erlang-autonomics-signals-sub` | Pub/Sub subscription name |
| `PUBSUB_MAX_MESSAGES` | `100` | Max messages to pull at once |
| `PUBSUB_DEADLETTER_TOPIC` | `erlang-autonomics-dlq` | Dead letter queue topic |

### Firestore Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `FIRESTORE_ENABLED` | `true` | Enable Firestore writes |
| `FIRESTORE_COLLECTION` | `receipts` | Firestore collection name |
| `FIRESTORE_BATCH_SIZE` | `100` | Batch write size |
| `FIRESTORE_BATCH_TIMEOUT_MS` | `5000` | Batch timeout |
| `FIRESTORE_TTL_DAYS` | `7` | Receipt retention (days) |

### Receipt Ledger

| Variable | Default | Description |
|----------|---------|-------------|
| `RECEIPT_LEDGER_BACKEND` | `ets` | Backend: ets, firestore, bigquery |
| `RECEIPT_BUFFER_SIZE` | `10000` | In-memory receipt buffer |
| `RECEIPT_FLUSH_INTERVAL_MS` | `1000` | Flush to storage interval |
| `RECEIPT_VERIFY_HASHES` | `true` | Verify hash chain on read |

### Observability

| Variable | Default | Description |
|----------|---------|-------------|
| `LOG_LEVEL` | `info` | Logging level: debug, info, warn, error |
| `JSON_LOGS` | `true` | Use structured JSON logging |
| `TRACING_ENABLED` | `true` | Enable OpenTelemetry tracing |
| `TRACING_SAMPLE_RATE` | `0.1` | Trace sample rate (0.0-1.0) |
| `METRICS_ENABLED` | `true` | Enable Prometheus metrics |
| `METRICS_COLLECTION_INTERVAL_MS` | `10000` | Metrics collection interval |

### Security

| Variable | Default | Description |
|----------|---------|-------------|
| `VERIFY_SIGNATURES` | `false` | Verify JWT signatures on marketplace |
| `JWT_SECRET_KEY` | - | JWT secret key (base64) |
| `HMAC_SECRET_KEY` | - | HMAC secret key (base64) |
| `ALLOWED_ORIGINS` | `*` | CORS allowed origins (comma-separated) |

### Advanced Options

| Variable | Default | Description |
|----------|---------|-------------|
| `MAX_CONCURRENT_REQUESTS` | `1000` | Max concurrent HTTP requests |
| `ACTION_POOL_SIZE` | `10` | Bounded executor pool size |
| `ACTION_QUEUE_SIZE` | `1000` | Action queue size |
| `ACTION_TIMEOUT_MS` | `60000` | Action execution timeout |
| `GOVERNOR_STATE_CHECK_INTERVAL_MS` | `1000` | Governor state check interval |
| `HEALTH_CHECK_INTERVAL_MS` | `5000` | Health check interval |

---

## Configuration Files

### sys.config

Application-level configuration for Erlang.

**Location**: `config/sys.config`

**Example**:

```erlang
[
  {tai_autonomics, [
    {port, 8080},
    {gcp_project_id, "my-project-id"},
    {pubsub_subscription, "erlang-autonomics-signals-sub"},
    {firestore_enabled, true},
    {receipt_ledger_backend, ets},
    {log_level, info},
    {json_logs, true}
  ]},
  {kernel, [
    {logger, [
      {handler, default, logger_std_h, #{
        config => #{
          type => {file, "/var/log/tai-autonomics.log"}
        },
        formatter => {logger_formatter, #{
          template => [timestamp, " [", level, "] ", msg, "\n"]
        }}
      }}
    ]}
  ]}
].
```

### vm.args

Erlang VM configuration.

**Location**: `config/vm.args`

**Example**:

```
-name tai_autonomics@127.0.0.1
-setcookie tai_autonomics_secret

-pa ./ebin
-pa ./deps/*/ebin

-kernel inet_dist_listen_min 9001
-kernel inet_dist_listen_max 9010

-env ERL_MAX_PORTS 65536
-env ERL_MAX_ETS_TABLES 100000

+S 8:8
+sbt db
+A 128

-kernel logger_level info

-heart
-env HEART_BEAT_TIMEOUT 30

-eval "application:ensure_all_started(tai_autonomics, permanent)"
```

### Environment-Specific Configuration

**Development** (`config/dev.config`):
```erlang
[
  {tai_autonomics, [
    {port, 8080},
    {receipt_ledger_backend, ets},
    {tracing_sample_rate, 1.0},
    {log_level, debug}
  ]}
].
```

**Production** (`config/prod.config`):
```erlang
[
  {tai_autonomics, [
    {port, 8080},
    {receipt_ledger_backend, firestore},
    {tracing_sample_rate, 0.1},
    {log_level, info},
    {verify_signatures, true}
  ]}
].
```

---

## Docker Environment

### Environment Variables in Container

```dockerfile
ENV PORT=8080
ENV GCP_PROJECT_ID=my-project
ENV RECEIPT_LEDGER_BACKEND=firestore
ENV TRACING_ENABLED=true
ENV LOG_LEVEL=info
```

### Example .env File

```bash
# Server
PORT=8080
BIND_ADDRESS=0.0.0.0

# GCP
GCP_PROJECT_ID=my-gcp-project
GCP_REGION=us-central1

# Pub/Sub
PUBSUB_SUBSCRIPTION=erlang-autonomics-signals-sub

# Firestore
FIRESTORE_ENABLED=true
FIRESTORE_COLLECTION=receipts

# Receipt Ledger
RECEIPT_LEDGER_BACKEND=firestore
RECEIPT_BUFFER_SIZE=10000

# Observability
LOG_LEVEL=info
JSON_LOGS=true
TRACING_ENABLED=true
TRACING_SAMPLE_RATE=0.1
METRICS_ENABLED=true

# Security
VERIFY_SIGNATURES=false
ALLOWED_ORIGINS=*

# Advanced
MAX_CONCURRENT_REQUESTS=1000
ACTION_POOL_SIZE=10
ACTION_TIMEOUT_MS=60000
```

---

## Terraform Variables

### terraform.tfvars

```hcl
# GCP Configuration
project_id = "my-gcp-project"
region     = "us-central1"

# Environment
environment = "prod"

# Container
image_tag = "latest"

# Cloud Run
min_instances = 1
max_instances = 10
cpu_limit     = "2"
memory_limit  = "2Gi"
timeout       = 900

# Custom Configuration
custom_env_vars = {
  LOG_LEVEL                       = "info"
  TRACING_SAMPLE_RATE             = "0.1"
  RECEIPT_LEDGER_BACKEND          = "firestore"
  FIRESTORE_BATCH_SIZE            = "100"
  ACTION_POOL_SIZE                = "10"
  GOVERNOR_STATE_CHECK_INTERVAL_MS = "1000"
}
```

---

## Advanced Configuration

### Governor Configuration

```erlang
[
  {tai_autonomics, [
    {governors, [
      {entitlement_governor, #{
        initial_state => unentitled,
        state_check_interval_ms => 1000,
        max_concurrent_actions => 10
      }},
      {subscription_governor, #{
        initial_state => inactive,
        state_check_interval_ms => 1000
      }},
      {quota_sla_governor, #{
        initial_state => unentitled,
        check_interval_ms => 1000
      }}
    ]}
  ]}
].
```

### Action Executor Configuration

```erlang
[
  {tai_autonomics, [
    {action_executor, #{
      pool_size => 10,
      queue_size => 1000,
      worker_timeout_ms => 60000,
      max_retries => 3,
      retry_backoff_ms => 1000
    }}
  ]}
].
```

### Receipt Ledger Configuration

```erlang
[
  {tai_autonomics, [
    {receipt_ledger, #{
      backend => firestore,
      collection => "receipts",
      batch_size => 100,
      batch_timeout_ms => 5000,
      buffer_size => 10000,
      flush_interval_ms => 1000,
      ttl_days => 7,
      verify_hashes => true
    }}
  ]}
].
```

### Observability Configuration

```erlang
[
  {tai_autonomics, [
    {tracing, #{
      enabled => true,
      sample_rate => 0.1,
      exporter => otel_exporter_trace_otlp,
      resource_attributes => #{
        <<"service.name">> => <<"tai-autonomics">>,
        <<"service.version">> => <<"1.0.0">>,
        <<"environment">> => <<"production">>
      }
    }},
    {metrics, #{
      enabled => true,
      collection_interval_ms => 10000,
      export_interval_ms => 60000,
      exporter => otel_exporter_metrics_otlp
    }}
  ]}
].
```

---

## Configuration by Environment

### Local Development

```bash
export PORT=8080
export GCP_PROJECT_ID=local-dev
export RECEIPT_LEDGER_BACKEND=ets
export LOG_LEVEL=debug
export TRACING_SAMPLE_RATE=1.0
export VERIFY_SIGNATURES=false

rebar3 release
_build/default/rel/tai_autonomics/bin/tai_autonomics start
```

### Docker Compose

```bash
docker-compose --env-file .env up
```

**Environment**:
- Pub/Sub Emulator: localhost:8085
- Firestore Emulator: localhost:8081

### Staging (GCP)

```bash
export GCP_PROJECT_ID=my-project-staging
export GCP_REGION=us-central1
export ENVIRONMENT=staging
export TRACING_SAMPLE_RATE=0.5
export LOG_LEVEL=info
export VERIFY_SIGNATURES=true

make terraform-plan ENVIRONMENT=staging
make terraform-apply ENVIRONMENT=staging
```

### Production (GCP)

```bash
export GCP_PROJECT_ID=my-project-prod
export GCP_REGION=us-central1
export ENVIRONMENT=prod
export TRACING_SAMPLE_RATE=0.1
export LOG_LEVEL=info
export VERIFY_SIGNATURES=true
export MIN_INSTANCES=2
export MAX_INSTANCES=100
export CPU_LIMIT="4"
export MEMORY_LIMIT="4Gi"

make terraform-plan ENVIRONMENT=prod
make terraform-apply ENVIRONMENT=prod
```

---

## Performance Tuning

### High Throughput

For handling 1000+ requests/second:

```erlang
[
  {tai_autonomics, [
    {port, 8080},
    {max_concurrent_requests, 5000},
    {action_pool_size, 50},
    {action_queue_size, 10000},
    {receipt_buffer_size, 100000},
    {receipt_flush_interval_ms, 100}
  ]},
  {kernel, [
    {inet, [
      {inet_default_connect_options, [
        {nodelay, true},
        {recbuf, 1048576},
        {sndbuf, 1048576}
      ]}
    ]}
  ]}
].
```

### Low Latency

For minimizing request latency:

```erlang
[
  {tai_autonomics, [
    {action_pool_size, 20},
    {action_timeout_ms, 5000},
    {governor_state_check_interval_ms, 100},
    {receipt_flush_interval_ms, 10},
    {tracing_sample_rate, 0.01}
  ]}
].
```

### Memory Constrained

For minimal memory footprint:

```erlang
[
  {tai_autonomics, [
    {max_concurrent_requests, 100},
    {action_pool_size, 5},
    {action_queue_size, 100},
    {receipt_buffer_size, 1000},
    {tracing_sample_rate, 0.01},
    {receipt_ledger_backend, firestore}
  ]}
].
```

---

## Configuration Validation

### Syntax Check

```bash
# Check sys.config syntax
erl -config config/sys.config -noshell -eval 'halt(0).'

# Or via rebar3
rebar3 check
```

### Runtime Validation

```erlang
% Verify configuration at startup
validate_config() ->
  case application:get_env(tai_autonomics, port) of
    {ok, Port} when is_integer(Port), Port > 0 ->
      ok;
    _ ->
      {error, invalid_port}
  end.
```

---

## Secrets Management

### Local Development

Use `.env` file (gitignored):

```bash
# .env
JWT_SECRET_KEY=$(echo -n "secret" | base64)
HMAC_SECRET_KEY=$(echo -n "secret" | base64)
```

### GCP Secret Manager

```bash
# Create secrets
gcloud secrets create jwt-secret --data-file=jwt-secret.txt
gcloud secrets create hmac-secret --data-file=hmac-secret.txt

# Grant access to service account
gcloud secrets add-iam-policy-binding jwt-secret \
  --member=serviceAccount:tai-autonomics-sa@PROJECT_ID.iam.gserviceaccount.com \
  --role=roles/secretmanager.secretAccessor

# Reference in Cloud Run
gcloud run deploy tai-autonomics \
  --set-env-vars=JWT_SECRET_KEY_REF=jwt-secret,HMAC_SECRET_KEY_REF=hmac-secret
```

### Loading Secrets at Runtime

```erlang
load_secrets() ->
  JwtSecret = os:getenv("JWT_SECRET_KEY", ""),
  HmacSecret = os:getenv("HMAC_SECRET_KEY", ""),

  case {JwtSecret, HmacSecret} of
    {"", _} -> {error, missing_jwt_secret};
    {_, ""} -> {error, missing_hmac_secret};
    {J, H} -> {ok, #{jwt => J, hmac => H}}
  end.
```

---

## Configuration Reference

### Complete sys.config

```erlang
[
  {tai_autonomics, [
    % HTTP Server
    {port, 8080},
    {bind_address, "0.0.0.0"},
    {request_timeout_ms, 30000},
    {graceful_shutdown_ms, 30000},

    % GCP Configuration
    {gcp_project_id, "my-project"},
    {gcp_region, "us-central1"},

    % Pub/Sub
    {pubsub_subscription, "erlang-autonomics-signals-sub"},
    {pubsub_max_messages, 100},
    {pubsub_deadletter_topic, "erlang-autonomics-dlq"},

    % Firestore
    {firestore_enabled, true},
    {firestore_collection, "receipts"},
    {firestore_batch_size, 100},
    {firestore_batch_timeout_ms, 5000},
    {firestore_ttl_days, 7},

    % Receipt Ledger
    {receipt_ledger_backend, ets},
    {receipt_buffer_size, 10000},
    {receipt_flush_interval_ms, 1000},
    {receipt_verify_hashes, true},

    % Observability
    {log_level, info},
    {json_logs, true},
    {tracing_enabled, true},
    {tracing_sample_rate, 0.1},
    {metrics_enabled, true},
    {metrics_collection_interval_ms, 10000},

    % Security
    {verify_signatures, false},
    {allowed_origins, "*"},

    % Advanced
    {max_concurrent_requests, 1000},
    {action_pool_size, 10},
    {action_queue_size, 1000},
    {action_timeout_ms, 60000},
    {governor_state_check_interval_ms, 1000},
    {health_check_interval_ms, 5000}
  ]},
  {kernel, [
    {logger_level, info}
  ]}
].
```

---

## Migration Guide

### Upgrading Configuration Format

From v0.x to v1.0:

```erlang
% Old (v0.x)
{tai_autonomics, [
  {server_port, 8080}
]}

% New (v1.0)
{tai_autonomics, [
  {port, 8080}
]}
```

---

## Troubleshooting

### Configuration Not Applied

```bash
# Check which config file is loaded
erl -config config/sys.config -noshell \
  -eval "application:get_all_env(tai_autonomics)" \
  -eval "halt(0)."
```

### Environment Variable Precedence

1. Environment variable (highest)
2. sys.config setting
3. Application default
4. Code default (lowest)

### Getting Current Configuration

```erlang
% Get all config
application:get_all_env(tai_autonomics).

% Get specific setting
application:get_env(tai_autonomics, port).

% Get with default
application:get_env(tai_autonomics, port, 8080).
```

---

## References

- Configuration Files: `config/sys.config`, `config/vm.args`
- Terraform Variables: `terraform/terraform.tfvars`
- Environment Template: `env.example`
- Erlang Logger: https://erlang.org/doc/apps/kernel/logger.html
