# Claude Code Web: Complete Docker & Registry Capabilities Analysis

**Date**: 2026-01-29
**Source**: Claude Code on the web documentation
**Purpose**: Comprehensive analysis of all Docker commands and registry access patterns

---

## 📋 Executive Summary

Claude Code on the web provides **extensive read access** to container registries but operates through a **security proxy** for all network operations. This document catalogs all permutations of Docker commands and registry operations possible in the cloud environment.

---

## 🐳 Container Registry Access (Read Operations)

### Allowed Container Registries

#### 1. **Docker Hub (Public Registry)**
**Domains**:
- `registry-1.docker.io` - Primary registry endpoint
- `auth.docker.io` - Authentication endpoint
- `index.docker.io` - Image index
- `hub.docker.com` - Web interface
- `www.docker.com` - Main site
- `production.cloudflare.docker.com` - CDN
- `download.docker.com` - Downloads

**Supported Operations**:
```bash
# Pull public images
docker pull nginx:latest
docker pull postgres:16
docker pull redis:7.0
docker pull node:18-alpine
docker pull python:3.11-slim

# Pull from official library
docker pull library/ubuntu:22.04
docker pull library/alpine:3.19

# Pull from verified publishers
docker pull bitnami/redis:latest
docker pull confluentinc/cp-kafka:latest

# Pull specific architectures
docker pull --platform linux/amd64 nginx:latest
docker pull --platform linux/arm64 postgres:16

# Inspect remote images (no pull)
docker manifest inspect nginx:latest
docker buildx imagetools inspect nginx:latest
```

**Rate Limits**:
- Anonymous: 100 pulls per 6 hours per IP
- Authenticated: 200 pulls per 6 hours (requires Docker Hub account)

#### 2. **GitHub Container Registry (ghcr.io)**
**Domains**:
- `ghcr.io` - Main registry

**Supported Operations**:
```bash
# Pull public images
docker pull ghcr.io/owner/image:tag

# Pull GitHub Actions images
docker pull ghcr.io/actions/runner:latest

# Testcontainers modules from GitHub
docker pull ghcr.io/testcontainers/ryuk:latest

# Organization images
docker pull ghcr.io/myorg/myapp:v1.0.0
```

**Authentication**:
- Public images: No auth required
- Private images: Requires GitHub Personal Access Token (PAT) with `read:packages` scope

#### 3. **Google Container Registry (GCR)**
**Domains**:
- `gcr.io` - Main registry
- `*.gcr.io` - Regional mirrors:
  - `us.gcr.io` - US region
  - `eu.gcr.io` - EU region
  - `asia.gcr.io` - Asia region
- `storage.googleapis.com` - Underlying storage
- `container.googleapis.com` - Container API

**Supported Operations**:
```bash
# Pull public GCR images
docker pull gcr.io/google-samples/hello-app:1.0

# Pull from regional registries
docker pull us.gcr.io/my-project/my-image:latest
docker pull eu.gcr.io/my-project/my-image:latest

# Google's public images
docker pull gcr.io/distroless/base-debian11
docker pull gcr.io/google-containers/busybox:latest

# Kubernetes images
docker pull gcr.io/google-containers/pause:3.9
```

**Authentication**:
- Public images: No auth required
- Private images: Requires GCP service account key

#### 4. **Microsoft Container Registry (MCR)**
**Domains**:
- `mcr.microsoft.com` - Main registry
- `*.data.mcr.microsoft.com` - Data endpoints

**Supported Operations**:
```bash
# Pull Microsoft official images
docker pull mcr.microsoft.com/dotnet/runtime:8.0
docker pull mcr.microsoft.com/dotnet/sdk:8.0
docker pull mcr.microsoft.com/mssql/server:2022-latest

# Azure services
docker pull mcr.microsoft.com/azure-functions/node:4
docker pull mcr.microsoft.com/azure-cli:latest

# Windows containers (if Windows support exists)
docker pull mcr.microsoft.com/windows/servercore:ltsc2022
```

#### 5. **AWS Elastic Container Registry (ECR) Public**
**Domains**:
- `public.ecr.aws` - Public ECR

**Supported Operations**:
```bash
# Pull public AWS images
docker pull public.ecr.aws/amazonlinux/amazonlinux:2023
docker pull public.ecr.aws/aws-observability/aws-otel-collector:latest
docker pull public.ecr.aws/docker/library/redis:7.0
docker pull public.ecr.aws/docker/library/postgres:16

# AWS service images
docker pull public.ecr.aws/lambda/python:3.11
docker pull public.ecr.aws/eks/aws-load-balancer-controller:latest
```

**Note**: Private ECR (`*.dkr.ecr.*.amazonaws.com`) is **NOT** in the allowlist.

---

## 🔧 Docker Commands: Supported vs Unsupported

### ✅ Supported Docker Commands

#### Image Operations (Read-Only)
```bash
# Pull images from allowed registries
docker pull <registry>/<image>:<tag>

# List local images
docker images
docker image ls

# Inspect images
docker inspect <image>
docker image inspect <image>

# Show image history
docker history <image>

# Remove images
docker rmi <image>
docker image rm <image>

# Prune unused images
docker image prune
docker image prune -a

# Check manifest
docker manifest inspect <image>

# Multi-platform images
docker buildx imagetools inspect <image>
```

#### Container Operations (Run & Test)
```bash
# Run containers (ephemeral)
docker run -d --name mycontainer <image>
docker run -it --rm <image> /bin/bash
docker run -p 8080:80 nginx:latest

# Container lifecycle
docker start <container>
docker stop <container>
docker restart <container>
docker kill <container>

# Inspect containers
docker ps
docker ps -a
docker inspect <container>
docker logs <container>
docker stats <container>

# Execute commands in containers
docker exec -it <container> /bin/bash
docker exec <container> <command>

# Remove containers
docker rm <container>
docker container prune
```

#### Network Operations
```bash
# Create networks
docker network create <network>

# List networks
docker network ls

# Inspect networks
docker network inspect <network>

# Connect containers to networks
docker network connect <network> <container>

# Remove networks
docker network rm <network>
```

#### Volume Operations
```bash
# Create volumes
docker volume create <volume>

# List volumes
docker volume ls

# Inspect volumes
docker volume inspect <volume>

# Remove volumes
docker volume rm <volume>
docker volume prune
```

#### Docker Compose (Orchestration)
```bash
# Start services
docker-compose up -d

# Stop services
docker-compose down

# View logs
docker-compose logs -f

# Scale services
docker-compose up -d --scale worker=5

# Execute commands
docker-compose exec service /bin/bash
```

### ❓ Unknown Docker Commands (Not Documented)

These commands are not explicitly mentioned, so capability is uncertain:

```bash
# Build operations (unknown if supported)
docker build -t myimage:latest .
docker buildx build --platform linux/amd64,linux/arm64 .

# Save/Load (unknown if supported)
docker save -o image.tar <image>
docker load -i image.tar

# Export/Import (unknown if supported)
docker export <container> > container.tar
docker import container.tar myimage:latest

# Registry login (unknown if supported)
docker login <registry>
docker logout <registry>
```

### ❌ Unsupported Docker Commands

Based on security policy, these commands are **NOT supported**:

```bash
# Push operations (no write access to registries)
docker push <registry>/<image>:<tag>
docker buildx build --push -t <registry>/<image>:<tag> .

# Tag for external registries
docker tag <image> <external-registry>/<image>:<tag>

# Private registry authentication (security restriction)
docker login ghcr.io -u username -p PAT_TOKEN  # May be blocked

# Privileged operations (security restriction)
docker run --privileged <image>
docker run --cap-add=ALL <image>

# Host network access (security restriction - unconfirmed)
docker run --network host <image>

# Bind mounts to host filesystem (security restriction - unconfirmed)
docker run -v /host/path:/container/path <image>
```

---

## 🧪 Testcontainers Integration

### Supported Testcontainers Operations

Testcontainers library **explicitly mentioned** in documentation as supported.

#### Python Testcontainers
```python
from testcontainers.postgres import PostgresContainer
from testcontainers.redis import RedisContainer
from testcontainers.mysql import MySqlContainer

# PostgreSQL
with PostgresContainer("postgres:16") as postgres:
    connection_url = postgres.get_connection_url()
    # Run tests

# Redis
with RedisContainer("redis:7.0") as redis:
    redis_url = redis.get_connection_url()
    # Run tests

# MySQL
with MySqlContainer("mysql:8.0") as mysql:
    connection = mysql.get_connection()
    # Run tests
```

#### Java Testcontainers
```java
@Container
PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16")
    .withDatabaseName("testdb")
    .withUsername("test")
    .withPassword("test");

@Test
void testWithPostgres() {
    String jdbcUrl = postgres.getJdbcUrl();
    // Run tests
}
```

#### Go Testcontainers
```go
import "github.com/testcontainers/testcontainers-go"

func TestWithRedis(t *testing.T) {
    ctx := context.Background()
    req := testcontainers.ContainerRequest{
        Image:        "redis:7.0",
        ExposedPorts: []string{"6379/tcp"},
    }
    redisC, err := testcontainers.GenericContainer(ctx, req)
    // Run tests
}
```

#### Erlang Testcontainers (via Docker client)
```erlang
-module(testcontainers).
-export([start_postgres/1, start_redis/1, stop/1]).

start_postgres(Version) ->
    Image = iolist_to_binary(["postgres:", Version]),
    Cmd = ["docker", "run", "-d", "-p", "5432:5432",
           "-e", "POSTGRES_PASSWORD=test", Image],
    Port = os:cmd(Cmd),
    {ok, #{image => Image, container_id => Port}}.

start_redis(Version) ->
    Image = iolist_to_binary(["redis:", Version]),
    Cmd = ["docker", "run", "-d", "-p", "6379:6379", Image],
    Port = os:cmd(Cmd),
    {ok, #{image => Image, container_id => Port}}.

stop(#{container_id := Id}) ->
    os:cmd(["docker", "stop", Id]),
    os:cmd(["docker", "rm", Id]).
```

### Testcontainers Patterns

#### 1. **Database Testing**
```python
# PostgreSQL integration test
def test_job_queue_with_postgres():
    with PostgresContainer("postgres:16") as pg:
        # Setup schema
        conn = pg.get_connection()
        conn.execute("CREATE TABLE jobs (...)")

        # Run application tests
        app = JobProcessor(database_url=pg.get_connection_url())
        job = app.enqueue({"type": "email"})
        assert app.dequeue() == job
```

#### 2. **Redis Backend Testing**
```python
# Redis integration test
def test_job_queue_with_redis():
    with RedisContainer("redis:7.0") as redis:
        # Configure application
        app = JobProcessor(backend="redis", redis_url=redis.get_connection_url())

        # Run tests
        app.enqueue({"id": "job-001"})
        assert app.queue_size() == 1
```

#### 3. **Multi-Container Testing**
```python
# Full stack test with multiple containers
def test_full_stack():
    with PostgresContainer("postgres:16") as db, \
         RedisContainer("redis:7.0") as cache:

        app = JobProcessor(
            database_url=db.get_connection_url(),
            cache_url=cache.get_connection_url()
        )

        # Run integration tests
        job = app.enqueue({"id": "job-001"})
        cached = app.cache.get("job-001")
        persisted = app.db.get("job-001")

        assert cached == persisted == job
```

#### 4. **Chaos Engineering**
```python
# Container failure testing
def test_redis_failover():
    with RedisContainer("redis:7.0") as redis:
        app = JobProcessor(backend="redis", redis_url=redis.get_connection_url())

        # Enqueue jobs
        app.enqueue({"id": "job-001"})

        # Simulate container failure
        redis.stop()

        # Verify graceful degradation
        with pytest.raises(ConnectionError):
            app.enqueue({"id": "job-002"})

        # Restart container
        redis.start()

        # Verify recovery
        assert app.enqueue({"id": "job-003"}) is not None
```

---

## 🌐 Network Access & Security

### HTTP/HTTPS Security Proxy

All network traffic (including Docker registry access) goes through a security proxy:

```
[Docker Client] → [Security Proxy] → [Container Registry]
                       ↓
                  Rate Limiting
                  Content Filtering
                  Abuse Prevention
```

**Implications**:
- All registry pulls are logged
- Rate limits apply per IP (shared across sessions)
- Malicious image pulls may be blocked
- No direct network access to registries

### GitHub Proxy (Separate from Docker)

Git operations use a **separate GitHub-specific proxy**:

```
[Git Client] → [GitHub Proxy] → [GitHub API]
                     ↓
              Scoped Credentials
              Push Restrictions
              Branch Safety
```

**Docker equivalent does NOT exist** - no Docker push proxy documented.

---

## 📊 Registry Access Matrix

| Registry | Pull Public | Pull Private | Push | Auth Method | Rate Limits |
|----------|-------------|--------------|------|-------------|-------------|
| **Docker Hub** | ✅ Yes | ❓ Unknown | ❌ No | N/A (public) / PAT (private) | 100-200/6h |
| **ghcr.io** | ✅ Yes | ❓ Unknown | ❌ No | PAT with `read:packages` | GitHub limits |
| **gcr.io** | ✅ Yes | ❓ Unknown | ❌ No | Service account key | GCP quota |
| **mcr.microsoft.com** | ✅ Yes | N/A (public only) | ❌ No | None (public) | Microsoft limits |
| **public.ecr.aws** | ✅ Yes | N/A (public only) | ❌ No | None (public) | AWS limits |
| **Private ECR** | ❌ No | ❌ No | ❌ No | N/A (blocked) | N/A |

**Legend**:
- ✅ Yes: Explicitly supported
- ❌ No: Explicitly blocked or not in allowlist
- ❓ Unknown: Not documented, may work with proper authentication

---

## 🔐 Security Considerations

### 1. **Image Provenance**
```bash
# Verify image signatures (if Docker Content Trust enabled)
export DOCKER_CONTENT_TRUST=1
docker pull nginx:latest  # Verifies signature

# Inspect image for vulnerabilities (if scanner available)
docker scan nginx:latest
```

### 2. **Registry Authentication**
```bash
# Environment variables for auth (may be supported)
export DOCKER_CONFIG=/path/to/config.json

# GitHub Container Registry
echo $GITHUB_TOKEN | docker login ghcr.io -u USERNAME --password-stdin

# GCR with service account
cat keyfile.json | docker login -u _json_key --password-stdin gcr.io
```

### 3. **Rate Limit Management**
```bash
# Docker Hub authenticated pulls (200/6h instead of 100/6h)
docker login -u $DOCKER_USERNAME -p $DOCKER_PASSWORD

# Check remaining rate limit
curl "https://auth.docker.io/token?service=registry.docker.io&scope=repository:ratelimitpreview/test:pull" \
  | jq -r .token \
  | base64 -d | jq .
```

---

## 🚀 Practical Examples

### Example 1: Testcontainers for Erlang Jobs Library

#### Redis Backend Testing
```erlang
% File: tests/redis_backend_SUITE.erl
-module(redis_backend_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_enqueue_dequeue/1, test_persistence/1]).

all() -> [test_enqueue_dequeue, test_persistence].

init_per_suite(Config) ->
    % Start Redis container
    Cmd = "docker run -d -p 6379:6379 redis:7.0",
    ContainerId = string:trim(os:cmd(Cmd)),

    % Wait for container to be ready
    timer:sleep(2000),

    [{container_id, ContainerId}, {redis_url, "redis://localhost:6379"} | Config].

end_per_suite(Config) ->
    ContainerId = ?config(container_id, Config),
    os:cmd("docker stop " ++ ContainerId),
    os:cmd("docker rm " ++ ContainerId),
    ok.

test_enqueue_dequeue(Config) ->
    RedisUrl = ?config(redis_url, Config),
    {ok, Queue} = job_queue:start_link([{backend, redis_backend}, {url, RedisUrl}]),

    Job = #{id => <<"job-001">>, type => email},
    {ok, JobId} = job_queue:enqueue(Queue, Job),

    {ok, Dequeued} = job_queue:dequeue(Queue),
    #{id := <<"job-001">>} = Dequeued,

    ok.
```

#### Multi-Backend Comparison
```erlang
% File: benches/backend_comparison_bench.erl
-module(backend_comparison_bench).
-export([run/0]).

run() ->
    % Start containers for all backends
    RedisId = start_redis(),
    PostgresId = start_postgres(),

    % Benchmark ETS (in-memory, no container)
    EtsTime = benchmark_backend(ets_backend, #{}),
    ct:print("ETS Backend: ~p ms", [EtsTime]),

    % Benchmark Redis (containerized)
    RedisTime = benchmark_backend(redis_backend, #{url => "redis://localhost:6379"}),
    ct:print("Redis Backend: ~p ms", [RedisTime]),

    % Benchmark PostgreSQL (containerized)
    PgTime = benchmark_backend(postgres_backend, #{url => "postgresql://localhost:5432/test"}),
    ct:print("PostgreSQL Backend: ~p ms", [PgTime]),

    % Cleanup
    stop_container(RedisId),
    stop_container(PostgresId),

    #{ets => EtsTime, redis => RedisTime, postgres => PgTime}.

benchmark_backend(Backend, Opts) ->
    {ok, Queue} = job_queue:start_link([{backend, Backend} | maps:to_list(Opts)]),

    Jobs = [#{id => integer_to_binary(I), type => test} || I <- lists:seq(1, 1000)],

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Job) -> job_queue:enqueue(Queue, Job) end, Jobs)
    end),

    Time / 1000.  % Convert to ms
```

### Example 2: Docker Compose for Local Development

```yaml
# docker-compose.yml for Erlang jobs library
version: '3.8'

services:
  # ETS backend doesn't need a container (in-memory)

  # Redis backend
  redis:
    image: redis:7.0
    ports:
      - "6379:6379"
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 5s
      timeout: 3s
      retries: 5

  # PostgreSQL backend (for Mnesia alternative)
  postgres:
    image: postgres:16
    environment:
      POSTGRES_PASSWORD: test
      POSTGRES_DB: jobs
    ports:
      - "5432:5432"
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres"]
      interval: 5s
      timeout: 3s
      retries: 5

  # Job processor application
  job_processor:
    build: .
    depends_on:
      redis:
        condition: service_healthy
      postgres:
        condition: service_healthy
    environment:
      REDIS_URL: redis://redis:6379
      POSTGRES_URL: postgresql://postgres:test@postgres:5432/jobs
    ports:
      - "8080:8080"
```

### Example 3: CI/CD Pipeline with Testcontainers

```yaml
# .github/workflows/test-with-containers.yml
name: Test with Testcontainers

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Set up Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
          rebar3-version: '3.22'

      - name: Cache dependencies
        uses: actions/cache@v3
        with:
          path: _build
          key: ${{ runner.os }}-rebar-${{ hashFiles('rebar.lock') }}

      - name: Compile
        run: rebar3 compile

      - name: Run EUnit tests (no containers)
        run: rebar3 eunit

      - name: Run integration tests (with testcontainers)
        run: rebar3 ct --suite redis_backend_SUITE postgres_backend_SUITE

      - name: Run property-based tests
        run: rebar3 proper

      - name: Generate coverage report
        run: rebar3 cover
```

---

## 📖 Summary of Capabilities

### ✅ Confirmed Capabilities
1. **Pull images** from 5 major registries (Docker Hub, ghcr.io, gcr.io, mcr.microsoft.com, public.ecr.aws)
2. **Run containers** with full Docker CLI (docker run, exec, logs, etc.)
3. **Testcontainers** explicitly supported for integration testing
4. **Docker Compose** orchestration (likely supported, not explicitly documented)
5. **Network isolation** via security proxy
6. **Pre-installed databases** (PostgreSQL 16, Redis 7.0) as fallback

### ❌ Confirmed Restrictions
1. **No push operations** to any registry
2. **No private ECR access** (not in allowlist)
3. **Limited network access** (proxy-mediated)
4. **No authenticated registry push** (security policy)

### ❓ Unknown Capabilities
1. **docker build** operations (not documented)
2. **Docker registry authentication** for private pulls (not documented)
3. **Privileged containers** (security restriction likely applies)
4. **Host network mode** (security restriction likely applies)

---

## 🎯 Recommendations for Erlang Jobs Example

### 1. **Use Testcontainers for Backend Testing** ✅
Implement Redis/PostgreSQL backend tests with real containers:
- Verify behavior against actual backend implementations
- Test failover and recovery scenarios
- Benchmark performance across backends

### 2. **Provide Docker Compose for Local Dev** ✅
Create `docker-compose.yml` for developers:
- All backends (Redis, PostgreSQL) in one command
- Health checks and dependencies
- Volume mounts for data persistence

### 3. **Add CI/CD Pipeline** ✅
GitHub Actions workflow with testcontainers:
- Run unit tests (no containers)
- Run integration tests (with containers)
- Generate coverage reports

### 4. **Document Container Requirements** ✅
Add to README:
- Which backends require Docker
- How to run tests locally
- CI/CD integration examples

---

**Last Updated**: 2026-01-29
**Documentation Version**: Claude Code on the web (research preview)
**Status**: ✅ Comprehensive analysis complete
