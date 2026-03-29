# Docker Database URL Resolution Pattern

## Runtime Evidence
- **File:** `docs/validation/evidence/docker-db-connection.log`
- **Test Case:** `DockerDBConnectionTest.testHostDockerInternalResolution`
- **Engine Version:** YAWL 6.0.0 GA
- **Docker:** Docker Desktop 4.x+

## The Problem

```yaml
# docker-compose.yml
services:
  yawl-engine:
    environment:
      - YAWL_DB_URL=jdbc:postgresql://localhost:5432/yawl
      # WRONG: localhost resolves to engine container, not host
```

**Error:** `Connection refused: localhost:5432` (engine container doesn't have PostgreSQL)

## The Fix

```yaml
# docker-compose.yml
services:
  yawl-engine:
    environment:
      - YAWL_DB_URL=jdbc:postgresql://host.docker.internal:5432/yawl
      # RIGHT: host.docker.internal resolves to host machine
```

## Why This Works

Docker containers run in an isolated network. `localhost` inside a container resolves to the container itself, not the host machine. Docker Desktop provides a special DNS name `host.docker.internal` that resolves to the host machine's internal IP.

On Linux, add `extra_hosts` to docker-compose.yml:
```yaml
services:
  yawl-engine:
    extra_hosts:
      - "host.docker.internal:host-gateway"
```

## Validation

Run: `make docker-up && docker logs yawl-engine 2>&1 | grep -i "connection established"`
Expected: Database connection succeeds, no "Connection refused" errors

## Files
- **Docker Compose:** `docker-compose.yml`
- **Engine Config:** `yawl-core/src/main/resources/hibernate.properties`
- **Test:** `test/org/yawlfoundation/yawl/validation/DockerDBConnectionTest.java`
