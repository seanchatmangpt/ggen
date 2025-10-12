# Backend Architecture

Cleanroom provides multiple execution backends that implement identical semantics for hermetic testing.

## Backend Selection

Cleanroom automatically selects the best available backend:

1. **Podman** (preferred) - Daemonless, rootless containers
2. **Docker** (isolated) - Industry standard containerization
3. **Local** (fallback) - Direct execution with best-effort determinism

## Auto Backend

The `AutoBackend` automatically detects and selects the optimal backend:

```rust
use cleanroom::backend::AutoBackend;

let backend = AutoBackend::detect()?;
let result = backend.run_cmd(["echo", "hello"])?;
```

Detection logic prioritizes:
- Podman for rootless execution
- Docker for maximum isolation
- Local for development environments

## Docker Backend

Provides containerized execution with security constraints:

```rust
use cleanroom::backend::DockerBackend;

let backend = DockerBackend::new("rust:1-slim");
backend.ensure_image()?; // Pull if not present
let result = backend.run_cmd(["cargo", "test"])?;
```

### Security Features

- Non-root execution (`--user=1000:1000`)
- Capability dropping (`--cap-drop=ALL`)
- Read-only root filesystem (`--read-only`)
- Tmpfs workdir (`--tmpfs=/workdir`)
- Network isolation (`--network=none`)
- Resource limits (CPU, memory, PIDs)

### Image Management

```rust
impl DockerBackend {
    pub fn ensure_image(&self) -> Result<()> // Pull if missing
    pub fn is_available(&self) -> bool        // Check Docker daemon
    pub fn docker_bin(mut self, path: String) -> Self // Custom Docker binary
}
```

## Podman Backend

Rootless container execution mirroring Docker behavior:

```rust
use cleanroom::backend::PodmanBackend;

let backend = PodmanBackend::new("rust:1-slim");
let result = backend.run_cmd(["./my_binary"])?;
```

Podman provides identical semantics to Docker but runs daemonless and rootless by default.

## Local Backend

Direct execution with best-effort determinism enforcement:

```rust
use cleanroom::backend::LocalBackend;

let backend = LocalBackend::new();
let result = backend.run_cmd(["./target/debug/mycli", "test"])?;
```

### Limitations

The local backend cannot enforce:
- Filesystem isolation (read-only rootfs)
- Network isolation (iptables rules)
- Process isolation (UID/GID mapping)

However, it still provides:
- Environment variable control
- Working directory isolation
- Timeout enforcement
- Determinism logging

## Backend Comparison

| Feature | Docker | Podman | Local |
|---------|--------|--------|-------|
| Rootless | ❌ | ✅ | ✅ |
| Daemon | Required | Optional | N/A |
| Security | Maximum | High | Limited |
| Performance | High | High | Highest |
| Setup | Complex | Simple | None |

## Custom Backends

Implement the `Backend` trait for custom execution environments:

```rust
use cleanroom::backend::{Backend, Cmd, RunResult};
use cleanroom::error::{Result, BackendError};

pub struct CustomBackend {
    // Custom fields
}

impl Backend for CustomBackend {
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult> {
        // Custom execution logic
        todo!()
    }

    fn name(&self) -> &str {
        "custom"
    }
}
```

## Configuration

Configure backends via Cargo.toml:

```toml
[package.metadata.cleanroom]
backend = "auto"  # auto, docker, podman, local
image = "rust:1-slim"
timeout_ms = 30000
```

Environment variables override config:

```bash
export CLEANROOM_BACKEND=docker
export CLEANROOM_IMAGE=ubuntu:22.04
export CLEANROOM_TIMEOUT_MS=60000
```

## Troubleshooting

### Docker Issues

```bash
# Check Docker daemon status
sudo systemctl status docker

# Pull base image manually
docker pull rust:1-slim

# Check available images
docker images

# Clean up stopped containers
docker container prune
```

### Podman Issues

```bash
# Check Podman installation
podman version

# Pull base image manually
podman pull rust:1-slim

# Run without daemon (if needed)
podman --runtime runc run --rm rust:1-slim echo hello
```

### Local Backend Issues

The local backend logs warnings for unenforceable constraints:

```
WARN: Local backend cannot enforce read-only filesystem
WARN: Local backend cannot enforce network isolation
WARN: Local backend cannot freeze system time
```

These warnings indicate reduced determinism guarantees but don't prevent execution.
