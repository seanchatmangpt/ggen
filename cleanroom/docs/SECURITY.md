# Security Model

Cleanroom enforces security baselines across all execution backends to protect host systems and ensure test isolation.

## Threat Model

Cleanroom assumes:
- **Trusted test code**: Tests are considered safe to execute
- **Untrusted external dependencies**: Network services may be malicious
- **Host system protection**: Primary goal is preventing test escape
- **Resource exhaustion**: Tests should not impact host performance

## Security Baselines

All backends enforce identical security constraints:

### Container Security (Docker/Podman)

```bash
docker run --rm \
  --user=1000:1000 \
  --cap-drop=ALL \
  --read-only \
  --tmpfs=/tmp:rw,nodev,nosuid,size=100m \
  --tmpfs=/workdir:rw,nodev,nosuid,size=100m \
  --workdir=/workdir \
  --network=none \
  --memory=256m \
  --cpus=0.5 \
  --pids-limit=128 \
  --security-opt=no-new-privileges \
  --security-opt=apparmor=unconfined \
  <image> <command>
```

### Local Backend Security

While less isolated, local backend provides:
- Environment variable sanitization
- Working directory isolation
- Resource limits and timeouts
- Sensitive data redaction in forensics

## User Isolation

### Non-Root Execution

All containers run as UID/GID 1000:1000:
- Prevents privilege escalation
- Limits file system access
- Isolates from host processes

### Capability Dropping

All Linux capabilities are dropped (`--cap-drop=ALL`):
- No raw network access
- No module loading
- No device access
- No sysadmin operations

## Filesystem Security

### Read-Only Root Filesystem

Container root filesystem is read-only (`--read-only`):
- Prevents accidental host file modifications
- Forces explicit tmpfs for writable areas
- Maintains container image integrity

### Tmpfs Work Directory

Work directory uses tmpfs with restrictions:
```bash
--tmpfs=/workdir:rw,nodev,nosuid,size=100m
```

- `rw`: Read-write for test operations
- `nodev`: No device files allowed
- `nosuid`: No setuid binaries allowed
- `size=100m`: Memory usage limit

### Temporary Directory

Tmp directory also uses tmpfs:
```bash
--tmpfs=/tmp:rw,nodev,nosuid,size=100m
```

## Network Security

### Default Network Isolation

By default, containers run with no network access (`--network=none`):
- No external API calls
- No DNS resolution
- No network-based attacks

### Limited Network Access

When network access is needed:
```rust
NetProfile::Limited {
    allowed_ports: vec![5432, 6379] // PostgreSQL, Redis
}
```

This creates a network namespace with only whitelisted ports accessible.

## Resource Limits

### Memory Limits

Default memory limit prevents resource exhaustion:
```bash
--memory=256m
```

Configurable via policy:
```rust
ResourceLimits {
    memory_bytes: Some(512 * 1024 * 1024), // 512MB
    ..Default::default()
}
```

### CPU Limits

CPU usage is limited to prevent host impact:
```bash
--cpus=0.5  # Half CPU core
```

### Process Limits

Process count is limited:
```bash
--pids-limit=128
```

## Security Options

### No New Privileges

Prevents privilege escalation:
```bash
--security-opt=no-new-privileges
```

### AppArmor Profile

Applies mandatory access control (when available):
```bash
--security-opt=apparmor=unconfined
```

## Environment Redaction

Sensitive environment variables are automatically redacted:

### Redaction Patterns

- `*_KEY` (API keys, encryption keys)
- `*_TOKEN` (authentication tokens)
- `*_SECRET` (shared secrets, passwords)
- `*_PASSWORD` (database passwords, etc.)
- `*_PASS` (password fields)
- `*_AUTH` (authentication data)
- `AWS_*` (AWS credentials and config)
- `GITHUB_*` (GitHub tokens and secrets)
- `GITLAB_*` (GitLab tokens and secrets)
- `DOCKER_*` (Docker credentials)
- `KUBE_*` (Kubernetes credentials)

### Forensics Bundle

Redacted variables appear as `[REDACTED]` in forensics:
```json
{
  "environment": {
    "API_KEY": "[REDACTED]",
    "DATABASE_URL": "postgresql://user:pass@localhost/db",
    "HOME": "/home/user"
  }
}
```

## Image Security

### Digest Pinning

Container images are pinned by digest when possible:
```rust
DockerBackend::new("rust:1-slim@sha256:...")
```

### Signature Verification

With the `signing` feature enabled:
```rust
impl DockerBackend {
    pub fn verify_signature(&self, image: &str) -> Result<bool>
}
```

Uses `cosign` for signature verification.

## Runtime Security

### Seccomp Filters

Default seccomp profile restricts syscalls:
- No direct hardware access
- Limited file system operations
- Controlled process management

### Timeout Enforcement

All executions have configurable timeouts:
```rust
timeout_ms: Some(30000), // 30 second timeout
```

Prevents runaway processes.

## Configuration Security

### Secure Defaults

All security settings use locked-down defaults:
```rust
impl Default for Policy {
    fn default() -> Self {
        Self::locked() // Maximum security
    }
}
```

### Explicit Opt-In

Non-deterministic features require explicit configuration:
```rust
Policy {
    net: NetProfile::Open, // Must explicitly allow network
    ..Policy::locked()
}
```

## Audit Logging

Security events are logged for compliance:

### Security Events

- Policy violations
- Capability usage attempts
- Network access requests
- Resource limit hits
- Signature verification failures

### Forensics Integration

Security events are captured in forensics bundles:
```json
{
  "logs": [
    {
      "timestamp": 1640995200,
      "level": "WARN",
      "component": "security",
      "message": "Network access blocked by policy",
      "context": {
        "destination": "api.example.com:443"
      }
    }
  ]
}
```

## Compliance Features

### SBOM Generation

Software Bill of Materials for supply chain security:
```rust
use cleanroom::ArtifactCollector;

let sbom = collector.generate_sbom()?;
```

### Provenance Tracking

Execution provenance for audit trails:
```rust
use cleanroom::AttestationGenerator;

let attestation = generator.generate(&run_info)?;
```

## Best Practices

### Test Code Security

1. **Validate inputs**: Sanitize all test inputs
2. **Limit scope**: Use minimal permissions for test operations
3. **Resource cleanup**: Ensure proper cleanup in test teardown
4. **Dependency scanning**: Regularly audit test dependencies

### Host Security

1. **Container user**: Run container daemon as non-root
2. **Image updates**: Keep base images updated
3. **Network policies**: Use container network policies
4. **Resource quotas**: Set appropriate resource limits

### CI/CD Security

1. **Isolated runners**: Use isolated build environments
2. **Artifact signing**: Sign test artifacts
3. **Access controls**: Limit repository access
4. **Dependency checks**: Scan for vulnerable dependencies

## Security Monitoring

### Runtime Monitoring

Monitor for security anomalies:
- Unexpected network connections
- Resource usage spikes
- Privilege escalation attempts
- File system modifications

### Incident Response

1. **Isolate affected systems**: Stop test execution immediately
2. **Capture forensics**: Collect complete execution artifacts
3. **Analyze root cause**: Determine how security was bypassed
4. **Apply fixes**: Update policies and constraints
5. **Verify remediation**: Test fixes across all backends
