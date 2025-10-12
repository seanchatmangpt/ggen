# Determinism Guarantees

Cleanroom provides multiple surfaces for controlling determinism in test execution.

## Deterministic Surfaces

### Time Determinism

Control time for reproducible test execution:

```rust
use cleanroom::{TimeProfile, scenario};

let result = scenario("time_test")
    .determinism(
        TimeProfile::Frozen(1640995200), // Fixed Unix timestamp
        RngProfile::Seed(42)
    )
    .step("date", ["date", "+%Y-%m-%d"])
    .run()?;
```

#### Time Profiles

- **`Frozen(u64)`**: Fixed Unix timestamp for all operations
- **`Monotonic`**: Predictable time advancement for testing timing logic
- **`System`**: Real system time (default, non-deterministic)

Frozen time ensures:
- Stable timestamps across test runs
- Consistent file modification times
- Predictable timeout behavior

### RNG Determinism

Control randomness for reproducible outputs:

```rust
use cleanroom::{RngProfile, scenario};

let result = scenario("rng_test")
    .determinism(
        TimeProfile::System,
        RngProfile::Seed(12345) // Fixed seed
    )
    .step("uuid", ["uuidgen"])
    .run()?;
```

#### RNG Profiles

- **`Seed(u64)`**: Deterministic pseudo-random sequence
- **`System`**: True randomness (default, non-deterministic)

Seeded RNG ensures:
- Consistent UUID generation
- Predictable random file names
- Stable shuffle operations

### Filesystem Determinism

Control filesystem access patterns:

```rust
use cleanroom::{Policy, FsProfile};

let policy = Policy {
    fs: FsProfile::ReadOnly { workdir: true },
    ..Policy::locked()
};
```

#### Filesystem Profiles

- **`ReadOnly { workdir: bool }`**: Read-only rootfs with optional tmpfs workdir
- **`Writable { workdir: bool }`**: Full filesystem access (dangerous for determinism)

Read-only execution ensures:
- No accidental file modifications
- Consistent initial state
- Tmpfs workdir for test outputs

### Network Determinism

Control network access for tests:

```rust
use cleanroom::{Policy, NetProfile};

let policy = Policy {
    net: NetProfile::Offline, // No network access
    ..Policy::locked()
};
```

#### Network Profiles

- **`Offline`**: No network access (default)
- **`Limited { allowed_ports: Vec<u16> }`**: Whitelist specific ports
- **`Open`**: Full network access (non-deterministic)

Network isolation ensures:
- No external API dependencies
- Consistent test timing
- No network-induced flakiness

### Process Determinism

Control process execution environment:

```rust
use cleanroom::{Policy, ProcProfile};

let policy = Policy {
    proc: ProcProfile::Isolated,
    ..Policy::locked()
};
```

#### Process Profiles

- **`Standard`**: Basic process execution
- **`Isolated`**: UID/GID mapping, resource limits
- **`Strict`**: Maximum isolation for production

Process isolation ensures:
- Consistent UID/GID across environments
- Resource limit enforcement
- Capability restrictions

## Determinism Enforcement

### Container Backends (Docker/Podman)

Full determinism enforcement:
- Time freezing via container environment
- RNG seeding via environment variables
- Filesystem isolation via bind mounts and tmpfs
- Network isolation via network namespaces
- Process isolation via user namespaces

### Local Backend

Best-effort determinism:
- Logs warnings for unenforceable constraints
- Attempts environment variable injection
- Provides working directory isolation
- Enforces timeouts and resource limits

## Environment Variables

Cleanroom injects determinism controls via environment variables:

```bash
# Time control
CLEANROOM_TIME_FROZEN=1640995200

# RNG control
CLEANROOM_RNG_SEED=42

# Filesystem control
CLEANROOM_FS_READONLY=1
CLEANROOM_WORKDIR_TMPFS=1

# Network control
CLEANROOM_NET_OFFLINE=1

# Process control
CLEANROOM_UID=1000
CLEANROOM_GID=1000
```

## Testing Deterministic Behavior

### Acceptance Tests

Cleanroom includes acceptance tests for determinism:

```rust
#[test]
fn test_time_frozen() {
    let result1 = scenario("time_test")
        .determinism(TimeProfile::Frozen(1640995200), RngProfile::Seed(42))
        .step("date", ["date"])
        .run()?;

    let result2 = scenario("time_test")
        .determinism(TimeProfile::Frozen(1640995200), RngProfile::Seed(42))
        .step("date", ["date"])
        .run()?;

    // Outputs should be identical
    assert_eq!(result1.stdout, result2.stdout);
}

#[test]
fn test_rng_seeded() {
    let result1 = scenario("uuid_test")
        .determinism(TimeProfile::System, RngProfile::Seed(42))
        .step("uuid", ["uuidgen"])
        .run()?;

    let result2 = scenario("uuid_test")
        .determinism(TimeProfile::System, RngProfile::Seed(42))
        .step("uuid", ["uuidgen"])
        .run()?;

    // UUIDs should be identical
    assert_eq!(result1.stdout, result2.stdout);
}
```

## Common Determinism Issues

### Timestamp Variance

**Problem**: Tests fail due to timestamp differences

**Solution**:
```rust
.determinism(TimeProfile::Frozen(1640995200), RngProfile::Seed(42))
```

### Randomness in Outputs

**Problem**: Tests fail due to random file names, UUIDs, etc.

**Solution**:
```rust
.determinism(TimeProfile::System, RngProfile::Seed(42))
```

### File System State

**Problem**: Tests depend on existing files or directories

**Solution**:
```rust
policy(Policy {
    fs: FsProfile::ReadOnly { workdir: true },
    ..Policy::locked()
})
```

### Network Dependencies

**Problem**: Tests fail due to network issues or timing

**Solution**:
```rust
policy(Policy {
    net: NetProfile::Offline,
    ..Policy::locked()
})
```

### Process Environment

**Problem**: Tests behave differently based on user permissions

**Solution**:
```rust
policy(Policy {
    proc: ProcProfile::Isolated,
    ..Policy::locked()
})
```

## Performance Impact

Determinism features have minimal performance impact:

| Feature | Overhead | When to Use |
|---------|----------|-------------|
| Time Frozen | <1% | Timestamp-sensitive tests |
| RNG Seeded | <1% | Randomness-sensitive tests |
| FS ReadOnly | 5-10% | File system tests |
| Net Offline | <1% | Network-independent tests |
| Proc Isolated | 10-20% | Permission-sensitive tests |

Use only the determinism features your tests actually need to minimize overhead.
