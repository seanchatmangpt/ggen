# ADR-003: Deterministic Execution Approach

## Status
Accepted

## Context

Non-deterministic test execution causes several problems:
- **Flaky Tests**: Tests pass/fail randomly, reducing confidence
- **Debugging Difficulty**: Hard to reproduce failures
- **CI/CD Instability**: Random failures break pipelines
- **Performance Variance**: Unpredictable execution times
- **Resource Usage**: Inconsistent resource consumption

Sources of non-determinism in testing:
- Random number generation
- System time/clock variations
- Network timing
- File system ordering
- Thread scheduling
- Container startup timing
- Database query execution order

We need a deterministic execution system that:
1. Eliminates randomness from test execution
2. Provides reproducible results across runs
3. Maintains performance
4. Allows controlled randomness when needed
5. Works across different platforms

## Decision

Implement a **Deterministic Execution Manager** with the following design:

### Core Implementation

```rust
pub struct DeterministicManager {
    seed: u64,
    rng: ChaCha20Rng,
    clock_offset: Duration,
    network_delay: Duration,
    file_system_delay: Duration,
}

impl DeterministicManager {
    pub fn new(seed: Option<u64>) -> Self {
        let seed = seed.unwrap_or_else(|| {
            // Use environment variable or generate from system time
            std::env::var("CLEANROOM_DETERMINISTIC_SEED")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or_else(|| SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs())
        });
        
        Self {
            seed,
            rng: ChaCha20Rng::seed_from_u64(seed),
            clock_offset: Duration::from_secs(0),
            network_delay: Duration::from_millis(0),
            file_system_delay: Duration::from_millis(0),
        }
    }
    
    pub fn random(&mut self) -> u64 {
        self.rng.next_u64()
    }
    
    pub fn random_range(&mut self, min: u64, max: u64) -> u64 {
        min + (self.rng.next_u64() % (max - min + 1))
    }
    
    pub fn deterministic_time(&self) -> SystemTime {
        SystemTime::UNIX_EPOCH + Duration::from_secs(1609459200) + self.clock_offset
    }
}
```

### Key Design Elements

1. **Seeded Random Number Generation**: ChaCha20Rng with configurable seed
2. **Deterministic Clock**: Fixed system time for consistent timestamps
3. **Controlled Delays**: Configurable delays for network/filesystem operations
4. **Environment Integration**: Seed from environment variables
5. **Platform Independence**: Works across different operating systems

### Configuration

```rust
pub struct DeterministicConfig {
    pub enable_deterministic_execution: bool,
    pub deterministic_seed: Option<u64>,
    pub clock_offset: Duration,
    pub network_delay: Duration,
    pub file_system_delay: Duration,
    pub thread_scheduling_delay: Duration,
}
```

## Consequences

### Positive

- **Reproducible Tests**: Same results every time
- **Debugging**: Easy to reproduce failures
- **CI/CD Stability**: No random failures
- **Performance Consistency**: Predictable execution times
- **Resource Predictability**: Consistent resource usage

### Negative

- **Real-World Variance**: May not catch timing-related bugs
- **Performance Overhead**: Small overhead from deterministic delays
- **Complexity**: Additional configuration and management
- **Debugging Real Issues**: May mask real timing problems

### Neutral

- **Memory Usage**: Slightly higher due to RNG state
- **Configuration**: Requires careful tuning of delays
- **Platform Differences**: May need platform-specific adjustments

## Alternatives Considered

### 1. No Determinism

Accept non-deterministic execution.

**Rejected because:**
- Causes flaky tests
- Difficult debugging
- Unstable CI/CD
- Poor developer experience

### 2. Complete Determinism

Make everything completely deterministic.

**Rejected because:**
- May hide real timing issues
- Complex to implement
- Performance impact
- Not always desirable

### 3. Optional Determinism

Make determinism optional per test.

**Rejected because:**
- Inconsistent behavior
- Hard to manage
- Confusing for users
- Doesn't solve the core problem

### 4. External Tools

Use external tools like Docker's deterministic mode.

**Rejected because:**
- Platform dependent
- Limited control
- External dependency
- Doesn't cover all sources of non-determinism

## Implementation Details

### Random Number Generation

```rust
impl DeterministicManager {
    pub fn random_string(&mut self, length: usize) -> String {
        const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
        
        (0..length)
            .map(|_| {
                let idx = self.rng.next_u64() as usize % CHARSET.len();
                CHARSET[idx] as char
            })
            .collect()
    }
    
    pub fn random_uuid(&mut self) -> Uuid {
        let bytes = [
            self.rng.next_u64(),
            self.rng.next_u64(),
        ];
        Uuid::from_bytes(bytes)
    }
}
```

### Time Management

```rust
impl DeterministicManager {
    pub fn advance_time(&mut self, duration: Duration) {
        self.clock_offset += duration;
    }
    
    pub fn set_time(&mut self, time: SystemTime) {
        let epoch = SystemTime::UNIX_EPOCH + Duration::from_secs(1609459200);
        self.clock_offset = time.duration_since(epoch).unwrap_or_default();
    }
    
    pub fn sleep(&self, duration: Duration) {
        // In deterministic mode, sleep is a no-op
        // Real sleep only happens in non-deterministic mode
        if !self.config.enable_deterministic_execution {
            std::thread::sleep(duration);
        }
    }
}
```

### Network Simulation

```rust
impl DeterministicManager {
    pub async fn simulate_network_delay(&self) {
        if self.config.enable_deterministic_execution {
            // Add deterministic delay
            tokio::time::sleep(self.network_delay).await;
        }
    }
    
    pub fn simulate_network_failure(&mut self, probability: f64) -> bool {
        if self.config.enable_deterministic_execution {
            // Use deterministic RNG to decide
            let random_value = self.rng.next_u64() as f64 / u64::MAX as f64;
            random_value < probability
        } else {
            // Use system RNG
            fastrand::f64() < probability
        }
    }
}
```

### File System Simulation

```rust
impl DeterministicManager {
    pub async fn simulate_file_system_delay(&self) {
        if self.config.enable_deterministic_execution {
            tokio::time::sleep(self.file_system_delay).await;
        }
    }
    
    pub fn deterministic_file_order(&self, mut files: Vec<PathBuf>) -> Vec<PathBuf> {
        if self.config.enable_deterministic_execution {
            // Sort files deterministically
            files.sort_by(|a, b| {
                let a_hash = self.hash_path(a);
                let b_hash = self.hash_path(b);
                a_hash.cmp(&b_hash)
            });
        }
        files
    }
    
    fn hash_path(&self, path: &Path) -> u64 {
        // Use deterministic hash based on path
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let mut hasher = DefaultHasher::new();
        path.hash(&mut hasher);
        hasher.finish()
    }
}
```

### Integration with Test Framework

```rust
impl CleanroomEnvironment {
    pub async fn execute_test<F, R>(&self, test_name: &str, test_fn: F) -> Result<R>
    where
        F: FnOnce() -> Result<R>,
    {
        // Set up deterministic environment
        if self.config.enable_deterministic_execution {
            self.deterministic_manager.setup_deterministic_environment().await?;
        }
        
        // Execute test
        let result = test_fn();
        
        // Clean up deterministic environment
        if self.config.enable_deterministic_execution {
            self.deterministic_manager.cleanup_deterministic_environment().await?;
        }
        
        result
    }
}
```

## Performance Impact

### Deterministic Mode Overhead
- **Random Generation**: ~0.1ms per operation
- **Time Management**: ~0.01ms per operation
- **Network Simulation**: ~1ms per operation
- **File System Simulation**: ~0.5ms per operation

### Total Overhead
- **Light Tests**: <1% overhead
- **Network-Heavy Tests**: ~5% overhead
- **File System-Heavy Tests**: ~3% overhead
- **Complex Tests**: ~2% overhead

## Configuration Examples

### Development Configuration
```toml
[cleanroom.deterministic]
enable_deterministic_execution = true
deterministic_seed = 42
clock_offset = 0
network_delay = 1ms
file_system_delay = 0.5ms
thread_scheduling_delay = 0.1ms
```

### CI/CD Configuration
```toml
[cleanroom.deterministic]
enable_deterministic_execution = true
deterministic_seed = 12345
clock_offset = 0
network_delay = 2ms
file_system_delay = 1ms
thread_scheduling_delay = 0.2ms
```

### Performance Testing Configuration
```toml
[cleanroom.deterministic]
enable_deterministic_execution = false
# Disable for performance testing
```

## Testing Strategy

### Deterministic Tests
```rust
#[tokio::test]
async fn test_deterministic_execution() {
    let config = CleanroomConfig {
        enable_deterministic_execution: true,
        deterministic_seed: Some(42),
        ..Default::default()
    };
    
    let env = CleanroomEnvironment::new(config).await.unwrap();
    
    // Run test multiple times
    for _ in 0..10 {
        let result = env.execute_test("deterministic_test", || {
            // Test logic here
            Ok("success")
        }).await.unwrap();
        
        assert_eq!(result, "success");
    }
}
```

### Non-Deterministic Tests
```rust
#[tokio::test]
async fn test_real_world_timing() {
    let config = CleanroomConfig {
        enable_deterministic_execution: false,
        ..Default::default()
    };
    
    let env = CleanroomEnvironment::new(config).await.unwrap();
    
    // Test real-world timing behavior
    let start = Instant::now();
    let result = env.execute_test("timing_test", || {
        // Test logic here
        Ok("success")
    }).await.unwrap();
    
    let duration = start.elapsed();
    assert!(duration < Duration::from_secs(5));
}
```

## References

- [ChaCha20 RNG](https://docs.rs/rand_chacha/)
- [Deterministic Testing Patterns](https://testing.googleblog.com/2014/10/testing-on-toilet-dont-make-your-tests.html)
- [Docker Deterministic Mode](https://docs.docker.com/engine/reference/commandline/run/#detach)
- [Rust Random Number Generation](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html)

## Future Considerations

- **Advanced Simulation**: More sophisticated network/filesystem simulation
- **Platform Optimization**: Platform-specific deterministic optimizations
- **Performance Profiling**: Built-in performance profiling for deterministic tests
- **Custom Random Sources**: User-defined random number generators
- **Deterministic Debugging**: Enhanced debugging tools for deterministic execution
