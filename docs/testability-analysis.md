# Lifecycle System Testability Analysis

## Executive Summary

The ggen lifecycle system has **severe testability issues** that prevent London School TDD. The code directly accesses the filesystem, executes shell commands, and uses concrete types throughout. This analysis identifies specific problems and provides refactoring strategies with code examples.

---

## Current Testability Issues

### 🔴 Critical: Hard Dependencies

#### 1. Direct Shell Execution (`exec.rs:160-185`)

**Problem**: `execute_command()` directly instantiates `std::process::Command`

```rust
// CURRENT: Untestable
fn execute_command(cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
    let mut command = if cfg!(target_os = "windows") {
        let mut c = Command::new("cmd");
        c.arg("/C");
        c
    } else {
        let mut c = Command::new("sh");
        c.arg("-lc");
        c
    };

    command.current_dir(cwd).arg(cmd);
    let status = command.status()?;  // ❌ Cannot mock!
    // ...
}
```

**Impact**:
- ❌ Cannot test without actually running shell commands
- ❌ Cannot verify command arguments passed
- ❌ Cannot simulate command failures
- ❌ Tests are slow and brittle

---

#### 2. Direct Filesystem Access (`state.rs:46-64`)

**Problem**: `load_state()` and `save_state()` directly call `std::fs`

```rust
// CURRENT: Untestable
pub fn load_state<P: AsRef<Path>>(path: P) -> LifecycleState {
    std::fs::read_to_string(path)  // ❌ Cannot mock filesystem!
        .ok()
        .and_then(|s| serde_json::from_str(&s).ok())
        .unwrap_or_default()
}

pub fn save_state<P: AsRef<Path>>(path: P, state: &LifecycleState) -> std::io::Result<()> {
    if let Some(parent) = path.as_ref().parent() {
        std::fs::create_dir_all(parent)?;  // ❌ Cannot mock!
    }
    let json = serde_json::to_string_pretty(state)?;
    std::fs::write(path, json)?;  // ❌ Cannot mock!
    Ok(())
}
```

**Impact**:
- ❌ Cannot test without touching real filesystem
- ❌ Requires complex test setup/teardown
- ❌ Tests pollute filesystem
- ❌ Parallel test execution is unsafe

---

#### 3. Direct Filesystem in Cache (`cache.rs:44-46`)

**Problem**: Cache reads files directly in hash computation

```rust
// CURRENT: Untestable
pub fn cache_key(..., inputs: &[String]) -> String {
    // ...
    for input_path in inputs {
        hasher.update(input_path.as_bytes());

        // ❌ Direct filesystem access!
        if let Ok(content) = std::fs::read(input_path) {
            hasher.update(&content);
        }
    }
    // ...
}
```

**Impact**:
- ❌ Cannot test cache key generation without real files
- ❌ Cannot simulate file read errors
- ❌ Cannot verify hash inputs

---

### 🟠 Major: Tight Coupling

#### 1. Context Struct with Borrowed Lifetimes (`exec.rs:10-15`)

**Problem**: Rigid lifetime requirements prevent flexible composition

```rust
// CURRENT: Inflexible
pub struct Context<'a> {
    pub root: &'a Path,
    pub make: &'a Make,
    pub state_path: &'a Path,
    pub env: Vec<(String, String)>,
}
```

**Issues**:
- Borrow checker fights during testing
- Cannot easily create test fixtures
- Cannot swap implementations
- Hard to use in mocks

---

#### 2. Recursive Function Calls (`exec.rs:100-127`)

**Problem**: Hooks call `run_phase()` which calls hooks recursively

```rust
// CURRENT: Tightly coupled
fn run_before_hooks(ctx: &Context, phase_name: &str) -> Result<()> {
    if let Some(hooks) = &ctx.make.hooks {
        if let Some(before_all) = &hooks.before_all {
            for hook_phase in before_all {
                run_phase(ctx, hook_phase)?;  // ❌ Recursive call!
            }
        }
        // ...
    }
    Ok(())
}
```

**Impact**:
- ❌ Cannot verify hook execution without running phases
- ❌ Cannot test hooks in isolation
- ❌ Infinite recursion risk
- ❌ Stack overflow potential

---

### 🟡 Moderate: Hidden Dependencies

#### 1. Global Time Functions (`exec.rs:188-193`)

**Problem**: Direct `SystemTime` calls prevent time mocking

```rust
// CURRENT: Untestable time
fn current_time_ms() -> u128 {
    std::time::SystemTime::now()  // ❌ Cannot mock time!
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis()
}
```

**Impact**:
- ❌ Cannot test time-dependent behavior
- ❌ Cannot simulate time-based failures
- ❌ Tests are non-deterministic

---

#### 2. Hard-coded OS Detection (`exec.rs:161`)

**Problem**: `cfg!` macro prevents cross-platform testing

```rust
// CURRENT: Platform-specific
let mut command = if cfg!(target_os = "windows") {  // ❌ Compile-time!
    let mut c = Command::new("cmd");
    c.arg("/C");
    c
} else {
    let mut c = Command::new("sh");
    c.arg("-lc");
    c
};
```

**Impact**:
- ❌ Cannot test Windows behavior on Unix
- ❌ Cannot test Unix behavior on Windows
- ❌ Platform-specific bugs

---

### 🔵 Minor: Lack of Seam Points

#### 1. No Dependency Injection

All dependencies are created inline:
- `std::process::Command` instantiated directly
- `std::fs` called directly
- `std::time::SystemTime` accessed directly

#### 2. No Trait Abstractions

No interfaces for:
- Command execution
- Filesystem operations
- State persistence
- Time providers

#### 3. Static Methods Only

All functions are free functions, not methods on structs with injected dependencies.

---

## Refactoring Recommendations

### ✅ Strategy 1: Extract Trait Abstractions

#### CommandExecutor Trait

**BEFORE** (`exec.rs:160-185`):
```rust
fn execute_command(cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
    let mut command = if cfg!(target_os = "windows") {
        let mut c = Command::new("cmd");
        c.arg("/C");
        c
    } else {
        let mut c = Command::new("sh");
        c.arg("-lc");
        c
    };

    command.current_dir(cwd).arg(cmd);
    for (key, value) in env {
        command.env(key, value);
    }

    let status = command.status()?;
    if !status.success() {
        return Err(anyhow::anyhow!("Command failed: {}", cmd));
    }
    Ok(())
}
```

**AFTER** (Refactored with trait):
```rust
// NEW: Testable trait abstraction
pub trait CommandExecutor {
    fn execute(&self, cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()>;
}

// Production implementation
pub struct ShellExecutor;

impl CommandExecutor for ShellExecutor {
    fn execute(&self, cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
        let mut command = if cfg!(target_os = "windows") {
            let mut c = Command::new("cmd");
            c.arg("/C");
            c
        } else {
            let mut c = Command::new("sh");
            c.arg("-lc");
            c
        };

        command.current_dir(cwd).arg(cmd);
        for (key, value) in env {
            command.env(key, value);
        }

        let status = command.status()?;
        if !status.success() {
            return Err(anyhow::anyhow!("Command failed: {}", cmd));
        }
        Ok(())
    }
}

// Test double (mock)
#[cfg(test)]
pub struct MockCommandExecutor {
    pub calls: std::sync::Mutex<Vec<(String, PathBuf, Vec<(String, String)>)>>,
    pub results: std::sync::Mutex<Vec<Result<()>>>,
}

#[cfg(test)]
impl CommandExecutor for MockCommandExecutor {
    fn execute(&self, cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
        self.calls.lock().unwrap().push((
            cmd.to_string(),
            cwd.to_path_buf(),
            env.to_vec()
        ));
        self.results.lock().unwrap().pop()
            .unwrap_or_else(|| Ok(()))
    }
}
```

**Benefits**:
- ✅ Can verify commands without execution
- ✅ Can simulate failures
- ✅ Fast, isolated tests
- ✅ No platform dependencies in tests

---

#### StateRepository Trait

**BEFORE** (`state.rs:46-64`):
```rust
pub fn load_state<P: AsRef<Path>>(path: P) -> LifecycleState {
    std::fs::read_to_string(path)
        .ok()
        .and_then(|s| serde_json::from_str(&s).ok())
        .unwrap_or_default()
}

pub fn save_state<P: AsRef<Path>>(path: P, state: &LifecycleState) -> std::io::Result<()> {
    if let Some(parent) = path.as_ref().parent() {
        std::fs::create_dir_all(parent)?;
    }
    let json = serde_json::to_string_pretty(state)?;
    std::fs::write(path, json)?;
    Ok(())
}
```

**AFTER** (Refactored with trait):
```rust
// NEW: Testable trait abstraction
pub trait StateRepository {
    fn load(&self, path: &Path) -> LifecycleState;
    fn save(&self, path: &Path, state: &LifecycleState) -> Result<()>;
}

// Production implementation
pub struct FileStateRepository;

impl StateRepository for FileStateRepository {
    fn load(&self, path: &Path) -> LifecycleState {
        std::fs::read_to_string(path)
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok())
            .unwrap_or_default()
    }

    fn save(&self, path: &Path, state: &LifecycleState) -> Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let json = serde_json::to_string_pretty(state)?;
        std::fs::write(path, json)?;
        Ok(())
    }
}

// Test double (mock)
#[cfg(test)]
pub struct MockStateRepository {
    pub states: std::sync::Mutex<std::collections::HashMap<PathBuf, LifecycleState>>,
    pub load_calls: std::sync::Mutex<Vec<PathBuf>>,
    pub save_calls: std::sync::Mutex<Vec<(PathBuf, LifecycleState)>>,
}

#[cfg(test)]
impl StateRepository for MockStateRepository {
    fn load(&self, path: &Path) -> LifecycleState {
        self.load_calls.lock().unwrap().push(path.to_path_buf());
        self.states.lock().unwrap()
            .get(path)
            .cloned()
            .unwrap_or_default()
    }

    fn save(&self, path: &Path, state: &LifecycleState) -> Result<()> {
        self.save_calls.lock().unwrap().push((path.to_path_buf(), state.clone()));
        self.states.lock().unwrap().insert(path.to_path_buf(), state.clone());
        Ok(())
    }
}
```

**Benefits**:
- ✅ No filesystem access in tests
- ✅ Can verify save/load calls
- ✅ Fast, parallel tests
- ✅ Deterministic behavior

---

#### FileReader Trait (for cache)

**BEFORE** (`cache.rs:13-50`):
```rust
pub fn cache_key(
    phase_name: &str,
    cmd_lines: &[String],
    env: &[(String, String)],
    inputs: &[String],
) -> String {
    let mut hasher = Sha256::new();
    // ... hashing logic ...

    for input_path in inputs {
        hasher.update(input_path.as_bytes());

        // Direct filesystem access!
        if let Ok(content) = std::fs::read(input_path) {
            hasher.update(&content);
        }
    }

    format!("{:x}", hasher.finalize())
}
```

**AFTER** (Refactored with trait):
```rust
// NEW: Testable trait abstraction
pub trait FileReader {
    fn read(&self, path: &str) -> Option<Vec<u8>>;
}

// Production implementation
pub struct RealFileReader;

impl FileReader for RealFileReader {
    fn read(&self, path: &str) -> Option<Vec<u8>> {
        std::fs::read(path).ok()
    }
}

// Refactored cache_key with dependency injection
pub fn cache_key<R: FileReader>(
    phase_name: &str,
    cmd_lines: &[String],
    env: &[(String, String)],
    inputs: &[String],
    file_reader: &R,  // ✅ Injected dependency!
) -> String {
    let mut hasher = Sha256::new();
    hasher.update(phase_name.as_bytes());

    for cmd in cmd_lines {
        hasher.update(b"\n");
        hasher.update(cmd.as_bytes());
    }

    for (key, value) in env {
        hasher.update(b"\n");
        hasher.update(key.as_bytes());
        hasher.update(b"=");
        hasher.update(value.as_bytes());
    }

    for input_path in inputs {
        hasher.update(b"\n");
        hasher.update(input_path.as_bytes());

        // ✅ Use injected file reader!
        if let Some(content) = file_reader.read(input_path) {
            hasher.update(&content);
        }
    }

    format!("{:x}", hasher.finalize())
}

// Test double (mock)
#[cfg(test)]
pub struct MockFileReader {
    pub files: std::collections::HashMap<String, Vec<u8>>,
}

#[cfg(test)]
impl FileReader for MockFileReader {
    fn read(&self, path: &str) -> Option<Vec<u8>> {
        self.files.get(path).cloned()
    }
}
```

**Benefits**:
- ✅ No filesystem in tests
- ✅ Deterministic hash testing
- ✅ Can test missing files
- ✅ Fast cache key generation

---

#### TimeProvider Trait

**BEFORE** (`exec.rs:188-193`):
```rust
fn current_time_ms() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis()
}
```

**AFTER** (Refactored with trait):
```rust
// NEW: Testable trait abstraction
pub trait TimeProvider {
    fn now_ms(&self) -> u128;
}

// Production implementation
pub struct SystemTimeProvider;

impl TimeProvider for SystemTimeProvider {
    fn now_ms(&self) -> u128 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis()
    }
}

// Test double (mock)
#[cfg(test)]
pub struct MockTimeProvider {
    pub time: std::sync::Mutex<u128>,
}

#[cfg(test)]
impl TimeProvider for MockTimeProvider {
    fn now_ms(&self) -> u128 {
        *self.time.lock().unwrap()
    }
}

#[cfg(test)]
impl MockTimeProvider {
    pub fn advance(&self, ms: u128) {
        *self.time.lock().unwrap() += ms;
    }
}
```

**Benefits**:
- ✅ Deterministic time in tests
- ✅ Can simulate time passage
- ✅ Fast tests (no sleeps)

---

### ✅ Strategy 2: Introduce Seam Points with Builder Pattern

**BEFORE** (`exec.rs:10-15`):
```rust
pub struct Context<'a> {
    pub root: &'a Path,
    pub make: &'a Make,
    pub state_path: &'a Path,
    pub env: Vec<(String, String)>,
}

pub fn run_phase(ctx: &Context, phase_name: &str) -> Result<()> {
    // Direct calls to global functions
    run_before_hooks(ctx, phase_name)?;
    execute_command(...)?;
    let mut state = load_state(ctx.state_path);
    save_state(ctx.state_path, &state)?;
    run_after_hooks(ctx, phase_name)?;
    Ok(())
}
```

**AFTER** (Refactored with dependency injection):
```rust
// NEW: Dependency container with owned types
pub struct PhaseExecutor<E, S, T>
where
    E: CommandExecutor,
    S: StateRepository,
    T: TimeProvider,
{
    executor: E,
    state_repo: S,
    time_provider: T,
}

impl<E, S, T> PhaseExecutor<E, S, T>
where
    E: CommandExecutor,
    S: StateRepository,
    T: TimeProvider,
{
    pub fn new(executor: E, state_repo: S, time_provider: T) -> Self {
        Self { executor, state_repo, time_provider }
    }

    pub fn run_phase(
        &self,
        root: &Path,
        make: &Make,
        state_path: &Path,
        env: &[(String, String)],
        phase_name: &str,
    ) -> Result<()> {
        let phase = make.lifecycle.get(phase_name)
            .ok_or_else(|| anyhow::anyhow!("Phase '{}' not found", phase_name))?;

        let cmds = get_phase_commands(phase);
        if cmds.is_empty() {
            println!("⚠️  Phase '{}' has no commands", phase_name);
            return Ok(());
        }

        // ✅ Use injected dependencies!
        let started = self.time_provider.now_ms();
        let timer = Instant::now();

        println!("▶️  Running phase: {}", phase_name);
        for cmd in &cmds {
            self.executor.execute(cmd, root, env)?;
        }

        let duration = timer.elapsed().as_millis();
        println!("✅ Phase '{}' completed in {}ms", phase_name, duration);

        let mut state = self.state_repo.load(state_path);
        state.record_run(phase_name.to_string(), started, duration, true);
        self.state_repo.save(state_path, &state)?;

        Ok(())
    }
}

// Builder for easier construction
pub struct PhaseExecutorBuilder<E, S, T> {
    executor: Option<E>,
    state_repo: Option<S>,
    time_provider: Option<T>,
}

impl PhaseExecutorBuilder<ShellExecutor, FileStateRepository, SystemTimeProvider> {
    pub fn default_production() -> PhaseExecutor<ShellExecutor, FileStateRepository, SystemTimeProvider> {
        PhaseExecutor::new(
            ShellExecutor,
            FileStateRepository,
            SystemTimeProvider,
        )
    }
}

#[cfg(test)]
impl PhaseExecutorBuilder<MockCommandExecutor, MockStateRepository, MockTimeProvider> {
    pub fn default_test() -> PhaseExecutor<MockCommandExecutor, MockStateRepository, MockTimeProvider> {
        PhaseExecutor::new(
            MockCommandExecutor {
                calls: std::sync::Mutex::new(vec![]),
                results: std::sync::Mutex::new(vec![]),
            },
            MockStateRepository {
                states: std::sync::Mutex::new(std::collections::HashMap::new()),
                load_calls: std::sync::Mutex::new(vec![]),
                save_calls: std::sync::Mutex::new(vec![]),
            },
            MockTimeProvider {
                time: std::sync::Mutex::new(1000),
            },
        )
    }
}
```

**Benefits**:
- ✅ All dependencies injected
- ✅ No lifetime issues
- ✅ Easy to create test fixtures
- ✅ Clear ownership model

---

### ✅ Strategy 3: Extract Hook Registry

**BEFORE** (`exec.rs:100-157`):
```rust
fn run_before_hooks(ctx: &Context, phase_name: &str) -> Result<()> {
    if let Some(hooks) = &ctx.make.hooks {
        if let Some(before_all) = &hooks.before_all {
            for hook_phase in before_all {
                run_phase(ctx, hook_phase)?;  // ❌ Recursive!
            }
        }

        let before_hooks = match phase_name {
            "init" => &hooks.before_init,
            "setup" => &hooks.before_setup,
            "build" => &hooks.before_build,
            "test" => &hooks.before_test,
            "deploy" => &hooks.before_deploy,
            _ => &None,
        };

        if let Some(hooks_list) = before_hooks {
            for hook_phase in hooks_list {
                run_phase(ctx, hook_phase)?;  // ❌ Recursive!
            }
        }
    }
    Ok(())
}
```

**AFTER** (Refactored with registry):
```rust
// NEW: Testable hook registry
pub trait HookRegistry {
    fn before_hooks(&self, phase: &str) -> Vec<String>;
    fn after_hooks(&self, phase: &str) -> Vec<String>;
}

// Production implementation
pub struct MakeHookRegistry<'a> {
    hooks: Option<&'a Hooks>,
}

impl<'a> HookRegistry for MakeHookRegistry<'a> {
    fn before_hooks(&self, phase: &str) -> Vec<String> {
        let mut result = Vec::new();

        if let Some(hooks) = self.hooks {
            // Global before_all
            if let Some(before_all) = &hooks.before_all {
                result.extend(before_all.clone());
            }

            // Phase-specific hooks
            let phase_hooks = match phase {
                "init" => &hooks.before_init,
                "setup" => &hooks.before_setup,
                "build" => &hooks.before_build,
                "test" => &hooks.before_test,
                "deploy" => &hooks.before_deploy,
                _ => &None,
            };

            if let Some(hooks_list) = phase_hooks {
                result.extend(hooks_list.clone());
            }
        }

        result
    }

    fn after_hooks(&self, phase: &str) -> Vec<String> {
        // Similar logic...
        Vec::new()
    }
}

// Test double (spy)
#[cfg(test)]
pub struct SpyHookRegistry {
    pub before_calls: std::sync::Mutex<Vec<String>>,
    pub after_calls: std::sync::Mutex<Vec<String>>,
    pub before_hooks: std::collections::HashMap<String, Vec<String>>,
    pub after_hooks: std::collections::HashMap<String, Vec<String>>,
}

#[cfg(test)]
impl HookRegistry for SpyHookRegistry {
    fn before_hooks(&self, phase: &str) -> Vec<String> {
        self.before_calls.lock().unwrap().push(phase.to_string());
        self.before_hooks.get(phase)
            .cloned()
            .unwrap_or_default()
    }

    fn after_hooks(&self, phase: &str) -> Vec<String> {
        self.after_calls.lock().unwrap().push(phase.to_string());
        self.after_hooks.get(phase)
            .cloned()
            .unwrap_or_default()
    }
}

// Updated PhaseExecutor
impl<E, S, T, H> PhaseExecutor<E, S, T, H>
where
    E: CommandExecutor,
    S: StateRepository,
    T: TimeProvider,
    H: HookRegistry,
{
    pub fn run_phase_with_hooks(
        &self,
        phase_name: &str,
        // ... other params
    ) -> Result<()> {
        // Get hooks from registry (no recursion!)
        let before = self.hook_registry.before_hooks(phase_name);

        // Execute hooks explicitly
        for hook in before {
            self.run_phase(&hook)?;  // ✅ Controlled recursion
        }

        // Run main phase
        self.run_phase(phase_name)?;

        // After hooks
        let after = self.hook_registry.after_hooks(phase_name);
        for hook in after {
            self.run_phase(&hook)?;
        }

        Ok(())
    }
}
```

**Benefits**:
- ✅ Can verify hook calls without execution
- ✅ No uncontrolled recursion
- ✅ Testable in isolation
- ✅ Clear hook ordering

---

## Example: Complete TDD Test Suite

With the refactored design, we can write London School TDD tests:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_phase_executes_commands_in_order() {
        // Arrange
        let executor = MockCommandExecutor::default();
        let state_repo = MockStateRepository::default();
        let time_provider = MockTimeProvider::new(1000);
        let hook_registry = SpyHookRegistry::default();

        let phase_exec = PhaseExecutor::new(
            executor.clone(),
            state_repo,
            time_provider,
            hook_registry,
        );

        let mut make = Make::default();
        make.lifecycle.insert("build".to_string(), Phase {
            commands: Some(vec![
                "cargo build".to_string(),
                "cargo test".to_string(),
            ]),
            ..Default::default()
        });

        // Act
        phase_exec.run_phase(
            Path::new("/project"),
            &make,
            Path::new(".ggen/state.json"),
            &[],
            "build",
        ).unwrap();

        // Assert
        let calls = executor.calls.lock().unwrap();
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].0, "cargo build");
        assert_eq!(calls[1].0, "cargo test");
    }

    #[test]
    fn test_run_phase_saves_state_after_success() {
        // Arrange
        let executor = MockCommandExecutor::default();
        let state_repo = MockStateRepository::default();
        let time_provider = MockTimeProvider::new(1000);

        let phase_exec = PhaseExecutor::new(executor, state_repo.clone(), time_provider);

        // Act
        phase_exec.run_phase(/* ... */).unwrap();

        // Assert
        let save_calls = state_repo.save_calls.lock().unwrap();
        assert_eq!(save_calls.len(), 1);

        let saved_state = &save_calls[0].1;
        assert_eq!(saved_state.last_phase, Some("build".to_string()));
    }

    #[test]
    fn test_run_phase_calls_before_hooks() {
        // Arrange
        let hook_registry = SpyHookRegistry::default();
        hook_registry.before_hooks.insert(
            "build".to_string(),
            vec!["lint".to_string(), "test".to_string()]
        );

        let phase_exec = PhaseExecutor::new(
            MockCommandExecutor::default(),
            MockStateRepository::default(),
            MockTimeProvider::new(1000),
            hook_registry.clone(),
        );

        // Act
        phase_exec.run_phase_with_hooks("build").unwrap();

        // Assert
        let calls = hook_registry.before_calls.lock().unwrap();
        assert!(calls.contains(&"build".to_string()));
    }

    #[test]
    fn test_command_failure_prevents_state_save() {
        // Arrange
        let executor = MockCommandExecutor::default();
        executor.results.lock().unwrap().push(
            Err(anyhow::anyhow!("Command failed"))
        );

        let state_repo = MockStateRepository::default();
        let phase_exec = PhaseExecutor::new(
            executor,
            state_repo.clone(),
            MockTimeProvider::new(1000),
        );

        // Act
        let result = phase_exec.run_phase(/* ... */);

        // Assert
        assert!(result.is_err());
        assert_eq!(state_repo.save_calls.lock().unwrap().len(), 0);
    }
}
```

---

## Implementation Roadmap

### Phase 1: Extract Traits (Week 1)
1. Create `CommandExecutor` trait with mock
2. Create `StateRepository` trait with mock
3. Create `FileReader` trait with mock
4. Create `TimeProvider` trait with mock
5. Add tests proving traits work

### Phase 2: Refactor Context (Week 2)
1. Create `PhaseExecutor` struct with generics
2. Add constructor injection
3. Create builder pattern
4. Update call sites
5. Remove old `Context` struct

### Phase 3: Hook Registry (Week 3)
1. Create `HookRegistry` trait
2. Implement production registry
3. Create spy for testing
4. Refactor hook execution
5. Add hook ordering tests

### Phase 4: TDD Test Suite (Week 4)
1. Write interaction tests
2. Write state verification tests
3. Write hook execution tests
4. Write error handling tests
5. Achieve 90%+ coverage

---

## Success Metrics

### Before Refactoring
- ❌ 0% unit test coverage (no mocks possible)
- ❌ Tests require real filesystem
- ❌ Tests require shell execution
- ❌ Cannot verify interactions
- ❌ Slow, brittle tests

### After Refactoring
- ✅ 90%+ unit test coverage
- ✅ No filesystem access in tests
- ✅ No shell execution in tests
- ✅ Full interaction verification
- ✅ Fast, reliable tests
- ✅ London School TDD ready

---

## Conclusion

The current lifecycle system is **not testable** using London School TDD due to:
1. Direct filesystem access
2. Direct shell execution
3. No trait abstractions
4. No dependency injection
5. Tight coupling

The refactoring strategy provides:
1. **Trait abstractions** for all external dependencies
2. **Constructor injection** for testability
3. **Mock implementations** for verification
4. **Builder patterns** for easy construction
5. **Clear seam points** for test doubles

This enables **true London School TDD** with fast, isolated, interaction-based tests.
