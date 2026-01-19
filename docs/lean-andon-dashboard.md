# ANDON - Visual Management & Stopping the Line
## Phase 2: ggen.toml + clap-noun-verb Integration

**LEAN PRINCIPLE**: Make problems visible. Stop and fix issues immediately.

**Analysis Date**: 2025-11-18
**Target**: Real-time quality signals with automatic stopping rules
**Inspiration**: Toyota's Andon Cord - anyone can stop the line when quality suffers

---

## What is ANDON?

**Andon (行灯)** = Visual management system for quality control

In Toyota factories:
- **Green light**: Normal operation
- **Yellow light**: Warning (attention needed)
- **Red light**: Stop the line (critical issue)

Anyone can pull the Andon cord to stop production if they detect a problem.

---

## 🚦 ANDON System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    ANDON DASHBOARD                          │
│  ┌──────────┬──────────┬──────────┬──────────┬──────────┐  │
│  │  BUILD   │  TESTS   │  QUALITY │ SECURITY │  PERF    │  │
│  │          │          │          │          │          │  │
│  │   🟢     │   🟡     │   🟢     │   🔴     │   🟢     │  │
│  │  0.64s   │  95.2%   │  0 warn  │  1 CVE   │  1.2s    │  │
│  └──────────┴──────────┴──────────┴──────────┴──────────┘  │
│                                                             │
│  Status: 🔴 STOPPED - Security issue detected               │
│  Action: Fix CVE-2024-1234 before merging                  │
└─────────────────────────────────────────────────────────────┘
```

---

## 📊 Signal 1: Build Status

### Metrics Tracked

| Metric | Target | Warning | Critical |
|--------|--------|---------|----------|
| Compile time | <3s | 3-5s | >5s |
| Binary size | <10MB | 10-15MB | >15MB |
| Dependencies | <50 | 50-75 | >75 |
| Warnings | 0 | 1-3 | >3 |

### Implementation

```rust
// ANDON: Build status monitoring
#[derive(Debug, Clone)]
struct BuildStatus {
    compile_time: Duration,
    binary_size: u64,
    dependency_count: usize,
    warning_count: usize,
}

impl BuildStatus {
    fn signal(&self) -> Signal {
        // Red (Critical): Stop the line
        if self.compile_time > Duration::from_secs(5) {
            return Signal::Red("Compile time >5s - investigate build optimization");
        }
        if self.warning_count > 3 {
            return Signal::Red("Too many warnings - fix before merging");
        }

        // Yellow (Warning): Needs attention
        if self.compile_time > Duration::from_secs(3) {
            return Signal::Yellow("Compile time 3-5s - consider optimization");
        }
        if self.binary_size > 10 * 1024 * 1024 {
            return Signal::Yellow("Binary >10MB - check for bloat");
        }

        // Green (Normal): All good
        Signal::Green("Build healthy")
    }
}

// Example output:
// 🟢 Build: 0.64s, 8.2MB, 47 deps, 0 warnings
// 🟡 Build: 4.1s, 12.5MB, 53 deps, 1 warning
// 🔴 Build: 6.3s, 16.8MB, 82 deps, 5 warnings - STOP!
```

### Stopping Rule

**STOP THE LINE IF**:
- Compile time >5s (investigate performance regression)
- Any warnings >3 (enforce zero-warning policy gradually)
- Binary size >15MB (check for dependency bloat)

---

## ✅ Signal 2: Test Results

### Metrics Tracked

| Metric | Target | Warning | Critical |
|--------|--------|---------|----------|
| Pass rate | 100% | 95-99% | <95% |
| Coverage | >95% | 80-95% | <80% |
| Test time | <10s | 10-30s | >30s |
| Flaky tests | 0 | 1-2 | >2 |

### Implementation

```rust
// ANDON: Test status monitoring
#[derive(Debug, Clone)]
struct TestStatus {
    total_tests: usize,
    passed: usize,
    failed: usize,
    coverage_percent: f64,
    execution_time: Duration,
    flaky_tests: Vec<String>,
}

impl TestStatus {
    fn pass_rate(&self) -> f64 {
        (self.passed as f64 / self.total_tests as f64) * 100.0
    }

    fn signal(&self) -> Signal {
        // Red (Critical): Stop the line
        if self.failed > 0 {
            return Signal::Red(format!(
                "{} tests FAILED - fix before merging",
                self.failed
            ));
        }
        if self.coverage_percent < 80.0 {
            return Signal::Red(format!(
                "Coverage {:.1}% <80% - add tests",
                self.coverage_percent
            ));
        }
        if !self.flaky_tests.is_empty() {
            return Signal::Red(format!(
                "{} flaky tests detected: {:?}",
                self.flaky_tests.len(),
                self.flaky_tests
            ));
        }

        // Yellow (Warning): Needs attention
        if self.coverage_percent < 95.0 {
            return Signal::Yellow(format!(
                "Coverage {:.1}% <95% - consider adding tests",
                self.coverage_percent
            ));
        }
        if self.execution_time > Duration::from_secs(10) {
            return Signal::Yellow(format!(
                "Test time {:.1}s >10s - optimize slow tests",
                self.execution_time.as_secs_f64()
            ));
        }

        // Green (Normal): All good
        Signal::Green(format!(
            "{}/{} passed, {:.1}% coverage",
            self.passed,
            self.total_tests,
            self.coverage_percent
        ))
    }
}

// Example output:
// 🟢 Tests: 245/245 passed, 97.3% coverage, 8.2s
// 🟡 Tests: 245/245 passed, 92.1% coverage, 12.5s - needs attention
// 🔴 Tests: 243/245 passed (2 FAILED) - STOP!
```

### Stopping Rule

**STOP THE LINE IF**:
- ANY test failure (100% pass required)
- Coverage <80% (quality threshold)
- Flaky tests detected (non-determinism is a bug)

---

## 🔍 Signal 3: Code Quality

### Metrics Tracked

| Metric | Target | Warning | Critical |
|--------|--------|---------|----------|
| Clippy warnings | 0 | 1-3 | >3 |
| Cyclomatic complexity | <10 | 10-15 | >15 |
| Function length | <50 lines | 50-100 | >100 |
| TODO/FIXME count | 0 | 1-5 | >5 |

### Implementation

```rust
// ANDON: Code quality monitoring
#[derive(Debug, Clone)]
struct CodeQuality {
    clippy_warnings: usize,
    max_complexity: usize,
    max_function_length: usize,
    todo_count: usize,
    fixme_count: usize,
}

impl CodeQuality {
    fn signal(&self) -> Signal {
        // Red (Critical): Stop the line
        if self.clippy_warnings > 3 {
            return Signal::Red(format!(
                "{} clippy warnings - fix before merging",
                self.clippy_warnings
            ));
        }
        if self.max_complexity > 15 {
            return Signal::Red(format!(
                "Cyclomatic complexity {} >15 - refactor",
                self.max_complexity
            ));
        }

        // Yellow (Warning): Needs attention
        if self.clippy_warnings > 0 {
            return Signal::Yellow(format!(
                "{} clippy warnings - should fix",
                self.clippy_warnings
            ));
        }
        if self.todo_count + self.fixme_count > 5 {
            return Signal::Yellow(format!(
                "{} TODO/FIXME - consider addressing",
                self.todo_count + self.fixme_count
            ));
        }

        // Green (Normal): All good
        Signal::Green("Code quality excellent")
    }
}

// Example output:
// 🟢 Quality: 0 warnings, complexity 8, 0 TODOs
// 🟡 Quality: 2 warnings, complexity 12, 3 TODOs - needs attention
// 🔴 Quality: 5 warnings, complexity 18, 12 TODOs - STOP!
```

### Stopping Rule

**STOP THE LINE IF**:
- Clippy warnings >3 (enforce quality standards)
- Cyclomatic complexity >15 (code too complex to maintain)
- Function length >100 lines (refactoring needed)

---

## 🛡️ Signal 4: Security Issues

### Metrics Tracked

| Metric | Target | Warning | Critical |
|--------|--------|---------|----------|
| CVE count | 0 | 0 | >0 |
| Secret exposure | 0 | 0 | >0 |
| Path traversal risk | 0 | 0 | >0 |
| Command injection risk | 0 | 0 | >0 |

### Implementation

```rust
// ANDON: Security monitoring
#[derive(Debug, Clone)]
struct SecurityStatus {
    cve_count: usize,
    cve_list: Vec<String>,
    secrets_exposed: Vec<String>,
    path_traversal_risks: Vec<String>,
    injection_risks: Vec<String>,
}

impl SecurityStatus {
    fn signal(&self) -> Signal {
        // Red (Critical): Stop the line IMMEDIATELY
        if self.cve_count > 0 {
            return Signal::Red(format!(
                "{} CVEs detected: {:?} - MUST FIX",
                self.cve_count,
                self.cve_list
            ));
        }
        if !self.secrets_exposed.is_empty() {
            return Signal::Red(format!(
                "{} secrets exposed: {:?} - REMOVE",
                self.secrets_exposed.len(),
                self.secrets_exposed
            ));
        }
        if !self.path_traversal_risks.is_empty() {
            return Signal::Red(format!(
                "{} path traversal risks - SANITIZE",
                self.path_traversal_risks.len()
            ));
        }

        // Green (Normal): All good
        Signal::Green("No security issues")
    }
}

// Example output:
// 🟢 Security: 0 CVEs, 0 secrets, 0 risks
// 🔴 Security: 1 CVE (CVE-2024-1234), 0 secrets, 2 path risks - STOP!
```

### Stopping Rule

**STOP THE LINE IF**:
- ANY CVE detected (security vulnerability)
- ANY secret exposed (API keys, passwords)
- ANY path traversal risk (../../etc/passwd)
- ANY command injection risk (shell escape failure)

**CRITICAL**: Security issues are ALWAYS red (no yellow warnings)

---

## ⚡ Signal 5: Performance

### Metrics Tracked

| Metric | Target | Warning | Critical |
|--------|--------|---------|----------|
| Startup time | <1s | 1-2s | >2s |
| Template gen time | <500ms | 500ms-1s | >1s |
| Memory usage | <50MB | 50-100MB | >100MB |
| CPU usage | <50% | 50-80% | >80% |

### Implementation

```rust
// ANDON: Performance monitoring
#[derive(Debug, Clone)]
struct PerformanceStatus {
    startup_time: Duration,
    generation_time: Duration,
    memory_usage_mb: f64,
    cpu_usage_percent: f64,
}

impl PerformanceStatus {
    fn signal(&self) -> Signal {
        // Red (Critical): Stop the line
        if self.startup_time > Duration::from_secs(2) {
            return Signal::Red(format!(
                "Startup time {:.1}s >2s - investigate regression",
                self.startup_time.as_secs_f64()
            ));
        }
        if self.memory_usage_mb > 100.0 {
            return Signal::Red(format!(
                "Memory usage {:.1}MB >100MB - check for leaks",
                self.memory_usage_mb
            ));
        }

        // Yellow (Warning): Needs attention
        if self.startup_time > Duration::from_secs(1) {
            return Signal::Yellow(format!(
                "Startup time {:.1}s >1s - consider optimization",
                self.startup_time.as_secs_f64()
            ));
        }
        if self.generation_time > Duration::from_millis(500) {
            return Signal::Yellow(format!(
                "Generation time {:.0}ms >500ms - slow",
                self.generation_time.as_millis()
            ));
        }

        // Green (Normal): All good
        Signal::Green(format!(
            "{:.1}s startup, {:.0}ms generation, {:.1}MB memory",
            self.startup_time.as_secs_f64(),
            self.generation_time.as_millis(),
            self.memory_usage_mb
        ))
    }
}

// Example output:
// 🟢 Perf: 0.8s startup, 320ms gen, 42MB memory
// 🟡 Perf: 1.5s startup, 680ms gen, 72MB memory - needs attention
// 🔴 Perf: 2.8s startup, 1.2s gen, 125MB memory - STOP!
```

### Stopping Rule

**STOP THE LINE IF**:
- Startup time >2s (user experience degradation)
- Memory usage >100MB (potential memory leak)
- CPU usage >80% sustained (inefficient algorithm)

---

## 📈 ANDON Dashboard Implementation

### CLI Dashboard

```rust
// ANDON: Real-time dashboard in terminal
fn display_andon_dashboard(status: &SystemStatus) {
    println!("┌─────────────────────────────────────────────────┐");
    println!("│           ANDON QUALITY DASHBOARD             │");
    println!("├─────────────────────────────────────────────────┤");

    // Build status
    let build_signal = status.build.signal();
    println!("│ {} Build:    {} │", build_signal.icon(), build_signal.message());

    // Test status
    let test_signal = status.tests.signal();
    println!("│ {} Tests:    {} │", test_signal.icon(), test_signal.message());

    // Quality status
    let quality_signal = status.quality.signal();
    println!("│ {} Quality:  {} │", quality_signal.icon(), quality_signal.message());

    // Security status
    let security_signal = status.security.signal();
    println!("│ {} Security: {} │", security_signal.icon(), security_signal.message());

    // Performance status
    let perf_signal = status.performance.signal();
    println!("│ {} Perf:     {} │", perf_signal.icon(), perf_signal.message());

    println!("└─────────────────────────────────────────────────┘");

    // Overall status
    let overall = status.overall_signal();
    println!("\nStatus: {} {}", overall.icon(), overall.status());

    if overall.is_red() {
        println!("\n⚠️  LINE STOPPED - Fix critical issues before proceeding!");
        println!("Action: {}", overall.action());
        std::process::exit(1);  // STOP THE LINE
    }
}
```

### Example Dashboard Output

```
┌─────────────────────────────────────────────────┐
│           ANDON QUALITY DASHBOARD             │
├─────────────────────────────────────────────────┤
│ 🟢 Build:    0.64s, 8.2MB, 0 warnings         │
│ 🟡 Tests:    245/245 passed, 92.1% coverage   │
│ 🟢 Quality:  0 warnings, complexity 8         │
│ 🔴 Security: 1 CVE (CVE-2024-1234) detected   │
│ 🟢 Perf:     0.8s startup, 320ms gen          │
└─────────────────────────────────────────────────┘

Status: 🔴 STOPPED - Security issue detected

⚠️  LINE STOPPED - Fix critical issues before proceeding!
Action: Fix CVE-2024-1234 in dependency 'vulnerable-crate'
        Run: cargo update vulnerable-crate
```

---

## 🚨 Stopping Rules Summary

| Condition | Severity | Action |
|-----------|----------|--------|
| **Build**|||
| Compile time >5s | 🔴 Critical | Stop - investigate regression |
| Warnings >3 | 🔴 Critical | Stop - fix warnings |
| Binary size >15MB | 🔴 Critical | Stop - check dependency bloat |
| **Tests**|||
| ANY test failure | 🔴 Critical | Stop - fix failing tests |
| Coverage <80% | 🔴 Critical | Stop - add tests |
| Flaky tests >0 | 🔴 Critical | Stop - fix non-determinism |
| **Quality**|||
| Clippy warnings >3 | 🔴 Critical | Stop - fix warnings |
| Cyclomatic complexity >15 | 🔴 Critical | Stop - refactor |
| Function length >100 lines | 🔴 Critical | Stop - break up function |
| **Security**|||
| ANY CVE detected | 🔴 Critical | Stop - update dependencies |
| ANY secret exposed | 🔴 Critical | Stop - remove secrets |
| ANY path traversal risk | 🔴 Critical | Stop - sanitize paths |
| ANY injection risk | 🔴 Critical | Stop - escape inputs |
| **Performance**|||
| Startup time >2s | 🔴 Critical | Stop - profile and optimize |
| Memory usage >100MB | 🔴 Critical | Stop - check for leaks |
| CPU usage >80% sustained | 🔴 Critical | Stop - optimize algorithm |

---

## 🤖 Automation: CI/CD Integration

```yaml
# .github/workflows/andon.yml
name: ANDON Quality Gates

on: [push, pull_request]

jobs:
  andon-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: 🔨 Build Check
        run: |
          START=$(date +%s)
          cargo build --release
          END=$(date +%s)
          DURATION=$((END - START))
          if [ $DURATION -gt 5 ]; then
            echo "🔴 Build time ${DURATION}s >5s - STOP"
            exit 1
          fi

      - name: ✅ Test Check
        run: |
          cargo test --all-targets
          cargo tarpaulin --out Json > coverage.json
          COVERAGE=$(jq '.coverage' coverage.json)
          if [ $(echo "$COVERAGE < 80" | bc) -eq 1 ]; then
            echo "🔴 Coverage ${COVERAGE}% <80% - STOP"
            exit 1
          fi

      - name: 🔍 Quality Check
        run: |
          cargo clippy --all-targets -- -D warnings
          # Fail if ANY warnings

      - name: 🛡️ Security Check
        run: |
          cargo audit
          # Fail if ANY CVEs

      - name: ⚡ Performance Check
        run: |
          cargo bench --no-fail-fast
          # Fail if regression >10%

      - name: 📊 ANDON Dashboard
        if: always()
        run: |
          cargo run -- andon dashboard
          # Display final status
```

---

## Success Metrics

- **Build Health**: 🟢 Green >95% of time
- **Test Pass Rate**: 100% (0 failures)
- **Code Quality**: 0 clippy warnings
- **Security**: 0 CVEs, 0 secrets exposed
- **Performance**: <1s startup, <500ms generation
- **Line Stops**: <5% of commits (only for critical issues)

**LEAN MOTTO**: "Stop to fix, then resume with quality. A thousand miles of progress built on broken foundations is worthless."
