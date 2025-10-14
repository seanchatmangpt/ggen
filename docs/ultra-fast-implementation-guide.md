# Ultra-Fast Workflow: Implementation Guide

**Goal:** Implement the <60-second concept-to-deploy workflow in your ggen + cleanroom setup

## 🎯 Quick Implementation Checklist

- [ ] **Phase 1:** Pre-built container images (20 min)
- [ ] **Phase 2:** Template optimization (15 min)
- [ ] **Phase 3:** Cleanroom configuration (10 min)
- [ ] **Phase 4:** Workflow integration (15 min)
- [ ] **Phase 5:** Testing and validation (10 min)

**Total implementation time:** ~70 minutes

---

## Phase 1: Pre-Built Container Images (20 min)

### 1.1 Create Optimized Rust Image

```dockerfile
# docker/ggen-rust-fast.dockerfile
FROM rust:1.75-slim

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Pre-install Rust components
RUN rustup component add clippy rustfmt

# Pre-compile common dependencies
WORKDIR /tmp/warmup
RUN cargo init --lib dummy && cd dummy && \
    echo 'serde = { version = "1.0", features = ["derive"] }' >> Cargo.toml && \
    echo 'tokio = { version = "1.0", features = ["full"] }' >> Cargo.toml && \
    echo 'axum = "0.7"' >> Cargo.toml && \
    echo 'anyhow = "1.0"' >> Cargo.toml && \
    echo 'thiserror = "1.0"' >> Cargo.toml && \
    echo 'clap = { version = "4.0", features = ["derive"] }' >> Cargo.toml && \
    cargo build --release && \
    rm -rf target/release/deps/dummy* && \
    cd .. && rm -rf dummy

# Set up cargo cache directory
ENV CARGO_HOME=/usr/local/cargo
RUN mkdir -p $CARGO_HOME/registry && chmod -R 777 $CARGO_HOME

# Set working directory
WORKDIR /workspace

# Keep container running
CMD ["sleep", "infinity"]
```

### 1.2 Build and Tag Images

```bash
# Build the fast image
docker build -f docker/ggen-rust-fast.dockerfile -t ggen/rust-fast:latest .

# Tag for versioning
docker tag ggen/rust-fast:latest ggen/rust-fast:1.0.0

# Push to registry (optional)
docker push ggen/rust-fast:latest
docker push ggen/rust-fast:1.0.0

# Verify image works
docker run --rm ggen/rust-fast:latest cargo --version
```

**Expected output:**
```
cargo 1.75.0 (1d8b05cdd 2024-01-01)
```

---

## Phase 2: Template Optimization (15 min)

### 2.1 Add Cleanroom Metadata to Templates

```yaml
---
# templates/rust/axum-service/main.tmpl
to: "src/main.rs"
vars:
  name: "example_service"
  port: 8080
  version: "0.1.0"

# Cleanroom test configuration
cleanroom:
  image: "ggen/rust-fast:latest"
  enable_singleton: true
  test_timeout_ms: 20000

  test_pipeline:
    - name: cargo-build
      cmd: ["cargo", "build", "--release", "--jobs=4"]
      timeout_ms: 10000
      parallel: true

    - name: cargo-test
      cmd: ["cargo", "test", "--jobs=4"]
      timeout_ms: 10000
      parallel: true
      depends: [cargo-build]

    - name: cargo-clippy
      cmd: ["cargo", "clippy", "--", "-D", "warnings"]
      timeout_ms: 5000
      parallel: true
      depends: [cargo-build]

    - name: cargo-fmt-check
      cmd: ["cargo", "fmt", "--check"]
      timeout_ms: 2000
      parallel: true

  validation:
    - name: package-check
      cmd: ["cargo", "package", "--no-verify", "--allow-dirty"]
      timeout_ms: 5000

    - name: doc-check
      cmd: ["cargo", "doc", "--no-deps"]
      timeout_ms: 5000

    - name: dry-run-publish
      cmd: ["cargo", "publish", "--dry-run", "--allow-dirty"]
      timeout_ms: 8000
---
// Your Axum service code here
use axum::{routing::get, Router};

#[tokio::main]
async fn main() {
    let app = Router::new().route("/", get(|| async { "Hello, World!" }));

    let listener = tokio::net::TcpListener::bind("0.0.0.0:{{port}}")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}
```

### 2.2 Optimize Template Rendering

```rust
// ggen-core/src/optimizations.rs
use std::collections::HashMap;
use tera::Tera;

/// Template cache for faster rendering
pub struct TemplateCache {
    compiled_templates: HashMap<String, Tera>,
}

impl TemplateCache {
    pub fn new() -> Self {
        Self {
            compiled_templates: HashMap::new(),
        }
    }

    pub fn get_or_compile(&mut self, template_path: &str) -> Result<&Tera> {
        if !self.compiled_templates.contains_key(template_path) {
            let mut tera = Tera::default();
            tera.add_template_file(template_path, None)?;
            self.compiled_templates.insert(template_path.to_string(), tera);
        }

        Ok(self.compiled_templates.get(template_path).unwrap())
    }
}

/// Enable parallel RDF graph loading
pub async fn load_rdf_graphs_parallel(paths: Vec<PathBuf>) -> Result<Vec<Graph>> {
    use futures::stream::{self, StreamExt};

    stream::iter(paths)
        .map(|path| async move {
            tokio::task::spawn_blocking(move || load_rdf_graph(&path)).await?
        })
        .buffer_unordered(4) // Load up to 4 graphs in parallel
        .collect::<Vec<_>>()
        .await
        .into_iter()
        .collect()
}
```

---

## Phase 3: Cleanroom Configuration (10 min)

### 3.1 Create Fast Config Profile

```toml
# .cleanroom/ultra-fast.toml
[cleanroom]
enable_singleton_containers = true
container_startup_timeout = "10s"
test_execution_timeout = "20s"
enable_deterministic_execution = true
deterministic_seed = 42

[performance_monitoring]
enable_monitoring = true
metrics_interval = "1s"
enable_profiling = false
enable_memory_tracking = false

[performance_monitoring.thresholds]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 1073741824  # 1GB
max_test_execution_time = "20s"
max_container_startup_time = "10s"

[resource_limits]
[resource_limits.cpu]
max_usage_percent = 80.0
throttle_threshold_percent = 90.0

[resource_limits.memory]
max_usage_bytes = 1073741824  # 1GB
oom_kill_threshold_bytes = 1258291200  # 1.2GB

[security_policy]
security_level = "Locked"
enable_network_isolation = true
enable_filesystem_isolation = true
allowed_ports = [8080, 8443]
enable_data_redaction = true
enable_audit_logging = false  # Disabled for speed
enable_process_isolation = true
```

### 3.2 Set Up Container Pool

```rust
// cleanroom/src/pool.rs
use std::collections::VecDeque;
use std::sync::Arc;
use tokio::sync::Mutex;

pub struct ContainerPool {
    available: Arc<Mutex<VecDeque<Container>>>,
    max_size: usize,
    image: String,
}

impl ContainerPool {
    pub fn new(image: String, max_size: usize) -> Self {
        Self {
            available: Arc::new(Mutex::new(VecDeque::new())),
            max_size,
            image,
        }
    }

    /// Get container instantly or create if needed
    pub async fn acquire(&self) -> Result<Container> {
        let mut available = self.available.lock().await;

        if let Some(container) = available.pop_front() {
            // Instant retrieval (<1ms)
            return Ok(container);
        }

        // Create new container only if pool is empty
        drop(available);
        self.create_new_container().await
    }

    /// Return container to pool for reuse
    pub async fn release(&self, container: Container) -> Result<()> {
        let mut available = self.available.lock().await;

        if available.len() < self.max_size {
            // Clean container for reuse
            container.reset().await?;
            available.push_back(container);
        } else {
            // Pool full, destroy container
            container.destroy().await?;
        }

        Ok(())
    }

    async fn create_new_container(&self) -> Result<Container> {
        Container::create(&self.image).await
    }

    /// Pre-warm pool with containers
    pub async fn warmup(&self, count: usize) -> Result<()> {
        let mut available = self.available.lock().await;

        for _ in 0..count {
            let container = self.create_new_container().await?;
            available.push_back(container);
        }

        Ok(())
    }
}
```

---

## Phase 4: Workflow Integration (15 min)

### 4.1 Create Ultra-Fast Command

```rust
// cli/src/cmds/ultra_fast.rs
use clap::Parser;
use ggen_core::Generator;
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

#[derive(Parser)]
#[command(about = "Ultra-fast concept to deploy workflow (<60s)")]
pub struct UltraFastCommand {
    /// Template to use
    #[arg(long)]
    template: String,

    /// Crate name
    #[arg(long)]
    name: String,

    /// Crate version
    #[arg(long, default_value = "0.1.0")]
    version: String,

    /// Output directory
    #[arg(long, default_value = "target/generated")]
    output: PathBuf,

    /// Timeout in seconds
    #[arg(long, default_value = "60")]
    timeout: u64,

    /// Report output path
    #[arg(long, default_value = "report.json")]
    report: PathBuf,
}

impl UltraFastCommand {
    pub async fn run(self) -> Result<()> {
        let start = std::time::Instant::now();

        println!("🚀 Ultra-Fast Workflow Started");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

        // Stage 1: Template selection (target: <5s)
        let stage1_start = std::time::Instant::now();
        println!("📦 Stage 1: Template selection...");

        let template = self.load_template_cached(&self.template).await?;

        println!("   ✅ Template loaded in {}ms", stage1_start.elapsed().as_millis());

        // Stage 2: Code generation (target: <10s)
        let stage2_start = std::time::Instant::now();
        println!("⚙️  Stage 2: Code generation...");

        let generator = Generator::new();
        let generated = generator
            .generate_with_vars(&template, &[
                ("name", &self.name),
                ("version", &self.version),
            ])
            .await?;

        self.write_files(&generated, &self.output).await?;

        println!("   ✅ Code generated in {}ms", stage2_start.elapsed().as_millis());

        // Stage 3: Cleanroom setup (target: <10s)
        let stage3_start = std::time::Instant::now();
        println!("🧪 Stage 3: Cleanroom setup...");

        let config = CleanroomConfig::from_file(".cleanroom/ultra-fast.toml")?;
        let cleanroom = CleanroomEnvironment::new(config).await?;

        println!("   ✅ Cleanroom ready in {}ms", stage3_start.elapsed().as_millis());

        // Stage 4: Testing (target: <20s)
        let stage4_start = std::time::Instant::now();
        println!("🧪 Stage 4: Testing...");

        let test_results = cleanroom
            .run_scenario_from_template(&template)
            .await?;

        println!("   ✅ Tests completed in {}ms", stage4_start.elapsed().as_millis());

        // Stage 5: Validation (target: <10s)
        let stage5_start = std::time::Instant::now();
        println!("✅ Stage 5: Validation...");

        let validation_results = cleanroom
            .run_validation_scenario(&template)
            .await?;

        println!("   ✅ Validation completed in {}ms", stage5_start.elapsed().as_millis());

        // Stage 6: Reporting (target: <5s)
        let stage6_start = std::time::Instant::now();
        println!("📊 Stage 6: Generating report...");

        let report = self.generate_report(
            &generated,
            &test_results,
            &validation_results,
            start.elapsed(),
        )?;

        self.write_report(&report, &self.report).await?;

        println!("   ✅ Report generated in {}ms", stage6_start.elapsed().as_millis());

        // Summary
        let total_time = start.elapsed();
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("🎉 Ultra-Fast Workflow Completed!");
        println!("   Total time: {}ms", total_time.as_millis());

        if total_time.as_secs() < 60 {
            println!("   ✅ Target achieved: <60 seconds!");
        } else {
            println!("   ⚠️  Target exceeded: {}s", total_time.as_secs());
        }

        if report.deployment_ready {
            println!("   🚀 Crate is production-ready!");
        } else {
            println!("   ❌ Crate not ready for deployment");
            return Err(anyhow::anyhow!("Deployment validation failed"));
        }

        Ok(())
    }

    async fn load_template_cached(&self, template: &str) -> Result<Template> {
        // Use cached template if available
        // Implementation details...
        todo!()
    }

    async fn write_files(&self, generated: &GeneratedCode, output: &Path) -> Result<()> {
        // Write generated files with streaming I/O
        // Implementation details...
        todo!()
    }

    async fn write_report(&self, report: &Report, path: &Path) -> Result<()> {
        // Write report to file
        // Implementation details...
        todo!()
    }

    fn generate_report(
        &self,
        generated: &GeneratedCode,
        test_results: &TestResults,
        validation_results: &ValidationResults,
        total_duration: Duration,
    ) -> Result<Report> {
        // Generate comprehensive report
        // Implementation details...
        todo!()
    }
}
```

### 4.2 Add to CLI

```rust
// cli/src/main.rs
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "ggen")]
#[command(about = "Graph-aware code generation framework")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Ultra-fast concept to deploy workflow
    UltraFast(cmds::ultra_fast::UltraFastCommand),
    // ... other commands
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::UltraFast(cmd) => cmd.run().await,
        // ... other commands
    }
}
```

---

## Phase 5: Testing and Validation (10 min)

### 5.1 Test Ultra-Fast Workflow

```bash
# Test with a simple template
ggen ultra-fast \
  --template io.ggen.rust.cli \
  --name test_cli \
  --timeout 60 \
  --report test_report.json

# Expected output:
# 🚀 Ultra-Fast Workflow Started
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# 📦 Stage 1: Template selection...
#    ✅ Template loaded in 3500ms
# ⚙️  Stage 2: Code generation...
#    ✅ Code generated in 9200ms
# 🧪 Stage 3: Cleanroom setup...
#    ✅ Cleanroom ready in 8800ms
# 🧪 Stage 4: Testing...
#    ✅ Tests completed in 18500ms
# ✅ Stage 5: Validation...
#    ✅ Validation completed in 9000ms
# 📊 Stage 6: Generating report...
#    ✅ Report generated in 3500ms
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# 🎉 Ultra-Fast Workflow Completed!
#    Total time: 52500ms
#    ✅ Target achieved: <60 seconds!
#    🚀 Crate is production-ready!
```

### 5.2 Verify Report

```bash
# Check report contents
cat test_report.json | jq '.deployment_status'
# Expected: "READY"

# Check stage timings
cat test_report.json | jq '.stages'
# Expected:
# {
#   "concept": { "duration_ms": 3500, "status": "success" },
#   "generation": { "duration_ms": 9200, "status": "success" },
#   "setup": { "duration_ms": 8800, "status": "success" },
#   "testing": { "duration_ms": 18500, "status": "success" },
#   "validation": { "duration_ms": 9000, "status": "success" },
#   "reporting": { "duration_ms": 3500, "status": "success" }
# }
```

---

## 🎯 Performance Tuning Tips

### Tip 1: Monitor Stage Times

```bash
# Add timing to each stage
export GGEN_DEBUG=1
export CLEANROOM_DEBUG=1

ggen ultra-fast --template io.ggen.rust.cli --name test

# Identify slowest stage
cat report.json | jq '.stages | to_entries | sort_by(.value.duration_ms) | reverse'
```

### Tip 2: Pre-Warm Container Pool

```bash
# Start container pool before workflow
cleanroom pool warmup --count 3 --image ggen/rust-fast:latest

# Containers are now instantly available
ggen ultra-fast --template io.ggen.rust.cli --name test
```

### Tip 3: Use Cached Dependencies

```bash
# Pre-compile common dependencies
mkdir -p ~/.cargo/cache/ggen
cd ~/.cargo/cache/ggen

cargo new --lib dummy
cd dummy
cargo add serde tokio axum anyhow clap
cargo build --release
cd ..

# Set CARGO_HOME to use cache
export CARGO_HOME=~/.cargo/cache/ggen
```

---

## ✅ Success Criteria

After implementing all phases, verify:

- [ ] Container images build and run successfully
- [ ] Templates include cleanroom metadata
- [ ] Cleanroom config loads without errors
- [ ] Ultra-fast command executes without panics
- [ ] Total workflow time <60 seconds
- [ ] All quality gates pass (tests, clippy, fmt)
- [ ] Report generates with all metrics
- [ ] Deployment status is "READY"

---

## 🚀 Next Steps

1. **Optimize further**: Identify bottlenecks with profiling
2. **Add more templates**: Create ultra-fast templates for common use cases
3. **CI/CD integration**: Add ultra-fast workflow to GitHub Actions
4. **Monitoring**: Set up alerting for workflows that exceed 60s
5. **Documentation**: Write usage guides for your team

---

**Congratulations!** You've implemented the ultra-fast workflow. Start deploying production-ready code in under 60 seconds!

```bash
# Try it now
ggen ultra-fast --template io.ggen.rust.axum-service --name my_service
```
