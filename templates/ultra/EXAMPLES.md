# Ultra-Fast Template Examples

Real-world usage examples for the ultra-fast code generation templates.

## Quick Start Examples

### Example 1: CLI Tool in 15 Seconds

Generate a production-ready CLI tool in under 15 seconds:

```bash
# Step 1: Generate (5s)
ggen template generate rust-cli-minimal.tmpl --var project_name=myapp

# Step 2: Test (3s)
cd myapp && cargo test

# Step 3: Build (5s)
cargo build --release

# Step 4: Run
./target/release/myapp help
```

**Output:**
```
test tests::test_version_output ... ok
test tests::test_run_command_empty ... ok
test tests::test_run_command_with_args ... ok

test result: ok. 3 passed; 0 failed
```

### Example 2: Library in 12 Seconds

Create a reusable library with error handling:

```bash
# Step 1: Generate (5s)
ggen template generate rust-lib-minimal.tmpl --var project_name=mylib

# Step 2: Test (3s)
cd mylib && cargo test

# Step 3: Validate (4s)
cargo clippy -- -D warnings
```

**Usage in another project:**
```rust
// Add to Cargo.toml:
// mylib = { path = "../mylib" }

use mylib::{process, validate};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    validate("hello")?;
    let result = process("world")?;
    println!("{}", result);
    Ok(())
}
```

### Example 3: Web Service in 20 Seconds

Deploy a web service with health checks:

```bash
# Step 1: Generate (5s)
ggen template generate rust-web-minimal.tmpl \
  --var project_name=api \
  --var port=8080

# Step 2: Test (3s)
cd api && cargo test

# Step 3: Run (1s startup)
cargo run &

# Step 4: Test endpoints (1s)
curl http://localhost:8080/health
```

**Output:**
```json
{"status":"healthy","uptime":0}
```

## Workflow Examples

### Workflow 1: Microservice Development

Create multiple services in parallel:

```bash
# Generate services
ggen template generate rust-web-minimal.tmpl --var project_name=user-service --var port=8001
ggen template generate rust-web-minimal.tmpl --var project_name=auth-service --var port=8002
ggen template generate rust-web-minimal.tmpl --var project_name=api-gateway --var port=8000

# Start all services
cd user-service && cargo run &
cd auth-service && cargo run &
cd api-gateway && cargo run &

# Test the stack
curl http://localhost:8000/health
curl http://localhost:8001/health
curl http://localhost:8002/health
```

### Workflow 2: Library + CLI Application

Create a library and a CLI that uses it:

```bash
# Step 1: Generate library
ggen template generate rust-lib-minimal.tmpl --var project_name=core

# Step 2: Generate CLI
ggen template generate rust-cli-minimal.tmpl --var project_name=cli

# Step 3: Link them
cd cli
# Add to Cargo.toml:
# [dependencies]
# core = { path = "../core" }

# Step 4: Use the library in CLI
# Edit src/main.rs to use core::process()
```

### Workflow 3: Rapid Prototyping

Test an idea in under 30 seconds:

```bash
# Generate prototype
ggen template generate rust-cli-minimal.tmpl --var project_name=prototype

# Add your code
cd prototype
cat >> src/main.rs << 'EOF'

// Your prototype code here
fn prototype_feature() {
    println!("Testing new idea...");
}
EOF

# Test immediately
cargo test && cargo run -- run
```

## Cleanroom Integration Examples

### Example 4: Validated Deployment

Generate and validate for production:

```bash
# Step 1: Generate
ggen template generate rust-web-minimal.tmpl --var project_name=prod-api

# Step 2: Cleanroom validation
cd prod-api
cargo test --all-features
cargo clippy -- -D warnings
cargo fmt --check
cargo audit

# Step 3: Build for production
cargo build --release --target x86_64-unknown-linux-musl

# Step 4: Deploy
docker build -t prod-api .
```

### Example 5: Continuous Integration

Use templates in CI/CD pipeline:

```yaml
# .github/workflows/template-test.yml
name: Template Validation

on: [push]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Generate from template
        run: ggen template generate rust-lib-minimal.tmpl --var project_name=ci-test

      - name: Test generated code
        run: |
          cd ci-test
          cargo test
          cargo clippy -- -D warnings

      - name: Benchmark performance
        run: |
          cd ci-test
          cargo bench
```

## Performance Benchmarks

### Benchmark 1: Generation Speed

Measure template generation performance:

```bash
#!/bin/bash
# benchmark.sh

echo "Template Generation Benchmarks"
echo "=============================="

for template in rust-cli-minimal rust-lib-minimal rust-web-minimal; do
    echo ""
    echo "Testing: $template"

    start=$(date +%s%N)
    ggen template generate "$template.tmpl" --var project_name="bench-$template"
    end=$(date +%s%N)

    duration=$(( (end - start) / 1000000 ))
    echo "Generation time: ${duration}ms"

    cd "bench-$template"

    start=$(date +%s%N)
    cargo test 2>&1 >/dev/null
    end=$(date +%s%N)
    test_duration=$(( (end - start) / 1000000 ))
    echo "Test time: ${test_duration}ms"

    cd ..
done
```

**Expected Results:**
```
Template Generation Benchmarks
==============================

Testing: rust-cli-minimal
Generation time: 250ms
Test time: 2800ms

Testing: rust-lib-minimal
Generation time: 280ms
Test time: 2500ms

Testing: rust-web-minimal
Generation time: 450ms
Test time: 3200ms
```

### Benchmark 2: Binary Size Optimization

Compare binary sizes:

```bash
# Generate and build all templates
for template in rust-cli-minimal rust-lib-minimal rust-web-minimal; do
    ggen template generate "$template.tmpl" --var project_name="$template"
    cd "$template"
    cargo build --release
    ls -lh target/release/"$template" 2>/dev/null || ls -lh target/release/lib*.rlib
    cd ..
done
```

**Expected Sizes:**
- CLI: ~400KB
- Library: ~200KB (rlib)
- Web: ~500KB

## Advanced Examples

### Example 6: Custom Template Variables

Use template variables for customization:

```bash
# Generate with custom configuration
ggen template generate rust-web-minimal.tmpl \
  --var project_name=custom-api \
  --var port=9000 \
  --var determinism=12345

# The generated code will use port 9000
cd custom-api
cargo run
# Server listening on http://127.0.0.1:9000
```

### Example 7: Batch Generation

Generate multiple projects from a config file:

```bash
# projects.txt
cli-tool-1
cli-tool-2
lib-core
lib-utils
web-api

# Generate all
while read name; do
    if [[ $name == lib-* ]]; then
        template="rust-lib-minimal.tmpl"
    elif [[ $name == web-* ]]; then
        template="rust-web-minimal.tmpl"
    else
        template="rust-cli-minimal.tmpl"
    fi

    echo "Generating $name from $template"
    ggen template generate "$template" --var project_name="$name"
done < projects.txt
```

### Example 8: Template Testing Suite

Create a test suite for templates:

```bash
#!/bin/bash
# test-suite.sh

TEMPLATES=(
    "rust-cli-minimal.tmpl:cli"
    "rust-lib-minimal.tmpl:lib"
    "rust-web-minimal.tmpl:web"
)

RESULTS=()

for entry in "${TEMPLATES[@]}"; do
    IFS=: read -r template name <<< "$entry"

    echo "Testing $template..."

    # Generate
    ggen template generate "$template" --var project_name="test-$name"

    # Test
    cd "test-$name"
    if cargo test 2>&1 >/dev/null; then
        RESULTS+=("✅ $template: PASS")
    else
        RESULTS+=("❌ $template: FAIL")
    fi
    cd ..
done

echo ""
echo "Test Results:"
for result in "${RESULTS[@]}"; do
    echo "  $result"
done
```

## Troubleshooting Examples

### Issue 1: Template Generation Fails

```bash
# Validate template syntax
ggen template validate rust-cli-minimal.tmpl

# Check variables
ggen template info rust-cli-minimal.tmpl

# Generate with verbose output
ggen template generate rust-cli-minimal.tmpl \
  --var project_name=debug \
  --verbose
```

### Issue 2: Tests Fail After Generation

```bash
# Check Rust toolchain
rustc --version
cargo --version

# Clean and rebuild
cd my-project
cargo clean
cargo test --verbose

# Check for conflicts
cargo tree
```

### Issue 3: Slow Performance

```bash
# Profile template generation
time ggen template generate rust-cli-minimal.tmpl --var project_name=perf

# Profile compilation
cd perf
cargo build --release --timings

# Check for unnecessary dependencies
cargo tree | grep -v "(indirect)"
```

## Integration Examples

### Integration 1: With Docker

```dockerfile
# Dockerfile
FROM rust:1.75-alpine AS builder

# Generate from template
RUN cargo install ggen
RUN ggen template generate rust-web-minimal.tmpl --var project_name=api

# Build
WORKDIR /api
RUN cargo build --release

# Runtime
FROM alpine:latest
COPY --from=builder /api/target/release/api /usr/local/bin/
CMD ["api"]
```

### Integration 2: With GitHub Actions

```yaml
# .github/workflows/deploy.yml
name: Deploy from Template

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Generate application
        run: |
          cargo install ggen
          ggen template generate rust-web-minimal.tmpl --var project_name=app

      - name: Test
        run: cd app && cargo test

      - name: Build
        run: cd app && cargo build --release

      - name: Deploy
        run: |
          # Deploy to production
          scp app/target/release/app user@server:/opt/app/
```

### Integration 3: With Kubernetes

```yaml
# k8s-deploy.yml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: template-generated-api
spec:
  replicas: 3
  selector:
    matchLabels:
      app: api
  template:
    metadata:
      labels:
        app: api
    spec:
      containers:
      - name: api
        image: myregistry/api:latest
        ports:
        - containerPort: 3000
        livenessProbe:
          httpGet:
            path: /health
            port: 3000
          initialDelaySeconds: 5
          periodSeconds: 10
```

## Best Practices from Real Usage

1. **Always Test Immediately**: Run `cargo test` right after generation
2. **Use Deterministic Seeds**: Set `--var determinism=42` for reproducibility
3. **Version Control Early**: Commit generated code before modifications
4. **Profile Before Optimizing**: Use `cargo build --timings` to find bottlenecks
5. **Validate for Production**: Run cleanroom validation before deployment
6. **Document Customizations**: Keep a log of changes made to generated code
7. **Batch Operations**: Generate multiple projects in parallel when possible

## Summary

These templates enable:
- **Sub-15s** CLI tool creation
- **Sub-12s** library creation
- **Sub-20s** web service deployment
- **Zero dependencies** for maximum speed
- **Production-ready** code out of the box
- **Cleanroom validated** for quality assurance

Start with the simplest template that meets your needs, then customize as required.
