# Ultra-Fast Code Generation Templates

Speed-optimized templates for the <60s deployment workflow. These templates generate valid, testable Rust code in under 10 seconds.

## Templates

### 1. rust-cli-minimal.tmpl (Target: <5s)
Minimal CLI application with:
- Command-line argument parsing
- Multiple commands (run, version, help)
- Built-in tests
- Zero external dependencies
- ~50 lines of code

**Usage:**
```bash
ggen template generate rust-cli-minimal.tmpl --var project_name=my-cli
cd my-cli
cargo test    # <3s
cargo build   # <5s
```

### 2. rust-lib-minimal.tmpl (Target: <5s)
Minimal library with:
- Error handling (custom Error type)
- Public API (process, transform, validate)
- Comprehensive tests
- Zero external dependencies
- ~80 lines of code

**Usage:**
```bash
ggen template generate rust-lib-minimal.tmpl --var project_name=my-lib
cd my-lib
cargo test    # <3s
cargo build   # <5s
```

### 3. rust-web-minimal.tmpl (Target: <10s)
Minimal web service with:
- HTTP server (std::net only)
- JSON API endpoints
- Health checks
- Multi-threaded
- Zero external dependencies
- ~120 lines of code

**Usage:**
```bash
ggen template generate rust-web-minimal.tmpl --var project_name=my-web --var port=8080
cd my-web
cargo test    # <3s
cargo run     # <1s startup
```

## Performance Characteristics

| Template | Lines | Dependencies | Test Time | Build Time | Binary Size |
|----------|-------|--------------|-----------|------------|-------------|
| CLI      | ~50   | 0            | <3s       | <5s        | ~400KB      |
| Lib      | ~80   | 0            | <3s       | <5s        | ~200KB      |
| Web      | ~120  | 0            | <3s       | <8s        | ~500KB      |

## Template Variables

### Common Variables
- `project_name`: Name of the generated project (required)
- `determinism`: Random seed for reproducible generation (default: 42)

### Web-Specific Variables
- `port`: HTTP server port (default: 3000)

## Cleanroom Validation

All templates are designed to pass cleanroom validation:

```bash
# Generate project
ggen template generate rust-cli-minimal.tmpl --var project_name=test-cli

# Validate with cleanroom
cd test-cli
cargo test --all-features
cargo clippy -- -D warnings
cargo fmt --check
```

## Speed Optimizations

1. **Zero Dependencies**: No external crates = instant `cargo check`
2. **Minimal Code**: <150 lines per template = fast compilation
3. **Pre-configured**: Cargo.toml included = no setup time
4. **Deterministic**: Fixed seeds = reproducible builds
5. **Tested**: Built-in tests = immediate validation

## Workflow Integration

### 60-Second Deployment
```bash
# Step 1: Generate (5s)
ggen template generate rust-cli-minimal.tmpl --var project_name=fast-cli

# Step 2: Test (3s)
cd fast-cli && cargo test

# Step 3: Build (5s)
cargo build --release

# Step 4: Validate (2s)
cargo clippy

# Total: ~15s
```

### With Cleanroom
```bash
# Step 1: Generate (5s)
ggen template generate rust-lib-minimal.tmpl --var project_name=fast-lib

# Step 2: Test (3s)
cd fast-lib && cargo test

# Step 3: Cleanroom validate (10s)
cargo test --all-features
cargo clippy -- -D warnings

# Total: ~18s
```

## Template Structure

Each template uses YAML frontmatter for metadata:

```yaml
---
to: "{{project_name}}/src/main.rs"
vars:
  project_name: "example"
  determinism: 42
---
// Generated code here
```

Multiple files per template:
```yaml
---
to: "{{project_name}}/src/main.rs"
---
// Main code
---
to: "{{project_name}}/Cargo.toml"
---
# Cargo config
---
to: "{{project_name}}/README.md"
---
# Documentation
```

## Best Practices

1. **Deterministic Generation**: Always set `determinism` variable
2. **Fast Testing**: Run `cargo test` immediately after generation
3. **Validate Early**: Use cleanroom validation before deployment
4. **Minimal Changes**: Templates work out-of-the-box, avoid modifications
5. **Version Control**: Commit generated code immediately

## Advanced Usage

### Custom Variables
```bash
# CLI with custom name
ggen template generate rust-cli-minimal.tmpl \
  --var project_name=my-custom-cli \
  --var determinism=123

# Web with custom port
ggen template generate rust-web-minimal.tmpl \
  --var project_name=api-service \
  --var port=8080 \
  --var determinism=456
```

### Batch Generation
```bash
# Generate multiple projects
for name in cli-1 cli-2 cli-3; do
  ggen template generate rust-cli-minimal.tmpl --var project_name=$name
done
```

### Template Chaining
```bash
# Generate library + CLI that uses it
ggen template generate rust-lib-minimal.tmpl --var project_name=core-lib
ggen template generate rust-cli-minimal.tmpl --var project_name=cli-app
# Then manually link them in Cargo.toml
```

## Troubleshooting

### Generation Fails
- Check template syntax with `ggen template validate`
- Ensure all required variables are provided
- Verify write permissions in target directory

### Tests Fail
- Run `cargo test --verbose` for details
- Check determinism seed matches expected values
- Verify no external dependencies conflict

### Slow Build Times
- Ensure no extra dependencies added
- Use `cargo build --release` for optimized builds
- Check system resources (CPU, memory)

## Contributing

To add new ultra-fast templates:

1. Keep code under 150 lines
2. Use zero external dependencies
3. Include comprehensive tests
4. Document all variables
5. Verify <10s generation time
6. Test with cleanroom validation

## License

MIT License - See LICENSE file for details
