# Ultra-Fast Templates - Quick Start Guide

Get started with ultra-fast code generation in under 60 seconds!

## Prerequisites

- Rust toolchain installed (`rustc` and `cargo`)
- Ggen CLI installed (`cargo install ggen`)

## 1-Minute Quick Start

### Generate Your First Project

```bash
# CLI Tool (15 seconds total)
ggen template generate /Users/sac/ggen/templates/ultra/rust-cli-minimal.tmpl --var project_name=myapp
cd myapp
cargo test && cargo build --release
./target/release/myapp help
```

**Output:**
```
Usage: myapp <command> [args]

Commands:
  run [args]    Run the main command
  version       Show version information
  help          Show this help message
```

### Generate a Library

```bash
# Library (12 seconds total)
ggen template generate /Users/sac/ggen/templates/ultra/rust-lib-minimal.tmpl --var project_name=mylib
cd mylib
cargo test && cargo clippy
```

**Output:**
```
test tests::test_process_valid_input ... ok
test tests::test_process_empty_input ... ok
test tests::test_transform_valid_data ... ok
test tests::test_transform_empty_data ... ok
test tests::test_validate_valid_input ... ok
test tests::test_validate_short_input ... ok

test result: ok. 6 passed; 0 failed
```

### Generate a Web Service

```bash
# Web Service (20 seconds total)
ggen template generate /Users/sac/ggen/templates/ultra/rust-web-minimal.tmpl \
  --var project_name=api \
  --var port=8080
cd api
cargo test && cargo run &
curl http://localhost:8080/health
```

**Output:**
```json
{"status":"healthy","uptime":0}
```

## Template Locations

All templates are located in `/Users/sac/ggen/templates/ultra/`:

- `rust-cli-minimal.tmpl` - CLI applications
- `rust-lib-minimal.tmpl` - Libraries
- `rust-web-minimal.tmpl` - Web services

## Next Steps

1. **Customize**: Modify generated code to fit your needs
2. **Test**: Run comprehensive tests with `cargo test --all-features`
3. **Deploy**: Build release binaries with `cargo build --release`
4. **Learn More**: Check `README.md` and `EXAMPLES.md`

## Common Commands

```bash
# Generate with custom variables
ggen template generate rust-web-minimal.tmpl \
  --var project_name=custom-api \
  --var port=9000 \
  --var determinism=42

# Test generated code
cd custom-api
cargo test --verbose

# Build optimized binary
cargo build --release --target x86_64-unknown-linux-musl

# Check code quality
cargo clippy -- -D warnings
cargo fmt --check
```

## Troubleshooting

### Template not found
```bash
# Use absolute path
ggen template generate /Users/sac/ggen/templates/ultra/rust-cli-minimal.tmpl \
  --var project_name=myapp
```

### Tests fail
```bash
# Clean and rebuild
cd myapp
cargo clean
cargo test --verbose
```

### Slow build
```bash
# Use release mode with optimization
cargo build --release
```

## Support

- **Documentation**: See `README.md` for detailed information
- **Examples**: See `EXAMPLES.md` for real-world usage
- **Issues**: Report problems in the main ggen repository

---

**Ready to build? Run your first template now!**
