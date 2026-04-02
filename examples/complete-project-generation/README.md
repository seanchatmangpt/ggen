# Complete Project Generation Example

This example demonstrates how to use ggen to generate a complete, production-ready Rust web service from templates. The generated project includes multiple modules, tests, and a working API server.

## Overview

This example generates a complete microservice with:
- **REST API** with multiple endpoints
- **Data models** with validation
- **Configuration management** using environment variables
- **Error handling** with custom error types
- **Integration tests** that validate the entire stack
- **Logging and middleware**

### Generated Project Structure

```
generated-project/
├── Cargo.toml              # Package configuration
├── src/
│   ├── main.rs             # Application entry point
│   ├── models/
│   │   ├── mod.rs          # Models module
│   │   ├── user.rs         # User model
│   │   └── product.rs      # Product model
│   ├── api/
│   │   ├── mod.rs          # API module
│   │   ├── handlers.rs     # Request handlers
│   │   └── routes.rs       # Route configuration
│   └── config/
│       ├── mod.rs          # Config module
│       └── settings.rs     # Application settings
└── tests/
    └── integration_test.rs # Integration tests
```

## Step-by-Step Generation Process

### 1. Validate Templates

First, validate all templates to ensure they're correctly formatted:

```bash
./generate-project.sh validate
```

This checks:
- Template syntax
- Variable consistency
- Required fields
- File structure

### 2. Generate Project

Generate the complete project with default or custom variables:

```bash
# Generate with default settings
./generate-project.sh generate

# Generate with custom settings
./generate-project.sh generate \
  --project-name my-service \
  --port 8080 \
  --author "Your Name"
```

### 3. Build Generated Project

The script automatically builds the generated project:

```bash
cd output/generated-project
cargo build --release
```

### 4. Run Tests

Execute integration tests to verify everything works:

```bash
cargo test
```

### 5. Run the Service

Start the web service:

```bash
cargo run
```

The API will be available at `http://localhost:3000` (or your configured port).

## API Endpoints

The generated service includes these endpoints:

- `GET /health` - Health check endpoint
- `GET /api/users` - List all users
- `GET /api/users/:id` - Get user by ID
- `POST /api/users` - Create new user
- `GET /api/products` - List all products
- `GET /api/products/:id` - Get product by ID

## Customization

### Variables

Edit `project-spec.yaml` to customize:

```yaml
variables:
  project_name: my-awesome-service
  version: 0.1.0
  author: Your Name
  port: 8080
  log_level: info
  database_url: postgresql://localhost/mydb
```

### Adding New Modules

1. Create template in `templates/new-module.tmpl`
2. Add module to `project-spec.yaml`:
   ```yaml
   modules:
     - name: new_module
       template: new-module.tmpl
       output: src/new_module/mod.rs
   ```
3. Regenerate project

### Adding Dependencies

Edit `templates/cargo-toml.tmpl`:

```toml
[dependencies]
your-new-dep = "1.0"
```

## How It Works

### Template Processing

1. **Load Specification**: Read `project-spec.yaml`
2. **Validate Templates**: Check all `.tmpl` files
3. **Process Variables**: Replace `{{variable}}` placeholders
4. **Generate Files**: Create directory structure and files
5. **Post-Processing**: Format code, run tests

### Variable Substitution

Templates use `{{variable_name}}` syntax:

```rust
// In template:
const PORT: u16 = {{port}};

// After generation:
const PORT: u16 = 3000;
```

### Conditional Generation

Templates support conditionals:

```rust
{{#if enable_auth}}
use auth::middleware::AuthMiddleware;
{{/if}}
```

## Build and Run

### Prerequisites

- Rust 1.70+ (`rustc --version`)
- Cargo (`cargo --version`)
- ggen CLI tool

### Quick Start

```bash
# 1. Validate everything
./generate-project.sh validate

# 2. Generate project
./generate-project.sh generate

# 3. Build and test
cd output/generated-project
cargo build
cargo test

# 4. Run service
cargo run

# 5. Test API
curl http://localhost:3000/health
curl http://localhost:3000/api/users
```

### Development Workflow

```bash
# Watch for changes and rebuild
cargo watch -x run

# Run tests on save
cargo watch -x test

# Check without building
cargo check
```

## Testing

### Unit Tests

Each module includes unit tests:

```bash
cargo test --lib
```

### Integration Tests

Full end-to-end tests in `tests/`:

```bash
cargo test --test integration_test
```

### Coverage

Generate test coverage report:

```bash
cargo tarpaulin --out Html
```

## Production Deployment

### Build Release Binary

```bash
cargo build --release
./target/release/{{project_name}}
```

### Docker Container

Generated project includes Dockerfile:

```bash
docker build -t {{project_name}} .
docker run -p 3000:3000 {{project_name}}
```

### Environment Configuration

Set via environment variables:

```bash
export PORT=8080
export LOG_LEVEL=debug
export DATABASE_URL=postgresql://...
cargo run
```

## Troubleshooting

### Template Validation Errors

```bash
Error: Missing variable 'project_name' in template
```

**Solution**: Add variable to `project-spec.yaml`

### Build Failures

```bash
error: unresolved import `crate::models`
```

**Solution**: Check module paths in generated `mod.rs` files

### Test Failures

```bash
test result: FAILED. 0 passed; 1 failed
```

**Solution**: Review integration test expectations vs. actual output

## Advanced Usage

### Custom Template Functions

Add helper functions to templates:

```rust
// In template:
{{uppercase project_name}}

// Result:
MY-AWESOME-SERVICE
```

### Multi-Stage Generation

Generate in phases:

```bash
# Phase 1: Core modules only
./generate-project.sh generate --phase core

# Phase 2: Add API layer
./generate-project.sh generate --phase api

# Phase 3: Add tests
./generate-project.sh generate --phase tests
```

### Template Inheritance

Create base templates and extend them:

```yaml
templates:
  - name: base-handler
    file: base-handler.tmpl
  - name: user-handler
    extends: base-handler
    file: user-handler.tmpl
```

## Examples

### Generate Microservice

```bash
./generate-project.sh generate \
  --project-name user-service \
  --port 8001 \
  --modules user,auth
```

### Generate CLI Tool

```bash
./generate-project.sh generate \
  --template cli \
  --project-name my-cli \
  --no-api
```

### Generate Library

```bash
./generate-project.sh generate \
  --template lib \
  --project-name my-lib \
  --no-binary
```

## Performance

- **Template validation**: ~50ms for 10 templates
- **File generation**: ~200ms for complete project
- **Build time**: 30-60s for first build (with dependencies)
- **Test execution**: 1-3s for integration tests

## Best Practices

1. **Version Control**: Commit templates, not generated code
2. **Template Testing**: Validate templates in CI/CD
3. **Incremental Generation**: Generate only changed modules
4. **Documentation**: Keep templates well-documented
5. **Error Handling**: Templates should include robust error handling

## Resources

- [ggen Documentation](../../README.md)
- [Rust Web Development](https://www.rust-lang.org/what/wasm)
- [Actix Web Framework](https://actix.rs/)
- [Template Best Practices](../../docs/templates.md)

## License

This example is part of the ggen project and follows the same license.
