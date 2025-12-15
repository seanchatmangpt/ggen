# Fixes Applied to GGEN Marketplace Test Files

## Date: 2025-11-02

## Summary

All GGEN marketplace test files have been fixed to work with the updated clnrm parser.

## Changes Made

### 1. Removed `type` Field Requirement
- **Before**: `type = "generic_container"` was required
- **After**: `plugin` defaults to `"generic_container"` automatically
- **Files affected**: All `.clnrm.toml` files

### 2. Fixed OTEL Exporter Type
- **Before**: `exporter = "otlp-http"` (invalid)
- **After**: `exporter = "otlp"` (valid)
- **Files affected**: All test files with `[otel]` section

### 3. Fixed Volumes Syntax
- **Before**: `volumes = ["./:/app"]` (invalid string format)
- **After**: `volumes = [{ host_path = "./", container_path = "/app" }]` (proper VolumeConfig)
- **Files affected**: Files using volume mounts

### 4. Fixed Environment Variable Syntax
- **Before**: `environment = { ... }` (invalid key)
- **After**: `env = { ... }` (correct ServiceConfig field)
- **Files affected**: Files with environment variables

### 5. Fixed Inline Tables
- **Before**: Multi-line inline tables with newlines
- **After**: Single-line inline tables
- **Files affected**: `error_handling.clnrm.toml` and others with `attrs.all = { ... }`

## Files Status

- ✅ `search_test.clnrm.toml` - Valid
- ✅ `p2p_test.clnrm.toml` - Valid
- ✅ `search.clnrm.toml` - Fixed and validated
- ✅ `p2p.clnrm.toml` - Fixed and validated
- ✅ `install.clnrm.toml` - Fixed and validated
- ✅ `error_handling.clnrm.toml` - Fixed and validated
- ✅ `otel_validation.clnrm.toml` - Fixed and validated

## Valid Configuration Format

```toml
[services.my_service]
# plugin defaults to "generic_container" - no need to specify
image = "alpine:latest"

# Optional: explicit plugin
plugin = "generic_container"

# Volumes (proper syntax)
volumes = [{ host_path = "./", container_path = "/app" }]

# Environment variables
env = {
  RUST_LOG = "info",
  OTEL_EXPORTER_OTLP_ENDPOINT = "http://localhost:4318"
}

[otel]
exporter = "otlp"  # Valid exporter types: stdout, otlp, jaeger, zipkin
resources = { "service.name" = "my-service" }
```

## Next Steps

1. All test files are now valid and ready to use
2. Run tests: `clnrm run search_test.clnrm.toml p2p_test.clnrm.toml`
3. Update Docker images when ggen services are built
4. Add more comprehensive test scenarios

