# GGEN Marketplace & P2P - CLNRM Test Configuration

These tests validate GGEN marketplace and P2P functionality with OpenTelemetry instrumentation.

## Test Files

- `search_test.clnrm.toml` - Tests marketplace search operations
- `p2p_test.clnrm.toml` - Tests P2P peer discovery operations

## Configuration Format

**Note**: The `plugin` field defaults to `"generic_container"`, so it doesn't need to be specified. Only specify `image` for generic containers.

```toml
[services.service_name]
image = "alpine:latest"
```

The `type` field is not required - `plugin` defaults to `generic_container`.

## Running Tests

```bash
# Validate test files
clnrm validate search_test.clnrm.toml p2p_test.clnrm.toml

# Run tests
clnrm run search_test.clnrm.toml p2p_test.clnrm.toml
```
