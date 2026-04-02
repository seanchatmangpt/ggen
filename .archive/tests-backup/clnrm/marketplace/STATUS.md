# GGEN Marketplace Test Files - Status

## Date: 2025-11-02

## ✅ Working Test Files

These files validate successfully and are ready to use:

1. **search_test.clnrm.toml** ✅
   - Simple marketplace search test
   - Uses `[[steps]]` format
   - Validates OTEL spans

2. **p2p_test.clnrm.toml** ✅
   - P2P peer discovery test
   - Uses `[[steps]]` format
   - Multi-service configuration

## ⚠️ Advanced Test Files (Scenario Format)

These files use the `[[scenario]]` format which requires additional setup:

1. **search.clnrm.toml** - Comprehensive search tests with scenarios
2. **p2p.clnrm.toml** - Comprehensive P2P tests with scenarios
3. **install.clnrm.toml** - Installation tests with scenarios
4. **error_handling.clnrm.toml** - Error handling tests with scenarios
5. **otel_validation.clnrm.toml** - OTEL validation with scenarios

**Note**: These files may require:
- Running actual ggen services in containers
- Setting up proper test infrastructure
- More complex validation logic

## Quick Start

```bash
# Validate the working test files
cd /Users/sac/ggen/tests/clnrm/marketplace
clnrm validate search_test.clnrm.toml p2p_test.clnrm.toml

# Run the tests
clnrm run search_test.clnrm.toml p2p_test.clnrm.toml
```

## Configuration Format

All files now use the correct format:

```toml
[services.my_service]
# plugin defaults to "generic_container"
image = "alpine:latest"

[otel]
exporter = "otlp"  # Fixed: was "otlp-http"
resources = { "service.name" = "my-service" }

[[steps]]
name = "test_operation"
service = "my_service"
command = ["echo", "test"]
expected_output_regex = "test"
```

## Next Steps

1. ✅ Parser fixes complete - no more `type` field requirement
2. ✅ Basic test files working
3. ⏭️ Run tests against actual services
4. ⏭️ Expand scenario-based tests

