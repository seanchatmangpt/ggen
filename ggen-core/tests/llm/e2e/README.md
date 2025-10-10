# End-to-End LLM Testing Suite

## Overview

This directory contains comprehensive end-to-end tests that validate complete user workflows for the LLM module. These tests simulate real-world usage patterns and ensure the system works correctly from user request to final response.

## Test Categories

### 1. Chat Workflow Tests (`chat_workflow.rs`)

Tests complete request/response workflows:

- **Basic Chat**: Simple request/response cycle
- **Multi-turn Conversations**: Conversation history management
- **System Messages**: System prompt integration
- **Parameter Customization**: Temperature, max tokens, etc.
- **Empty Messages**: Error handling for invalid input
- **Long Conversations**: Context window management
- **Concurrent Requests**: Parallel request handling
- **Response Metadata**: Usage tracking and metadata

### 2. Streaming Workflow Tests (`streaming_workflow.rs`)

Tests streaming response workflows:

- **Basic Streaming**: Chunk-by-chunk response delivery
- **Chunk Processing**: Real-time content processing
- **Stream Cancellation**: Early termination handling
- **Error Handling**: Stream error recovery
- **Backpressure**: Slow consumer handling
- **Reconnection**: Stream interruption recovery
- **Progress Tracking**: Real-time progress monitoring

### 3. Configuration Workflow Tests (`configuration_workflow.rs`)

Tests configuration and provider management:

- **Provider Switching**: Multiple provider support
- **Configuration Updates**: Dynamic config changes
- **Model Switching**: Model selection and switching
- **Timeout Configuration**: Request timeout management
- **API Key Rotation**: Key management workflows
- **Environment Configuration**: Multi-environment support
- **Feature Flags**: Optional feature management
- **Configuration Validation**: Invalid config detection

### 4. Error Recovery Workflow Tests (`error_recovery_workflow.rs`)

Tests error handling and recovery:

- **Network Errors**: Retry logic and recovery
- **Authentication Errors**: Invalid credentials handling
- **Rate Limit Errors**: Rate limit detection and backoff
- **Timeout Errors**: Timeout recovery strategies
- **Partial Responses**: Interrupted stream handling
- **Fallback Providers**: Provider failover
- **Graceful Degradation**: Feature degradation on errors
- **Error Messages**: Clear error communication

### 5. Rate Limit Workflow Tests (`rate_limit_workflow.rs`)

Tests rate limiting and backoff:

- **Exponential Backoff**: Progressive retry delays
- **Rate Limit Headers**: Header-based rate limiting
- **Burst Requests**: High-volume request handling
- **Rate Limit Recovery**: Cooldown and recovery
- **Concurrent Limiting**: Multi-task rate limiting
- **Adaptive Rate Limiting**: Dynamic rate adjustment
- **Request Queuing**: Queue-based rate management
- **Priority Limiting**: Priority-based request handling

## Running Tests

### Run All E2E Tests

```bash
cargo test --test llm_e2e
```

### Run Specific Test Category

```bash
# Chat workflows
cargo test --test llm_e2e chat_workflow

# Streaming workflows
cargo test --test llm_e2e streaming_workflow

# Configuration workflows
cargo test --test llm_e2e configuration_workflow

# Error recovery workflows
cargo test --test llm_e2e error_recovery_workflow

# Rate limit workflows
cargo test --test llm_e2e rate_limit_workflow
```

### Run Specific Test

```bash
cargo test --test llm_e2e test_chat_request_response_workflow
```

### Run with Output

```bash
cargo test --test llm_e2e -- --nocapture
```

## Test Scenarios

### Scenario 1: User Chat Session

1. User sends initial message
2. Receives response with metadata
3. Continues multi-turn conversation
4. System maintains context
5. Response includes usage tracking

**Tests**: `chat_workflow::test_chat_request_response_workflow`, `chat_workflow::test_multi_turn_conversation_workflow`

### Scenario 2: Streaming Response

1. User requests streaming response
2. Chunks arrive progressively
3. User processes chunks in real-time
4. Stream completes successfully
5. Progress tracked throughout

**Tests**: `streaming_workflow::test_basic_streaming_workflow`, `streaming_workflow::test_streaming_progress_tracking_workflow`

### Scenario 3: Configuration Change

1. System starts with provider A
2. Configuration updated to provider B
3. Provider switch occurs seamlessly
4. Both providers tested
5. Settings validated

**Tests**: `configuration_workflow::test_provider_switching_workflow`, `configuration_workflow::test_configuration_update_workflow`

### Scenario 4: Error Recovery

1. Network error occurs
2. System retries with backoff
3. Retry succeeds after delay
4. User receives response
5. No data loss

**Tests**: `error_recovery_workflow::test_network_error_retry_workflow`, `error_recovery_workflow::test_fallback_provider_workflow`

### Scenario 5: Rate Limit Handling

1. User sends burst of requests
2. Rate limit hit
3. System backs off exponentially
4. Requests queued
5. Processing resumes after cooldown

**Tests**: `rate_limit_workflow::test_burst_request_handling_workflow`, `rate_limit_workflow::test_exponential_backoff_workflow`

## Mock Provider

Tests use a mock provider for consistent, reproducible testing:

```rust
let config = LlmConfig {
    provider: "mock".to_string(),
    model: "test-model".to_string(),
    api_key: Some("test-key".to_string()),
    ..Default::default()
};
```

The mock provider:
- Returns predictable responses
- Simulates various error conditions
- Supports streaming
- Tracks usage metrics
- Handles rate limiting

## Real Provider Testing (Optional)

To test with real providers, set environment variables:

```bash
export OPENAI_API_KEY=your_key
export ANTHROPIC_API_KEY=your_key
cargo test --test llm_e2e --features real_providers
```

**Note**: Real provider tests are expensive and may hit rate limits. Use sparingly.

## CI/CD Integration

### GitHub Actions Configuration

```yaml
name: E2E Tests

on: [push, pull_request]

jobs:
  e2e-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Run E2E Tests
        run: cargo test --test llm_e2e

      - name: Generate Coverage
        run: cargo tarpaulin --out Xml --test llm_e2e

      - name: Upload Coverage
        uses: codecov/codecov-action@v2
```

### Test Matrix

```yaml
strategy:
  matrix:
    rust: [stable, beta, nightly]
    os: [ubuntu-latest, macos-latest, windows-latest]
```

## Performance Benchmarks

### Expected Test Duration

- Chat workflows: ~2s
- Streaming workflows: ~3s
- Configuration workflows: ~1s
- Error recovery workflows: ~4s (includes retry delays)
- Rate limit workflows: ~5s (includes backoff delays)

**Total**: ~15 seconds for complete E2E suite

### Resource Usage

- Memory: <100MB
- CPU: Minimal (mock provider)
- Network: None (mock provider)

## Test Coverage Goals

- **Line Coverage**: >90%
- **Branch Coverage**: >85%
- **Function Coverage**: >95%

## Success Criteria

All E2E tests must pass before:
- Merging pull requests
- Releasing new versions
- Deploying to production

### Test Quality Metrics

- ✅ All workflows tested end-to-end
- ✅ Real-world scenarios covered
- ✅ Error paths validated
- ✅ Performance acceptable
- ✅ No flaky tests
- ✅ Clear failure messages

## Debugging Failed Tests

### Enable Verbose Logging

```bash
RUST_LOG=debug cargo test --test llm_e2e -- --nocapture
```

### Run Single Test

```bash
cargo test --test llm_e2e test_name -- --nocapture --exact
```

### Check Test Output

```bash
cargo test --test llm_e2e 2>&1 | grep -A 10 "FAILED"
```

## Adding New E2E Tests

1. Identify user workflow to test
2. Create test in appropriate file
3. Follow naming convention: `test_*_workflow`
4. Include Arrange-Act-Assert structure
5. Add descriptive documentation
6. Verify test passes
7. Update this README

## Test Data

Test data is generated programmatically to ensure:
- Consistency across test runs
- No external dependencies
- Reproducible results
- Fast execution

## Known Limitations

- Mock provider doesn't test real API integration
- Network conditions not fully simulated
- Real rate limits not tested (use real provider tests)
- Some timing-sensitive tests may be flaky in CI

## Future Enhancements

- [ ] Add real provider integration tests
- [ ] Implement performance regression testing
- [ ] Add chaos engineering tests
- [ ] Create visual test reports
- [ ] Add load testing scenarios
- [ ] Implement contract testing
- [ ] Add mutation testing

## Maintenance

This test suite should be updated when:
- New features are added
- API changes occur
- Bug fixes are implemented
- New error conditions are discovered
- Performance requirements change

## Contact

For questions or issues with E2E tests, contact the testing team or open an issue.
