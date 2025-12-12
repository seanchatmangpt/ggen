# Ollama Integration Tests

This directory contains integration tests for Ollama LLM functionality in ggen-ai.

## Prerequisites

Before running these tests, ensure you have:

1. **Ollama installed and running**:
   ```bash
   # Install Ollama (if not already installed)
   curl -fsSL https://ollama.ai/install.sh | sh
   
   # Start Ollama service
   ollama serve
   ```

2. **ministral-3:3b model pulled**:
   ```bash
   ollama pull ministral-3:3b
   ```

3. **Verify Ollama is running**:
   ```bash
   curl http://localhost:11434/api/tags
   ```

## Running the Tests

### Run All Ollama Integration Tests
```bash
cargo make test-ollama
```

### Run Performance Tests
```bash
cargo make test-ollama-performance
```

### Run Resilience Tests
```bash
cargo make test-ollama-resilience
```

### Run All Ollama Tests
```bash
cargo make test-ollama-all
```

### Run Specific Test
```bash
cargo test --features ollama-integration --test ollama_integration test_ollama_template_generation
```

### Run with Verbose Output
```bash
cargo test --features ollama-integration --test ollama_integration -- --nocapture
```

## Test Coverage

### Integration Tests (`ollama_integration.rs`)
- ✅ **Template Generation**: AI-powered template creation with Ollama
- ✅ **SPARQL Generation**: Query generation from natural language descriptions
- ✅ **Ontology Generation**: RDF/OWL ontology creation
- ✅ **MCP Tools**: Model Context Protocol server functionality
- ✅ **CLI Integration**: End-to-end CLI command testing
- ✅ **Frontmatter Generation**: Template frontmatter creation
- ✅ **Error Handling**: Graceful failure scenarios
- ✅ **Model Availability**: Ollama model support verification

### Performance Tests (`ollama_performance.rs`)
- ⚡ **Performance Benchmarks**: Template generation speed and efficiency
- 🔄 **Concurrent Operations**: Multiple simultaneous generations
- 🛡️ **Error Recovery**: Handling edge cases and failures
- 🎯 **Deterministic Output**: Consistency across multiple runs
- 💾 **Memory Usage**: Resource management and cleanup

### Resilience Tests (`ollama_resilience.rs`)
- 🌐 **Network Resilience**: Handling network issues and timeouts
- 🔄 **Model Switching**: Switching between different Ollama models
- 🔄 **Concurrent Error Handling**: Managing multiple concurrent failures
- 🧹 **Resource Cleanup**: Proper cleanup after operations
- 🛡️ **Graceful Degradation**: Handling reduced functionality scenarios

## Test Behavior

- **Graceful Skipping**: Tests automatically skip if Ollama is not available
- **Timeout Protection**: All tests have reasonable timeouts to prevent hanging
- **Cleanup**: Tests use temporary directories and clean up after themselves
- **Isolation**: Each test is independent and can be run individually

## Troubleshooting

### Common Issues

1. **"Ollama not available"**: Ensure Ollama is running on port 11434
2. **"Model not found"**: Run `ollama pull ministral-3:3b`
3. **Timeout errors**: Check Ollama performance and system resources
4. **Connection errors**: Verify Ollama is accessible at `http://localhost:11434`

### Debug Mode

Run tests with debug output:
```bash
RUST_LOG=debug cargo test --features ollama-integration --test ollama_integration -- --nocapture
```

### Manual Verification

Test Ollama manually:
```bash
# Test basic completion
curl http://localhost:11434/api/generate -d '{
  "model": "ministral-3:3b",
  "prompt": "Hello, world!",
  "stream": false
}'
```

## Integration with CI/CD

These tests are excluded from normal CI runs to avoid external dependencies. They can be run manually or in dedicated environments with Ollama available.

To include in CI (optional):
```yaml
- name: Run Ollama Integration Tests
  run: |
    ollama serve &
    sleep 5
    ollama pull ministral-3:3b
    cargo make test-ollama
```
