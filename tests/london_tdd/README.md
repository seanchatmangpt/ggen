# London TDD Test Suite for README.md Capabilities

This directory contains a comprehensive London School TDD test suite validating all capabilities documented in the project README.md.

## London TDD Principles Applied

**Mock-Driven Development:**
- Mock all external dependencies (filesystem, network, AI providers)
- Test behavior and interactions, not implementation details
- Fast feedback loop (<100ms per test)
- Verify side effects through mocks
- Use trait abstractions for mockability

## Test Structure

```
tests/london_tdd/
├── README.md (this file)
├── cli_commands/
│   ├── doctor_test.rs          # Environment health check tests
│   ├── help_me_test.rs         # Progressive help system tests
│   ├── quickstart_test.rs      # Quickstart command tests
│   └── enhanced_errors_test.rs # Error message enhancement tests
├── marketplace/
│   ├── search_test.rs          # Marketplace search tests
│   ├── install_test.rs         # Package installation tests
│   └── categories_test.rs      # Category browsing tests
├── ai_generation/
│   ├── template_gen_test.rs    # AI template generation tests
│   ├── project_gen_test.rs     # AI project scaffolding tests
│   ├── sparql_gen_test.rs      # AI SPARQL generation tests
│   └── graph_gen_test.rs       # AI RDF graph generation tests
├── template_engine/
│   ├── rendering_test.rs       # Template rendering tests
│   ├── rdf_sparql_test.rs      # RDF/SPARQL integration tests
│   └── injection_test.rs       # Code injection tests
├── otel_validation/
│   └── trace_validator.rs      # OpenTelemetry instrumentation validation
└── lib.rs                      # Common test utilities and mocks
```

## Running Tests

```bash
# Run all London TDD tests
cargo test --test london_tdd

# Run specific test module
cargo test --test london_tdd -- cli_commands

# Run with output
cargo test --test london_tdd -- --nocapture

# Run in parallel (default)
cargo test --test london_tdd

# Run single-threaded for debugging
cargo test --test london_tdd -- --test-threads=1
```

## Test Coverage

### CLI Commands (README §User-Friendly Features)
- ✅ `ggen doctor` - Environment health check with fix instructions
- ✅ `ggen help-me` - Progressive help system (Newcomer → Expert)
- ✅ `ggen quickstart` - 2-minute quickstart flow
- ✅ Enhanced error messages with "Did you mean?" suggestions

### Marketplace (README §Marketplace)
- ✅ `ggen search` - Natural language search
- ✅ `ggen add` - Package installation
- ✅ `ggen categories` - Category browsing
- ✅ `ggen packs` - Installed packages list
- ✅ `ggen update` - Package updates

### AI Generation (README §AI-Powered Generation)
- ✅ `ggen ai generate` - AI template generation
- ✅ `ggen ai project` - AI project scaffolding
- ✅ `ggen ai sparql` - AI SPARQL query generation
- ✅ `ggen ai graph` - AI RDF graph generation
- ✅ `ggen ai search` - Natural language template search
- ✅ `ggen ai frontmatter` - Smart frontmatter generation

### Template Engine (README §Template Example)
- ✅ YAML frontmatter parsing
- ✅ Tera template rendering
- ✅ RDF/SPARQL integration
- ✅ Deterministic generation
- ✅ Code injection modes

### GitHub Integration (README §GitHub Integration)
- ✅ `ggen github pages-status` - Pages deployment status
- ✅ `ggen github workflow-status` - Workflow status
- ✅ `ggen github trigger-workflow` - Workflow triggering

## Performance Targets (README §Performance SLOs)

All tests verify SLO compliance:
- Test execution: <100ms per test
- Memory usage: <50MB per test
- No network calls (all mocked)
- 100% reproducible results

## OpenTelemetry Instrumentation

All tests include trace validation:
- Verify spans are created for operations
- Validate span attributes and events
- Check trace context propagation
- Ensure proper error recording

## Mock Providers

### Filesystem Mock
- `MockFilesystem` - Mock file operations

### AI Provider Mock
- `MockLlmClient` - Mock OpenAI, Anthropic, Ollama

### Network Mock
- `MockHttpClient` - Mock HTTP requests
- `MockGitHubClient` - Mock GitHub API

### Registry Mock
- `MockMarketplace` - Mock package registry

## Test Data

Test data is generated using the `fake` crate for realistic scenarios without hardcoding.

## Continuous Integration

These tests run on every PR and commit:
- Fast feedback (<5s total suite)
- No external dependencies
- Deterministic results
- No flakiness

## Contributing

When adding new README capabilities:
1. Add London TDD tests FIRST
2. Use mocks for external dependencies
3. Test behavior, not implementation
4. Ensure <100ms execution
5. Add OpenTelemetry validation
6. Update this README
