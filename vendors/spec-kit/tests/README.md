# Spec-Kit Testcontainer Validation

This directory contains testcontainer-based integration tests that validate the mcpp v6 RDF-first workflow.

## What is Tested

### Constitutional Equation: `spec.md = μ(feature.ttl)`

The tests verify the fundamental principle of RDF-first architecture:
- **μ₁ (Normalization)**: TTL syntax validation
- **μ₂ (Extraction)**: SPARQL query execution
- **μ₃ (Emission)**: Tera template rendering
- **μ₄ (Canonicalization)**: Markdown formatting
- **μ₅ (Receipt)**: Cryptographic provenance

### Test Coverage

1. **test_mcpp_sync_generates_markdown**: Verifies `mcpp sync` produces expected markdown from TTL sources
2. **test_mcpp_sync_idempotence**: Verifies μ∘μ = μ (running twice produces identical output)
3. **test_mcpp_validates_ttl_syntax**: Verifies invalid TTL is rejected
4. **test_constitutional_equation_verification**: Verifies deterministic transformation with hash verification

## Prerequisites

### Required

- **Docker**: Must be running (testcontainers needs it)
- **Python 3.11+**: Required for test execution
- **uv**: For dependency management

### Install Test Dependencies

```bash
# Install with test dependencies
uv pip install -e ".[test]"

# Or using pip
pip install -e ".[test]"
```

This installs:
- pytest (test framework)
- pytest-cov (coverage reporting)
- testcontainers (Docker container orchestration)
- rdflib (RDF parsing and validation)

## Running Tests

### Run All Tests

```bash
# Using pytest directly
pytest tests/

# With coverage report
pytest tests/ --cov=src --cov-report=term-missing

# Verbose output
pytest tests/ -v -s
```

### Run Integration Tests Only

```bash
pytest tests/integration/ -v -s
```

### Run Specific Test

```bash
pytest tests/integration/test_mcpp_sync.py::test_mcpp_sync_generates_markdown -v -s
```

### Skip Slow Tests

```bash
pytest tests/ -m "not integration"
```

## How It Works

### Testcontainer Architecture

1. **Container Spin-up**:
   - Uses official `rust:latest` Docker image
   - Installs mcpp from source (`https://github.com/seanchatmangpt/mcpp.git`)
   - Verifies installation with `mcpp --version`

2. **Test Fixtures**:
   - `fixtures/feature-content.ttl` - Sample RDF feature specification
   - `fixtures/mcpp.toml` - mcpp configuration with SPARQL query and template
   - `fixtures/spec.tera` - Tera template for markdown generation
   - `fixtures/expected-spec.md` - Expected output for validation

3. **Test Execution**:
   - Copies fixtures into container workspace
   - Runs `mcpp sync` inside container
   - Validates generated markdown matches expected output
   - Verifies idempotence and determinism

### Validation Pipeline

```
TTL Source (feature-content.ttl)
    ↓ μ₁ Normalization (syntax check)
    ↓ μ₂ Extraction (SPARQL query)
    ↓ μ₃ Emission (Tera template)
    ↓ μ₄ Canonicalization (format)
    ↓ μ₅ Receipt (hash)
Generated Markdown (spec.md)
```

## Troubleshooting

### Docker Not Running

```
Error: Cannot connect to the Docker daemon
```

**Solution**: Start Docker Desktop or Docker daemon:
```bash
# macOS
open -a Docker

# Linux
sudo systemctl start docker
```

### mcpp Installation Fails

```
Error: Failed to install mcpp
```

**Solution**: Check Rust/Cargo version in container, verify git access to mcpp repo.

### Tests Take Too Long

Integration tests pull Docker images and compile Rust code (mcpp installation).

**First run**: ~5-10 minutes (downloads Rust image, compiles mcpp)
**Subsequent runs**: ~1-2 minutes (uses cached container layers)

**Speed up**:
```bash
# Pre-pull Rust image
docker pull rust:latest
```

### Output Doesn't Match Expected

The test compares generated markdown with `expected-spec.md`. If mcpp output format changes:

1. Review generated output in test logs
2. Update `fixtures/expected-spec.md` to match new format
3. Verify the change is intentional (not a bug)

## CI/CD Integration

### GitHub Actions

Add to `.github/workflows/test.yml`:

```yaml
name: Test

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - uses: actions/setup-python@v5
        with:
          python-version: '3.11'

      - name: Install dependencies
        run: |
          pip install uv
          uv pip install -e ".[test]"

      - name: Run tests
        run: pytest tests/ -v --cov=src --cov-report=xml

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          file: ./coverage.xml
```

## Adding New Tests

### Create New Test File

```python
# tests/integration/test_new_feature.py

import pytest
from testcontainers.core.container import DockerContainer

@pytest.mark.integration
@pytest.mark.requires_docker
def test_new_mcpp_feature(mcpp_container):
    """Test description."""
    # Use mcpp_container fixture from conftest
    exit_code, output = mcpp_container.exec(["mcpp", "your-command"])
    assert exit_code == 0
```

### Add New Fixtures

1. Add TTL files to `tests/integration/fixtures/`
2. Add corresponding templates and expected outputs
3. Update `mcpp.toml` if needed for new SPARQL queries

## Coverage Goals

- **Line Coverage**: 80%+ (minimum)
- **Branch Coverage**: 70%+ (goal)
- **Integration Coverage**: All critical workflows

## References

- [Testcontainers Python Docs](https://testcontainers-python.readthedocs.io/)
- [mcpp Documentation](https://github.com/seanchatmangpt/mcpp)
- [RDF Workflow Guide](../docs/RDF_WORKFLOW_GUIDE.md)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [Tera Template Engine](https://keats.github.io/tera/)
