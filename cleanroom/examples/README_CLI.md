# Cleanroom CLI Integration Guide

This guide demonstrates how to integrate the cleanroom CLI into your applications using Python, Node.js, and Bash.

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Language Integration](#language-integration)
  - [Python](#python-integration)
  - [Node.js](#nodejs-integration)
  - [Bash](#bash-integration)
- [Common Patterns](#common-patterns)
- [Error Handling](#error-handling)
- [JSON Parsing](#json-parsing)
- [Best Practices](#best-practices)
- [Examples](#examples)

## Installation

### Install Cleanroom CLI

```bash
# Build from source
cd cleanroom
cargo build --release

# Add to PATH
export PATH="$PATH:$(pwd)/target/release"

# Verify installation
cleanroom --version
```

### Install Language Dependencies

**Python:**
```bash
# No external dependencies required for basic usage
# Optional: install for better JSON handling
pip install pydantic  # For schema validation
```

**Node.js:**
```bash
# No external dependencies required
# Core modules used: child_process, util
```

**Bash:**
```bash
# Install jq for JSON parsing (highly recommended)
# macOS
brew install jq

# Linux
apt-get install jq  # Debian/Ubuntu
yum install jq      # RedHat/CentOS
```

## Quick Start

### Basic Usage

**Create and manage test environments:**

```bash
# Create environment
cleanroom environment create --name myenv

# Start a PostgreSQL container
cleanroom container start postgres --db testdb --user admin

# Run tests
cleanroom test run --file integration_tests.rs

# Get metrics
cleanroom metrics show --output json

# Cleanup
cleanroom environment delete --name myenv
```

### JSON Output

All commands support `--output json` for programmatic access:

```bash
cleanroom container start postgres --output json
# {
#   "id": "abc123",
#   "name": "postgres",
#   "port": 5432,
#   "status": "running"
# }
```

## Language Integration

### Python Integration

**Basic Usage:**

```python
from cli_python import CleanroomCLI, CleanroomError

cli = CleanroomCLI()

try:
    # Create environment
    env = cli.run(['environment', 'create', '--name', 'myenv'])

    # Start container
    container = cli.run(['container', 'start', 'postgres', '--db', 'testdb'])
    print(f"PostgreSQL running on port: {container['port']}")

    # Run tests
    results = cli.run(['test', 'run', '--file', 'test.rs'])
    print(f"Tests passed: {results['passed']}")

except CleanroomError as e:
    print(f"Error: {e}")
finally:
    # Cleanup
    cli.run(['environment', 'delete', '--name', 'myenv'])
```

**Features:**
- ✅ Type hints and docstrings
- ✅ Custom exception handling
- ✅ JSON parsing built-in
- ✅ Both sync and async support (see examples)

**Run Examples:**
```bash
python examples/cli_python.py
```

### Node.js Integration

**Basic Usage:**

```javascript
const { CleanroomCLI } = require('./cli_nodejs');

const cli = new CleanroomCLI();

async function main() {
  try {
    // Create environment
    await cli.runAsync(['environment', 'create', '--name', 'myenv']);

    // Start container
    const container = await cli.runAsync([
      'container', 'start', 'postgres', '--db', 'testdb'
    ]);
    console.log(`PostgreSQL running on port: ${container.port}`);

    // Run tests
    const results = await cli.runAsync(['test', 'run', '--file', 'test.rs']);
    console.log(`Tests passed: ${results.passed}`);

  } catch (error) {
    console.error('Error:', error.message);
  } finally {
    // Cleanup
    await cli.runAsync(['environment', 'delete', '--name', 'myenv']);
  }
}

main();
```

**Features:**
- ✅ Async/await and promises
- ✅ Synchronous option available
- ✅ Custom error class
- ✅ Parallel execution support

**Run Examples:**
```bash
node examples/cli_nodejs.js
```

### Bash Integration

**Basic Usage:**

```bash
#!/usr/bin/env bash
source examples/cli_bash.sh  # Or use functions directly

# Create environment
cleanroom_json environment create --name "myenv"

# Start container and get port
CONTAINER=$(cleanroom_json container start postgres --db testdb)
PORT=$(echo "$CONTAINER" | jq -r '.port')
echo "PostgreSQL running on port: $PORT"

# Run tests and check results
RESULTS=$(cleanroom_json test run --file test.rs)
PASSED=$(echo "$RESULTS" | jq -r '.passed')
echo "Tests passed: $PASSED"

# Cleanup
cleanroom_json environment delete --name "myenv"
```

**Features:**
- ✅ Helper functions for common tasks
- ✅ Error handling with set -euo pipefail
- ✅ jq integration for JSON parsing
- ✅ Colorized output
- ✅ CI/CD ready

**Run Examples:**
```bash
bash examples/cli_bash.sh
```

## Common Patterns

### Pattern 1: Create and Teardown

**Python:**
```python
cli = CleanroomCLI()

try:
    cli.run(['environment', 'create', '--name', 'test'])
    # ... do work ...
finally:
    cli.run(['environment', 'delete', '--name', 'test'])
```

**Node.js:**
```javascript
try {
  await cli.runAsync(['environment', 'create', '--name', 'test']);
  // ... do work ...
} finally {
  await cli.runAsync(['environment', 'delete', '--name', 'test']);
}
```

**Bash:**
```bash
trap 'cleanroom_json environment delete --name test' EXIT
cleanroom_json environment create --name test
# ... do work ...
```

### Pattern 2: Multi-Container Setup

**Python:**
```python
services = [
    ('postgres', ['--db', 'testdb']),
    ('redis', []),
    ('rabbitmq', [])
]

for name, args in services:
    cli.run(['container', 'start', name] + args)
```

**Node.js:**
```javascript
const services = [
  { name: 'postgres', args: ['--db', 'testdb'] },
  { name: 'redis', args: [] },
  { name: 'rabbitmq', args: [] }
];

// Parallel start
await Promise.all(
  services.map(s => cli.runAsync(['container', 'start', s.name, ...s.args]))
);
```

**Bash:**
```bash
for service in postgres redis rabbitmq; do
  cleanroom_json container start "$service"
done
```

### Pattern 3: Test Runner with Retry

**Python:**
```python
max_retries = 3
for attempt in range(max_retries):
    try:
        results = cli.run(['test', 'run', '--file', 'test.rs'])
        if results['failed'] == 0:
            break
    except CleanroomError:
        if attempt == max_retries - 1:
            raise
        time.sleep(1)
```

**Node.js:**
```javascript
const maxRetries = 3;
for (let i = 0; i < maxRetries; i++) {
  try {
    const results = await cli.runAsync(['test', 'run', '--file', 'test.rs']);
    if (results.failed === 0) break;
  } catch (error) {
    if (i === maxRetries - 1) throw error;
    await new Promise(resolve => setTimeout(resolve, 1000));
  }
}
```

**Bash:**
```bash
retry=3
for i in $(seq 1 $retry); do
  if cleanroom_json test run --file test.rs; then
    break
  fi
  [[ $i -eq $retry ]] && exit 1
  sleep 1
done
```

## Error Handling

### Python

```python
from cli_python import CleanroomCLI, CleanroomError

cli = CleanroomCLI()

try:
    result = cli.run(['container', 'start', 'postgres'])
except CleanroomError as e:
    # Handle cleanroom-specific errors
    print(f"Cleanroom error: {e}")
    sys.exit(1)
except Exception as e:
    # Handle unexpected errors
    print(f"Unexpected error: {e}")
    sys.exit(2)
```

### Node.js

```javascript
const { CleanroomCLI, CleanroomError } = require('./cli_nodejs');

try {
  await cli.runAsync(['container', 'start', 'postgres']);
} catch (error) {
  if (error instanceof CleanroomError) {
    console.error('Cleanroom error:', error.message);
    console.error('Stderr:', error.stderr);
  } else {
    console.error('Unexpected error:', error);
  }
  process.exit(1);
}
```

### Bash

```bash
set -euo pipefail  # Exit on error, undefined vars, pipe failures

# Error handler
error() {
    echo "Error: $1" >&2
    exit 1
}

# Use with || operator
cleanroom_json container start postgres || error "Failed to start postgres"

# Or with if statement
if ! cleanroom_json container start postgres; then
    error "Failed to start postgres"
fi
```

## JSON Parsing

### Python (Built-in)

```python
import json

result = cli.run(['metrics', 'show'])

# Access fields
startup_time = result.get('startup_time', 0)
memory_mb = result.get('memory_mb', 0)

# Pretty print
print(json.dumps(result, indent=2))
```

### Node.js (Built-in)

```javascript
const result = await cli.runAsync(['metrics', 'show']);

// Access fields
const startupTime = result.startup_time || 0;
const memoryMb = result.memory_mb || 0;

// Pretty print
console.log(JSON.stringify(result, null, 2));
```

### Bash (jq)

```bash
# Check if jq is available
if command -v jq &> /dev/null; then
    # Parse with jq
    RESULT=$(cleanroom_json metrics show)
    STARTUP_TIME=$(echo "$RESULT" | jq -r '.startup_time // 0')
    MEMORY_MB=$(echo "$RESULT" | jq -r '.memory_mb // 0')

    # Pretty print
    echo "$RESULT" | jq '.'
else
    # Fallback without jq
    echo "Install jq for better JSON parsing"
    cleanroom_json metrics show
fi
```

**Common jq Queries:**

```bash
# Get specific field
echo "$JSON" | jq -r '.field_name'

# Get nested field
echo "$JSON" | jq -r '.parent.child'

# Get array element
echo "$JSON" | jq -r '.array[0]'

# Filter array
echo "$JSON" | jq -r '.[] | select(.status == "running")'

# Default value
echo "$JSON" | jq -r '.field // "default"'

# Multiple fields
echo "$JSON" | jq -r '"\(.name): \(.port)"'
```

## Best Practices

### 1. Always Cleanup

**Python:**
```python
try:
    cli.run(['environment', 'create', '--name', 'test'])
    # ... work ...
finally:
    try:
        cli.run(['environment', 'delete', '--name', 'test'])
    except CleanroomError:
        pass  # Ignore cleanup errors
```

**Node.js:**
```javascript
try {
  // ... work ...
} finally {
  try {
    await cli.runAsync(['environment', 'delete', '--name', 'test']);
  } catch (error) {
    // Ignore cleanup errors
  }
}
```

**Bash:**
```bash
trap 'cleanroom_json environment delete --name test 2>/dev/null || true' EXIT
```

### 2. Use JSON Output

Always use `--output json` for programmatic access:

```bash
# Good
result=$(cleanroom container start postgres --output json)

# Bad (parsing text output is fragile)
result=$(cleanroom container start postgres)
```

### 3. Handle Errors Gracefully

```python
# Good - specific error handling
try:
    cli.run(['container', 'start', 'postgres'])
except CleanroomError as e:
    if 'already exists' in str(e):
        print("Container already running")
    else:
        raise

# Bad - generic error handling
try:
    cli.run(['container', 'start', 'postgres'])
except:
    pass  # Silently ignoring errors
```

### 4. Use Timeouts in CI/CD

**Python:**
```python
import subprocess

try:
    subprocess.run(
        ['cleanroom', 'test', 'run'],
        timeout=300,  # 5 minutes
        check=True
    )
except subprocess.TimeoutExpired:
    print("Tests timed out")
```

**Node.js:**
```javascript
const { execSync } = require('child_process');

try {
  execSync('cleanroom test run', { timeout: 300000 }); // 5 minutes
} catch (error) {
  if (error.killed) {
    console.error('Tests timed out');
  }
}
```

**Bash:**
```bash
timeout 300 cleanroom test run || {
    echo "Tests timed out"
    exit 1
}
```

### 5. Validate Dependencies

**Python:**
```python
import shutil

if not shutil.which('cleanroom'):
    print("Error: cleanroom CLI not found")
    sys.exit(1)
```

**Node.js:**
```javascript
const { execSync } = require('child_process');

try {
  execSync('which cleanroom', { stdio: 'ignore' });
} catch (error) {
  console.error('Error: cleanroom CLI not found');
  process.exit(1);
}
```

**Bash:**
```bash
if ! command -v cleanroom &> /dev/null; then
    echo "Error: cleanroom CLI not found"
    exit 1
fi
```

## Examples

### Example 1: CI/CD Integration

```python
#!/usr/bin/env python3
# ci_runner.py - Run tests in CI environment

import sys
from cli_python import CleanroomCLI, CleanroomError

def run_ci_tests():
    cli = CleanroomCLI()
    exit_code = 0

    try:
        # Create test environment
        print("Setting up test environment...")
        cli.run(['environment', 'create', '--name', 'ci-test'])

        # Start services
        print("Starting services...")
        cli.run(['container', 'start', 'postgres', '--db', 'testdb'])
        cli.run(['container', 'start', 'redis'])

        # Run tests
        print("Running tests...")
        results = cli.run(['test', 'run', '--file', 'integration_tests.rs'])

        # Check results
        if results['failed'] > 0:
            print(f"❌ {results['failed']} tests failed")
            exit_code = 1
        else:
            print(f"✅ All {results['passed']} tests passed")

        # Collect metrics
        metrics = cli.run(['metrics', 'show'])
        print(f"Duration: {metrics.get('duration', 0)}s")

    except CleanroomError as e:
        print(f"Error: {e}")
        exit_code = 2
    finally:
        # Cleanup
        try:
            cli.run(['environment', 'delete', '--name', 'ci-test'])
        except CleanroomError:
            pass

    sys.exit(exit_code)

if __name__ == '__main__':
    run_ci_tests()
```

### Example 2: Development Environment

```javascript
#!/usr/bin/env node
// dev_env.js - Set up development environment

const { CleanroomCLI } = require('./cli_nodejs');

async function setupDevEnv() {
  const cli = new CleanroomCLI();

  try {
    console.log('Setting up development environment...\n');

    // Create environment
    await cli.runAsync(['environment', 'create', '--name', 'dev']);

    // Start all services
    const services = [
      { name: 'postgres', args: ['--db', 'devdb', '--user', 'dev'] },
      { name: 'redis', args: [] },
      { name: 'rabbitmq', args: [] }
    ];

    console.log('Starting services...');
    const results = await Promise.all(
      services.map(s => cli.runAsync(['container', 'start', s.name, ...s.args]))
    );

    // Print connection info
    console.log('\n✅ Development environment ready!\n');
    console.log('Services:');
    results.forEach((r, i) => {
      console.log(`  ${services[i].name}: localhost:${r.port}`);
    });

    console.log('\nPress Ctrl+C to stop services');

    // Wait for interrupt
    await new Promise(() => {});

  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  } finally {
    await cli.runAsync(['environment', 'delete', '--name', 'dev']);
  }
}

// Handle Ctrl+C
process.on('SIGINT', () => {
  console.log('\nShutting down...');
  process.exit(0);
});

setupDevEnv();
```

### Example 3: Performance Benchmarking

```bash
#!/usr/bin/env bash
# bench.sh - Run performance benchmarks

source examples/cli_bash.sh

benchmark() {
    local name="$1"
    local iterations="${2:-10}"

    echo "Running benchmark: $name ($iterations iterations)"

    local total_time=0

    for i in $(seq 1 $iterations); do
        local start=$(date +%s%N)

        # Run benchmark
        cleanroom_json environment create --name "bench-$i" > /dev/null
        cleanroom_json container start postgres --db testdb > /dev/null
        cleanroom_json test run --file bench_test.rs > /dev/null
        cleanroom_json environment delete --name "bench-$i" > /dev/null

        local end=$(date +%s%N)
        local duration=$(( (end - start) / 1000000 ))  # Convert to ms
        total_time=$(( total_time + duration ))

        echo "  Iteration $i: ${duration}ms"
    done

    local avg=$(( total_time / iterations ))
    echo "Average: ${avg}ms"
    echo
}

main() {
    echo "Cleanroom Performance Benchmarks"
    echo "================================"
    echo

    benchmark "Full Test Cycle" 10

    # Export results
    cat > /tmp/bench-results.json <<EOF
{
  "timestamp": "$(date -Iseconds)",
  "iterations": 10,
  "average_ms": ${avg}
}
EOF

    success "Results saved to /tmp/bench-results.json"
}

main
```

## Troubleshooting

### Issue: Command Not Found

```bash
# Check if cleanroom is in PATH
which cleanroom

# If not found, add to PATH
export PATH="$PATH:/path/to/cleanroom/target/release"

# Or use absolute path
CLEANROOM_CLI="/path/to/cleanroom/target/release/cleanroom"
```

### Issue: JSON Parsing Fails

**Python:**
```python
try:
    result = json.loads(output)
except json.JSONDecodeError as e:
    print(f"Failed to parse JSON: {e}")
    print(f"Output was: {output}")
```

**Bash:**
```bash
# Check if output is valid JSON
if echo "$output" | jq . > /dev/null 2>&1; then
    # Parse JSON
    echo "$output" | jq .
else
    echo "Invalid JSON output"
    echo "$output"
fi
```

### Issue: Permission Denied

```bash
# Make scripts executable
chmod +x examples/cli_python.py
chmod +x examples/cli_nodejs.js
chmod +x examples/cli_bash.sh
```

## Additional Resources

- **Cleanroom Documentation**: `../docs/`
- **API Reference**: `../docs/api.md`
- **Test Examples**: `../tests/`
- **GitHub**: https://github.com/yourusername/cleanroom

## Contributing

Contributions are welcome! Please see `../CONTRIBUTING.md` for guidelines.

## License

See `../LICENSE` for details.
