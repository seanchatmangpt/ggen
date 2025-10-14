#!/usr/bin/env node
/**
 * Example: Using cleanroom CLI from Node.js
 *
 * This example demonstrates how to integrate the cleanroom CLI into Node.js applications.
 * It shows proper error handling, JSON parsing, and async/await patterns.
 */

const { execSync, exec } = require('child_process');
const { promisify } = require('util');
const execAsync = promisify(exec);

/**
 * Custom error for cleanroom CLI failures
 */
class CleanroomError extends Error {
  constructor(message, stderr) {
    super(message);
    this.name = 'CleanroomError';
    this.stderr = stderr;
  }
}

/**
 * Wrapper class for cleanroom CLI commands
 */
class CleanroomCLI {
  constructor(cliPath = 'cleanroom') {
    this.cliPath = cliPath;
  }

  /**
   * Run cleanroom CLI command synchronously
   * @param {string[]} args - Command arguments
   * @param {string} outputFormat - Output format ('json' or 'text')
   * @returns {object|null} Parsed JSON result or null
   */
  run(args, outputFormat = 'json') {
    const cmd = [this.cliPath, ...args];
    if (outputFormat === 'json') {
      cmd.push('--output', 'json');
    }

    try {
      const result = execSync(cmd.join(' '), { encoding: 'utf8' });

      if (outputFormat === 'json' && result) {
        return JSON.parse(result);
      }
      return null;
    } catch (error) {
      const stderr = error.stderr?.toString() || 'Unknown error';
      throw new CleanroomError(`Command failed: ${stderr}`, stderr);
    }
  }

  /**
   * Run cleanroom CLI command asynchronously
   * @param {string[]} args - Command arguments
   * @param {string} outputFormat - Output format ('json' or 'text')
   * @returns {Promise<object|null>} Parsed JSON result or null
   */
  async runAsync(args, outputFormat = 'json') {
    const cmd = [this.cliPath, ...args];
    if (outputFormat === 'json') {
      cmd.push('--output', 'json');
    }

    try {
      const { stdout, stderr } = await execAsync(cmd.join(' '));

      if (stderr) {
        console.warn(`Warning: ${stderr}`);
      }

      if (outputFormat === 'json' && stdout) {
        return JSON.parse(stdout);
      }
      return null;
    } catch (error) {
      const stderr = error.stderr?.toString() || 'Unknown error';
      throw new CleanroomError(`Command failed: ${stderr}`, stderr);
    }
  }
}

/**
 * Example: Basic cleanroom operations (synchronous)
 */
function exampleBasicUsage() {
  console.log('=== Basic Usage Example (Sync) ===\n');

  const cli = new CleanroomCLI();

  try {
    // Create environment
    console.log('Creating environment...');
    const envResult = cli.run(['environment', 'create', '--name', 'test-env']);
    console.log('Created environment:', JSON.stringify(envResult, null, 2), '\n');

    // Start PostgreSQL container
    console.log('Starting PostgreSQL container...');
    const container = cli.run([
      'container', 'start', 'postgres',
      '--db', 'testdb',
      '--user', 'testuser',
      '--password', 'testpass'
    ]);
    console.log('Started container:', JSON.stringify(container, null, 2), '\n');

    // Get connection info
    if (container && container.port) {
      console.log(`Connect to: postgresql://testuser:testpass@localhost:${container.port}/testdb\n`);
    }

    // List running containers
    console.log('Listing containers...');
    const containers = cli.run(['container', 'list']);
    console.log('Running containers:', JSON.stringify(containers, null, 2), '\n');

    // Cleanup
    console.log('Cleaning up...');
    cli.run(['environment', 'delete', '--name', 'test-env']);
    console.log('Environment deleted\n');

  } catch (error) {
    if (error instanceof CleanroomError) {
      console.error('Error:', error.message);
    } else {
      console.error('Unexpected error:', error);
    }
    process.exit(1);
  }
}

/**
 * Example: Async/await pattern
 */
async function exampleAsyncUsage() {
  console.log('=== Async Usage Example ===\n');

  const cli = new CleanroomCLI();

  try {
    // Create environment
    console.log('Creating environment...');
    await cli.runAsync(['environment', 'create', '--name', 'async-env']);

    // Start multiple services in parallel
    console.log('Starting services in parallel...');
    const [postgres, redis] = await Promise.all([
      cli.runAsync(['container', 'start', 'postgres', '--db', 'testdb']),
      cli.runAsync(['container', 'start', 'redis'])
    ]);

    console.log('PostgreSQL port:', postgres?.port);
    console.log('Redis port:', redis?.port, '\n');

    // Run tests
    console.log('Running tests...');
    const results = await cli.runAsync(['test', 'run', '--file', 'test.rs']);

    if (results) {
      console.log(`Tests passed: ${results.passed || 0}`);
      console.log(`Tests failed: ${results.failed || 0}`);
      console.log(`Duration: ${results.duration || 0}s\n`);
    }

  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  } finally {
    // Cleanup
    console.log('Cleaning up...');
    try {
      await cli.runAsync(['environment', 'delete', '--name', 'async-env']);
    } catch {
      // Ignore cleanup errors
    }
  }
}

/**
 * Example: Test runner with retry logic
 */
async function exampleTestRunner() {
  console.log('=== Test Runner Example ===\n');

  const cli = new CleanroomCLI();
  const maxRetries = 3;

  try {
    // Create environment
    await cli.runAsync(['environment', 'create', '--name', 'test-env']);

    // Start services
    console.log('Starting services...');
    await cli.runAsync(['container', 'start', 'postgres', '--db', 'testdb']);

    // Run tests with retry
    let attempt = 0;
    let testResults = null;

    while (attempt < maxRetries) {
      attempt++;
      console.log(`Test attempt ${attempt}/${maxRetries}...`);

      try {
        testResults = await cli.runAsync(['test', 'run', '--file', 'test.rs']);

        if (testResults && testResults.failed === 0) {
          console.log('All tests passed!\n');
          break;
        } else if (testResults) {
          console.log(`Some tests failed (${testResults.failed}), retrying...\n`);
        }
      } catch (error) {
        console.log(`Tests failed with error: ${error.message}\n`);

        if (attempt === maxRetries) {
          throw error;
        }

        // Wait before retry
        await new Promise(resolve => setTimeout(resolve, 1000));
      }
    }

    // Collect metrics
    console.log('Collecting metrics...');
    const metrics = await cli.runAsync(['metrics', 'show']);
    console.log('Metrics:', JSON.stringify(metrics, null, 2), '\n');

  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  } finally {
    await cli.runAsync(['environment', 'delete', '--name', 'test-env']);
  }
}

/**
 * Example: Multi-container orchestration
 */
async function exampleMultiContainer() {
  console.log('=== Multi-Container Example ===\n');

  const cli = new CleanroomCLI();

  const services = [
    { name: 'postgres', args: ['--db', 'users', '--user', 'admin'] },
    { name: 'redis', args: [] },
    { name: 'rabbitmq', args: [] }
  ];

  try {
    // Create environment
    await cli.runAsync(['environment', 'create', '--name', 'microservices']);

    // Start services sequentially
    console.log('Starting services...');
    const containers = [];

    for (const service of services) {
      console.log(`  Starting ${service.name}...`);
      const result = await cli.runAsync([
        'container', 'start', service.name,
        ...service.args
      ]);

      if (result && result.port) {
        console.log(`    ${service.name} available on port ${result.port}`);
      }
      containers.push({ name: service.name, ...result });
    }

    console.log('\nAll services started!\n');

    // Check health
    console.log('Checking health...');
    const health = await cli.runAsync(['health', 'check']);
    console.log('Health status:', JSON.stringify(health, null, 2), '\n');

    // Export connection info
    const connectionInfo = containers.reduce((acc, c) => {
      if (c.port) {
        acc[c.name] = {
          host: 'localhost',
          port: c.port,
          url: `${c.name}://localhost:${c.port}`
        };
      }
      return acc;
    }, {});

    console.log('Connection info:', JSON.stringify(connectionInfo, null, 2));

  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  } finally {
    // Cleanup
    console.log('\nStopping services...');
    try {
      await cli.runAsync(['environment', 'delete', '--name', 'microservices']);
      console.log('Services stopped');
    } catch {
      // Ignore cleanup errors
    }
  }
}

/**
 * Example: Error handling patterns
 */
async function exampleErrorHandling() {
  console.log('=== Error Handling Example ===\n');

  const cli = new CleanroomCLI();

  // Test 1: Non-existent environment
  try {
    await cli.runAsync(['environment', 'delete', '--name', 'non-existent']);
  } catch (error) {
    if (error instanceof CleanroomError) {
      console.log('Expected error caught:', error.message, '\n');
    }
  }

  // Test 2: Invalid container
  try {
    await cli.runAsync(['container', 'start', 'invalid-service']);
  } catch (error) {
    if (error instanceof CleanroomError) {
      console.log('Expected error caught:', error.message, '\n');
    }
  }

  console.log('Error handling works correctly!');
}

/**
 * Main function - run all examples
 */
async function main() {
  console.log('Cleanroom CLI Node.js Integration Examples\n');
  console.log('=' .repeat(60) + '\n');

  const examples = [
    { name: 'Basic Usage (Sync)', fn: exampleBasicUsage },
    { name: 'Async Usage', fn: exampleAsyncUsage },
    { name: 'Test Runner', fn: exampleTestRunner },
    { name: 'Multi-Container', fn: exampleMultiContainer },
    { name: 'Error Handling', fn: exampleErrorHandling }
  ];

  for (const example of examples) {
    try {
      if (example.fn.constructor.name === 'AsyncFunction') {
        await example.fn();
      } else {
        example.fn();
      }
    } catch (error) {
      console.error(`\nExample '${example.name}' failed:`, error.message, '\n');
    }
    console.log('=' .repeat(60) + '\n');
  }
}

// Run examples
if (require.main === module) {
  main().catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
}

// Export for use as module
module.exports = { CleanroomCLI, CleanroomError };
