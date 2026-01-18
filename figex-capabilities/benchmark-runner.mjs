/**
 * Runs a command with specified arguments and measures performance over multiple iterations.
 * @param {string} command - The command to execute.
 * @param {Array<string>} args - The arguments for the command.
 * @param {number} iterations - Number of iterations to run the command.
 * @returns {Promise<{duration: number, memory: number, result: any}>} A promise that resolves to an object containing performance metrics and the result.
 * @throws {Error} If the command execution fails.
 * @example
 * const result = await benchmarkCommand('node', ['script.js'], 5);
 * console.log(result.duration, result.memory, result.result);
 */
export async function benchmarkCommand(command, args, iterations) {
  if (!command || typeof command !== 'string') {
    throw new Error('Command must be a non-empty string.');
  }

  if (!Array.isArray(args)) {
    throw new Error('Args must be an array of strings.');
  }

  if (iterations <= 0) {
    throw new Error('Iterations must be a positive integer.');
  }

  const results = [];

  for (let i = 0; i < iterations; i++) {
    const startTime = performance.now();
    const startMemory = process.memoryUsage().heapUsed;

    try {
      const result = await new Promise((resolve, reject) => {
        const child = require('child_process').spawn(command, args, { stdio: 'inherit' });
        child.on('close', (code) => {
          if (code !== 0) {
            reject(new Error(`Command exited with code ${code}`));
          } else {
            resolve();
          }
        });
      });

      const endTime = performance.now();
      const endMemory = process.memoryUsage().heapUsed;

      results.push({
        duration: endTime - startTime,
        memory: endMemory - startMemory,
        result: result
      });
    } catch (err) {
      throw new Error(`Command failed in iteration ${i + 1}: ${err.message}`);
    }
  }

  return {
    duration: results.reduce((sum, r) => sum + r.duration, 0) / iterations,
    memory: results.reduce((sum, r) => sum + r.memory, 0) / iterations,
    result: results[0].result
  };
}

/**
 * Measures memory usage in bytes.
 * @returns {number} Memory usage in bytes.
 * @example
 * const memory = measureMemory();
 * console.log(`Memory used: ${memory} bytes`);
 */
export function measureMemory() {
  return process.memoryUsage().heapUsed;
}

/**
 * Measures the duration of a function call in milliseconds.
 * @param {Function} fn - The function to measure.
 * @param {...any} args - Arguments to pass to the function.
 * @returns {Promise<{duration: number, result: any}>} A promise that resolves to an object containing the duration and result.
 * @example
 * const result = await measureDuration(() => Promise.resolve(42));
 * console.log(result.duration, result.result);
 */
export async function measureDuration(fn, ...args) {
  if (typeof fn !== 'function') {
    throw new Error('First argument must be a function.');
  }

  const startTime = performance.now();
  const result = await fn(...args);
  const endTime = performance,now();

  return {
    duration: endTime - startTime,
    result
  };
}

/**
 * Generates a benchmark report from an array of benchmark results.
 * @param {Array<{duration: number, memory: number, result: any}>} results - Array of benchmark results.
 * @returns {Object} A benchmark report with aggregated metrics.
 * @example
 * const report = generateBenchmarkReport(results);
 * console.log(report);
 */
export function generateBenchmarkReport(results) {
  if (!Array.isArray(results)) {
    throw new Error('Results must be an array of benchmark result objects.');
  }

  if (results.length === 0) {
    throw new Error('Results array cannot be empty.');
  }

  const totalDuration = results.reduce((sum, r) => sum + r.duration, 0);
  const totalMemory = results.reduce((sum, r) => sum + r.memory, 0);

  return {
    totalDuration,
    averageDuration: totalDuration / results.length,
    totalMemory,
    averageMemory: totalMemory / results.length,
    results
  };
}