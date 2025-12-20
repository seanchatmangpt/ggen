/**
 * Measures the execution time of a function and returns the result.
 * @param {Function} fn - Function to measure
 * @param {...*} args - Arguments to pass to the function
 * @returns {Promise<{ duration: number, result: any }>} A promise that resolves to an object with duration (ms) and result
 * @throws {Error} If the function throws an error
 * @example
 * const result = await measureTime(async () => {
 *   await new Promise(resolve => setTimeout(resolve, 100));
 *   return 'Done';
 * });
 * console.log(`Duration: ${result.duration}ms, Result: ${result.result}`);
 */
export async function measureTime(fn, ...args) {
  if (typeof fn !== 'function') {
    throw new TypeError('First argument must be a function');
  }

  const start = performance.now();
  try {
    const result = await fn(...args);
    const duration = performance.now() - start;
    return { duration, result };
  } catch (error) {
    const duration = performance.now() - start;
    throw new Error(`Function failed after ${duration.toFixed(2)}ms: ${error.message}`);
  }
}

/**
 * Tracks memory usage before and after a function call.
 * @param {Function} fn - Function to track memory usage for
 * @param {...*} args - Arguments to pass to the function
 * @returns {Promise<{ before: number, after: number, result: any }>} A promise that resolves to an object with memory usage before and after (in bytes), and the result
 * @throws {Error} If the function throws an error
 * @example
 * const result = await trackMemory(async () => {
 *   const arr = new Array(1000000).fill(1);
 *   return arr.length;
 * });
 * console.log(`Memory before: ${result.before}B, Memory after: ${result.after}B, Result: ${result.result}`);
 */
export async function trackMemory(fn, ...args) {
  if (typeof fn !== 'function') {
    throw new TypeError('First argument must be a function');
  }

  const before = process.memoryUsage().heapUsed;
  try {
    const result = await fn(...args);
    const after = process.memoryUsage().heapUsed;
    return { before, after, result };
  } catch (error) {
    const after = process.memoryUsage().heapUsed;
    throw new Error(`Function failed with memory usage: Before: ${before}B, After: ${after}B. Error: ${error.message}`);
  }
}

/**
 * Logs performance metrics to the console.
 * @param {Object} metrics - An object containing performance metrics
 * @param {number} metrics.duration - Duration in milliseconds
 * @param {number} metrics.memoryBefore - Memory usage before function call (in bytes)
 * @param {number} metrics.memoryAfter - Memory usage after function call (in bytes)
 * @param {any} metrics.result - Result of the function call
 * @param {string} [metrics.label='Performance Metrics'] - Optional label for the log
 * @returns {void}
 * @example
 * const metrics = {
 *   duration: 123,
 *   memoryBefore: 1024,
 *   memoryAfter: 2048,
 *   result: 'Success'
 * };
 * logResults(metrics);
 * // Output: [Performance Metrics] Duration: 123ms, Memory Before: 1024B, Memory After: 2048B, Result: Success
 */
export function logResults(metrics, label = 'Performance Metrics') {
  if (typeof metrics !== 'object' || metrics === null) {
    throw new TypeError('First argument must be an object');
  }

  const { duration, memoryBefore, memoryAfter, result } = metrics;

  if (typeof duration !== 'number' || typeof memoryBefore !== 'number' || typeof memoryAfter !== 'number' || typeof result === 'undefined') {
    throw new TypeError('Metrics object must contain duration, memoryBefore, memoryAfter, and result');
  }

  console.log(`[${label}] Duration: ${duration}ms, Memory Before: ${memoryBefore}B, Memory After: ${memoryAfter}B, Result: ${result}`);
}