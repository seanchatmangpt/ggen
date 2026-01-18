/**
 * Executes a function with exponential backoff retry logic.
 * @param {Function} fn - The function to execute.
 * @param {number} maxRetries - The maximum number of retry attempts.
 * @param {number} delay - The initial delay in milliseconds before the first retry.
 * @returns {Promise<any>} A promise that resolves with the result of the function or rejects with the error after max retries.
 * @throws {Error} If the function throws an error and maxRetries is reached.
 * @example
 * const result = await retry(() => fetch('https://api.example.com/data'), 3, 1000);
 * // result === { data: 'success' }
 * @example
 * try {
 *   await retry(() => fetch('https://api.example.com/data'), 2, 1000);
 * } catch (e) {
 *   console.error('Final failure:', e.message);
 * }
 */
export async function retry(fn, maxRetries, delay) {
  if (typeof fn !== 'function') {
    throw new TypeError('First argument must be a function.');
  }
  if (typeof maxRetries !== 'number' || maxRetries < 0) {
    throw new TypeError('Second argument must be a non-negative number.');
  }
  if (typeof delay !== 'number' || delay < 0) {
    throw new TypeError('Third argument must be a non-negative number.');
  }

  let retries = 0;
  let currentDelay = delay;

  while (retries <= maxRetries) {
    try {
      return await fn();
    } catch (error) {
      if (retries === maxRetries) {
        throw error;
      }
      retries++;
      currentDelay *= 2;
      await new Promise(resolve => setTimeout(resolve, currentDelay));
    }
  }

  throw new Error('Unexpected error during retry logic.');
}