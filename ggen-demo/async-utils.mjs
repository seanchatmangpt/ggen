/**
 * Sleeps for the specified number of milliseconds
 * @param {number} ms - Number of milliseconds to sleep
 * @returns {Promise<void>} A promise that resolves after the delay
 * @throws {TypeError} If ms is not a number
 * @example
 * // Example usage of sleep
 * const delay = 1000;
 * await sleep(delay);
 * console.log(`Slept for ${delay} milliseconds`);
 */
export function sleep(ms) {
  if (typeof ms !== 'number') {
    throw new TypeError('ms must be a number');
  }
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Times out a promise after the specified number of milliseconds
 * @param {Promise<any>} promise - Promise to timeout
 * @param {number} ms - Timeout in milliseconds
 * @returns {Promise<any>} A promise that resolves with the original promise's result or rejects with a timeout error
 * @throws {TypeError} If promise is not a Promise or ms is not a number
 * @example
 * // Example usage of timeout
 * const fetchData = () => fetch('https://example.com/data').then(res => res.json());
 * const timeoutMs = 5000;
 * const result = await timeout(fetchData(), timeoutMs);
 * console.log('Data fetched:', result);
 */
export function timeout(promise, ms) {
  if (!(promise instanceof Promise)) {
    throw new TypeError('First argument must be a Promise');
  }
  if (typeof ms !== 'number') {
    throw new TypeError('ms must be a number');
  }

  return new Promise((resolve, reject) => {
    const timeoutId = setTimeout(() => {
      reject(new Error(`Timeout exceeded: ${ms}ms`));
    }, ms);

    promise.then(value => {
      clearTimeout(timeoutId);
      resolve(value);
    }).catch(reject);
  });
}

/**
 * Retries a function with exponential backoff
 * @param {Function} fn - Function to retry
 * @param {number} attempts - Max number of attempts
 * @param {number} [initialDelay=1000] - Initial delay in milliseconds
 * @returns {Promise<any>} Result of the function
 * @throws {Error} After max retries
 * @example
 * // Example usage of retry
 * const fetchData = () => fetch('https://example.com/data').then(res => res.json());
 * const result = await retry(fetchData, 3, 1000);
 * console.log('Data fetched:', result);
 */
export async function retry(fn, attempts, initialDelay = 1000) {
  if (typeof fn !== 'function') {
    throw new TypeError('fn must be a function');
  }
  if (typeof attempts !== 'number' || attempts <= 0) {
    throw new TypeError('attempts must be a positive number');
  }
  if (typeof initialDelay !== 'number' || initialDelay < 0) {
    throw new TypeError('initialDelay must be a non-negative number');
  }

  for (let attempt = 1; attempt <= attempts; attempt++) {
    try {
      return await fn();
    } catch (error) {
      if (attempt === attempts) {
        throw error;
      }
      await sleep(initialDelay * Math.pow(2, attempt - 1));
    }
  }
}