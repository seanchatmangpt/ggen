/**
 * Debounce function that executes a function after a delay, canceling previous calls.
 * @param {Function} fn - The function to be debounced.
 * @param {number} delay - The delay in milliseconds.
 * @returns {Function} A debounced function.
 * @example
 * const debouncedLog = debounce(console.log, 300);
 * debouncedLog('Hello'); // Logs 'Hello' after 300ms
 */
export function debounce(fn, delay) {
  let timer;
  return function(...args) {
    clearTimeout(timer);
    timer = setTimeout(() => fn.apply(this, args), delay);
  };
}

/**
 * Throttle function that limits the rate at which a function is executed.
 * @param {Function} fn - The function to be throttled.
 * @param {number} delay - The delay in milliseconds.
 * @returns {Function} A throttled function.
 * @example
 * const throttledLog = throttle(console.log, 1000);
 * throttledLog('Hello'); // Logs 'Hello' once every 1000ms
 */
export function throttle(fn, delay) {
  let lastCall = 0;
  return function(...args) {
    const now = Date.now();
    if (now - lastCall >= delay) {
      fn.apply(this, args);
      lastCall = now;
    }
  };
}