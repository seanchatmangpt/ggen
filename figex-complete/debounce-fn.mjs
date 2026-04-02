/**
 * Debounce function that executes a callback after a delay, ignoring subsequent calls.
 * @param {Function} fn - The function to be debounced.
 * @param {number} delay - The delay in milliseconds before the function is executed.
 * @returns {Function} A debounced function that delays execution of the provided function.
 * @throws {Error} If `fn` is not a function or `delay` is not a positive number.
 * @example
 * const debouncedLog = debounce(console.log, 300)
 * debouncedLog('First') // No output immediately
 * debouncedLog('Second') // No output immediately
 * debouncedLog('Third') // No output immediately
 * setTimeout(() => { debouncedLog('Final') }, 400) // 'Final' is logged after 300ms
 * @example
 * try { debounce('not a function', 300) } catch (e) { console.error(e.message) } // Throws "fn must be a function"
 */
export function debounce(fn, delay) {
  if (typeof fn !== 'function') {
    throw new Error('fn must be a function')
  }
  if (typeof delay !== 'number' || delay <= 0) {
    throw new Error('delay must be a positive number')
  }

  let timer

  return function debounced(...args) {
    clearTimeout(timer)
    timer = setTimeout(() => {
      fn.apply(this, args)
    }, delay)
  }
}