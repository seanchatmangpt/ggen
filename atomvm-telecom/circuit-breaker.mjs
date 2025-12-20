/**
 * A Circuit Breaker pattern implementation to prevent cascading failures
 * @class
 * @param {Object} options - Configuration options
 * @param {number} options.failureThreshold - Number of failures before opening the circuit
 * @param {number} options.resetTimeout - Timeout in ms before attempting to reset the circuit
 * @param {string} [options.state='closed'] - Initial state of the circuit breaker
 * @throws {Error} If failureThreshold or resetTimeout is not a positive number
 */
export class CircuitBreaker {
  /**
   * Creates an instance of CircuitBreaker
   * @constructor
   * @param {Object} options - Configuration options
   * @param {number} options.failureThreshold - Number of failures before opening the circuit
   * @param {number} options.resetTimeout - Timeout in ms before attempting to reset the circuit
   * @param {string} [options.state='closed'] - Initial state of the circuit breaker
   * @throws {Error} If failureThreshold or reset,Timeout is not a positive number
   */
  constructor(options) {
    if (!options || typeof options !== 'object') {
      throw new Error('Options must be an object');
    }

    if (typeof options.failureThreshold !== 'number' || options.failureThreshold <= 0) {
      throw new Error('failureThreshold must be a positive number');
    }

    if (typeof options.resetTimeout !== 'number' || options.resetTimeout <= 0) {
      throw new Error('resetTimeout must be a positive number');
    }

    this.failureThreshold = options.failureThreshold;
    this.resetTimeout = options.resetTimeout;
    this.state = options.state || 'closed';
    this.failureCount = 0;
    this.lastFailureTime = 0;
  }

  /**
   * Calls the provided function and manages circuit state
   * @param {Function} fn - Function to call
   * @returns {Promise<any>} Result of the function call
   * @throws {Error} If circuit is open or if the function throws an error
   * @example
   * const breaker = new CircuitBreaker({ failureThreshold: 3, resetTimeout: 5000 });
   * const result = await breaker.call(() => fetch('https://api.example.com/data'));
   * console.log(result);
   */
  async call(fn) {
    if (this.state === 'open') {
      throw new Error('Circuit is open, cannot call function');
    }

    try {
      const result = await fn();
      this.failureCount = 0;
      this.state = 'closed';
      return result;
    } catch (error) {
      this.failureCount++;
      this.lastFailureTime = Date.now();

      if (this.state === 'closed' && this.failureCount >= this.failureThreshold) {
        this.state = 'open';
        throw new Error('Circuit opened due to failure threshold');
      }

      if (this.state === 'open') {
        throw new Error('Circuit is open, cannot call function');
      }

      throw error;
    }
  }

  /**
   * Checks if the circuit can be closed based on the reset timeout
   * @returns {boolean} True if circuit can be closed, false otherwise
   */
  canClose() {
    return this.state === 'open' && Date.now() - this.lastFailureTime >= this.resetTimeout;
  }

  /**
   * Manually closes the circuit
   * @returns {void}
   */
  close() {
    this.state = 'closed';
  }

  /**
   * Returns the current state of the circuit
   * @returns {string} Current state ('closed', 'open', or 'half-open')
   */
  getState() {
    return this.state;
  }

  /**
   * Returns the number of failures recorded
   * @returns {number} Failure count
   */
  getFailureCount() {
    return this.failureCount;
  }
}