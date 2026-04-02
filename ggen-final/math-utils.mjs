/**
 * Adds two numbers
 * @param {number} a - First number
 * @param {number} b - Second number
 * @returns {number} Sum of a and b
 * @throws {TypeError} If either a or b is not a number
 * @example
 * const result = add(5, 3)
 * // result === 8
 */
export function add(a, b) {
  if (typeof a !== 'number' || typeof b !== 'number') {
    throw new TypeError('Both arguments must be numbers')
  }
  return a + b
}

/**
 * Multiplies two numbers
 * @param {number} a - First number
 * @param {number} b - Second number
 * @returns {number} Product of a and b
 * @throws {TypeError} If either a or b is not a number
 * @example
 * const result = multiply(4, 5)
 * // result === 20
 */
export function multiply(a, b) {
  if (typeof a !== 'number' || typeof b !== 'number') {
    throw new TypeError('Both arguments must be numbers')
  }
  return a * b
}

/**
 * Finds the maximum value in an array
 * @param {number[]} arr - Array of numbers
 * @returns {number} Maximum value in the array
 * @throws {TypeError} If input is not an array
 * @throws {Error} If array is empty
 * @example
 * const result = max([1, 2, 3, 4, 5])
 * // result === 5
 */
export function max(arr) {
  if (!Array.isArray(arr)) {
    throw new TypeError('Input must be an array')
  }
  if (arr.length === 0) {
    throw new Error('Array cannot be empty')
  }
  return Math.max(...arr)
}