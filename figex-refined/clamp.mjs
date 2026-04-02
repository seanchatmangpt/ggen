/**
 * Clamps a number between a minimum and maximum value.
 * @param {number} num - The number to clamp.
 * @param {number} min - The minimum value.
 * @param {number} max - The maximum value.
 * @returns {number} The clamped number.
 * @example
 * const result = clamp(5, 1, 10);
 * // result === 5
 */
function clamp(num, min, max) {
  return Math.min(Math.max(num, min), max);
}

/**
 * Checks if a number is within a specified range (inclusive).
 * @param {number} num - The number to check.
 * @param {number} min - The minimum value of the range.
 * @param {number} max - The maximum value of the range.
 * @returns {boolean} True if the number is within the range, false otherwise.
 * @example
 * const result = inRange(5, 1, 10);
 * // result === true
 */
function inRange(num, min, max) {
  return num >= min && num <= max;
}

export { clamp, inRange };