/**
 * Splits an array into chunks of a specified size.
 * @param {Array} array - The array to split.
 * @param {number} size - The size of each chunk. Must be a positive integer.
 * @returns {Array<Array>} An array of arrays, each containing elements from the original array.
 * @throws {Error} If the input is not an array or size is not a positive integer.
 * @example
 * const inputArray = [1, 2, 3, 4, 5];
 * const chunkSize = 2;
 * const result = chunk(inputArray, chunkSize);
 * // result is [[1, 2], [3, 4], [5]]
 */
export function chunk(array, size) {
  if (!Array.isArray(array)) {
    throw new Error('Input must be an array.');
  }
  if (typeof size !== 'number' || size <= 0) {
    throw new Error('Size must be a positive integer.');
  }

  const chunks = [];
  for (let i = 0; i < array.length; i += size) {
    chunks.push(array.slice(i, i + size));
  }
  return chunks;
}

/**
 * Returns a new array with duplicate values removed.
 * @param {Array} array - The array to deduplicate.
 * @returns {Array} A new array with unique elements.
 * @throws {Error} If the input is not an array.
 * @example
 * const inputArray = [1, 2, 2, 3, 3, 3];
 * const result = unique(inputArray);
 * // result is [1, 2, 3]
 */
export function unique(array) {
  if (!Array.isArray(array)) {
    throw new Error('Input must be an array.');
  }

  return Array.from(new Set(array));
}

/**
 * Flattens a nested array up to a specified depth.
 * @param {Array} array - The array to flatten.
 * @param {number} [depth=1] - The maximum depth to flatten (default is 1).
 * @returns {Array} A new array with elements from the original array, flattened up to the specified depth.
 * @throws {Error} If the input is not an array or depth is not a non-negative integer.
 * @example
 * const inputArray = [[1, 2], [3, [4, 5]]];
 * const flattenDepth = 2;
 * const result = flatten(inputArray, flattenDepth);
 * // result is [1, 2, 3, 4, 5]
 */
export function flatten(array, depth = 1) {
  if (!Array.isArray(array)) {
    throw new Error('Input must be an array.');
  }
  if (typeof depth !== 'number' || depth < 0) {
    throw new Error('Depth must be a non-negative integer.');
  }

  const result = [];

  function _flatten(arr, currentDepth) {
    for (const item of arr) {
      if (Array.isArray(item) && currentDepth > 0) {
        _flatten(item, currentDepth - 1);
      } else {
        result.push(item);
      }
    }
  }

  _flatten(array, depth);
  return result;
}