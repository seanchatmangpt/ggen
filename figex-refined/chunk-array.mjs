/**
 * Splits an array into chunks of a specified size.
 * @param {Array} array - The array to be split into chunks.
 * @param {number} size - The size of each chunk. Must be a positive integer.
 * @returns {Array<Array>} A new array where each element is a chunk of the original array.
 * @throws {TypeError} If the input is not an array.
 * @throws {RangeError} If the size is less than or equal to zero.
 * @example
 * const result = chunk([1, 2, 3, 4, 5], 2);
 * // result === [[1, 2], [3, 4], [5]]
 */
function chunk(array, size) {
  if (!Array.isArray(array)) {
    throw new TypeError('Input must be an array.');
  }
  if (size <= 0) {
    throw new RangeError('Size must be a positive integer.');
  }
  const result = [];
  for (let i = 0; i < array.length; i += size) {
    result.push(array.slice(i, i + size));
  }
  return result;
}

export { chunk };