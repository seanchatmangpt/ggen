/**
 * Splits an array into chunks of a specified size.
 * @param {Array} arr - The array to chunk.
 * @param {number} size - The size of each chunk.
 * @returns {Array<Array>} An array of arrays, each containing elements from the original array.
 * @throws {Error} If `arr` is not an array or `size` is not a positive integer.
 * @example
 * const result = chunk([1,2,3,4,5], 2)
 * // result is [[1,2], [3,4], [5]]
 */
export function chunk(arr, size) {
  if (!Array.isArray(arr)) {
    throw new Error('First argument must be an array.');
  }
  if (typeof size !== 'number' || size <= 0 || !Number.isInteger(size)) {
    throw new Error('Second argument must be a positive integer.');
  }

  const chunks = [];
  for (let i = 0; i < arr.length; i += size) {
    chunks.push(arr.slice(i, i + size));
  }
  return chunks;
}

/**
 * Returns the first element of an array.
 * @param {Array} arr - The array to get the first element from.
 * @returns {*} The first element of the array, or `undefined` if the array is empty.
 * @throws {Error} If `arr` is not an array.
 * @example
 * const result = first([1,2,3])
 * // result is 1
 */
export function first(arr) {
  if (!Array.isArray(arr)) {
    throw new Error('First argument must be an array.');
  }
  return arr.length > 0 ? arr[0] : undefined;
}

/**
 * Returns the last element of an array.
 * @param {Array} arr - The array to get the last element from.
 * @returns {*} The last element of the array, or `undefined` if the array is empty.
 * @throws {Error} If `arr` is not an array.
 * @example
 * const result = last([1,2,3])
 * // result is 3
 */
export function last(arr) {
  if (!Array.isArray(arr)) {
    throw new Error('First argument must be an array.');
  }
  return arr.length > 0 ? arr[arr.length - 1] : undefined;
}