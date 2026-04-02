/**
 * Flattens an array recursively up to a specified depth.
 * @param {Array} array - The array to flatten.
 * @param {number} [depth=1] - The maximum depth to flatten. Defaults to 1.
 * @returns {Array} A new array containing all elements from the original array, flattened up to the specified depth.
 * @throws {TypeError} If the input is not an array.
 * @example
 * const result = flatten([1, [2, [3, 4]]]);
 * // result === [1, 2, 3, 4]
 * @example
 * const result = flatten([1, [2, [3, [4]]]], 2);
 * // result === [1, 2, 3, 4]
 */
function flatten(array, depth = 1) {
  if (!Array.isArray(array)) {
    throw new TypeError('Input must be an array');
  }
  return array.reduce((acc, item) => {
    if (Array.isArray(item) && depth > 0) {
      acc.push(...flatten(item, depth - 1));
    } else {
      acc.push(item);
    }
    return acc;
  }, []);
}