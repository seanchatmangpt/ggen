/**
 * Deep clones an object, making a copy of all nested properties.
 * @param {Object} obj - The object to clone.
 * @returns {Object} A deep clone of the input object.
 * @throws {Error} If the input is not an object.
 * @example
 * const original = { a: 1, b: { c: 2 } };
 * const clone = deepClone(original);
 * console.log(clone); // { a: 1, b: { c: 2 } }
 */
function deepClone(obj) {
  if (typeof obj !== 'object' || obj === null) {
    throw new Error('Input must be an object');
  }
  return JSON.parse(JSON.stringify(obj));
}

/**
 * Deep merges two objects, with the second object's properties taking precedence.
 * @param {Object} obj1 - The first object to merge.
 * @param {Object} obj2 - The second object to merge, whose properties take precedence.
 * @returns {Object} A new object that is the result of merging obj1 and obj2.
 * @throws {Error} If either input is not an object.
 * @example
 * const obj1 = { a: 1, b: { c: 2 } };
 * const obj2 = { b: { d: 3 }, e: 4 };
 * const merged = deepMerge(obj1, obj2);
 * console.log(merged); // { a: 1, b: { d: 3 }, e: 4 }
 */
function deepMerge(obj1, obj2) {
  if (typeof obj1 !== 'object' || obj1 === null || typeof obj2 !== 'object' || obj2 === null) {
    throw new Error('Both inputs must be objects');
  }
  const result = { ...obj1 };
  for (const key in obj2) {
    if (obj2.hasOwnProperty(key)) {
      result[key] = obj2[key];
    }
  }
  return result;
}