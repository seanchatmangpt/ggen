/**
 * Deep merges two objects, recursively merging nested objects and concatenating arrays.
 * @param {Object} obj1 - The first object to merge.
 * @param {Object} obj2 - The second object to merge. Values from obj2 will overwrite obj1 where keys match.
 * @returns {Object} A new object that is the deep merge of obj1 and obj2.
 * @throws {Error} If either obj1 or obj2 is not an object.
 * @example
 * const obj1 = { a: 1, b: { c: 2, d: [3, 4] } };
 * const obj2 = { b: { d: [5, 6], e: 7 }, f: 8 };
 * const result = deepMerge(obj1, obj2);
 * // result === { a: 1, b: { c: 2, d: [3, 4, 5, 6], e: 7 }, f: 8 }
 * @example
 * try { deepMerge(123, { a: 1 }) } catch (e) { console.error(e.message); }
 */
export function deepMerge(obj1, obj2) {
  if (typeof obj1 !== 'object' || obj1 === null || typeof obj2 !== 'object' || obj2 === null) {
    throw new Error('Both arguments must be objects.');
  }

  const merged = { ...obj1 };

  for (const key in obj2) {
    if (obj2.hasOwnProperty(key)) {
      const val1 = merged[key];
      const val2 = obj2[key];

      if (val1 === undefined) {
        merged[key] = val2;
      } else if (typeof val1 === 'object' && typeof val2 === 'object' && val1 !== null && val2 !== null) {
        merged[key] = deepMerge(val1, val2);
      } else if (Array.isArray(val1) && Array.isArray(val2)) {
        merged[key] = val1.concat(val2);
      } else {
        merged[key] = val2;
      }
    }
  }

  return merged;
}