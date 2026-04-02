/**
 * Converts the first character of a string to uppercase and the rest to lowercase.
 * @param {string} input - The input string to capitalize.
 * @returns {string} The capitalized string.
 * @throws {TypeError} If the input is not a string.
 * @example
 * const input = "hello world";
 * const result = capitalize(input);
 * console.log(result); // Output: "Hello world"
 */
export function capitalize(input) {
  if (typeof input !== 'string') {
    throw new TypeError('Input must be a string');
  }
  return input.charAt(0).toUpperCase() + input.slice(1).toLowerCase();
}

/**
 * Truncates a string to a maximum length, appending an ellipsis if truncated.
 * @param {string} input - The input string to truncate.
 * @param {number} maxLength - The maximum length of the output string.
 * @returns {string} The truncated string with an ell.
 * @throws {TypeError} If the input is not a string or max length is not a number.
 * @example
 * const input = "This is a long string";
 * const maxLength = 10;
 * const result = truncate(input, maxLength);
 * console.log(result); // Output: "This is a..."
 */
export function truncate(input, maxLength) {
  if (typeof input !== 'string') {
    throw new TypeError('Input must be a string');
  }
  if (typeof maxLength !== 'number' || maxLength <= 0) {
    throw new TypeError('Max length must be a positive number');
  }
  if (input.length <= maxLength) {
    return input;
  }
  return input.slice(0, maxLength) + '...';
}

/**
 * Converts a string to a URL-friendly slug by removing special characters and spaces.
 * @param {string} input - The input string to slugify.
 * @returns {string} The slugified string.
 * @throws {TypeError} If the input is not a string.
 * @example
 * const input = "Hello World!";
 * const result = slugify(input);
 * console.log(result); // Output: "hello-world"
 */
export function slugify(input) {
  if (typeof input !== 'string') {
    throw new TypeError('Input must be a string');
  }
  return input
    .toLowerCase()
    .replace(/[^a-z0-9\s-]/g, '') // Remove non-alphanumeric and non-space characters
    .replace(/[\s-]+/g, '-')      // Replace spaces and hyphens with a single hyphen
    .replace(/^-|-$/g, '');       // Remove leading and trailing hyphens
}