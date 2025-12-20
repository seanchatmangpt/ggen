/**
 * Encodes a string using Base64 encoding.
 * @param {string} str - The string to encode.
 * @returns {string} The Base64 encoded string.
 * @example
 * const encoded = encode('hello');
 * // encoded === 'aGVsbG8='
 */
function encode(str) {
  return btoa(str);
}

/**
 * Decodes a Base64 encoded string back to its original form.
 * @param {string} str - The Base64 encoded string to decode.
 * @returns {string} The original string.
 * @example
 * const decoded = decode('aGVsbG8=');
 * // decoded === 'hello'
 */
function decode(str) {
  return atob(str);
}