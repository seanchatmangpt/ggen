/**
 * Formats a number of bytes into a human-readable string with appropriate units.
 * @param {number} bytes - The number of bytes to format.
 * @returns {string} A string representing the formatted byte value with unit.
 * @example
 * const result = formatBytes(1024);
 * // result === '1 KB'
 */
function formatBytes(bytes) {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const dm = 3;
  const sizes = ['B', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + ' ' + sizes[i];
}

export { formatBytes };