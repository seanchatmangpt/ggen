/**
 * Watches a file system path for changes and triggers a callback when a change is detected.
 * @param {string} path - The file system path to watch (absolute path).
 * @param {function} callback - Function to call when a change is detected.
 * @returns {Object} An object with an `onFileChange` method to register handlers.
 * @throws {Error} If the path is invalid or not a string.
 * @example
 * // Example 1: Basic usage
 * const fileWatcher = watch('/path/to/file.txt', (event, filename) => {
 *   console.log(`File ${filename} was ${event}`);
 * });
 *
 * fileWatcher.onFileChange((event, filename) => {
 *   console
 *     .log(`File ${filename} was ${event}`);
 * });
 *
 * @example
 * // Example 2: Custom handler and callback
 * const fileWatcher = watch('/path/to/another.txt', (event, filename) => {
 *   console.log(`Main callback: File ${filename} was ${event}`);
 * });
 *
 * fileWatcher.onFileChange((event, filename) => {
 *   console.log(`Custom handler: File ${filename} was ${event}`);
 * });
 */
export function watch(path, callback) {
  if (typeof path !== 'string' || !path.trim()) {
    throw new Error('Invalid path: must be a non-empty string.');
  }

  if (typeof callback !== 'function') {
    throw new Error('Callback must be a function.');
  }

  const handlers = [];

  return {
    /**
     * Registers a handler function to be called when a file change is detected.
     * @param {function} handler - The handler function to register.
     * @throws {Error} If the handler is not a function.
     */
    onFileChange(handler) {
      if (typeof handler !== 'function') {
        throw new Error('Handler must be a function.');
      }
      handlers.push(handler);
    },

    /**
     * Triggers all registered handlers and the main callback with the event and filename.
     * @param {string} event - The type of event (e.g., 'created', 'modified', 'deleted').
     * @param {string} filename - The name of the file that changed.
     */
    trigger(event, filename) {
      handlers.forEach(handler => handler(event, filename));
      if (callback) callback(event, filename);
    }
  };
}