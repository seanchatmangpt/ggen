/**
 * Registers a plugin with the system.
 * @param {string} name - The name of the plugin.
 * @param {Function} handler - The handler function that the plugin will execute.
 * @throws {Error} If name is empty or handler is not a function.
 * @example
 * // Register a logger plugin
 * registerPlugin('logger', (message) => {
 *   console.log(`[Logger] ${message}`);
 * });
 */
export function registerPlugin(name, handler) {
  if (!name || typeof name !== 'string') {
    throw new Error('Plugin name must be a non-empty string.');
  }
  if (typeof handler !== 'function') {
    throw new Error('Handler must be a function.');
  }

  // Use WeakMap to store plugins to avoid memory leaks
  const pluginMap = globalThis.__plugins__ || new WeakMap();
  pluginMap.set(name, handler);

  // Save to the global object
  globalThis.__plugins__ = pluginMap;
}

/**
 * Retrieves a plugin by its name.
 * @param {string} name - The name of the plugin.
 * @returns {Function|undefined} The handler function if the plugin exists, otherwise undefined.
 * @throws {Error} If name is empty.
 * @example
 * // Get the logger plugin and use it
 * const logger = getPlugin('logger');
 * if (logger) {
 *   logger('Hello, world!');
 * }
 */
export function getPlugin(name) {
  if (!name || typeof name !== 'string') {
    throw new Error('Plugin name must be a non-empty string.');
  }

  const pluginMap = globalThis.__plugins__;
  if (!pluginMap) {
    return undefined;
  }

  return pluginMap.get(name);
}

/**
 * Lists all registered plugins.
 * @returns {string[]} An array of plugin names.
 * @example
 * // List all plugins
 * const plugins = listPlugins();
 * console.log(plugins); // ['logger', 'mailer']
 */
export function listPlugins() {
  const pluginMap = globalThis.__plugins__;
  if (!pluginMap) {
    return [];
  }

  return Array.from(pluginMap.keys());
}