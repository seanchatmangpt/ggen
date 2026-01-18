import { describe, it, expect } from 'vitest'
import { registerPlugin, getPlugin, listPlugins } from './plugin-registry.mjs'

describe('plugin-registry - JSDoc Examples', () => {
  it('example 1', async () => {
    // Register a logger plugin
    registerPlugin('logger', (message) => {
      console.log(`[Logger] ${message}`);
    });
  })

  it('example 2', async () => {
    // Get the logger plugin and use it
    const logger = getPlugin('logger');
    if (logger) {
      logger('Hello, world!');
    }
  })

  it('example 3', async () => {
    // List all plugins
    const plugins = listPlugins();
    console.log(plugins); // ['logger', 'mailer']
  })

})
