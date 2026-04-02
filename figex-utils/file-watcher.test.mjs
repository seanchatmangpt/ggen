import { describe, it, expect } from 'vitest'
import { watch } from './file-watcher.mjs'

describe('file-watcher - JSDoc Examples', () => {
  it('example 1', async () => {
    // Example 1: Basic usage
    const fileWatcher = watch('/path/to/file.txt', (event, filename) => {
      console.log(`File ${filename} was ${event}`);
    });
    
    fileWatcher.onFileChange((event, filename) => {
      console
        .log(`File ${filename} was ${event}`);
    });
  })

  it('example 2', async () => {
    // Example 2: Custom handler and callback
    const fileWatcher = watch('/path/to/another.txt', (event, filename) => {
      console.log(`Main callback: File ${filename} was ${event}`);
    });
    
    fileWatcher.onFileChange((event, filename) => {
      console.log(`Custom handler: File ${filename} was ${event}`);
    });
  })

})
