import { describe, it, expect } from 'vitest'
import { measureTime, trackMemory, logResults } from './benchmark-utils.mjs'

describe('benchmark-utils - JSDoc Examples', () => {
  it('example 1', async () => {
    const result = await measureTime(async () => {
      await new Promise(resolve => setTimeout(resolve, 100));
      return 'Done';
    });
    console.log(`Duration: ${result.duration}ms, Result: ${result.result}`);
  })

  it('example 2', async () => {
    const result = await trackMemory(async () => {
      const arr = new Array(1000000).fill(1);
      return arr.length;
    });
    console.log(`Memory before: ${result.before}B, Memory after: ${result.after}B, Result: ${result.result}`);
  })

  it('example 3', async () => {
    const metrics = {
      duration: 123,
      memoryBefore: 1024,
      memoryAfter: 2048,
      result: 'Success'
    };
    logResults(metrics);
    // Output: [Performance Metrics] Duration: 123ms, Memory Before: 1024B, Memory After: 2048B, Result: Success
  })

})
