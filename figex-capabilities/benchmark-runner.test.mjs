import { describe, it, expect } from 'vitest'
import { benchmarkCommand, measureMemory, measureDuration, generateBenchmarkReport } from './benchmark-runner.mjs'

describe('benchmark-runner - JSDoc Examples', () => {
  it('example 1', async () => {
    const result = await benchmarkCommand('node', ['script.js'], 5);
    console.log(result.duration, result.memory, result.result);
  })

  it('example 2', async () => {
    const memory = measureMemory();
    console.log(`Memory used: ${memory} bytes`);
  })

  it('example 3', async () => {
    const result = await measureDuration(() => Promise.resolve(42));
    console.log(result.duration, result.result);
  })

  it('example 4', async () => {
    const report = generateBenchmarkReport(results);
    console.log(report);
  })

})
