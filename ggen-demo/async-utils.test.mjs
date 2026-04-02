import { describe, it, expect } from 'vitest'
import { sleep, timeout, retry } from './async-utils.mjs'

describe('async-utils - JSDoc Examples', () => {
  it('example 1', async () => {
    // Example usage of sleep
    const delay = 1000;
    await sleep(delay);
    console.log(`Slept for ${delay} milliseconds`);
  })

  it('example 2', async () => {
    // Example usage of timeout
    const fetchData = () => fetch('https://example.com/data').then(res => res.json());
    const timeoutMs = 5000;
    const result = await timeout(fetchData(), timeoutMs);
    console.log('Data fetched:', result);
  })

  it('example 3', async () => {
    // Example usage of retry
    const fetchData = () => fetch('https://example.com/data').then(res => res.json());
    const result = await retry(fetchData, 3, 1000);
    console.log('Data fetched:', result);
  })

})
