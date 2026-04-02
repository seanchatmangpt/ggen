import { describe, it, expect } from 'vitest'
import { CircuitBreaker } from './circuit-breaker.mjs'

describe('circuit-breaker - JSDoc Examples', () => {
  it('example 1', async () => {
    const breaker = new CircuitBreaker({ failureThreshold: 3, resetTimeout: 5000 });
    const result = await breaker.call(() => fetch('https://api.example.com/data'));
    console.log(result);
  })

})
