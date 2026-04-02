import { describe, it, expect } from 'vitest'
import * as mod from './retry-util.mjs'

describe('retry-util - JSDoc Examples', () => {
  it('example 1: with', () => {
    const result = await retry(() => fetch('https://api.example.com/data'), 3, 1000);
    expect(result).toBe({ data: 'success' })
  })

})
