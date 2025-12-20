import { describe, it, expect } from 'vitest'
import { add, multiply, max } from './math-utils.mjs'

describe('math-utils - JSDoc Examples', () => {
  it('example 1', async () => {
    const result = add(5, 3)
    // result === 8
  })

  it('example 2', async () => {
    const result = multiply(4, 5)
    // result === 20
  })

  it('example 3', async () => {
    const result = max([1, 2, 3, 4, 5])
    // result === 5
  })

})
