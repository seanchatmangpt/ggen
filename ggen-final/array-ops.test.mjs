import { describe, it, expect } from 'vitest'
import { chunk, first, last } from './array-ops.mjs'

describe('array-ops - JSDoc Examples', () => {
  it('example 1', async () => {
    const result = chunk([1,2,3,4,5], 2)
    // result is [[1,2], [3,4], [5]]
  })

  it('example 2', async () => {
    const result = first([1,2,3])
    // result is 1
  })

  it('example 3', async () => {
    const result = last([1,2,3])
    // result is 3
  })

})
