import { describe, it, expect } from 'vitest'
import { chunk, unique, flatten } from './array-utils.mjs'

describe('array-utils - JSDoc Examples', () => {
  it('example 1', async () => {
    const inputArray = [1, 2, 3, 4, 5];
    const chunkSize = 2;
    const result = chunk(inputArray, chunkSize);
    // result is [[1, 2], [3, 4], [5]]
  })

  it('example 2', async () => {
    const inputArray = [1, 2, 2, 3, 3, 3];
    const result = unique(inputArray);
    // result is [1, 2, 3]
  })

  it('example 3', async () => {
    const inputArray = [[1, 2], [3, [4, 5]]];
    const flattenDepth = 2;
    const result = flatten(inputArray, flattenDepth);
    // result is [1, 2, 3, 4, 5]
  })

})
