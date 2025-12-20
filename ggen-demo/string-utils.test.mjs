import { describe, it, expect } from 'vitest'
import { capitalize, truncate, slugify } from './string-utils.mjs'

describe('string-utils - JSDoc Examples', () => {
  it('example 1', async () => {
    const input = "hello world";
    const result = capitalize(input);
    console.log(result); // Output: "Hello world"
  })

  it('example 2', async () => {
    const input = "This is a long string";
    const maxLength = 10;
    const result = truncate(input, maxLength);
    console.log(result); // Output: "This is a..."
  })

  it('example 3', async () => {
    const input = "Hello World!";
    const result = slugify(input);
    console.log(result); // Output: "hello-world"
  })

})
