import { describe, it, expect } from 'vitest'
import { toJSON, toCSV, toHTML } from './export-helpers.mjs'

describe('export-helpers - JSDoc Examples', () => {
  it('example 1', async () => {
    const result = toJSON({ name: "John", age: 30 });
    console.log(result); // '{"name":"John","age":30}'
  })

  it('example 2', async () => {
    const csvData = [{ name: "John", age: 30 }, { name: "Jane", age: 25 }];
    const csv = toCSV(csvData);
    console.log(csv); // 'name,age\nJohn,30\nJane,25'
  })

  it('example 3', async () => {
    const htmlContent = ['<h1>Hello</h1>', '<p>World</p>'];
    const html = toHTML(htmlContent);
    console.log(html); // '<h1>Hello</h1><p>World</p>'
  })

})
