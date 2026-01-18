import { describe, it, expect } from 'vitest'
import { planMigration, findEquivalents, generateCodemod } from './migration-planner.mjs'

describe('migration-planner - JSDoc Examples', () => {
  it('example 1', async () => {
    await planMigration('Bootstrap', 'TailwindCSS')
  })

  it('example 2', async () => {
    const equivalents = await findEquivalents('Button', 'TailwindCSS')
    console.log(equivalents) // ['Button', 'CallToAction']
  })

  it('example 3', async () => {
    const mapping = {
      'Button': 'CallToAction',
      'Input': 'Textfield'
    };
    const codemod = generateCodemod(mapping);
    console.log(codemod);
  })

})
