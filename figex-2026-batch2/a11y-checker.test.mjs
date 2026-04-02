import { describe, it, expect } from 'vitest'
import { checkWCAG, validateAria, testKeyboard } from './a11y-checker.mjs'

describe('a11y-checker - JSDoc Examples', () => {
  it('example 1', async () => {
    const heading = document.querySelector('h1')
    const result = checkWCAG(heading)
    console.log(result) // { level: 'A', violations: ['No text content'], fixes: ['Add text content to the element'] }
  })

  it('example 2', async () => {
    const ariaAttrs = { role: 'navigation', aria-label: 'Main navigation', aria-hidden: 'true' }
    const result = validateAria(ariaAttrs)
    console.log(result) // { level: 'A', violations: ['aria-hidden on non-interactive element'], fixes: ['Remove aria-hidden from non-interactive elements'] }
  })

  it('example 3', async () => {
    const button = document.querySelector('button')
    const result = testKeyboard(button)
    console.log(result) // { level: 'A', violations: ['No keyboard event handlers'], fixes: ['Add keyboard event handlers'] }
  })

})
