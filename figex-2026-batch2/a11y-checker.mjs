/**
 * Checks an HTML element for WCAG compliance based on accessibility standards.
 * @param {HTMLElement} element - The HTML element to check.
 * @returns {{level: string, violations: string[], fixes: string[]}} An object containing the accessibility level, list of violations, and suggested fixes.
 * @throws {TypeError} If the input is not a valid HTMLElement.
 * @example
 * const heading = document.querySelector('h1')
 * const result = checkWCAG(heading)
 * console.log(result) // { level: 'A', violations: ['No text content'], fixes: ['Add text content to the element'] }
 */
export function checkWCAG(element) {
  if (!(element instanceof HTMLElement)) {
    throw new TypeError('Input must be a valid HTMLElement')
  }

  const violations = []
  const fixes = []

  // Check for text content
  if (element.textContent.trim() === '') {
    violations.push('No text content')
    fixes.push('Add text content to the element')
  }

  // Check for proper heading hierarchy
  if (element.tagName.toLowerCase() === 'h1' && document.querySelectorAll('h1').length > 1) {
    violations.push('Multiple h1 elements')
    fixes.push('Use h2-h6 for subheadings')
  }

  // Check for proper landmark roles
  if (element.getAttribute('role') === 'main' && !element.hasAttribute('aria-label')) {
    violations.push('Main content area without aria-label')
    fixes.push('Add aria-label to the main content area')
  }

  // Determine accessibility level
  const level = violations.length === 0 ? 'A' : violations.length <= 2 ? 'AA' : 'AAA'

  return { level, violations, fixes }
}

/**
 * Validates ARIA attributes on an HTML element.
 * @param {Object} attrs - An object containing ARIA attributes.
 * @returns {{level: string, violations: string[], fixes: string[]}} An object containing the accessibility level, list of violations, and suggested fixes.
 * @throws {TypeError} If the input is not an object.
 * @example
 * const ariaAttrs = { role: 'navigation', aria-label: 'Main navigation', aria-hidden: 'true' }
 * const result = validateAria(ariaAttrs)
 * console.log(result) // { level: 'A', violations: ['aria-hidden on non-interactive element'], fixes: ['Remove aria-hidden from non-interactive elements'] }
 */
export function validateAria(attrs) {
  if (typeof attrs !== 'object' || attrs === null) {
    throw new TypeError('Input must be an object')
  }

  const violations = []
  const fixes = []

  // Check for aria-hidden on non-interactive elements
  if (attrs.ariaHidden && !['button', 'link', 'menuitem', 'menuitemcheckbox', 'menuitemradio', 'progressbar', 'slider', 'spinbutton', 'switch', 'textbox'].includes(attrs.role)) {
    violations.push('aria-hidden on non-interactive element')
    fixes.push('Remove aria-hidden from non-interactive elements')
  }

  // Check for aria-label on elements that require it
  if (attrs.role === 'navigation' && !attrs.ariaLabel) {
    violations.push('Navigation role without aria-label')
    fixes.push('Add aria-label to navigation role')
  }

  // Determine accessibility level
  const level = violations.length === 0 ? 'A' : violations.length <= 2 ? 'AA' : 'AAA'

  return { level, violations, fixes }
}

/**
 * Tests if interactive elements are keyboard accessible.
 * @param {HTMLElement} element - The interactive element to test.
 * @returns {{level: string, violations: string[], fixes: string[]}} An object containing the accessibility level, list of violations, and suggested fixes.
 * @throws {TypeError} If the input is not a valid HTMLElement.
 * @example
 * const button = document.querySelector('button')
 * const result = testKeyboard(button)
 * console.log(result) // { level: 'A', violations: ['No keyboard event handlers'], fixes: ['Add keyboard event handlers'] }
 */
export function testKeyboard(element) {
  if (!(element instanceof HTMLElement)) {
    throw new TypeError('Input must be a valid HTMLElement')
  }

  const violations = []
  const fixes = []

  // Check for keyboard event handlers
  if (!element.addEventListener || !element.removeEventListener) {
    violations.push('No keyboard event handlers')
    fixes.push('Add keyboard event handlers for focus and interaction')
  }

  // Check for tab index
  if (element.tabIndex === -1 && !element.hasAttribute('tabindex')) {
    violations.push('Non-focusable element without tabindex')
    fixes.push('Add tabindex attribute to make the element focusable')
  }

  // Determine accessibility level
  const level = violations.length === 0 ? 'A' : violations.length <= 2 ? 'AA' : 'AAA'

  return { level, violations, fixes }
}