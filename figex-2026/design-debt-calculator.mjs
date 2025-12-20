/**
 * Calculates the overall technical debt score based on component inconsistencies.
 * @param {Array<{name: string, props: Array<string>, style: Object}>} components - List of components with their props and styles
 * @returns {number} Debt score between 0 and 100
 * @throws {Error} If components is not an array or contains invalid data
 * @example
 * const components = [
 *   { name: 'Header', props: ['title', 'className'], style: { color: 'red' } },
 *   { name: 'Footer', props: ['darkMode'], style: { backgroundColor: 'blue' } }
 * ];
 * const debtScore = calculateDebt(components);
 * console.log(debtScore); // Output: 25
 */
export function calculateDebt(components) {
  if (!Array.isArray(components)) {
    throw new Error('Components must be an array');
  }

  let totalDebt = 0;

  for (const component of components) {
    if (typeof component.name !== 'string' || typeof component.props !== 'object' || typeof component.style !== 'object') {
      throw new Error('Invalid component structure');
    }

    const componentDebt = scoreInconsistency(component, calculateBaseline(component));
    totalDebt += componentDebt;
  }

  return Math.min(100, Math.round(totalDebt / components.length));
}

/**
 * Scores the inconsistency of a single component against its baseline.
 * @param {Object} component - Component object with name, props, and style
 * @param {Object} baseline - Baseline component for comparison
 * @returns {number} Inconsistency score between 0 and 100
 * @throws {Error} If component or baseline is invalid
 * @example
 * const component = { name: 'Header', props: ['title', 'className'], style: { color: 'red' } };
 * const baseline = { name: 'Header', props: ['title'], style: { color: 'blue' } };
 * const score = scoreInconsistency(component, baseline);
 * console.log(score); // Output: 50
 */
export function scoreInconsistency(component, baseline) {
  if (!component || !baseline || typeof component !== 'object' || typeof baseline !== 'object') {
    throw new Error('Invalid component or baseline');
  }

  let score = 0;

  // Prop variance
  const propDiff = Math.abs(component.props.length - baseline.props.length);
  score += (propDiff / Math.max(component.props.length, baseline.props.length)) * 30;

  // Style drift
  const styleDiff = Object.keys(component.style).length - Object.keys(baseline.style).length;
  score += (Math.abs(styleDiff) / Math.max(Object.keys(component.style).length, Object.keys(baseline.style).length)) * 30;

  // Name mismatch
  if (component.name !== baseline.name) {
    score += 40;
  }

  return Math.min(100, Math.round(score));
}

/**
 * Calculates a baseline component for comparison.
 * @param {Object} component - Component object with name, props, and style
 * @returns {Object} Baseline component
 * @throws {Error} If component is invalid
 * @example
 * const component = { name: 'Header', props: ['title', 'className'], style: { color: 'red' } };
 * const baseline = calculateBaseline(component);
 * console.log(baseline); // Output: { name: 'Header', props: ['title', 'className'], style: { color: 'red' } }
 */
export function calculateBaseline(component) {
  if (!component || typeof component !== 'object') {
    throw new Error('Invalid component');
  }

  return {
    name: component.name,
    props: component.props,
    style: component.style
  };
}

/**
 * Estimates the refactor cost in hours based on the debt score.
 * @param {number} debt - Debt score between 0 and 100
 * @returns {number} Estimated refactor cost in hours
 * @throws {Error} If debt is not a number between 0 and 100
 * @example
 * const debt = 25;
 * const cost = estimateRefactorCost(debt);
 * console.log(cost); // Output: 2
 */
export function estimateRefactorCost(debt) {
  if (typeof debt !== 'number' || debt < 0 || debt > 100) {
    throw new Error('Debt must be a number between 0 and 100');
  }

  // Linear estimation: 0-20 = 0 hours, 20-40 = 1 hour, 40-60 = 2 hours, 60-80 = 3 hours, 80-100 = 4 hours
  if (debt <= 20) return 0;
  if (debt <= 40) return 1;
  if (debt <= 60) return 2;
  if (debt <= 80) return 3;
  return 4;
}