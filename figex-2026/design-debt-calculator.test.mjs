import { describe, it, expect } from 'vitest'
import { calculateDebt, scoreInconsistency, calculateBaseline, estimateRefactorCost } from './design-debt-calculator.mjs'

describe('design-debt-calculator - JSDoc Examples', () => {
  it('example 1', async () => {
    const components = [
      { name: 'Header', props: ['title', 'className'], style: { color: 'red' } },
      { name: 'Footer', props: ['darkMode'], style: { backgroundColor: 'blue' } }
    ];
    const debtScore = calculateDebt(components);
    console.log(debtScore); // Output: 25
  })

  it('example 2', async () => {
    const component = { name: 'Header', props: ['title', 'className'], style: { color: 'red' } };
    const baseline = { name: 'Header', props: ['title'], style: { color: 'blue' } };
    const score = scoreInconsistency(component, baseline);
    console.log(score); // Output: 50
  })

  it('example 3', async () => {
    const component = { name: 'Header', props: ['title', 'className'], style: { color: 'red' } };
    const baseline = calculateBaseline(component);
    console.log(baseline); // Output: { name: 'Header', props: ['title', 'className'], style: { color: 'red' } }
  })

  it('example 4', async () => {
    const debt = 25;
    const cost = estimateRefactorCost(debt);
    console.log(cost); // Output: 2
  })

})
