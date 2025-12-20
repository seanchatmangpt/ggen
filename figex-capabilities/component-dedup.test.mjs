import { describe, it, expect } from 'vitest'
import { findDuplicates, calculateSimilarity, mergeDuplicates } from './component-dedup.mjs'

describe('component-dedup - JSDoc Examples', () => {
  it('example 1', async () => {
    const components = [
      { id: 1, name: 'Button', type: 'UI', description: 'Primary action button' },
      { id: 2, name: 'Button', type: 'UI', description: 'Primary action button' },
      { id: 3, name: 'Input', type: 'UI', description: 'Text input field' },
      { id: 4, name: 'Input', type: 'UI', description: 'Text input field' },
      { id: 5, name: 'Card', type: 'UI', description: 'Reusable card component' },
    ];
    const duplicates = findDuplicates(components);
    // duplicates will be [[components[0], components[1]], [components[2], components[3]]]
  })

  it('example 2', async () => {
    const comp1 = { name: 'Button', type: 'UI', description: 'Primary action button' };
    const comp2 = { name: 'Button', type: 'UI', description: 'Primary action button' };
    const similarity = calculateSimilarity(comp1, comp2);
    // similarity will be 1
  })

  it('example 3', async () => {
    const duplicates = [
      { id: 1, name: 'Button', type: 'UI', description: 'Primary action button' },
      { id: 2, name: 'Button', type: 'UI', description: 'Primary action button' },
    ];
    const merged = mergeDuplicates(duplicates);
    // merged will be { id: 1, name: 'Button', type: 'UI', description: 'Primary action button' }
  })

})
