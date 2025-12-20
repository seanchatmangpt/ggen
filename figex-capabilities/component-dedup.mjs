/**
 * Finds duplicate design components based on similarity.
 * @param {Array<DesignComponent>} components - Array of design components to check for duplicates
 * @returns {Array<Array<DesignComponent>>} - Array of arrays, where each inner array contains duplicate components
 * @throws {Error} If input is not an array or contains invalid components
 * @example
 * const components = [
 *   { id: 1, name: 'Button', type: 'UI', description: 'Primary action button' },
 *   { id: 2, name: 'Button', type: 'UI', description: 'Primary action button' },
 *   { id: 3, name: 'Input', type: 'UI', description: 'Text input field' },
 *   { id: 4, name: 'Input', type: 'UI', description: 'Text input field' },
 *   { id: 5, name: 'Card', type: 'UI', description: 'Reusable card component' },
 * ];
 * const duplicates = findDuplicates(components);
 * // duplicates will be [[components[0], components[1]], [components[2], components[3]]]
 */
export function findDuplicates(components) {
  if (!Array.isArray(components)) {
    throw new Error('Input must be an array of design components');
  }

  const componentMap = new Map();
  const duplicates = [];

  for (const component of components) {
    if (!component || typeof component !== 'object' || !('name' in component)) {
      throw new Error('Each component must have a "name" property');
    }

    const key = component.name;
    if (!componentMap.has(key)) {
      componentMap.set(key, []);
    }

    const group = componentMap.get(key);
    group.push(component);

    if (group.length === 2) {
      duplicates.push(group);
    }
  }

  return duplicates;
}

/**
 * Calculates similarity between two design components.
 * Similarity is based on matching name, type, and description.
 * @param {DesignComponent} comp1 - First component
 * @param {DesignComponent} comp2 - Second component
 * @returns {number} - Similarity score between 0 and 1
 * @throws {Error} If either component is invalid
 * @example
 * const comp1 = { name: 'Button', type: 'UI', description: 'Primary action button' };
 * const comp2 = { name: 'Button', type: 'UI', description: 'Primary action button' };
 * const similarity = calculateSimilarity(comp1, comp2);
 * // similarity will be 1
 */
export function calculateSimilarity(comp1, comp2) {
  if (!comp1 || typeof comp1 !== 'object' || !('name' in comp1)) {
    throw new Error('First component is invalid');
  }
  if (!comp2 || typeof comp2 !== 'object' || !('name' in comp2)) {
    throw new Error('Second component is invalid');
  }

  let score = 0;

  if (comp1.name === comp2.name) score += 0.5;
  if (comp1.type === comp2.type) score += 0.25;
  if (comp1.description === comp2.description) score += 0.25;

  return Math.min(1, score);
}

/**
 * Merges duplicate design components into a single component.
 * @param {Array<DesignComponent>} duplicates - Array of duplicate components to merge
 * @returns {DesignComponent} - Merged component
 * @throws {Error} If input is not an array or contains invalid components
 * @example
 * const duplicates = [
 *   { id: 1, name: 'Button', type: 'UI', description: 'Primary action button' },
 *   { id: 2, name: 'Button', type: 'UI', description: 'Primary action button' },
 * ];
 * const merged = mergeDuplicates(duplicates);
 * // merged will be { id: 1, name: 'Button', type: 'UI', description: 'Primary action button' }
 */
export function mergeDuplicates(duplicates) {
  if (!Array.isArray(duplicates)) {
    throw new Error('Input must be an array of duplicate components');
  }

  if (duplicates.length < 2) {
    throw new Error('At least two components are required to merge');
  }

  const first = duplicates[0];
  const merged = { ...first };

  for (let i = 1; i < duplicates.length; i++) {
    const comp = duplicates[i];
    if (comp.id && merged.id !== comp.id) {
      merged.id = comp.id;
    }
    if (comp.type && merged.type !== comp.type) {
      merged.type = comp.type;
    }
    if (comp.description && merged.description !== comp.description) {
      merged.description = comp.description;
    }
  }

  return merged;
}