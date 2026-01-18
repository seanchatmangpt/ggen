/**
 * Finds breaking changes between old and new property objects.
 * @param {Object} oldProps - Old property object
 * @param {Object} newProps - New property object
 * @returns {Object} - { breaking, changed, added, removed }
 * @throws {Error} If inputs are not objects
 * @example
 * const oldProps = { name: 'John', age: 30, email: 'john@example.com' };
 * const newProps = { name: 'Jane', age: 35, email: 'jane@example.com', role: 'admin' };
 * const result = findBreaking(oldProps, newProps);
 * // result.breaking === true
 * // result.changed === { name: 'John' => 'Jane', age: 30 => 35 }
 * // result.added === { role: 'admin' }
 * // result.removed === { email: 'john@example.com' }
 */
export function findBreaking(oldProps, newProps) {
  if (typeof oldProps !== 'object' || oldProps === null || typeof newProps !== 'object' || newProps === null) {
    throw new Error('Both oldProps and newProps must be objects');
  }

  const breaking = false;
  const changed = {};
  const added = {};
  const removed = {};

  const oldKeys = Object.keys(oldProps);
  const newKeys = Object.keys(newProps);

  for (const key of oldKeys) {
    if (!newKeys.includes(key)) {
      removed[key] = oldProps[key];
    } else if (oldProps[key] !== newProps[key]) {
      changed[key] = { old: oldProps[key], new: newProps[key] };
    }
  }

  for (const key of newKeys) {
    if (!oldKeys.includes(key)) {
      added[key] = newProps[key];
    }
  }

  // Determine if any breaking change occurred
  for (const key in changed) {
    if (typeof oldProps[key] !== typeof newProps[key]) {
      breaking = true;
      break;
    }
  }

  return { breaking, changed, added, removed };
}

/**
 * Classifies the impact of a property change.
 * @param {Object} change - Change object { old, new }
 * @returns {string} - 'breaking', 'non-breaking', or 'no-change'
 * @throws {Error} If input is not a valid change object
 * @example
 * const change = { old: 'John', new: 'Jane' };
 * const impact = classifyImpact(change);
 * // impact === 'non-breaking'
 */
export function classifyImpact(change) {
  if (!change || typeof change !== 'object' || typeof change.old !== 'undefined' && typeof change.new !== 'undefined') {
    throw new Error('Invalid change object');
  }

  if (typeof change.old !== 'undefined' && typeof change.new !== 'undefined') {
    if (typeof change.old !== typeof change.new) {
      return 'breaking';
    }
    return 'non-breaking';
  }

  return 'no-change';
}

/**
 * Suggests migration steps based on breaking changes.
 * @param {Object} breaking - Breaking changes object
 * @returns {Object} - Migration suggestions
 * @throws {Error} If input is not a valid breaking object
 * @example
 * const breaking = { changed: { name: { old: 'John', new: 'Jane' }, age: { old: 30, new: 35 } }, added: { role: 'admin' }, removed: { email: 'john@example.com' } };
 * const migration = suggestMigration(breaking);
 * // migration.suggestions === ['Update name field from "John" to "Jane", Update age field from 30 to 35, Add role field with value "admin", Remove email field']
 */
export function suggestMigration(breaking) {
  if (!breaking || typeof breaking !== 'object' || typeof breaking.changed !== 'object' || typeof breaking.added !== 'object' || typeof breaking.removed !== 'object') {
    throw new Error('Invalid breaking object');
  }

  const suggestions = [];

  for (const key in breaking.changed) {
    const { old, new } = breaking.changed[key];
    suggestions.push(`Update ${key} field from "${old}" to "${new}"`);
  }

  for (const key in breaking.added) {
    const value = breaking.added[key];
    suggestions.push(`Add ${key} field with value "${value}"`);
  }

  for (const key in breaking.removed) {
    const value = breaking.removed[key];
    suggestions.push(`Remove ${key} field with value "${value}"`);
  }

  return { suggestions };
}