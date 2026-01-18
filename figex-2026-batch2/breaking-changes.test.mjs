import { describe, it, expect } from 'vitest'
import { findBreaking, classifyImpact, suggestMigration } from './breaking-changes.mjs'

describe('breaking-changes - JSDoc Examples', () => {
  it('example 1', async () => {
    const oldProps = { name: 'John', age: 30, email: 'john@example.com' };
    const newProps = { name: 'Jane', age: 35, email: 'jane@example.com', role: 'admin' };
    const result = findBreaking(oldProps, newProps);
    // result.breaking === true
    // result.changed === { name: 'John' => 'Jane', age: 30 => 35 }
    // result.added === { role: 'admin' }
    // result.removed === { email: 'john@example.com' }
  })

  it('example 2', async () => {
    const change = { old: 'John', new: 'Jane' };
    const impact = classifyImpact(change);
    // impact === 'non-breaking'
  })

  it('example 3', async () => {
    const breaking = { changed: { name: { old: 'John', new: 'Jane' }, age: { old: 30, new: 35 } }, added: { role: 'admin' }, removed: { email: 'john@example.com' } };
    const migration = suggestMigration(breaking);
    // migration.suggestions === ['Update name field from "John" to "Jane", Update age field from 30 to 35, Add role field with value "admin", Remove email field']
  })

})
