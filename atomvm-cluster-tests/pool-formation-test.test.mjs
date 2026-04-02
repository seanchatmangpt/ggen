import { describe, it, expect } from 'vitest'
import { testPoolFormation, verifyPoolLeader, checkNodeMembership, getPoolValidationSuite } from './pool-formation-test.mjs'

describe('pool-formation-test - JSDoc Examples', () => {
  it('example 1', async () => {
    const pool = testPoolFormation(['node1', 'node2', 'node3'])
    console.log(pool)
  })

  it('example 2', async () => {
    const pool = testPoolFormation(['node1', 'node2', 'node3'])
    const isValid = verifyPoolLeader(pool)
    console.log(isValid) // true
  })

  it('example 3', async () => {
    const pool = testPoolFormation(['node1', 'node2', 'node3'])
    const isMember = checkNodeMembership('node2', pool)
    console.log(isMember) // true
  })

  it('example 4', async () => {
    const poolSuite = getPoolValidationSuite(['node1', 'node2', 'node3'])
    const pool = poolSuite.testPoolFormation(poolSuite.nodes)
    const isValidLeader = poolSuite.verifyPoolLeader(pool)
    const isMember = poolSuite.checkNodeMembership('node2', pool)
    console.log(isValidLeader, isMember)
  })

})
