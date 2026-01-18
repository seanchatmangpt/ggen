import { describe, it, expect } from 'vitest'
import { testDistribution } from './erlang-distribution-test.mjs'

describe('erlang-distribution-test - JSDoc Examples', () => {
  it('example 1', async () => {
    const testSuite = testDistribution(['node1@host1', 'node2@host2'])
    await testSuite.testDistribution()
    await testSuite.verifyConnected('node1@host1', 'node2@host2')
    await testSuite.pingNode('node1@host1', 'node2@host2')
  })

  it('example 2', async () => {
    await testSuite.testDistribution()
  })

  it('example 3', async () => {
    await testSuite.verifyConnected('node1@host1', 'node2@host2')
  })

  it('example 4', async () => {
    await testSuite.pingNode('node1@host1', 'node2@host2')
  })

})
