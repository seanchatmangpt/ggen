import { describe, it, expect } from 'vitest'
import { runFailureRecoveryTest } from './node-failure-recovery.mjs'

describe('node-failure-recovery - JSDoc Examples', () => {
  it('example 1', async () => {
    const cluster = {
      killNode: (id) => console.log(`Killed node ${id}`),
      verifyRecovery: () => console.log('Cluster recovered successfully'),
      getNodeStatus: () => ({ activeNodes: [1, 2, 3] })
    };
    await runFailureRecoveryTest(cluster, 2);
  })

})
