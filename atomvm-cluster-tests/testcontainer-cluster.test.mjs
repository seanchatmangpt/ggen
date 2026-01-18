import { describe, it, expect } from 'vitest'
import { startCluster, stopCluster, getNodeConnections } from './testcontainer-cluster.mjs'

describe('testcontainer-cluster - JSDoc Examples', () => {
  it('example 1', async () => {
    const clusterManager = await startCluster(3)
    console.log('Cluster started with 3 nodes')
  })

  it('example 2', async () => {
    const clusterManager = await startCluster(3)
    await clusterManager.stop()
    console.log('Cluster stopped')
  })

  it('example 3', async () => {
    const clusterManager = await startCluster(3)
    const connections = await clusterManager.getNodeConnections('node1')
    console.log('Node 1 is connected to:', connections)
  })

})
