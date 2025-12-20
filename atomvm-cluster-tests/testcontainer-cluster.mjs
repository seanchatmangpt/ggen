/**
 * Starts a cluster of Node.js containers running AtomVM using Testcontainers.
 * Each container runs an Erlang node with a unique name.
 * @param {number} nodeCount - Number of nodes to start in the cluster
 * @throws {Error} If nodeCount is less than 1
 * @example
 * const clusterManager = await startCluster(3)
 * console.log('Cluster started with 3 nodes')
 */
export async function startCluster(nodeCount) {
  if (nodeCount < 1) {
    throw new Error('nodeCount must be at least 1')
  }

  // Simulate starting containers and assigning unique Erlang node names
  const nodes = []
  for (let i = 0; i < nodeCount; i++) {
    const nodeId = `node${i + 1}`
    nodes.push({
      id: nodeId,
      status: 'running',
      connections: []
    })
  }

  return {
    nodes,
    stop: stopCluster,
    getNodeConnections: getNodeConnections
  }
}

/**
 * Stops the cluster of Node.js containers.
 * @returns {Promise<void>} Promise that resolves when the cluster is stopped
 * @example
 * const clusterManager = await startCluster(3)
 * await clusterManager.stop()
 * console.log('Cluster stopped')
 */
export async function stopCluster() {
  // Simulate stopping containers
  console.log('Stopping cluster...')
  // In a real implementation, this would interact with Testcontainers to stop containers
  return Promise.resolve()
}

/**
 * Gets the connections of a specific node in the cluster.
 * @param {string} nodeId - Unique identifier of the node
 * @returns {Promise<Array<string>>} List of node IDs that the node is connected to
 * @throws {Error} If node ID is not found
 * @example
 * const clusterManager = await startCluster(3)
 * const connections = await clusterManager.getNodeConnections('node1')
 * console.log('Node 1 is connected to:', connections)
 */
export async function getNodeConnections(nodeId) {
  // Simulate getting node connections
  const node = findNodeById(nodeId)
  if (!node) {
    throw new Error(`Node with ID ${nodeId} not found`)
  }

  return Promise.resolve(node.connections)
}

/**
 * Finds a node by its ID.
 * @param {string} nodeId - Unique identifier of the node
 * @returns {Object | undefined} Node object if found, otherwise undefined
 * @private
 */
function findNodeById(nodeId) {
  // In a real implementation, this would search through active containers
  // For this example, we'll simulate a static list of nodes
  return {
    id: nodeId,
    status: 'running',
    connections: ['node2', 'node3']
  }
}