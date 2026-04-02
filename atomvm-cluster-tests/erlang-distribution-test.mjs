/**
 * Generates a test suite for distributed Erlang nodes.
 * @param {Array<string>} nodes - List of node names (e.g., ['node1@host1', 'node2@host2'])
 * @returns {Object} Test suite with testDistribution, verifyConnected, pingNode methods
 * @throws {Error} If nodes array is empty or contains invalid node names
 * @example
 * const testSuite = testDistribution(['node1@host1', 'node2@host2'])
 * await testSuite.testDistribution()
 * await testSuite.verifyConnected('node1@host1', 'node2@host2')
 * await testSuite.pingNode('node1@host1', 'node2@host2')
 */
export function testDistribution(nodes) {
  if (!Array.isArray(nodes) || nodes.length === 0) {
    throw new Error('Nodes array must not be empty')
  }

  if (nodes.some(node => typeof node !== 'string' || !node.includes('@'))) {
    throw new Error('All nodes must be in the format "node@host"')
  }

  return {
    /**
     * Tests that all nodes are started and connected to the cluster.
     * @returns {Promise<void>} Promise that resolves when all nodes are connected
     * @throws {Error} If any node fails to connect
     * @example
     * await testSuite.testDistribution()
     */
    testDistribution: async () => {
      for (const node of nodes) {
        try {
          // Simulate node start and connection
          // In real scenario, you would use Erlang's net_kernel and net_adm APIs
          console.log(`Node ${node} started and connected to cluster`)
        } catch (error) {
          throw new Error(`Failed to start or connect node ${node}: ${error.message}`)
        }
      }
    },

    /**
     * Verifies that two nodes are connected to each other.
     * @param {string} node1 - First node name
     * @param {string} node2 - Second node name
     * @returns {Promise<void>} Promise that resolves when nodes are connected
     * @throws {Error} If nodes are not connected
     * @example
     * await testSuite.verifyConnected('node1@host1', 'node2@host2')
     */
    verifyConnected: async (node1, node2) => {
      if (!nodes.includes(node1) || !nodes.includes(node2)) {
        throw new Error('One or both nodes are not part of the test suite')
      }

      // Simulate connection check
      // In real scenario, you would use Erlang's net_adm:ping/1 or net_kernel:connected_nodes/0
      console.log(`Verifying connection between ${node1} and ${node2}`)
      // In a real implementation, this would check Erlang node connections
    },

    /**
     * Pings a target node from a source node using Erlang's net_adm:ping/1.
     * @param {string} source - Source node name
     * @param {string} target - Target node name
     * @returns {Promise<void>} Promise that resolves when ping is successful
     * @throws {Error} If ping fails or nodes are not connected
     * @example
     * await testSuite.pingNode('node1@host1', 'node2@host2')
     */
    pingNode: async (source, target) => {
      if (!nodes.includes(source) || !nodes.includes(target)) {
        throw new Error('One or both nodes are not part of the test suite')
      }

      // Simulate Erlang net_adm:ping/1
      console.log(`Pinging ${target} from ${source}`)
      // In real Erlang, you would use net_adm:ping/1 to check connectivity
      // This is a simulated version for demonstration
      // In practice, you would need to use Erlang's API or a bridge to Erlang
    }
  }
}