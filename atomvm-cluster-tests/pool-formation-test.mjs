/**
 * Generates a test pool formation structure for process groups (pg).
 * @param {Array<string>} nodes - List of node identifiers (e.g., ['node1', 'node2', 'node3'])
 * @returns {Object} Pool formation structure with nodes and leader
 * @throws {Error} If nodes is not an array or is empty
 * @example
 * const pool = testPoolFormation(['node1', 'node2', 'node3'])
 * console.log(pool)
 */
export function testPoolFormation(nodes) {
  if (!Array.isArray(nodes) || nodes.length === 0) {
    throw new Error('Nodes must be a non-empty array');
  }

  const pool = {
    nodes,
    leader: nodes[0],
    status: 'formed'
  };

  return pool;
}

/**
 * Verifies if a pool has a valid leader based on node membership.
 * @param {Object} pool - Pool object with nodes and leader
 * @returns {boolean} True if leader exists in nodes, false otherwise
 * @throws {Error} If pool is not an object or lacks required properties
 * @example
 * const pool = testPoolFormation(['node1', 'node2', 'node3'])
 * const isValid = verifyPoolLeader(pool)
 * console.log(isValid) // true
 */
export function verifyPoolLeader(pool) {
  if (typeof pool !== 'object' || !pool.nodes || !pool.leader) {
    throw new Error('Pool must be an object with nodes and leader properties');
  }

  return pool.nodes.includes(pool.leader);
}

/**
 * Checks if a node is a member of a given pool.
 * @param {string} node - Node identifier
 * @param {Object} pool - Pool object with nodes array
 * @returns {boolean} True if node is in the pool, false otherwise
 * @throws {Error} If node is not a string or pool is not an object
 * @example
 * const pool = testPoolFormation(['node1', 'node2', 'node3'])
 * const isMember = checkNodeMembership('node2', pool)
 * console.log(isMember) // true
 */
export function checkNodeMembership(node, pool) {
  if (typeof node !== 'string' || typeof pool !== 'object' || !pool.nodes) {
    throw new Error('Node must be a string and pool must be an object with nodes array');
  }

  return pool.nodes.includes(node);
}

/**
 * Returns a validation suite for process group (pg) pool.
 * @param {Array<string>} nodes - List of node identifiers
 * @returns {Object} Validation suite with testPoolFormation, verifyPoolLeader, checkNodeMembership
 * @throws {Error} If nodes is not an array or is empty
 * @example
 * const poolSuite = getPoolValidationSuite(['node1', 'node2', 'node3'])
 * const pool = poolSuite.testPoolFormation(poolSuite.nodes)
 * const isValidLeader = poolSuite.verifyPoolLeader(pool)
 * const isMember = poolSuite.checkNodeMembership('node2', pool)
 * console.log(isValidLeader, isMember)
 */
export function getPoolValidationSuite(nodes) {
  if (!Array.isArray(nodes) || nodes.length === 0) {
    throw new Error('Nodes must be a non-empty array');
  }

  return {
    nodes,
    testPoolFormation,
    verifyPoolLeader,
    checkNodeMembership
  };
}