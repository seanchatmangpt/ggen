/**
 * Simulates a swarm coordination test where tasks are distributed among nodes.
 * This function runs a series of coordination tests to ensure tasks are evenly distributed.
 * @param {Array<Function>} tasks - Array of task functions to be executed by nodes.
 * @param {Array<string>} nodes - Array of node identifiers (e.g., node1, node2, etc.).
 * @returns {Promise<Array<{node: string, tasks: Array<Function>, load: number}>>} Results of the coordination tests.
 * @throws {Error} If tasks or nodes are not provided or are invalid.
 * @example
 * const task1 = () => console.log('Task 1 executed');
 * const task2 = () => console.log('Task 2 executed');
 * const task3 = () => console.log('Task 3 executed');
 * const nodes = ['node1', 'node2', 'node3'];
 * const results = await testSwarmCoordination([task1, task2, task3], nodes);
 * // results contains each node's assigned tasks and load.
 */
export async function testSwarmCoordination(tasks, nodes) {
  if (!Array.isArray(tasks) || tasks.length === 0) {
    throw new Error('Tasks must be a non-empty array.');
  }
  if (!Array.isArray(nodes) || nodes.length === 0) {
    throw new Error('Nodes must be a non-empty array.');
  }

  const results = await Promise.all(nodes.map(node => distributeWork(node, tasks)));
  return results;
}

/**
 * Distributes tasks to a node and simulates work execution.
 * @param {string} node - Node identifier.
 * @param {Array<Function>} tasks - Array of task functions to be assigned to the node.
 * @returns {Promise<{node: string, tasks: Array<Function>, load: number}>} Result of task distribution and execution.
 * @throws {Error} If node or tasks are invalid.
 * @example
 * const node = 'node1';
 * const tasks = [() => console.log('Task 1'), () => console.log('Task 2')];
 * const result = await distributeWork(node, tasks);
 * // result contains node, assigned tasks, and load (number of tasks).
 */
export async function distributeWork(node, tasks) {
  if (!node || typeof node !== 'string') {
    throw new Error('Node must be a valid string identifier.');
  }
  if (!Array.isArray(tasks) || tasks.length === 0) {
    throw new Error('Tasks must be a non-empty array.');
  }

  const assignedTasks = tasks.slice(0, Math.floor(tasks.length / 2));
  const load = assignedTasks.length;

  // Simulate task execution
  await Promise.all(assignedTasks.map(task => task()));

  return { node, tasks: assignedTasks, load };
}

/**
 * Verifies that the load across nodes is balanced.
 * @param {Array<{node: string, tasks: Array<Function>, load: number}>} results - Results from distributeWork.
 * @returns {boolean} True if load is balanced, false otherwise.
 * @throws {Error} If results are invalid.
 * @example
 * const results = await testSwarmCoordination([task1, task2, task3], ['node1', 'node2', 'node3']);
 * const isBalanced = await verifyLoadBalance(results);
 * // isBalanced is true if all nodes have similar load.
 */
export async function verifyLoadBalance(results) {
  if (!Array.isArray(results) || results.length === 0) {
    throw new Error('Results must be a non-empty array.');
  }

  const loads = results.map(result => result.load);
  const avgLoad = loads.reduce((a, b) => a + b, 0) / loads.length;

  // Allow a small variance for load balancing
  const variance = loads.reduce((a, b) => a + Math.pow(b - avgLoad, 2), 0) / loads.length;
  const tolerance = 0.2; // 20% variance tolerance

  return variance <= Math.pow(avgLoad * tolerance, 2);
}