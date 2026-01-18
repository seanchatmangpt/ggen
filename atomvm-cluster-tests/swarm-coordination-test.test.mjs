import { describe, it, expect } from 'vitest'
import { testSwarmCoordination, distributeWork, verifyLoadBalance } from './swarm-coordination-test.mjs'

describe('swarm-coordination-test - JSDoc Examples', () => {
  it('example 1', async () => {
    const task1 = () => console.log('Task 1 executed');
    const task2 = () => console.log('Task 2 executed');
    const task3 = () => console.log('Task 3 executed');
    const nodes = ['node1', 'node2', 'node3'];
    const results = await testSwarmCoordination([task1, task2, task3], nodes);
    // results contains each node's assigned tasks and load.
  })

  it('example 2', async () => {
    const node = 'node1';
    const tasks = [() => console.log('Task 1'), () => console.log('Task 2')];
    const result = await distributeWork(node, tasks);
    // result contains node, assigned tasks, and load (number of tasks).
  })

  it('example 3', async () => {
    const results = await testSwarmCoordination([task1, task2, task3], ['node1', 'node2', 'node3']);
    const isBalanced = await verifyLoadBalance(results);
    // isBalanced is true if all nodes have similar load.
  })

})
