import { describe, it, expect } from 'vitest'
import { SupervisorTree } from './supervisor-tree.mjs'

describe('supervisor-tree - JSDoc Examples', () => {
  it('example 1', async () => {
    // Example usage of SupervisorTree
    const supervisor = new SupervisorTree('main-supervisor', 'rest_for_one');
    
    supervisor.addChild('child1', () => Promise.resolve('child1 started'), 'one_for_one');
    supervisor.addChild('child2', () => Promise.resolve('child2 started'), 'one_for_all');
    supervisor.addChild('child3', () => Promise.resolve('child3 started'), 'one_for_one');
    
    try {
      await supervisor.start();
      console.log('All children started');
    
      await supervisor.restart('child2');
      console.log('Child2 restarted');
    } catch (error) {
      console.error('Supervisor error:', error.message);
    }
  })

})
