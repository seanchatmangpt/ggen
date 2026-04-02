/**
 * SupervisorTree manages a tree of child processes with restart strategies.
 * Supports one_for_one, one_for_all, and rest_for_one restart strategies.
 * @class
 */
export class SupervisorTree {
  /**
   * Creates a new SupervisorTree.
   * @constructor
   * @param {string} id - Unique identifier for the supervisor
   * @param {string} restartStrategy - The restart strategy to use: 'one_for_one', 'one_for_all', or 'rest_for_one'
   * @throws {Error} If restartStrategy is invalid
   */
  constructor(id, restartStrategy) {
    this.id = id;
    this.children = new Map();
    this.restartStrategy = this._validateRestartStrategy(restartStrategy);
  }

  /**
   * Adds a child to the supervisor with a specified restart strategy.
   * @param {string} childId - Unique identifier for the child
   * @param {Function} child - The child function to be supervised
   * @param {string} childRestartStrategy - The restart strategy for this child: 'one_for_one' or 'one_for_all'
   * @throws {Error} If childId is already in use or childRestartStrategy is invalid
   */
  addChild(childId, child, childRestartStrategy) {
    if (this.children.has(childId)) {
      throw new Error(`Child with ID "${childId}" already exists.`);
    }

    if (!['one_for_one', 'one_for_all'].includes(childRestartStrategy)) {
      throw new Error(`Invalid child restart strategy: ${childRestartStrategy}`);
    }

    this.children.set(childId, {
      id: childId,
      child,
      restartStrategy: childRestartStrategy,
      status: 'stopped',
    });
  }

  /**
   * Starts all children under this supervisor.
   * @returns {Promise<void>} A promise that resolves when all children are started
   * @throws {Error} If any child fails to start
   */
  async start() {
    for (const child of this.children.values()) {
      try {
        await child.child();
        child.status = 'started';
      } catch (error) {
        throw new Error(`Failed to start child "${child.id}": ${error.message}`);
      }
    }
  }

  /**
   * Restarts a child based on the supervisor's restart strategy.
   * @param {string} childId - The ID of the child to restart
   * @throws {Error} If childId is not found
   */
  async restart(childId) {
    const child = this.children.get(childId);
    if (!child) {
      throw new Error(`Child with ID "${childId}" not found.`);
    }

    if (child.status === 'stopped') {
      throw new Error(`Child "${childId}" is already stopped.`);
    }

    try {
      // Stop the child
      child.status = 'stopped';

      // Restart the child
      await child.child();
      child.status = 'started';

      // Apply restart strategy
      await this._applyRestartStrategy(childId);
    } catch (error) {
      throw new Error(`Failed to restart child "${childId}": ${error.message}`);
    }
  }

  /**
   * Applies the restart strategy to the supervisor.
   * @private
   * @param {string} childId - The ID of the child that failed
   * @throws {Error} If restart strategy is invalid
   */
  async _applyRestartStrategy(childId) {
    switch (this.restartStrategy) {
      case 'one_for_one':
        await this._restartOneForOne(childId);
        break;
      case 'one_for_all':
        await this._restartOneForAll();
        break;
      case 'rest_for_one':
        await this._restartRestForOne(childId);
        break;
      default:
        throw new Error(`Invalid restart strategy: ${this.restartStrategy}`);
    }
  }

  /**
   * Restarts only the specified child (one_for_one).
   * @private
   * @param {string} childId - The ID of the child to restart
   * @throws {Error} If childId is not found
   */
  async _restartOneForOne(childId) {
    const child = this.children.get(childId);
    if (!child) {
      throw new Error(`Child with ID "${childId}" not found.`);
    }

    if (child.status === 'stopped') {
      throw new Error(`Child "${childId}" is already stopped.`);
    }

    try {
      child.status = 'stopped';
      await child.child();
      child.status = 'started';
    } catch (error) {
      throw new Error(`Failed to restart child "${childId}": ${error.message}`);
    }
  }

  /**
   * Restarts all children (one_for_all).
   * @private
   * @throws {Error} If any child fails to restart
   */
  async _restartOneForAll() {
    for (const child of this.children.values()) {
      if (child.status === 'stopped') {
        throw new Error(`Child "${child.id}" is already stopped.`);
      }

      try {
        child.status = 'stopped';
        await child.child();
        child.status = 'started';
      } catch (error) {
        throw new Error(`Failed to restart child "${child.id}": ${error.message}`);
      }
    }
  }

  /**
   * Restarts all children except the one that failed (rest_for_one).
   * @private
   * @param {string} childId - The ID of the child that failed
   * @throws {Error} If childId is not found
   */
  async _restartRestForOne(childId) {
    const child = this.children.get(childId);
    if (!child) {
      throw new Error(`Child with ID "${childId}" not found.`);
    }

    if (child.status === 'stopped') {
      throw new Error(`Child "${childId}" is already stopped.`);
    }

    try {
      child.status = 'stopped';
      await child.child();
      child.status = 'started';
    } catch (error) {
      throw new Error(`Failed to restart child "${childId}": ${error.message}`);
    }

    // Restart all other children
    for (const otherChild of this.children.values()) {
      if (otherChild.id === childId) continue;

      if (otherChild.status === 'stopped') {
        throw new Error(`Child "${otherChild.id}" is already stopped.`);
      }

      try {
        otherChild.status = 'stopped';
        await otherChild.child();
        otherChild.status = 'started';
      } catch (error) {
        throw new Error(`Failed to restart child "${otherChild.id}": ${error.message}`);
      }
    }
  }

  /**
   * Validates the restart strategy.
   * @private
   * @param {string} strategy - The restart strategy to validate
   * @returns {string} The validated strategy
   * @throws {Error} If strategy is invalid
   */
  _validateRestartStrategy(strategy) {
    if (!['one_for_one', 'one_for_all', 'rest_for_one'].includes(strategy)) {
      throw new Error(`Invalid restart strategy: ${strategy}`);
    }
    return strategy;
  }
}

/**
 * @example
 * // Example usage of SupervisorTree
 * const supervisor = new SupervisorTree('main-supervisor', 'rest_for_one');
 *
 * supervisor.addChild('child1', () => Promise.resolve('child1 started'), 'one_for_one');
 * supervisor.addChild('child2', () => Promise.resolve('child2 started'), 'one_for_all');
 * supervisor.addChild('child3', () => Promise.resolve('child3 started'), 'one_for_one');
 *
 * try {
 *   await supervisor.start();
 *   console.log('All children started');
 *
 *   await supervisor.restart('child2');
 *   console.log('Child2 restarted');
 * } catch (error) {
 *   console.error('Supervisor error:', error.message);
 * }
 */