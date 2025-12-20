/**
 * Records usage of a component with associated context.
 * @param {string} component - The identifier of the component.
 * @param {Object} context - Additional context information (e.g., user, location, etc.).
 * @throws {Error} If component is not a string or context is not an object.
 * @example
 * recordUsage("login", { user: "alice", location: "US" });
 */
export function recordUsage(component, context) {
  if (typeof component !== 'string') {
    throw new Error('Component must be a string.');
  }
  if (typeof context !== 'object' || context === null) {
    throw new Error('Context must be an object.');
  }

  // In a real app, this would persist the usage data
  console.log(`Recorded usage for component: ${component} with context:`, context);
}

/**
 * Retrieves statistics for a component within a specified time range.
 * @param {string} componentId - The identifier of the component.
 * @param {string} timeRange - Time range filter (e.g., 'lastWeek', 'lastMonth').
 * @returns {{count: number, lastUsed: Date, contexts: Array}} - Component usage statistics.
 * @throws {Error} If componentId is not a string or timeRange is invalid.
 * @example
 * const stats = await getStats("login", "lastWeek");
 * console.log(stats.count, stats.lastUsed, stats.contexts);
 */
export function getStats(componentId, timeRange) {
  if (typeof componentId !== 'string') {
    throw new Error('Component ID must be a string.');
  }
  const validTimeRanges = ['lastWeek', 'lastMonth', 'allTime'];
  if (!validTimeRanges.includes(timeRange)) {
    throw new Error('Invalid time range. Valid options: lastWeek, lastMonth, allTime.');
  }

  // In a real app, this would query a database or cache
  const stats = {
    count: 150,
    lastUsed: new Date('2023-10-15'),
    contexts: [
      { user: 'alice', location: 'US' },
      { user: 'bob', location: 'UK' }
    ]
  };

  return stats;
}

/**
 * Ranks the popularity of components based on usage count.
 * @returns {{component: string, count: number}[]} - List of components ranked by usage count.
 * @example
 * const popular = await rankPopular();
 * console.log(popular[0].component, popular[0].count);
 */
export function rankPopular() {
  // In a real app, this would fetch data from a database or cache
  return [
    { component: 'login', count: 150 },
    { component: 'dashboard', count: 120 },
    { component: 'settings', count: 90 }
  ];
}