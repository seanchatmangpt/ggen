import { describe, it, expect } from 'vitest'
import { recordUsage, getStats, rankPopular } from './usage-analytics.mjs'

describe('usage-analytics - JSDoc Examples', () => {
  it('example 1', async () => {
    recordUsage("login", { user: "alice", location: "US" });
  })

  it('example 2', async () => {
    const stats = await getStats("login", "lastWeek");
    console.log(stats.count, stats.lastUsed, stats.contexts);
  })

  it('example 3', async () => {
    const popular = await rankPopular();
    console.log(popular[0].component, popular[0].count);
  })

})
