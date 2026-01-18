import { describe, it, expect } from 'vitest'
import { analyzeVersionDiff, categorizeChanges, assessImpact, recommendVersion } from './version-diff-analyzer.mjs'

describe('version-diff-analyzer - JSDoc Examples', () => {
  it('example 1', async () => {
    const diff = analyzeVersionDiff("1.2.3", "1.3.0")
    // diff => { major: 0, minor: 1, patch: 0 }
  })

  it('example 2', async () => {
    const category = categorizeChanges({ major: 0, minor: 1, patch: 0 })
    // category => "minor"
  })

  it('example 3', async () => {
    const impact = assessImpact({ major: 0, minor: 1, patch: 0 })
    // impact => { impact: "medium", description: "Breaking changes in minor version" }
  })

  it('example 4', async () => {
    const recommendation = recommendVersion({ major: 0, minor: 1, patch: 0 }, { impact: "medium", description: "Breaking changes in minor version" })
    // recommendation => { recommendedVersion: "1.1.0", changelog: "BREAKING: New features introduced in minor version" }
  })

})
