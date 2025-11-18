import { useState, useCallback } from "react"

// ============================================================================
// COLLECTIVE LEARNING TYPES
// ============================================================================

export interface Experience {
  id: string
  agentId: string
  taskId: string
  state: Record<string, any>
  action: string
  reward: number
  nextState: Record<string, any>
  timestamp: number
}

export interface Skill {
  id: string
  name: string
  description: string
  successRate: number
  usageCount: number
  lastUsed: number
  agents: Set<string> // Agents that know this skill
  difficulty: number // 0-100
}

export interface LearningPolicy {
  id: string
  name: string
  strategy: "exploration" | "exploitation" | "balanced"
  explorationRate: number // 0-1
  learningRate: number // 0-1
  discountFactor: number // 0-1
  successThreshold: number // 0-100
}

export interface AdaptiveStrategy {
  id: string
  name: string
  taskType: string
  successRate: number
  adaptationRate: number // How quickly it adapts
  lastUpdated: number
  parameters: Record<string, number>
}

export interface SkillTransfer {
  from: string // Source agent
  to: string // Target agent
  skill: string
  transferEfficiency: number // 0-100, how well skill transferred
  timestamp: number
}

// ============================================================================
// SWARM LEARNING HOOK
// ============================================================================

export function useSwarmLearning() {
  const [experiences, setExperiences] = useState<Map<string, Experience>>(new Map())
  const [skills, setSkills] = useState<Map<string, Skill>>(new Map())
  const [policies, setPolicies] = useState<Map<string, LearningPolicy>>(new Map())
  const [strategies, setStrategies] = useState<Map<string, AdaptiveStrategy>>(new Map())
  const [skillTransfers, setSkillTransfers] = useState<SkillTransfer[]>([])
  const [policyGradients, setPolicyGradients] = useState<Map<string, number>>(new Map())

  // ========================================================================
  // EXPERIENCE MANAGEMENT
  // ========================================================================

  const recordExperience = useCallback(
    (
      agentId: string,
      taskId: string,
      state: Record<string, any>,
      action: string,
      reward: number,
      nextState: Record<string, any>
    ) => {
      const experience: Experience = {
        id: `exp-${Date.now()}`,
        agentId,
        taskId,
        state,
        action,
        reward,
        nextState,
        timestamp: Date.now(),
      }

      setExperiences((prev) => new Map(prev).set(experience.id, experience))
      return experience
    },
    []
  )

  const getExperienceReplay = useCallback(
    (agentId: string, limit = 32): Experience[] => {
      const agentExperiences = Array.from(experiences.values()).filter((e) => e.agentId === agentId)

      // Return most recent experiences (prioritize recent over old)
      return agentExperiences.sort((a, b) => b.timestamp - a.timestamp).slice(0, limit)
    },
    [experiences]
  )

  const computeReturnEstimate = useCallback(
    (experienceId: string, discountFactor = 0.99): number => {
      const experience = experiences.get(experienceId)
      if (!experience) return 0

      // Simple return: reward + discounted future rewards
      const futureExperiences = Array.from(experiences.values())
        .filter((e) => e.timestamp > experience.timestamp && e.agentId === experience.agentId)
        .sort((a, b) => a.timestamp - b.timestamp)
        .slice(0, 5)

      let estimatedReturn = experience.reward
      futureExperiences.forEach((future, index) => {
        estimatedReturn += Math.pow(discountFactor, index + 1) * future.reward
      })

      return estimatedReturn
    },
    [experiences]
  )

  // ========================================================================
  // SKILL MANAGEMENT & TRANSFER
  // ========================================================================

  const registerSkill = useCallback(
    (name: string, description: string, difficulty: number, agents: string[] = []) => {
      const skill: Skill = {
        id: `skill-${Date.now()}`,
        name,
        description,
        successRate: 0.5,
        usageCount: 0,
        lastUsed: Date.now(),
        agents: new Set(agents),
        difficulty,
      }

      setSkills((prev) => new Map(prev).set(skill.id, skill))
      return skill
    },
    []
  )

  const updateSkillSuccess = useCallback((skillId: string, success: boolean) => {
    setSkills((prev) => {
      const skill = prev.get(skillId)
      if (!skill) return prev

      const totalUses = skill.usageCount
      const newSuccessRate = (skill.successRate * totalUses + (success ? 1 : 0)) / (totalUses + 1)

      return new Map(prev).set(skillId, {
        ...skill,
        successRate: newSuccessRate,
        usageCount: totalUses + 1,
        lastUsed: Date.now(),
      })
    })
  }, [])

  const transferSkill = useCallback(
    (fromAgentId: string, toAgentId: string, skillId: string): SkillTransfer | null => {
      const skill = skills.get(skillId)
      if (!skill || !skill.agents.has(fromAgentId)) return null

      // Calculate transfer efficiency based on skill difficulty and agent reputation
      const transferEfficiency = Math.max(20, 100 - skill.difficulty * 0.5)

      const transfer: SkillTransfer = {
        from: fromAgentId,
        to: toAgentId,
        skill: skillId,
        transferEfficiency,
        timestamp: Date.now(),
      }

      // Add skill to target agent
      setSkills((prev) => {
        const updatedSkill = { ...skill }
        updatedSkill.agents.add(toAgentId)
        return new Map(prev).set(skillId, updatedSkill)
      })

      setSkillTransfers((prev) => [...prev, transfer])
      return transfer
    },
    [skills]
  )

  const getAgentSkills = useCallback(
    (agentId: string): Skill[] => {
      return Array.from(skills.values()).filter((skill) => skill.agents.has(agentId))
    },
    [skills]
  )

  // ========================================================================
  // LEARNING POLICIES
  // ========================================================================

  const createLearningPolicy = useCallback(
    (
      name: string,
      strategy: "exploration" | "exploitation" | "balanced"
    ): LearningPolicy => {
      const policy: LearningPolicy = {
        id: `policy-${Date.now()}`,
        name,
        strategy,
        explorationRate: strategy === "exploration" ? 0.8 : strategy === "exploitation" ? 0.1 : 0.3,
        learningRate: 0.1,
        discountFactor: 0.99,
        successThreshold: 70,
      }

      setPolicies((prev) => new Map(prev).set(policy.id, policy))
      return policy
    },
    []
  )

  const updatePolicy = useCallback(
    (policyId: string, updates: Partial<Omit<LearningPolicy, "id" | "name">>) => {
      setPolicies((prev) => {
        const policy = prev.get(policyId)
        if (!policy) return prev

        return new Map(prev).set(policyId, { ...policy, ...updates })
      })
    },
    []
  )

  const selectAction = useCallback(
    (
      policyId: string,
      availableActions: string[],
      actionValues: Record<string, number>
    ): string => {
      const policy = policies.get(policyId)
      if (!policy) return availableActions[0]

      // Epsilon-greedy strategy
      if (Math.random() < policy.explorationRate) {
        // Exploration: random action
        return availableActions[Math.floor(Math.random() * availableActions.length)]
      } else {
        // Exploitation: best value action
        let bestAction = availableActions[0]
        let bestValue = actionValues[bestAction] || 0

        availableActions.forEach((action) => {
          const value = actionValues[action] || 0
          if (value > bestValue) {
            bestValue = value
            bestAction = action
          }
        })

        return bestAction
      }
    },
    [policies]
  )

  // ========================================================================
  // ADAPTIVE STRATEGIES
  // ========================================================================

  const createAdaptiveStrategy = useCallback((taskType: string): AdaptiveStrategy => {
    const strategy: AdaptiveStrategy = {
      id: `strategy-${Date.now()}`,
      name: `Strategy for ${taskType}`,
      taskType,
      successRate: 0.5,
      adaptationRate: 0.1,
      lastUpdated: Date.now(),
      parameters: {
        timeout: 5000,
        maxRetries: 3,
        parallelization: 1,
      },
    }

    setStrategies((prev) => new Map(prev).set(strategy.id, strategy))
    return strategy
  }, [])

  const updateStrategyPerformance = useCallback((strategyId: string, success: boolean) => {
    setStrategies((prev) => {
      const strategy = prev.get(strategyId)
      if (!strategy) return prev

      const newSuccessRate =
        strategy.successRate * 0.9 + (success ? 1 : 0) * 0.1 // Exponential moving average

      return new Map(prev).set(strategyId, {
        ...strategy,
        successRate: newSuccessRate,
        lastUpdated: Date.now(),
      })
    })
  }, [])

  const adaptStrategyParameters = useCallback((strategyId: string) => {
    setStrategies((prev) => {
      const strategy = prev.get(strategyId)
      if (!strategy) return prev

      const newParams = { ...strategy.parameters }

      // Adapt parameters based on success rate
      if (strategy.successRate < 0.6) {
        // Low success: increase retries and timeouts
        newParams.maxRetries = Math.min(5, (newParams.maxRetries || 3) + 1)
        newParams.timeout = Math.min(15000, (newParams.timeout || 5000) + 1000)
      } else if (strategy.successRate > 0.85) {
        // High success: be more aggressive
        newParams.maxRetries = Math.max(1, (newParams.maxRetries || 3) - 1)
        newParams.timeout = Math.max(1000, (newParams.timeout || 5000) - 500)
        newParams.parallelization = Math.min(10, (newParams.parallelization || 1) + 1)
      }

      return new Map(prev).set(strategyId, {
        ...strategy,
        parameters: newParams,
      })
    })
  }, [])

  // ========================================================================
  // POLICY GRADIENTS & OPTIMIZATION
  // ========================================================================

  const computePolicyGradient = useCallback((agentId: string): number => {
    const agentExperiences = Array.from(experiences.values()).filter((e) => e.agentId === agentId)

    if (agentExperiences.length === 0) return 0

    // Compute gradient as average advantage
    let totalAdvantage = 0
    agentExperiences.forEach((exp) => {
      const advantage = exp.reward // Simplified: just the immediate reward
      totalAdvantage += advantage
    })

    const gradient = totalAdvantage / agentExperiences.length
    setPolicyGradients((prev) => new Map(prev).set(agentId, gradient))

    return gradient
  }, [experiences])

  const optimizePolicies = useCallback(() => {
    const optimizationResults: Record<string, number> = {}

    policyGradients.forEach((gradient, agentId) => {
      optimizationResults[agentId] = gradient
    })

    return optimizationResults
  }, [policyGradients])

  // ========================================================================
  // LEARNING ANALYTICS
  // ========================================================================

  const getLearningProgress = useCallback(
    (agentId: string, windowSize = 100) => {
      const agentExperiences = Array.from(experiences.values())
        .filter((e) => e.agentId === agentId)
        .sort((a, b) => b.timestamp - a.timestamp)
        .slice(0, windowSize)

      if (agentExperiences.length === 0) {
        return {
          averageReward: 0,
          successRate: 0,
          improvementTrend: 0,
        }
      }

      const averageReward = agentExperiences.reduce((sum, e) => sum + e.reward, 0) / agentExperiences.length

      const successCount = agentExperiences.filter((e) => e.reward > 0).length
      const successRate = successCount / agentExperiences.length

      // Trend: compare first half vs second half
      const firstHalf = agentExperiences.slice(0, Math.ceil(agentExperiences.length / 2))
      const secondHalf = agentExperiences.slice(Math.ceil(agentExperiences.length / 2))

      const firstHalfAvg = firstHalf.reduce((sum, e) => sum + e.reward, 0) / firstHalf.length
      const secondHalfAvg = secondHalf.reduce((sum, e) => sum + e.reward, 0) / secondHalf.length

      const improvementTrend = secondHalfAvg - firstHalfAvg

      return {
        averageReward,
        successRate,
        improvementTrend,
      }
    },
    [experiences]
  )

  const getCollectiveInsights = useCallback(() => {
    const allExperiences = Array.from(experiences.values())

    if (allExperiences.length === 0) {
      return {
        mostSuccessfulSkill: "none",
        averageSwarmReward: 0,
        skillDiversification: 0,
        topPerformers: [],
      }
    }

    const avgReward = allExperiences.reduce((sum, e) => sum + e.reward, 0) / allExperiences.length

    // Find most successful skill
    const skillSuccesses: Record<string, { success: number; total: number }> = {}
    allExperiences.forEach((exp) => {
      if (!skillSuccesses[exp.action]) {
        skillSuccesses[exp.action] = { success: 0, total: 0 }
      }
      skillSuccesses[exp.action].total++
      if (exp.reward > 0) skillSuccesses[exp.action].success++
    })

    let mostSuccessfulSkill = "none"
    let bestSuccessRate = 0
    Object.entries(skillSuccesses).forEach(([skill, { success, total }]) => {
      const rate = success / total
      if (rate > bestSuccessRate) {
        bestSuccessRate = rate
        mostSuccessfulSkill = skill
      }
    })

    // Agent performance ranking
    const agentPerformance: Record<string, number> = {}
    allExperiences.forEach((exp) => {
      if (!agentPerformance[exp.agentId]) {
        agentPerformance[exp.agentId] = 0
      }
      agentPerformance[exp.agentId] += exp.reward
    })

    const topPerformers = Object.entries(agentPerformance)
      .sort(([, a], [, b]) => b - a)
      .slice(0, 5)
      .map(([agent, score]) => ({ agent, score }))

    return {
      mostSuccessfulSkill,
      averageSwarmReward: avgReward,
      skillDiversification: Object.keys(skillSuccesses).length,
      topPerformers,
    }
  }, [experiences])

  // ========================================================================
  // RETURN PUBLIC API
  // ========================================================================

  return {
    // Experience Management
    recordExperience,
    getExperienceReplay,
    computeReturnEstimate,

    // Skill Management
    registerSkill,
    updateSkillSuccess,
    transferSkill,
    getAgentSkills,
    getSkills: () => Object.fromEntries(skills),

    // Learning Policies
    createLearningPolicy,
    updatePolicy,
    selectAction,
    getPolicies: () => Object.fromEntries(policies),

    // Adaptive Strategies
    createAdaptiveStrategy,
    updateStrategyPerformance,
    adaptStrategyParameters,
    getStrategies: () => Object.fromEntries(strategies),

    // Optimization
    computePolicyGradient,
    optimizePolicies,

    // Analytics
    getLearningProgress,
    getCollectiveInsights,
    getSkillTransfers: () => skillTransfers,
  }
}
