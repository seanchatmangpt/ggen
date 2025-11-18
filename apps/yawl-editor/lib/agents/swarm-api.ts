/**
 * Agent Swarm API Integration
 * High-level API for swarm operations and management
 *
 * 2028+ Innovation: Swarm-as-a-Service Interface
 */

import { Agent, AgentTask, CoordinatorAgent, WorkerAgent, ScoutAgent, LearningAgent } from './agent-core'
import { SwarmOrchestrator, SwarmMetrics } from './swarm-orchestration'
import {
  PBFTConsensus,
  RaftConsensus,
  ConsensusManager,
  ConsensusResult,
} from './consensus-mechanisms'
import {
  ParticleSwarmOptimization,
  AntColonyOptimization,
  GeneticAlgorithm,
  SwarmBehaviorAnalyzer,
} from './swarm-intelligence'

/**
 * Swarm API - Unified interface for swarm operations
 */
export class SwarmAPI {
  private swarms: Map<string, SwarmOrchestrator> = new Map()
  private consensusManagers: Map<string, ConsensusManager> = new Map()
  private psoEngines: Map<string, ParticleSwarmOptimization> = new Map()
  private acoEngines: Map<string, AntColonyOptimization> = new Map()
  private gaEngines: Map<string, GeneticAlgorithm> = new Map()
  private behaviorAnalyzers: Map<string, SwarmBehaviorAnalyzer> = new Map()

  /**
   * Create new swarm
   */
  createSwarm(swarmId: string, config?: any): SwarmOrchestrator {
    if (this.swarms.has(swarmId)) {
      throw new Error(`Swarm ${swarmId} already exists`)
    }

    const swarm = new SwarmOrchestrator(swarmId, config)
    this.swarms.set(swarmId, swarm)

    // Initialize associated components
    const consensus = new ConsensusManager(swarmId, [], config?.consensusAlgorithm || 'pbft')
    this.consensusManagers.set(swarmId, consensus)

    const analyzer = new SwarmBehaviorAnalyzer()
    this.behaviorAnalyzers.set(swarmId, analyzer)

    return swarm
  }

  /**
   * Get swarm by ID
   */
  getSwarm(swarmId: string): SwarmOrchestrator | undefined {
    return this.swarms.get(swarmId)
  }

  /**
   * List all swarms
   */
  listSwarms(): string[] {
    return Array.from(this.swarms.keys())
  }

  /**
   * Delete swarm
   */
  deleteSwarm(swarmId: string): boolean {
    return (
      this.swarms.delete(swarmId) &&
      this.consensusManagers.delete(swarmId) &&
      this.behaviorAnalyzers.delete(swarmId)
    )
  }

  // ===== Agent Management =====

  /**
   * Create and register agent
   */
  createAgent(
    swarmId: string,
    agentType: 'coordinator' | 'worker' | 'scout' | 'learner',
    agentId: string,
    name: string,
    options?: any
  ): Agent | null {
    const swarm = this.getSwarm(swarmId)
    if (!swarm) return null

    let agent: Agent

    switch (agentType) {
      case 'coordinator':
        agent = new CoordinatorAgent(agentId, name)
        break
      case 'worker':
        agent = new WorkerAgent(agentId, name, options?.specialization || 'general')
        break
      case 'scout':
        agent = new ScoutAgent(agentId, name)
        break
      case 'learner':
        agent = new LearningAgent(agentId, name)
        if (options?.learningRate) {
          agent.setLearningRate(options.learningRate)
        }
        break
      default:
        return null
    }

    swarm.registerAgent(agent)
    return agent
  }

  /**
   * Get agent from swarm
   */
  getAgent(swarmId: string, agentId: string): Agent | undefined {
    const swarm = this.getSwarm(swarmId)
    return swarm?.getAgent(agentId)
  }

  /**
   * List agents in swarm
   */
  listAgents(
    swarmId: string,
    filter?: { role?: string; status?: string }
  ): Agent[] {
    const swarm = this.getSwarm(swarmId)
    if (!swarm) return []

    return swarm.getAgents(filter as any)
  }

  /**
   * Remove agent from swarm
   */
  removeAgent(swarmId: string, agentId: string): boolean {
    const swarm = this.getSwarm(swarmId)
    return swarm?.unregisterAgent(agentId) || false
  }

  // ===== Task Management =====

  /**
   * Queue task in swarm
   */
  queueTask(swarmId: string, task: Omit<AgentTask, 'id' | 'status'>): boolean {
    const swarm = this.getSwarm(swarmId)
    if (!swarm) return false

    const fullTask: AgentTask = {
      ...task,
      id: `task-${Date.now()}-${Math.random()}`,
      status: 'pending',
    }

    return swarm.queueTask(fullTask)
  }

  /**
   * Get task queue status
   */
  getTaskQueueStatus(swarmId: string) {
    const swarm = this.getSwarm(swarmId)
    if (!swarm) return null

    return swarm.getTaskQueueStatus()
  }

  // ===== Consensus Operations =====

  /**
   * Request consensus in swarm
   */
  async requestConsensus(swarmId: string, proposal: any): Promise<ConsensusResult | null> {
    const manager = this.consensusManagers.get(swarmId)
    if (!manager) return null

    return manager.requestConsensus(proposal)
  }

  /**
   * Get consensus health
   */
  getConsensusHealth(swarmId: string): number {
    const swarm = this.getSwarm(swarmId)
    return swarm?.getConsensusHealth() || 0
  }

  /**
   * Report suspicious behavior
   */
  reportSuspicious(
    swarmId: string,
    reporterId: string,
    suspectId: string,
    reason: string,
    severity: number
  ): void {
    const manager = this.consensusManagers.get(swarmId)
    if (manager) {
      manager.reportSuspicious(reporterId, suspectId, reason, severity)
    }
  }

  /**
   * Get Byzantine fault report
   */
  getByzantineReport(swarmId: string) {
    const manager = this.consensusManagers.get(swarmId)
    return manager?.getByzantineReport() || null
  }

  // ===== Optimization & Intelligence =====

  /**
   * Run Particle Swarm Optimization
   */
  optimizeWithPSO(
    swarmId: string,
    agentIds: string[],
    dimensions?: number,
    iterations?: number
  ): { bestValue: number; bestPosition: number[] } | null {
    const key = `${swarmId}-pso`

    if (!this.psoEngines.has(key)) {
      this.psoEngines.set(key, new ParticleSwarmOptimization(agentIds, dimensions || 5))
    }

    const pso = this.psoEngines.get(key)!
    return pso.optimize(iterations || 100)
  }

  /**
   * Run Ant Colony Optimization
   */
  optimizeWithACO(swarmId: string, nodeIds: string[], iterations?: number, antCount?: number): number | null {
    const key = `${swarmId}-aco`

    if (!this.acoEngines.has(key)) {
      this.acoEngines.set(key, new AntColonyOptimization(nodeIds))
    }

    const aco = this.acoEngines.get(key)!
    return aco.optimize(iterations || 50, antCount || 10)
  }

  /**
   * Run Genetic Algorithm
   */
  evolveWithGA(swarmId: string, agentIds: string[], generations?: number): { bestGenes: number[]; bestFitness: number } | null {
    const key = `${swarmId}-ga`

    if (!this.gaEngines.has(key)) {
      this.gaEngines.set(key, new GeneticAlgorithm(agentIds))
    }

    const ga = this.gaEngines.get(key)!
    return ga.runEvolution(generations || 50)
  }

  /**
   * Detect emergent patterns
   */
  detectEmergentPatterns(swarmId: string): Map<string, number> | null {
    const swarm = this.getSwarm(swarmId)
    return swarm?.detectEmergentPatterns() || null
  }

  /**
   * Get emergent behavior score
   */
  getEmergentBehaviorScore(swarmId: string): number {
    const swarm = this.getSwarm(swarmId)
    return swarm?.getEmergentBehaviorScore() || 0
  }

  /**
   * Trigger swarm learning
   */
  triggerLearning(swarmId: string): void {
    const swarm = this.getSwarm(swarmId)
    swarm?.triggerSwarmLearning()
  }

  // ===== Health & Healing =====

  /**
   * Monitor and heal swarm
   */
  async monitorAndHeal(swarmId: string): Promise<void> {
    const swarm = this.getSwarm(swarmId)
    if (swarm) {
      await swarm.monitorAndHeal()
    }
  }

  /**
   * Detect anomalies
   */
  detectAnomalies(swarmId: string): Map<string, string[]> | null {
    const swarm = this.getSwarm(swarmId)
    return swarm?.detectAnomalies() || null
  }

  /**
   * Get swarm health report
   */
  getHealthReport(swarmId: string) {
    const swarm = this.getSwarm(swarmId)
    if (!swarm) return null

    const anomalies = swarm.detectAnomalies()
    const metrics = swarm.getSwarmMetrics()

    return {
      metrics,
      anomalies: Array.from(anomalies.entries()),
      healthStatus: metrics.avgHealthScore > 70 ? 'healthy' : 'degraded',
      needsAttention: anomalies.size > 0,
    }
  }

  // ===== Energy Management =====

  /**
   * Manage energy distribution
   */
  manageEnergy(swarmId: string): void {
    const swarm = this.getSwarm(swarmId)
    swarm?.manageEnergyDistribution()
  }

  /**
   * Get energy status
   */
  getEnergyStatus(swarmId: string) {
    const swarm = this.getSwarm(swarmId)
    if (!swarm) return null

    const agents = swarm.getAgents()
    const energies = agents.map((a) => a.getEnergyLevel())

    return {
      totalAgents: agents.length,
      avgEnergy: energies.reduce((a, b) => a + b, 0) / agents.length,
      minEnergy: Math.min(...energies),
      maxEnergy: Math.max(...energies),
      criticalAgents: agents.filter((a) => a.getEnergyLevel() < 20).length,
    }
  }

  // ===== Metrics & Reporting =====

  /**
   * Get comprehensive swarm metrics
   */
  getSwarmMetrics(swarmId: string): SwarmMetrics | null {
    const swarm = this.getSwarm(swarmId)
    return swarm?.getSwarmMetrics() || null
  }

  /**
   * Get detailed swarm report
   */
  getSwarmReport(swarmId: string) {
    const swarm = this.getSwarm(swarmId)
    return swarm?.getSwarmReport() || null
  }

  /**
   * Get agent metrics
   */
  getAgentMetrics(swarmId: string, agentId: string) {
    const agent = this.getAgent(swarmId, agentId)
    return agent?.getMetrics() || null
  }

  // ===== Event Management =====

  /**
   * Subscribe to swarm events
   */
  onSwarmEvent(swarmId: string, event: string, callback: Function): void {
    const swarm = this.getSwarm(swarmId)
    if (swarm) {
      swarm.on(event, callback)
    }
  }

  /**
   * Subscribe to agent events
   */
  onAgentEvent(swarmId: string, agentId: string, event: string, callback: Function): void {
    const agent = this.getAgent(swarmId, agentId)
    if (agent) {
      agent.on(event, callback)
    }
  }

  // ===== Batch Operations =====

  /**
   * Create swarm with initial agents
   */
  createSwarmWithAgents(
    swarmId: string,
    agentConfigs: Array<{
      type: 'coordinator' | 'worker' | 'scout' | 'learner'
      name: string
      options?: any
    }>,
    swarmConfig?: any
  ): SwarmOrchestrator | null {
    const swarm = this.createSwarm(swarmId, swarmConfig)

    agentConfigs.forEach((config, index) => {
      this.createAgent(swarmId, config.type, `${config.type}-${index}`, config.name, config.options)
    })

    return swarm
  }

  /**
   * Run full swarm cycle
   */
  async runSwarmCycle(swarmId: string, iterations: number = 1): Promise<void> {
    for (let i = 0; i < iterations; i++) {
      // Monitor health
      await this.monitorAndHeal(swarmId)

      // Manage energy
      this.manageEnergy(swarmId)

      // Detect patterns
      this.detectEmergentPatterns(swarmId)

      // Small delay
      await new Promise((resolve) => setTimeout(resolve, 100))
    }
  }

  /**
   * Serialize swarm state
   */
  serializeSwarm(swarmId: string): any {
    const swarm = this.getSwarm(swarmId)
    if (!swarm) return null

    return {
      swarmId,
      report: swarm.getSwarmReport(),
      serialized: swarm.toJSON(),
      timestamp: new Date().toISOString(),
    }
  }

  /**
   * Get API version
   */
  getVersion(): string {
    return '1.0.0'
  }

  /**
   * Get API status
   */
  getStatus() {
    return {
      version: this.getVersion(),
      swarms: this.swarms.size,
      totalAgents: Array.from(this.swarms.values()).reduce((sum, s) => sum + s.getAgents().length, 0),
      timestamp: new Date().toISOString(),
    }
  }
}

/**
 * Global API instance
 */
let globalSwarmAPI: SwarmAPI | null = null

/**
 * Get or create global API instance
 */
export function getSwarmAPI(): SwarmAPI {
  if (!globalSwarmAPI) {
    globalSwarmAPI = new SwarmAPI()
  }
  return globalSwarmAPI
}

/**
 * Initialize swarm API with preset configuration
 */
export function initializeSwarmAPI(config?: any): SwarmAPI {
  globalSwarmAPI = new SwarmAPI()
  return globalSwarmAPI
}
