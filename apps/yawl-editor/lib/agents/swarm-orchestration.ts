/**
 * Swarm Orchestration Engine
 * Manages multiple agents, coordinates activities, and enables emergent behavior
 *
 * 2028+ Innovation: Distributed Intelligence, Consensus, Self-Organization
 */

import { Agent, AgentTask, AgentMessage, AgentStatus, CoordinatorAgent, WorkerAgent } from './agent-core'

/**
 * Swarm configuration
 */
export interface SwarmConfig {
  maxAgents?: number
  consensusAlgorithm?: 'pbft' | 'raft' | 'paxos'
  selfHealingEnabled?: boolean
  learningEnabled?: boolean
  energyManagement?: boolean
  taskQueueSize?: number
}

/**
 * Swarm metrics
 */
export interface SwarmMetrics {
  totalAgents: number
  activeAgents: number
  totalTasks: number
  completedTasks: number
  failedTasks: number
  avgEnergyLevel: number
  avgHealthScore: number
  avgReputation: number
  swarmEfficiency: number // 0-100
  consensusHealth: number // 0-100
  emergentBehaviorScore: number // 0-100
}

/**
 * Task queue entry with priority
 */
interface QueuedTask {
  task: AgentTask
  priority: number
  timestamp: Date
  retries: number
  maxRetries: number
}

/**
 * Swarm Orchestration Engine
 * Central management system for agent swarms
 */
export class SwarmOrchestrator {
  private swarmId: string
  private agents: Map<string, Agent> = new Map()
  private coordinator: CoordinatorAgent | null = null
  private taskQueue: QueuedTask[] = []
  private config: Required<SwarmConfig>
  private messageRouter: Map<string, AgentMessage[]> = new Map()
  private consensusLog: any[] = []
  private listeners: Map<string, Set<Function>> = new Map()
  private swarmCreatedAt: Date = new Date()
  private emergentPatterns: Map<string, number> = new Map()

  constructor(swarmId: string, config: SwarmConfig = {}) {
    this.swarmId = swarmId
    this.config = {
      maxAgents: config.maxAgents || 100,
      consensusAlgorithm: config.consensusAlgorithm || 'pbft',
      selfHealingEnabled: config.selfHealingEnabled !== false,
      learningEnabled: config.learningEnabled !== false,
      energyManagement: config.energyManagement !== false,
      taskQueueSize: config.taskQueueSize || 10000,
    }
  }

  // ===== Swarm Lifecycle =====

  getSwarmId(): string {
    return this.swarmId
  }

  /**
   * Register an agent in the swarm
   */
  registerAgent(agent: Agent): boolean {
    if (this.agents.size >= this.config.maxAgents) {
      console.warn('Swarm is at maximum capacity')
      return false
    }

    this.agents.set(agent.getId(), agent)

    // Create coordinator if this is first agent
    if (this.agents.size === 1 && !this.coordinator) {
      this.createCoordinator()
    }

    // Setup message routing
    this.messageRouter.set(agent.getId(), [])

    // Setup event listeners
    agent.on('messageSent', (data: any) => {
      this.routeMessage(data.message)
    })

    agent.on('taskCompleted', (data: any) => {
      this.emit('taskCompleted', { agentId: agent.getId(), ...data })
    })

    this.emit('agentRegistered', { agentId: agent.getId() })
    return true
  }

  /**
   * Unregister an agent from the swarm
   */
  unregisterAgent(agentId: string): boolean {
    const agent = this.agents.get(agentId)
    if (!agent) return false

    agent.setStatus(AgentStatus.TERMINATED)
    this.agents.delete(agentId)
    this.messageRouter.delete(agentId)

    this.emit('agentUnregistered', { agentId })
    return true
  }

  /**
   * Get agent by ID
   */
  getAgent(agentId: string): Agent | undefined {
    return this.agents.get(agentId)
  }

  /**
   * Get all agents
   */
  getAgents(filter?: { role?: string; status?: AgentStatus }): Agent[] {
    let result = Array.from(this.agents.values())

    if (filter?.role) {
      result = result.filter((a) => a.getRole() === filter.role)
    }
    if (filter?.status) {
      result = result.filter((a) => a.getStatus() === filter.status)
    }

    return result
  }

  /**
   * Create and register coordinator
   */
  private createCoordinator(): void {
    const coordinator = new CoordinatorAgent(`coord-${this.swarmId}`, `Swarm Coordinator`)
    this.registerAgent(coordinator)
    this.coordinator = coordinator
  }

  // ===== Task Management =====

  /**
   * Queue a task for the swarm
   */
  queueTask(task: AgentTask): boolean {
    if (this.taskQueue.length >= this.config.taskQueueSize) {
      console.warn('Task queue is full')
      return false
    }

    const queuedTask: QueuedTask = {
      task,
      priority: task.priority === 'critical' ? 10 : task.priority === 'high' ? 7 : task.priority === 'normal' ? 5 : 2,
      timestamp: new Date(),
      retries: 0,
      maxRetries: 3,
    }

    // Insert in priority order
    let inserted = false
    for (let i = this.taskQueue.length - 1; i >= 0; i--) {
      if (this.taskQueue[i].priority >= queuedTask.priority) {
        this.taskQueue.splice(i + 1, 0, queuedTask)
        inserted = true
        break
      }
    }

    if (!inserted) {
      this.taskQueue.unshift(queuedTask)
    }

    this.emit('taskQueued', { task })
    this.distributeNextTask()
    return true
  }

  /**
   * Distribute next task from queue
   */
  private distributeNextTask(): void {
    if (this.taskQueue.length === 0 || !this.coordinator) return

    const queuedTask = this.taskQueue.shift()
    if (!queuedTask) return

    const suitableAgent = this.findSuitableAgent(queuedTask.task)
    if (!suitableAgent) {
      // Requeue if no suitable agent
      this.taskQueue.unshift(queuedTask)
      return
    }

    // Assign and execute
    const taskId = queuedTask.task.id
    suitableAgent.executeTask(taskId)
    this.emit('taskAssigned', { taskId, agentId: suitableAgent.getId() })
  }

  /**
   * Find most suitable agent for a task
   */
  private findSuitableAgent(task: AgentTask): Agent | null {
    const activeAgents = this.getAgents({ status: AgentStatus.IDLE }).sort((a, b) => {
      // Sort by: reputation, energy, health
      const scoreA =
        a.getReputation() * 0.5 + (a.getEnergyLevel() / 100) * 30 + (a.getHealthScore() / 100) * 20
      const scoreB =
        b.getReputation() * 0.5 + (b.getEnergyLevel() / 100) * 30 + (b.getHealthScore() / 100) * 20
      return scoreB - scoreA
    })

    return activeAgents[0] || null
  }

  /**
   * Get task queue status
   */
  getTaskQueueStatus(): { queued: number; total: number; capacity: number } {
    return {
      queued: this.taskQueue.length,
      total: this.taskQueue.reduce((sum, qt) => sum + 1, 0),
      capacity: this.config.taskQueueSize,
    }
  }

  // ===== Message Routing =====

  /**
   * Route message to recipient(s)
   */
  private routeMessage(message: AgentMessage): void {
    const recipients = Array.isArray(message.to) ? message.to : [message.to]

    recipients.forEach((recipientId) => {
      const recipient = this.getAgent(recipientId)
      if (recipient) {
        recipient.receiveMessage(message)

        if (this.messageRouter.has(recipientId)) {
          this.messageRouter.get(recipientId)!.push(message)
        }
      }
    })

    // Log for consensus
    if (message.type === 'broadcast') {
      this.consensusLog.push({
        timestamp: new Date(),
        message: message.id,
        from: message.from,
        count: recipients.length,
      })
    }
  }

  // ===== Consensus Mechanisms =====

  /**
   * Reach consensus on a decision
   */
  async reachConsensus(proposal: any, voters?: string[]): Promise<boolean> {
    if (!this.coordinator) return false

    const agentsToVote = voters || this.getAgents().map((a) => a.getId()).slice(0, 5) // Max 5 voters

    const votes: Map<string, boolean> = new Map()
    const quorum = Math.ceil(agentsToVote.length * 0.67) // 2/3 majority for PBFT

    // Request votes
    for (const agentId of agentsToVote) {
      const agent = this.getAgent(agentId)
      if (agent) {
        // Simulate voting based on reputation and health
        const score = (agent.getReputation() + agent.getHealthScore()) / 2
        votes.set(agentId, score > 50)
      }
    }

    const yesVotes = Array.from(votes.values()).filter((v) => v).length
    const consensus = yesVotes >= quorum

    this.consensusLog.push({
      timestamp: new Date(),
      proposal: proposal.id || 'unknown',
      voters: agentsToVote.length,
      yesVotes,
      quorum,
      consensus,
    })

    this.emit('consensusReached', { proposal, consensus, yesVotes, quorum })
    return consensus
  }

  /**
   * Get consensus health
   */
  getConsensusHealth(): number {
    if (this.consensusLog.length === 0) return 100

    const recentLogs = this.consensusLog.slice(-100)
    const consensusCount = recentLogs.filter((log) => log.consensus).length

    return (consensusCount / recentLogs.length) * 100
  }

  // ===== Self-Healing System =====

  /**
   * Monitor swarm health and trigger healing
   */
  async monitorAndHeal(): Promise<void> {
    if (!this.config.selfHealingEnabled) return

    const agents = this.getAgents()

    for (const agent of agents) {
      const health = agent.getHealthScore()
      const energy = agent.getEnergyLevel()

      // Healing actions
      if (health < 40) {
        agent.heal(30)
        this.emit('agentHealed', { agentId: agent.getId() })
      }

      if (energy < 20) {
        agent.restoreEnergy(50)
        this.emit('agentRecharged', { agentId: agent.getId() })
      }

      // Corruption detection
      if (health === 0 || agent.getStatus() === AgentStatus.CORRUPTED) {
        if (Math.random() < 0.1) {
          // 10% chance to recover
          agent.setStatus(AgentStatus.RECOVERING)
          agent.heal(50)
          this.emit('agentRecovered', { agentId: agent.getId() })
        }
      }
    }
  }

  /**
   * Detect anomalies in swarm behavior
   */
  detectAnomalies(): Map<string, string[]> {
    const anomalies: Map<string, string[]> = new Map()

    const agents = this.getAgents()
    for (const agent of agents) {
      const issues: string[] = []
      const metrics = agent.getMetrics()

      if (metrics.successRate < 0.5) {
        issues.push('Low success rate')
      }
      if (agent.getReputation() < 20) {
        issues.push('Poor reputation')
      }
      if (agent.getHealthScore() < 30) {
        issues.push('Critical health')
      }
      if (agent.getEnergyLevel() < 10) {
        issues.push('Low energy')
      }

      if (issues.length > 0) {
        anomalies.set(agent.getId(), issues)
      }
    }

    return anomalies
  }

  // ===== Swarm Intelligence & Learning =====

  /**
   * Detect emergent patterns in swarm behavior
   */
  detectEmergentPatterns(): Map<string, number> {
    if (!this.config.learningEnabled) return new Map()

    const patterns: Map<string, number> = new Map()

    // Analyze collaboration patterns
    const agents = this.getAgents()
    const collaborationScore = agents.filter((a) => a.getMessages().length > 5).length / agents.length

    patterns.set('collaboration', collaborationScore)

    // Analyze specialization
    const roles = new Set(agents.map((a) => a.getRole()))
    patterns.set('specialization', roles.size / agents.length)

    // Analyze efficiency
    const efficiency =
      agents.reduce((sum, a) => sum + a.getMetrics().successRate, 0) / (agents.length || 1)
    patterns.set('efficiency', efficiency)

    // Analyze autonomy
    const autonomousCount = agents.filter((a) => a.getStatus() === AgentStatus.LEARNING).length
    patterns.set('autonomy', autonomousCount / agents.length)

    this.emergentPatterns = patterns
    return patterns
  }

  /**
   * Get emergent behavior score
   */
  getEmergentBehaviorScore(): number {
    const patterns = Array.from(this.emergentPatterns.values())
    return patterns.length === 0 ? 0 : (patterns.reduce((sum, v) => sum + v, 0) / patterns.length) * 100
  }

  /**
   * Trigger swarm-wide learning
   */
  triggerSwarmLearning(): void {
    if (!this.config.learningEnabled) return

    const agents = this.getAgents()

    // Share knowledge between agents
    for (let i = 0; i < agents.length - 1; i++) {
      const sourceAgent = agents[i]
      const targetAgent = agents[i + 1]
      const knowledge = sourceAgent.getKnowledgeByDomain('shared').slice(0, 3)

      knowledge.forEach((k) => {
        const sharedKnowledge = { ...k, source: 'shared' as const, timestamp: new Date() }
        targetAgent.addKnowledge(sharedKnowledge)
      })
    }

    this.emit('swarmLearning', { agentsInvolved: agents.length })
  }

  // ===== Energy Management =====

  /**
   * Manage swarm energy distribution
   */
  manageEnergyDistribution(): void {
    if (!this.config.energyManagement) return

    const agents = this.getAgents()
    const totalEnergy = agents.reduce((sum, a) => sum + a.getEnergyLevel(), 0)
    const avgEnergy = totalEnergy / (agents.length || 1)

    // Redistribute energy from high to low energy agents
    const highEnergyAgents = agents.filter((a) => a.getEnergyLevel() > avgEnergy + 20)
    const lowEnergyAgents = agents.filter((a) => a.getEnergyLevel() < avgEnergy - 20)

    highEnergyAgents.forEach((high) => {
      if (lowEnergyAgents.length > 0) {
        const recipient = lowEnergyAgents[Math.floor(Math.random() * lowEnergyAgents.length)]
        const transfer = 10

        high.consumeEnergy(-transfer) // Negative to restore
        recipient.restoreEnergy(transfer)

        this.emit('energyTransferred', {
          from: high.getId(),
          to: recipient.getId(),
          amount: transfer,
        })
      }
    })
  }

  // ===== Metrics & Analytics =====

  /**
   * Get comprehensive swarm metrics
   */
  getSwarmMetrics(): SwarmMetrics {
    const agents = this.getAgents()

    const activateAgents = agents.filter((a) => a.getStatus() !== AgentStatus.IDLE).length
    const totalTasks = agents.reduce((sum, a) => sum + a.getTasks().length, 0)
    const completedTasks = agents.reduce((sum, a) => {
      return sum + a.getMetrics().completedTasks
    }, 0)
    const failedTasks = agents.reduce((sum, a) => {
      return sum + a.getMetrics().failedTasks
    }, 0)

    const avgEnergy = agents.length > 0 ? agents.reduce((sum, a) => sum + a.getEnergyLevel(), 0) / agents.length : 0
    const avgHealth = agents.length > 0 ? agents.reduce((sum, a) => sum + a.getHealthScore(), 0) / agents.length : 0
    const avgReputation =
      agents.length > 0 ? agents.reduce((sum, a) => sum + a.getReputation(), 0) / agents.length : 0

    const swarmEfficiency =
      totalTasks === 0 ? 0 : (completedTasks / (completedTasks + failedTasks || 1)) * 100

    return {
      totalAgents: agents.length,
      activeAgents: activateAgents,
      totalTasks,
      completedTasks,
      failedTasks,
      avgEnergyLevel: avgEnergy,
      avgHealthScore: avgHealth,
      avgReputation,
      swarmEfficiency,
      consensusHealth: this.getConsensusHealth(),
      emergentBehaviorScore: this.getEmergentBehaviorScore(),
    }
  }

  /**
   * Get detailed swarm report
   */
  getSwarmReport(): any {
    const metrics = this.getSwarmMetrics()
    const anomalies = this.detectAnomalies()
    const patterns = this.detectEmergentPatterns()

    return {
      swarmId: this.swarmId,
      uptime: Date.now() - this.swarmCreatedAt.getTime(),
      metrics,
      anomalies: Array.from(anomalies.entries()),
      emergentPatterns: Array.from(patterns.entries()),
      agentDetails: this.getAgents().map((a) => ({
        id: a.getId(),
        name: a.getName(),
        role: a.getRole(),
        status: a.getStatus(),
        metrics: a.getMetrics(),
      })),
      timestamp: new Date().toISOString(),
    }
  }

  // ===== Event System =====

  on(event: string, callback: Function): void {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, new Set())
    }
    this.listeners.get(event)!.add(callback)
  }

  off(event: string, callback: Function): void {
    const listeners = this.listeners.get(event)
    if (listeners) {
      listeners.delete(callback)
    }
  }

  private emit(event: string, data?: any): void {
    const listeners = this.listeners.get(event)
    if (listeners) {
      listeners.forEach((callback) => {
        try {
          callback(data)
        } catch (error) {
          console.error(`Error in ${event} listener:`, error)
        }
      })
    }
  }

  // ===== Serialization =====

  toJSON() {
    return {
      swarmId: this.swarmId,
      uptime: Date.now() - this.swarmCreatedAt.getTime(),
      config: this.config,
      metrics: this.getSwarmMetrics(),
      agents: this.getAgents().map((a) => a.toJSON()),
      taskQueue: this.taskQueue.length,
      consensusHealth: this.getConsensusHealth(),
    }
  }
}
