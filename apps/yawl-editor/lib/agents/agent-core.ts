/**
 * AI Agent Swarm Framework - Core Agent System
 * 2028+ Innovation Phase: Distributed Intelligence & Emergent Behavior
 *
 * Provides foundational agent architecture for building intelligent swarms
 * with multi-agent coordination, consensus, self-healing, and learning
 */

/**
 * Agent Status enumeration
 */
export enum AgentStatus {
  IDLE = 'idle',
  ACTIVE = 'active',
  PROCESSING = 'processing',
  LEARNING = 'learning',
  CORRUPTED = 'corrupted',
  RECOVERING = 'recovering',
  TERMINATED = 'terminated',
}

/**
 * Agent capability descriptor
 */
export interface AgentCapability {
  name: string
  description: string
  version: string
  requirements?: string[]
  performanceRating: number // 0-100
  energyCost: number // Relative energy consumption
}

/**
 * Agent knowledge item
 */
export interface AgentKnowledge {
  id: string
  domain: string
  topic: string
  confidence: number // 0-1
  source: 'learned' | 'shared' | 'external'
  timestamp: Date
  metadata?: Record<string, any>
}

/**
 * Agent task
 */
export interface AgentTask {
  id: string
  type: string
  priority: 'low' | 'normal' | 'high' | 'critical'
  goal: string
  status: 'pending' | 'assigned' | 'executing' | 'completed' | 'failed'
  estimatedDuration?: number
  deadline?: Date
  reward?: number
  assignedAgent?: string
  result?: any
  error?: string
}

/**
 * Agent message
 */
export interface AgentMessage {
  id: string
  from: string
  to: string | string[] // Single or broadcast
  type: 'command' | 'query' | 'response' | 'broadcast' | 'alert'
  content: any
  timestamp: Date
  priority: number // 0-10
  requiresAck?: boolean
  correlationId?: string // Link to related messages
  ttl?: number // Time to live in milliseconds
}

/**
 * Agent state snapshot
 */
export interface AgentState {
  id: string
  status: AgentStatus
  energyLevel: number // 0-100
  healthScore: number // 0-100
  reputation: number // 0-100
  knowledgeSize: number
  tasksCompleted: number
  tasksFaild: number
  averageTaskDuration: number
  successRate: number // 0-1
  lastUpdated: Date
  metadata?: Record<string, any>
}

/**
 * Core Agent Class
 * Foundation for all agents in the swarm
 */
export class Agent {
  private id: string
  private name: string
  private role: string
  private status: AgentStatus = AgentStatus.IDLE
  private capabilities: Map<string, AgentCapability> = new Map()
  private knowledge: Map<string, AgentKnowledge> = new Map()
  private tasks: Map<string, AgentTask> = new Map()
  private messages: AgentMessage[] = []
  private energyLevel: number = 100
  private healthScore: number = 100
  private reputation: number = 50
  private tasksCompleted: number = 0
  private tasksFailed: number = 0
  private listeners: Map<string, Set<Function>> = new Map()
  private memoryBuffer: any[] = []
  private maxMemory: number = 1000
  private createdAt: Date = new Date()
  private lastActivity: Date = new Date()

  constructor(
    id: string,
    name: string,
    role: string,
    initialCapabilities?: AgentCapability[]
  ) {
    this.id = id
    this.name = name
    this.role = role

    if (initialCapabilities) {
      initialCapabilities.forEach((cap) => {
        this.capabilities.set(cap.name, cap)
      })
    }
  }

  // ===== Identity & Basic Properties =====

  getId(): string {
    return this.id
  }

  getName(): string {
    return this.name
  }

  getRole(): string {
    return this.role
  }

  getStatus(): AgentStatus {
    return this.status
  }

  setStatus(status: AgentStatus): void {
    this.status = status
    this.lastActivity = new Date()
    this.emit('statusChanged', { from: this.status, to: status })
  }

  // ===== Capability Management =====

  addCapability(capability: AgentCapability): void {
    this.capabilities.set(capability.name, capability)
    this.emit('capabilityAdded', { capability })
  }

  removeCapability(name: string): void {
    this.capabilities.delete(name)
    this.emit('capabilityRemoved', { name })
  }

  getCapabilities(): AgentCapability[] {
    return Array.from(this.capabilities.values())
  }

  hasCapability(name: string): boolean {
    return this.capabilities.has(name)
  }

  getCapabilityRating(name: string): number {
    return this.capabilities.get(name)?.performanceRating ?? 0
  }

  // ===== Knowledge Management =====

  addKnowledge(knowledge: AgentKnowledge): void {
    this.knowledge.set(knowledge.id, knowledge)

    // Manage memory buffer
    if (this.memoryBuffer.length >= this.maxMemory) {
      this.memoryBuffer.shift() // Remove oldest
    }
    this.memoryBuffer.push({
      type: 'knowledge',
      content: knowledge,
      timestamp: Date.now(),
    })

    this.emit('knowledgeAdded', { knowledge })
  }

  getKnowledge(id: string): AgentKnowledge | undefined {
    return this.knowledge.get(id)
  }

  getKnowledgeByDomain(domain: string): AgentKnowledge[] {
    return Array.from(this.knowledge.values()).filter((k) => k.domain === domain)
  }

  getKnowledgeSize(): number {
    return this.knowledge.size
  }

  shareKnowledge(agentId: string, knowledge: AgentKnowledge): AgentMessage {
    const message: AgentMessage = {
      id: `msg-${Date.now()}-${Math.random()}`,
      from: this.id,
      to: agentId,
      type: 'broadcast',
      content: { type: 'knowledge_share', knowledge },
      timestamp: new Date(),
      priority: 5,
    }

    this.messages.push(message)
    this.emit('messageSent', { message })
    return message
  }

  // ===== Task Management =====

  createTask(task: Omit<AgentTask, 'id' | 'status'>): AgentTask {
    const fullTask: AgentTask = {
      ...task,
      id: `task-${Date.now()}-${Math.random()}`,
      status: 'pending',
    }

    this.tasks.set(fullTask.id, fullTask)
    this.memoryBuffer.push({
      type: 'task_created',
      content: fullTask,
      timestamp: Date.now(),
    })
    this.emit('taskCreated', { task: fullTask })
    return fullTask
  }

  assignTask(taskId: string, agentId: string): boolean {
    const task = this.tasks.get(taskId)
    if (!task) return false

    task.status = 'assigned'
    task.assignedAgent = agentId
    this.emit('taskAssigned', { taskId, agentId })
    return true
  }

  executeTask(taskId: string): boolean {
    const task = this.tasks.get(taskId)
    if (!task) return false

    this.status = AgentStatus.PROCESSING
    task.status = 'executing'
    this.emit('taskStarted', { taskId })
    return true
  }

  completeTask(taskId: string, result?: any): boolean {
    const task = this.tasks.get(taskId)
    if (!task) return false

    task.status = 'completed'
    task.result = result
    this.tasksCompleted++
    this.reputation = Math.min(100, this.reputation + 2)
    this.status = AgentStatus.IDLE

    this.memoryBuffer.push({
      type: 'task_completed',
      content: task,
      timestamp: Date.now(),
    })

    this.emit('taskCompleted', { taskId, result })
    return true
  }

  failTask(taskId: string, error?: string): boolean {
    const task = this.tasks.get(taskId)
    if (!task) return false

    task.status = 'failed'
    task.error = error
    this.tasksFailed++
    this.reputation = Math.max(0, this.reputation - 5)
    this.status = AgentStatus.IDLE

    this.emit('taskFailed', { taskId, error })
    return true
  }

  getTasks(filter?: { status?: string; priority?: string }): AgentTask[] {
    let tasks = Array.from(this.tasks.values())

    if (filter?.status) {
      tasks = tasks.filter((t) => t.status === filter.status)
    }
    if (filter?.priority) {
      tasks = tasks.filter((t) => t.priority === filter.priority)
    }

    return tasks
  }

  getSuccessRate(): number {
    const total = this.tasksCompleted + this.tasksFailed
    return total === 0 ? 0 : this.tasksCompleted / total
  }

  // ===== Communication =====

  sendMessage(message: Omit<AgentMessage, 'id' | 'timestamp'>): AgentMessage {
    const fullMessage: AgentMessage = {
      ...message,
      id: `msg-${Date.now()}-${Math.random()}`,
      timestamp: new Date(),
    }

    this.messages.push(fullMessage)
    this.memoryBuffer.push({
      type: 'message_sent',
      content: fullMessage,
      timestamp: Date.now(),
    })

    this.emit('messageSent', { message: fullMessage })
    return fullMessage
  }

  receiveMessage(message: AgentMessage): void {
    this.messages.push(message)
    this.memoryBuffer.push({
      type: 'message_received',
      content: message,
      timestamp: Date.now(),
    })

    this.emit('messageReceived', { message })

    // Handle specific message types
    if (message.type === 'command') {
      this.emit('commandReceived', { message })
    } else if (message.type === 'query') {
      this.emit('queryReceived', { message })
    } else if (message.type === 'alert') {
      this.emit('alertReceived', { message })
    }
  }

  getMessages(filter?: { from?: string; type?: string }): AgentMessage[] {
    let msgs = [...this.messages]

    if (filter?.from) {
      msgs = msgs.filter((m) => m.from === filter.from)
    }
    if (filter?.type) {
      msgs = msgs.filter((m) => m.type === filter.type)
    }

    return msgs
  }

  clearMessages(): void {
    this.messages = []
  }

  // ===== Energy & Health Management =====

  getEnergyLevel(): number {
    return this.energyLevel
  }

  consumeEnergy(amount: number): void {
    this.energyLevel = Math.max(0, this.energyLevel - amount)
    if (this.energyLevel === 0) {
      this.status = AgentStatus.IDLE
      this.emit('outOfEnergy', {})
    }
  }

  restoreEnergy(amount: number): void {
    this.energyLevel = Math.min(100, this.energyLevel + amount)
    this.emit('energyRestored', { amount })
  }

  recharge(): void {
    this.energyLevel = 100
  }

  getHealthScore(): number {
    return this.healthScore
  }

  takeDamage(amount: number): void {
    this.healthScore = Math.max(0, this.healthScore - amount)
    if (this.healthScore <= 50) {
      this.status = AgentStatus.RECOVERING
    }
    this.emit('healthDegraded', { amount, currentHealth: this.healthScore })
  }

  heal(amount: number): void {
    this.healthScore = Math.min(100, this.healthScore + amount)
    if (this.healthScore > 50 && this.status === AgentStatus.RECOVERING) {
      this.status = AgentStatus.IDLE
    }
    this.emit('healed', { amount })
  }

  // ===== Reputation System =====

  getReputation(): number {
    return this.reputation
  }

  adjustReputation(delta: number): void {
    this.reputation = Math.max(0, Math.min(100, this.reputation + delta))
    this.emit('reputationChanged', { reputation: this.reputation })
  }

  // ===== State & Metrics =====

  getState(): AgentState {
    return {
      id: this.id,
      status: this.status,
      energyLevel: this.energyLevel,
      healthScore: this.healthScore,
      reputation: this.reputation,
      knowledgeSize: this.knowledge.size,
      tasksCompleted: this.tasksCompleted,
      tasksFaild: this.tasksFailed,
      averageTaskDuration: 0, // Calculate based on memory
      successRate: this.getSuccessRate(),
      lastUpdated: new Date(),
    }
  }

  getMetrics(): Record<string, any> {
    const completedTasks = this.getTasks({ status: 'completed' })
    const activeTasks = this.getTasks({ status: 'executing' })
    const pendingTasks = this.getTasks({ status: 'pending' })

    return {
      id: this.id,
      name: this.name,
      role: this.role,
      uptime: Date.now() - this.createdAt.getTime(),
      lastActivity: this.lastActivity,
      capabilities: this.capabilities.size,
      knowledge: this.knowledge.size,
      memory: this.memoryBuffer.length,
      completedTasks: this.tasksCompleted,
      failedTasks: this.tasksFailed,
      activeTasks: activeTasks.length,
      pendingTasks: pendingTasks.length,
      successRate: this.getSuccessRate(),
      energyLevel: this.energyLevel,
      healthScore: this.healthScore,
      reputation: this.reputation,
      averageCapabilityRating:
        Array.from(this.capabilities.values()).reduce((sum, c) => sum + c.performanceRating, 0) /
        (this.capabilities.size || 1),
    }
  }

  // ===== Memory Management =====

  getMemory(limit?: number): any[] {
    if (limit) {
      return this.memoryBuffer.slice(-limit)
    }
    return [...this.memoryBuffer]
  }

  clearMemory(): void {
    this.memoryBuffer = []
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

  emit(event: string, data?: any): void {
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
      id: this.id,
      name: this.name,
      role: this.role,
      status: this.status,
      capabilities: Array.from(this.capabilities.values()),
      knowledge: Array.from(this.knowledge.values()),
      tasks: Array.from(this.tasks.values()),
      metrics: this.getMetrics(),
      state: this.getState(),
    }
  }
}

/**
 * Specialized Agent Types
 */

/**
 * Coordinator Agent - Orchestrates other agents
 */
export class CoordinatorAgent extends Agent {
  private coordinatedAgents: Set<string> = new Set()

  constructor(id: string, name: string) {
    super(id, name, 'coordinator')

    this.addCapability({
      name: 'task_distribution',
      description: 'Distribute tasks among agents',
      version: '1.0',
      performanceRating: 90,
      energyCost: 5,
    })

    this.addCapability({
      name: 'conflict_resolution',
      description: 'Resolve conflicts between agents',
      version: '1.0',
      performanceRating: 80,
      energyCost: 8,
    })
  }

  addCoordinatedAgent(agentId: string): void {
    this.coordinatedAgents.add(agentId)
  }

  getCoordinatedAgents(): string[] {
    return Array.from(this.coordinatedAgents)
  }
}

/**
 * Worker Agent - Executes tasks
 */
export class WorkerAgent extends Agent {
  private specialization: string

  constructor(id: string, name: string, specialization: string) {
    super(id, name, 'worker')
    this.specialization = specialization

    this.addCapability({
      name: 'task_execution',
      description: `Execute ${specialization} tasks`,
      version: '1.0',
      performanceRating: 85,
      energyCost: 10,
    })
  }

  getSpecialization(): string {
    return this.specialization
  }
}

/**
 * Scout Agent - Explores and gathers information
 */
export class ScoutAgent extends Agent {
  constructor(id: string, name: string) {
    super(id, name, 'scout')

    this.addCapability({
      name: 'exploration',
      description: 'Explore and gather information',
      version: '1.0',
      performanceRating: 88,
      energyCost: 7,
    })

    this.addCapability({
      name: 'reconnaissance',
      description: 'Perform reconnaissance',
      version: '1.0',
      performanceRating: 85,
      energyCost: 6,
    })
  }
}

/**
 * Learning Agent - Acquires and shares knowledge
 */
export class LearningAgent extends Agent {
  private learningRate: number = 0.1

  constructor(id: string, name: string) {
    super(id, name, 'learner')

    this.addCapability({
      name: 'machine_learning',
      description: 'Learn patterns and improve',
      version: '1.0',
      performanceRating: 92,
      energyCost: 12,
    })

    this.addCapability({
      name: 'knowledge_synthesis',
      description: 'Synthesize knowledge from multiple sources',
      version: '1.0',
      performanceRating: 88,
      energyCost: 8,
    })
  }

  getLearningRate(): number {
    return this.learningRate
  }

  setLearningRate(rate: number): void {
    this.learningRate = Math.max(0, Math.min(1, rate))
  }
}
