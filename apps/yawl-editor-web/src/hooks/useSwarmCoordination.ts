import { useState, useCallback, useRef } from "react"

// ============================================================================
// CORE TYPES
// ============================================================================

export interface Agent {
  id: string
  name: string
  role: "worker" | "coordinator" | "observer" | "specialist"
  capability: string[]
  status: "idle" | "busy" | "offline" | "error"
  lastHeartbeat: number
  reputation: number // 0-100
  workload: number // 0-100
}

export interface SwarmTask {
  id: string
  description: string
  priority: "critical" | "high" | "normal" | "low"
  deadline?: number
  requiredCapabilities: string[]
  subtasks: Subtask[]
  status: "pending" | "executing" | "completed" | "failed"
  result?: any
  assignedAgent?: string
  startTime?: number
  endTime?: number
}

export interface Subtask {
  id: string
  description: string
  dependencies: string[]
  status: "pending" | "executing" | "completed" | "failed"
  assignedAgent?: string
}

export interface Message {
  id: string
  from: string
  to?: string // undefined = broadcast
  type: "task" | "query" | "response" | "heartbeat" | "election" | "sync"
  payload: any
  timestamp: number
  priority: number // 0-100
  ttl: number // time to live in ms
}

export interface SwarmState {
  agents: Map<string, Agent>
  tasks: Map<string, SwarmTask>
  messages: Message[]
  coordinator?: string
  reputation: Map<string, number>
  sharedMemory: Record<string, any>
}

export interface ConsensusVote {
  voter: string
  decision: "accept" | "reject" | "abstain"
  confidence: number
  rationale?: string
}

// ============================================================================
// SWARM COORDINATION HOOK
// ============================================================================

export function useSwarmCoordination() {
  const [agents, setAgents] = useState<Map<string, Agent>>(new Map())
  const [tasks, setTasks] = useState<Map<string, SwarmTask>>(new Map())
  const [messages, setMessages] = useState<Message[]>([])
  const [coordinator, setCoordinator] = useState<string | undefined>()
  const [sharedMemory, setSharedMemory] = useState<Record<string, any>>({})
  const [consensusState, setConsensusState] = useState<Map<string, ConsensusVote[]>>(new Map())
  const [agentHealth, setAgentHealth] = useState<Map<string, number>>(new Map()) // 0-100

  const messageQueueRef = useRef<Message[]>([])
  const taskQueueRef = useRef<SwarmTask[]>([])

  // ========================================================================
  // AGENT MANAGEMENT
  // ========================================================================

  const registerAgent = useCallback(
    (id: string, name: string, role: Agent["role"], capabilities: string[]) => {
      const agent: Agent = {
        id,
        name,
        role,
        capability: capabilities,
        status: "idle",
        lastHeartbeat: Date.now(),
        reputation: 75,
        workload: 0,
      }

      setAgents((prev) => new Map(prev).set(id, agent))
      setAgentHealth((prev) => new Map(prev).set(id, 100))

      // Broadcast agent registration
      broadcastMessage({
        type: "heartbeat",
        payload: { agentId: id, status: "online" },
      })

      return agent
    },
    []
  )

  const deregisterAgent = useCallback((agentId: string) => {
    setAgents((prev) => {
      const updated = new Map(prev)
      updated.delete(agentId)
      return updated
    })

    setAgentHealth((prev) => {
      const updated = new Map(prev)
      updated.delete(agentId)
      return updated
    })
  }, [])

  const updateAgentStatus = useCallback((agentId: string, status: Agent["status"]) => {
    setAgents((prev) => {
      const agent = prev.get(agentId)
      if (!agent) return prev

      return new Map(prev).set(agentId, {
        ...agent,
        status,
        lastHeartbeat: Date.now(),
      })
    })
  }, [])

  const updateAgentWorkload = useCallback((agentId: string, workload: number) => {
    setAgents((prev) => {
      const agent = prev.get(agentId)
      if (!agent) return prev

      return new Map(prev).set(agentId, { ...agent, workload: Math.min(100, workload) })
    })
  }, [])

  // ========================================================================
  // MESSAGING SYSTEM
  // ========================================================================

  const broadcastMessage = useCallback(
    (
      message: Omit<Message, "id" | "from" | "timestamp" | "to" | "priority" | "ttl"> & {
        from?: string
        priority?: number
        ttl?: number
      },
      from = "system"
    ) => {
      const msg: Message = {
        id: `msg-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
        from: message.from || from,
        type: message.type,
        payload: message.payload,
        timestamp: Date.now(),
        priority: message.priority || 50,
        ttl: message.ttl || 30000,
      }

      messageQueueRef.current.push(msg)
      setMessages((prev) => [...prev, msg].slice(-1000)) // Keep last 1000 messages
    },
    []
  )

  const sendMessage = useCallback(
    (
      to: string,
      message: Omit<Message, "id" | "from" | "timestamp" | "to" | "priority" | "ttl"> & {
        from?: string
        priority?: number
        ttl?: number
      },
      from = "system"
    ) => {
      const msg: Message = {
        id: `msg-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
        from: message.from || from,
        to,
        type: message.type,
        payload: message.payload,
        timestamp: Date.now(),
        priority: message.priority || 50,
        ttl: message.ttl || 30000,
      }

      messageQueueRef.current.push(msg)
      setMessages((prev) => [...prev, msg].slice(-1000))
    },
    []
  )

  const getMessageQueue = useCallback(() => {
    return messageQueueRef.current.filter((m) => m.ttl > Date.now() - m.timestamp)
  }, [])

  // ========================================================================
  // TASK DISTRIBUTION & EXECUTION
  // ========================================================================

  const submitTask = useCallback((task: Omit<SwarmTask, "id" | "status">) => {
    const swarmTask: SwarmTask = {
      ...task,
      id: `task-${Date.now()}`,
      status: "pending",
      subtasks: task.subtasks || [],
    }

    taskQueueRef.current.push(swarmTask)
    setTasks((prev) => new Map(prev).set(swarmTask.id, swarmTask))

    // Broadcast task to swarm
    broadcastMessage({
      type: "task",
      payload: { task: swarmTask },
      priority: task.priority === "critical" ? 100 : 50,
    })

    return swarmTask
  }, [broadcastMessage])

  const assignTask = useCallback((taskId: string, agentId: string) => {
    setTasks((prev) => {
      const task = prev.get(taskId)
      if (!task) return prev

      return new Map(prev).set(taskId, {
        ...task,
        assignedAgent: agentId,
        status: "executing",
        startTime: Date.now(),
      })
    })

    updateAgentStatus(agentId, "busy")

    // Notify agent of task assignment
    sendMessage(agentId, {
      type: "task",
      payload: { taskId, action: "execute" },
    })
  }, [sendMessage, updateAgentStatus])

  const completeTask = useCallback((taskId: string, result: any) => {
    setTasks((prev) => {
      const task = prev.get(taskId)
      if (!task) return prev

      return new Map(prev).set(taskId, {
        ...task,
        status: "completed",
        result,
        endTime: Date.now(),
      })
    })

    // Update agent status
    const task = tasks.get(taskId)
    if (task?.assignedAgent) {
      updateAgentStatus(task.assignedAgent, "idle")
      updateAgentWorkload(task.assignedAgent, 0)
    }

    // Store result in shared memory
    setSharedMemory((prev) => ({
      ...prev,
      [`result_${taskId}`]: result,
    }))
  }, [tasks, updateAgentStatus, updateAgentWorkload])

  const getOptimalAgent = useCallback(
    (requiredCapabilities: string[]): string | undefined => {
      let bestAgent: Agent | undefined
      let bestScore = -1

      agents.forEach((agent) => {
        // Check if agent has required capabilities
        const hasCapabilities = requiredCapabilities.every((cap) =>
          agent.capability.includes(cap)
        )

        if (!hasCapabilities) return

        // Score based on: workload (lower is better), reputation (higher is better), status
        const workloadScore = 100 - agent.workload
        const reputationScore = agent.reputation
        const statusScore = agent.status === "idle" ? 100 : agent.status === "busy" ? 0 : 50

        const totalScore = workloadScore * 0.5 + reputationScore * 0.3 + statusScore * 0.2

        if (totalScore > bestScore) {
          bestScore = totalScore
          bestAgent = agent
        }
      })

      return bestAgent?.id
    },
    [agents]
  )

  // ========================================================================
  // COLLECTIVE DECISION MAKING (CONSENSUS)
  // ========================================================================

  const initiateConsensus = useCallback(
    (
      proposal: string,
      voters: string[],
      timeout = 5000
    ): Promise<{ decision: "accept" | "reject" | "timeout"; votes: ConsensusVote[] }> => {
      return new Promise((resolve) => {
        const proposalId = `proposal-${Date.now()}`
        const votes: ConsensusVote[] = []

        // Request votes from all agents
        broadcastMessage({
          type: "query",
          payload: { proposalId, proposal },
          priority: 80,
        })

        // Simulate vote collection
        const simulateVotes = () => {
          agents.forEach((agent) => {
            if (voters.includes(agent.id)) {
              votes.push({
                voter: agent.id,
                decision: Math.random() > 0.3 ? "accept" : "reject",
                confidence: 50 + Math.random() * 50,
              })
            }
          })

          const acceptCount = votes.filter((v) => v.decision === "accept").length
          const decision = acceptCount > votes.length / 2 ? "accept" : "reject"

          setConsensusState((prev) => new Map(prev).set(proposalId, votes))
          resolve({ decision, votes })
        }

        // Simulate async vote collection
        setTimeout(simulateVotes, Math.min(timeout / 2, 1000))
      })
    },
    [agents, broadcastMessage]
  )

  const getConsensusStats = useCallback(() => {
    const stats = {
      totalProposals: consensusState.size,
      acceptedProposals: 0,
      rejectedProposals: 0,
      averageConfidence: 0,
    }

    let totalConfidence = 0
    let totalVotes = 0

    consensusState.forEach((votes) => {
      const acceptCount = votes.filter((v) => v.decision === "accept").length
      if (acceptCount > votes.length / 2) {
        stats.acceptedProposals++
      } else {
        stats.rejectedProposals++
      }

      votes.forEach((vote) => {
        totalConfidence += vote.confidence
        totalVotes++
      })
    })

    stats.averageConfidence = totalVotes > 0 ? totalConfidence / totalVotes : 0

    return stats
  }, [consensusState])

  // ========================================================================
  // LEADER ELECTION & COORDINATION
  // ========================================================================

  const electCoordinator = useCallback(() => {
    if (agents.size === 0) return

    // Select agent with highest reputation and best status
    let selectedAgent: Agent | undefined
    let bestScore = -1

    agents.forEach((agent) => {
      const score = agent.reputation * 0.6 + (100 - agent.workload) * 0.4
      if (score > bestScore) {
        bestScore = score
        selectedAgent = agent
      }
    })

    if (selectedAgent) {
      setCoordinator(selectedAgent.id)

      broadcastMessage({
        type: "election",
        payload: { coordinatorId: selectedAgent.id, timestamp: Date.now() },
        priority: 90,
      })

      return selectedAgent.id
    }

    return undefined
  }, [agents, broadcastMessage])

  // ========================================================================
  // SWARM MEMORY & LEARNING
  // ========================================================================

  const storeMemory = useCallback((key: string, value: any) => {
    setSharedMemory((prev) => ({
      ...prev,
      [key]: {
        value,
        timestamp: Date.now(),
        accessCount: (prev[key]?.accessCount || 0) + 1,
      },
    }))
  }, [])

  const retrieveMemory = useCallback((key: string) => {
    return sharedMemory[key]?.value
  }, [sharedMemory])

  const getMemoryStats = useCallback(() => {
    const stats = {
      totalEntries: Object.keys(sharedMemory).length,
      mostAccessed: "",
      memorySize: JSON.stringify(sharedMemory).length,
    }

    let maxAccess = 0
    Object.entries(sharedMemory).forEach(([key, data]) => {
      if (data.accessCount > maxAccess) {
        maxAccess = data.accessCount
        stats.mostAccessed = key
      }
    })

    return stats
  }, [sharedMemory])

  // ========================================================================
  // HEALTH MONITORING & FAULT DETECTION
  // ========================================================================

  const updateAgentHealth = useCallback((agentId: string, health: number) => {
    setAgentHealth((prev) => new Map(prev).set(agentId, Math.max(0, Math.min(100, health))))
  }, [])

  const detectFaults = useCallback(() => {
    const faults: string[] = []

    agents.forEach((agent) => {
      const health = agentHealth.get(agent.id) || 100
      const timeoutMs = 10000

      // Check heartbeat timeout
      if (Date.now() - agent.lastHeartbeat > timeoutMs) {
        faults.push(`Agent ${agent.id} heartbeat timeout`)
      }

      // Check health threshold
      if (health < 30) {
        faults.push(`Agent ${agent.id} health critical: ${health}%`)
      }
    })

    return faults
  }, [agents, agentHealth])

  const getSwarmHealth = useCallback(() => {
    if (agents.size === 0) return 100

    let totalHealth = 0
    agents.forEach((agent) => {
      totalHealth += agentHealth.get(agent.id) || 100
    })

    return Math.round(totalHealth / agents.size)
  }, [agents, agentHealth])

  // ========================================================================
  // PERFORMANCE METRICS
  // ========================================================================

  const getPerformanceMetrics = useCallback(() => {
    const completedTasks: SwarmTask[] = []
    const failedTasks: SwarmTask[] = []

    tasks.forEach((task) => {
      if (task.status === "completed") {
        completedTasks.push(task)
      } else if (task.status === "failed") {
        failedTasks.push(task)
      }
    })

    const avgTaskDuration =
      completedTasks.length > 0
        ? completedTasks.reduce((sum, t) => sum + ((t.endTime || 0) - (t.startTime || 0)), 0) /
          completedTasks.length
        : 0

    return {
      totalAgents: agents.size,
      activeAgents: Array.from(agents.values()).filter((a) => a.status !== "offline").length,
      pendingTasks: Array.from(tasks.values()).filter((t) => t.status === "pending").length,
      completedTasks: completedTasks.length,
      failedTasks: failedTasks.length,
      avgTaskDuration,
      successRate: completedTasks.length + failedTasks.length > 0
        ? completedTasks.length / (completedTasks.length + failedTasks.length)
        : 0,
      swarmHealth: getSwarmHealth(),
      messageQueue: getMessageQueue().length,
      taskQueue: taskQueueRef.current.length,
    }
  }, [agents, tasks, getSwarmHealth, getMessageQueue])

  // ========================================================================
  // RETURN PUBLIC API
  // ========================================================================

  return {
    // Agent Management
    registerAgent,
    deregisterAgent,
    updateAgentStatus,
    updateAgentWorkload,
    getAgents: () => Object.fromEntries(agents),

    // Messaging
    broadcastMessage,
    sendMessage,
    getMessageQueue,
    getMessages: () => messages,

    // Task Distribution
    submitTask,
    assignTask,
    completeTask,
    getOptimalAgent,
    getTasks: () => Object.fromEntries(tasks),

    // Consensus & Coordination
    initiateConsensus,
    getConsensusStats,
    electCoordinator,
    getCoordinator: () => coordinator,

    // Memory & Learning
    storeMemory,
    retrieveMemory,
    getMemoryStats,
    getSharedMemory: () => sharedMemory,

    // Health & Monitoring
    updateAgentHealth,
    detectFaults,
    getSwarmHealth,
    getAgentHealth: () => Object.fromEntries(agentHealth),

    // Performance
    getPerformanceMetrics,
  }
}
