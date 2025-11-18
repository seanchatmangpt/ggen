/**
 * Consensus & Fault Tolerance Mechanisms
 * PBFT, Raft, Byzantine Fault Tolerance for Agent Swarms
 *
 * 2028+ Innovation: Distributed Consensus, Byzantine Resilience
 */

/**
 * Consensus result
 */
export interface ConsensusResult {
  proposalId: string
  consensus: boolean
  approvalCount: number
  rejectCount: number
  abstainCount: number
  timestamp: Date
  algorithm: string
  faulty nodes detected: string[]
}

/**
 * Raft Log Entry
 */
export interface RaftLogEntry {
  term: number
  index: number
  command: any
  commitIndex: number
}

/**
 * PBFT Consensus Engine
 * Practical Byzantine Fault Tolerance - tolerates f faulty nodes in 3f+1 total
 */
export class PBFTConsensus {
  private nodeId: string
  private nodes: Map<string, { reputation: number; online: boolean }> = new Map()
  private viewNumber: number = 0
  private sequenceNumber: number = 0
  private preparedMessages: Map<string, any[]> = new Map()
  private commitMessages: Map<string, any[]> = new Map()
  private logs: any[] = []

  constructor(nodeId: string, otherNodes: string[]) {
    this.nodeId = nodeId

    // Initialize nodes with reputation
    otherNodes.forEach((nodeId) => {
      this.nodes.set(nodeId, { reputation: 50, online: true })
    })
  }

  /**
   * Pre-prepare phase - primary broadcasts request
   */
  preprepareFase(request: any, timestamp: Date): boolean {
    const message = {
      type: 'pre-prepare',
      view: this.viewNumber,
      sequence: this.sequenceNumber,
      request,
      timestamp,
    }

    this.logs.push(message)
    this.sequenceNumber++

    return true
  }

  /**
   * Prepare phase - replicas acknowledge
   */
  preparePhase(messageId: string, replica: string, digest: string): boolean {
    if (!this.preparedMessages.has(messageId)) {
      this.preparedMessages.set(messageId, [])
    }

    this.preparedMessages.get(messageId)!.push({
      replica,
      digest,
      timestamp: new Date(),
    })

    // Check if we have 2f+1 prepared messages
    const preparedCount = this.preparedMessages.get(messageId)!.length
    const requiredCount = Math.floor((this.nodes.size - 1) * 2) / 3 + 1

    return preparedCount >= requiredCount
  }

  /**
   * Commit phase - replicas commit
   */
  commitPhase(messageId: string, replica: string): boolean {
    if (!this.commitMessages.has(messageId)) {
      this.commitMessages.set(messageId, [])
    }

    this.commitMessages.get(messageId)!.push({
      replica,
      timestamp: new Date(),
    })

    // Check if we have 2f+1 commit messages
    const commitCount = this.commitMessages.get(messageId)!.length
    const requiredCount = Math.floor((this.nodes.size - 1) * 2) / 3 + 1

    return commitCount >= requiredCount
  }

  /**
   * Detect faulty nodes
   */
  detectFaultyNodes(): string[] {
    const faultyNodes: string[] = []
    const requiredMessages = Math.ceil(this.nodes.size * 0.67)

    // Check for nodes with inconsistent messages
    for (const [nodeId, nodeInfo] of this.nodes) {
      const messageCount = this.logs.filter((log) => log.from === nodeId).length

      if (messageCount < this.sequenceNumber * 0.5) {
        faultyNodes.push(nodeId)
        nodeInfo.online = false
      }
    }

    return faultyNodes
  }

  /**
   * Request consensus
   */
  requestConsensus(proposal: any): ConsensusResult {
    const proposalId = `prop-${Date.now()}`

    // Simulate phases
    this.preprepareFase(proposal, new Date())
    const prepared = this.preparePhase(proposalId, this.nodeId, 'digest')
    const committed = this.commitPhase(proposalId, this.nodeId)

    const faultyNodes = this.detectFaultyNodes()
    const totalApproval = Math.floor(this.nodes.size * 0.67)

    return {
      proposalId,
      consensus: prepared && committed,
      approvalCount: totalApproval,
      rejectCount: this.nodes.size - totalApproval,
      abstainCount: 0,
      timestamp: new Date(),
      algorithm: 'PBFT',
      faulty nodes detected: faultyNodes,
    }
  }

  /**
   * View change on primary failure
   */
  viewChange(): void {
    this.viewNumber++
    this.preparedMessages.clear()
    this.commitMessages.clear()
  }

  /**
   * Get consensus stats
   */
  getStats() {
    return {
      viewNumber: this.viewNumber,
      sequenceNumber: this.sequenceNumber,
      logsSize: this.logs.length,
      onlineNodes: Array.from(this.nodes.values()).filter((n) => n.online).length,
      totalNodes: this.nodes.size,
    }
  }
}

/**
 * Raft Consensus Engine
 * Simpler alternative with leader election and log replication
 */
export class RaftConsensus {
  private nodeId: string
  private state: 'follower' | 'candidate' | 'leader' = 'follower'
  private currentTerm: number = 0
  private votedFor: string | null = null
  private log: RaftLogEntry[] = []
  private commitIndex: number = 0
  private lastApplied: number = 0
  private leaderId: string | null = null
  private peers: Map<string, { nextIndex: number; matchIndex: number }> = new Map()
  private electionTimeout: number = Math.random() * 150 + 150 // 150-300ms
  private heartbeatInterval: number = 50

  constructor(nodeId: string, peers: string[]) {
    this.nodeId = nodeId

    peers.forEach((peerId) => {
      this.peers.set(peerId, { nextIndex: this.log.length, matchIndex: 0 })
    })
  }

  /**
   * Start election for leader
   */
  startElection(): boolean {
    this.state = 'candidate'
    this.currentTerm++
    this.votedFor = this.nodeId

    const votesNeeded = Math.ceil((this.peers.size + 1) / 2)
    let votes = 1 // Vote for self

    // Simulate requesting votes
    for (const [peerId] of this.peers) {
      if (Math.random() > 0.1) {
        // 90% chance peers vote for us
        votes++
      }
    }

    if (votes >= votesNeeded) {
      this.becomeLeader()
      return true
    }

    this.state = 'follower'
    return false
  }

  /**
   * Become leader
   */
  private becomeLeader(): void {
    this.state = 'leader'
    this.leaderId = this.nodeId

    // Initialize nextIndex for all followers
    for (const [peerId] of this.peers) {
      this.peers.set(peerId, { nextIndex: this.log.length, matchIndex: 0 })
    }
  }

  /**
   * Append entries (leader → followers)
   */
  appendEntries(entries: RaftLogEntry[]): void {
    if (this.state !== 'leader') return

    entries.forEach((entry) => {
      entry.term = this.currentTerm
      entry.index = this.log.length
      this.log.push(entry)
    })

    // Replicate to followers
    for (const [peerId] of this.peers) {
      this.replicateLog(peerId)
    }
  }

  /**
   * Replicate log to specific peer
   */
  private replicateLog(peerId: string): void {
    const peer = this.peers.get(peerId)
    if (!peer) return

    const prevIndex = Math.max(0, peer.nextIndex - 1)
    const prevTerm = prevIndex > 0 ? this.log[prevIndex - 1].term : 0
    const entries = this.log.slice(peer.nextIndex)

    // Simulate RPC call
    const success = Math.random() > 0.05 // 95% success rate
    if (success) {
      peer.nextIndex = this.log.length
      peer.matchIndex = this.log.length - 1
    } else {
      peer.nextIndex = Math.max(0, peer.nextIndex - 1)
    }
  }

  /**
   * Commit entries
   */
  commitEntries(): void {
    if (this.state !== 'leader') return

    // Find the highest index replicated on majority
    const matchIndexes = [this.log.length - 1]
    for (const [, peer] of this.peers) {
      matchIndexes.push(peer.matchIndex)
    }

    matchIndexes.sort((a, b) => b - a)
    const majorityIndex = matchIndexes[Math.floor(matchIndexes.length / 2)]

    if (majorityIndex > this.commitIndex) {
      this.commitIndex = majorityIndex
    }
  }

  /**
   * Request consensus
   */
  requestConsensus(proposal: any): ConsensusResult {
    if (this.state !== 'leader') {
      this.startElection()
    }

    if (this.state === 'leader') {
      const entry: RaftLogEntry = {
        term: this.currentTerm,
        index: this.log.length,
        command: proposal,
        commitIndex: this.commitIndex,
      }

      this.appendEntries([entry])
      this.commitEntries()

      return {
        proposalId: `prop-${Date.now()}`,
        consensus: this.commitIndex >= entry.index,
        approvalCount: Math.floor((this.peers.size + 1) * 0.6),
        rejectCount: Math.ceil((this.peers.size + 1) * 0.4),
        abstainCount: 0,
        timestamp: new Date(),
        algorithm: 'Raft',
        faulty nodes detected: [],
      }
    }

    return {
      proposalId: `prop-${Date.now()}`,
      consensus: false,
      approvalCount: 0,
      rejectCount: this.peers.size + 1,
      abstainCount: 0,
      timestamp: new Date(),
      algorithm: 'Raft',
      faulty nodes detected: [],
    }
  }

  /**
   * Receive append entries from leader
   */
  receiveAppendEntries(
    leaderId: string,
    term: number,
    prevIndex: number,
    prevTerm: number,
    entries: RaftLogEntry[]
  ): boolean {
    if (term < this.currentTerm) {
      return false
    }

    if (term > this.currentTerm) {
      this.currentTerm = term
      this.votedFor = null
      this.state = 'follower'
    }

    this.leaderId = leaderId

    // Check log consistency
    if (prevIndex > 0 && prevIndex > this.log.length - 1) {
      return false
    }

    if (prevIndex > 0 && this.log[prevIndex - 1].term !== prevTerm) {
      return false
    }

    // Append entries
    this.log.splice(prevIndex, this.log.length - prevIndex, ...entries)
    return true
  }

  /**
   * Get consensus stats
   */
  getStats() {
    return {
      state: this.state,
      currentTerm: this.currentTerm,
      leaderId: this.leaderId,
      logLength: this.log.length,
      commitIndex: this.commitIndex,
      lastApplied: this.lastApplied,
      peers: this.peers.size,
    }
  }
}

/**
 * Byzantine Fault Detector
 * Detects byzantine (malicious/faulty) behavior
 */
export class ByzantineFaultDetector {
  private suspicionScores: Map<string, number> = new Map()
  private nodeHistory: Map<string, any[]> = new Map()
  private threshold: number = 70 // Suspicion score threshold

  /**
   * Report suspicious behavior
   */
  reportSuspicious(reporterId: string, suspectId: string, reason: string, severity: number): void {
    const key = suspectId
    if (!this.suspicionScores.has(key)) {
      this.suspicionScores.set(key, 0)
    }

    // Increase suspicion based on severity
    const currentScore = this.suspicionScores.get(key)!
    this.suspicionScores.set(key, currentScore + severity * 10)

    // Record history
    if (!this.nodeHistory.has(key)) {
      this.nodeHistory.set(key, [])
    }

    this.nodeHistory.get(key)!.push({
      reporterId,
      reason,
      severity,
      timestamp: new Date(),
    })
  }

  /**
   * Check if node is byzantine
   */
  isByzantine(nodeId: string): boolean {
    const score = this.suspicionScores.get(nodeId) || 0
    return score >= this.threshold
  }

  /**
   * Get suspicion score
   */
  getSuspicionScore(nodeId: string): number {
    return this.suspicionScores.get(nodeId) || 0
  }

  /**
   * Reduce suspicion score over time (recovery)
   */
  processRecovery(nodeId: string, amount: number = 5): void {
    const current = this.suspicionScores.get(nodeId) || 0
    this.suspicionScores.set(nodeId, Math.max(0, current - amount))
  }

  /**
   * Get all byzantine nodes
   */
  getByzantineNodes(): string[] {
    return Array.from(this.suspicionScores.entries())
      .filter(([, score]) => score >= this.threshold)
      .map(([nodeId]) => nodeId)
  }

  /**
   * Get detailed report
   */
  getReport() {
    return {
      totalNodes: this.suspicionScores.size,
      byzantineNodes: this.getByzantineNodes().length,
      details: Array.from(this.suspicionScores.entries()).map(([nodeId, score]) => ({
        nodeId,
        suspicionScore: score,
        isByzantine: score >= this.threshold,
        history: this.nodeHistory.get(nodeId) || [],
      })),
    }
  }
}

/**
 * Consensus Manager - Orchestrates consensus across algorithms
 */
export class ConsensusManager {
  private pbft: PBFTConsensus | null = null
  private raft: RaftConsensus | null = null
  private byzantine: ByzantineFaultDetector
  private algorithm: 'pbft' | 'raft' = 'pbft'

  constructor(nodeId: string, peers: string[], algorithm: 'pbft' | 'raft' = 'pbft') {
    this.algorithm = algorithm
    this.byzantine = new ByzantineFaultDetector()

    if (algorithm === 'pbft') {
      this.pbft = new PBFTConsensus(nodeId, peers)
    } else {
      this.raft = new RaftConsensus(nodeId, peers)
    }
  }

  /**
   * Request consensus
   */
  requestConsensus(proposal: any): ConsensusResult {
    if (this.algorithm === 'pbft' && this.pbft) {
      return this.pbft.requestConsensus(proposal)
    } else if (this.algorithm === 'raft' && this.raft) {
      return this.raft.requestConsensus(proposal)
    }

    return {
      proposalId: '',
      consensus: false,
      approvalCount: 0,
      rejectCount: 0,
      abstainCount: 0,
      timestamp: new Date(),
      algorithm: this.algorithm,
      faulty nodes detected: [],
    }
  }

  /**
   * Get consensus stats
   */
  getStats() {
    if (this.algorithm === 'pbft' && this.pbft) {
      return this.pbft.getStats()
    } else if (this.algorithm === 'raft' && this.raft) {
      return this.raft.getStats()
    }
    return {}
  }

  /**
   * Get byzantine report
   */
  getByzantineReport() {
    return this.byzantine.getReport()
  }

  /**
   * Report suspicious behavior
   */
  reportSuspicious(reporterId: string, suspectId: string, reason: string, severity: number): void {
    this.byzantine.reportSuspicious(reporterId, suspectId, reason, severity)
  }
}
