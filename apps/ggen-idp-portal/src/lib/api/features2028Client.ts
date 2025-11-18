import {
  DIDDocument,
  ZeroKnowledgeProof,
  AutonomousAgent,
  QuantumSafeKeyPair,
  AgentSwarm,
  ConsensusProposal,
  ReputationScore,
  BiometricEnrollment,
} from '@/types'

const API_BASE = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000'

class Features2028Client {
  private baseUrl: string

  constructor(baseUrl: string = API_BASE) {
    this.baseUrl = baseUrl
  }

  // ==================== DIDs ====================
  async getDIDs(): Promise<DIDDocument[]> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/dids`)
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to fetch DIDs:', error)
      return []
    }
  }

  async createDID(method: 'web' | 'key' | 'ethereum'): Promise<DIDDocument | null> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/dids`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ method }),
      })
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to create DID:', error)
      return null
    }
  }

  // ==================== Zero-Knowledge Proofs ====================
  async getZKProofs(): Promise<ZeroKnowledgeProof[]> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/zk-proofs`)
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to fetch ZK proofs:', error)
      return []
    }
  }

  async generateZKProof(type: string, data: Record<string, any>): Promise<ZeroKnowledgeProof | null> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/zk-proofs`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ type, data }),
      })
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to generate ZK proof:', error)
      return null
    }
  }

  // ==================== Autonomous Agents ====================
  async getAgents(): Promise<AutonomousAgent[]> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/agents`)
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to fetch agents:', error)
      return []
    }
  }

  async createAgent(type: string, name: string): Promise<AutonomousAgent | null> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/agents`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ type, name }),
      })
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to create agent:', error)
      return null
    }
  }

  // ==================== Quantum-Safe Cryptography ====================
  async getQuantumKeyPairs(): Promise<QuantumSafeKeyPair[]> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/quantum-keys`)
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to fetch quantum keys:', error)
      return []
    }
  }

  async generateQuantumKeyPair(algorithm: string): Promise<QuantumSafeKeyPair | null> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/quantum-keys`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ algorithm }),
      })
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to generate quantum key pair:', error)
      return null
    }
  }

  // ==================== Swarm Coordination ====================
  async getSwarms(): Promise<AgentSwarm[]> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/swarms`)
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to fetch swarms:', error)
      return []
    }
  }

  // ==================== Consensus Proposals ====================
  async getProposals(): Promise<ConsensusProposal[]> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/proposals`)
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to fetch proposals:', error)
      return []
    }
  }

  async voteOnProposal(proposalId: string, agentId: string, vote: string): Promise<boolean> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/proposals/${proposalId}/vote`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ agentId, vote }),
      })
      return response.ok
    } catch (error) {
      console.error('Failed to vote on proposal:', error)
      return false
    }
  }

  // ==================== Reputation ====================
  async getReputationScores(): Promise<ReputationScore[]> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/reputation`)
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to fetch reputation scores:', error)
      return []
    }
  }

  // ==================== Biometric ====================
  async getBiometricEnrollments(): Promise<BiometricEnrollment[]> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/biometric/enrollments`)
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to fetch biometric enrollments:', error)
      return []
    }
  }

  async enrollBiometric(type: string, userId: string): Promise<BiometricEnrollment | null> {
    try {
      const response = await fetch(`${this.baseUrl}/api/features/biometric/enroll`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ type, userId }),
      })
      if (!response.ok) throw new Error(`HTTP ${response.status}`)
      return response.json()
    } catch (error) {
      console.error('Failed to enroll biometric:', error)
      return null
    }
  }
}

export const features2028Client = new Features2028Client()
