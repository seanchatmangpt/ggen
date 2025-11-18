import { create } from 'zustand'
import {
  DIDDocument,
  VerifiableCredential,
  AutonomousAgent,
  ZeroKnowledgeProof,
  QuantumSafeKeyPair,
  AgentSwarm,
  ConsensusProposal,
  ReputationScore,
  BiometricEnrollment,
} from '@/types'

interface Features2028State {
  // DIDs
  dids: DIDDocument[]
  selectedDid: DIDDocument | null
  setDids: (dids: DIDDocument[]) => void
  setSelectedDid: (did: DIDDocument | null) => void
  addDid: (did: DIDDocument) => void
  removeDid: (id: string) => void

  // Zero-Knowledge Proofs
  zkProofs: ZeroKnowledgeProof[]
  selectedProof: ZeroKnowledgeProof | null
  setZkProofs: (proofs: ZeroKnowledgeProof[]) => void
  setSelectedProof: (proof: ZeroKnowledgeProof | null) => void
  addZkProof: (proof: ZeroKnowledgeProof) => void

  // Autonomous Agents
  agents: AutonomousAgent[]
  selectedAgent: AutonomousAgent | null
  setAgents: (agents: AutonomousAgent[]) => void
  setSelectedAgent: (agent: AutonomousAgent | null) => void
  addAgent: (agent: AutonomousAgent) => void
  updateAgent: (id: string, agent: Partial<AutonomousAgent>) => void

  // Quantum-Safe Cryptography
  quantumKeyPairs: QuantumSafeKeyPair[]
  setQuantumKeyPairs: (keys: QuantumSafeKeyPair[]) => void
  addQuantumKeyPair: (key: QuantumSafeKeyPair) => void

  // Swarms
  swarms: AgentSwarm[]
  selectedSwarm: AgentSwarm | null
  setSwarms: (swarms: AgentSwarm[]) => void
  setSelectedSwarm: (swarm: AgentSwarm | null) => void
  addSwarm: (swarm: AgentSwarm) => void

  // Consensus Proposals
  proposals: ConsensusProposal[]
  setProposals: (proposals: ConsensusProposal[]) => void
  addProposal: (proposal: ConsensusProposal) => void
  voteOnProposal: (proposalId: string, agentId: string, vote: 'Approve' | 'Reject' | 'Abstain') => void

  // Reputation Scores
  reputationScores: ReputationScore[]
  setReputationScores: (scores: ReputationScore[]) => void

  // Biometric Enrollments
  biometricEnrollments: BiometricEnrollment[]
  selectedBiometric: BiometricEnrollment | null
  setBiometricEnrollments: (enrollments: BiometricEnrollment[]) => void
  setSelectedBiometric: (enrollment: BiometricEnrollment | null) => void
  addBiometricEnrollment: (enrollment: BiometricEnrollment) => void

  // Loading & Error states
  isLoading: boolean
  error: string | null
  setIsLoading: (loading: boolean) => void
  setError: (error: string | null) => void
}

export const useFeatures2028Store = create<Features2028State>((set) => ({
  // DIDs
  dids: [],
  selectedDid: null,
  setDids: (dids) => set({ dids }),
  setSelectedDid: (did) => set({ selectedDid: did }),
  addDid: (did) => set((state) => ({ dids: [...state.dids, did] })),
  removeDid: (id) =>
    set((state) => ({
      dids: state.dids.filter((d) => d.id !== id),
      selectedDid: state.selectedDid?.id === id ? null : state.selectedDid,
    })),

  // Zero-Knowledge Proofs
  zkProofs: [],
  selectedProof: null,
  setZkProofs: (proofs) => set({ zkProofs: proofs }),
  setSelectedProof: (proof) => set({ selectedProof: proof }),
  addZkProof: (proof) => set((state) => ({ zkProofs: [...state.zkProofs, proof] })),

  // Autonomous Agents
  agents: [],
  selectedAgent: null,
  setAgents: (agents) => set({ agents }),
  setSelectedAgent: (agent) => set({ selectedAgent: agent }),
  addAgent: (agent) => set((state) => ({ agents: [...state.agents, agent] })),
  updateAgent: (id, updates) =>
    set((state) => ({
      agents: state.agents.map((a) => (a.id === id ? { ...a, ...updates } : a)),
      selectedAgent: state.selectedAgent?.id === id ? { ...state.selectedAgent, ...updates } : state.selectedAgent,
    })),

  // Quantum-Safe Cryptography
  quantumKeyPairs: [],
  setQuantumKeyPairs: (keys) => set({ quantumKeyPairs: keys }),
  addQuantumKeyPair: (key) => set((state) => ({ quantumKeyPairs: [...state.quantumKeyPairs, key] })),

  // Swarms
  swarms: [],
  selectedSwarm: null,
  setSwarms: (swarms) => set({ swarms }),
  setSelectedSwarm: (swarm) => set({ selectedSwarm: swarm }),
  addSwarm: (swarm) => set((state) => ({ swarms: [...state.swarms, swarm] })),

  // Consensus Proposals
  proposals: [],
  setProposals: (proposals) => set({ proposals }),
  addProposal: (proposal) => set((state) => ({ proposals: [...state.proposals, proposal] })),
  voteOnProposal: (proposalId, agentId, vote) =>
    set((state) => ({
      proposals: state.proposals.map((p) =>
        p.id === proposalId
          ? {
              ...p,
              votes: [...p.votes.filter((v) => v.agentId !== agentId), { agentId, vote }],
            }
          : p
      ),
    })),

  // Reputation Scores
  reputationScores: [],
  setReputationScores: (scores) => set({ reputationScores: scores }),

  // Biometric Enrollments
  biometricEnrollments: [],
  selectedBiometric: null,
  setBiometricEnrollments: (enrollments) => set({ biometricEnrollments: enrollments }),
  setSelectedBiometric: (enrollment) => set({ selectedBiometric: enrollment }),
  addBiometricEnrollment: (enrollment) =>
    set((state) => ({ biometricEnrollments: [...state.biometricEnrollments, enrollment] })),

  // Loading & Error states
  isLoading: false,
  error: null,
  setIsLoading: (loading) => set({ isLoading: loading }),
  setError: (error) => set({ error }),
}))
