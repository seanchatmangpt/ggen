'use client'

import React, { useState } from 'react'
import { Plus, Users, Vote, Network } from 'lucide-react'
import { AgentSwarm, ConsensusProposal } from '@/types'
import toast from 'react-hot-toast'

export const SwarmCoordinationManager: React.FC = () => {
  const [swarms, setSwarms] = useState<AgentSwarm[]>([
    {
      id: 'swarm-001',
      name: 'Credential Issuance Swarm',
      agents: [],
      governanceModel: 'Consensus',
      consensusAlgorithm: 'PBFT',
      totalMembers: 7,
      activeMembers: 6,
      reputationThreshold: 70,
      createdAt: new Date().toISOString(),
    },
  ])

  const [proposals, setProposals] = useState<ConsensusProposal[]>([
    {
      id: 'prop-001',
      swarmId: 'swarm-001',
      type: 'CredentialIssue',
      title: 'Issue Bachelor Degree Credential',
      description: 'Credential issuance for Alice Smith',
      proposer: 'agent-001',
      votes: [
        { agentId: 'agent-001', vote: 'Approve', reason: 'Valid credentials' },
        { agentId: 'agent-002', vote: 'Approve' },
        { agentId: 'agent-003', vote: 'Approve' },
        { agentId: 'agent-004', vote: 'Abstain' },
      ],
      status: 'Voting',
      deadline: new Date(Date.now() + 24 * 60 * 60 * 1000).toISOString(),
      createdAt: new Date().toISOString(),
    },
  ])

  const [selectedSwarm, setSelectedSwarm] = useState<AgentSwarm | null>(swarms[0])
  const [selectedProposal, setSelectedProposal] = useState<ConsensusProposal | null>(proposals[0])
  const [showCreateProposal, setShowCreateProposal] = useState(false)

  const handleVote = (proposalId: string, vote: 'Approve' | 'Reject' | 'Abstain') => {
    const updated = proposals.map((p) => {
      if (p.id === proposalId) {
        return {
          ...p,
          votes: [
            ...p.votes.filter((v) => v.agentId !== 'agent-current'),
            { agentId: 'agent-current', vote },
          ],
        }
      }
      return p
    })
    setProposals(updated)
    toast.success(`Vote cast: ${vote}`)
  }

  const getProposalStatus = (proposal: ConsensusProposal) => {
    const approvals = proposal.votes.filter((v) => v.vote === 'Approve').length
    const rejections = proposal.votes.filter((v) => v.vote === 'Reject').length
    const threshold = Math.ceil(proposal.votes.length * 0.66)

    if (approvals >= threshold) return 'Approved'
    if (rejections >= threshold) return 'Rejected'
    return 'In Progress'
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h2 className="text-2xl font-bold text-slate-900">Swarm Coordination</h2>
        <button className="btn btn-primary gap-2">
          <Plus size={20} />
          New Swarm
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-4 gap-6">
        {/* Swarm List */}
        <div className="lg:col-span-1">
          <div className="card space-y-2 max-h-[400px] overflow-y-auto">
            <h3 className="font-bold text-slate-900 mb-4 flex items-center gap-2">
              <Network size={16} />
              Swarms ({swarms.length})
            </h3>
            {swarms.map((swarm) => (
              <div
                key={swarm.id}
                onClick={() => setSelectedSwarm(swarm)}
                className={`p-3 rounded-lg cursor-pointer transition-all ${
                  selectedSwarm?.id === swarm.id
                    ? 'bg-blue-50 border border-blue-300'
                    : 'bg-slate-50 hover:bg-slate-100'
                }`}
              >
                <div className="font-medium text-slate-900 text-sm truncate">{swarm.name}</div>
                <div className="text-xs text-slate-600 mt-1">
                  {swarm.activeMembers}/{swarm.totalMembers} Active
                </div>
                <div className="text-xs text-slate-500 mt-1">{swarm.consensusAlgorithm}</div>
              </div>
            ))}
          </div>
        </div>

        {/* Swarm Details */}
        <div className="lg:col-span-3 space-y-4">
          {selectedSwarm ? (
            <>
              <div className="card">
                <h3 className="text-xl font-bold text-slate-900 mb-4">{selectedSwarm.name}</h3>

                <div className="grid grid-cols-2 gap-4 mb-4">
                  <div>
                    <label className="text-sm font-medium text-slate-700">Governance Model</label>
                    <div className="mt-1 px-3 py-2 bg-slate-50 rounded text-sm">
                      {selectedSwarm.governanceModel}
                    </div>
                  </div>
                  <div>
                    <label className="text-sm font-medium text-slate-700">Consensus Algorithm</label>
                    <div className="mt-1 px-3 py-2 bg-slate-50 rounded text-sm">
                      {selectedSwarm.consensusAlgorithm}
                    </div>
                  </div>
                </div>

                <div className="space-y-3">
                  <div>
                    <div className="flex items-center justify-between mb-2">
                      <span className="text-sm font-medium text-slate-700">Member Health</span>
                      <span className="text-sm font-bold text-slate-900">
                        {Math.round((selectedSwarm.activeMembers / selectedSwarm.totalMembers) * 100)}%
                      </span>
                    </div>
                    <div className="w-full bg-slate-200 rounded-full h-2">
                      <div
                        className="h-2 bg-green-600 rounded-full"
                        style={{ width: `${(selectedSwarm.activeMembers / selectedSwarm.totalMembers) * 100}%` }}
                      />
                    </div>
                  </div>
                </div>
              </div>

              <div className="grid grid-cols-3 gap-4">
                <div className="card text-center">
                  <div className="text-3xl font-bold text-blue-600">{selectedSwarm.totalMembers}</div>
                  <div className="text-sm text-slate-600 mt-1">Total Members</div>
                </div>
                <div className="card text-center">
                  <div className="text-3xl font-bold text-green-600">{selectedSwarm.activeMembers}</div>
                  <div className="text-sm text-slate-600 mt-1">Active Now</div>
                </div>
                <div className="card text-center">
                  <div className="text-3xl font-bold text-purple-600">{selectedSwarm.reputationThreshold}</div>
                  <div className="text-sm text-slate-600 mt-1">Min Reputation</div>
                </div>
              </div>
            </>
          ) : (
            <div className="card h-[300px] flex items-center justify-center">
              <p className="text-slate-500">Select a swarm to view details</p>
            </div>
          )}
        </div>
      </div>

      {/* Consensus Proposals */}
      <div className="card">
        <div className="flex items-center justify-between mb-4">
          <h3 className="text-xl font-bold text-slate-900 flex items-center gap-2">
            <Vote size={20} />
            Consensus Proposals
          </h3>
          <button
            onClick={() => setShowCreateProposal(true)}
            className="btn btn-primary gap-1 text-sm"
          >
            <Plus size={16} />
            New Proposal
          </button>
        </div>

        <div className="space-y-3">
          {proposals.map((proposal) => {
            const status = getProposalStatus(proposal)
            const approvals = proposal.votes.filter((v) => v.vote === 'Approve').length

            return (
              <div
                key={proposal.id}
                onClick={() => setSelectedProposal(proposal)}
                className={`p-4 border rounded-lg cursor-pointer transition-all ${
                  selectedProposal?.id === proposal.id
                    ? 'bg-blue-50 border-blue-300'
                    : 'bg-white border-slate-200 hover:border-slate-300'
                }`}
              >
                <div className="flex items-start justify-between mb-2">
                  <h4 className="font-semibold text-slate-900">{proposal.title}</h4>
                  <span
                    className={`px-2 py-1 rounded-full text-xs font-medium ${
                      status === 'Approved'
                        ? 'bg-green-100 text-green-800'
                        : status === 'Rejected'
                        ? 'bg-red-100 text-red-800'
                        : 'bg-yellow-100 text-yellow-800'
                    }`}
                  >
                    {status}
                  </span>
                </div>

                <p className="text-sm text-slate-600 mb-3">{proposal.description}</p>

                <div className="flex items-center justify-between text-sm">
                  <div className="flex items-center gap-4">
                    <span className="text-slate-600">
                      Type: <span className="font-medium text-slate-900">{proposal.type}</span>
                    </span>
                    <span className="text-slate-600">
                      Votes: <span className="font-medium text-slate-900">{approvals}/{proposal.votes.length}</span>
                    </span>
                  </div>
                  <span className="text-slate-500">
                    {new Date(proposal.deadline).toLocaleDateString()}
                  </span>
                </div>

                {selectedProposal?.id === proposal.id && (
                  <div className="mt-3 pt-3 border-t border-slate-200">
                    <div className="flex gap-2">
                      <button
                        onClick={(e) => {
                          e.stopPropagation()
                          handleVote(proposal.id, 'Approve')
                        }}
                        className="btn btn-sm bg-green-600 text-white hover:bg-green-700 flex-1"
                      >
                        Approve
                      </button>
                      <button
                        onClick={(e) => {
                          e.stopPropagation()
                          handleVote(proposal.id, 'Reject')
                        }}
                        className="btn btn-sm bg-red-600 text-white hover:bg-red-700 flex-1"
                      >
                        Reject
                      </button>
                      <button
                        onClick={(e) => {
                          e.stopPropagation()
                          handleVote(proposal.id, 'Abstain')
                        }}
                        className="btn btn-sm bg-slate-600 text-white hover:bg-slate-700 flex-1"
                      >
                        Abstain
                      </button>
                    </div>
                  </div>
                )}
              </div>
            )
          })}
        </div>
      </div>
    </div>
  )
}
