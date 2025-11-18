'use client'

import React, { useState } from 'react'
import { Plus, Brain, Activity, TrendingUp } from 'lucide-react'
import { AutonomousAgent, AgentGoal, AgentReasoning } from '@/types'
import toast from 'react-hot-toast'

export const AutonomousAgentManager: React.FC = () => {
  const [agents, setAgents] = useState<AutonomousAgent[]>([
    {
      id: 'agent-001',
      name: 'Credential Issuer Alpha',
      type: 'CredentialIssuer',
      did: 'did:web:ggen.dev/agents/issuer-001',
      publicKey: 'MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAE...',
      capabilities: [
        { name: 'issueCredential', description: 'Issue verifiable credentials', parameters: {}, rateLimit: 1000 },
        { name: 'revokeCredential', description: 'Revoke issued credentials', parameters: {}, rateLimit: 500 },
      ],
      llmModel: 'Claude3',
      isActive: true,
      reputation: {
        overallScore: 92,
        trustScore: 95,
        capabilityScore: 88,
        integrityScore: 90,
        collaborationScore: 87,
        eventCount: 156,
        lastUpdated: new Date().toISOString(),
      },
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    },
  ])

  const [selectedAgent, setSelectedAgent] = useState<AutonomousAgent | null>(agents[0])
  const [showCreate, setShowCreate] = useState(false)
  const [newAgentType, setNewAgentType] = useState<'CredentialIssuer' | 'Verifier' | 'RiskAssessment'>('CredentialIssuer')

  const handleCreateAgent = () => {
    const agentNames: Record<string, string> = {
      CredentialIssuer: 'Issuer',
      Verifier: 'Verifier',
      RiskAssessment: 'Risk Assessor',
      DelegatedSigner: 'Signer',
    }

    const newAgent: AutonomousAgent = {
      id: `agent-${Date.now()}`,
      name: `${agentNames[newAgentType]} ${Math.random().toString(36).substr(2, 5).toUpperCase()}`,
      type: newAgentType,
      did: `did:web:ggen.dev/agents/${newAgentType.toLowerCase()}-${Date.now()}`,
      publicKey: 'Generated public key...',
      capabilities: [],
      llmModel: 'Claude3',
      isActive: true,
      reputation: {
        overallScore: 75,
        trustScore: 75,
        capabilityScore: 75,
        integrityScore: 75,
        collaborationScore: 75,
        eventCount: 0,
        lastUpdated: new Date().toISOString(),
      },
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    }

    setAgents([...agents, newAgent])
    setSelectedAgent(newAgent)
    setShowCreate(false)
    toast.success('Autonomous agent created successfully')
  }

  const getReputationColor = (score: number) => {
    if (score >= 85) return 'text-green-600'
    if (score >= 70) return 'text-yellow-600'
    return 'text-red-600'
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <h2 className="text-2xl font-bold text-slate-900">Autonomous Agents</h2>
        <button
          onClick={() => setShowCreate(true)}
          className="btn btn-primary gap-2"
        >
          <Plus size={20} />
          Create Agent
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-4 gap-6">
        {/* Agent List */}
        <div className="lg:col-span-1">
          <div className="card space-y-2 max-h-[600px] overflow-y-auto">
            <h3 className="font-bold text-slate-900 mb-4">Agents ({agents.length})</h3>
            {agents.map((agent) => (
              <div
                key={agent.id}
                onClick={() => setSelectedAgent(agent)}
                className={`p-3 rounded-lg cursor-pointer transition-all ${
                  selectedAgent?.id === agent.id
                    ? 'bg-blue-50 border border-blue-300'
                    : 'bg-slate-50 hover:bg-slate-100'
                }`}
              >
                <div className="flex items-center gap-2">
                  <div className={`w-2 h-2 rounded-full ${agent.isActive ? 'bg-green-600' : 'bg-red-600'}`} />
                  <span className="font-medium text-slate-900 text-sm truncate">{agent.name}</span>
                </div>
                <div className="text-xs text-slate-500 truncate mt-1">{agent.type}</div>
                <div className={`text-xs font-semibold mt-1 ${getReputationColor(agent.reputation.overallScore)}`}>
                  Reputation: {agent.reputation.overallScore}
                </div>
              </div>
            ))}
          </div>
        </div>

        {/* Agent Details */}
        <div className="lg:col-span-3 space-y-4">
          {selectedAgent ? (
            <>
              <div className="card">
                <div className="flex items-center justify-between mb-4">
                  <h3 className="text-xl font-bold text-slate-900">{selectedAgent.name}</h3>
                  <div className="flex items-center gap-2">
                    <span className={`px-3 py-1 rounded-full text-xs font-medium ${
                      selectedAgent.isActive
                        ? 'bg-green-100 text-green-800'
                        : 'bg-red-100 text-red-800'
                    }`}>
                      {selectedAgent.isActive ? '● Active' : '● Inactive'}
                    </span>
                  </div>
                </div>

                <div className="grid grid-cols-2 gap-4 mb-4">
                  <div>
                    <label className="text-sm font-medium text-slate-700">Agent Type</label>
                    <div className="mt-1 px-3 py-2 bg-slate-50 rounded text-sm">
                      {selectedAgent.type}
                    </div>
                  </div>
                  <div>
                    <label className="text-sm font-medium text-slate-700">LLM Model</label>
                    <div className="mt-1 px-3 py-2 bg-slate-50 rounded text-sm">
                      {selectedAgent.llmModel}
                    </div>
                  </div>
                </div>

                <div>
                  <label className="text-sm font-medium text-slate-700">DID</label>
                  <code className="block mt-1 bg-slate-50 p-2 rounded text-xs font-mono break-all">
                    {selectedAgent.did}
                  </code>
                </div>
              </div>

              {/* Reputation Scores */}
              <div className="card">
                <h3 className="font-bold text-slate-900 mb-4 flex items-center gap-2">
                  <TrendingUp size={18} />
                  Reputation Metrics
                </h3>

                <div className="space-y-3">
                  <div>
                    <div className="flex items-center justify-between mb-1">
                      <span className="text-sm font-medium text-slate-700">Overall Score</span>
                      <span className={`text-lg font-bold ${getReputationColor(selectedAgent.reputation.overallScore)}`}>
                        {selectedAgent.reputation.overallScore}/100
                      </span>
                    </div>
                    <div className="w-full bg-slate-200 rounded-full h-2">
                      <div
                        className={`h-2 rounded-full transition-all ${
                          selectedAgent.reputation.overallScore >= 85
                            ? 'bg-green-600'
                            : selectedAgent.reputation.overallScore >= 70
                            ? 'bg-yellow-600'
                            : 'bg-red-600'
                        }`}
                        style={{ width: `${selectedAgent.reputation.overallScore}%` }}
                      />
                    </div>
                  </div>

                  <div className="grid grid-cols-2 gap-4 pt-2">
                    {[
                      { label: 'Trust', value: selectedAgent.reputation.trustScore },
                      { label: 'Capability', value: selectedAgent.reputation.capabilityScore },
                      { label: 'Integrity', value: selectedAgent.reputation.integrityScore },
                      { label: 'Collaboration', value: selectedAgent.reputation.collaborationScore },
                    ].map((metric) => (
                      <div key={metric.label}>
                        <div className="text-xs text-slate-600 mb-1">{metric.label}</div>
                        <div className="text-sm font-bold text-slate-900">{metric.value}</div>
                      </div>
                    ))}
                  </div>
                </div>
              </div>

              {/* Capabilities */}
              <div className="card">
                <h3 className="font-bold text-slate-900 mb-3 flex items-center gap-2">
                  <Brain size={18} />
                  Capabilities
                </h3>
                {selectedAgent.capabilities.length === 0 ? (
                  <p className="text-slate-500 text-sm">No capabilities assigned</p>
                ) : (
                  <div className="space-y-2">
                    {selectedAgent.capabilities.map((cap) => (
                      <div key={cap.name} className="p-3 bg-slate-50 rounded-lg">
                        <div className="font-medium text-slate-900">{cap.name}</div>
                        <div className="text-xs text-slate-600 mt-1">{cap.description}</div>
                        <div className="text-xs text-slate-500 mt-2">Rate Limit: {cap.rateLimit}/hour</div>
                      </div>
                    ))}
                  </div>
                )}
              </div>

              {/* Activity */}
              <div className="card">
                <h3 className="font-bold text-slate-900 mb-3 flex items-center gap-2">
                  <Activity size={18} />
                  Activity
                </h3>
                <div className="grid grid-cols-3 gap-4">
                  <div className="text-center p-3 bg-slate-50 rounded-lg">
                    <div className="text-2xl font-bold text-blue-600">
                      {selectedAgent.reputation.eventCount}
                    </div>
                    <div className="text-xs text-slate-600 mt-1">Total Events</div>
                  </div>
                  <div className="text-center p-3 bg-slate-50 rounded-lg">
                    <div className="text-2xl font-bold text-green-600">
                      {Math.round((selectedAgent.reputation.eventCount * selectedAgent.reputation.overallScore) / 100)}
                    </div>
                    <div className="text-xs text-slate-600 mt-1">Successful</div>
                  </div>
                  <div className="text-center p-3 bg-slate-50 rounded-lg">
                    <div className="text-2xl font-bold text-purple-600">
                      {selectedAgent.capabilities.length}
                    </div>
                    <div className="text-xs text-slate-600 mt-1">Capabilities</div>
                  </div>
                </div>
              </div>
            </>
          ) : (
            <div className="card h-[400px] flex items-center justify-center">
              <p className="text-slate-500">Select an agent to view details</p>
            </div>
          )}
        </div>
      </div>

      {/* Create Agent Modal */}
      {showCreate && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
          <div className="bg-white rounded-lg shadow-lg max-w-md w-full p-6 space-y-4">
            <h3 className="text-xl font-bold text-slate-900">Create Autonomous Agent</h3>

            <div>
              <label className="text-sm font-medium text-slate-700">Agent Type</label>
              <select
                value={newAgentType}
                onChange={(e) => setNewAgentType(e.target.value as any)}
                className="input mt-1"
              >
                <option value="CredentialIssuer">Credential Issuer</option>
                <option value="Verifier">Verifier</option>
                <option value="RiskAssessment">Risk Assessment</option>
              </select>
            </div>

            <div>
              <label className="text-sm font-medium text-slate-700">LLM Model</label>
              <select className="input mt-1">
                <option>Claude 3 Opus</option>
                <option>GPT-4 Turbo</option>
                <option>Mistral Large</option>
              </select>
            </div>

            <div className="flex gap-2 pt-4">
              <button
                onClick={handleCreateAgent}
                className="btn btn-primary flex-1"
              >
                Create
              </button>
              <button
                onClick={() => setShowCreate(false)}
                className="btn btn-secondary flex-1"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  )
}
