'use client'

import React, { useState } from 'react'
import { TrendingUp, Award, AlertCircle } from 'lucide-react'
import { ReputationScore, ReputationEvent } from '@/types'

export const ReputationManager: React.FC = () => {
  const [reputationScores, setReputationScores] = useState<ReputationScore[]>([
    {
      agentId: 'agent-001',
      overallScore: 92,
      trustScore: 95,
      capabilityScore: 88,
      integrityScore: 90,
      collaborationScore: 87,
      historyDays: 180,
      lastUpdated: new Date().toISOString(),
    },
    {
      agentId: 'agent-002',
      overallScore: 78,
      trustScore: 80,
      capabilityScore: 75,
      integrityScore: 78,
      collaborationScore: 77,
      historyDays: 90,
      lastUpdated: new Date().toISOString(),
    },
  ])

  const [reputationEvents, setReputationEvents] = useState<ReputationEvent[]>([
    {
      id: 'event-001',
      agentId: 'agent-001',
      type: 'SuccessfulTransaction',
      impact: 5,
      reason: 'Successfully issued 100 credentials',
      timestamp: new Date(Date.now() - 2 * 60 * 60 * 1000).toISOString(),
    },
    {
      id: 'event-002',
      agentId: 'agent-002',
      type: 'TimeoutViolation',
      impact: -15,
      reason: 'Missed SLA deadline',
      timestamp: new Date(Date.now() - 5 * 60 * 60 * 1000).toISOString(),
    },
    {
      id: 'event-003',
      agentId: 'agent-001',
      type: 'Collaboration',
      impact: 10,
      reason: 'Excellent collaboration with other agents',
      timestamp: new Date(Date.now() - 24 * 60 * 60 * 1000).toISOString(),
    },
  ])

  const [selectedAgent, setSelectedAgent] = useState<string>('agent-001')
  const agentReputation = reputationScores.find((r) => r.agentId === selectedAgent)
  const agentEvents = reputationEvents.filter((e) => e.agentId === selectedAgent)

  const getScoreColor = (score: number) => {
    if (score >= 85) return 'text-green-600'
    if (score >= 70) return 'text-yellow-600'
    return 'text-red-600'
  }

  const getScoreBgColor = (score: number) => {
    if (score >= 85) return 'bg-green-50'
    if (score >= 70) return 'bg-yellow-50'
    return 'bg-red-50'
  }

  const getEventIcon = (type: string) => {
    switch (type) {
      case 'SuccessfulTransaction':
        return '✓'
      case 'FailedTransaction':
        return '✗'
      case 'Excellence':
        return '⭐'
      case 'Collaboration':
        return '🤝'
      case 'SecurityBreach':
        return '⚠️'
      default:
        return '•'
    }
  }

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold text-slate-900">Reputation System</h2>
        <p className="text-slate-600 mt-1">
          Decentralized reputation tracking for autonomous agents
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-4 gap-6">
        {/* Agent Selection */}
        <div className="lg:col-span-1">
          <div className="card space-y-2">
            <h3 className="font-bold text-slate-900 mb-3">Agents</h3>
            {reputationScores.map((score) => (
              <button
                key={score.agentId}
                onClick={() => setSelectedAgent(score.agentId)}
                className={`w-full text-left p-3 rounded-lg transition-all ${
                  selectedAgent === score.agentId
                    ? 'bg-blue-50 border border-blue-300'
                    : 'bg-slate-50 hover:bg-slate-100'
                }`}
              >
                <div className="text-sm font-medium text-slate-900">{score.agentId}</div>
                <div className={`text-lg font-bold mt-1 ${getScoreColor(score.overallScore)}`}>
                  {score.overallScore}
                </div>
              </button>
            ))}
          </div>
        </div>

        {/* Main Reputation Display */}
        <div className="lg:col-span-3 space-y-4">
          {agentReputation ? (
            <>
              {/* Overall Score */}
              <div className={`card ${getScoreBgColor(agentReputation.overallScore)}`}>
                <div className="flex items-center justify-between mb-4">
                  <h3 className="text-xl font-bold text-slate-900">Overall Reputation</h3>
                  <TrendingUp size={24} className={getScoreColor(agentReputation.overallScore)} />
                </div>

                <div className="text-5xl font-bold text-center my-4">
                  <span className={getScoreColor(agentReputation.overallScore)}>
                    {agentReputation.overallScore}
                  </span>
                  <span className="text-xl text-slate-500">/100</span>
                </div>

                <div className="flex gap-2">
                  {agentReputation.overallScore >= 85 && (
                    <div className="flex-1 text-center py-2 bg-white rounded">
                      <Award size={16} className="mx-auto text-gold" />
                      <div className="text-xs text-slate-600 mt-1">Trusted</div>
                    </div>
                  )}
                  <div className="flex-1 text-center py-2 bg-white rounded">
                    <span className="text-sm font-semibold text-slate-900">
                      {agentReputation.historyDays} days history
                    </span>
                  </div>
                </div>
              </div>

              {/* Dimension Scores */}
              <div className="card">
                <h3 className="font-bold text-slate-900 mb-4">Reputation Dimensions</h3>
                <div className="space-y-3">
                  {[
                    { label: 'Trust Score', value: agentReputation.trustScore },
                    { label: 'Capability', value: agentReputation.capabilityScore },
                    { label: 'Integrity', value: agentReputation.integrityScore },
                    { label: 'Collaboration', value: agentReputation.collaborationScore },
                  ].map((dimension) => (
                    <div key={dimension.label}>
                      <div className="flex items-center justify-between mb-1">
                        <span className="text-sm font-medium text-slate-700">{dimension.label}</span>
                        <span className={`text-sm font-bold ${getScoreColor(dimension.value)}`}>
                          {dimension.value}
                        </span>
                      </div>
                      <div className="w-full bg-slate-200 rounded-full h-2">
                        <div
                          className={`h-2 rounded-full transition-all ${
                            dimension.value >= 85
                              ? 'bg-green-600'
                              : dimension.value >= 70
                              ? 'bg-yellow-600'
                              : 'bg-red-600'
                          }`}
                          style={{ width: `${dimension.value}%` }}
                        />
                      </div>
                    </div>
                  ))}
                </div>
              </div>
            </>
          ) : (
            <div className="card h-[300px] flex items-center justify-center">
              <p className="text-slate-500">Select an agent to view reputation</p>
            </div>
          )}
        </div>
      </div>

      {/* Recent Events */}
      <div className="card">
        <h3 className="text-xl font-bold text-slate-900 mb-4">Reputation Events</h3>
        <div className="space-y-2 max-h-[400px] overflow-y-auto">
          {agentEvents.length === 0 ? (
            <p className="text-slate-500 text-sm">No events for this agent</p>
          ) : (
            agentEvents.map((event) => (
              <div
                key={event.id}
                className={`p-4 rounded-lg border-l-4 ${
                  event.impact > 0
                    ? 'border-l-green-500 bg-green-50'
                    : 'border-l-red-500 bg-red-50'
                }`}
              >
                <div className="flex items-start justify-between">
                  <div className="flex-1">
                    <div className="flex items-center gap-2 mb-1">
                      <span className="text-xl">{getEventIcon(event.type)}</span>
                      <span className="font-semibold text-slate-900">{event.type}</span>
                      <span
                        className={`text-sm font-bold ${
                          event.impact > 0 ? 'text-green-600' : 'text-red-600'
                        }`}
                      >
                        {event.impact > 0 ? '+' : ''}{event.impact}
                      </span>
                    </div>
                    <p className="text-sm text-slate-600">{event.reason}</p>
                    <p className="text-xs text-slate-500 mt-1">
                      {new Date(event.timestamp).toLocaleString()}
                    </p>
                  </div>
                </div>
              </div>
            ))
          )}
        </div>
      </div>

      {/* Global Leaderboard */}
      <div className="card">
        <h3 className="text-xl font-bold text-slate-900 mb-4 flex items-center gap-2">
          <Award size={20} />
          Agent Leaderboard
        </h3>
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              <tr className="border-b border-slate-200">
                <th className="text-left px-4 py-2 text-sm font-semibold text-slate-700">Rank</th>
                <th className="text-left px-4 py-2 text-sm font-semibold text-slate-700">Agent</th>
                <th className="text-center px-4 py-2 text-sm font-semibold text-slate-700">Score</th>
                <th className="text-center px-4 py-2 text-sm font-semibold text-slate-700">Status</th>
              </tr>
            </thead>
            <tbody>
              {reputationScores
                .sort((a, b) => b.overallScore - a.overallScore)
                .map((score, idx) => (
                  <tr key={score.agentId} className="border-b border-slate-200 hover:bg-slate-50">
                    <td className="px-4 py-2 text-sm font-bold text-slate-900">{idx + 1}</td>
                    <td className="px-4 py-2 text-sm text-slate-900">{score.agentId}</td>
                    <td className="px-4 py-2 text-center">
                      <span className={`font-bold ${getScoreColor(score.overallScore)}`}>
                        {score.overallScore}
                      </span>
                    </td>
                    <td className="px-4 py-2 text-center">
                      <span
                        className={`text-xs px-2 py-1 rounded-full font-medium ${
                          score.overallScore >= 85
                            ? 'bg-green-100 text-green-800'
                            : score.overallScore >= 70
                            ? 'bg-yellow-100 text-yellow-800'
                            : 'bg-red-100 text-red-800'
                        }`}
                      >
                        {score.overallScore >= 85 ? 'Excellent' : score.overallScore >= 70 ? 'Good' : 'Fair'}
                      </span>
                    </td>
                  </tr>
                ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}
