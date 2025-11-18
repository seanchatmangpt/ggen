'use client'

import { useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { SwarmMonitor, CompactSwarmMonitor } from '@/components/agents/swarm-monitor'
import {
  Network,
  Plus,
  Play,
  Pause,
  Trash2,
  Settings,
  Book,
  Zap,
  BarChart3,
} from 'lucide-react'

/**
 * Agent Swarm Dashboard
 * 2028+ Innovation: AI Agent Swarm Management Interface
 */
export default function AgentSwarmsDashboard() {
  const [swarms, setSwarms] = useState<string[]>(['demo-swarm-1'])
  const [activeSwarm, setActiveSwarm] = useState('demo-swarm-1')
  const [newSwarmId, setNewSwarmId] = useState('')
  const [isCreating, setIsCreating] = useState(false)

  const handleCreateSwarm = async () => {
    if (!newSwarmId.trim()) return

    try {
      setIsCreating(true)
      const response = await fetch('/api/agents/swarm?action=create', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          swarmId: newSwarmId,
          agents: [
            { type: 'coordinator', name: 'Coordinator' },
            { type: 'worker', name: 'Worker 1', options: { specialization: 'processing' } },
            { type: 'worker', name: 'Worker 2', options: { specialization: 'analysis' } },
            { type: 'scout', name: 'Scout' },
            { type: 'learner', name: 'Learner' },
          ],
          config: {
            consensusAlgorithm: 'pbft',
            selfHealingEnabled: true,
            learningEnabled: true,
            energyManagement: true,
          },
        }),
      })

      const data = await response.json()
      if (data.success) {
        setSwarms([...swarms, newSwarmId])
        setActiveSwarm(newSwarmId)
        setNewSwarmId('')
      }
    } catch (error) {
      console.error('Failed to create swarm:', error)
    } finally {
      setIsCreating(false)
    }
  }

  const handleDeleteSwarm = async (swarmId: string) => {
    try {
      const response = await fetch(`/api/agents/swarm?swarmId=${swarmId}`, {
        method: 'DELETE',
      })

      const data = await response.json()
      if (data.success) {
        setSwarms(swarms.filter((s) => s !== swarmId))
        if (activeSwarm === swarmId) {
          setActiveSwarm(swarms[0] || '')
        }
      }
    } catch (error) {
      console.error('Failed to delete swarm:', error)
    }
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 dark:from-slate-900 dark:to-slate-800 p-4 md:p-8">
      {/* Header */}
      <div className="max-w-7xl mx-auto mb-8">
        <div className="flex items-center gap-4 mb-6">
          <div className="p-3 bg-gradient-to-br from-blue-100 to-purple-100 dark:from-blue-900 dark:to-purple-900 rounded-lg">
            <Network className="h-8 w-8 text-blue-600 dark:text-blue-400" />
          </div>
          <div>
            <h1 className="text-4xl font-bold">Agent Swarm Dashboard</h1>
            <p className="text-slate-600 dark:text-slate-400 text-lg">
              Manage and monitor distributed intelligent agent swarms
            </p>
          </div>
        </div>

        {/* Info Cards */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-8">
          <Card className="border-blue-200 dark:border-blue-900">
            <CardHeader className="pb-3">
              <CardTitle className="text-base flex items-center gap-2">
                <Zap className="h-5 w-5 text-blue-600" />
                Swarms Active
              </CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-3xl font-bold">{swarms.length}</p>
              <p className="text-sm text-slate-600 dark:text-slate-400 mt-1">
                Distributed intelligence networks
              </p>
            </CardContent>
          </Card>

          <Card className="border-purple-200 dark:border-purple-900">
            <CardHeader className="pb-3">
              <CardTitle className="text-base flex items-center gap-2">
                <BarChart3 className="h-5 w-5 text-purple-600" />
                Total Agents
              </CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-3xl font-bold">{swarms.length * 5}</p>
              <p className="text-sm text-slate-600 dark:text-slate-400 mt-1">
                Cooperative autonomous systems
              </p>
            </CardContent>
          </Card>

          <Card className="border-green-200 dark:border-green-900">
            <CardHeader className="pb-3">
              <CardTitle className="text-base flex items-center gap-2">
                <Settings className="h-5 w-5 text-green-600" />
                Status
              </CardTitle>
            </CardHeader>
            <CardContent>
              <Badge className="bg-green-100 text-green-800">Operational</Badge>
              <p className="text-sm text-slate-600 dark:text-slate-400 mt-3">
                All swarms healthy and running
              </p>
            </CardContent>
          </Card>
        </div>

        {/* Quick Actions */}
        <Card className="mb-8">
          <CardHeader>
            <CardTitle className="text-base">Create New Swarm</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="flex gap-2">
              <Input
                placeholder="Enter swarm ID (e.g., production-swarm-1)"
                value={newSwarmId}
                onChange={(e) => setNewSwarmId(e.target.value)}
                onKeyPress={(e) => e.key === 'Enter' && handleCreateSwarm()}
              />
              <Button
                onClick={handleCreateSwarm}
                disabled={isCreating || !newSwarmId.trim()}
                className="gap-2"
              >
                <Plus className="h-4 w-4" />
                Create
              </Button>
            </div>
            <p className="text-xs text-slate-600 dark:text-slate-400 mt-3">
              Creates swarm with default agents: 1 Coordinator, 2 Workers, 1 Scout, 1 Learner
            </p>
          </CardContent>
        </Card>
      </div>

      {/* Swarm List & Monitor */}
      <div className="max-w-7xl mx-auto">
        <div className="grid grid-cols-1 lg:grid-cols-4 gap-8">
          {/* Swarm List */}
          <div className="lg:col-span-1">
            <Card>
              <CardHeader>
                <CardTitle className="text-base">Swarms</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="space-y-2">
                  {swarms.map((swarm) => (
                    <div
                      key={swarm}
                      className={`p-3 rounded-lg cursor-pointer border-2 transition-all ${
                        activeSwarm === swarm
                          ? 'border-blue-500 bg-blue-50 dark:bg-blue-950'
                          : 'border-slate-200 dark:border-slate-700 hover:border-blue-300'
                      }`}
                      onClick={() => setActiveSwarm(swarm)}
                    >
                      <div className="flex items-center justify-between">
                        <div className="flex-1 min-w-0">
                          <p className="font-medium text-sm truncate">{swarm}</p>
                          <p className="text-xs text-slate-500">5 agents</p>
                        </div>
                        <button
                          onClick={(e) => {
                            e.stopPropagation()
                            handleDeleteSwarm(swarm)
                          }}
                          className="ml-2 text-slate-400 hover:text-red-600"
                        >
                          <Trash2 className="h-4 w-4" />
                        </button>
                      </div>
                    </div>
                  ))}
                </div>
              </CardContent>
            </Card>

            {/* Quick Stats */}
            {activeSwarm && (
              <div className="mt-4">
                <Card>
                  <CardHeader>
                    <CardTitle className="text-base">Quick Stats</CardTitle>
                  </CardHeader>
                  <CardContent>
                    <CompactSwarmMonitor swarmId={activeSwarm} />
                  </CardContent>
                </Card>
              </div>
            )}
          </div>

          {/* Main Monitor */}
          <div className="lg:col-span-3">
            {activeSwarm ? (
              <SwarmMonitor swarmId={activeSwarm} />
            ) : (
              <Card>
                <CardContent className="pt-6">
                  <div className="text-center text-slate-500">
                    Create or select a swarm to monitor
                  </div>
                </CardContent>
              </Card>
            )}
          </div>
        </div>
      </div>

      {/* Documentation Section */}
      <div className="max-w-7xl mx-auto mt-12 pt-12 border-t border-slate-200 dark:border-slate-800">
        <div className="flex items-center gap-3 mb-6">
          <Book className="h-6 w-6 text-slate-600" />
          <h2 className="text-2xl font-bold">Documentation</h2>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Agent Types</CardTitle>
            </CardHeader>
            <CardContent className="space-y-3 text-sm">
              <div>
                <p className="font-semibold mb-1">Coordinator Agent</p>
                <p className="text-slate-600">Orchestrates tasks, resolves conflicts, manages resources</p>
              </div>
              <div>
                <p className="font-semibold mb-1">Worker Agents</p>
                <p className="text-slate-600">Execute specialized tasks, report results, consume energy</p>
              </div>
              <div>
                <p className="font-semibold mb-1">Scout Agent</p>
                <p className="text-slate-600">Explores, gathers information, performs reconnaissance</p>
              </div>
              <div>
                <p className="font-semibold mb-1">Learning Agent</p>
                <p className="text-slate-600">Learns from experience, shares knowledge, adapts behavior</p>
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle className="text-base">Key Features</CardTitle>
            </CardHeader>
            <CardContent className="space-y-2 text-sm">
              <div className="flex items-start gap-2">
                <span className="text-green-600 font-bold mt-1">✓</span>
                <span>Byzantine Fault Tolerance (PBFT/Raft consensus)</span>
              </div>
              <div className="flex items-start gap-2">
                <span className="text-green-600 font-bold mt-1">✓</span>
                <span>Self-healing with automatic recovery</span>
              </div>
              <div className="flex items-start gap-2">
                <span className="text-green-600 font-bold mt-1">✓</span>
                <span>Emergent behavior & collective intelligence</span>
              </div>
              <div className="flex items-start gap-2">
                <span className="text-green-600 font-bold mt-1">✓</span>
                <span>Energy management and reputation tracking</span>
              </div>
              <div className="flex items-start gap-2">
                <span className="text-green-600 font-bold mt-1">✓</span>
                <span>PSO, ACO, Genetic Algorithm optimization</span>
              </div>
              <div className="flex items-start gap-2">
                <span className="text-green-600 font-bold mt-1">✓</span>
                <span>Real-time monitoring and analytics</span>
              </div>
            </CardContent>
          </Card>

          <Card className="md:col-span-2">
            <CardHeader>
              <CardTitle className="text-base">Getting Started</CardTitle>
            </CardHeader>
            <CardContent className="space-y-3 text-sm">
              <div>
                <p className="font-semibold mb-1 flex items-center gap-2">
                  <span>1</span> Create a Swarm
                </p>
                <p className="text-slate-600 ml-7">
                  Use the form above to create a new swarm with a unique ID. Default configuration includes 5 agents with different specializations.
                </p>
              </div>
              <div>
                <p className="font-semibold mb-1 flex items-center gap-2">
                  <span>2</span> Monitor Performance
                </p>
                <p className="text-slate-600 ml-7">
                  Select a swarm to view real-time metrics, agent status, energy levels, and intelligence scores. Dashboard updates every 2 seconds.
                </p>
              </div>
              <div>
                <p className="font-semibold mb-1 flex items-center gap-2">
                  <span>3</span> Trigger Operations
                </p>
                <p className="text-slate-600 ml-7">
                  Use the Heal button to trigger health monitoring/recovery, or Learn button to trigger swarm-wide learning and knowledge sharing.
                </p>
              </div>
            </CardContent>
          </Card>
        </div>
      </div>
    </div>
  )
}
