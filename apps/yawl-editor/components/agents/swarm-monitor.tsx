'use client'

import React, { useEffect, useState, useRef } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import {
  LineChart,
  Line,
  BarChart,
  Bar,
  PieChart,
  Pie,
  Cell,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer,
} from 'recharts'
import {
  Activity,
  Zap,
  Heart,
  Users,
  Target,
  TrendingUp,
  AlertCircle,
  CheckCircle,
  Clock,
  Network,
  Brain,
  Shield,
} from 'lucide-react'

interface SwarmMetrics {
  totalAgents: number
  activeAgents: number
  totalTasks: number
  completedTasks: number
  failedTasks: number
  avgEnergyLevel: number
  avgHealthScore: number
  avgReputation: number
  swarmEfficiency: number
  consensusHealth: number
  emergentBehaviorScore: number
}

interface AgentStatus {
  id: string
  name: string
  role: string
  status: string
  energy: number
  health: number
  reputation: number
}

/**
 * Agent Swarm Monitor Component
 * Real-time monitoring and visualization of agent swarms
 */
export function SwarmMonitor({ swarmId }: { swarmId: string }) {
  const [metrics, setMetrics] = useState<SwarmMetrics | null>(null)
  const [agents, setAgents] = useState<AgentStatus[]>([])
  const [isConnected, setIsConnected] = useState(false)
  const [historyData, setHistoryData] = useState<any[]>([])
  const [selectedAgent, setSelectedAgent] = useState<string | null>(null)
  const [autoRefresh, setAutoRefresh] = useState(true)
  const refreshIntervalRef = useRef<NodeJS.Timeout | null>(null)

  // Fetch swarm metrics
  const fetchMetrics = async () => {
    try {
      const response = await fetch(`/api/agents/swarm?swarmId=${swarmId}&action=metrics`)
      const data = await response.json()

      if (data.success && data.metrics) {
        setMetrics(data.metrics)
        setIsConnected(true)

        // Add to history
        setHistoryData((prev) => [
          ...prev,
          {
            timestamp: new Date().toLocaleTimeString(),
            efficiency: data.metrics.swarmEfficiency,
            consensus: data.metrics.consensusHealth,
            behavior: data.metrics.emergentBehaviorScore,
            energy: data.metrics.avgEnergyLevel,
          },
        ])
      }
    } catch (error) {
      setIsConnected(false)
    }
  }

  // Fetch agent list
  const fetchAgents = async () => {
    try {
      const response = await fetch(`/api/agents/swarm?swarmId=${swarmId}&action=agents`)
      const data = await response.json()

      if (data.success && data.agents) {
        const agentStatuses = data.agents.map((a: any) => ({
          id: a.id,
          name: a.name,
          role: a.role,
          status: a.status,
          energy: a.metrics?.energyLevel || 0,
          health: a.metrics?.healthScore || 0,
          reputation: a.metrics?.reputation || 0,
        }))

        setAgents(agentStatuses)
      }
    } catch (error) {
      console.error('Failed to fetch agents:', error)
    }
  }

  // Auto-refresh effect
  useEffect(() => {
    fetchMetrics()
    fetchAgents()

    if (autoRefresh) {
      refreshIntervalRef.current = setInterval(() => {
        fetchMetrics()
        fetchAgents()
      }, 2000)
    }

    return () => {
      if (refreshIntervalRef.current) {
        clearInterval(refreshIntervalRef.current)
      }
    }
  }, [swarmId, autoRefresh])

  // Trigger healing action
  const handleHeal = async () => {
    try {
      const response = await fetch(
        `/api/agents/swarm?swarmId=${swarmId}&action=heal`,
        { method: 'POST' }
      )
      const data = await response.json()

      if (data.success) {
        fetchMetrics()
        fetchAgents()
      }
    } catch (error) {
      console.error('Heal failed:', error)
    }
  }

  // Trigger learning
  const handleLearn = async () => {
    try {
      const response = await fetch(
        `/api/agents/swarm?swarmId=${swarmId}&action=learn`,
        { method: 'POST' }
      )
      const data = await response.json()

      if (data.success) {
        setTimeout(() => {
          fetchMetrics()
          fetchAgents()
        }, 1000)
      }
    } catch (error) {
      console.error('Learning failed:', error)
    }
  }

  if (!metrics) {
    return (
      <Card>
        <CardContent className="pt-6">
          <div className="text-center text-slate-500">Loading swarm data...</div>
        </CardContent>
      </Card>
    )
  }

  const getHealthColor = (value: number) => {
    if (value >= 70) return '#10b981'
    if (value >= 50) return '#f59e0b'
    return '#ef4444'
  }

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'active':
      case 'processing':
        return <Badge className="bg-green-100 text-green-800">Active</Badge>
      case 'idle':
        return <Badge className="bg-blue-100 text-blue-800">Idle</Badge>
      case 'learning':
        return <Badge className="bg-purple-100 text-purple-800">Learning</Badge>
      case 'recovering':
        return <Badge className="bg-yellow-100 text-yellow-800">Recovering</Badge>
      default:
        return <Badge variant="secondary">{status}</Badge>
    }
  }

  return (
    <div className="space-y-6">
      {/* Header with Controls */}
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-3">
          <div className="p-3 bg-gradient-to-br from-blue-100 to-purple-100 dark:from-blue-900 dark:to-purple-900 rounded-lg">
            <Network className="h-6 w-6 text-blue-600 dark:text-blue-400" />
          </div>
          <div>
            <h2 className="text-2xl font-bold">Agent Swarm Monitor</h2>
            <p className="text-sm text-slate-600 dark:text-slate-400">
              Swarm ID: {swarmId}
            </p>
          </div>
        </div>

        <div className="flex gap-2">
          <Button
            variant={autoRefresh ? 'default' : 'outline'}
            onClick={() => setAutoRefresh(!autoRefresh)}
            size="sm"
          >
            <Activity className="h-4 w-4 mr-2" />
            {autoRefresh ? 'Live' : 'Manual'}
          </Button>
          <Button onClick={handleHeal} variant="outline" size="sm" className="gap-2">
            <Heart className="h-4 w-4" />
            Heal
          </Button>
          <Button onClick={handleLearn} variant="outline" size="sm" className="gap-2">
            <Brain className="h-4 w-4" />
            Learn
          </Button>
        </div>
      </div>

      {/* Status Indicator */}
      <Card className={isConnected ? 'border-green-200' : 'border-red-200'}>
        <CardContent className="pt-6">
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-3">
              {isConnected ? (
                <>
                  <CheckCircle className="h-5 w-5 text-green-600" />
                  <span className="font-medium">Connected</span>
                </>
              ) : (
                <>
                  <AlertCircle className="h-5 w-5 text-red-600" />
                  <span className="font-medium">Disconnected</span>
                </>
              )}
            </div>
            <Badge variant={isConnected ? 'default' : 'destructive'}>
              {metrics.totalAgents} Agents
            </Badge>
          </div>
        </CardContent>
      </Card>

      {/* Key Metrics Grid */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        {[
          {
            icon: Users,
            label: 'Active Agents',
            value: metrics.activeAgents,
            total: metrics.totalAgents,
            color: 'text-blue-600',
          },
          {
            icon: Zap,
            label: 'Avg Energy',
            value: metrics.avgEnergyLevel.toFixed(1),
            unit: '%',
            color: 'text-yellow-600',
          },
          {
            icon: Heart,
            label: 'Avg Health',
            value: metrics.avgHealthScore.toFixed(1),
            unit: '%',
            color: 'text-red-600',
          },
          {
            icon: TrendingUp,
            label: 'Efficiency',
            value: metrics.swarmEfficiency.toFixed(1),
            unit: '%',
            color: 'text-green-600',
          },
        ].map((metric, idx) => {
          const Icon = metric.icon
          return (
            <Card key={idx}>
              <CardContent className="pt-6">
                <div className="flex items-center gap-4">
                  <div className={`p-3 bg-slate-100 dark:bg-slate-800 rounded-lg ${metric.color}`}>
                    <Icon className="h-6 w-6" />
                  </div>
                  <div>
                    <p className="text-xs text-slate-600 dark:text-slate-400">{metric.label}</p>
                    <p className="text-2xl font-bold">
                      {metric.value}
                      {metric.unit && <span className="text-sm ml-1">{metric.unit}</span>}
                      {metric.total && <span className="text-sm ml-1">/ {metric.total}</span>}
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          )
        })}
      </div>

      {/* Main Analytics */}
      <Tabs defaultValue="metrics" className="space-y-4">
        <TabsList className="grid w-full grid-cols-3">
          <TabsTrigger value="metrics">Metrics</TabsTrigger>
          <TabsTrigger value="agents">Agents</TabsTrigger>
          <TabsTrigger value="intelligence">Intelligence</TabsTrigger>
        </TabsList>

        {/* Metrics Tab */}
        <TabsContent value="metrics" className="space-y-4">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
            {/* Performance Over Time */}
            <Card>
              <CardHeader>
                <CardTitle className="text-base">Swarm Performance</CardTitle>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <LineChart data={historyData.slice(-20)}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis dataKey="timestamp" tick={{ fontSize: 12 }} />
                    <YAxis />
                    <Tooltip />
                    <Legend />
                    <Line type="monotone" dataKey="efficiency" stroke="#10b981" strokeWidth={2} />
                    <Line type="monotone" dataKey="consensus" stroke="#3b82f6" strokeWidth={2} />
                  </LineChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>

            {/* Health Distribution */}
            <Card>
              <CardHeader>
                <CardTitle className="text-base">Health Distribution</CardTitle>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <PieChart>
                    <Pie
                      data={[
                        { name: 'Healthy', value: metrics.avgHealthScore },
                        { name: 'Degraded', value: 100 - metrics.avgHealthScore },
                      ]}
                      cx="50%"
                      cy="50%"
                      innerRadius={60}
                      outerRadius={90}
                      dataKey="value"
                    >
                      <Cell fill="#10b981" />
                      <Cell fill="#ef4444" />
                    </Pie>
                    <Tooltip />
                  </PieChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>
          </div>

          {/* Intelligence Metrics */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {[
              {
                label: 'Consensus Health',
                value: metrics.consensusHealth,
                icon: Shield,
              },
              {
                label: 'Emergent Behavior',
                value: metrics.emergentBehaviorScore,
                icon: Brain,
              },
              {
                label: 'Task Completion',
                value: metrics.totalTasks > 0 ? (metrics.completedTasks / metrics.totalTasks) * 100 : 0,
                icon: Target,
              },
            ].map((item, idx) => (
              <Card key={idx}>
                <CardContent className="pt-6">
                  <div className="text-center">
                    <div className="flex justify-center mb-3">
                      <item.icon className="h-6 w-6 text-slate-400" />
                    </div>
                    <p className="text-sm text-slate-600 dark:text-slate-400 mb-2">
                      {item.label}
                    </p>
                    <div className="relative w-full bg-slate-200 rounded-full h-2">
                      <div
                        className="bg-gradient-to-r from-blue-500 to-purple-500 h-2 rounded-full"
                        style={{ width: `${Math.min(100, item.value)}%` }}
                      />
                    </div>
                    <p className="text-lg font-bold mt-2">{item.value.toFixed(1)}%</p>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </TabsContent>

        {/* Agents Tab */}
        <TabsContent value="agents">
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Agent Status</CardTitle>
              <CardDescription>{agents.length} agents in swarm</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {agents.map((agent) => (
                  <div
                    key={agent.id}
                    className="flex items-center justify-between p-4 border rounded-lg hover:bg-slate-50 dark:hover:bg-slate-900 cursor-pointer"
                    onClick={() => setSelectedAgent(agent.id)}
                  >
                    <div className="flex-1">
                      <div className="flex items-center gap-3">
                        <div className="w-8 h-8 bg-gradient-to-br from-blue-400 to-purple-400 rounded-full flex items-center justify-center text-white text-sm font-bold">
                          {agent.name[0]}
                        </div>
                        <div>
                          <p className="font-medium">{agent.name}</p>
                          <p className="text-xs text-slate-500">{agent.role}</p>
                        </div>
                      </div>
                    </div>

                    <div className="flex items-center gap-4">
                      <div className="text-right">
                        <p className="text-sm font-medium">{agent.energy.toFixed(0)}%</p>
                        <p className="text-xs text-slate-500">Energy</p>
                      </div>

                      <div className="w-px h-8 bg-slate-200" />

                      <div className="text-right">
                        <p className="text-sm font-medium">{agent.health.toFixed(0)}%</p>
                        <p className="text-xs text-slate-500">Health</p>
                      </div>

                      <div className="w-px h-8 bg-slate-200" />

                      {getStatusBadge(agent.status)}
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Intelligence Tab */}
        <TabsContent value="intelligence" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Swarm Intelligence Metrics</CardTitle>
            </CardHeader>
            <CardContent className="space-y-6">
              <div>
                <h3 className="font-semibold mb-3 flex items-center gap-2">
                  <Brain className="h-4 w-4" />
                  Emergent Behavior
                </h3>
                <p className="text-slate-600 dark:text-slate-400 text-sm mb-3">
                  Score: {metrics.emergentBehaviorScore.toFixed(1)}/100
                </p>
                <div className="bg-slate-100 dark:bg-slate-800 rounded-lg p-4 text-sm space-y-2">
                  <p>• Collective intelligence emerging from agent interactions</p>
                  <p>• Synchronization: Agents working in harmony</p>
                  <p>• Specialization: Agents developing unique roles</p>
                  <p>• Adaptation: Swarm adjusting to changing conditions</p>
                </div>
              </div>

              <div>
                <h3 className="font-semibold mb-3 flex items-center gap-2">
                  <Shield className="h-4 w-4" />
                  Consensus Health
                </h3>
                <p className="text-slate-600 dark:text-slate-400 text-sm mb-3">
                  Score: {metrics.consensusHealth.toFixed(1)}/100
                </p>
                <div className="bg-slate-100 dark:bg-slate-800 rounded-lg p-4 text-sm space-y-2">
                  <p>• Byzantine Fault Tolerance active</p>
                  <p>• Consensus agreement rate: {metrics.consensusHealth.toFixed(1)}%</p>
                  <p>• Faulty agents detected and isolated</p>
                  <p>• Distributed decision-making operational</p>
                </div>
              </div>

              <div>
                <h3 className="font-semibold mb-3 flex items-center gap-2">
                  <TrendingUp className="h-4 w-4" />
                  System Efficiency
                </h3>
                <p className="text-slate-600 dark:text-slate-400 text-sm mb-3">
                  Score: {metrics.swarmEfficiency.toFixed(1)}/100
                </p>
                <div className="bg-slate-100 dark:bg-slate-800 rounded-lg p-4 text-sm space-y-2">
                  <p>• Task completion rate: {metrics.completedTasks}/{metrics.totalTasks}</p>
                  <p>• Failed tasks: {metrics.failedTasks}</p>
                  <p>• Avg reputation: {metrics.avgReputation.toFixed(1)}/100</p>
                  <p>• System optimizing for performance and reliability</p>
                </div>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  )
}

/**
 * Compact swarm monitor widget
 */
export function CompactSwarmMonitor({ swarmId }: { swarmId: string }) {
  const [metrics, setMetrics] = useState<SwarmMetrics | null>(null)

  useEffect(() => {
    const fetchMetrics = async () => {
      try {
        const response = await fetch(`/api/agents/swarm?swarmId=${swarmId}&action=metrics`)
        const data = await response.json()
        if (data.success) {
          setMetrics(data.metrics)
        }
      } catch (error) {
        console.error('Failed to fetch metrics:', error)
      }
    }

    fetchMetrics()
    const interval = setInterval(fetchMetrics, 5000)
    return () => clearInterval(interval)
  }, [swarmId])

  if (!metrics) return null

  return (
    <Card>
      <CardContent className="pt-6">
        <div className="space-y-3 text-sm">
          <div className="flex justify-between">
            <span className="text-slate-600">Agents:</span>
            <span className="font-medium">{metrics.activeAgents}/{metrics.totalAgents}</span>
          </div>
          <div className="flex justify-between">
            <span className="text-slate-600">Efficiency:</span>
            <span className="font-medium">{metrics.swarmEfficiency.toFixed(0)}%</span>
          </div>
          <div className="flex justify-between">
            <span className="text-slate-600">Health:</span>
            <span className="font-medium">{metrics.avgHealthScore.toFixed(0)}%</span>
          </div>
          <div className="flex justify-between">
            <span className="text-slate-600">Behavior:</span>
            <span className="font-medium">{metrics.emergentBehaviorScore.toFixed(0)}%</span>
          </div>
        </div>
      </CardContent>
    </Card>
  )
}
