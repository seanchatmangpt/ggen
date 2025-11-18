'use client'

import { useState, useCallback } from 'react'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import {
  Zap,
  BarChart3,
  Users,
  Settings,
  Workflow,
  TrendingUp,
  AlertCircle,
  CheckCircle2
} from 'lucide-react'

/**
 * Innovation Hub - Unified dashboard integrating all platform capabilities
 */
export default function InnovationHubPage() {
  const [selectedTab, setSelectedTab] = useState('overview')

  // Mock data for demonstration
  const stats = {
    totalProcesses: 24,
    activeProcesses: 18,
    totalCases: 156,
    activeCases: 42,
    totalWorkitems: 203,
    completedWorkitems: 178,
    teamMembers: 12,
    onlineNow: 7,
  }

  const keyMetrics = [
    {
      label: 'Process Efficiency',
      value: '87%',
      trend: 'up',
      icon: TrendingUp,
    },
    {
      label: 'Average Case Time',
      value: '2.4 days',
      trend: 'down',
      icon: TrendingUp,
    },
    {
      label: 'Team Utilization',
      value: '94%',
      trend: 'up',
      icon: Users,
    },
    {
      label: 'System Uptime',
      value: '99.9%',
      trend: 'up',
      icon: CheckCircle2,
    },
  ]

  const recentInsights = [
    {
      type: 'success',
      title: 'Process Optimization Complete',
      description: 'Invoice Processing workflow now 23% faster',
      timestamp: '2 hours ago',
    },
    {
      type: 'warning',
      title: 'High Workitem Volume',
      description: 'Queue building up - consider load balancing',
      timestamp: '1 hour ago',
    },
    {
      type: 'info',
      title: 'New Team Member',
      description: 'Alex Chen joined the team and is now online',
      timestamp: '30 minutes ago',
    },
    {
      type: 'success',
      title: 'Case Resolution',
      description: 'Customer complaint case CSE-2024-5123 resolved',
      timestamp: '15 minutes ago',
    },
  ]

  const features = [
    {
      id: 'designer',
      icon: Workflow,
      title: 'Process Designer',
      description: 'Design and optimize BPMN workflows',
      href: '/designer',
      status: 'active',
    },
    {
      id: 'cases',
      icon: Zap,
      title: 'Case Management',
      description: 'Manage and track process cases',
      href: '/cases',
      status: 'active',
    },
    {
      id: 'collaboration',
      icon: Users,
      title: 'Team Collaboration',
      description: 'Real-time workspace collaboration',
      href: '/collaboration',
      status: 'active',
    },
    {
      id: 'analytics',
      icon: BarChart3,
      title: 'Advanced Analytics',
      description: 'Insights and performance metrics',
      href: '/analytics',
      status: 'active',
    },
  ]

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 p-4 md:p-8">
      {/* Header */}
      <div className="mb-8">
        <div className="flex items-center justify-between mb-2">
          <h1 className="text-4xl font-bold bg-gradient-to-r from-blue-600 to-purple-600 bg-clip-text text-transparent">
            Innovation Hub
          </h1>
          <Badge variant="outline" className="text-lg py-1 px-3">
            <span className="inline-block h-2 w-2 rounded-full bg-green-500 mr-2"></span>
            System Healthy
          </Badge>
        </div>
        <p className="text-slate-600">
          Unified platform for workflow management, case processing, and team collaboration
        </p>
      </div>

      {/* Quick Stats */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 mb-8">
        {[
          { label: 'Active Processes', value: `${stats.activeProcesses}/${stats.totalProcesses}`, color: 'bg-blue-100 text-blue-800' },
          { label: 'Active Cases', value: `${stats.activeCases}/${stats.totalCases}`, color: 'bg-purple-100 text-purple-800' },
          { label: 'Completed Today', value: stats.completedWorkitems, color: 'bg-green-100 text-green-800' },
          { label: 'Online Team', value: `${stats.onlineNow}/${stats.teamMembers}`, color: 'bg-orange-100 text-orange-800' },
        ].map((stat, idx) => (
          <Card key={idx} className="bg-white">
            <CardContent className="pt-6">
              <p className="text-sm font-medium text-slate-600 mb-2">{stat.label}</p>
              <p className={`text-3xl font-bold ${stat.color} w-fit px-3 py-1 rounded`}>{stat.value}</p>
            </CardContent>
          </Card>
        ))}
      </div>

      {/* Main Content Tabs */}
      <Tabs value={selectedTab} onValueChange={setSelectedTab} className="space-y-4">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="overview">Overview</TabsTrigger>
          <TabsTrigger value="metrics">Metrics</TabsTrigger>
          <TabsTrigger value="features">Features</TabsTrigger>
          <TabsTrigger value="activity">Activity</TabsTrigger>
        </TabsList>

        {/* Overview Tab */}
        <TabsContent value="overview" className="space-y-4">
          <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
            {/* Main Dashboard Card */}
            <Card className="lg:col-span-2">
              <CardHeader>
                <CardTitle>System Overview</CardTitle>
                <CardDescription>
                  Real-time status of all platform components
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-6">
                {/* Status Items */}
                {[
                  { name: 'Process Engine', status: 'operational', uptime: '99.9%' },
                  { name: 'SPARQL Store', status: 'operational', uptime: '99.95%' },
                  { name: 'WebSocket Server', status: 'operational', uptime: '99.8%' },
                  { name: 'AI Analytics', status: 'operational', uptime: '99.7%' },
                ].map((component, idx) => (
                  <div key={idx} className="flex items-center justify-between p-3 rounded-lg bg-slate-50">
                    <span className="font-medium">{component.name}</span>
                    <div className="flex items-center gap-4">
                      <Badge className="bg-green-100 text-green-800">
                        <span className="inline-block h-2 w-2 rounded-full bg-green-600 mr-2"></span>
                        {component.status}
                      </Badge>
                      <span className="text-sm text-slate-600">{component.uptime}</span>
                    </div>
                  </div>
                ))}
              </CardContent>
            </Card>

            {/* Quick Actions Card */}
            <Card>
              <CardHeader>
                <CardTitle className="text-base">Quick Actions</CardTitle>
              </CardHeader>
              <CardContent className="space-y-2">
                <Button className="w-full justify-start" variant="outline" asChild>
                  <a href="/designer">Design Process</a>
                </Button>
                <Button className="w-full justify-start" variant="outline" asChild>
                  <a href="/cases">Create Case</a>
                </Button>
                <Button className="w-full justify-start" variant="outline" asChild>
                  <a href="/collaboration">View Team</a>
                </Button>
                <Button className="w-full justify-start" variant="outline" asChild>
                  <a href="/analytics">View Analytics</a>
                </Button>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        {/* Metrics Tab */}
        <TabsContent value="metrics" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {keyMetrics.map((metric, idx) => {
              const IconComponent = metric.icon
              return (
                <Card key={idx}>
                  <CardContent className="pt-6">
                    <div className="flex items-start justify-between">
                      <div>
                        <p className="text-sm font-medium text-slate-600 mb-1">
                          {metric.label}
                        </p>
                        <p className="text-4xl font-bold">{metric.value}</p>
                      </div>
                      <div className={`p-2 rounded-lg ${metric.trend === 'up' ? 'bg-green-100' : 'bg-red-100'}`}>
                        <IconComponent className={`h-6 w-6 ${metric.trend === 'up' ? 'text-green-600' : 'text-red-600'}`} />
                      </div>
                    </div>
                  </CardContent>
                </Card>
              )
            })}
          </div>

          {/* Detailed Metrics */}
          <Card>
            <CardHeader>
              <CardTitle>Performance Trends</CardTitle>
              <CardDescription>
                Key performance indicators over the last 30 days
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {[
                  { label: 'Case Completion Rate', value: '92%', change: '+3.2%' },
                  { label: 'Average Resolution Time', value: '2.4 days', change: '-0.8 days' },
                  { label: 'Resource Utilization', value: '94%', change: '+2.1%' },
                  { label: 'Error Rate', value: '0.3%', change: '-0.1%' },
                ].map((item, idx) => (
                  <div key={idx} className="flex items-center justify-between p-3 rounded-lg border">
                    <span className="font-medium">{item.label}</span>
                    <div className="text-right">
                      <p className="text-lg font-bold">{item.value}</p>
                      <p className="text-xs text-green-600">{item.change}</p>
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Features Tab */}
        <TabsContent value="features" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {features.map((feature) => {
              const IconComponent = feature.icon
              return (
                <Card key={feature.id} className="flex flex-col">
                  <CardHeader>
                    <div className="flex items-start justify-between">
                      <div className="flex items-center gap-3">
                        <div className="p-2 rounded-lg bg-gradient-to-br from-blue-100 to-purple-100">
                          <IconComponent className="h-6 w-6 text-blue-600" />
                        </div>
                        <div>
                          <CardTitle className="text-base">{feature.title}</CardTitle>
                          <CardDescription className="text-xs">
                            {feature.description}
                          </CardDescription>
                        </div>
                      </div>
                      <Badge className="bg-green-100 text-green-800">
                        {feature.status}
                      </Badge>
                    </div>
                  </CardHeader>
                  <CardContent className="mt-auto">
                    <Button className="w-full" asChild>
                      <a href={feature.href}>Access Feature</a>
                    </Button>
                  </CardContent>
                </Card>
              )
            })}
          </div>
        </TabsContent>

        {/* Activity Tab */}
        <TabsContent value="activity" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Recent Insights & Alerts</CardTitle>
              <CardDescription>
                Important events and recommendations from the system
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {recentInsights.map((insight, idx) => {
                  const iconMap = {
                    success: <CheckCircle2 className="h-5 w-5 text-green-600" />,
                    warning: <AlertCircle className="h-5 w-5 text-orange-600" />,
                    info: <Zap className="h-5 w-5 text-blue-600" />,
                  }

                  const bgMap = {
                    success: 'bg-green-50 border-green-200',
                    warning: 'bg-orange-50 border-orange-200',
                    info: 'bg-blue-50 border-blue-200',
                  }

                  return (
                    <div
                      key={idx}
                      className={`p-4 rounded-lg border ${bgMap[insight.type as keyof typeof bgMap]}`}
                    >
                      <div className="flex gap-3">
                        {iconMap[insight.type as keyof typeof iconMap]}
                        <div className="flex-1">
                          <h4 className="font-semibold">{insight.title}</h4>
                          <p className="text-sm text-slate-600 mt-1">
                            {insight.description}
                          </p>
                          <p className="text-xs text-slate-500 mt-2">
                            {insight.timestamp}
                          </p>
                        </div>
                      </div>
                    </div>
                  )
                })}
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  )
}
