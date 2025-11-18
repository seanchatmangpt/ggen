'use client'

import { useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import {
  BarChart3,
  TrendingUp,
  TrendingDown,
  Users,
  Clock,
  CheckCircle2,
  AlertCircle,
  Download,
} from 'lucide-react'

/**
 * Advanced Analytics Dashboard
 * Comprehensive reporting and insights for workflow management
 */
export default function AnalyticsPage() {
  const [dateRange, setDateRange] = useState('30days')

  // Sample metrics data
  const metrics = {
    caseCompletion: {
      current: '92%',
      previous: '88%',
      trend: 'up',
      change: '+4%',
    },
    avgResolutionTime: {
      current: '2.4 days',
      previous: '3.1 days',
      trend: 'down',
      change: '-0.7 days',
    },
    resourceUtilization: {
      current: '94%',
      previous: '91%',
      trend: 'up',
      change: '+3%',
    },
    errorRate: {
      current: '0.3%',
      previous: '0.8%',
      trend: 'down',
      change: '-0.5%',
    },
  }

  const processMetrics = [
    {
      name: 'Invoice Processing',
      cases: 234,
      completed: 215,
      avgTime: '1.2 days',
      efficiency: 92,
    },
    {
      name: 'Approval Workflow',
      cases: 156,
      completed: 143,
      avgTime: '2.1 days',
      efficiency: 91,
    },
    {
      name: 'Complaint Resolution',
      cases: 89,
      completed: 76,
      avgTime: '3.4 days',
      efficiency: 85,
    },
    {
      name: 'Onboarding Process',
      cases: 45,
      completed: 44,
      avgTime: '5.2 days',
      efficiency: 98,
    },
  ]

  const teamMetrics = [
    {
      name: 'Sarah Chen',
      completed: 47,
      avgTime: '2.1 days',
      quality: 98,
      utilization: 95,
    },
    {
      name: 'Mike Johnson',
      completed: 38,
      avgTime: '2.8 days',
      quality: 96,
      utilization: 88,
    },
    {
      name: 'Alex Rivera',
      completed: 42,
      avgTime: '2.4 days',
      quality: 97,
      utilization: 91,
    },
    {
      name: 'Jordan Smith',
      completed: 35,
      avgTime: '3.1 days',
      quality: 95,
      utilization: 82,
    },
  ]

  const renderTrendIcon = (trend: string) => {
    if (trend === 'up') {
      return <TrendingUp className="h-4 w-4 text-green-600" />
    }
    return <TrendingDown className="h-4 w-4 text-green-600" />
  }

  return (
    <div className="min-h-screen bg-slate-50 p-4 md:p-8">
      {/* Header */}
      <div className="mb-8">
        <h1 className="text-3xl font-bold mb-2">Advanced Analytics</h1>
        <p className="text-slate-600 mb-4">
          Comprehensive reporting and performance insights
        </p>
        <div className="flex gap-2">
          {['7days', '30days', '90days', 'custom'].map((range) => (
            <Button
              key={range}
              variant={dateRange === range ? 'default' : 'outline'}
              size="sm"
              onClick={() => setDateRange(range)}
              className="capitalize"
            >
              {range.replace('days', ' days')}
            </Button>
          ))}
        </div>
      </div>

      {/* Key Metrics */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 mb-8">
        <Card>
          <CardContent className="pt-6">
            <div className="flex items-start justify-between">
              <div>
                <p className="text-sm font-medium text-slate-600 mb-1">
                  Case Completion Rate
                </p>
                <p className="text-3xl font-bold">{metrics.caseCompletion.current}</p>
                <div className="flex items-center gap-1 mt-2 text-sm">
                  {renderTrendIcon(metrics.caseCompletion.trend)}
                  <span className="text-green-600">{metrics.caseCompletion.change}</span>
                </div>
              </div>
              <CheckCircle2 className="h-8 w-8 text-blue-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="flex items-start justify-between">
              <div>
                <p className="text-sm font-medium text-slate-600 mb-1">
                  Avg Resolution Time
                </p>
                <p className="text-3xl font-bold">{metrics.avgResolutionTime.current}</p>
                <div className="flex items-center gap-1 mt-2 text-sm">
                  {renderTrendIcon(metrics.avgResolutionTime.trend)}
                  <span className="text-green-600">{metrics.avgResolutionTime.change}</span>
                </div>
              </div>
              <Clock className="h-8 w-8 text-purple-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="flex items-start justify-between">
              <div>
                <p className="text-sm font-medium text-slate-600 mb-1">
                  Resource Utilization
                </p>
                <p className="text-3xl font-bold">{metrics.resourceUtilization.current}</p>
                <div className="flex items-center gap-1 mt-2 text-sm">
                  {renderTrendIcon(metrics.resourceUtilization.trend)}
                  <span className="text-green-600">{metrics.resourceUtilization.change}</span>
                </div>
              </div>
              <Users className="h-8 w-8 text-orange-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="flex items-start justify-between">
              <div>
                <p className="text-sm font-medium text-slate-600 mb-1">Error Rate</p>
                <p className="text-3xl font-bold">{metrics.errorRate.current}</p>
                <div className="flex items-center gap-1 mt-2 text-sm">
                  {renderTrendIcon(metrics.errorRate.trend)}
                  <span className="text-green-600">{metrics.errorRate.change}</span>
                </div>
              </div>
              <AlertCircle className="h-8 w-8 text-red-500" />
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Detailed Analytics */}
      <Tabs defaultValue="processes" className="space-y-4">
        <TabsList className="grid w-full grid-cols-3">
          <TabsTrigger value="processes">Process Metrics</TabsTrigger>
          <TabsTrigger value="team">Team Performance</TabsTrigger>
          <TabsTrigger value="insights">Insights</TabsTrigger>
        </TabsList>

        {/* Process Metrics Tab */}
        <TabsContent value="processes" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Process Performance</CardTitle>
              <CardDescription>
                Metrics for all active workflows
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {processMetrics.map((process, idx) => (
                  <div key={idx} className="p-4 rounded-lg border">
                    <div className="flex items-start justify-between mb-3">
                      <div>
                        <h4 className="font-semibold">{process.name}</h4>
                        <p className="text-sm text-slate-500">
                          {process.cases} total cases
                        </p>
                      </div>
                      <Badge className={`${
                        process.efficiency >= 90
                          ? 'bg-green-100 text-green-800'
                          : process.efficiency >= 80
                            ? 'bg-yellow-100 text-yellow-800'
                            : 'bg-red-100 text-red-800'
                      }`}>
                        {process.efficiency}% efficient
                      </Badge>
                    </div>

                    <div className="grid grid-cols-3 gap-4">
                      <div>
                        <p className="text-xs text-slate-600 mb-1">Completed</p>
                        <p className="text-lg font-bold">
                          {process.completed}/{process.cases}
                        </p>
                      </div>
                      <div>
                        <p className="text-xs text-slate-600 mb-1">Avg Time</p>
                        <p className="text-lg font-bold">{process.avgTime}</p>
                      </div>
                      <div>
                        <p className="text-xs text-slate-600 mb-1">Completion Rate</p>
                        <p className="text-lg font-bold">
                          {Math.round((process.completed / process.cases) * 100)}%
                        </p>
                      </div>
                    </div>

                    {/* Simple bar */}
                    <div className="mt-3 h-2 bg-slate-200 rounded-full overflow-hidden">
                      <div
                        className="h-full bg-gradient-to-r from-blue-500 to-blue-600"
                        style={{ width: `${(process.completed / process.cases) * 100}%` }}
                      />
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Team Performance Tab */}
        <TabsContent value="team" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Team Performance</CardTitle>
              <CardDescription>Individual team member metrics</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="overflow-x-auto">
                <table className="w-full">
                  <thead>
                    <tr className="border-b">
                      <th className="text-left py-3 px-4 font-semibold text-sm">
                        Member
                      </th>
                      <th className="text-center py-3 px-4 font-semibold text-sm">
                        Completed
                      </th>
                      <th className="text-center py-3 px-4 font-semibold text-sm">
                        Avg Time
                      </th>
                      <th className="text-center py-3 px-4 font-semibold text-sm">
                        Quality
                      </th>
                      <th className="text-center py-3 px-4 font-semibold text-sm">
                        Utilization
                      </th>
                    </tr>
                  </thead>
                  <tbody>
                    {teamMetrics.map((member, idx) => (
                      <tr key={idx} className="border-b hover:bg-slate-50">
                        <td className="py-3 px-4 font-medium">{member.name}</td>
                        <td className="py-3 px-4 text-center">{member.completed}</td>
                        <td className="py-3 px-4 text-center">{member.avgTime}</td>
                        <td className="py-3 px-4 text-center">
                          <Badge
                            variant={
                              member.quality >= 97
                                ? 'default'
                                : member.quality >= 95
                                  ? 'secondary'
                                  : 'outline'
                            }
                          >
                            {member.quality}%
                          </Badge>
                        </td>
                        <td className="py-3 px-4 text-center">
                          <div className="flex items-center justify-center">
                            <div className="w-20 h-2 bg-slate-200 rounded-full overflow-hidden">
                              <div
                                className="h-full bg-gradient-to-r from-blue-500 to-blue-600"
                                style={{ width: `${member.utilization}%` }}
                              />
                            </div>
                            <span className="ml-2 text-sm font-medium">
                              {member.utilization}%
                            </span>
                          </div>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Insights Tab */}
        <TabsContent value="insights" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>AI-Generated Insights</CardTitle>
              <CardDescription>
                Recommendations based on historical data
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              {[
                {
                  type: 'success',
                  icon: '✨',
                  title: 'Invoice Processing Optimized',
                  description:
                    'Process efficiency improved by 23% through gateway consolidation',
                },
                {
                  type: 'warning',
                  icon: '⚠️',
                  title: 'High Approval Queue',
                  description:
                    'Approval workflow has 12 pending items. Consider allocating additional resources.',
                },
                {
                  type: 'info',
                  icon: '💡',
                  title: 'Team Capacity Available',
                  description:
                    'Sarah Chen and Alex Rivera have available capacity for additional workitems.',
                },
                {
                  type: 'success',
                  icon: '📈',
                  title: 'Performance Trending Up',
                  description:
                    'Overall case completion rate improved by 4% month-over-month.',
                },
              ].map((insight, idx) => {
                const bgMap: Record<string, string> = {
                  success: 'bg-green-50 border-green-200',
                  warning: 'bg-orange-50 border-orange-200',
                  info: 'bg-blue-50 border-blue-200',
                }
                return (
                  <div
                    key={idx}
                    className={`p-4 rounded-lg border ${bgMap[insight.type]}`}
                  >
                    <div className="flex gap-3">
                      <span className="text-2xl">{insight.icon}</span>
                      <div>
                        <h4 className="font-semibold text-sm">{insight.title}</h4>
                        <p className="text-sm text-slate-600 mt-1">
                          {insight.description}
                        </p>
                      </div>
                    </div>
                  </div>
                )
              })}
            </CardContent>
          </Card>

          {/* Export Options */}
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Export Reports</CardTitle>
            </CardHeader>
            <CardContent className="space-y-2">
              <Button className="w-full justify-start" variant="outline">
                <Download className="h-4 w-4 mr-2" />
                Download Performance Report (PDF)
              </Button>
              <Button className="w-full justify-start" variant="outline">
                <Download className="h-4 w-4 mr-2" />
                Export Team Metrics (CSV)
              </Button>
              <Button className="w-full justify-start" variant="outline">
                <Download className="h-4 w-4 mr-2" />
                Export Process Analysis (XLSX)
              </Button>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  )
}
