'use client'

import { useState } from 'react'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { CatalogExplorer } from '@/components/backstage/catalog-explorer'
import { DeploymentPipeline } from '@/components/backstage/deployment-pipeline'
import { ServiceRegistry } from '@/components/backstage/service-registry'
import {
  Boxes,
  Zap,
  GitBranch,
  Users,
  BookOpen,
  Settings,
  Search,
  TrendingUp,
  AlertCircle,
  CheckCircle2,
  Clock,
  BarChart3,
  Plus,
} from 'lucide-react'
import { useTeams, useAPICatalog } from '@/hooks/use-backstage'

/**
 * Backstage IDP Main Page
 * Comprehensive Internal Developer Platform for teams
 */
export default function BackstagePage() {
  const [activeTab, setActiveTab] = useState('overview')
  const [selectedComponent, setSelectedComponent] = useState(null)

  const { teams } = useTeams()
  const { apis } = useAPICatalog()

  // Mock metrics
  const metrics = {
    totalServices: 48,
    activeServices: 42,
    failingServices: 3,
    deploymentSuccess: 94,
    apiUsage: 12500,
    teams: teams.length,
    components: 156,
    apis: apis.length,
  }

  const recentActivity = [
    {
      icon: '✅',
      title: 'Deployment Successful',
      description: 'user-service v2.1.0 deployed to production',
      time: '5 minutes ago',
    },
    {
      icon: '🚀',
      title: 'New Component Released',
      description: 'auth-library v1.5.0 now available',
      time: '1 hour ago',
    },
    {
      icon: '⚠️',
      title: 'Service Alert',
      description: 'payment-api experiencing high latency',
      time: '2 hours ago',
    },
    {
      icon: '👥',
      title: 'Team Update',
      description: 'Platform team added 2 new members',
      time: '3 hours ago',
    },
  ]

  return (
    <div className="min-h-screen bg-slate-50 p-4 md:p-8">
      {/* Header */}
      <div className="mb-8">
        <div className="flex items-center gap-3 mb-2">
          <div className="p-2 bg-gradient-to-br from-purple-100 to-blue-100 rounded-lg">
            <Boxes className="h-6 w-6 text-purple-600" />
          </div>
          <h1 className="text-4xl font-bold">Backstage</h1>
        </div>
        <p className="text-slate-600">
          Internal Developer Platform - Discover, manage, and deploy services
        </p>
      </div>

      {/* Stats Grid */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-8">
        {[
          {
            label: 'Active Services',
            value: metrics.activeServices,
            total: metrics.totalServices,
            icon: CheckCircle2,
            color: 'text-green-600',
          },
          {
            label: 'Deployment Success',
            value: `${metrics.deploymentSuccess}%`,
            icon: TrendingUp,
            color: 'text-blue-600',
          },
          {
            label: 'API Usage',
            value: '12.5K',
            icon: BarChart3,
            color: 'text-purple-600',
          },
          {
            label: 'Teams',
            value: metrics.teams,
            icon: Users,
            color: 'text-orange-600',
          },
        ].map((stat, idx) => {
          const Icon = stat.icon
          return (
            <Card key={idx}>
              <CardContent className="pt-6">
                <div className="flex items-start justify-between">
                  <div>
                    <p className="text-sm font-medium text-slate-600 mb-1">
                      {stat.label}
                    </p>
                    <p className="text-3xl font-bold">{stat.value}</p>
                    {'total' in stat && (
                      <p className="text-xs text-slate-500">
                        of {stat.total} total
                      </p>
                    )}
                  </div>
                  <Icon className={`h-8 w-8 ${stat.color}`} />
                </div>
              </CardContent>
            </Card>
          )
        })}
      </div>

      {/* Main Content Tabs */}
      <Tabs
        value={activeTab}
        onValueChange={setActiveTab}
        className="space-y-4"
      >
        <TabsList className="grid w-full grid-cols-2 md:grid-cols-6">
          <TabsTrigger value="overview" className="gap-2">
            <Zap className="h-4 w-4" />
            <span className="hidden sm:inline">Overview</span>
          </TabsTrigger>
          <TabsTrigger value="catalog" className="gap-2">
            <Boxes className="h-4 w-4" />
            <span className="hidden sm:inline">Catalog</span>
          </TabsTrigger>
          <TabsTrigger value="services" className="gap-2">
            <GitBranch className="h-4 w-4" />
            <span className="hidden sm:inline">Services</span>
          </TabsTrigger>
          <TabsTrigger value="deployments" className="gap-2">
            <Clock className="h-4 w-4" />
            <span className="hidden sm:inline">Deployments</span>
          </TabsTrigger>
          <TabsTrigger value="docs" className="gap-2">
            <BookOpen className="h-4 w-4" />
            <span className="hidden sm:inline">Docs</span>
          </TabsTrigger>
          <TabsTrigger value="admin" className="gap-2">
            <Settings className="h-4 w-4" />
            <span className="hidden sm:inline">Admin</span>
          </TabsTrigger>
        </TabsList>

        {/* Overview Tab */}
        <TabsContent value="overview" className="space-y-6">
          <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
            {/* Quick Start */}
            <Card className="lg:col-span-2">
              <CardHeader>
                <CardTitle>Getting Started</CardTitle>
                <CardDescription>
                  Common actions and shortcuts
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                  {[
                    {
                      icon: '🔍',
                      title: 'Explore Catalog',
                      description: 'Browse available components',
                    },
                    {
                      icon: '➕',
                      title: 'Create Service',
                      description: 'Register a new service',
                    },
                    {
                      icon: '📦',
                      title: 'Deploy',
                      description: 'Create deployment pipeline',
                    },
                    {
                      icon: '👥',
                      title: 'Team Settings',
                      description: 'Manage team members',
                    },
                  ].map((action, idx) => (
                    <Button
                      key={idx}
                      variant="outline"
                      className="h-auto p-4 justify-start text-left"
                    >
                      <div className="text-2xl mr-3">{action.icon}</div>
                      <div>
                        <p className="font-semibold text-sm">{action.title}</p>
                        <p className="text-xs text-slate-600">
                          {action.description}
                        </p>
                      </div>
                    </Button>
                  ))}
                </div>
              </CardContent>
            </Card>

            {/* Notifications */}
            <Card>
              <CardHeader>
                <CardTitle className="text-base">Alerts</CardTitle>
              </CardHeader>
              <CardContent className="space-y-3">
                {[
                  {
                    type: 'warning',
                    title: 'Failing Services',
                    value: `${metrics.failingServices} services`,
                  },
                  {
                    type: 'info',
                    title: 'Total Components',
                    value: metrics.components.toString(),
                  },
                  {
                    type: 'success',
                    title: 'API Endpoints',
                    value: metrics.apis.toString(),
                  },
                ].map((alert, idx) => (
                  <div
                    key={idx}
                    className={`p-3 rounded-lg border ${
                      alert.type === 'warning'
                        ? 'bg-yellow-50 border-yellow-200'
                        : alert.type === 'info'
                          ? 'bg-blue-50 border-blue-200'
                          : 'bg-green-50 border-green-200'
                    }`}
                  >
                    <p className="text-xs font-semibold">{alert.title}</p>
                    <p className="text-sm font-bold mt-1">{alert.value}</p>
                  </div>
                ))}
              </CardContent>
            </Card>
          </div>

          {/* Recent Activity */}
          <Card>
            <CardHeader>
              <CardTitle>Recent Activity</CardTitle>
              <CardDescription>Latest events across the platform</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {recentActivity.map((activity, idx) => (
                  <div
                    key={idx}
                    className="flex gap-3 pb-3 border-b last:border-0"
                  >
                    <span className="text-xl mt-0.5">{activity.icon}</span>
                    <div className="flex-1 min-w-0">
                      <p className="font-medium text-sm">{activity.title}</p>
                      <p className="text-xs text-slate-600 mt-1">
                        {activity.description}
                      </p>
                      <p className="text-xs text-slate-500 mt-1">
                        {activity.time}
                      </p>
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Catalog Tab */}
        <TabsContent value="catalog" className="space-y-4">
          <Card className="mb-4">
            <CardHeader>
              <CardTitle>Component Catalog</CardTitle>
              <CardDescription>
                Discover and use pre-built components, services, and templates
              </CardDescription>
            </CardHeader>
          </Card>
          <CatalogExplorer onSelectComponent={setSelectedComponent} />
        </TabsContent>

        {/* Services Tab */}
        <TabsContent value="services" className="space-y-4">
          <Card className="mb-4">
            <CardHeader>
              <CardTitle>Service Registry</CardTitle>
              <CardDescription>
                Manage and monitor all registered services
              </CardDescription>
            </CardHeader>
          </Card>
          <ServiceRegistry />
        </TabsContent>

        {/* Deployments Tab */}
        <TabsContent value="deployments" className="space-y-4">
          <Card className="mb-4">
            <CardHeader>
              <CardTitle>Deployment Pipelines</CardTitle>
              <CardDescription>
                View and manage CI/CD pipelines and deployments
              </CardDescription>
            </CardHeader>
          </Card>
          <DeploymentPipeline />
        </TabsContent>

        {/* Documentation Tab */}
        <TabsContent value="docs" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Documentation</CardTitle>
              <CardDescription>
                Knowledge base and runbooks
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                {[
                  {
                    title: 'Getting Started',
                    description: 'Setup and configuration guide',
                    pages: 12,
                  },
                  {
                    title: 'API Documentation',
                    description: 'Complete API reference',
                    pages: 28,
                  },
                  {
                    title: 'Deployment Guide',
                    description: 'How to deploy services',
                    pages: 15,
                  },
                  {
                    title: 'Troubleshooting',
                    description: 'Common issues and solutions',
                    pages: 23,
                  },
                ].map((doc, idx) => (
                  <Button
                    key={idx}
                    variant="outline"
                    className="h-auto p-4 justify-between"
                  >
                    <div className="text-left">
                      <p className="font-semibold">{doc.title}</p>
                      <p className="text-xs text-slate-600">
                        {doc.description}
                      </p>
                    </div>
                    <Badge variant="secondary">{doc.pages} pages</Badge>
                  </Button>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Admin Tab */}
        <TabsContent value="admin" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Administration</CardTitle>
              <CardDescription>
                Platform configuration and management
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                {[
                  {
                    title: 'User Management',
                    description: 'Manage users and permissions',
                    icon: '👥',
                  },
                  {
                    title: 'Team Settings',
                    description: 'Configure team preferences',
                    icon: '⚙️',
                  },
                  {
                    title: 'Integration',
                    description: 'Connect external services',
                    icon: '🔗',
                  },
                  {
                    title: 'Audit Logs',
                    description: 'View platform activity logs',
                    icon: '📋',
                  },
                ].map((admin, idx) => (
                  <Button
                    key={idx}
                    variant="outline"
                    className="h-auto p-4 justify-start"
                  >
                    <div className="text-2xl mr-3">{admin.icon}</div>
                    <div className="text-left">
                      <p className="font-semibold text-sm">{admin.title}</p>
                      <p className="text-xs text-slate-600">
                        {admin.description}
                      </p>
                    </div>
                  </Button>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  )
}
