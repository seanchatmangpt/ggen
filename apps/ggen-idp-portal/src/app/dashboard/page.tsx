'use client'

import React, { useEffect, useState } from 'react'
import { useAuthStore } from '@/stores/authStore'
import { DashboardMetrics } from '@/types'
import { Users, Lock, Activity, AlertCircle, TrendingUp } from 'lucide-react'

const mockMetrics: DashboardMetrics = {
  totalUsers: 1250,
  activeUsers: 892,
  totalAuthFlows: 24,
  failedLogins: 12,
  avgSessionDuration: 3600,
  auditLogCount: 5240,
}

export default function DashboardPage() {
  const { user, organization } = useAuthStore()
  const [metrics, setMetrics] = useState<DashboardMetrics>(mockMetrics)
  const [isLoading, setIsLoading] = useState(false)

  useEffect(() => {
    // Load metrics from API
    setIsLoading(true)
    // TODO: Load actual metrics from IDP backend
    setTimeout(() => setIsLoading(false), 500)
  }, [organization?.id])

  return (
    <div className="space-y-6">
      {/* Welcome Header */}
      <div className="space-y-2">
        <h1 className="text-3xl font-bold text-slate-900">
          Welcome, {user?.displayName || user?.username}!
        </h1>
        <p className="text-slate-600">
          {organization?.name} • Identity Provider Management Dashboard
        </p>
      </div>

      {/* Key Metrics */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        <MetricCard
          icon={<Users size={24} />}
          title="Total Users"
          value={metrics.totalUsers}
          subtitle={`${metrics.activeUsers} active`}
          color="blue"
        />
        <MetricCard
          icon={<Activity size={24} />}
          title="Active Sessions"
          value={metrics.avgSessionDuration}
          subtitle="avg duration"
          color="green"
        />
        <MetricCard
          icon={<Lock size={24} />}
          title="Auth Flows"
          value={metrics.totalAuthFlows}
          subtitle="configured"
          color="purple"
        />
        <MetricCard
          icon={<AlertCircle size={24} />}
          title="Failed Logins"
          value={metrics.failedLogins}
          subtitle="in last 24h"
          color="red"
        />
        <MetricCard
          icon={<TrendingUp size={24} />}
          title="Audit Logs"
          value={metrics.auditLogCount}
          subtitle="recorded"
          color="cyan"
        />
      </div>

      {/* Quick Actions */}
      <div className="card">
        <h2 className="text-xl font-bold mb-4">Quick Actions</h2>
        <div className="grid grid-cols-2 md:grid-cols-4 gap-3">
          <QuickActionButton href="/auth-flows" label="Auth Flows" />
          <QuickActionButton href="/roles" label="Manage Roles" />
          <QuickActionButton href="/users" label="Users" />
          <QuickActionButton href="/audit" label="Audit Logs" />
          <QuickActionButton href="/oauth2" label="OAuth2" />
          <QuickActionButton href="/settings" label="Settings" />
        </div>
      </div>

      {/* Recent Activity */}
      <div className="card">
        <h2 className="text-xl font-bold mb-4">Recent Activity</h2>
        <div className="space-y-3">
          {[1, 2, 3].map((i) => (
            <div key={i} className="flex items-start gap-3 pb-3 border-b border-slate-200 last:border-0">
              <div className="w-2 h-2 rounded-full bg-blue-600 mt-2 flex-shrink-0" />
              <div className="flex-1 min-w-0">
                <p className="font-medium text-slate-900">User login</p>
                <p className="text-sm text-slate-600">192.168.1.{100 + i} • 2 hours ago</p>
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  )
}

interface MetricCardProps {
  icon: React.ReactNode
  title: string
  value: number | string
  subtitle: string
  color: 'blue' | 'green' | 'purple' | 'red' | 'cyan'
}

function MetricCard({ icon, title, value, subtitle, color }: MetricCardProps) {
  const colorMap = {
    blue: 'bg-blue-50 text-blue-600',
    green: 'bg-green-50 text-green-600',
    purple: 'bg-purple-50 text-purple-600',
    red: 'bg-red-50 text-red-600',
    cyan: 'bg-cyan-50 text-cyan-600',
  }

  return (
    <div className="card">
      <div className={`${colorMap[color]} w-12 h-12 rounded-lg flex items-center justify-center mb-3`}>
        {icon}
      </div>
      <h3 className="text-sm font-medium text-slate-600">{title}</h3>
      <p className="text-2xl font-bold text-slate-900 mt-1">{value}</p>
      <p className="text-xs text-slate-500 mt-1">{subtitle}</p>
    </div>
  )
}

interface QuickActionButtonProps {
  href: string
  label: string
}

function QuickActionButton({ href, label }: QuickActionButtonProps) {
  return (
    <a
      href={href}
      className="px-4 py-2 rounded-lg bg-slate-100 hover:bg-slate-200 text-slate-700 font-medium text-center transition-colors"
    >
      {label}
    </a>
  )
}
