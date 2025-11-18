"use client"

import React, { useState } from "react"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { BarChart3, TrendingUp, AlertTriangle, Zap, Clock, DollarSign, Shield, Activity } from "lucide-react"

interface MetricCard {
  label: string
  value: string | number
  unit?: string
  trend?: number
  status?: "good" | "warning" | "critical"
  icon: React.ReactNode
}

export function AnalyticsDashboard() {
  const [timeRange, setTimeRange] = useState("7d")

  const performanceMetrics: MetricCard[] = [
    {
      label: "Avg Execution Time",
      value: "2.45",
      unit: "s",
      trend: -12,
      status: "good",
      icon: <Clock className="h-5 w-5" />,
    },
    {
      label: "Throughput",
      value: "1,250",
      unit: "ops/min",
      trend: 8,
      status: "good",
      icon: <Zap className="h-5 w-5" />,
    },
    {
      label: "Success Rate",
      value: "99.2",
      unit: "%",
      trend: 1,
      status: "good",
      icon: <TrendingUp className="h-5 w-5" />,
    },
    {
      label: "Error Rate",
      value: "0.8",
      unit: "%",
      trend: -2,
      status: "good",
      icon: <AlertTriangle className="h-5 w-5" />,
    },
  ]

  const reliabilityMetrics: MetricCard[] = [
    {
      label: "MTBF",
      value: "847",
      unit: "hours",
      trend: 15,
      status: "good",
      icon: <Shield className="h-5 w-5" />,
    },
    {
      label: "MTTR",
      value: "12.3",
      unit: "min",
      trend: -8,
      status: "good",
      icon: <Clock className="h-5 w-5" />,
    },
    {
      label: "Uptime SLA",
      value: "99.95",
      unit: "%",
      trend: 0.05,
      status: "good",
      icon: <Activity className="h-5 w-5" />,
    },
    {
      label: "Incident Count",
      value: "2",
      unit: "this week",
      trend: -3,
      status: "good",
      icon: <AlertTriangle className="h-5 w-5" />,
    },
  ]

  const costMetrics: MetricCard[] = [
    {
      label: "Monthly Cost",
      value: "$2,450",
      unit: "USD",
      trend: -5,
      status: "good",
      icon: <DollarSign className="h-5 w-5" />,
    },
    {
      label: "Cost per Operation",
      value: "0.0019",
      unit: "USD",
      trend: -3,
      status: "good",
      icon: <DollarSign className="h-5 w-5" />,
    },
    {
      label: "Resource Utilization",
      value: "67",
      unit: "%",
      trend: 5,
      status: "good",
      icon: <Zap className="h-5 w-5" />,
    },
    {
      label: "Potential Savings",
      value: "$340",
      unit: "/month",
      trend: 12,
      status: "good",
      icon: <TrendingUp className="h-5 w-5" />,
    },
  ]

  const renderMetricCard = (metric: MetricCard, index: number) => (
    <Card key={index} className="bg-background border border-border">
      <CardContent className="pt-6">
        <div className="flex items-start justify-between">
          <div>
            <p className="text-xs text-muted-foreground mb-1">{metric.label}</p>
            <div className="flex items-baseline gap-1">
              <span className="text-2xl font-bold">{metric.value}</span>
              {metric.unit && <span className="text-xs text-muted-foreground">{metric.unit}</span>}
            </div>
          </div>
          <div
            className={`p-2 rounded-lg ${
              metric.status === "good"
                ? "bg-green-500/10 text-green-600"
                : metric.status === "warning"
                  ? "bg-yellow-500/10 text-yellow-600"
                  : "bg-red-500/10 text-red-600"
            }`}
          >
            {metric.icon}
          </div>
        </div>
        {metric.trend !== undefined && (
          <div className="mt-3 text-xs">
            <span
              className={
                metric.trend >= 0 ? "text-green-600" : "text-red-600"
              }
            >
              {metric.trend >= 0 ? "+" : ""}{metric.trend}% vs last period
            </span>
          </div>
        )}
      </CardContent>
    </Card>
  )

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-3xl font-bold tracking-tight">Analytics Dashboard</h2>
          <p className="text-muted-foreground mt-1">Comprehensive workflow metrics and insights</p>
        </div>
        <select
          value={timeRange}
          onChange={(e) => setTimeRange(e.target.value)}
          className="px-3 py-2 rounded-lg border border-border bg-background text-sm"
        >
          <option value="24h">Last 24 Hours</option>
          <option value="7d">Last 7 Days</option>
          <option value="30d">Last 30 Days</option>
          <option value="90d">Last 90 Days</option>
        </select>
      </div>

      {/* Tabs */}
      <Tabs defaultValue="performance" className="w-full">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="performance">Performance</TabsTrigger>
          <TabsTrigger value="reliability">Reliability</TabsTrigger>
          <TabsTrigger value="cost">Cost</TabsTrigger>
          <TabsTrigger value="insights">AI Insights</TabsTrigger>
        </TabsList>

        <TabsContent value="performance" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            {performanceMetrics.map(renderMetricCard)}
          </div>

          {/* Performance Chart */}
          <Card className="bg-background border border-border">
            <CardHeader>
              <CardTitle>Execution Time Trends</CardTitle>
              <CardDescription>Average workflow execution time over time</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="h-64 flex items-center justify-center bg-muted/50 rounded-lg">
                <div className="text-center text-muted-foreground">
                  <BarChart3 className="h-12 w-12 mx-auto mb-2 opacity-50" />
                  <p>Time series chart</p>
                </div>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="reliability" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            {reliabilityMetrics.map(renderMetricCard)}
          </div>

          {/* SLA Compliance */}
          <Card className="bg-background border border-border">
            <CardHeader>
              <CardTitle>SLA Compliance</CardTitle>
              <CardDescription>Service level agreement adherence</CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              {[
                { name: "Availability", value: 99.95, target: 99.9 },
                { name: "Response Time", value: 150, target: 200, unit: "ms" },
                { name: "Error Rate", value: 0.05, target: 0.1, unit: "%" },
              ].map((sla, idx) => (
                <div key={idx}>
                  <div className="flex justify-between text-sm mb-1">
                    <span>{sla.name}</span>
                    <span className="font-medium">
                      {sla.value}
                      {sla.unit}
                    </span>
                  </div>
                  <div className="w-full h-2 rounded-full bg-muted">
                    <div
                      className="h-full rounded-full bg-green-500"
                      style={{ width: `${Math.min((sla.value / sla.target) * 100, 100)}%` }}
                    />
                  </div>
                </div>
              ))}
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="cost" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            {costMetrics.map(renderMetricCard)}
          </div>

          {/* Cost Breakdown */}
          <Card className="bg-background border border-border">
            <CardHeader>
              <CardTitle>Cost Breakdown</CardTitle>
              <CardDescription>Resource allocation and spending</CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              {[
                { label: "Compute", amount: 1200, percentage: 49 },
                { label: "Storage", amount: 600, percentage: 24 },
                { label: "Network", amount: 400, percentage: 16 },
                { label: "Other", amount: 250, percentage: 11 },
              ].map((item, idx) => (
                <div key={idx}>
                  <div className="flex justify-between text-sm mb-1">
                    <span>{item.label}</span>
                    <span className="font-medium">${item.amount}</span>
                  </div>
                  <div className="w-full h-2 rounded-full bg-muted">
                    <div
                      className="h-full rounded-full bg-blue-500"
                      style={{ width: `${item.percentage}%` }}
                    />
                  </div>
                  <p className="text-xs text-muted-foreground mt-1">{item.percentage}% of total</p>
                </div>
              ))}
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="insights" className="space-y-4">
          <Card className="bg-background border border-border">
            <CardHeader>
              <CardTitle>AI-Generated Insights</CardTitle>
              <CardDescription>Machine learning powered recommendations</CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              {[
                {
                  type: "prediction",
                  title: "Peak Load Expected",
                  description: "Based on historical patterns, expect 40% increase in traffic in the next 2 hours",
                  action: "Auto-scale resources",
                },
                {
                  type: "anomaly",
                  title: "Unusual Error Pattern",
                  description: "Error rate increased by 150% compared to baseline - investigate database connection pool",
                  action: "View error logs",
                },
                {
                  type: "optimization",
                  title: "Cost Savings Opportunity",
                  description: "Task C is over-provisioned - can reduce instance size without impacting performance",
                  action: "Apply optimization",
                },
                {
                  type: "security",
                  title: "Security Risk Detected",
                  description: "API endpoint handling sensitive data lacks rate limiting",
                  action: "Implement rate limiting",
                },
              ].map((insight, idx) => (
                <div
                  key={idx}
                  className="p-4 rounded-lg border border-border bg-muted/50 hover:bg-muted transition-colors"
                >
                  <div className="flex items-start justify-between">
                    <div className="flex-1">
                      <h4 className="font-medium mb-1">{insight.title}</h4>
                      <p className="text-sm text-muted-foreground">{insight.description}</p>
                    </div>
                    <button className="text-xs px-3 py-1 rounded bg-primary text-primary-foreground hover:bg-primary/90 whitespace-nowrap ml-4">
                      {insight.action}
                    </button>
                  </div>
                </div>
              ))}
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  )
}
