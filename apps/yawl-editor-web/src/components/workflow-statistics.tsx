"use client"

import React, { useMemo } from "react"
import { Node, Edge } from "reactflow"
import { Badge } from "@/components/ui/badge"
import {
  Circle,
  Square,
  Diamond,
  ArrowRight,
  Layers,
} from "lucide-react"

interface WorkflowStatisticsProps {
  nodes: Node[]
  edges: Edge[]
}

export function WorkflowStatistics({ nodes, edges }: WorkflowStatisticsProps) {
  const stats = useMemo(() => {
    const nodeTypes: Record<string, number> = {}
    nodes.forEach((node) => {
      nodeTypes[node.type || "unknown"] = (nodeTypes[node.type || "unknown"] || 0) + 1
    })

    const maxDepth = calculateWorkflowDepth(nodes, edges)
    const avgInDegree =
      edges.length > 0 ? edges.length / Math.max(nodes.length, 1) : 0

    return {
      totalNodes: nodes.length,
      totalEdges: edges.length,
      nodeTypes,
      maxDepth,
      avgConnectivity: avgInDegree.toFixed(2),
    }
  }, [nodes, edges])

  const getNodeTypeIcon = (type: string) => {
    switch (type) {
      case "start":
      case "end":
        return <Circle className="h-4 w-4" />
      case "task":
        return <Square className="h-4 w-4" />
      case "condition":
      case "split":
      case "join":
        return <Diamond className="h-4 w-4" />
      default:
        return <Layers className="h-4 w-4" />
    }
  }

  const getNodeTypeColor = (type: string) => {
    switch (type) {
      case "start":
        return "bg-green-100 text-green-800"
      case "end":
        return "bg-red-100 text-red-800"
      case "task":
        return "bg-blue-100 text-blue-800"
      case "condition":
      case "split":
        return "bg-cyan-100 text-cyan-800"
      case "join":
        return "bg-purple-100 text-purple-800"
      default:
        return "bg-gray-100 text-gray-800"
    }
  }

  return (
    <div className="space-y-4">
      <div className="grid grid-cols-4 gap-2">
        <div className="p-3 bg-muted rounded-lg">
          <div className="text-xs text-muted-foreground">Total Nodes</div>
          <div className="text-2xl font-bold">{stats.totalNodes}</div>
        </div>
        <div className="p-3 bg-muted rounded-lg">
          <div className="text-xs text-muted-foreground flex items-center gap-1">
            <ArrowRight className="h-3 w-3" />
            Connections
          </div>
          <div className="text-2xl font-bold">{stats.totalEdges}</div>
        </div>
        <div className="p-3 bg-muted rounded-lg">
          <div className="text-xs text-muted-foreground">Max Depth</div>
          <div className="text-2xl font-bold">{stats.maxDepth}</div>
        </div>
        <div className="p-3 bg-muted rounded-lg">
          <div className="text-xs text-muted-foreground">Avg Connectivity</div>
          <div className="text-2xl font-bold">{stats.avgConnectivity}</div>
        </div>
      </div>

      <div className="space-y-2">
        <h4 className="text-sm font-semibold">Node Types</h4>
        <div className="flex flex-wrap gap-2">
          {Object.entries(stats.nodeTypes).map(([type, count]) => (
            <div key={type} className="flex items-center gap-2">
              <div className={`p-1 rounded ${getNodeTypeColor(type)}`}>
                {getNodeTypeIcon(type)}
              </div>
              <Badge variant="outline" className="capitalize">
                {type}: {count}
              </Badge>
            </div>
          ))}
        </div>
      </div>
    </div>
  )
}

// Calculate the maximum depth of the workflow
function calculateWorkflowDepth(nodes: Node[], edges: Edge[]): number {
  if (nodes.length === 0) return 0

  const adjacencyList = new Map<string, string[]>()
  nodes.forEach((node) => adjacencyList.set(node.id, []))
  edges.forEach((edge) => {
    const neighbors = adjacencyList.get(edge.source) || []
    neighbors.push(edge.target)
    adjacencyList.set(edge.source, neighbors)
  })

  const depths = new Map<string, number>()
  const visited = new Set<string>()

  function dfs(nodeId: string): number {
    if (visited.has(nodeId)) {
      return depths.get(nodeId) || 0
    }

    visited.add(nodeId)
    const neighbors = adjacencyList.get(nodeId) || []

    if (neighbors.length === 0) {
      depths.set(nodeId, 1)
      return 1
    }

    const maxChildDepth = Math.max(...neighbors.map((n) => dfs(n)), 0)
    const depth = maxChildDepth + 1
    depths.set(nodeId, depth)
    return depth
  }

  let maxDepth = 0
  for (const nodeId of adjacencyList.keys()) {
    maxDepth = Math.max(maxDepth, dfs(nodeId))
  }

  return maxDepth
}
