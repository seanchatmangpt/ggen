import { useMemo } from "react"
import { Node, Edge } from "reactflow"

export interface ValidationError {
  id: string
  type: "error" | "warning" | "info"
  message: string
  nodeId?: string
  edgeId?: string
}

export function useWorkflowValidation(nodes: Node[], edges: Edge[]) {
  const errors = useMemo<ValidationError[]>(() => {
    const issues: ValidationError[] = []

    // Check for at least one start node
    const startNodes = nodes.filter((n) => n.type === "start")
    if (startNodes.length === 0) {
      issues.push({
        id: "no-start",
        type: "error",
        message: "Workflow must have at least one Start event",
      })
    }
    if (startNodes.length > 1) {
      issues.push({
        id: "multiple-starts",
        type: "warning",
        message: "Workflow has multiple Start events",
      })
    }

    // Check for at least one end node
    const endNodes = nodes.filter((n) => n.type === "end")
    if (endNodes.length === 0) {
      issues.push({
        id: "no-end",
        type: "error",
        message: "Workflow must have at least one End event",
      })
    }
    if (endNodes.length > 1) {
      issues.push({
        id: "multiple-ends",
        type: "info",
        message: "Workflow has multiple End events",
      })
    }

    // Check for disconnected nodes
    nodes.forEach((node) => {
      if (node.type === "start" || node.type === "end") return

      const hasIncoming = edges.some((e) => e.target === node.id)
      const hasOutgoing = edges.some((e) => e.source === node.id)

      if (!hasIncoming) {
        issues.push({
          id: `no-incoming-${node.id}`,
          type: "warning",
          message: `Node "${node.data?.label}" has no incoming connections`,
          nodeId: node.id,
        })
      }

      if (!hasOutgoing) {
        issues.push({
          id: `no-outgoing-${node.id}`,
          type: "warning",
          message: `Node "${node.data?.label}" has no outgoing connections`,
          nodeId: node.id,
        })
      }
    })

    // Check for cycles
    const hasCycle = detectCycles(nodes, edges)
    if (hasCycle) {
      issues.push({
        id: "cycle-detected",
        type: "warning",
        message: "Workflow contains cycles",
      })
    }

    // Check for orphaned nodes
    nodes.forEach((node) => {
      const connected = edges.some(
        (e) => e.source === node.id || e.target === node.id
      )
      if (!connected && node.type !== "start" && node.type !== "end") {
        issues.push({
          id: `orphaned-${node.id}`,
          type: "warning",
          message: `Node "${node.data?.label}" is not connected`,
          nodeId: node.id,
        })
      }
    })

    // Check for invalid edges
    edges.forEach((edge) => {
      const sourceExists = nodes.some((n) => n.id === edge.source)
      const targetExists = nodes.some((n) => n.id === edge.target)

      if (!sourceExists || !targetExists) {
        issues.push({
          id: `invalid-edge-${edge.id}`,
          type: "error",
          message: "Edge references non-existent nodes",
          edgeId: edge.id,
        })
      }
    })

    return issues
  }, [nodes, edges])

  const isValid = useMemo(() => {
    return errors.every((e) => e.type !== "error")
  }, [errors])

  const errorCount = useMemo(
    () => errors.filter((e) => e.type === "error").length,
    [errors]
  )

  const warningCount = useMemo(
    () => errors.filter((e) => e.type === "warning").length,
    [errors]
  )

  const infoCount = useMemo(
    () => errors.filter((e) => e.type === "info").length,
    [errors]
  )

  const getNodeErrors = (nodeId: string) => {
    return errors.filter((e) => e.nodeId === nodeId)
  }

  const getEdgeErrors = (edgeId: string) => {
    return errors.filter((e) => e.edgeId === edgeId)
  }

  return {
    errors,
    isValid,
    errorCount,
    warningCount,
    infoCount,
    getNodeErrors,
    getEdgeErrors,
    hasIssues: errors.length > 0,
  }
}

// Helper function to detect cycles using DFS
function detectCycles(nodes: Node[], edges: Edge[]): boolean {
  const adjacencyList = new Map<string, string[]>()

  nodes.forEach((node) => {
    adjacencyList.set(node.id, [])
  })

  edges.forEach((edge) => {
    const neighbors = adjacencyList.get(edge.source) || []
    neighbors.push(edge.target)
    adjacencyList.set(edge.source, neighbors)
  })

  const visited = new Set<string>()
  const recursionStack = new Set<string>()

  function hasCycleDFS(nodeId: string): boolean {
    visited.add(nodeId)
    recursionStack.add(nodeId)

    const neighbors = adjacencyList.get(nodeId) || []
    for (const neighbor of neighbors) {
      if (!visited.has(neighbor)) {
        if (hasCycleDFS(neighbor)) {
          return true
        }
      } else if (recursionStack.has(neighbor)) {
        return true
      }
    }

    recursionStack.delete(nodeId)
    return false
  }

  for (const nodeId of adjacencyList.keys()) {
    if (!visited.has(nodeId)) {
      if (hasCycleDFS(nodeId)) {
        return true
      }
    }
  }

  return false
}
