import { useCallback, useState } from "react"
import { Node, Edge } from "reactflow"

export interface WorkflowState {
  nodes: Node[]
  edges: Edge[]
}

export function useWorkflow(initialNodes: Node[], initialEdges: Edge[]) {
  const [nodes, setNodes] = useState<Node[]>(initialNodes)
  const [edges, setEdges] = useState<Edge[]>(initialEdges)

  const addNode = useCallback(
    (node: Node) => {
      setNodes((prev) => [...prev, node])
    },
    []
  )

  const removeNode = useCallback((nodeId: string) => {
    setNodes((prev) => prev.filter((n) => n.id !== nodeId))
    setEdges((prev) =>
      prev.filter((e) => e.source !== nodeId && e.target !== nodeId)
    )
  }, [])

  const updateNode = useCallback((nodeId: string, updates: Partial<Node>) => {
    setNodes((prev) =>
      prev.map((n) => (n.id === nodeId ? { ...n, ...updates } : n))
    )
  }, [])

  const addEdge = useCallback((edge: Edge) => {
    setEdges((prev) => [...prev, edge])
  }, [])

  const removeEdge = useCallback((edgeId: string) => {
    setEdges((prev) => prev.filter((e) => e.id !== edgeId))
  }, [])

  const clearAll = useCallback(() => {
    setNodes([])
    setEdges([])
  }, [])

  const getNodeById = useCallback(
    (nodeId: string) => {
      return nodes.find((n) => n.id === nodeId)
    },
    [nodes]
  )

  const getEdgeById = useCallback(
    (edgeId: string) => {
      return edges.find((e) => e.id === edgeId)
    },
    [edges]
  )

  const getConnectedNodes = useCallback(
    (nodeId: string) => {
      const connectedIds = new Set<string>()
      edges.forEach((e) => {
        if (e.source === nodeId) connectedIds.add(e.target)
        if (e.target === nodeId) connectedIds.add(e.source)
      })
      return nodes.filter((n) => connectedIds.has(n.id))
    },
    [nodes, edges]
  )

  const getNodesByType = useCallback(
    (type: string) => {
      return nodes.filter((n) => n.type === type)
    },
    [nodes]
  )

  return {
    nodes,
    edges,
    setNodes,
    setEdges,
    addNode,
    removeNode,
    updateNode,
    addEdge,
    removeEdge,
    clearAll,
    getNodeById,
    getEdgeById,
    getConnectedNodes,
    getNodesByType,
  }
}
