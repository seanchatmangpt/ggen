import { useCallback, useState } from "react"
import { Node } from "reactflow"

export function useNodeSelection() {
  const [selectedNodeIds, setSelectedNodeIds] = useState<Set<string>>(
    new Set()
  )
  const [focusedNodeId, setFocusedNodeId] = useState<string | null>(null)

  const selectNode = useCallback(
    (nodeId: string, multiSelect: boolean = false) => {
      setSelectedNodeIds((prev) => {
        if (multiSelect) {
          const newSet = new Set(prev)
          if (newSet.has(nodeId)) {
            newSet.delete(nodeId)
          } else {
            newSet.add(nodeId)
          }
          return newSet
        } else {
          return new Set([nodeId])
        }
      })
      setFocusedNodeId(nodeId)
    },
    []
  )

  const deselectNode = useCallback((nodeId: string) => {
    setSelectedNodeIds((prev) => {
      const newSet = new Set(prev)
      newSet.delete(nodeId)
      return newSet
    })
  }, [])

  const selectAll = useCallback((nodeIds: string[]) => {
    setSelectedNodeIds(new Set(nodeIds))
  }, [])

  const clearSelection = useCallback(() => {
    setSelectedNodeIds(new Set())
    setFocusedNodeId(null)
  }, [])

  const isSelected = useCallback(
    (nodeId: string) => selectedNodeIds.has(nodeId),
    [selectedNodeIds]
  )

  const getSelectedNodes = useCallback(
    (nodes: Node[]) => nodes.filter((n) => selectedNodeIds.has(n.id)),
    [selectedNodeIds]
  )

  const toggleNodeSelection = useCallback(
    (nodeId: string) => {
      selectNode(nodeId, true)
    },
    [selectNode]
  )

  const deleteSelected = useCallback(() => {
    return Array.from(selectedNodeIds)
  }, [selectedNodeIds])

  return {
    selectedNodeIds: Array.from(selectedNodeIds),
    focusedNodeId,
    selectNode,
    deselectNode,
    selectAll,
    clearSelection,
    isSelected,
    getSelectedNodes,
    toggleNodeSelection,
    deleteSelected,
    hasSelection: selectedNodeIds.size > 0,
    selectionCount: selectedNodeIds.size,
  }
}
