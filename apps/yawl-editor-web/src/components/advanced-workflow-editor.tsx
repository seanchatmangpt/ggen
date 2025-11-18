"use client"

import React, { useState, useEffect, useCallback } from "react"
import ReactFlow, {
  Node,
  Edge,
  Controls,
  Background,
  useNodesState,
  useEdgesState,
  MiniMap,
  addEdge,
  Connection,
  Panel,
} from "reactflow"
import "reactflow/dist/style.css"

import { Button } from "@/components/ui/button"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogFooter,
} from "@/components/ui/dialog"

import { TaskNode } from "@/components/nodes/task-node"
import { ConditionNode } from "@/components/nodes/condition-node"
import { StartNode } from "@/components/nodes/start-node"
import { EndNode } from "@/components/nodes/end-node"
import { JoinNode } from "@/components/nodes/join-node"
import { SplitNode } from "@/components/nodes/split-node"

import { NodePropertyEditor } from "@/components/node-property-editor"
import { ValidationReport } from "@/components/validation-report"
import { WorkflowStatistics } from "@/components/workflow-statistics"
import { EditorToolbar } from "@/components/editor-toolbar"

import {
  useHistory,
  useNodeSelection,
  useWorkflowValidation,
  usePersist,
} from "@/hooks"

import {
  Plus,
  AlertCircle,
  BarChart3,
} from "lucide-react"

const nodeTypes = {
  task: TaskNode,
  condition: ConditionNode,
  start: StartNode,
  end: EndNode,
  join: JoinNode,
  split: SplitNode,
}

const initialNodes: Node[] = [
  {
    id: "1",
    data: { label: "Start", description: "" },
    position: { x: 250, y: 0 },
    type: "start",
  },
  {
    id: "2",
    data: { label: "Task 1", description: "" },
    position: { x: 250, y: 100 },
    type: "task",
  },
  {
    id: "3",
    data: { label: "End", description: "" },
    position: { x: 250, y: 200 },
    type: "end",
  },
]

const initialEdges: Edge[] = [
  { id: "e1-2", source: "1", target: "2" },
  { id: "e2-3", source: "2", target: "3" },
]

export function AdvancedWorkflowEditor() {
  const [nodes, setNodes, onNodesChange] = useNodesState(initialNodes)
  const [edges, setEdges, onEdgesChange] = useEdgesState(initialEdges)
  const [nodeIdCounter, setNodeIdCounter] = useState(4)

  const history = useHistory({ nodes, edges }, 50)
  const selection = useNodeSelection()
  const validation = useWorkflowValidation(nodes, edges)
  const [workflowName, saveWorkflowName] = usePersist("workflow-name", "Untitled Workflow")

  const [selectedNode, setSelectedNode] = useState<Node | null>(null)
  const [isPropertyEditorOpen, setIsPropertyEditorOpen] = useState(false)
  const [isNameDialogOpen, setIsNameDialogOpen] = useState(false)
  const [newWorkflowName, setNewWorkflowName] = useState(workflowName)
  const [clipboard, setClipboard] = useState<{ nodes: Node[]; edges: Edge[] } | null>(null)

  // Sync workflow state with history
  // eslint-disable-next-line react-hooks/exhaustive-deps
  useEffect(() => {
    history.push({ nodes, edges })
  }, [nodes, edges])

  const onConnect = useCallback(
    (connection: Connection) => {
      setEdges((eds) => addEdge(connection, eds))
    },
    [setEdges]
  )

  const addNode = useCallback(
    (type: string, label: string) => {
      const newNode: Node = {
        id: `${nodeIdCounter}`,
        data: { label, description: "", properties: {} },
        position: { x: 250, y: 300 + nodeIdCounter * 50 },
        type,
      }
      setNodes((nds) => [...nds, newNode])
      setNodeIdCounter((prev) => prev + 1)
    },
    [nodeIdCounter, setNodes]
  )

  const handleNodeDoubleClick = (node: Node) => {
    setSelectedNode(node)
    setIsPropertyEditorOpen(true)
    selection.selectNode(node.id)
  }

  const handleSaveProperties = (nodeId: string, data: any) => {
    setNodes((nds) =>
      nds.map((n) => (n.id === nodeId ? { ...n, data } : n))
    )
  }

  const handleDelete = useCallback(() => {
    const nodesToDelete = selection.deleteSelected()
    setNodes((nds) => nds.filter((n) => !nodesToDelete.includes(n.id)))
    setEdges((eds) =>
      eds.filter(
        (e) => !nodesToDelete.includes(e.source) && !nodesToDelete.includes(e.target)
      )
    )
    selection.clearSelection()
  }, [selection, setNodes, setEdges])

  const handleCopy = useCallback(() => {
    const nodesToCopy = selection.getSelectedNodes(nodes)
    if (nodesToCopy.length === 0) return

    const edgesToCopy = edges.filter(
      (e) =>
        nodesToCopy.some((n) => n.id === e.source) &&
        nodesToCopy.some((n) => n.id === e.target)
    )

    setClipboard({ nodes: nodesToCopy, edges: edgesToCopy })
  }, [nodes, edges, selection])

  const handlePaste = useCallback(() => {
    if (!clipboard || clipboard.nodes.length === 0) return

    const idMap = new Map<string, string>()
    const offset = { x: 50, y: 50 }

    const newNodes = clipboard.nodes.map((node) => {
      const newId = `${nodeIdCounter + Math.random()}`
      idMap.set(node.id, newId)
      return {
        ...node,
        id: newId,
        position: {
          x: node.position.x + offset.x,
          y: node.position.y + offset.y,
        },
      }
    })

    const newEdges = clipboard.edges.map((edge) => ({
      ...edge,
      id: `e${idMap.get(edge.source)}-${idMap.get(edge.target)}`,
      source: idMap.get(edge.source) || edge.source,
      target: idMap.get(edge.target) || edge.target,
    }))

    setNodes((nds) => [...nds, ...newNodes])
    setEdges((eds) => [...eds, ...newEdges])
    setNodeIdCounter((prev) => prev + newNodes.length)
  }, [clipboard, nodeIdCounter, setNodes, setEdges])

  const handleExport = useCallback(() => {
    const workflow = { nodes, edges, name: workflowName }
    const dataStr = JSON.stringify(workflow, null, 2)
    const dataUri =
      "data:application/json;charset=utf-8," + encodeURIComponent(dataStr)
    const exportFileDefaultName = `${workflowName || "workflow"}.json`
    const linkElement = document.createElement("a")
    linkElement.setAttribute("href", dataUri)
    linkElement.setAttribute("download", exportFileDefaultName)
    linkElement.click()
  }, [nodes, edges, workflowName])

  const handleImport = useCallback(() => {
    const input = document.createElement("input")
    input.type = "file"
    input.accept = "application/json"
    input.onchange = (e: any) => {
      const file = e.target.files[0]
      const reader = new FileReader()
      reader.onload = (event: any) => {
        try {
          const data = JSON.parse(event.target.result)
          setNodes(data.nodes || [])
          setEdges(data.edges || [])
          if (data.name) saveWorkflowName(data.name)
          selection.clearSelection()
        } catch (error) {
          console.error("Failed to import workflow", error)
        }
      }
      reader.readAsText(file)
    }
    input.click()
  }, [setNodes, setEdges, saveWorkflowName, selection])

  const handleKeyDown = useCallback(
    (e: KeyboardEvent) => {
      if (e.ctrlKey || e.metaKey) {
        switch (e.key.toLowerCase()) {
          case "z":
            e.preventDefault()
            if (history.canUndo) {
              history.undo()
              setNodes(history.state?.nodes || [])
              setEdges(history.state?.edges || [])
            }
            break
          case "y":
            e.preventDefault()
            if (history.canRedo) {
              history.redo()
              setNodes(history.state?.nodes || [])
              setEdges(history.state?.edges || [])
            }
            break
          case "c":
            e.preventDefault()
            handleCopy()
            break
          case "v":
            e.preventDefault()
            handlePaste()
            break
          case "a":
            e.preventDefault()
            selection.selectAll(nodes.map((n) => n.id))
            break
          case "s":
            e.preventDefault()
            handleExport()
            break
        }
      } else if (e.key === "Delete") {
        e.preventDefault()
        if (selection.hasSelection) handleDelete()
      }
    },
    [history, selection, nodes, handleCopy, handlePaste, handleDelete, handleExport, setNodes, setEdges]
  )

  useEffect(() => {
    window.addEventListener("keydown", handleKeyDown)
    return () => window.removeEventListener("keydown", handleKeyDown)
  }, [handleKeyDown])

  return (
    <div className="flex flex-col h-screen bg-background">
      {/* Header */}
      <div className="border-b border-border bg-card p-4">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-4">
            <h1 className="text-2xl font-bold">{workflowName}</h1>
            <Button
              size="sm"
              variant="outline"
              onClick={() => {
                setNewWorkflowName(workflowName)
                setIsNameDialogOpen(true)
              }}
            >
              Rename
            </Button>
          </div>
          <div className="flex items-center gap-2">
            {validation.hasIssues && (
              <div className="flex items-center gap-1 px-3 py-1 bg-yellow-50 border border-yellow-200 rounded-lg">
                <AlertCircle className="h-4 w-4 text-yellow-600" />
                <span className="text-sm text-yellow-700">
                  {validation.errorCount + validation.warningCount} issues
                </span>
              </div>
            )}
            {validation.isValid && (
              <div className="text-sm text-green-700 px-3 py-1 bg-green-50 border border-green-200 rounded-lg">
                ✓ Valid
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Toolbar */}
      <EditorToolbar
        canUndo={history.canUndo}
        canRedo={history.canRedo}
        hasSelection={selection.hasSelection}
        isDirty={true}
        onUndo={() => {
          history.undo()
          setNodes(history.state?.nodes || [])
          setEdges(history.state?.edges || [])
        }}
        onRedo={() => {
          history.redo()
          setNodes(history.state?.nodes || [])
          setEdges(history.state?.edges || [])
        }}
        onExport={handleExport}
        onImport={handleImport}
        onDelete={handleDelete}
        onCopy={handleCopy}
        onPaste={handlePaste}
        onZoomIn={() => {}}
        onZoomOut={() => {}}
        onFitView={() => {}}
      />

      {/* Main Content */}
      <div className="flex flex-1 overflow-hidden gap-4 p-4">
        {/* Left Sidebar - Elements Panel */}
        <div className="w-64 flex flex-col border border-border rounded-lg bg-card overflow-hidden">
          <div className="p-4 border-b border-border">
            <h2 className="font-semibold text-sm mb-4">Workflow Elements</h2>
            <div className="space-y-2">
              <Button
                onClick={() => addNode("start", "Start")}
                variant="outline"
                size="sm"
                className="w-full justify-start"
              >
                <Plus className="mr-2 h-4 w-4" />
                Start Event
              </Button>
              <Button
                onClick={() => addNode("task", "New Task")}
                variant="outline"
                size="sm"
                className="w-full justify-start"
              >
                <Plus className="mr-2 h-4 w-4" />
                Task
              </Button>
              <Button
                onClick={() => addNode("condition", "Decision")}
                variant="outline"
                size="sm"
                className="w-full justify-start"
              >
                <Plus className="mr-2 h-4 w-4" />
                Decision
              </Button>
              <Button
                onClick={() => addNode("split", "Split")}
                variant="outline"
                size="sm"
                className="w-full justify-start"
              >
                <Plus className="mr-2 h-4 w-4" />
                Parallel Split
              </Button>
              <Button
                onClick={() => addNode("join", "Join")}
                variant="outline"
                size="sm"
                className="w-full justify-start"
              >
                <Plus className="mr-2 h-4 w-4" />
                Parallel Join
              </Button>
              <Button
                onClick={() => addNode("end", "End")}
                variant="outline"
                size="sm"
                className="w-full justify-start"
              >
                <Plus className="mr-2 h-4 w-4" />
                End Event
              </Button>
            </div>
          </div>

          {/* Selection Info */}
          {selection.hasSelection && (
            <div className="p-4 border-b border-border">
              <h3 className="text-xs font-semibold text-muted-foreground mb-2">
                SELECTION ({selection.selectionCount})
              </h3>
              <Button
                onClick={handleDelete}
                variant="destructive"
                size="sm"
                className="w-full"
              >
                Delete Selected
              </Button>
            </div>
          )}

          {/* Info Footer */}
          <div className="mt-auto p-4 border-t border-border text-xs text-muted-foreground">
            <div className="space-y-1">
              <p>Nodes: {nodes.length}</p>
              <p>Edges: {edges.length}</p>
              <p>Selected: {selection.selectionCount}</p>
            </div>
          </div>
        </div>

        {/* Canvas */}
        <div className="flex-1 border border-border rounded-lg bg-white overflow-hidden">
          <ReactFlow
            nodes={nodes}
            edges={edges}
            onNodesChange={onNodesChange}
            onEdgesChange={onEdgesChange}
            onConnect={onConnect}
            onNodeDoubleClick={(_, node) => handleNodeDoubleClick(node)}
            onNodeClick={(_, node) => {
              selection.selectNode(node.id, false)
            }}
            nodeTypes={nodeTypes}
            fitView
          >
            <Background />
            <Controls />
            <MiniMap />
            <Panel position="top-right" className="m-4">
              <div className="flex gap-2">
                <Button
                  onClick={() => setIsNameDialogOpen(true)}
                  size="sm"
                  variant="outline"
                >
                  Properties
                </Button>
              </div>
            </Panel>
          </ReactFlow>
        </div>

        {/* Right Sidebar - Stats & Validation */}
        <div className="w-80 flex flex-col border border-border rounded-lg bg-card overflow-hidden">
          <Tabs defaultValue="validation" className="flex flex-col h-full">
            <TabsList className="rounded-none border-b border-border w-full justify-start p-1">
              <TabsTrigger value="validation" className="flex items-center gap-1">
                <AlertCircle className="h-4 w-4" />
                Validation
              </TabsTrigger>
              <TabsTrigger value="statistics" className="flex items-center gap-1">
                <BarChart3 className="h-4 w-4" />
                Statistics
              </TabsTrigger>
            </TabsList>

            <div className="flex-1 overflow-y-auto p-4">
              <TabsContent value="validation" className="m-0">
                <ValidationReport
                  errors={validation.errors}
                  onErrorClick={(error) => {
                    if (error.nodeId) {
                      const node = nodes.find((n) => n.id === error.nodeId)
                      if (node) {
                        handleNodeDoubleClick(node)
                      }
                    }
                  }}
                />
              </TabsContent>

              <TabsContent value="statistics" className="m-0">
                <WorkflowStatistics nodes={nodes} edges={edges} />
              </TabsContent>
            </div>
          </Tabs>
        </div>
      </div>

      {/* Node Property Editor Dialog */}
      <NodePropertyEditor
        node={selectedNode}
        isOpen={isPropertyEditorOpen}
        onClose={() => {
          setIsPropertyEditorOpen(false)
          selection.clearSelection()
        }}
        onSave={handleSaveProperties}
      />

      {/* Workflow Name Dialog */}
      <Dialog open={isNameDialogOpen} onOpenChange={setIsNameDialogOpen}>
        <DialogContent>
          <DialogHeader>
            <DialogTitle>Rename Workflow</DialogTitle>
          </DialogHeader>
          <div className="space-y-4">
            <div className="space-y-2">
              <Label htmlFor="workflow-name">Workflow Name</Label>
              <Input
                id="workflow-name"
                value={newWorkflowName}
                onChange={(e) => setNewWorkflowName(e.target.value)}
                placeholder="Enter workflow name"
              />
            </div>
          </div>
          <DialogFooter>
            <Button variant="outline" onClick={() => setIsNameDialogOpen(false)}>
              Cancel
            </Button>
            <Button
              onClick={() => {
                saveWorkflowName(newWorkflowName)
                setIsNameDialogOpen(false)
              }}
            >
              Save
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </div>
  )
}
