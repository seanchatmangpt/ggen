"use client"

import React, { useState, useCallback } from "react"
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
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
  DropdownMenuSeparator,
} from "@/components/ui/dropdown-menu"
import { TaskNode } from "@/components/nodes/task-node"
import { ConditionNode } from "@/components/nodes/condition-node"
import { StartNode } from "@/components/nodes/start-node"
import { EndNode } from "@/components/nodes/end-node"
import { JoinNode } from "@/components/nodes/join-node"
import { SplitNode } from "@/components/nodes/split-node"
import {
  Plus,
  Download,
  Upload,
  Trash2,
  Menu,
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
    data: { label: "Start" },
    position: { x: 250, y: 0 },
    type: "start",
  },
  {
    id: "2",
    data: { label: "Task 1" },
    position: { x: 250, y: 100 },
    type: "task",
  },
  {
    id: "3",
    data: { label: "End" },
    position: { x: 250, y: 200 },
    type: "end",
  },
]

const initialEdges: Edge[] = [
  { id: "e1-2", source: "1", target: "2" },
  { id: "e2-3", source: "2", target: "3" },
]

export function WorkflowEditor() {
  const [nodes, setNodes, onNodesChange] = useNodesState(initialNodes)
  const [edges, setEdges, onEdgesChange] = useEdgesState(initialEdges)
  const [nodeIdCounter, setNodeIdCounter] = useState(4)

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
        data: { label },
        position: { x: 250, y: 300 + nodeIdCounter * 50 },
        type,
      }
      setNodes((nds) => [...nds, newNode])
      setNodeIdCounter((prev) => prev + 1)
    },
    [nodeIdCounter, setNodes]
  )

  const deleteSelected = useCallback(() => {
    setNodes((nds) => nds.filter((node) => !node.selected))
    setEdges((eds) => eds.filter((edge) => !edge.selected))
  }, [setNodes, setEdges])

  const clearAll = useCallback(() => {
    setNodes([])
    setEdges([])
  }, [setNodes, setEdges])

  const exportWorkflow = useCallback(() => {
    const workflow = { nodes, edges }
    const dataStr = JSON.stringify(workflow, null, 2)
    const dataUri =
      "data:application/json;charset=utf-8," + encodeURIComponent(dataStr)
    const exportFileDefaultName = "workflow.json"
    const linkElement = document.createElement("a")
    linkElement.setAttribute("href", dataUri)
    linkElement.setAttribute("download", exportFileDefaultName)
    linkElement.click()
  }, [nodes, edges])

  const importWorkflow = useCallback(() => {
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
        } catch (error) {
          console.error("Failed to import workflow", error)
        }
      }
      reader.readAsText(file)
    }
    input.click()
  }, [setNodes, setEdges])

  return (
    <div className="flex h-screen w-screen bg-white">
      {/* Sidebar */}
      <div className="flex w-64 flex-col border-r border-border bg-card p-4">
        <h1 className="mb-6 text-xl font-bold">YAWL Editor</h1>

        <div className="mb-6">
          <h2 className="mb-3 text-sm font-semibold text-muted-foreground">
            Add Elements
          </h2>
          <div className="space-y-2">
            <Button
              onClick={() => addNode("start", "Start")}
              variant="outline"
              className="w-full justify-start"
            >
              <Plus className="mr-2 h-4 w-4" />
              Start Event
            </Button>
            <Button
              onClick={() => addNode("task", "New Task")}
              variant="outline"
              className="w-full justify-start"
            >
              <Plus className="mr-2 h-4 w-4" />
              Task
            </Button>
            <Button
              onClick={() => addNode("condition", "Decision")}
              variant="outline"
              className="w-full justify-start"
            >
              <Plus className="mr-2 h-4 w-4" />
              Decision
            </Button>
            <Button
              onClick={() => addNode("split", "Split")}
              variant="outline"
              className="w-full justify-start"
            >
              <Plus className="mr-2 h-4 w-4" />
              Parallel Split
            </Button>
            <Button
              onClick={() => addNode("join", "Join")}
              variant="outline"
              className="w-full justify-start"
            >
              <Plus className="mr-2 h-4 w-4" />
              Parallel Join
            </Button>
            <Button
              onClick={() => addNode("end", "End")}
              variant="outline"
              className="w-full justify-start"
            >
              <Plus className="mr-2 h-4 w-4" />
              End Event
            </Button>
          </div>
        </div>

        <div className="space-y-2 border-t border-border pt-4">
          <Button
            onClick={deleteSelected}
            variant="ghost"
            className="w-full justify-start text-destructive hover:bg-destructive/10"
          >
            <Trash2 className="mr-2 h-4 w-4" />
            Delete Selected
          </Button>
        </div>

        <div className="mt-auto space-y-2 border-t border-border pt-4">
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button
                variant="outline"
                className="w-full justify-start"
              >
                <Menu className="mr-2 h-4 w-4" />
                Menu
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end" className="w-56">
              <DropdownMenuItem onClick={exportWorkflow}>
                <Download className="mr-2 h-4 w-4" />
                Export Workflow
              </DropdownMenuItem>
              <DropdownMenuItem onClick={importWorkflow}>
                <Upload className="mr-2 h-4 w-4" />
                Import Workflow
              </DropdownMenuItem>
              <DropdownMenuSeparator />
              <DropdownMenuItem onClick={clearAll}>
                <Trash2 className="mr-2 h-4 w-4" />
                Clear All
              </DropdownMenuItem>
            </DropdownMenuContent>
          </DropdownMenu>
        </div>
      </div>

      {/* Canvas */}
      <div className="flex-1 bg-gray-50">
        <ReactFlow
          nodes={nodes}
          edges={edges}
          onNodesChange={onNodesChange}
          onEdgesChange={onEdgesChange}
          onConnect={onConnect}
          nodeTypes={nodeTypes}
          fitView
        >
          <Background />
          <Controls />
          <MiniMap />
          <Panel position="top-right" className="m-4">
            <div className="flex gap-2">
              <Button
                onClick={exportWorkflow}
                size="sm"
                variant="outline"
              >
                <Download className="mr-2 h-4 w-4" />
                Export
              </Button>
              <Button
                onClick={importWorkflow}
                size="sm"
                variant="outline"
              >
                <Upload className="mr-2 h-4 w-4" />
                Import
              </Button>
            </div>
          </Panel>
        </ReactFlow>
      </div>
    </div>
  )
}
