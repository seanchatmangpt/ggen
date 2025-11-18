import { Node, Edge } from "reactflow"

export interface Workflow {
  id: string
  name: string
  description?: string
  nodes: Node[]
  edges: Edge[]
  createdAt: Date
  updatedAt: Date
}

export interface WorkflowElement {
  id: string
  type: "task" | "condition" | "start" | "end" | "join" | "split"
  label: string
  properties?: Record<string, any>
}

export interface WorkflowConnection {
  id: string
  source: string
  target: string
  label?: string
}
