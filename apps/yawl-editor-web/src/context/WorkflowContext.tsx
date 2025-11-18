import React, { createContext, useContext, useReducer, ReactNode } from "react"
import { Node, Edge } from "reactflow"

export interface WorkflowContextType {
  nodes: Node[]
  edges: Edge[]
  selectedNodes: string[]
  workflowName: string
  isDirty: boolean
  dispatch: React.Dispatch<WorkflowAction>
}

export type WorkflowAction =
  | { type: "SET_NODES"; payload: Node[] }
  | { type: "SET_EDGES"; payload: Edge[] }
  | { type: "ADD_NODE"; payload: Node }
  | { type: "REMOVE_NODE"; payload: string }
  | { type: "UPDATE_NODE"; payload: { id: string; data: Partial<Node> } }
  | { type: "ADD_EDGE"; payload: Edge }
  | { type: "REMOVE_EDGE"; payload: string }
  | { type: "SET_SELECTED_NODES"; payload: string[] }
  | { type: "SET_WORKFLOW_NAME"; payload: string }
  | { type: "CLEAR_ALL" }
  | { type: "MARK_CLEAN" }
  | { type: "MARK_DIRTY" }

const WorkflowContext = createContext<WorkflowContextType | undefined>(undefined)

interface WorkflowState {
  nodes: Node[]
  edges: Edge[]
  selectedNodes: string[]
  workflowName: string
  isDirty: boolean
}

const initialState: WorkflowState = {
  nodes: [],
  edges: [],
  selectedNodes: [],
  workflowName: "Untitled Workflow",
  isDirty: false,
}

function workflowReducer(
  state: WorkflowState,
  action: WorkflowAction
): WorkflowState {
  switch (action.type) {
    case "SET_NODES":
      return { ...state, nodes: action.payload, isDirty: true }
    case "SET_EDGES":
      return { ...state, edges: action.payload, isDirty: true }
    case "ADD_NODE":
      return {
        ...state,
        nodes: [...state.nodes, action.payload],
        isDirty: true,
      }
    case "REMOVE_NODE":
      return {
        ...state,
        nodes: state.nodes.filter((n) => n.id !== action.payload),
        edges: state.edges.filter(
          (e) => e.source !== action.payload && e.target !== action.payload
        ),
        isDirty: true,
      }
    case "UPDATE_NODE":
      return {
        ...state,
        nodes: state.nodes.map((n) =>
          n.id === action.payload.id ? { ...n, ...action.payload.data } : n
        ),
        isDirty: true,
      }
    case "ADD_EDGE":
      return {
        ...state,
        edges: [...state.edges, action.payload],
        isDirty: true,
      }
    case "REMOVE_EDGE":
      return {
        ...state,
        edges: state.edges.filter((e) => e.id !== action.payload),
        isDirty: true,
      }
    case "SET_SELECTED_NODES":
      return { ...state, selectedNodes: action.payload }
    case "SET_WORKFLOW_NAME":
      return { ...state, workflowName: action.payload, isDirty: true }
    case "CLEAR_ALL":
      return {
        ...initialState,
        workflowName: state.workflowName,
        isDirty: true,
      }
    case "MARK_CLEAN":
      return { ...state, isDirty: false }
    case "MARK_DIRTY":
      return { ...state, isDirty: true }
    default:
      return state
  }
}

export function WorkflowProvider({ children }: { children: ReactNode }) {
  const [state, dispatch] = useReducer(workflowReducer, initialState)

  return (
    <WorkflowContext.Provider
      value={{
        nodes: state.nodes,
        edges: state.edges,
        selectedNodes: state.selectedNodes,
        workflowName: state.workflowName,
        isDirty: state.isDirty,
        dispatch,
      }}
    >
      {children}
    </WorkflowContext.Provider>
  )
}

export function useWorkflowContext(): WorkflowContextType {
  const context = useContext(WorkflowContext)
  if (!context) {
    throw new Error("useWorkflowContext must be used within WorkflowProvider")
  }
  return context
}
