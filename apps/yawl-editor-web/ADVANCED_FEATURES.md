# Advanced Features Documentation

This document outlines the advanced components, hooks, and features added to the YAWL Editor Web application.

## Table of Contents

1. [Advanced UI Components](#advanced-ui-components)
2. [Custom Hooks](#custom-hooks)
3. [Context Management](#context-management)
4. [Advanced Features](#advanced-features)
5. [Usage Examples](#usage-examples)

## Advanced UI Components

### Dialog Component

A versatile modal dialog component built on Radix UI.

```tsx
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogDescription,
  DialogFooter,
} from "@/components/ui/dialog"

<Dialog open={isOpen} onOpenChange={setIsOpen}>
  <DialogContent>
    <DialogHeader>
      <DialogTitle>Delete Node</DialogTitle>
      <DialogDescription>
        This action cannot be undone.
      </DialogDescription>
    </DialogHeader>
    <DialogFooter>
      <Button onClick={handleDelete}>Delete</Button>
    </DialogFooter>
  </DialogContent>
</Dialog>
```

### Input & Label Components

Form input and label components with full accessibility support.

```tsx
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"

<div>
  <Label htmlFor="name">Node Name</Label>
  <Input id="name" placeholder="Enter node name" />
</div>
```

### Textarea Component

Multi-line text input component.

```tsx
import { Textarea } from "@/components/ui/textarea"

<Textarea placeholder="Enter description..." rows={4} />
```

### Tabs Component

Tabbed interface for organizing content.

```tsx
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"

<Tabs defaultValue="basic">
  <TabsList>
    <TabsTrigger value="basic">Basic</TabsTrigger>
    <TabsTrigger value="advanced">Advanced</TabsTrigger>
  </TabsList>
  <TabsContent value="basic">Basic content</TabsContent>
  <TabsContent value="advanced">Advanced content</TabsContent>
</Tabs>
```

### Popover Component

Floating popover for contextual content.

```tsx
import { Popover, PopoverTrigger, PopoverContent } from "@/components/ui/popover"

<Popover>
  <PopoverTrigger asChild>
    <Button>Click me</Button>
  </PopoverTrigger>
  <PopoverContent>Popover content here</PopoverContent>
</Popover>
```

### Badge Component

Visual badge for status or categorization.

```tsx
import { Badge } from "@/components/ui/badge"

<Badge variant="default">Task</Badge>
<Badge variant="outline">Pending</Badge>
<Badge variant="secondary">Processing</Badge>
```

## Custom Hooks

### useWorkflow

Comprehensive workflow management hook for nodes and edges manipulation.

```tsx
import { useWorkflow } from "@/hooks"

const {
  nodes,
  edges,
  addNode,
  removeNode,
  updateNode,
  addEdge,
  removeEdge,
  clearAll,
  getNodeById,
  getConnectedNodes,
  getNodesByType,
} = useWorkflow(initialNodes, initialEdges)

// Add a new node
const newNode = {
  id: "3",
  type: "task",
  data: { label: "New Task" },
  position: { x: 250, y: 200 },
}
addNode(newNode)

// Update node
updateNode("1", { data: { label: "Updated Label" } })

// Remove node
removeNode("2")

// Get connected nodes
const connectedNodes = getConnectedNodes("1")

// Get nodes by type
const allTasks = getNodesByType("task")
```

### useHistory

Undo/redo functionality with state history management.

```tsx
import { useHistory } from "@/hooks"

const {
  state,
  push,
  undo,
  redo,
  canUndo,
  canRedo,
  reset,
  history,
  currentIndex,
} = useHistory({ nodes, edges }, 50)

// Push new state
push({ nodes: updatedNodes, edges: updatedEdges })

// Undo/Redo
if (canUndo) undo()
if (canRedo) redo()

// Reset to initial state
reset({ nodes: [], edges: [] })
```

### useNodeSelection

Advanced node selection management with multi-select support.

```tsx
import { useNodeSelection } from "@/hooks"

const {
  selectedNodeIds,
  focusedNodeId,
  selectNode,
  deselectNode,
  selectAll,
  clearSelection,
  isSelected,
  getSelectedNodes,
  hasSelection,
  selectionCount,
} = useNodeSelection()

// Single selection
selectNode("node-1")

// Multi-select
selectNode("node-2", true)

// Check if selected
if (isSelected("node-1")) {
  console.log("Node is selected")
}

// Get selected nodes
const selected = getSelectedNodes(allNodes)
console.log(`${selectionCount} nodes selected`)
```

### useWorkflowValidation

Comprehensive workflow validation with error detection and analysis.

```tsx
import { useWorkflowValidation } from "@/hooks"

const {
  errors,
  isValid,
  errorCount,
  warningCount,
  infoCount,
  getNodeErrors,
  getEdgeErrors,
  hasIssues,
} = useWorkflowValidation(nodes, edges)

// Check validity
if (!isValid) {
  console.error("Workflow has errors")
}

// Get specific node errors
const nodeErrors = getNodeErrors("node-1")

// Display error summary
console.log(`${errorCount} errors, ${warningCount} warnings`)
```

**Validation Rules:**

- At least one Start event required
- At least one End event required
- No disconnected nodes (except Start/End)
- No circular dependencies
- No invalid edge references

### useDebounce

Debounce hook for performance optimization.

```tsx
import { useDebounce } from "@/hooks"

const [searchTerm, setSearchTerm] = useState("")
const debouncedSearchTerm = useDebounce(searchTerm, 500)

// Use debouncedSearchTerm to trigger searches
useEffect(() => {
  // This effect runs 500ms after searchTerm stops changing
  performSearch(debouncedSearchTerm)
}, [debouncedSearchTerm])
```

### usePersist

Local storage persistence hook for state management.

```tsx
import { usePersist } from "@/hooks"

const [workflow, saveWorkflow, clearWorkflow] = usePersist(
  "my-workflow",
  initialWorkflow
)

// Save workflow
saveWorkflow({ nodes, edges })

// Clear stored data
clearWorkflow()
```

## Context Management

### WorkflowContext

Global workflow state management using React Context and Reducer pattern.

```tsx
import { WorkflowProvider, useWorkflowContext } from "@/context/WorkflowContext"

// Wrap your app with the provider
<WorkflowProvider>
  <App />
</WorkflowProvider>

// Use in components
function MyComponent() {
  const {
    nodes,
    edges,
    selectedNodes,
    workflowName,
    isDirty,
    dispatch,
  } = useWorkflowContext()

  // Dispatch actions
  const handleAddNode = (node) => {
    dispatch({ type: "ADD_NODE", payload: node })
  }

  const handleSetName = (name) => {
    dispatch({ type: "SET_WORKFLOW_NAME", payload: name })
  }

  const handleSave = () => {
    dispatch({ type: "MARK_CLEAN" })
  }

  return (
    <div>
      <h1>{workflowName}</h1>
      {isDirty && <span>Unsaved changes</span>}
      {/* ... */}
    </div>
  )
}
```

**Available Actions:**

- `SET_NODES(payload: Node[])` - Set all nodes
- `SET_EDGES(payload: Edge[])` - Set all edges
- `ADD_NODE(payload: Node)` - Add a single node
- `REMOVE_NODE(payload: string)` - Remove node by ID
- `UPDATE_NODE(payload: {id, data})` - Update node properties
- `ADD_EDGE(payload: Edge)` - Add a single edge
- `REMOVE_EDGE(payload: string)` - Remove edge by ID
- `SET_SELECTED_NODES(payload: string[])` - Set selected node IDs
- `SET_WORKFLOW_NAME(payload: string)` - Set workflow name
- `CLEAR_ALL()` - Clear all nodes and edges
- `MARK_CLEAN()` - Mark as saved
- `MARK_DIRTY()` - Mark as unsaved

## Advanced Features

### Node Property Editor

Advanced dialog for editing node properties with tabs for basic and advanced settings.

```tsx
import { NodePropertyEditor } from "@/components/node-property-editor"

const [selectedNode, setSelectedNode] = useState<Node | null>(null)
const [isEditorOpen, setIsEditorOpen] = useState(false)

const handleNodeDoubleClick = (node: Node) => {
  setSelectedNode(node)
  setIsEditorOpen(true)
}

const handleSaveProperties = (nodeId: string, data: any) => {
  updateNode(nodeId, { data })
}

<NodePropertyEditor
  node={selectedNode}
  isOpen={isEditorOpen}
  onClose={() => setIsEditorOpen(false)}
  onSave={handleSaveProperties}
/>
```

**Features:**

- Basic tab: Edit label and description
- Advanced tab: Manage custom properties
- Type-safe property storage
- Visual property management interface

### Validation Report

Component for displaying workflow validation issues.

```tsx
import { ValidationReport } from "@/components/validation-report"
import { useWorkflowValidation } from "@/hooks"

const { errors } = useWorkflowValidation(nodes, edges)

const handleErrorClick = (error) => {
  if (error.nodeId) {
    selectNode(error.nodeId)
  }
}

<ValidationReport
  errors={errors}
  onErrorClick={handleErrorClick}
/>
```

**Features:**

- Color-coded error types (error, warning, info)
- Clickable errors for navigation
- Summary statistics
- Success state when valid

### Workflow Statistics

Component displaying workflow metrics and analysis.

```tsx
import { WorkflowStatistics } from "@/components/workflow-statistics"

<WorkflowStatistics nodes={nodes} edges={edges} />
```

**Metrics:**

- Total nodes count
- Total connections count
- Maximum workflow depth
- Average connectivity
- Node type distribution
- Visual node type indicators

### Editor Toolbar

Advanced toolbar with undo/redo, zoom, and file operations.

```tsx
import { EditorToolbar } from "@/components/editor-toolbar"

<EditorToolbar
  canUndo={history.canUndo}
  canRedo={history.canRedo}
  hasSelection={selectedNodeIds.length > 0}
  isDirty={isDirty}
  onUndo={() => history.undo()}
  onRedo={() => history.redo()}
  onExport={handleExport}
  onImport={handleImport}
  onDelete={handleDelete}
  onCopy={handleCopy}
  onPaste={handlePaste}
  onZoomIn={handleZoomIn}
  onZoomOut={handleZoomOut}
  onFitView={handleFitView}
/>
```

**Features:**

- Undo/Redo controls
- Zoom controls (in, out, fit)
- Clipboard operations (copy, paste)
- File operations (export, import)
- Unsaved changes indicator
- Disabled state management

## Usage Examples

### Complete Advanced Editor Setup

```tsx
"use client"

import { useState } from "react"
import { useWorkflow, useHistory, useNodeSelection, useWorkflowValidation } from "@/hooks"
import { NodePropertyEditor } from "@/components/node-property-editor"
import { ValidationReport } from "@/components/validation-report"
import { WorkflowStatistics } from "@/components/workflow-statistics"
import { EditorToolbar } from "@/components/editor-toolbar"

export function AdvancedWorkflowEditor() {
  const workflow = useWorkflow([], [])
  const history = useHistory({ nodes: workflow.nodes, edges: workflow.edges })
  const selection = useNodeSelection()
  const validation = useWorkflowValidation(workflow.nodes, workflow.edges)

  const [selectedNode, setSelectedNode] = useState(null)
  const [isPropertyEditorOpen, setIsPropertyEditorOpen] = useState(false)

  const handleNodeDoubleClick = (node) => {
    setSelectedNode(node)
    setIsPropertyEditorOpen(true)
  }

  const handleSaveProperties = (nodeId, data) => {
    workflow.updateNode(nodeId, { data })
    history.push({ nodes: workflow.nodes, edges: workflow.edges })
  }

  const handleDelete = () => {
    selection.deleteSelected().forEach((nodeId) => {
      workflow.removeNode(nodeId)
    })
    history.push({ nodes: workflow.nodes, edges: workflow.edges })
    selection.clearSelection()
  }

  return (
    <div className="flex flex-col h-screen">
      <EditorToolbar
        canUndo={history.canUndo}
        canRedo={history.canRedo}
        hasSelection={selection.hasSelection}
        isDirty={true}
        onUndo={() => history.undo()}
        onRedo={() => history.redo()}
        onExport={() => console.log("Export")}
        onImport={() => console.log("Import")}
        onDelete={handleDelete}
        onCopy={() => console.log("Copy")}
        onPaste={() => console.log("Paste")}
        onZoomIn={() => console.log("Zoom In")}
        onZoomOut={() => console.log("Zoom Out")}
        onFitView={() => console.log("Fit View")}
      />

      <div className="flex flex-1 gap-4 p-4 overflow-hidden">
        {/* Editor Canvas */}
        <div className="flex-1 bg-white rounded-lg border">
          {/* React Flow Editor */}
        </div>

        {/* Right Sidebar */}
        <div className="w-80 space-y-4 overflow-y-auto">
          <div className="border rounded-lg p-4">
            <h2 className="font-semibold mb-3">Validation</h2>
            <ValidationReport errors={validation.errors} />
          </div>

          <div className="border rounded-lg p-4">
            <h2 className="font-semibold mb-3">Statistics</h2>
            <WorkflowStatistics nodes={workflow.nodes} edges={workflow.edges} />
          </div>
        </div>
      </div>

      <NodePropertyEditor
        node={selectedNode}
        isOpen={isPropertyEditorOpen}
        onClose={() => setIsPropertyEditorOpen(false)}
        onSave={handleSaveProperties}
      />
    </div>
  )
}
```

### Keyboard Shortcuts Implementation

```tsx
useEffect(() => {
  const handleKeyDown = (e: KeyboardEvent) => {
    if (e.ctrlKey || e.metaKey) {
      switch (e.key.toLowerCase()) {
        case "z":
          e.preventDefault()
          if (history.canUndo) history.undo()
          break
        case "y":
          e.preventDefault()
          if (history.canRedo) history.redo()
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
          selection.selectAll(workflow.nodes.map((n) => n.id))
          break
      }
    } else if (e.key === "Delete") {
      e.preventDefault()
      handleDelete()
    }
  }

  window.addEventListener("keydown", handleKeyDown)
  return () => window.removeEventListener("keydown", handleKeyDown)
}, [history, selection, workflow])
```

## Best Practices

1. **State Management**: Use `useWorkflowContext` for global state and local hooks for component-specific state
2. **Performance**: Use `useDebounce` for search/filter operations to avoid excessive re-renders
3. **Persistence**: Use `usePersist` to automatically save user work
4. **Validation**: Always validate before saving or exporting workflows
5. **Error Handling**: Implement proper error boundaries and validation reports
6. **Accessibility**: All components follow WCAG guidelines with proper ARIA attributes

## Performance Considerations

- Hooks use `useCallback` and `useMemo` to prevent unnecessary re-renders
- Validation uses efficient DFS algorithm for cycle detection
- History is limited to 50 entries by default (configurable)
- Debounce is recommended for rapid state changes
- React Flow rendering is optimized for large workflows

## Contributing

When adding new advanced features:

1. Add the component to `src/components/`
2. Export from component index files
3. Add corresponding TypeScript types
4. Include usage examples in documentation
5. Ensure build passes: `npm run build`
