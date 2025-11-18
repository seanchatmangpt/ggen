# YAWL Editor Web - Advanced Edition

A hyper-advanced, modern, web-based visual workflow/process editor built with Next.js, React Flow, and shadcn/ui. This is a contemporary alternative to the Java Swing-based YAWL Editor with enterprise-grade features.

## 🚀 Key Features

### Core Workflow Editing
- **Visual Workflow Editing**: Drag-and-drop interface for creating and editing YAWL workflow diagrams
- **Process Elements**: Full support for workflow elements:
  - Start/End events (circular nodes)
  - Tasks (rectangular nodes with editable labels)
  - Decision gates (diamond-shaped conditions)
  - Parallel splits and joins (cyan/purple boxes)
  - Connections/flows between elements
- **Professional Rendering**: Uses React Flow for production-grade diagram visualization
- **Node Properties**: Double-click any node to edit properties, descriptions, and custom key-value pairs

### Advanced Features

#### 🎯 Workflow Validation
- **Automatic Validation**: Validates workflows with comprehensive rule checking
- **Cycle Detection**: Detects circular dependencies using DFS algorithm
- **Error Reporting**: Color-coded errors, warnings, and info messages
- **Validation Rules**:
  - At least one Start event required
  - At least one End event required
  - No disconnected nodes (except Start/End)
  - No circular dependencies
  - No invalid edge references

#### 📊 Workflow Statistics
- **Real-time Metrics**: Displays workflow statistics on-demand
- **Metrics Tracked**:
  - Total node count
  - Connection count
  - Maximum workflow depth
  - Average connectivity
  - Node type distribution
- **Visual Indicators**: Color-coded node type badges

#### 🔄 History Management
- **Undo/Redo**: Full undo/redo history with 50-entry limit
- **State Tracking**: Automatic state persistence on every change
- **Keyboard Shortcuts**: Ctrl+Z (undo), Ctrl+Y (redo)

#### 🎨 Advanced UI Components
- **Tabs**: Organized property editor with basic/advanced tabs
- **Dialogs**: Modal dialogs for workflow naming and property editing
- **Popover**: Context-aware floating panels
- **Badge**: Status and type indicators
- **Input/Label/Textarea**: Full form components with accessibility

#### ⌨️ Keyboard Shortcuts
| Shortcut | Action |
|----------|--------|
| **Ctrl+Z** | Undo |
| **Ctrl+Y** | Redo |
| **Ctrl+C** | Copy selected nodes |
| **Ctrl+V** | Paste nodes |
| **Ctrl+A** | Select all nodes |
| **Ctrl+S** | Export workflow |
| **Delete** | Delete selected nodes |

#### 💾 File Operations
- **Export**: Save workflows as JSON with metadata
- **Import**: Load previously saved workflows
- **Auto-save**: Automatic persistence using localStorage
- **State Recovery**: Resume work from previous sessions

#### 🖱️ Advanced Selection
- **Multi-select**: Ctrl+click to select multiple nodes
- **Batch Operations**: Delete, copy, and manipulate groups of nodes
- **Focus Tracking**: Track focused node for keyboard navigation
- **Selection Count**: Real-time selection statistics

#### 📋 Clipboard Operations
- **Copy/Paste**: Full copy-paste support for node groups
- **Edge Preservation**: Maintains connections within copied nodes
- **Position Offset**: Auto-offsets pasted nodes for visibility

## 🛠️ Tech Stack

| Technology | Purpose |
|-----------|---------|
| **Next.js 14** | React framework with TypeScript |
| **React 18** | UI library |
| **React Flow 11** | Diagram/graph visualization |
| **shadcn/ui** | Accessible UI components |
| **Radix UI** | Headless component library |
| **Tailwind CSS** | Utility-first styling |
| **TypeScript 5** | Type safety |
| **Lucide React** | Icon library |

## 📦 Advanced Hooks

### useWorkflow
Comprehensive workflow state management for nodes and edges.

```typescript
const {
  nodes,
  edges,
  addNode,
  removeNode,
  updateNode,
  getConnectedNodes,
  getNodesByType,
} = useWorkflow(initialNodes, initialEdges)
```

### useHistory
Undo/redo functionality with configurable history limit.

```typescript
const { state, push, undo, redo, canUndo, canRedo } = useHistory(initialState, 50)
```

### useNodeSelection
Advanced node selection with multi-select and focus management.

```typescript
const {
  selectedNodeIds,
  selectNode,
  deselectNode,
  selectAll,
  clearSelection,
  hasSelection,
  selectionCount,
} = useNodeSelection()
```

### useWorkflowValidation
Workflow validation with comprehensive error detection.

```typescript
const { errors, isValid, errorCount, warningCount } = useWorkflowValidation(nodes, edges)
```

### useDebounce
Performance optimization hook for rapid state changes.

```typescript
const debouncedValue = useDebounce(value, 500)
```

### usePersist
LocalStorage persistence for auto-save functionality.

```typescript
const [workflow, save, clear] = usePersist("key", initialValue)
```

## 📁 Project Structure

```
src/
├── app/
│   ├── layout.tsx              # Root layout
│   ├── page.tsx                # Main page (uses AdvancedWorkflowEditor)
│   └── globals.css             # Tailwind + React Flow styles
├── components/
│   ├── ui/                     # shadcn/ui base components
│   │   ├── button.tsx
│   │   ├── dialog.tsx
│   │   ├── input.tsx
│   │   ├── label.tsx
│   │   ├── tabs.tsx
│   │   ├── popover.tsx
│   │   ├── badge.tsx
│   │   └── textarea.tsx
│   ├── nodes/                  # React Flow node types
│   │   ├── task-node.tsx
│   │   ├── start-node.tsx
│   │   ├── end-node.tsx
│   │   ├── condition-node.tsx
│   │   ├── split-node.tsx
│   │   └── join-node.tsx
│   ├── advanced-workflow-editor.tsx    # Main editor component
│   ├── node-property-editor.tsx        # Node property dialog
│   ├── validation-report.tsx           # Validation display
│   ├── workflow-statistics.tsx         # Statistics panel
│   ├── editor-toolbar.tsx              # Main toolbar
│   └── workflow-editor.tsx             # Basic editor (legacy)
├── context/
│   └── WorkflowContext.tsx     # Global state context
├── hooks/
│   ├── useWorkflow.ts
│   ├── useHistory.ts
│   ├── useNodeSelection.ts
│   ├── useWorkflowValidation.ts
│   ├── useDebounce.ts
│   ├── usePersist.ts
│   └── index.ts
├── lib/
│   └── utils.ts                # Utility functions
└── types/
    └── workflow.ts             # TypeScript definitions
```

## 🚀 Getting Started

### Prerequisites
- Node.js 18+
- npm or yarn

### Installation

```bash
cd apps/yawl-editor-web
npm install
```

### Development

```bash
npm run dev
```

Open [http://localhost:3000](http://localhost:3000) in your browser.

### Production Build

```bash
npm run build
npm run start
```

## 💡 Usage Guide

### Creating a Workflow

1. **Add Start Event**: Click "Start Event" in the left sidebar
2. **Add Tasks**: Click "Task" and add activity nodes
3. **Add Decisions**: Use "Decision" for branching logic
4. **Connect Elements**: Drag from node handles to create flows
5. **Edit Properties**: Double-click nodes to edit labels and properties

### Advanced Operations

#### Node Properties
- **Double-click** any node to open the property editor
- **Basic Tab**: Edit label and description
- **Advanced Tab**: Manage custom key-value properties

#### Validation & Statistics
- **Validation Tab**: View workflow validation results
- **Statistics Tab**: See workflow metrics and analysis
- Click on errors to navigate to problematic nodes

#### Keyboard Navigation
- **Ctrl+A**: Select all nodes
- **Ctrl+C**: Copy selected nodes
- **Ctrl+V**: Paste nodes
- **Delete**: Remove selected nodes
- **Ctrl+Z/Y**: Undo/Redo

#### File Management
- **Export**: Ctrl+S or toolbar button to export as JSON
- **Import**: Upload a JSON workflow file
- **Auto-save**: Workflows are automatically saved to localStorage

## 📚 Advanced Documentation

See [ADVANCED_FEATURES.md](./ADVANCED_FEATURES.md) for comprehensive documentation on:
- Advanced component usage
- Hook API reference
- Context management patterns
- Complete code examples
- Best practices and performance tips

## 🎨 Theming

The editor supports light and dark modes using Tailwind CSS dark mode. CSS variables are fully customizable in `src/app/globals.css`.

## ♿ Accessibility

All components follow WCAG guidelines with:
- Proper ARIA attributes
- Keyboard navigation support
- Focus management
- Color contrast compliance
- Screen reader compatibility

## 🧪 Testing

```bash
npm run lint    # Run ESLint
npm run build   # Build for production (includes type checking)
```

## 📊 Performance

- **Bundle Size**: ~181 kB First Load JS (excellent)
- **Optimizations**:
  - useCallback for all event handlers
  - useMemo for computed values
  - Efficient history with 50-entry limit
  - Debounced search/filter operations
  - React Flow rendering optimization

## 🔄 Workflow Export Format

Workflows are exported as JSON:

```json
{
  "name": "My Workflow",
  "nodes": [
    {
      "id": "1",
      "type": "start",
      "data": {
        "label": "Start",
        "description": "",
        "properties": {}
      },
      "position": { "x": 250, "y": 0 }
    }
  ],
  "edges": [
    {
      "id": "e1-2",
      "source": "1",
      "target": "2"
    }
  ]
}
```

## 🚀 Feature Roadmap

- [ ] AI-powered workflow suggestions
- [ ] Real-time collaboration
- [ ] Advanced layout algorithms
- [ ] Workflow simulation
- [ ] Process mining integration
- [ ] Custom plugins system
- [ ] Team workspaces

## 📄 License

See LICENSE file for details.

## 🤝 Contributing

Contributions welcome! Please see the main ggen repository for guidelines.

---

**Built with ❤️ using Next.js, React Flow, and shadcn/ui**
