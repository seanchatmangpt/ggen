# YAWL Editor Web

A modern, web-based visual workflow/process editor built with Next.js and shadcn/ui, providing a contemporary alternative to the Java Swing-based YAWL Editor.

## Features

- **Visual Workflow Editing**: Drag-and-drop interface for creating and editing YAWL workflow diagrams
- **Process Elements**: Support for various workflow elements:
  - Start/End events
  - Tasks (atomic activities)
  - Decision gates (conditions)
  - Parallel splits and joins
  - Connections/flows between elements
- **Modern UI**: Built with shadcn/ui components and Tailwind CSS
- **File Operations**: Export and import workflows in JSON format
- **Real-time Editing**: Intuitive editing with selection, deletion, and property modification
- **Diagram Visualization**: Uses React Flow for professional diagram rendering

## Tech Stack

- **Framework**: Next.js 14 with TypeScript
- **UI Components**: shadcn/ui with Radix UI
- **Styling**: Tailwind CSS
- **Diagram Editing**: React Flow
- **Icons**: Lucide React

## Getting Started

### Prerequisites

- Node.js 18+
- npm or yarn

### Installation

```bash
cd apps/yawl-editor-web
npm install
```

### Running the Development Server

```bash
npm run dev
```

Open [http://localhost:3000](http://localhost:3000) in your browser to see the editor.

### Building for Production

```bash
npm run build
npm run start
```

## Project Structure

```
src/
├── app/
│   ├── layout.tsx          # Root layout with metadata
│   ├── page.tsx            # Main page entry point
│   └── globals.css         # Global styles and Tailwind setup
├── components/
│   ├── ui/                 # shadcn/ui components
│   │   ├── button.tsx
│   │   └── dropdown-menu.tsx
│   ├── nodes/              # React Flow node components
│   │   ├── task-node.tsx
│   │   ├── condition-node.tsx
│   │   ├── start-node.tsx
│   │   ├── end-node.tsx
│   │   ├── join-node.tsx
│   │   └── split-node.tsx
│   └── workflow-editor.tsx # Main editor component
├── lib/
│   └── utils.ts            # Utility functions (cn for class merging)
└── types/
    └── workflow.ts         # TypeScript type definitions
```

## Usage

### Adding Elements

1. Click buttons in the left sidebar to add workflow elements
2. Click on the canvas to place elements
3. Double-click element labels to edit them

### Creating Connections

1. Drag from the output handle (bottom) of a source element
2. Drop onto the input handle (top) of a target element

### Managing Workflows

- **Delete Selected**: Click "Delete Selected" to remove selected elements
- **Clear All**: Remove all elements from the canvas
- **Export**: Save your workflow as a JSON file
- **Import**: Load a previously saved workflow

## Architecture Overview

The editor is built with a component-based architecture:

- **WorkflowEditor**: Main container component managing state and interactions
- **Node Components**: Specialized React Flow nodes for different element types
- **UI Components**: Reusable shadcn/ui components for buttons, menus, etc.
- **React Flow**: Handles the graph visualization and interaction logic

## Workflow Format

Workflows are exported as JSON in the following format:

```json
{
  "nodes": [
    {
      "id": "1",
      "data": { "label": "Start" },
      "position": { "x": 250, "y": 0 },
      "type": "start"
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

## Comparison with Java YAWL Editor

| Feature | YAWL Editor (Java) | YAWL Editor Web (Next.js) |
|---------|-------------------|--------------------------|
| Platform | Desktop (Java Swing) | Web Browser |
| Tech Stack | Java, Swing | React, Next.js, TypeScript |
| UI Framework | Swing Components | shadcn/ui, Tailwind CSS |
| Diagram Engine | Custom JComponent | React Flow |
| Modern UX | Legacy | Modern, responsive |
| Deployment | Desktop app | Web application |
| Accessibility | Limited | WCAG compliant |

## Future Enhancements

- [ ] YAWL-specific validation and verification
- [ ] Advanced resourcing and data mapping UI
- [ ] Workflow simulation
- [ ] Team collaboration features
- [ ] Integration with YAWL runtime engine
- [ ] Advanced layout algorithms
- [ ] Custom themes and styling
- [ ] API integration for backend services

## Contributing

This project is part of the ggen monorepo. See the root README for contribution guidelines.

## License

See LICENSE file for details.
