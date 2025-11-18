# BPMN.js Integration Guide

Complete guide to using BPMN.js for visual workflow design in the YAWL Editor.

## 🎨 Overview

The YAWL Editor now features a powerful visual process designer using BPMN.js, allowing users to:

- **Design workflows visually** with drag-and-drop interface
- **Convert BPMN to YAWL** RDF/OWL ontology automatically
- **Export to multiple formats** (BPMN XML, YAWL Turtle, SPARQL)
- **Analyze processes** with AI-powered insights
- **Detect risks** and get optimization suggestions
- **Validate workflows** for correctness

## 📦 Installation

Dependencies added to `package.json`:

```json
{
  "dependencies": {
    "bpmn-js": "^14.2.0",
    "diagram-js": "^14.2.0",
    "ai": "^3.1.0",
    "zod-to-json-schema": "^3.23.2"
  },
  "devDependencies": {
    "@types/bpmn-moddle": "^5.0.9"
  }
}
```

Install with:
```bash
npm install
```

## 🚀 Quick Start

### Access the Designer

Navigate to `/designer` to open the BPMN process designer:

```
http://localhost:3000/designer
```

### Create a New Process

1. The designer opens with an empty diagram
2. Drag elements from the palette:
   - **Start Event** - Process entry point
   - **End Event** - Process termination
   - **Task** - User or system task
   - **User Task** - Manual task
   - **Service Task** - Automated task
   - **Gateway** - Decision point
3. Connect elements with arrows
4. Edit properties in the right panel
5. Click **Analyze Process** for insights
6. Export to desired format

## 📁 Components

### BpmnDiagram Component

Main viewer and editor component for BPMN diagrams.

```tsx
import { BpmnDiagram } from '@/components/bpmn/bpmn-diagram'

<BpmnDiagram
  diagramXml={xmlContent}
  onDiagramChange={(xml) => console.log('Updated:', xml)}
  readOnly={false}
/>
```

**Props:**
- `diagramXml?: string` - Initial diagram XML
- `onDiagramChange?: (xml: string) => void` - Called when diagram changes
- `readOnly?: boolean` - Disable editing if true

## 🔄 Conversions

### BPMN → YAWL Ontology

Convert visual BPMN diagrams to RDF/OWL format:

```typescript
import { parseBpmnXml, bpmnToYawlOntology } from '@/lib/bpmn-to-yawl'

const process = parseBpmnXml(bpmnXml)
const yawlOntology = bpmnToYawlOntology(process)

console.log(yawlOntology)
// Outputs Turtle format RDF/OWL
```

**Output Format:**

```turtle
PREFIX ex: <http://yawlfoundation.org/ontology#>
PREFIX data: <http://yawlfoundation.org/data#>

data:process_1 a ex:Process ;
  ex:processIdentifier "process_1" ;
  ex:processName "My Process" ;
  ex:processVersion "1.0" .

data:task_review a ex:Task ;
  ex:taskName "Review Application" ;
  ex:taskType ex:ATOMIC ;
  ex:belongsToProcess data:process_1 .
```

### BPMN → SPARQL INSERT

Generate SPARQL queries to insert workflow into RDF store:

```typescript
import { bpmnToSparqlInsert } from '@/lib/bpmn-to-yawl'

const sparqlQuery = bpmnToSparqlInsert(process)

// Use with SPARQL endpoint
const response = await fetch('/api/sparql/update', {
  method: 'POST',
  body: sparqlQuery
})
```

### Export as BPMN XML

Round-trip export compatible with BPMN tools:

```typescript
import { exportAsBpmnXml } from '@/lib/bpmn-to-yawl'

const bpmnFile = exportAsBpmnXml(process)
// Save to file: process.bpmn
```

## 🎣 Custom Hooks

### useBpmn

Main hook for BPMN diagram operations:

```typescript
import { useBpmn } from '@/hooks/use-bpmn'

const {
  xml,
  loading,
  error,
  modeler,
  updateDiagram,
  save,
  parseProcess,
  exportAsYawl,
  exportAsSparql,
  exportAsFile,
  downloadFile,
} = useBpmn({
  initialXml: bpmnXml,
  onSave: async (xml) => {
    await api.saveProcess(xml)
  },
})

// Use in component
<BpmnDiagram
  diagramXml={xml}
  onDiagramChange={updateDiagram}
/>

<button onClick={save}>Save</button>
<button onClick={() => downloadFile('yawl', 'process.ttl')}>
  Export as YAWL
</button>
```

### useBpmnValidation

Validate BPMN diagram for errors and warnings:

```typescript
import { useBpmnValidation } from '@/hooks/use-bpmn'

const { isValid, errors, warnings, validate } = useBpmnValidation(bpmnXml)

validate() // Run validation

{!isValid && (
  <div>
    <p>Errors:</p>
    {errors.map(e => <p key={e}>{e}</p>)}
  </div>
)}
```

## 📊 AI Process Analysis

### Automatic Analysis

Get AI-powered insights about your workflow:

```typescript
import {
  analyzeProcess,
  createProcessAnalysisReport,
  generateOptimizations,
  detectProcessRisks,
} from '@/lib/ai-process-analyzer'

const process = parseBpmnXml(bpmnXml)
const report = createProcessAnalysisReport(process)

console.log(report.summary)
console.log(report.insights)
console.log(report.suggestions)
console.log(report.risks)
console.log(report.metrics)
```

### Analysis Report

```typescript
{
  summary: "Process contains 8 tasks organized with 2 decision points...",
  insights: [
    "Complexity Level: Medium",
    "Average branching factor: 1.5 paths per element",
    "Process depth: 6 steps"
  ],
  suggestions: [
    "Consider consolidating decision gateways",
    "Break into sub-processes for better manageability"
  ],
  risks: [
    "No end event defined - add termination point",
    "Task X has no outgoing connections"
  ],
  metrics: {
    totalTasks: 8,
    totalGateways: 2,
    startEvents: 1,
    endEvents: 0,
    branchingFactor: 1.5,
    depth: 6,
    complexity: "Medium"
  }
}
```

### Process Metrics

Understanding the metrics:

- **totalTasks**: Number of work activities
- **totalGateways**: Number of decision points
- **startEvents**: Process entry points
- **endEvents**: Process termination points
- **branchingFactor**: Average paths per element
- **depth**: Longest path from start to end
- **complexity**: Overall process complexity

## 🔍 Process Designer Features

### Design Page: `/designer`

Complete process design interface with:

1. **Visual Editor**
   - Drag-and-drop elements
   - Connect with arrows
   - Edit element properties

2. **Toolbar**
   - Analyze Process button
   - Export options (BPMN, YAWL, SPARQL)
   - Save Process button

3. **Analysis Dialog**
   - Process summary
   - Key metrics
   - AI insights
   - Improvement suggestions
   - Risk detection

4. **Export Dialog**
   - Preview exported content
   - Copy to clipboard
   - Download file

### Supported Elements

| Element | Type | Usage |
|---------|------|-------|
| Start Event | startEvent | Begin process |
| End Event | endEvent | Complete process |
| Task | task | Generic work |
| User Task | userTask | Manual work |
| Service Task | serviceTask | Automated work |
| Exclusive Gateway | exclusiveGateway | XOR decision |
| Parallel Gateway | parallelGateway | AND split/join |
| Inclusive Gateway | inclusiveGateway | OR decision |
| Sub-Process | subProcess | Grouped tasks |

## 🔗 Integration with SPARQL

### Store in RDF

Convert BPMN to SPARQL and store in triple store:

```typescript
import { bpmnToSparqlInsert } from '@/lib/bpmn-to-yawl'
import { createDefaultSparqlClient } from '@/lib/sparql-client'

const process = parseBpmnXml(bpmnXml)
const sparqlQuery = bpmnToSparqlInsert(process)

const client = createDefaultSparqlClient()
await client.update(sparqlQuery)

// Process is now in RDF store!
```

### Query Processes

Retrieve stored processes via SPARQL:

```typescript
import { SPARQL_QUERIES } from '@/lib/sparql-queries'
import { createDefaultSparqlClient } from '@/lib/sparql-client'

const client = createDefaultSparqlClient()
const result = await client.query(SPARQL_QUERIES.getAllProcesses)

// Returns all processes stored in RDF
```

## 💾 Example Workflow

### 1. Design a Process

```
1. Open /designer
2. Add Start Event
3. Add Task "Review Application"
4. Add Exclusive Gateway "Approved?"
5. Add Tasks for "Yes" and "No" paths
6. Add End Events
```

### 2. Analyze & Optimize

```
1. Click "Analyze Process"
2. Review insights and risks
3. Apply suggestions
4. Click "Export as YAWL"
```

### 3. Store in SPARQL

```
1. Select export format: SPARQL
2. Copy the SPARQL query
3. Execute against your RDF endpoint
4. Process now queryable via SPARQL
```

### 4. Query in Application

```typescript
const result = await useSparql(
  SPARQL_QUERIES.getProcessTasks('proc-review')
)

// Use results in UI
```

## 🎓 Complete Example

### Full Process Design Workflow

```tsx
'use client'

import { useState } from 'react'
import { BpmnDiagram } from '@/components/bpmn/bpmn-diagram'
import { useBpmn } from '@/hooks/use-bpmn'
import { useBpmnValidation } from '@/hooks/use-bpmn'
import { createProcessAnalysisReport, parseBpmnXml } from '@/lib/bpmn-to-yawl'

export default function WorkflowDesigner() {
  const {
    xml,
    updateDiagram,
    downloadFile,
    exportAsYawl,
  } = useBpmn()

  const { isValid, errors, validate } = useBpmnValidation(xml)

  const handleAnalyze = () => {
    validate()
    if (!isValid) return

    const process = parseBpmnXml(xml)
    const report = createProcessAnalysisReport(process)
    console.log(report)
  }

  const handleExport = () => {
    const yawl = exportAsYawl()
    if (yawl) {
      downloadFile('yawl', 'my-process.ttl')
    }
  }

  return (
    <div className="space-y-4">
      <div className="flex gap-2">
        <button onClick={handleAnalyze}>
          Analyze
        </button>
        <button onClick={handleExport}>
          Export to YAWL
        </button>
      </div>

      {!isValid && (
        <div className="bg-red-50 p-4">
          <p>Validation errors:</p>
          {errors.map(e => <p key={e}>{e}</p>)}
        </div>
      )}

      <BpmnDiagram
        diagramXml={xml}
        onDiagramChange={updateDiagram}
      />
    </div>
  )
}
```

## 🚀 Advanced Features

### Process Comparison

Compare two BPMN processes:

```typescript
import { compareProcesses } from '@/lib/ai-process-analyzer'

const diff = compareProcesses(process1, process2)

if (!diff.same) {
  console.log('Differences:')
  diff.differences.forEach(d => console.log(d))
}
```

### Custom BPMN Modeler Configuration

Extend BPMN modeler with custom modules:

```typescript
import BpmnModeler from 'bpmn-js/lib/Modeler'
import CustomRulesModule from './custom-rules'

const modeler = new BpmnModeler({
  container: containerElement,
  additionalModules: [CustomRulesModule],
  moddleExtensions: {
    yawl: yawlExtensions
  }
})
```

## 📚 Resources

- [BPMN.js Official Documentation](https://bpmn.io/)
- [BPMN 2.0 Specification](https://www.omg.org/bpmn/)
- [YAWL Language Reference](http://www.yawlfoundation.org/)
- [RDF/OWL Tutorials](https://www.w3.org/TR/owl2-overview/)

## 🔧 Troubleshooting

### Diagram Won't Load

```typescript
// Check XML validity
const process = parseBpmnXml(xml)
if (!process) {
  console.error('Invalid BPMN XML')
}
```

### Export Returns Empty

```typescript
// Ensure XML is set
if (!xml) {
  console.error('No diagram to export')
  return
}

// Validate before export
const process = parseBpmnXml(xml)
if (!process) {
  console.error('Cannot parse diagram')
}
```

### Style Issues

Import BPMN CSS:

```typescript
import 'bpmn-js/dist/assets/diagram-js.css'
import 'bpmn-js/dist/assets/bpmn-font/font/bpmn.css'
import 'bpmn-js-properties-panel/dist/assets/bpmn-js-properties-panel.css'
```

## 🎯 Best Practices

1. **Name elements clearly** - Helps in analysis and documentation
2. **Add start and end events** - Validates process flow
3. **Use appropriate task types** - Improves automation potential
4. **Review analysis insights** - Helps optimize workflow
5. **Export for backup** - Save designs in multiple formats
6. **Validate regularly** - Catch issues early
7. **Keep processes focused** - Use sub-processes for complex sections
8. **Document decisions** - Add descriptions to gateways

## 📝 Next Steps

1. Design your workflow in `/designer`
2. Run analysis to identify improvements
3. Export to YAWL format
4. Store in SPARQL endpoint
5. Query and manage processes via API
6. Monitor execution with dashboards
