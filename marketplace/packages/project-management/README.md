# Project Management

Comprehensive project management with Gantt charts, sprints, resource allocation, and budget tracking.

## Features

- **Project Planning**: Gantt charts and dependencies
- **Agile/Scrum**: Sprint management and backlogs
- **Kanban Boards**: Visual workflow management
- **Resource Management**: Allocation and time tracking
- **Budget Tracking**: Cost management and invoicing
- **Risk Management**: Risk register and mitigation

## Quick Start

```typescript
import { ProjectManagement } from '@ggen/project-management';

const pm = new ProjectManagement();

// Create project
const project = await pm.createProject({
  name: 'Website Redesign',
  startDate: '2025-01-15',
  endDate: '2025-06-30',
  budget: 150000
});

// Add tasks with dependencies
await pm.addTask(project.id, {
  name: 'Design mockups',
  duration: 14,
  assignedTo: 'designer@example.com'
});

// Track progress
const status = await pm.getProjectStatus(project.id);
```

## Documentation

- RDF Ontology: 300+ lines
- SPARQL Queries: 12 templates
- Chicago TDD Tests: 600+ lines

See full documentation at https://docs.ggen.ai/packages/project-management
