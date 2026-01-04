# YAWL Workflow Platform

A comprehensive workflow automation platform built on YAWL (Yet Another Workflow Language) with RDF/SPARQL integration, servlet handlers, and worklet exception management. All code is generated from RDF ontology specifications using ggen.

## Overview

This example demonstrates ggen's power to generate a complete workflow automation platform from declarative RDF specifications. It implements Van der Aalst's workflow patterns and integrates with the [unrdf YAWL package](https://github.com/seanchatmangpt/unrdf).

### Key Features

- **YAWL XML Generation**: Generate YAWL 4.0 XML specifications from RDF
- **Van der Aalst Patterns**: Full support for WP1-WP7 workflow patterns
- **Servlet Handlers**: TypeScript HTTP handlers for workflow operations
- **Worklet Exception Handlers**: RDF-driven rule-based exception handling
- **Cryptographic Receipts**: BLAKE3-hashed audit trail for all operations
- **Time-Travel Replay**: Event-sourced case replay via KGC-4D integration

## Architecture

```
┌──────────────────────────────────────────────────────────────────────────┐
│                          RDF Specifications                               │
│  ┌─────────────────────┐  ┌──────────────────────┐                       │
│  │   yawl-vocab.ttl    │  │ platform-workflows.ttl│                      │
│  │   (Ontology Schema) │  │   (Instance Data)     │                      │
│  └─────────┬───────────┘  └──────────┬───────────┘                       │
│            │                         │                                    │
│            └────────────┬────────────┘                                    │
│                         ▼                                                 │
│  ┌──────────────────────────────────────────────────────────────────┐    │
│  │                      ggen.toml                                    │    │
│  │  14 Generation Rules with SPARQL Queries + Tera Templates         │    │
│  └──────────────────────────────────────────────────────────────────┘    │
│                         │                                                 │
│                         ▼ ggen sync                                       │
│  ┌──────────────────────────────────────────────────────────────────┐    │
│  │                   Generated Code                                  │    │
│  │  • YAWL XML Specs          • REST API Routes                      │    │
│  │  • Servlet Handlers        • TypeScript Types                     │    │
│  │  • Worklet Handlers        • Vitest Integration Tests             │    │
│  │  • Task Definitions        • Flow Definitions                     │    │
│  │  • Workflow Engine         • Receipt Types                        │    │
│  └──────────────────────────────────────────────────────────────────┘    │
└──────────────────────────────────────────────────────────────────────────┘
```

## Included Workflows

### 1. Document Approval (XOR-Split)
Van der Aalst Pattern: **WP4 (Exclusive Choice) + WP5 (Simple Merge)**
- Submit → Review → (Approve XOR Reject) → Notify

### 2. Order Processing (AND-Split)
Van der Aalst Pattern: **WP2 (Parallel Split) + WP3 (Synchronization)**
- Receive → (Verify Payment AND Check Inventory) → Fulfill → Ship

### 3. HR Onboarding (Multiple Instance)
Van der Aalst Pattern: **WP12-15 (Multiple Instance Patterns)**
- Create Record → Assign Training → Complete Modules (MI) → Setup → Finalize

### 4. Incident Management (Worklet Exception Handling)
- Log → Classify → Assign → Resolve (with worklet handlers) → Verify → Close

### 5. CI/CD Pipeline (OR-Split)
Van der Aalst Pattern: **WP6 (Multi-Choice) + WP7 (Structured Synchronizing Merge)**
- Checkout → Build → (Unit OR Integration OR E2E Tests) → Merge Results → Deploy

## Project Structure

```
yawl-workflow-platform/
├── ontology/
│   ├── yawl-vocab.ttl           # YAWL vocabulary schema (85+ properties)
│   └── platform-workflows.ttl    # 5 workflow definitions + servlets + worklets
├── templates/
│   ├── yawl-spec.tera           # YAWL XML generation
│   ├── servlet-handler.tera     # HTTP servlet handlers
│   ├── worklet-handler.tera     # Exception handlers with SPARQL rules
│   ├── workflow-engine.tera     # Main engine with pattern implementations
│   ├── task-definitions.tera    # Task definitions with Zod schemas
│   ├── flow-definitions.tera    # Flow definitions with conditions
│   ├── rest-api.tera            # Express routes
│   ├── vitest-tests.tera        # Integration tests with MSW
│   └── ...                      # Additional templates
├── ggen.toml                    # 14 generation rules
├── package.json
└── README.md
```

## Quick Start

### Prerequisites

- [ggen](https://github.com/seanchatmangpt/ggen) CLI installed
- Node.js 18+
- npm or pnpm

### Generate Code

```bash
# From the example directory
cd examples/yawl-workflow-platform

# Generate all code from RDF specifications
ggen sync

# Or preview without writing
ggen sync --dry-run
```

### Install Dependencies

```bash
npm install
```

### Run Tests

```bash
npm test
```

### Start Development Server

```bash
npm run dev
# Server runs at http://localhost:3000
```

## API Endpoints

Generated from RDF servlet definitions:

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/api/workflows` | List all workflow specifications |
| `POST` | `/api/workflows` | Create a new workflow |
| `GET` | `/api/workflows/:id` | Get workflow by ID |
| `GET` | `/api/cases` | List all cases |
| `POST` | `/api/cases` | Create a new case |
| `GET` | `/api/cases/:id` | Get case by ID |
| `POST` | `/api/cases/:id/cancel` | Cancel a case |
| `GET` | `/api/workitems` | List work items |
| `POST` | `/api/tasks/:taskId/enable` | Enable a task |
| `POST` | `/api/tasks/:taskId/start` | Start a task |
| `POST` | `/api/tasks/:taskId/complete` | Complete a task |
| `GET` | `/api/admin/metrics` | Get platform metrics |
| `POST` | `/api/admin/cases/:id/replay` | Time-travel replay |

## Van der Aalst Pattern Implementations

### WP1: Sequence
```typescript
function evaluateSequence(completedTaskId, flows) {
  return flows.filter(f => f.sourceTaskId === completedTaskId)
              .map(f => f.targetTaskId);
}
```

### WP2: Parallel Split (AND-Split)
```typescript
function evaluateAndSplit(completedTaskId, flows) {
  return flows.filter(f => f.sourceTaskId === completedTaskId)
              .map(f => f.targetTaskId); // Enable ALL targets
}
```

### WP3: Synchronization (AND-Join)
```typescript
async function evaluateAndJoin(taskId, store, caseId) {
  // SPARQL ASK: All incoming branches completed?
  return executeSparqlAsk(store, `
    ASK {
      FILTER NOT EXISTS {
        ?flow yawl:targetTask <${taskId}> .
        ?flow yawl:sourceTask ?source .
        FILTER NOT EXISTS {
          ?workItem yawl:taskRef ?source ;
                    yawl:status yawl:WorkItem_Completed .
        }
      }
    }
  `);
}
```

### WP4: Exclusive Choice (XOR-Split)
```typescript
function evaluateXorSplit(completedTaskId, flows, caseData) {
  for (const flow of flows.sort((a, b) => a.priority - b.priority)) {
    if (evaluateCondition(flow.condition, caseData)) {
      return [flow.targetTaskId]; // Only ONE target
    }
  }
  return [flows.find(f => f.isDefault)?.targetTaskId];
}
```

## Worklet Exception Handling

Worklets use RDF-defined rules with SPARQL ASK conditions:

```turtle
<#timeout-rule-1> a yawl-worklet:Rule ;
    yawl-worklet:ruleCondition "ASK { ?workItem yawl:retryCount ?count . FILTER(?count < 3) }" ;
    yawl-worklet:ruleAction "retry" ;
    yawl-worklet:rulePriority 1 .
```

Generated TypeScript evaluates rules:

```typescript
async evaluateCondition(sparqlAsk, context) {
  const boundQuery = sparqlAsk
    .replace(/\?workItem/g, `<${context.workItem.workItemId}>`);
  return executeSparqlAsk(context.store, boundQuery);
}
```

## Cryptographic Receipts

Every operation generates a cryptographic receipt for audit:

```typescript
const receipt = await generateReceipt({
  eventType: YAWL_EVENT_TYPES.TASK_COMPLETED,
  entityId: workItem.workItemId,
  data: workItem,
  previousHash: proofChain.getLatestHash(),
});
```

Receipt chain integrity is verifiable:

```typescript
const isValid = await engine.verifyReceiptChain();
```

## Integration with unrdf

This example leverages the [unrdf YAWL package](https://github.com/seanchatmangpt/unrdf):

```typescript
import {
  createWorkflow,
  createCase,
  enableTask,
  completeTask,
  generateReceipt,
  verifyReceipt,
  YAWL_NS,
  YAWL_EVENT_TYPES,
} from '@unrdf/yawl';

import { createStore, executeSparqlSelect, executeSparqlAsk } from '@unrdf/oxigraph';
```

## Generation Rules

The `ggen.toml` defines 14 generation rules:

1. **yawl-xml-specs** - YAWL XML from workflow definitions
2. **task-definitions** - TypeScript task types
3. **flow-definitions** - Flow definitions with conditions
4. **servlet-handlers** - HTTP handlers per servlet
5. **worklet-handlers** - Exception handlers with rules
6. **workflow-engine** - Main engine with patterns
7. **rest-api-routes** - Express routes
8. **vitest-tests** - Integration tests with MSW
9. **workflow-index** - Workflow specification index
10. **receipt-types** - Audit trail types
11. **case-types** - Case management types
12. **workitem-types** - Work item types
13. **package-json** - Node.js package configuration
14. **tsconfig** - TypeScript configuration

## Extending

### Add a New Workflow

1. Define the workflow in `ontology/platform-workflows.ttl`:
   ```turtle
   <http://example.org/workflows/my-workflow> a yawl:WorkflowSpec ;
       yawl:specId "my-workflow" ;
       yawl:specName "My Workflow" ;
       yawl:hasTasks <#task-1>, <#task-2> ;
       yawl:hasFlows <#flow-1-2> .
   ```

2. Regenerate:
   ```bash
   ggen sync
   ```

### Add a New Servlet

1. Add to `ontology/platform-workflows.ttl`:
   ```turtle
   <http://example.org/servlets/my-servlet> a yawl-servlet:TaskServlet ;
       yawl-servlet:servletId "my-servlet" ;
       yawl-servlet:urlPattern "/api/my-endpoint" ;
       yawl-servlet:httpMethod "POST" ;
       yawl-servlet:outputPath "lib/servlets/MyServlet.ts" .
   ```

2. Regenerate:
   ```bash
   ggen sync
   ```

### Add a New Exception Handler

1. Define worklet in RDF:
   ```turtle
   <http://example.org/worklets/my-handler> a yawl-worklet:ExceptionHandler ;
       yawl-worklet:handlesException yawl-worklet:MyException ;
       yawl-worklet:hasRuleSet <#my-rules> .
   ```

2. Define rules with SPARQL conditions
3. Regenerate

## Best Practices Demonstrated

1. **RDF-First**: All specifications in Turtle, code is derived
2. **SPARQL-Driven**: File paths, routes, handlers from queries
3. **No Hardcoding**: Everything comes from ontology
4. **Type Safety**: Zod schemas generated from RDF
5. **Audit Trail**: Cryptographic receipts for all operations
6. **Test Coverage**: Integration tests with MSW mocking
7. **Pattern Implementation**: Van der Aalst workflow patterns

## License

MIT
