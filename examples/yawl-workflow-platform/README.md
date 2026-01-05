# YAWL Workflow Platform

A comprehensive workflow automation platform built on YAWL (Yet Another Workflow Language) with RDF/SPARQL integration, servlet handlers, and worklet exception management. All code is generated from RDF ontology specifications using ggen.

## Overview

This example demonstrates ggen's power to generate a complete workflow automation platform from declarative RDF specifications. It implements Van der Aalst's workflow patterns and integrates with the [unrdf YAWL package](https://github.com/seanchatmangpt/unrdf).

### Key Features

- **YAWL XML Generation**: Generate YAWL 4.0 XML specifications from RDF
- **Van der Aalst Patterns**: Full support for WP1-WP7 workflow patterns
- **Servlet Handlers**: ESM HTTP handlers for workflow operations
- **Worklet Exception Handlers**: RDF-driven rule-based exception handling
- **Cryptographic Receipts**: SHA-256 hashed audit trail for all operations (via hash-wasm)
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
│  │  • Servlet Handlers        • ESM Types with Zod                   │    │
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

### Configure Environment

```bash
# Copy example configuration
cp .env.example .env

# Edit as needed
vim .env
```

### Start Development Server

```bash
# Run the generated platform
node lib/main.mjs

# Or with npm script
npm run dev

# Server runs at http://localhost:3000
```

### Verify Golden Files

```bash
# Compare generated output against expected results
node scripts/compare-golden.mjs

# Update golden files if changes are intentional
node scripts/compare-golden.mjs --update
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
```javascript
function evaluateSequence(completedTaskId, flows) {
  return flows.filter(f => f.sourceTaskId === completedTaskId)
              .map(f => f.targetTaskId);
}
```

### WP2: Parallel Split (AND-Split)
```javascript
function evaluateAndSplit(completedTaskId, flows) {
  return flows.filter(f => f.sourceTaskId === completedTaskId)
              .map(f => f.targetTaskId); // Enable ALL targets
}
```

### WP3: Synchronization (AND-Join)
```javascript
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
```javascript
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

Generated JavaScript evaluates rules:

```javascript
async evaluateCondition(sparqlAsk, context) {
  const boundQuery = sparqlAsk
    .replace(/\?workItem/g, `<${context.workItem.workItemId}>`);
  return executeSparqlAsk(context.store, boundQuery);
}
```

## Cryptographic Receipts

Every operation generates a cryptographic receipt for audit using SHA-256 (via hash-wasm):

```javascript
const receipt = await generateReceipt({
  eventType: YAWL_EVENT_TYPES.TASK_COMPLETED,
  entityId: workItem.workItemId,
  data: workItem,
  previousHash: proofChain.getLatestHash(),
});
```

Receipt chain integrity is verifiable:

```javascript
const isValid = await engine.verifyReceiptChain();
```

## Integration with unrdf

This example leverages the [unrdf YAWL package](https://github.com/seanchatmangpt/unrdf):

```javascript
import {
  createWorkflow,
  createCase,
  enableTask,
  completeTask,
  generateReceipt,
  verifyReceipt,
  YAWL_NS,
  YAWL_EVENT_TYPES,
  createYawlStore,
  executeSparqlSelect,
  executeSparqlAsk,
} from '@unrdf/yawl';
```

## Generation Rules

The `ggen.toml` defines 14 generation rules:

1. **yawl-xml-specs** - YAWL XML from workflow definitions
2. **task-definitions** - ESM task types with Zod
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
14. **worklet-index** - Worklet registry module

## Extending

### Complete Extension Workflow

Follow this workflow when modifying the platform:

```bash
# 1. Edit the RDF ontology
vim ontology/platform-workflows.ttl

# 2. Regenerate code from specifications
ggen sync

# 3. Review differences against golden files
node scripts/compare-golden.mjs

# 4. Run tests to verify changes
npm test

# 5. If changes are intentional, update golden files
node scripts/compare-golden.mjs --update

# 6. Commit BOTH ontology changes AND golden updates
git add ontology/ golden/
git commit -m "feat: Add new workflow to platform"
```

### Add a New Workflow

1. Define the workflow in `ontology/platform-workflows.ttl`:
   ```turtle
   <http://example.org/workflows/my-workflow> a yawl:WorkflowSpec ;
       yawl:specId "my-workflow" ;
       yawl:specName "My Workflow" ;
       yawl:specVersion "1.0.0" ;
       yawl:inputCondition <#my-input> ;
       yawl:outputCondition <#my-output> ;
       yawl:hasTasks <#task-1>, <#task-2> ;
       yawl:hasFlows <#flow-1-2> .

   <#my-input> a yawl:Condition ; rdfs:label "Start" .
   <#my-output> a yawl:Condition ; rdfs:label "End" .

   <#task-1> a yawl:AtomicTask ;
       yawl:taskId "task-1" ;
       yawl:taskName "First Task" ;
       yawl:taskKind "automated" ;
       yawl:splitBehavior yawl:XOR_Split ;
       yawl:joinBehavior yawl:XOR_Join .
   ```

2. Regenerate and verify:
   ```bash
   ggen sync
   node scripts/compare-golden.mjs
   npm test
   ```

### Add a New Servlet

1. Add to `ontology/platform-workflows.ttl`:
   ```turtle
   <http://example.org/servlets/my-servlet> a yawl-servlet:TaskServlet ;
       yawl-servlet:servletId "my-servlet" ;
       yawl-servlet:urlPattern "/api/my-endpoint" ;
       yawl-servlet:httpMethod "POST" ;
       yawl-servlet:outputPath "lib/servlets/MyServlet.mjs" .
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
4. **Type Safety**: Zod schemas generated from RDF (pure ESM, no TypeScript)
5. **Audit Trail**: Cryptographic receipts for all operations
6. **Test Coverage**: Integration tests with MSW mocking
7. **Pattern Implementation**: Van der Aalst workflow patterns

## License

MIT
