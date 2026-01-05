# Golden Results

This directory contains the expected output files from running `ggen sync` on the YAWL workflow platform example.

## Purpose

Golden files serve as regression tests for the ggen code generator. When you run `ggen sync`, the generated output should match these golden files exactly.

## Directory Structure

```
golden/
├── lib/
│   ├── definitions/
│   │   ├── tasks.mjs      # Task definitions from RDF ontology
│   │   ├── flows.mjs      # Flow definitions with conditions
│   │   └── index.mjs      # Workflow index and exports
│   ├── types/
│   │   ├── cases.mjs      # Case status types and transitions
│   │   ├── workitems.mjs  # Work item status and helpers
│   │   └── receipts.mjs   # Receipt chain for audit trails
│   ├── api/
│   │   └── routes.mjs     # REST API route definitions
│   ├── engine/
│   │   └── WorkflowEngine.mjs  # Van der Aalst pattern engine
│   ├── worklets/
│   │   └── index.mjs      # Worklet registry and handlers
│   └── tests/
│       └── workflow.test.mjs   # Vitest integration tests
├── package.json           # Generated package.json
└── README.md              # This file
```

## Usage

### Compare Generated Output Against Golden Files

```bash
# From the yawl-workflow-platform directory
node scripts/compare-golden.mjs
```

### Update Golden Files from Generated Output

```bash
# After running ggen sync
node scripts/compare-golden.mjs --update
```

### Compare Specific File

```bash
node scripts/compare-golden.mjs --file=tasks.mjs
```

### Verbose Output

```bash
node scripts/compare-golden.mjs --verbose
```

## Workflow Specifications

The golden files are generated from the following RDF-defined workflows:

| Workflow | Pattern | Tasks | Flows |
|----------|---------|-------|-------|
| cicd-pipeline | OR-split/join (WP6/WP7) | 7 | 8 |
| document-approval | XOR-split (WP4/WP5) | 5 | 5 |
| hr-onboarding | Multiple Instance (WP12-15) | 5 | 4 |
| incident-management | Sequential | 6 | 5 |
| order-processing | AND-split/join (WP2/WP3) | 5 | 5 |

## Updating Golden Files

When the ontology or templates change:

1. Run `ggen sync` to regenerate output
2. Review the changes carefully
3. Run `node scripts/compare-golden.mjs` to see differences
4. If changes are intentional, run `node scripts/compare-golden.mjs --update`
5. Commit both the ontology/template changes and the updated golden files

## Van der Aalst Patterns

The golden files implement the following workflow patterns from Van der Aalst's taxonomy:

- **WP1**: Sequence (all workflows)
- **WP2**: Parallel Split (order-processing)
- **WP3**: Synchronization (order-processing)
- **WP4**: Exclusive Choice (document-approval)
- **WP5**: Simple Merge (document-approval)
- **WP6**: Multi-Choice (cicd-pipeline)
- **WP7**: Structured Synchronizing Merge (cicd-pipeline)
- **WP12-15**: Multiple Instance Patterns (hr-onboarding)
