<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-to: Run Process Conformance Checking](#how-to-run-process-conformance-checking)
  - [Scenario](#scenario)
  - [1. Trace Your Execution](#1-trace-your-execution)
  - [2. Discover the Process Model](#2-discover-the-process-model)
  - [3. Synthesize a Semantic OS Law](#3-synthesize-a-semantic-os-law)
  - [4. Run Conformance Audit](#4-run-conformance-audit)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-to: Run Process Conformance Checking

This guide explains how to use the high-performance `pictl` engine to validate your execution logs against a discovered process model.

## Scenario
You have an execution log (in JSON format) and you want to ensure the actual execution matches the "lawful" Petri net discovered from your workflow.

## 1. Trace Your Execution
As your system runs, emit events to a JSON log:
```bash
# Record start of a task
ggen workflow event my-log.json case-001 "StartProcessing" --resource "agent-alpha"

# Record completion
ggen workflow event my-log.json case-001 "CompleteProcessing"
```

## 2. Discover the Process Model
Use the Alpha miner to discover the underlying Petri net from your log:
```bash
ggen workflow discover my-log.json
```
This will output a Mermaid graph and summary of the places/transitions discovered.

## 3. Synthesize a Semantic OS Law
Once you are happy with the model, project it as a formal Semantic OS Law:
```bash
ggen workflow synthesize my-log.json law-01 "ProcessingLaw"
```

## 4. Run Conformance Audit
Finally, use the audit skill to verify that new logs conform to this law:
```bash
ggen audit conformance --log new-execution.json --law law-01.ttl
```

The system will return a **fitness score**. If the score is 1.0, the execution perfectly aligns with the constitutional law.
