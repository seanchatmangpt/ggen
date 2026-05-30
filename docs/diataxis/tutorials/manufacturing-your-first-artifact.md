<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Tutorial: Manufacturing Your First Semantic Artifact](#tutorial-manufacturing-your-first-semantic-artifact)
  - [Learning Objectives](#learning-objectives)
  - [Prerequisites](#prerequisites)
  - [Step 1: Initialize the Project](#step-1-initialize-the-project)
  - [Step 2: Create a Domain Ontology](#step-2-create-a-domain-ontology)
  - [Step 3: Define Manufacturing Intent](#step-3-define-manufacturing-intent)
  - [Step 4: Run the μ-Pipeline](#step-4-run-the-%CE%BC-pipeline)
  - [Step 5: Verify the Audit Trail](#step-5-verify-the-audit-trail)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Tutorial: Manufacturing Your First Semantic Artifact

This tutorial walks you through the end-to-end process of projecting a domain ontology into verified source code using the `ggen` manufacturing pipeline.

## Learning Objectives
- Initialize a manufacturing project.
- Define a simple `ManufacturingIntent`.
- Run the μ-pipeline (`ggen sync`).
- Verify the artifact against Proof Gates.

## Prerequisites
- `ggen` CLI installed.
- A basic understanding of RDF/Turtle.

## Step 1: Initialize the Project
Create a new directory and initialize the `ggen` manifest:
```bash
mkdir my-factory && cd my-factory
ggen init
```

## Step 2: Create a Domain Ontology
Create a file named `domain.ttl` with a single class:
```turtle
@prefix ex: <http://example.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

ex:SoftwareComponent a owl:Class .
```

## Step 3: Define Manufacturing Intent
Initialize your governance intent. This defines *why* we are manufacturing this artifact:
```bash
# This creates a default intent configuration
ggen governance init
```
Edit the generated intent to set your objective:
```json
{
  "objective": "Bootstrap core domain types for the Vision 2030 project."
}
```

## Step 4: Run the μ-Pipeline
Project your ontology into code. `ggen` will automatically run all 5 μ-stages plus the Stage 6 Proof Gates:
```bash
ggen sync --ontology domain.ttl
```

## Step 5: Verify the Audit Trail
Check the cryptographic receipt and the proof gate report:
```bash
ggen audit verify .ggen/receipts/latest.json
```

**Congratulations!** You have manufactured your first verified software artifact. The artifact is now cryptographically bound to its source ontology and validated against the manufacturing constitution.
