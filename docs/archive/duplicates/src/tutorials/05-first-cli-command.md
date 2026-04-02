<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Your First CLI Command](#your-first-cli-command)
  - [Goal](#goal)
  - [Prerequisites](#prerequisites)
  - [Step 1: Define Your Domain Ontology](#step-1-define-your-domain-ontology)
  - [Step 2: Validate Your Ontology](#step-2-validate-your-ontology)
  - [Step 3: Extract to Schema](#step-3-extract-to-schema)
  - [Step 4: Generate Code](#step-4-generate-code)
  - [Step 5: Use Your Generated Models](#step-5-use-your-generated-models)
  - [Step 6: Verify Your Work](#step-6-verify-your-work)
  - [Next Steps](#next-steps)
  - [Key Concepts](#key-concepts)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Your First CLI Command

Learn how to build your own ggen CLI command from scratch - from RDF definition through code generation to running your new command.

## Goal

By the end of this tutorial, you'll have created a custom CLI command that demonstrates the full ontology-to-code workflow.

## Prerequisites

- ggen installed ([Installation Guide](../how-to-guides/installation.md))
- Basic familiarity with RDF/OWL concepts
- Completed [Getting Started](getting-started.md) tutorial

## Step 1: Define Your Domain Ontology

Create a simple ontology file `project.ttl` for a project management domain:

```turtle
@prefix ex: <http://example.org/project/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
ex:Project a rdfs:Class ;
  rdfs:label "Project" ;
  rdfs:comment "A project with tasks" .

ex:Task a rdfs:Class ;
  rdfs:label "Task" ;
  rdfs:comment "A task within a project" .

ex:Status a rdfs:Class ;
  rdfs:label "Status" ;
  rdfs:comment "Task status enum" .

# Properties
ex:name a rdf:Property ;
  rdfs:domain ex:Project, ex:Task ;
  rdfs:range xsd:string .

ex:description a rdf:Property ;
  rdfs:domain ex:Project, ex:Task ;
  rdfs:range xsd:string .

ex:hasTask a rdf:Property ;
  rdfs:domain ex:Project ;
  rdfs:range ex:Task .

ex:status a rdf:Property ;
  rdfs:domain ex:Task ;
  rdfs:range ex:Status .

ex:priority a rdf:Property ;
  rdfs:domain ex:Task ;
  rdfs:range xsd:integer .
```

## Step 2: Validate Your Ontology

Check your ontology for correctness:

```bash
ggen ontology validate project.ttl
```

Output should show no errors:
```
✓ Ontology validated successfully
- Classes: 3
- Properties: 5
- Relationships: Valid
```

## Step 3: Extract to Schema

Extract your ontology into a machine-readable schema:

```bash
ggen ontology extract project.ttl \
  --namespace http://example.org/project# \
  --output project-schema.json
```

This generates a JSON schema you can inspect:

```json
{
  "classes": [
    {
      "name": "Project",
      "properties": ["name", "description", "hasTask"]
    },
    {
      "name": "Task",
      "properties": ["name", "description", "status", "priority"]
    }
  ]
}
```

## Step 4: Generate Code

Generate TypeScript types from your schema:

```bash
ggen ontology generate project-schema.json \
  --language typescript \
  --output src/models/project.ts
```

This creates:

```typescript
export class Project {
  name: string;
  description: string;
  tasks: Task[];
}

export class Task {
  name: string;
  description: string;
  status: Status;
  priority: number;
}

export enum Status {
  TODO = "TODO",
  IN_PROGRESS = "IN_PROGRESS",
  DONE = "DONE"
}
```

## Step 5: Use Your Generated Models

Create a simple application using the generated types:

```typescript
import { Project, Task, Status } from './models/project';

const myProject = new Project();
myProject.name = "Build Documentation";
myProject.description = "Complete Diataxis refactor";

const task1 = new Task();
task1.name = "Write tutorials";
task1.status = Status.IN_PROGRESS;
task1.priority = 1;

const task2 = new Task();
task2.name = "Create how-to guides";
task2.status = Status.TODO;
task2.priority = 2;

myProject.tasks = [task1, task2];

console.log(`Project: ${myProject.name}`);
myProject.tasks.forEach(task => {
  console.log(`  - ${task.name} [${task.status}] Priority: ${task.priority}`);
});
```

## Step 6: Verify Your Work

Run your application to verify the generated code works:

```bash
npx ts-node src/app.ts
```

Expected output:
```
Project: Build Documentation
  - Write tutorials [IN_PROGRESS] Priority: 1
  - Create how-to guides [TODO] Priority: 2
```

## Next Steps

Now that you've completed the basic workflow:

1. **Expand your ontology**: Add more classes and relationships
2. **Generate to other languages**: Use `--language python` or `--language rust`
3. **Create custom templates**: [Template Creation](06-template-creation.md)
4. **Multi-language sync**: [Multi-Language Project](07-multi-language-project.md)

## Key Concepts

**Ontology-Driven Development**: Your domain model (ontology) is the single source of truth. All code is generated from it, ensuring consistency across languages and eliminating drift.

**Type Safety**: Generated code includes full type information, enabling compile-time error detection and IDE support.

**Deterministic Generation**: The same ontology always produces identical code - perfect for reproducible builds and version control.

## Summary

You've learned:
- ✅ How to define domain ontologies in Turtle format
- ✅ How to validate ontologies for correctness
- ✅ How to extract schemas from ontologies
- ✅ How to generate language-specific code
- ✅ How to use generated types in real applications

The ontology-to-code workflow is now in your toolkit!
