---
description: Execute the implementation planning workflow using the plan template to generate design artifacts.
handoffs: 
  - label: Create Tasks
    agent: speckit.tasks
    prompt: Break the plan into tasks
    send: true
  - label: Create Checklist
    agent: speckit.checklist
    prompt: Create a checklist for the following domain...
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## ⚡ RDF-First Architecture

**Constitutional Equation**: `plan.md = μ(plan.ttl)`

All planning artifacts are Turtle/RDF ontologies. Markdown is a **generated artifact** for GitHub viewing.

- **Source of Truth**: `plan.ttl` (edit this), reads from `feature.ttl`
- **Derived Artifact**: `plan.md` (generated via `ggen render`, NEVER edit manually)
- **Schema**: Plan ontology schema in `spec-kit-schema.ttl`

## Outline

1. **Setup**: Run `.specify/scripts/bash/setup-plan.sh --json` from repo root and parse JSON for FEATURE_SPEC_TTL (feature.ttl), IMPL_PLAN_TTL (plan.ttl), SPECS_DIR, BRANCH. For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").

2. **Load context**:
   - Read FEATURE_SPEC_TTL (feature.ttl - RDF source of truth)
   - Read `.specify/memory/constitution.ttl` (RDF source) and generated `constitution.md`
   - Load plan.ttl template (if not exists, create from RDF plan template)

3. **Execute RDF-first plan workflow**:
   - Create plan.ttl with RDF structure:
     - Technical decisions (architecture patterns, tech stack, libraries)
     - Constitution compliance (reference principles from constitution.ttl)
     - Research questions and resolutions
     - Data model entities (link to feature.ttl entities)
     - API contracts (endpoints, operations)
     - Quality gates and checkpoints
   - Validate plan.ttl against SHACL shapes
   - Phase 0: Generate research.md from plan.ttl research section (resolve all NEEDS CLARIFICATION)
   - Phase 1: Generate data-model.md, contracts/, quickstart.md from plan.ttl
   - Phase 1: Update agent context by running the agent script
   - Re-evaluate Constitution Check post-design

4. **Generate plan.md artifact**:
   - Run: `ggen render .specify/templates/plan.tera plan.ttl > plan.md`
   - Add header: `<!-- Generated from plan.ttl - DO NOT EDIT MANUALLY -->`
   - Add footer: `**Generated with**: ggen v6 ontology-driven planning system`

5. **Stop and report**:
   - Command ends after Phase 2 planning
   - Report: branch, plan.ttl path (source), plan.md path (artifact), generated artifacts
   - Reminder: **Edit plan.ttl, NOT plan.md. Regenerate with: `ggen render plan.tera plan.ttl > plan.md`**

## Phases

### Phase 0: Outline & Research

1. **Extract unknowns from Technical Context** above:
   - For each NEEDS CLARIFICATION → research task
   - For each dependency → best practices task
   - For each integration → patterns task

2. **Generate and dispatch research agents**:

   ```text
   For each unknown in Technical Context:
     Task: "Research {unknown} for {feature context}"
   For each technology choice:
     Task: "Find best practices for {tech} in {domain}"
   ```

3. **Consolidate findings** in `research.md` using format:
   - Decision: [what was chosen]
   - Rationale: [why chosen]
   - Alternatives considered: [what else evaluated]

**Output**: research.md with all NEEDS CLARIFICATION resolved

### Phase 1: Design & Contracts

**Prerequisites:** `research.md` complete

1. **Extract entities from feature spec** → `data-model.md`:
   - Entity name, fields, relationships
   - Validation rules from requirements
   - State transitions if applicable

2. **Generate API contracts** from functional requirements:
   - For each user action → endpoint
   - Use standard REST/GraphQL patterns
   - Output OpenAPI/GraphQL schema to `/contracts/`

3. **Agent context update**:
   - Run `.specify/scripts/bash/update-agent-context.sh claude`
   - These scripts detect which AI agent is in use
   - Update the appropriate agent-specific context file
   - Add only new technology from current plan
   - Preserve manual additions between markers

**Output**: data-model.md, /contracts/*, quickstart.md, agent-specific file

## Key rules

- Use absolute paths
- ERROR on gate failures or unresolved clarifications
