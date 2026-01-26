---
description: Generate an actionable, dependency-ordered tasks.md for the feature based on available design artifacts.
handoffs: 
  - label: Analyze For Consistency
    agent: speckit.analyze
    prompt: Run a project analysis for consistency
    send: true
  - label: Implement Project
    agent: speckit.implement
    prompt: Start the implementation in phases
    send: true
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## ⚡ RDF-First Architecture

**Constitutional Equation**: `tasks.md = μ(tasks.ttl)`

All task artifacts are Turtle/RDF ontologies. Markdown is a **generated artifact** for GitHub viewing.

- **Source of Truth**: `tasks.ttl` (edit this), reads from `feature.ttl` and `plan.ttl`
- **Derived Artifact**: `tasks.md` (generated via `ggen render`, NEVER edit manually)
- **Schema**: Task ontology schema in `spec-kit-schema.ttl`

## Outline

1. **Setup**: Run `.specify/scripts/bash/check-prerequisites.sh --json` from repo root and parse FEATURE_DIR and AVAILABLE_DOCS list. All paths must be absolute. For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").

2. **Load RDF design documents** (SOURCE OF TRUTH): Read from FEATURE_DIR:
   - **Required TTL sources**: feature.ttl (user stories with priorities), plan.ttl (tech stack, libraries, structure)
   - **Optional TTL sources**: entities.ttl (domain model), contracts.ttl (API endpoints)
   - **Generated markdown** (for human viewing only): spec.md, plan.md, data-model.md, contracts/, research.md, quickstart.md
   - **CRITICAL**: Always read from .ttl files as source of truth, use .md files only for reference
   - Note: Not all projects have all documents. Generate tasks based on what's available in TTL.

3. **Execute RDF-first task generation workflow**:
   - SPARQL query plan.ttl to extract tech stack, libraries, project structure
   - SPARQL query feature.ttl to extract user stories with priorities (P1, P2, P3)
   - If entities.ttl exists: SPARQL query to extract entities and map to user stories
   - If contracts.ttl exists: SPARQL query to map endpoints to user stories
   - If research data in plan.ttl: Extract decisions for setup tasks
   - Create tasks.ttl with RDF structure:
     - Task instances with IDs, descriptions, file paths
     - Dependencies between tasks (using task ontology predicates)
     - User story links (sk:relatedToStory)
     - Parallelizability markers
     - Phase groupings (Setup, Foundational, User Story phases, Polish)
   - Validate tasks.ttl against SHACL shapes
   - Generate dependency graph showing user story completion order
   - Create parallel execution examples per user story
   - Validate task completeness (each user story has all needed tasks, independently testable)

4. **Generate tasks.md artifact**: Create markdown from tasks.ttl RDF source:
   - Correct feature name from plan.md
   - Phase 1: Setup tasks (project initialization)
   - Phase 2: Foundational tasks (blocking prerequisites for all user stories)
   - Phase 3+: One phase per user story (in priority order from spec.md)
   - Each phase includes: story goal, independent test criteria, tests (if requested), implementation tasks
   - Final Phase: Polish & cross-cutting concerns
   - All tasks must follow the strict checklist format (see Task Generation Rules below)
   - Clear file paths for each task
   - Dependencies section showing story completion order
   - Parallel execution examples per story
   - Implementation strategy section (MVP first, incremental delivery)

5. **Generate tasks.md from tasks.ttl**:
   - Run: `ggen render .specify/templates/tasks.tera tasks.ttl > tasks.md`
   - Add header: `<!-- Generated from tasks.ttl - DO NOT EDIT MANUALLY -->`
   - Add footer: `**Generated with**: ggen v6 ontology-driven task system`

6. **Report**: Output paths and summary:
   - TTL source path: tasks.ttl (SOURCE OF TRUTH)
   - Generated markdown path: tasks.md (derived artifact)
   - Total task count
   - Task count per user story
   - Parallel opportunities identified
   - Independent test criteria for each story
   - Suggested MVP scope (typically just User Story 1)
   - Format validation: Confirm ALL tasks follow the checklist format (checkbox, ID, labels, file paths)
   - Reminder: **Edit tasks.ttl, NOT tasks.md. Regenerate with: `ggen render tasks.tera tasks.ttl > tasks.md`**

Context for task generation: $ARGUMENTS

The tasks.md should be immediately executable - each task must be specific enough that an LLM can complete it without additional context.

## Task Generation Rules

**CRITICAL**: Tasks MUST be organized by user story to enable independent implementation and testing.

**Tests are OPTIONAL**: Only generate test tasks if explicitly requested in the feature specification or if user requests TDD approach.

### Checklist Format (REQUIRED)

Every task MUST strictly follow this format:

```text
- [ ] [TaskID] [P?] [Story?] Description with file path
```

**Format Components**:

1. **Checkbox**: ALWAYS start with `- [ ]` (markdown checkbox)
2. **Task ID**: Sequential number (T001, T002, T003...) in execution order
3. **[P] marker**: Include ONLY if task is parallelizable (different files, no dependencies on incomplete tasks)
4. **[Story] label**: REQUIRED for user story phase tasks only
   - Format: [US1], [US2], [US3], etc. (maps to user stories from spec.md)
   - Setup phase: NO story label
   - Foundational phase: NO story label  
   - User Story phases: MUST have story label
   - Polish phase: NO story label
5. **Description**: Clear action with exact file path

**Examples**:

- ✅ CORRECT: `- [ ] T001 Create project structure per implementation plan`
- ✅ CORRECT: `- [ ] T005 [P] Implement authentication middleware in src/middleware/auth.py`
- ✅ CORRECT: `- [ ] T012 [P] [US1] Create User model in src/models/user.py`
- ✅ CORRECT: `- [ ] T014 [US1] Implement UserService in src/services/user_service.py`
- ❌ WRONG: `- [ ] Create User model` (missing ID and Story label)
- ❌ WRONG: `T001 [US1] Create model` (missing checkbox)
- ❌ WRONG: `- [ ] [US1] Create User model` (missing Task ID)
- ❌ WRONG: `- [ ] T001 [US1] Create model` (missing file path)

### Task Organization

1. **From User Stories (spec.md)** - PRIMARY ORGANIZATION:
   - Each user story (P1, P2, P3...) gets its own phase
   - Map all related components to their story:
     - Models needed for that story
     - Services needed for that story
     - Endpoints/UI needed for that story
     - If tests requested: Tests specific to that story
   - Mark story dependencies (most stories should be independent)

2. **From Contracts**:
   - Map each contract/endpoint → to the user story it serves
   - If tests requested: Each contract → contract test task [P] before implementation in that story's phase

3. **From Data Model**:
   - Map each entity to the user story(ies) that need it
   - If entity serves multiple stories: Put in earliest story or Setup phase
   - Relationships → service layer tasks in appropriate story phase

4. **From Setup/Infrastructure**:
   - Shared infrastructure → Setup phase (Phase 1)
   - Foundational/blocking tasks → Foundational phase (Phase 2)
   - Story-specific setup → within that story's phase

### Phase Structure

- **Phase 1**: Setup (project initialization)
- **Phase 2**: Foundational (blocking prerequisites - MUST complete before user stories)
- **Phase 3+**: User Stories in priority order (P1, P2, P3...)
  - Within each story: Tests (if requested) → Models → Services → Endpoints → Integration
  - Each phase should be a complete, independently testable increment
- **Final Phase**: Polish & Cross-Cutting Concerns
