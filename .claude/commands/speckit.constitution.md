---
description: Create or update the project constitution from interactive or provided principle inputs, ensuring all dependent templates stay in sync.
handoffs: 
  - label: Build Specification
    agent: speckit.specify
    prompt: Implement the feature specification based on the updated constitution. I want to build...
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## ⚡ RDF-First Architecture

**Constitutional Equation**: `constitution.md = μ(constitution.ttl)`

The project constitution is a Turtle/RDF ontology. Markdown is a **generated artifact** for GitHub viewing.

- **Source of Truth**: `constitution.ttl` (edit this)
- **Derived Artifact**: `constitution.md` (generated via `ggen render`, NEVER edit manually)
- **Schema**: Constitution ontology schema in `spec-kit-schema.ttl`

## Outline

You are updating the project constitution at `.specify/memory/constitution.ttl` (TTL source) and generating `.specify/memory/constitution.md` (markdown artifact). The TTL file defines principles as RDF instances. Your job is to (a) collect/derive concrete values, (b) create/update RDF instances in constitution.ttl, (c) generate constitution.md from TTL, and (d) propagate any amendments across dependent artifacts.

Follow this execution flow:

1. **Load or create constitution.ttl**:
   - If exists: Read `.specify/memory/constitution.ttl` and parse existing principles
   - If not exists: Create from RDF constitution template
   - Identify RDF instances of type `sk:Principle`, `sk:BuildStandard`, `sk:WorkflowRule`
   **IMPORTANT**: The user might require less or more principles than currently defined. If a number is specified, respect that - add/remove principle instances in the RDF graph accordingly.

2. Collect/derive values for placeholders:
   - If user input (conversation) supplies a value, use it.
   - Otherwise infer from existing repo context (README, docs, prior constitution versions if embedded).
   - For governance dates: `RATIFICATION_DATE` is the original adoption date (if unknown ask or mark TODO), `LAST_AMENDED_DATE` is today if changes are made, otherwise keep previous.
   - `CONSTITUTION_VERSION` must increment according to semantic versioning rules:
     - MAJOR: Backward incompatible governance/principle removals or redefinitions.
     - MINOR: New principle/section added or materially expanded guidance.
     - PATCH: Clarifications, wording, typo fixes, non-semantic refinements.
   - If version bump type ambiguous, propose reasoning before finalizing.

3. **Draft updated constitution RDF (constitution.ttl)**:
   - Create/update RDF instances in Turtle syntax:
     - Feature metadata (project name, version, ratification date)
     - Principle instances with properties:
       - `sk:principleName` - Succinct name
       - `sk:principleDescription` - Non-negotiable rules
       - `sk:rationale` - Why this principle exists
       - `sk:principleIndex` - Order (1, 2, 3...)
     - Build standards (cargo make rules, timeout requirements)
     - Workflow rules (TDD, error handling, concurrent execution)
     - Governance rules (amendment procedure, versioning policy)
   - Preserve RDF graph structure and use proper Turtle syntax
   - Ensure each Principle has: name, description, rationale, index
   - Ensure Governance section has: amendment procedure, versioning policy, compliance review expectations

4. Consistency propagation checklist (convert prior checklist into active validations):
   - Read `.specify/templates/plan-template.md` and ensure any "Constitution Check" or rules align with updated principles.
   - Read `.specify/templates/spec-template.md` for scope/requirements alignment—update if constitution adds/removes mandatory sections or constraints.
   - Read `.specify/templates/tasks-template.md` and ensure task categorization reflects new or removed principle-driven task types (e.g., observability, versioning, testing discipline).
   - Read each command file in `.specify/templates/commands/*.md` (including this one) to verify no outdated references (agent-specific names like CLAUDE only) remain when generic guidance is required.
   - Read any runtime guidance docs (e.g., `README.md`, `docs/quickstart.md`, or agent-specific guidance files if present). Update references to principles changed.

5. Produce a Sync Impact Report (prepend as an HTML comment at top of the constitution file after update):
   - Version change: old → new
   - List of modified principles (old title → new title if renamed)
   - Added sections
   - Removed sections
   - Templates requiring updates (✅ updated / ⚠ pending) with file paths
   - Follow-up TODOs if any placeholders intentionally deferred.

6. Validation before final output:
   - No remaining unexplained bracket tokens.
   - Version line matches report.
   - Dates ISO format YYYY-MM-DD.
   - Principles are declarative, testable, and free of vague language ("should" → replace with MUST/SHOULD rationale where appropriate).

7. **Write constitution.ttl** (SOURCE OF TRUTH):
   - Save updated RDF graph to `.specify/memory/constitution.ttl`
   - Validate against SHACL shapes

8. **Generate constitution.md** (DERIVED ARTIFACT):
   - Run: `ggen render .specify/templates/constitution.tera constitution.ttl > constitution.md`
   - Add header: `<!-- Generated from constitution.ttl - DO NOT EDIT MANUALLY -->`
   - Prepend Sync Impact Report as HTML comment (see step 5)
   - Add footer: `**Generated with**: ggen v6 ontology-driven constitution system`

9. **Output final summary** to the user with:
   - TTL source path: constitution.ttl (SOURCE OF TRUTH)
   - Generated markdown path: constitution.md (derived artifact)
   - New version and bump rationale
   - Any files flagged for manual follow-up
   - Suggested commit message (e.g., `docs: amend constitution to vX.Y.Z (principle additions + governance update)`)
   - Reminder: **Edit constitution.ttl, NOT constitution.md. Regenerate with: `ggen render constitution.tera constitution.ttl > constitution.md`**

Formatting & Style Requirements:

- Use Markdown headings exactly as in the template (do not demote/promote levels).
- Wrap long rationale lines to keep readability (<100 chars ideally) but do not hard enforce with awkward breaks.
- Keep a single blank line between sections.
- Avoid trailing whitespace.

If the user supplies partial updates (e.g., only one principle revision), still perform validation and version decision steps.

If critical info missing (e.g., ratification date truly unknown), insert `TODO(<FIELD_NAME>): explanation` and include in the Sync Impact Report under deferred items.

Do not create a new template; always operate on the existing `.specify/memory/constitution.md` file.
