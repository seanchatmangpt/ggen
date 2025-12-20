---
description: Create or update the feature specification from a natural language feature description.
handoffs: 
  - label: Build Technical Plan
    agent: speckit.plan
    prompt: Create a plan for the spec. I am building with...
  - label: Clarify Spec Requirements
    agent: speckit.clarify
    prompt: Clarify specification requirements
    send: true
scripts:
  sh: scripts/bash/create-new-feature.sh --json "{ARGS}"
  ps: scripts/powershell/create-new-feature.ps1 -Json "{ARGS}"
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Outline

The text the user typed after `/speckit.specify` in the triggering message **is** the feature description. Assume you always have it available in this conversation even if `{ARGS}` appears literally below. Do not ask the user to repeat it unless they provided an empty command.

Given that feature description, do this:

1. **Generate a concise short name** (2-4 words) for the branch:
   - Analyze the feature description and extract the most meaningful keywords
   - Create a 2-4 word short name that captures the essence of the feature
   - Use action-noun format when possible (e.g., "add-user-auth", "fix-payment-bug")
   - Preserve technical terms and acronyms (OAuth2, API, JWT, etc.)
   - Keep it concise but descriptive enough to understand the feature at a glance
   - Examples:
     - "I want to add user authentication" → "user-auth"
     - "Implement OAuth2 integration for the API" → "oauth2-api-integration"
     - "Create a dashboard for analytics" → "analytics-dashboard"
     - "Fix payment processing timeout bug" → "fix-payment-timeout"

2. **Check for existing branches before creating new one**:

   a. First, fetch all remote branches to ensure we have the latest information:

      ```bash
      git fetch --all --prune
      ```

   b. Find the highest feature number across all sources for the short-name:
      - Remote branches: `git ls-remote --heads origin | grep -E 'refs/heads/[0-9]+-<short-name>$'`
      - Local branches: `git branch | grep -E '^[* ]*[0-9]+-<short-name>$'`
      - Specs directories: Check for directories matching `specs/[0-9]+-<short-name>`

   c. Determine the next available number:
      - Extract all numbers from all three sources
      - Find the highest number N
      - Use N+1 for the new branch number

   d. Run the script `{SCRIPT}` with the calculated number and short-name:
      - Pass `--number N+1` and `--short-name "your-short-name"` along with the feature description
      - Bash example: `{SCRIPT} --json --number 5 --short-name "user-auth" "Add user authentication"`
      - PowerShell example: `{SCRIPT} -Json -Number 5 -ShortName "user-auth" "Add user authentication"`

   **IMPORTANT**:
   - Check all three sources (remote branches, local branches, specs directories) to find the highest number
   - Only match branches/directories with the exact short-name pattern
   - If no existing branches/directories found with this short-name, start with number 1
   - You must only ever run this script once per feature
   - The JSON is provided in the terminal as output - always refer to it to get the actual content you're looking for
   - The JSON output will contain BRANCH_NAME, SPEC_FILE, and FORMAT (markdown or rdf)
   - For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot")

3. **Check FORMAT from script JSON output**:

   The script returns a JSON object with a FORMAT field indicating specification mode:
   - `"FORMAT": "markdown"` - Traditional markdown-based specifications (default)
   - `"FORMAT": "rdf"` - RDF ontology-driven specifications (3T methodology)

   **If FORMAT is "rdf"**, follow the RDF Generation Workflow (Section 3a) below.
   **If FORMAT is "markdown"**, follow the Markdown Specification Workflow (Section 3b) below.

   ## 3a. RDF Generation Workflow (when FORMAT="rdf")

   When RDF mode is detected, you will generate specification content as RDF triples instead of markdown prose. The ggen v6 pipeline will then transform these triples into human-readable markdown through templates.

   **Key Principle**: You write semantic substrate (RDF), not presentation (markdown). Think in terms of:
   - **Classes**: Feature, UserStory, FunctionalRequirement, SuccessCriterion, Entity
   - **Properties**: sk:title, sk:priority, sk:description, sk:hasUserStory, sk:hasAcceptanceScenario
   - **Relationships**: Features have user stories, stories have acceptance scenarios

   ### RDF Generation Steps:

   1. **Load RDF Helper Templates**:
      Reference the copy-paste patterns in `vendors/spec-kit/templates/rdf-helpers/`:
      - `user-story.ttl.template` - User stories with Given-When-Then scenarios
      - `functional-requirement.ttl.template` - FR-XXX requirements
      - `success-criterion.ttl.template` - SC-XXX measurable outcomes
      - `entity.ttl.template` - Domain entities

   2. **Edit the RDF Ontology File** (SPEC_FILE from JSON output):
      Open `FEATURE_DIR/ontology/feature-content.ttl` and add RDF triples for:

      a. **User Stories** (use user-story.ttl.template pattern):
         ```turtle
         :us-001 a sk:UserStory ;
             sk:storyIndex 1 ;
             sk:title "Story Title (2-8 words)" ;
             sk:priority "P1" ;  # MUST be P1, P2, or P3 (SHACL validated)
             sk:description "User story description..." ;
             sk:priorityRationale "Why this priority..." ;
             sk:independentTest "How to verify independently..." ;
             sk:hasAcceptanceScenario :us-001-as-001 .

         :us-001-as-001 a sk:AcceptanceScenario ;
             sk:scenarioIndex 1 ;
             sk:given "Initial state" ;
             sk:when "Action" ;
             sk:then "Expected outcome" .

         :feature-name sk:hasUserStory :us-001 .
         ```

      b. **Functional Requirements** (use functional-requirement.ttl.template):
         ```turtle
         :fr-001 a sk:FunctionalRequirement ;
             sk:requirementId "FR-001" ;  # MUST match ^FR-[0-9]{3}$ (SHACL validated)
             sk:description "System MUST [capability]" ;
             sk:category "Optional Category" .

         :feature-name sk:hasFunctionalRequirement :fr-001 .
         ```

      c. **Success Criteria** (use success-criterion.ttl.template):
         ```turtle
         :sc-001 a sk:SuccessCriterion ;
             sk:criterionId "SC-001" ;  # MUST match ^SC-[0-9]{3}$ (SHACL validated)
             sk:measurable true ;
             sk:metric "Metric name" ;
             sk:target "Target value (e.g., < 30 seconds, >= 90%)" ;
             sk:description "Outcome description" .

         :feature-name sk:hasSuccessCriterion :sc-001 .
         ```

      d. **Entities** (if data involved, use entity.ttl.template):
         ```turtle
         :entity-name a sk:Entity ;
             sk:entityName "Entity Name" ;
             sk:definition "What this entity represents" ;
             sk:keyAttributes "attribute1, attribute2, attribute3" .

         :feature-name sk:hasEntity :entity-name .
         ```

   3. **SHACL Validation Rules** (enforced automatically):
      - Priority MUST be exactly "P1", "P2", or "P3" (not "HIGH", "MEDIUM", "LOW")
      - Requirement IDs MUST match pattern: FR-001, FR-002, etc. (not REQ-1, R-001)
      - Success criterion IDs MUST match pattern: SC-001, SC-002, etc.
      - User stories MUST have at least one acceptance scenario
      - Feature branch MUST match pattern: ###-feature-name

   4. **Generate Markdown from RDF**:
      After completing the RDF ontology, run the ggen sync command to generate markdown:
      ```bash
      cd FEATURE_DIR
      ggen sync
      ```
      This will:
      - Validate RDF against SHACL shapes (μ₁ - Normalization)
      - Extract bindings via SPARQL queries (μ₂ - Extraction)
      - Render markdown via Tera templates (μ₃ - Emission)
      - Canonicalize output format (μ₄ - Canonicalization)
      - Generate cryptographic receipt (μ₅ - Receipt)

   5. **View Generated Specification**:
      The human-readable spec is at: `FEATURE_DIR/generated/spec.md`

   6. **Verify Idempotence** (optional but recommended):
      ```bash
      ggen sync  # Second run should produce no file changes
      git status FEATURE_DIR/generated/  # Should show clean
      ```

   **RDF Mode Benefits**:
   - ✅ SHACL validation catches errors before generation (invalid priorities, wrong ID patterns)
   - ✅ Idempotent outputs (μ∘μ = μ) - running twice produces zero changes
   - ✅ Cryptographic provenance (receipt proves spec.md = μ(ontology))
   - ✅ Machine-readable specifications (SPARQL queryable)
   - ✅ Multi-view generation (same ontology → markdown, JSON, HTML, PDF)

   **Important**: In RDF mode, NEVER manually edit `generated/spec.md`. Always edit the `.ttl` ontology files and regenerate.

   After generating the spec, proceed to step 6 for validation.

   ## 3b. Markdown Specification Workflow (when FORMAT="markdown")

   Load `templates/spec-template.md` to understand required sections.

4. Follow this execution flow:

    1. Parse user description from Input
       If empty: ERROR "No feature description provided"
    2. Extract key concepts from description
       Identify: actors, actions, data, constraints
    3. For unclear aspects:
       - Make informed guesses based on context and industry standards
       - Only mark with [NEEDS CLARIFICATION: specific question] if:
         - The choice significantly impacts feature scope or user experience
         - Multiple reasonable interpretations exist with different implications
         - No reasonable default exists
       - **LIMIT: Maximum 3 [NEEDS CLARIFICATION] markers total**
       - Prioritize clarifications by impact: scope > security/privacy > user experience > technical details
    4. Fill User Scenarios & Testing section
       If no clear user flow: ERROR "Cannot determine user scenarios"
    5. Generate Functional Requirements
       Each requirement must be testable
       Use reasonable defaults for unspecified details (document assumptions in Assumptions section)
    6. Define Success Criteria
       Create measurable, technology-agnostic outcomes
       Include both quantitative metrics (time, performance, volume) and qualitative measures (user satisfaction, task completion)
       Each criterion must be verifiable without implementation details
    7. Identify Key Entities (if data involved)
    8. Return: SUCCESS (spec ready for planning)

5. Write the specification to SPEC_FILE using the template structure, replacing placeholders with concrete details derived from the feature description (arguments) while preserving section order and headings.

6. **Specification Quality Validation**: After writing the initial spec, validate it against quality criteria:

   a. **Create Spec Quality Checklist**: Generate a checklist file at `FEATURE_DIR/checklists/requirements.md` using the checklist template structure with these validation items:

      ```markdown
      # Specification Quality Checklist: [FEATURE NAME]
      
      **Purpose**: Validate specification completeness and quality before proceeding to planning
      **Created**: [DATE]
      **Feature**: [Link to spec.md]
      
      ## Content Quality
      
      - [ ] No implementation details (languages, frameworks, APIs)
      - [ ] Focused on user value and business needs
      - [ ] Written for non-technical stakeholders
      - [ ] All mandatory sections completed
      
      ## Requirement Completeness
      
      - [ ] No [NEEDS CLARIFICATION] markers remain
      - [ ] Requirements are testable and unambiguous
      - [ ] Success criteria are measurable
      - [ ] Success criteria are technology-agnostic (no implementation details)
      - [ ] All acceptance scenarios are defined
      - [ ] Edge cases are identified
      - [ ] Scope is clearly bounded
      - [ ] Dependencies and assumptions identified
      
      ## Feature Readiness
      
      - [ ] All functional requirements have clear acceptance criteria
      - [ ] User scenarios cover primary flows
      - [ ] Feature meets measurable outcomes defined in Success Criteria
      - [ ] No implementation details leak into specification
      
      ## Notes
      
      - Items marked incomplete require spec updates before `/speckit.clarify` or `/speckit.plan`
      ```

   b. **Run Validation Check**: Review the spec against each checklist item:
      - For each item, determine if it passes or fails
      - Document specific issues found (quote relevant spec sections)

   c. **Handle Validation Results**:

      - **If all items pass**: Mark checklist complete and proceed to step 6

      - **If items fail (excluding [NEEDS CLARIFICATION])**:
        1. List the failing items and specific issues
        2. Update the spec to address each issue
        3. Re-run validation until all items pass (max 3 iterations)
        4. If still failing after 3 iterations, document remaining issues in checklist notes and warn user

      - **If [NEEDS CLARIFICATION] markers remain**:
        1. Extract all [NEEDS CLARIFICATION: ...] markers from the spec
        2. **LIMIT CHECK**: If more than 3 markers exist, keep only the 3 most critical (by scope/security/UX impact) and make informed guesses for the rest
        3. For each clarification needed (max 3), present options to user in this format:

           ```markdown
           ## Question [N]: [Topic]
           
           **Context**: [Quote relevant spec section]
           
           **What we need to know**: [Specific question from NEEDS CLARIFICATION marker]
           
           **Suggested Answers**:
           
           | Option | Answer | Implications |
           |--------|--------|--------------|
           | A      | [First suggested answer] | [What this means for the feature] |
           | B      | [Second suggested answer] | [What this means for the feature] |
           | C      | [Third suggested answer] | [What this means for the feature] |
           | Custom | Provide your own answer | [Explain how to provide custom input] |
           
           **Your choice**: _[Wait for user response]_
           ```

        4. **CRITICAL - Table Formatting**: Ensure markdown tables are properly formatted:
           - Use consistent spacing with pipes aligned
           - Each cell should have spaces around content: `| Content |` not `|Content|`
           - Header separator must have at least 3 dashes: `|--------|`
           - Test that the table renders correctly in markdown preview
        5. Number questions sequentially (Q1, Q2, Q3 - max 3 total)
        6. Present all questions together before waiting for responses
        7. Wait for user to respond with their choices for all questions (e.g., "Q1: A, Q2: Custom - [details], Q3: B")
        8. Update the spec by replacing each [NEEDS CLARIFICATION] marker with the user's selected or provided answer
        9. Re-run validation after all clarifications are resolved

   d. **Update Checklist**: After each validation iteration, update the checklist file with current pass/fail status

7. Report completion with branch name, spec file path, checklist results, and readiness for the next phase (`/speckit.clarify` or `/speckit.plan`).

**NOTE:** The script creates and checks out the new branch and initializes the spec file before writing.

## General Guidelines

## Quick Guidelines

- Focus on **WHAT** users need and **WHY**.
- Avoid HOW to implement (no tech stack, APIs, code structure).
- Written for business stakeholders, not developers.
- DO NOT create any checklists that are embedded in the spec. That will be a separate command.

### Section Requirements

- **Mandatory sections**: Must be completed for every feature
- **Optional sections**: Include only when relevant to the feature
- When a section doesn't apply, remove it entirely (don't leave as "N/A")

### For AI Generation

When creating this spec from a user prompt:

1. **Make informed guesses**: Use context, industry standards, and common patterns to fill gaps
2. **Document assumptions**: Record reasonable defaults in the Assumptions section
3. **Limit clarifications**: Maximum 3 [NEEDS CLARIFICATION] markers - use only for critical decisions that:
   - Significantly impact feature scope or user experience
   - Have multiple reasonable interpretations with different implications
   - Lack any reasonable default
4. **Prioritize clarifications**: scope > security/privacy > user experience > technical details
5. **Think like a tester**: Every vague requirement should fail the "testable and unambiguous" checklist item
6. **Common areas needing clarification** (only if no reasonable default exists):
   - Feature scope and boundaries (include/exclude specific use cases)
   - User types and permissions (if multiple conflicting interpretations possible)
   - Security/compliance requirements (when legally/financially significant)

**Examples of reasonable defaults** (don't ask about these):

- Data retention: Industry-standard practices for the domain
- Performance targets: Standard web/mobile app expectations unless specified
- Error handling: User-friendly messages with appropriate fallbacks
- Authentication method: Standard session-based or OAuth2 for web apps
- Integration patterns: RESTful APIs unless specified otherwise

### Success Criteria Guidelines

Success criteria must be:

1. **Measurable**: Include specific metrics (time, percentage, count, rate)
2. **Technology-agnostic**: No mention of frameworks, languages, databases, or tools
3. **User-focused**: Describe outcomes from user/business perspective, not system internals
4. **Verifiable**: Can be tested/validated without knowing implementation details

**Good examples**:

- "Users can complete checkout in under 3 minutes"
- "System supports 10,000 concurrent users"
- "95% of searches return results in under 1 second"
- "Task completion rate improves by 40%"

**Bad examples** (implementation-focused):

- "API response time is under 200ms" (too technical, use "Users see results instantly")
- "Database can handle 1000 TPS" (implementation detail, use user-facing metric)
- "React components render efficiently" (framework-specific)
- "Redis cache hit rate above 80%" (technology-specific)
