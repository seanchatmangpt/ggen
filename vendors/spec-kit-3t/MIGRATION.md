# Migration Guide: Markdown Spec-Kit → RDF-Based Spec-Kit

## Overview

This guide walks through converting existing markdown-based Spec-Kit specifications to the pure 3T (TOML, Tera, Turtle) ontology-driven format.

## Migration Strategy

**Goal**: Transform markdown specifications into RDF triples while preserving all content and improving structural validation.

**Approach**: Manual transformation with automated validation (SHACL shapes catch errors early).

**Estimated Effort**: 1-2 hours per feature specification.

## Prerequisites

- Existing markdown specification following Spec-Kit template format
- Basic understanding of RDF/Turtle syntax
- ggen v6 CLI with `ggen sync` command (for validation)

## Step-by-Step Migration Process

### Step 1: Analyze Existing Markdown Specification

**Input**: `specs/NNN-feature-name/spec.md`

**Read the markdown specification and extract**:
1. Feature metadata (branch name, feature name, created date, status)
2. User input / feature description
3. User stories (count, priorities P1/P2/P3)
4. Acceptance scenarios per story (Given-When-Then)
5. Functional requirements (FR-001 format)
6. Success criteria (SC-001 format, measurable metrics)
7. Key entities (if data involved)
8. Edge cases
9. Dependencies, assumptions, non-goals (if present)

**Example Analysis Output**:
```markdown
Feature: Photo Albums
Branch: 001-photo-albums
Stories: 3 (US1-P1, US2-P2, US3-P1)
Acceptance Scenarios: 9 total (4 for US1, 2 for US2, 3 for US3)
Requirements: 5 (FR-001 to FR-005)
Success Criteria: 3 (SC-001 to SC-003, all measurable)
Entities: 2 (Album, Photo)
Edge Cases: 2
```

### Step 2: Create Feature Content RDF File

**Output**: `examples/NNN-feature-name-content.ttl`

**Structure**:
```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix sk: <http://github.com/github/spec-kit#> .
@prefix : <http://github.com/github/spec-kit/examples/feature-name#> .

# Feature metadata
:feature-name a sk:Feature ;
    sk:featureBranch "NNN-feature-name" ;
    sk:featureName "Human Readable Feature Name" ;
    sk:created "YYYY-MM-DD"^^xsd:date ;
    sk:status "Draft" ;
    sk:userInput "Original user description..." ;
    sk:hasUserStory :us-001, :us-002, :us-003 ;
    sk:hasFunctionalRequirement :fr-001, :fr-002, :fr-003 ;
    sk:hasSuccessCriterion :sc-001, :sc-002, :sc-003 ;
    sk:hasEntity :entity1, :entity2 ;
    sk:hasEdgeCase :ec-001, :ec-002 .
```

### Step 3: Convert User Stories to RDF

For each user story in the markdown spec:

**Markdown Input**:
```markdown
### User Story 1 - Create and Manage Albums (Priority: P1)

A user wants to create photo albums to organize their photos...

**Why this priority**: This is the foundational capability...

**Independent Test**: Can be fully tested by creating an album...

**Acceptance Scenarios**:
1. **Given** the application is open, **When** user clicks Create, **Then** album appears
2. **Given** an album exists, **When** user clicks it, **Then** album opens
```

**RDF Output**:
```turtle
:us-001 a sk:UserStory ;
    sk:storyIndex 1 ;
    sk:title "Create and Manage Albums" ;
    sk:priority "P1" ;
    sk:priorityRationale "This is the foundational capability..." ;
    sk:independentTest "Can be fully tested by creating an album..." ;
    sk:description "A user wants to create photo albums to organize their photos..." ;
    sk:hasAcceptanceScenario :us-001-as-001, :us-001-as-002 .

:us-001-as-001 a sk:AcceptanceScenario ;
    sk:scenarioIndex 1 ;
    sk:given "the application is open on the main album view" ;
    sk:when "the user clicks 'Create Album' and enters a name" ;
    sk:then "a new empty album appears in the album list" .

:us-001-as-002 a sk:AcceptanceScenario ;
    sk:scenarioIndex 2 ;
    sk:given "an album exists in the album list" ;
    sk:when "the user clicks on the album" ;
    sk:then "the album opens showing an empty tile grid view" .
```

**Key Conversions**:
- Story title → `sk:title`
- Priority (P1/P2/P3) → `sk:priority`
- "Why this priority" → `sk:priorityRationale`
- "Independent Test" → `sk:independentTest`
- Story description → `sk:description`
- Given-When-Then → `sk:given`, `sk:when`, `sk:then`

### Step 4: Convert Functional Requirements to RDF

**Markdown Input**:
```markdown
- **FR-001**: System MUST allow users to create albums with a name
- **FR-002** (Album Management): System MUST display albums in a list
```

**RDF Output**:
```turtle
:fr-001 a sk:FunctionalRequirement ;
    sk:requirementId "FR-001" ;
    sk:description "System MUST allow users to create albums with a user-provided name and auto-generated creation date" .

:fr-002 a sk:FunctionalRequirement ;
    sk:requirementId "FR-002" ;
    sk:category "Album Management" ;
    sk:description "System MUST display albums in a main list view with album name and creation date visible" .
```

**Note**: Category is optional - include only if present in markdown.

### Step 5: Convert Success Criteria to RDF

**Markdown Input**:
```markdown
- **SC-001** (Time to create album: < 30 seconds for 10 photos): Users can create an album and add 10 photos quickly
- **SC-002**: 90% of users successfully organize photos without assistance
```

**RDF Output**:
```turtle
:sc-001 a sk:SuccessCriterion ;
    sk:criterionId "SC-001" ;
    sk:measurable true ;
    sk:metric "Time to create album and add photos" ;
    sk:target "< 30 seconds for 10 photos" ;
    sk:description "Users can create an album and add 10 photos in under 30 seconds" .

:sc-002 a sk:SuccessCriterion ;
    sk:criterionId "SC-002" ;
    sk:measurable true ;
    sk:metric "Task completion rate" ;
    sk:target ">= 90%" ;
    sk:description "90% of users successfully organize photos into albums without assistance on first attempt" .
```

**Extracting Metrics**:
- If markdown has format "(Metric: Target)", extract to `sk:metric` and `sk:target`
- If only description exists, set `sk:measurable = false`

### Step 6: Convert Entities to RDF

**Markdown Input**:
```markdown
### Key Entities

- **Album**: A container for organizing photos by date, event, or theme
  - Key attributes: name, creation date, display order
- **Photo**: An image file stored locally with metadata
  - Key attributes: file path, thumbnail, upload date
```

**RDF Output**:
```turtle
:album a sk:Entity ;
    sk:entityName "Album" ;
    sk:definition "A container for organizing photos by date, event, or theme" ;
    sk:keyAttributes "name (user-provided), creation date (auto-generated), display order (user-customizable), photo count" .

:photo a sk:Entity ;
    sk:entityName "Photo" ;
    sk:definition "An image file stored locally with metadata for display and organization" ;
    sk:keyAttributes "file path, thumbnail image, full-size image, upload date, parent album reference" .
```

### Step 7: Convert Edge Cases, Dependencies, Assumptions

**Edge Cases**:
```turtle
:ec-001 a sk:EdgeCase ;
    sk:scenario "User attempts to create an album with an empty name" ;
    sk:expectedBehavior "System shows an error message 'Album name is required' and prevents creation" .
```

**Assumptions**:
```turtle
:a-001 a sk:Assumption ;
    sk:description "Photos are stored locally on the user's device, not uploaded to a server" .
```

**Dependencies** (if present):
```turtle
:d-001 a sk:Dependency ;
    sk:description "Requires drag-and-drop library for browser compatibility" .
```

### Step 8: Validate with SHACL

**Run SHACL validation**:
```bash
cd vendors/spec-kit-3t
ggen sync --validate-only
```

**Expected Output**:
```
✓ Feature branch matches pattern ^[0-9]{3}-[a-z0-9-]+$
✓ All priorities are P1, P2, or P3
✓ All requirement IDs match ^FR-[0-9]{3}$
✓ All success criterion IDs match ^SC-[0-9]{3}$
✓ All user stories have at least one acceptance scenario
✓ All SHACL shapes pass validation
```

**Common Errors**:
- ❌ Priority "HIGH" → Fix: Change to "P1"
- ❌ Requirement ID "REQ-1" → Fix: Change to "FR-001"
- ❌ User story without scenarios → Fix: Add at least one acceptance scenario
- ❌ Missing required fields → Fix: Add `sk:description`, `sk:title`, etc.

### Step 9: Generate and Compare

**Generate markdown from RDF**:
```bash
ggen sync
```

**Compare to original**:
```bash
# Original markdown
cat specs/001-photo-albums/spec.md

# Generated markdown
cat vendors/spec-kit-3t/generated/spec-header.md
cat vendors/spec-kit-3t/generated/user-stories.md
cat vendors/spec-kit-3t/generated/requirements.md
cat vendors/spec-kit-3t/generated/success-criteria.md
```

**Check for**:
- All user stories present with correct titles and priorities
- All acceptance scenarios (Given-When-Then) captured
- All functional requirements (FR-001 to FR-XXX) present
- All success criteria (SC-001 to SC-XXX) present with metrics
- All entities, edge cases, assumptions present

### Step 10: Iterate Until Content-Equivalent

If generated markdown is missing content or has incorrect formatting:

1. **Check SPARQL queries** in `ggen.toml`:
   - Verify query selects the correct properties
   - Ensure `ORDER BY` for deterministic output

2. **Check Tera templates**:
   - Verify template renders all extracted bindings
   - Check for missing template variables

3. **Check RDF triples**:
   - Verify all content exists in `.ttl` file
   - Check property names match schema

4. **Regenerate**:
   ```bash
   ggen sync
   ```

5. **Verify idempotence**:
   ```bash
   ggen sync  # Second run should produce no changes
   git status generated/  # Should show no modifications
   ```

## Common Migration Patterns

### Pattern 1: Markdown Lists → RDF Instances

**Markdown**:
```markdown
- **FR-001**: System MUST do X
- **FR-002**: System MUST do Y
- **FR-003**: System MUST do Z
```

**RDF**:
```turtle
:feature sk:hasFunctionalRequirement :fr-001, :fr-002, :fr-003 .

:fr-001 a sk:FunctionalRequirement ;
    sk:requirementId "FR-001" ;
    sk:description "System MUST do X" .

:fr-002 a sk:FunctionalRequirement ;
    sk:requirementId "FR-002" ;
    sk:description "System MUST do Y" .

:fr-003 a sk:FunctionalRequirement ;
    sk:requirementId "FR-003" ;
    sk:description "System MUST do Z" .
```

### Pattern 2: Nested Lists → Object Properties

**Markdown**:
```markdown
### User Story 1
**Acceptance Scenarios**:
1. Given A, When B, Then C
2. Given D, When E, Then F
```

**RDF**:
```turtle
:us-001 sk:hasAcceptanceScenario :us-001-as-001, :us-001-as-002 .

:us-001-as-001 a sk:AcceptanceScenario ;
    sk:scenarioIndex 1 ;
    sk:given "A" ; sk:when "B" ; sk:then "C" .

:us-001-as-002 a sk:AcceptanceScenario ;
    sk:scenarioIndex 2 ;
    sk:given "D" ; sk:when "E" ; sk:then "F" .
```

### Pattern 3: Inline Metadata → Separate Properties

**Markdown**:
```markdown
### User Story 1 - Title Here (Priority: P1)
```

**RDF**:
```turtle
:us-001 a sk:UserStory ;
    sk:storyIndex 1 ;
    sk:title "Title Here" ;
    sk:priority "P1" .
```

## Troubleshooting Common Issues

### Issue 1: SHACL Validation Fails with "pattern violation"

**Error**:
```
Violation: featureBranch must match pattern ^[0-9]{3}-[a-z0-9-]+$
Value: "v6-photo-albums"
```

**Fix**: Ensure branch name starts with 3 digits:
```turtle
sk:featureBranch "001-photo-albums" ;  # Not "v6-photo-albums"
```

### Issue 2: Generated markdown missing user stories

**Symptom**: `generated/user-stories.md` is empty

**Debug**:
1. Check SPARQL query returns results:
   ```sparql
   PREFIX sk: <http://github.com/github/spec-kit#>
   SELECT * WHERE { ?story a sk:UserStory }
   ```

2. Verify feature has `sk:hasUserStory` relationships:
   ```turtle
   :photo-albums sk:hasUserStory :us-001, :us-002 .
   ```

3. Verify user story instances exist and have required properties:
   ```turtle
   :us-001 a sk:UserStory ;
       sk:storyIndex 1 ;
       sk:title "..." ;
       sk:priority "P1" ;
       sk:description "..." .
   ```

### Issue 3: Acceptance scenarios not grouping under user stories

**Symptom**: Each scenario appears as separate story in generated markdown

**Cause**: Missing `ORDER BY ?storyIndex ?scenarioIndex` in SPARQL query

**Fix**: Add ORDER BY clause in `ggen.toml`:
```toml
query = """
...
WHERE { ... }
ORDER BY ?storyIndex ?scenarioIndex
"""
```

## Migration Checklist

**Before starting migration**:
- [ ] Backup original markdown specification
- [ ] Read and understand markdown spec structure
- [ ] Review RDF/Turtle syntax basics
- [ ] Install ggen v6 CLI with `ggen sync` command

**During migration**:
- [ ] Feature metadata converted (branch, name, date, status)
- [ ] All user stories converted with correct priorities
- [ ] All acceptance scenarios converted (Given-When-Then)
- [ ] All functional requirements converted (FR-XXX pattern)
- [ ] All success criteria converted with metrics (SC-XXX pattern)
- [ ] All entities converted with definitions
- [ ] All edge cases converted
- [ ] All dependencies/assumptions converted
- [ ] SHACL validation passes (no errors)

**After migration**:
- [ ] Generated markdown content-equivalent to original
- [ ] Idempotence verified (second `ggen sync` produces no changes)
- [ ] Cryptographic receipt generated successfully
- [ ] Original markdown archived or marked deprecated

## Best Practices

1. **Start with a simple feature**: Choose a feature with 2-3 user stories for first migration
2. **Validate early**: Run SHACL validation after each section (stories, requirements, etc.)
3. **Use consistent naming**: URI patterns like `:us-001`, `:fr-001`, `:sc-001`
4. **Preserve ordering**: Use `storyIndex`, `scenarioIndex` for deterministic output
5. **Test idempotence**: Always run `ggen sync` twice to verify μ∘μ = μ
6. **Compare carefully**: Use diff tools to compare original vs generated markdown

## Automated Migration Script (Future Work)

**Potential automation**:
```bash
#!/usr/bin/env bash
# migrate-spec-to-rdf.sh - Convert markdown spec to RDF

SPEC_FILE=$1
OUTPUT_TTL=$2

# Parse markdown spec
# Extract user stories, requirements, success criteria
# Generate RDF triples
# Run SHACL validation
# Generate markdown and compare

# Status: NOT YET IMPLEMENTED (manual migration for now)
```

## Next Steps After Migration

1. ✅ Feature specification migrated to RDF
2. Run `ggen sync` to generate specification
3. Verify idempotence with second `ggen sync`
4. Commit `.ttl` files to version control
5. Add `generated/` to `.gitignore`
6. Update team documentation on RDF-based workflow
7. Use as template for migrating other features

---

**Questions or issues?** Open an issue at https://github.com/github/spec-kit/issues
