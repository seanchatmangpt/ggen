---
description: "Validate RDF specification files (.ttl) in .specify/ directory. Checks SHACL constraints, generates markdown from TTL, and verifies feature completeness. Use before implementation."
allowed_tools: "Bash(cargo make speckit:*), Read, Grep, Glob"
argument_hint: "[feature-number] [check-type]"
---

# Speckit Check Command

Validate RDF-first specification system compliance and completeness.

## Check Types

- **syntax**: Verify TTL files are valid RDF
- **shacl**: Validate against SHACL shape constraints
- **completeness**: Ensure all required fields present
- **generation**: Regenerate markdown from TTL sources
- **evidence**: Verify evidence directory structure
- **all**: Run all checks (default)

## Specification Structure Checked

```
.specify/specs/NNN-feature/
├── feature.ttl (user stories, requirements)
├── entities.ttl (domain entities)
├── plan.ttl (architecture decisions)
├── tasks.ttl (task breakdown)
├── evidence/ (test results, artifacts)
└── Generated markdown (spec.md, plan.md, tasks.md)
```

## SHACL Validation Rules

✓ Priority must be "P1", "P2", or "P3" (not "HIGH", "LOW")
✓ All required fields present (title, description, acceptance scenarios)
✓ Minimum 1 acceptance scenario per user story
✓ Valid RDF syntax and Turtle structure
✓ No broken references between ontologies

## RDF-First Principle

- **Source of Truth**: .ttl files (edit these)
- **Derived Artifacts**: .md files (regenerate, never edit)
- **Constitutional Equation**: spec.md = μ(feature.ttl)

## Workflow

1. **Syntax Check**
   ```
   cargo make speckit-check
   ```

2. **SHACL Validation**
   ```
   cargo make speckit-validate
   ```

3. **Regenerate Markdown**
   ```
   cargo make speckit-render
   ```

4. **Verify Evidence**
   - Check `.specify/specs/$1/evidence/` exists
   - Verify test results and artifacts present
   - Link to implementation commits

## Common Issues & Fixes

| Issue | Cause | Fix |
|-------|-------|-----|
| Invalid RDF | Syntax error in TTL | Fix turtle syntax |
| Missing priority | User story incomplete | Add priority field |
| Broken references | Entity doesn't exist | Update ontology |
| No evidence | Tests not run | Run cargo make test, save results |

## Success Criteria

✓ All TTL files pass syntax validation
✓ All SHACL constraints satisfied
✓ Markdown generated from TTL without errors
✓ Evidence directory populated
✓ Feature ready for implementation
