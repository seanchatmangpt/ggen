<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern 009: PROJECT PLAN](#pattern-009-project-plan)
  - [Problem](#problem)
  - [Solution](#solution)
  - [Prerequisites](#prerequisites)
  - [Step-by-Step](#step-by-step)
    - [1. Create a Plan from a Single Template](#1-create-a-plan-from-a-single-template)
    - [2. Review the Plan](#2-review-the-plan)
    - [3. Create Plans for Multiple Templates](#3-create-plans-for-multiple-templates)
  - [Complete Example](#complete-example)
  - [Explanation](#explanation)
    - [How Planning Works](#how-planning-works)
    - [Plan File Structure](#plan-file-structure)
    - [Key Features](#key-features)
  - [Expected Output](#expected-output)
  - [Common Pitfalls](#common-pitfalls)
  - [Variations](#variations)
    - [💡 Plan with Environment-Specific Variables](#-plan-with-environment-specific-variables)
    - [💡 Plan for Multiple Templates in Sequence](#-plan-for-multiple-templates-in-sequence)
    - [💡 Plan with RDF Data](#-plan-with-rdf-data)
  - [Troubleshooting](#troubleshooting)
  - [See Also](#see-also)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern 009: PROJECT PLAN

**Difficulty**: ⭐⭐ Intermediate
**Time**: 15min
**Tags**: #workflow #planning #dry-run #preview

## Problem

You want to preview what files will be generated before actually creating them, similar to a database migration plan or Terraform plan. This helps you verify the changes, review them with your team, or stage the generation for later execution.

## Solution

Use `ggen project plan` to create a deterministic plan file that captures all the templates, variables, and intended outputs without actually modifying any files. The plan can then be reviewed, versioned, and applied later.

## Prerequisites

- ggen installed (v0.2.4+)
- A project with templates (or gpacks installed)
- Basic understanding of template generation

## Step-by-Step

### 1. Create a Plan from a Single Template

```bash
# Generate a plan for a single template
ggen project plan templates/rust.tmpl \
  --vars name=MyModule \
  --output-plan plans/my-module.plan.json
```

This creates a JSON plan file that contains:
- Template to be rendered
- All variables and their values
- Expected output paths
- Content that would be generated (hashed for verification)

### 2. Review the Plan

```bash
# View the plan in human-readable format
cat plans/my-module.plan.json | jq .

# Or use ggen's built-in plan viewer (if available)
ggen project show-plan plans/my-module.plan.json
```

### 3. Create Plans for Multiple Templates

```bash
# Plan multiple related templates
ggen project plan io.ggen.rust.api-endpoint:*.tmpl \
  --vars resource=User endpoint=users method=GET \
  --output-plan plans/user-api.plan.json
```

## Complete Example

**Scenario**: You're scaffolding a new CLI subcommand and want to preview all changes before applying them.

**Input Template** (`templates/cli-subcommand.tmpl`):
```yaml
---
to: "src/cmds/{{cmd}}.rs"
unless_exists: true
vars:
  cmd: "example"
  summary: "Example command"
---
use clap::Args;
use anyhow::Result;

#[derive(Args, Debug)]
pub struct {{cmd | title}}Args {
    // Add arguments here
}

pub async fn run(args: &{{cmd | title}}Args) -> Result<()> {
    println!("{{summary}}");
    Ok(())
}
```

**Command**:
```bash
# Create a plan
ggen project plan templates/cli-subcommand.tmpl \
  --vars cmd=hello summary="Print greeting message" \
  --output-plan plans/hello-cmd.plan.json
```

**Generated Plan** (`plans/hello-cmd.plan.json`):
```json
{
  "version": "1.0",
  "created_at": "2025-10-09T22:00:00Z",
  "templates": [
    {
      "template": "templates/cli-subcommand.tmpl",
      "variables": {
        "cmd": "hello",
        "summary": "Print greeting message"
      },
      "outputs": [
        {
          "path": "src/cmds/hello.rs",
          "action": "create",
          "content_hash": "sha256:abc123...",
          "unless_exists": true
        }
      ]
    }
  ],
  "metadata": {
    "total_files": 1,
    "determinism_seed": 42
  }
}
```

**Verification**:
```bash
# Verify plan is valid
ggen project validate-plan plans/hello-cmd.plan.json

# Preview what will be created
ggen project diff --plan plans/hello-cmd.plan.json
```

## Explanation

### How Planning Works

1. **Template Resolution**: ggen resolves template paths, loads frontmatter, and merges variables
2. **Dry Execution**: Templates are rendered in memory without writing files
3. **Plan Generation**: Output paths, content hashes, and metadata are serialized to JSON
4. **Deterministic**: Same inputs always produce the same plan (reproducible builds)

### Plan File Structure

```json
{
  "version": "1.0",                    // Plan format version
  "created_at": "ISO-8601 timestamp",   // When plan was created
  "templates": [                        // Array of templates to process
    {
      "template": "path/to/template",   // Template source
      "variables": { ... },             // Merged variables
      "outputs": [                      // Expected outputs
        {
          "path": "output/file.rs",     // Where file will be created
          "action": "create|inject|skip", // What will happen
          "content_hash": "sha256:...", // Verify content integrity
          "skip_if": "pattern",         // Idempotency check
          "unless_exists": true         // Only create if missing
        }
      ]
    }
  ],
  "metadata": {                         // Plan metadata
    "total_files": 1,
    "determinism_seed": 42
  }
}
```

### Key Features

- **Idempotent**: Re-running the plan with same inputs produces same output
- **Versionable**: Check plans into git for review
- **Portable**: Share plans across teams
- **Auditable**: See exactly what will change before applying
- **Deterministic**: Content hashes ensure reproducibility

## Expected Output

```
✅ Plan created: plans/hello-cmd.plan.json
📊 Summary:
   Templates: 1
   New files: 1
   Modified files: 0
   Skipped files: 0

💡 Next steps:
   Review plan: ggen project diff --plan plans/hello-cmd.plan.json
   Apply plan:  ggen project apply plans/hello-cmd.plan.json
```

## Common Pitfalls

❌ **Mistake**: Planning without `--output-plan` flag
- **Symptom**: Plan printed to stdout instead of saved
- **Fix**: Always use `--output-plan` to save plans to disk

❌ **Mistake**: Not checking plan validity before applying
- **Symptom**: Apply fails with validation errors
- **Fix**: Run `ggen project validate-plan` first

❌ **Mistake**: Forgetting to version control plans
- **Symptom**: Lost plan history, can't reproduce old generations
- **Fix**: Commit `plans/*.plan.json` to git

❌ **Mistake**: Manually editing plan files
- **Symptom**: Content hashes don't match, apply fails
- **Fix**: Regenerate plans instead of editing by hand

## Variations

### 💡 Plan with Environment-Specific Variables

```bash
# Development plan
ggen project plan templates/config.tmpl \
  --vars env=dev db_host=localhost \
  --output-plan plans/config-dev.plan.json

# Production plan
ggen project plan templates/config.tmpl \
  --vars env=prod db_host=prod.db.example.com \
  --output-plan plans/config-prod.plan.json
```

### 💡 Plan for Multiple Templates in Sequence

```bash
# Create a comprehensive plan for full feature
ggen project plan templates/{model,controller,tests}.tmpl \
  --vars resource=User \
  --output-plan plans/user-feature.plan.json
```

### 💡 Plan with RDF Data

```bash
# Plan generation using SPARQL queries
ggen project plan templates/api-routes.tmpl \
  --rdf graphs/api-schema.ttl \
  --output-plan plans/routes.plan.json
```

## Troubleshooting

**Issue**: Plan contains no outputs
**Cause**: Template conditions or `unless_exists` preventing generation
**Solution**: Check template frontmatter, verify file existence checks

**Issue**: Plan content hashes change between runs
**Cause**: Non-deterministic template (timestamps, random values)
**Solution**: Set `determinism: <seed>` in template frontmatter

**Issue**: Plan validation fails
**Cause**: Template changed after plan was created
**Solution**: Regenerate plan to match current template state

## See Also

- Pattern 010: Idempotent Apply - Safely apply plans multiple times
- Pattern 011: Dry-Run Diff - Preview changes with unified diff
- Pattern 012: CI Drift Check - Detect configuration drift in CI
- Recipe 6.1: Reproducible Output - Deterministic generation
- Recipe 11.2: Project Structure - `.ggen` directory and plans

## Next Steps

- Review your plan with `ggen project diff --plan <plan-file>`
- Apply the plan with `ggen project apply <plan-file>`
- Check plan into version control for team review
- Automate plan generation in CI/CD pipelines
