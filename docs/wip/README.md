<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Work-In-Progress Documentation](#work-in-progress-documentation)
  - [Purpose](#purpose)
  - [When to Use](#when-to-use)
  - [When to DELETE (Critical!)](#when-to-delete-critical)
  - [File Naming Convention](#file-naming-convention)
  - [Document Lifecycle](#document-lifecycle)
  - [Best Practices](#best-practices)
  - [Maintenance Schedule](#maintenance-schedule)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Work-In-Progress Documentation

This directory contains work-in-progress and intermediate documentation reports that are part of ongoing improvement processes (Kaizen, Mura elimination, Muda elimination, etc.).

## Purpose

The `wip/` directory serves as a staging area for:
- **Temporary reports**: Intermediate analysis and inventory documents
- **Action lists**: Task tracking and improvement plans
- **Current state snapshots**: Documents tracking ongoing work (e.g., `*_CURRENT.md` files)

**NOT for**: Historical reports, completed work summaries, or finished documentation

## When to Use

Place documentation here when:
- It's an intermediate step in a workflow (Mura inventory, Muda analysis, Gemba walks)
- It tracks current state that changes frequently (`*_CURRENT.md`)
- It's a temporary report that will be superseded
- It's actively being updated or referenced

## When to DELETE (Critical!)

Remove files from `wip/` when:
- ✅ **Work is complete** - Improvements have been applied to codebase
- ✅ **Document is superseded** - A newer `*_CURRENT.md` version exists
- ✅ **No longer referenced** - Report served its purpose, work is done

**DO NOT LET WASTE ACCUMULATE**: Delete completed work reports immediately.

## File Naming Convention

- Use `*_CURRENT.md` suffix for active tracking documents
  - Example: `KAIZEN_IMPROVEMENT_CURRENT.md`, `MURA_INVENTORY_CURRENT.md`
  - These are the source of truth and should be kept updated
  - Remove the previous version when creating a new one
- Use descriptive names that indicate purpose and date if multiple versions
- **Remove outdated versions immediately** - don't let duplicates accumulate

## Document Lifecycle

```
1. Create: Initial analysis/report created
   ↓
2. Update: Active tracking and iteration (use *_CURRENT.md)
   ↓
3. Complete: Work finished, improvements applied to codebase
   ↓
4. DELETE: Remove the completed work report from wip/
   ↓
5. Archive: If needed for historical reference, move to git history
```

## Best Practices

- **Keep clean**: Remove files when work is complete
- **No dumping ground**: This is NOT a permanent archive
- **One version per document**: Remove old versions when creating new ones
- **Active management**: Monthly review of `wip/` directory
- **Link to code**: When removing a report, the work should be visible in the code itself
- **Communicate deletion**: In commit message, explain what work was completed

## Maintenance Schedule

- **Daily**: Update active tracking documents (`*_CURRENT.md`)
- **Weekly**: Review for obvious completed work to remove
- **Monthly**: Full audit of all documents in `wip/`
  - Check if `*_CURRENT.md` files are still needed
  - Remove completed analysis and report documents
  - Consolidate information if multiple versions exist

