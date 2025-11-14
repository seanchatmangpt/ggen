# Work-In-Progress Documentation

This directory contains work-in-progress and intermediate documentation reports that are part of ongoing improvement processes.

## Purpose

The `wip/` directory serves as a staging area for:
- **Temporary reports**: Intermediate analysis and inventory documents
- **Action lists**: Task tracking and improvement plans
- **Current state snapshots**: Documents tracking ongoing work (e.g., `*_CURRENT.md` files)
- **Historical reports**: Completed work summaries that may be referenced but aren't core documentation

## When to Use

Place documentation here when:
- It's an intermediate step in a workflow (e.g., Mura inventory, Muda analysis)
- It tracks current state that changes frequently
- It's a temporary report that will be superseded
- It's a work-in-progress document that may be updated

## When to Archive

Move or remove files from `wip/` when:
- Work is complete and documented in final sources
- The document is superseded by a newer version
- The information is consolidated into permanent documentation

## File Naming

- Use `*_CURRENT.md` suffix for active tracking documents
- Use descriptive names that indicate the document's purpose
- Remove outdated versions when creating new ones

## Best Practices

- Keep `wip/` organized and clean
- Remove files when work is complete
- Don't let `wip/` become a permanent dumping ground
- Reference `wip/` files from `.cursor/commands` using `wip/FILENAME.md` path

