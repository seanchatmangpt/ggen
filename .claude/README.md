# .claude/ Configuration

This directory contains Claude Code configuration for the ggen project.

**See `/CLAUDE.md` for all project rules, stack, and paradigms.** This is the single source of truth.

## Directory Structure

- `settings.json` — Permissions, hooks, MCP stubs (loaded during sessions)
- `skills/` — Custom skills (bb80-*, cargo-make-protocol, chicago-tdd-pattern, poka-yoke-patterns, rdf-ontologies, mcp-servers)

## Reference Materials

This directory originally contained extended reference docs (poka-yoke-implementation.md, etc.). These are now consolidated into CLAUDE.md. Details can be elaborated in specs (.specify/*.ttl) if needed.

All files in .claude/ except settings.json and skills/ are **not in the critical context path** and can be removed to preserve token budget.
