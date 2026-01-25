<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Claude Code Web Best Practices (2026 Bleeding Edge)](#claude-code-web-best-practices-2026-bleeding-edge)
  - [1. HOOKS (Lifecycle Automation)](#1-hooks-lifecycle-automation)
    - [Hook Types](#hook-types)
    - [PreToolUse Permission Control](#pretooluse-permission-control)
    - [PostToolUse Auto-Actions](#posttooluse-auto-actions)
    - [Best Practices](#best-practices)
  - [2. MCP SERVERS (External Tool Integration)](#2-mcp-servers-external-tool-integration)
    - [Architecture](#architecture)
    - [Configuration](#configuration)
    - [Top MCP Servers (2026)](#top-mcp-servers-2026)
    - [Best Practices](#best-practices-1)
  - [3. SLASH COMMANDS (Custom Workflows)](#3-slash-commands-custom-workflows)
    - [Creating Commands](#creating-commands)
    - [Command Locations](#command-locations)
    - [Parameters](#parameters)
    - [Best Practices](#best-practices-2)
  - [4. MEMORY & CONTEXT (CLAUDE.md)](#4-memory--context-claudemd)
    - [File Locations (Priority Order)](#file-locations-priority-order)
    - [What to Include](#what-to-include)
    - [Best Practices](#best-practices-3)
  - [5. AGENT SDK (Subagent Patterns)](#5-agent-sdk-subagent-patterns)
    - [Parallel Execution Pattern](#parallel-execution-pattern)
    - [Orchestrator Pattern](#orchestrator-pattern)
    - [Custom Subagent Definition](#custom-subagent-definition)
    - [Best Practices](#best-practices-4)
  - [6. PERMISSIONS & SECURITY](#6-permissions--security)
    - [Permission Modes](#permission-modes)
    - [Configuration](#configuration-1)
    - [Best Practices](#best-practices-5)
  - [7. GIT WORKFLOWS](#7-git-workflows)
    - [Commit Patterns](#commit-patterns)
    - [PR Creation](#pr-creation)
    - [Best Practices](#best-practices-6)
  - [8. TESTING PATTERNS](#8-testing-patterns)
    - [Chicago TDD (State-Based)](#chicago-tdd-state-based)
    - [Test Organization](#test-organization)
    - [Best Practices](#best-practices-7)
  - [9. PERFORMANCE & TOKEN EFFICIENCY](#9-performance--token-efficiency)
    - [Context Window Management](#context-window-management)
    - [Token Optimization Strategies](#token-optimization-strategies)
    - [Best Practices](#best-practices-8)
  - [10. SKILLS SYSTEM (Advanced Features)](#10-skills-system-advanced-features)
    - [Skill Structure](#skill-structure)
    - [SKILL.md Format](#skillmd-format)
    - [Skill vs Other Options](#skill-vs-other-options)
    - [Pre-built Skills (2026)](#pre-built-skills-2026)
    - [Best Practices](#best-practices-9)
  - [SYNTHESIS: The Gestalt](#synthesis-the-gestalt)
    - [The 2026 Claude Code Web Stack](#the-2026-claude-code-web-stack)
    - [Key Principles (Bruce Lee Style)](#key-principles-bruce-lee-style)
    - [Quick Reference](#quick-reference)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Claude Code Web Best Practices (2026 Bleeding Edge)

**Synthesized Gestalt from 10-Domain Research**

---

## 1. HOOKS (Lifecycle Automation)

### Hook Types

| Hook | Trigger | Use Case |
|------|---------|----------|
| **PreToolUse** | Before tool executes | Block dangerous ops, validate inputs |
| **PostToolUse** | After tool completes | Auto-format, validate outputs |
| **SessionStart** | New session begins | Environment setup, context loading |
| **UserPromptSubmit** | User sends message | Validate instructions |
| **Stop** | Session ends | Cleanup, save state |

### PreToolUse Permission Control

```json
{
  "hooks": {
    "PreToolUse": [{
      "matcher": "Bash",
      "hooks": [{
        "type": "command",
        "command": "validate-command.sh"
      }]
    }]
  }
}
```

**Permission decisions**: `allow` (bypass), `deny` (block + feedback to Claude), `ask` (prompt user)

### PostToolUse Auto-Actions

```json
{
  "hooks": {
    "PostToolUse": [{
      "matcher": "Edit|Write",
      "hooks": [{
        "type": "command",
        "command": "prettier --write \"$TOOL_INPUT_FILE\""
      }]
    }]
  }
}
```

### Best Practices
- Log all Bash commands for audit trails
- Auto-format after file edits
- Validate writes against protected paths
- Initialize sessions with environment checks

**Sources**: [Hooks Reference](https://code.claude.com/docs/en/hooks), [Hooks Guide](https://docs.claude.com/en/docs/claude-code/hooks-guide)

---

## 2. MCP SERVERS (External Tool Integration)

### Architecture

Claude Code acts as both MCP **client** (connects to servers) and **server** (exposes tools).

### Configuration

```json
{
  "mcpServers": {
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"],
      "env": { "GITHUB_TOKEN": "${GITHUB_TOKEN}" }
    },
    "filesystem": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-filesystem", "/path"]
    }
  }
}
```

### Top MCP Servers (2026)

| Server | Purpose |
|--------|---------|
| **GitHub** | Issues, PRs, CI/CD workflows |
| **Figma** | Design-to-code extraction |
| **TaskMaster** | PRD → task breakdown |
| **Filesystem** | Scoped file access |
| **PostgreSQL/SQLite** | Database operations |
| **Puppeteer** | Browser automation |

### Best Practices
- Start with **read-only** servers (docs, search)
- Scope each server to **narrow blast radius**
- Use **per-project API keys**
- Log all MCP tool calls
- Monitor with `/context` - remove unused servers

**Sources**: [Best MCP Servers 2026](https://www.builder.io/blog/best-mcp-servers-2026), [Top 10 MCP Servers](https://apidog.com/blog/top-10-mcp-servers-for-claude-code/)

---

## 3. SLASH COMMANDS (Custom Workflows)

### Creating Commands

```markdown
<!-- .claude/commands/fix-issue.md -->
---
description: Fix GitHub issue by number
allowed-tools: Read, Edit, Bash, Task
---

# Fix Issue $ARGUMENTS

1. Fetch issue details from GitHub
2. Analyze the codebase for related files
3. Implement the fix
4. Write tests
5. Create PR
```

### Command Locations

| Location | Scope | Prefix |
|----------|-------|--------|
| `.claude/commands/` | Project | `/project:` |
| `~/.claude/commands/` | Personal | `/user:` |
| MCP servers | Dynamic | `/mcp:` |

### Parameters

- `$ARGUMENTS` - All arguments as string
- `$1`, `$2`, etc. - Positional arguments
- `$SELECTION` - Currently selected text

### Best Practices
- Use frontmatter for `description`, `allowed-tools`, `model`
- Document expected arguments in command body
- Reference commands by name in CLAUDE.md for auto-discovery
- Run `/help` to see all available commands

**Sources**: [Slash Commands Docs](https://code.claude.com/docs/en/slash-commands), [Commands Collection](https://github.com/wshobson/commands)

---

## 4. MEMORY & CONTEXT (CLAUDE.md)

### File Locations (Priority Order)

1. `.claude/CLAUDE.md` (project-specific)
2. `./CLAUDE.md` (repo root)
3. `~/.claude/CLAUDE.md` (global defaults)

Child directories inherit and can override parent CLAUDE.md files.

### What to Include

```markdown
# Project Name

## Build Commands
- `cargo make check` - Fast compilation
- `cargo make test` - Run all tests

## Code Conventions
- Use Result<T, E> for error handling
- Never use unwrap() in production

## Forbidden Actions
- Never edit files in /protected/
- Always run tests before commits

## Context
- Main entry: src/main.rs
- Config: ggen.toml
```

### Best Practices
- Keep CLAUDE.md **short and always-true**
- Document **repository etiquette** (branching, merging)
- Specify **forbidden directories**
- Include **essential commands** only
- Use `/init` to auto-generate initial CLAUDE.md

**Sources**: [Claude Code Customization Guide](https://alexop.dev/posts/claude-code-customization-guide-claudemd-skills-subagents/), [Best Practices](https://www.anthropic.com/engineering/claude-code-best-practices)

---

## 5. AGENT SDK (Subagent Patterns)

### Parallel Execution Pattern

```javascript
// Launch 4 parallel agents for codebase exploration
Task("explore-1", "Explore src/ directory", "Explore")
Task("explore-2", "Explore tests/ directory", "Explore")
Task("explore-3", "Explore docs/ directory", "Explore")
Task("explore-4", "Explore config files", "Explore")
```

### Orchestrator Pattern

```
Main Agent (orchestrator)
  ├── Analyst subagent (research)
  ├── Architect subagent (design)
  ├── Implementer subagent (code)
  ├── Tester subagent (validation)
  └── Security subagent (audit)
```

### Custom Subagent Definition

```markdown
<!-- .claude/agents/rust-coder.md -->
---
name: rust-coder
description: Specialized Rust implementation agent
tools: Read, Edit, Write, Bash(cargo make *)
model: sonnet
skills: cargo-make-protocol, chicago-tdd-pattern
---

You are a Rust implementation specialist...
```

### Best Practices
- **One job per subagent** - clear inputs/outputs
- **Orchestrator handles planning** - subagents execute
- **Parallelize safe operations** - serialize risky ones
- **Use for context isolation** - each gets fresh 200K window
- Subagents **cannot spawn subagents**

**Sources**: [Subagents in SDK](https://platform.claude.com/docs/en/agent-sdk/subagents), [Parallel Development](https://zachwills.net/how-to-use-claude-code-subagents-to-parallelize-development/)

---

## 6. PERMISSIONS & SECURITY

### Permission Modes

| Mode | Behavior |
|------|----------|
| **default** | Ask for each tool use |
| **allowlist** | Pre-approve specific tools |
| **full-auto** | Trust all operations |

### Configuration

```json
{
  "permissions": {
    "allow": [
      "Read(**)",
      "Bash(cargo make *)",
      "Edit(src/**)"
    ],
    "deny": [
      "Bash(rm -rf *)",
      "Write(.env*)",
      "Edit(Cargo.lock)"
    ]
  }
}
```

### Best Practices
- Start **restrictive**, loosen as needed
- **Never allow** `rm -rf`, force push, credential files
- Use **glob patterns** for scoped access
- Enable **audit logging** via hooks
- Review tool calls with `/context`

**Sources**: [Full Stack Guide](https://alexop.dev/posts/understanding-claude-code-full-stack/)

---

## 7. GIT WORKFLOWS

### Commit Patterns

```bash
# Check status first
git status && git diff

# Stage intentionally
git add -p  # Interactive staging

# Commit with conventional format
git commit -m "feat(module): add feature X"

# Push with tracking
git push -u origin feature-branch
```

### PR Creation

```bash
gh pr create --title "feat: Add X" --body "$(cat <<'EOF'
## Summary
- Added feature X
- Updated tests

## Test Plan
- [ ] Unit tests pass
- [ ] Integration tests pass
EOF
)"
```

### Best Practices
- **Never force push** to main/master
- **Never amend** others' commits
- Use **conventional commits** (feat, fix, docs, refactor)
- Create **focused PRs** (one feature per PR)
- Include **test plan** in PR description

---

## 8. TESTING PATTERNS

### Chicago TDD (State-Based)

```rust
#[test]
fn test_cache_stores_value() {
    // Arrange: Real objects
    let mut cache = Cache::new();

    // Act: Call public API
    cache.insert("key", "value");

    // Assert: Verify observable state
    assert_eq!(cache.get("key"), Some("value"));
}
```

### Test Organization

| Type | Location | Purpose |
|------|----------|---------|
| Unit | `src/*.rs` (#[cfg(test)]) | Fast, focused |
| Integration | `tests/*.rs` | Cross-module |
| E2E | `e2e/tests/*.rs` | Full system |
| Benchmark | `benches/*.rs` | Performance |

### Best Practices
- **Real objects** over mocks (Chicago TDD)
- **One assertion per concept**
- **Descriptive test names** (`test_cache_evicts_lru_on_full`)
- **Mutation testing** for quality assurance
- Tests SHOULD use `unwrap()` - fail fast on setup errors

---

## 9. PERFORMANCE & TOKEN EFFICIENCY

### Context Window Management

```bash
# Check current usage
/context

# Compact when >80% full
/compact

# Monitor MCP server usage
# Remove unused servers
```

### Token Optimization Strategies

| Strategy | Savings |
|----------|---------|
| Programmatic Tool Calling | ~37% |
| Compact files | ~20% |
| Scoped CLAUDE.md | ~15% |
| Session chunking at 80% | Prevents degradation |

### Best Practices
- **Check `/context` often**
- **Exit at 80%** context usage for complex tasks
- **Use subagents** for context isolation
- **Specify forbidden directories** in CLAUDE.md
- **Break large files** into focused modules

**Sources**: [Token Optimization](https://claudelog.com/faqs/how-to-optimize-claude-code-token-usage/), [Context Management](https://claudefa.st/blog/guide/mechanics/context-management)

---

## 10. SKILLS SYSTEM (Advanced Features)

### Skill Structure

```
.claude/skills/my-skill/
├── SKILL.md           # Required: name, description, instructions
├── reference.md       # Optional: detailed reference
├── examples/          # Optional: code examples
└── templates/         # Optional: file templates
```

### SKILL.md Format

```markdown
---
name: my-skill
description: Short description for auto-discovery
allowed_tools: Read, Write, Bash(specific-command)
---

# My Skill

Detailed instructions that load when skill activates...
```

### Skill vs Other Options

| Feature | Activation | Use Case |
|---------|------------|----------|
| **Skill** | Auto (context) | Always-on knowledge |
| **Command** | Manual (/) | Explicit workflows |
| **Subagent** | Task tool | Isolated execution |

### Pre-built Skills (2026)

- **PowerPoint** (pptx): Create presentations
- **Excel** (xlsx): Spreadsheets, charts
- **Word** (docx): Documents
- **PDF** (pdf): Generate reports

### Best Practices
- Skills **auto-load by context** - mention keywords
- Put **stable knowledge** in skills
- Put **workflows** in commands
- Use **subagent skills field** to grant skill access
- Install from marketplace: `/plugin install skill-name@repo`

**Sources**: [Agent Skills](https://code.claude.com/docs/en/skills), [Skills Deep Dive](https://leehanchung.github.io/blogs/2025/10/26/claude-skills-deep-dive/)

---

## SYNTHESIS: The Gestalt

### The 2026 Claude Code Web Stack

```
┌─────────────────────────────────────────────────┐
│                   USER LAYER                     │
│  CLAUDE.md │ Slash Commands │ Plugins           │
├─────────────────────────────────────────────────┤
│               ORCHESTRATION LAYER                │
│  Skills (auto) │ Subagents (parallel) │ Hooks   │
├─────────────────────────────────────────────────┤
│                INTEGRATION LAYER                 │
│  MCP Servers │ Git │ Testing │ Build Systems    │
├─────────────────────────────────────────────────┤
│               OPTIMIZATION LAYER                 │
│  Context Management │ Token Efficiency │ SLOs   │
└─────────────────────────────────────────────────┘
```

### Key Principles (Bruce Lee Style)

1. **Be like water** - Adapt to the task, use the right tool
2. **One job, one agent** - Focus beats generalization
3. **Parallel by default** - Sequential is the exception
4. **Evidence over narrative** - Receipts, not reviews
5. **Prevent, don't detect** - Hooks block bad actions
6. **Context is currency** - Spend it wisely

### Quick Reference

```bash
# Session start
/init                    # Generate CLAUDE.md
/context                 # Check token usage
/help                    # List all commands

# Development
/project:my-command      # Run project command
Task("agent", "task")    # Spawn subagent

# Validation
cargo make pre-commit    # Full validation
/compact                 # Reduce context

# Git
git commit -m "type: message"
gh pr create
```

---

**Version**: 1.0.0 | **Date**: 2026-01-05 | **Methodology**: EPIC 9 Parallel Synthesis

**Sources**:
- [Claude Code Best Practices](https://www.anthropic.com/engineering/claude-code-best-practices)
- [Hooks Reference](https://code.claude.com/docs/en/hooks)
- [Slash Commands](https://code.claude.com/docs/en/slash-commands)
- [Agent Skills](https://code.claude.com/docs/en/skills)
- [Subagents SDK](https://platform.claude.com/docs/en/agent-sdk/subagents)
- [MCP Servers 2026](https://www.builder.io/blog/best-mcp-servers-2026)
- [Full Stack Guide](https://alexop.dev/posts/understanding-claude-code-full-stack/)
- [Awesome Claude Code](https://github.com/hesreallyhim/awesome-claude-code)
