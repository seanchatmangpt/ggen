# Claude Code Configuration for ggen

This directory contains all Claude Code customization files for the ggen project.

## Directory Structure

```
.claude/
├── settings.json              # Shared project configuration (version controlled)
├── settings.local.json        # Personal settings (gitignored)
├── CLAUDE.md                  # Project context and instructions (symlink to root)
│
├── commands/                  # Custom slash commands
│   ├── optimize.md           # /optimize - Code optimization
│   ├── test-audit.md         # /test-audit - Run test quality audit
│   ├── bench-compare.md      # /bench-compare - Compare benchmarks
│   ├── speckit-check.md      # /speckit-check - Validate specifications
│   └── review-errors.md      # /review-errors - Audit error handling
│
├── agents/                    # Custom subagent definitions
│   ├── rust-coder.md         # Rust implementation specialist
│   ├── test-engineer.md      # Test writing and quality specialist
│   ├── reviewer.md           # Code review and compliance specialist
│   └── speckit-architect.md  # RDF specification designer
│
├── skills/                    # Rich capability modules
│   ├── cargo-make-protocol/
│   │   ├── SKILL.md          # Cargo Make usage mastery
│   │   └── reference.md      # Detailed command reference
│   │
│   ├── chicago-tdd-pattern/
│   │   └── SKILL.md          # Chicago TDD testing patterns
│   │
│   ├── rdf-ontologies/
│   │   └── SKILL.md          # RDF, Turtle, SPARQL mastery
│   │
│   └── poka-yoke-patterns/
│       └── SKILL.md          # Error-proofing and poka-yoke design
│
├── hooks/                     # Deterministic lifecycle hooks
│   ├── session-start.sh       # Initialize environment at session start
│   ├── pre-tool-safety-check.sh # Prevent dangerous operations
│   ├── post-bash-validation.sh  # Detect andon signals
│   └── user-prompt-validation.sh # Validate user instructions
│
└── helpers/                   # Helper scripts (optional)
    ├── api-key-helper.sh     # Manage ANTHROPIC_API_KEY
    └── bash-init.sh          # Bash environment initialization
```

## Quick Start

### Using Slash Commands

```bash
/optimize [crate]              # Optimize code for performance
/test-audit [crate]            # Run test quality audit
/bench-compare [branch1] [branch2]  # Compare performance
/speckit-check [feature]       # Validate RDF specifications
/review-errors [crate]         # Audit error handling
```

### Using Agents

Spawn specialized agents for complex tasks:

```bash
/agents spawn rust-coder "Implement feature X following plan"
/agents spawn test-engineer "Write tests for module Y"
/agents spawn reviewer "Review code in crates/ggen-core/src/lib.rs"
/agents spawn speckit-architect "Design specification for feature Z"
```

### Using Skills

Skills auto-load based on context. Trigger them by mentioning:

- **cargo-make-protocol**: Talk about cargo make, building, testing
- **chicago-tdd-pattern**: Discuss testing, test quality, mutations
- **rdf-ontologies**: Work with .ttl files, RDF, specifications
- **poka-yoke-patterns**: Discuss error-proofing, quality gates, FMEA

## Configuration Files

### settings.json (Shared)

**Version controlled** - shared with team

```json
{
  "permissions": { },           # Tool and file access controls
  "hooks": { },                 # Lifecycle hooks configuration
  "environment": { },           # Environment variables
  "tools_and_model": { },       # Model and tool settings
  "project_metadata": { }       # Project information
}
```

**Edit when**: Team needs to change shared rules, add team commands, configure hooks

**Do NOT edit**: Personal API keys, local overrides (use settings.local.json)

### settings.local.json (Personal)

**Gitignored** - personal overrides

Contains:
- Personal API keys (ANTHROPIC_API_KEY)
- Personal environment variables
- Local model preferences
- Debugging flags

**Create** by copying `settings.json` and adding personal overrides

## Features

### 1. Commands (Slash Commands)

Auto-discovered custom commands in `commands/` directory

**Benefits**:
- Project-specific workflows
- Consistent terminology
- Clear documentation
- Easy to discover (shown in /help)

**Creating a command**:

1. Create `.claude/commands/my-command.md`
2. Add YAML frontmatter with description
3. Write markdown instructions
4. Use `$ARGUMENTS`, `$1`, `$2` for parameters
5. Invoke with `/my-command arg1 arg2`

See `commands/optimize.md` for example

### 2. Agents (Subagents)

Specialized agents in `agents/` directory auto-loaded at session start

**Benefits**:
- Separate context for complex tasks
- Role-specific tools and capabilities
- Improved focus and reliability
- Better token efficiency

**Using agents**:

```bash
Task("rust-coder", "Implement X following plan", "rust-coder")
Task("test-engineer", "Write tests for Y", "test-engineer")
Task("reviewer", "Review code in Z", "reviewer")
Task("speckit-architect", "Design spec for W", "speckit-architect")
```

See `agents/` directory for detailed agent descriptions

### 3. Skills (Rich Capabilities)

Auto-discovered, semantic-triggered capabilities in `skills/` directories

**Benefits**:
- Context-aware activation (no slash needed)
- Rich supporting materials
- Examples and references
- Deep knowledge organization

**Using skills**:

Skills auto-load when you mention related concepts. No explicit invocation needed.

- Mention "cargo make" → cargo-make-protocol skill loads
- Talk about testing → chicago-tdd-pattern skill loads
- Work with RDF → rdf-ontologies skill loads
- Discuss error-proofing → poka-yoke-patterns skill loads

See `skills/*/SKILL.md` for full skill documentation

### 4. Hooks (Deterministic Lifecycle)

Shell scripts triggered at specific points in Claude Code lifecycle

**Triggers**:
- `SessionStart`: Initialize environment
- `PreToolUse`: Validate operations before tool runs
- `PostToolUse`: Check results after tool completes
- `UserPromptSubmit`: Validate user instructions
- `Stop`: Verify before ending session

**Benefits**:
- Automate validation
- Prevent dangerous operations
- Catch errors immediately
- Enforce project rules

See `hooks/*.sh` for example implementations

## Constitutional Rules (Key Principles)

Always follow these rules when using ggen:

### 1. Cargo Make Protocol

```bash
ALWAYS:  cargo make [target]
NEVER:   cargo [command]
```

Why: Enforces timeouts, quality gates, andon signals

### 2. Error Handling

```rust
PRODUCTION: Use Result<T, E> (NO unwrap/expect)
TESTS:      unwrap/expect ALLOWED (tests should panic)
```

Why: Production must handle errors gracefully

### 3. Andon Signals (Stop the Line)

```
RED (Error)   → STOP, fix immediately
YELLOW (Warn) → Investigate before release
GREEN (OK)    → Proceed safely
```

Why: Prevent defects from propagating

### 4. Chicago TDD

```rust
// State-based testing with real collaborators
#[test]
fn test_feature() {
    // Arrange: Real objects
    let obj = Object::new().unwrap();

    // Act: Call public API
    obj.do_something();

    // Assert: Verify state changed
    assert_eq!(obj.state(), expected);
}
```

Why: Tests verify behavior, catch 80% of bugs

### 5. RDF-First Specifications

```
Edit:   .specify/specs/NNN-feature/*.ttl (source)
Generate: .specify/specs/NNN-feature/*.md (derived)
NEVER:  Edit .md files directly
```

Why: TTL is source of truth, prevents manual inconsistencies

### 6. Poka-Yoke Error-Proofing

```
Prevent > Detect > React
```

Design systems to:
1. Make mistakes impossible (type system)
2. Catch mistakes immediately (timeouts, gates)
3. Stop when mistakes detected (andon signals)

## Getting Started

### 1. First Time Setup

```bash
# Copy personal settings template
cp settings.json settings.local.json

# Edit settings.local.json with your API key
# ANTHROPIC_API_KEY=your-key-here

# Make hooks executable
chmod +x hooks/*.sh

# Start a new session
# Claude Code will auto-initialize with session-start.sh
```

### 2. Learning the System

1. **Read** `/home/user/ggen/CLAUDE.md` - Project constitution
2. **Explore** `.claude/commands/` - See available slash commands
3. **Review** `.claude/agents/` - Understand specialized agents
4. **Study** `.claude/skills/*/SKILL.md` - Deep knowledge areas
5. **Understand** `settings.json` - Project configuration

### 3. Daily Workflow

```bash
# Check status
git status
cargo make check

# Make changes
# ... edit code ...

# Validate before commit
cargo make pre-commit

# Optimize if needed
/optimize ggen-core

# Audit tests if concerned
/test-audit

# Review for compliance
/review-errors

# Commit and push
git add .
git commit -m "feat: ..."
git push
```

## Command Reference

### Essential Slash Commands

| Command | Purpose | Example |
|---------|---------|---------|
| `/optimize` | Optimize for performance | `/optimize ggen-core` |
| `/test-audit` | Audit test quality | `/test-audit ggen-domain` |
| `/bench-compare` | Compare benchmarks | `/bench-compare main HEAD` |
| `/speckit-check` | Validate .ttl specs | `/speckit-check 004` |
| `/review-errors` | Audit error handling | `/review-errors ggen-cli` |

### Essential Cargo Make Targets

| Target | Purpose | Time |
|--------|---------|------|
| `check` | Compile check | ~5s |
| `test-unit` | Unit tests | ~10s |
| `lint` | Clippy check | ~60s |
| `pre-commit` | Full validation | ~60s |
| `test` | All tests | ~30s |
| `bench` | Benchmarks | 5min |
| `slo-check` | SLO compliance | ~2min |

## Troubleshooting

### "Command not found"

```bash
# Skills auto-load, commands are invoked via /
/my-command arg1 arg2
```

### "Skill not activating"

Ensure your message contains natural trigger words (e.g., "cargo make" for cargo-make-protocol skill)

### "Hook not executing"

```bash
# Make sure hook is executable
chmod +x .claude/hooks/hook-name.sh

# Verify settings.json has hook configured
```

### "Settings not applying"

Priority order (highest to lowest):
1. `.claude/settings.json` (project)
2. `.claude/settings.local.json` (project local)
3. `~/.claude/settings.json` (user global)
4. `~/.claude/settings.local.json` (user local)

## Advanced Topics

### Creating Custom Agents

See `agents/rust-coder.md` for detailed agent format

### Creating Custom Skills

See `skills/cargo-make-protocol/SKILL.md` for skill structure

### Adding Hooks

See `hooks/session-start.sh` for hook implementation

### Custom Commands with Arguments

See `commands/bench-compare.md` for command argument handling

## Documentation

- **CLAUDE.md** (root): Project architecture and philosophy
- **settings.json**: Configuration reference
- **commands/*.md**: Individual command documentation
- **agents/*.md**: Individual agent capabilities
- **skills/*/SKILL.md**: Skill deep-dives with examples
- **hooks/*.sh**: Hook implementation details

## Support

- **Documentation**: Read CLAUDE.md first
- **Issues**: Check `.claude/hooks/` for validation hints
- **Configuration**: Review `settings.json` structure
- **Learning**: Study skills in `skills/*/SKILL.md`

## Best Practices

1. **Always use slash commands** for complex workflows
2. **Spawn agents** for multi-step, complex tasks
3. **Read skill documentation** for deep knowledge areas
4. **Follow andon signals** (RED = stop, fix immediately)
5. **Use cargo make** exclusively (never direct cargo)
6. **Edit .ttl not .md** (RDF-first principle)
7. **Respect path protection** (protected paths prevent overwrites)

## Key Files Reference

| File | Purpose | When to Edit |
|------|---------|--------------|
| `settings.json` | Team configuration | When project rules change |
| `settings.local.json` | Personal settings | Always (gitignored) |
| `commands/*.md` | Slash commands | Adding new workflows |
| `agents/*.md` | Agent definitions | Customizing specialists |
| `skills/*/SKILL.md` | Rich capabilities | Rarely (knowledge base) |
| `hooks/*.sh` | Lifecycle hooks | When validation needed |

## See Also

- `.specify/` - RDF specification system (source of truth)
- `Makefile.toml` - Build orchestration (68K+ lines)
- `CLAUDE.md` - Project constitution and philosophy
- `README.md` (root) - Quick start guide

---

**Version**: 1.0.0
**Project**: ggen 5.0.2
**Last Updated**: 2025-12-28
**Philosophy**: Type-first thinking, zero-cost abstractions, memory safety, deterministic outputs
