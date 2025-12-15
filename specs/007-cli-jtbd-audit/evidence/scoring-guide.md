# Agent Accessibility Scoring Reference Card

## Scoring Rubric (100 points total)

### 1. Parseable Output (25 points)

| Score | Criteria |
|-------|----------|
| 5 | Text only, no structure |
| 10 | Basic structure (lines, columns) |
| 15 | JSON option available |
| 20 | JSON with consistent schema |
| 25 | JSON schema + documented format |

### 2. Error Messages (20 points)

| Score | Criteria |
|-------|----------|
| 4 | Stack trace only |
| 8 | Generic error message |
| 12 | Human-readable with context |
| 16 | Machine-parseable format |
| 20 | Structured errors with codes |

### 3. Idempotency (15 points)

| Score | Criteria |
|-------|----------|
| 3 | Destructive, no safeguards |
| 6 | Prompts before overwrite |
| 9 | Mostly safe, edge cases |
| 12 | Safe with --force flag |
| 15 | Fully idempotent |

### 4. Progress Feedback (10 points)

| Score | Criteria |
|-------|----------|
| 2 | Silent execution |
| 4 | Start/end messages |
| 6 | Final status only |
| 8 | Periodic updates |
| 10 | Streaming progress |

### 5. Dry-Run Support (10 points)

| Score | Criteria |
|-------|----------|
| 0 | No dry-run |
| 3 | Partial preview |
| 5 | Basic --dry-run |
| 7 | Detailed preview |
| 10 | Full diff preview |

### 6. Documentation (10 points)

| Score | Criteria |
|-------|----------|
| 2 | --help only, minimal |
| 4 | --help with examples |
| 6 | Man page / docs |
| 8 | JTBD documented |
| 10 | JTBD + examples + API ref |

### 7. Exit Codes (10 points)

| Score | Criteria |
|-------|----------|
| 2 | 0/1 only |
| 4 | Different for crash vs error |
| 6 | Differentiated by type |
| 8 | Documented codes |
| 10 | Semantic, documented codes |

---

## Maturity Level Mapping

| Score Range | Maturity Level | Agent-Usable |
|-------------|----------------|--------------|
| 0-19 | L1 (Initial) | No |
| 20-39 | L2 (Managed) | No |
| 40-59 | L3 (Defined) | No |
| 60-79 | L4 (Quantified) | **YES** |
| 80-100 | L5 (Optimized) | **YES** |

### Special Classifications

- **L0**: Command crashes or doesn't execute
- **L0-DEP**: Command deprecated, marked for removal

### Blocker Caps

| Blocker | Max Level |
|---------|-----------|
| Non-deterministic output | L3 |
| Missing JSON output | L3 |
| No error handling | L2 |
| Crashes on valid input | L0 |

---

## Quick Reference: Avatar Priorities

| Avatar | Primary Concerns |
|--------|------------------|
| Claude Code | JSON output, documentation, idempotency |
| Cursor AI | Context size, single-command usage |
| GitHub Copilot | Learnable patterns, examples |
| Aider | Git-trackable, deterministic |
| Devin | Self-contained, chained commands |
| OpenHands | Customizable, configurable |
| Windsurf | IDE preview, visual feedback |

---

## Validation Checksum

```
agent_score = parseable_output + error_messages + idempotency +
              progress_feedback + dry_run + documentation + exit_codes

maturity_level = min(floor(agent_score/20), blocker_cap) + 1
```

Where `blocker_cap` is the highest level allowed given blockers.
