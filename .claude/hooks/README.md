# Hooks System v2.0

Deterministic enforcement for the Pasadena TDD doctrine. Seven hooks wired to Claude Code lifecycle events.

## Overview

Hooks enforce doctrine mechanically. CLAUDE.md states the rules. Rules describe them. Hooks block violations at the point of action. Every named failure mode has a corresponding hook.

## Hook Map

| Hook | Event | Failure Modes Blocked | Exit Behavior |
|------|-------|----------------------|---------------|
| pre-tool-safety.sh | PreToolUse (Bash) | Direct cargo, destructive ops, root files | exit 1 = blocked |
| pre-edit-test-guard.sh | PreToolUse (Edit/Write) | TEST MURDER, MOCK COMFORT, SHALLOW GREEN | exit 2 = blocked |
| post-bash-validation.sh | PostToolUse (Bash) | Compiler errors, test failures (andon signals) | exit 2 = blocked |
| post-write-test-scan.sh | PostToolUse (Write) | MOCK COMFORT, SHALLOW GREEN | exit 2 = blocked |
| user-prompt.sh | UserPromptSubmit | NARRATION, SELF-CERT, completion without evidence | exit 0 = advisory |
| session-start.sh | SessionStart | None (injects live state) | exit 0 = informational |
| stop-receipt-check.sh | Stop | NARRATION (closing without proof) | exit 0 = advisory |

## Exit Code Semantics

- exit 0: Advisory. Output appears as context. Claude continues.
- exit 1: Blocked (PreToolUse). Claude must choose a different action.
- exit 2: Blocked (PostToolUse). Claude sees the error on stderr and must address it.

## Hook Details

### pre-tool-safety.sh

Blocks dangerous bash commands before execution.
- Direct cargo commands (requires cargo make)
- Force push to main/master
- Dangerous rm -rf patterns
- git reset --hard
- Saving files to repository root
- --no-verify flag
- unwrap()/expect() in production source files

### pre-edit-test-guard.sh

Blocks test file mutations that weaken verification.
- Mock imports: mockall, automock, MockFoo (exit 2)
- Vacuous assertions: assert!(result.is_ok()), assert!(x.is_some()) (exit 2)
- Assertion deletion: removing assert! lines (exit 2)
- #[ignore] without reason string (exit 2)

Reads JSON from stdin with file path and content. Parses for forbidden patterns.

### post-bash-validation.sh

Exits 2 (blocking) on compiler errors and test failures. This is the andon signal made mechanical.
- error[E...] patterns -> exit 2 "COMPILER ERROR: STOP THE LINE"
- test ... FAILED patterns -> exit 2 "TEST FAILURE: STOP THE LINE"
- warning: patterns -> exit 0 (advisory, stop before release)
- clippy:: patterns -> exit 0 (advisory, stop before release)

### post-write-test-scan.sh

Scans newly written test files for forbidden patterns.
- mockall/automock/mock! imports -> exit 2 (blocking)
- MockFoo/FakeBar struct patterns -> exit 0 (warning)
- Zero assertions in test file -> exit 0 (warning)

### user-prompt.sh

Detects doctrine violations in user prompts and warns Claude.
- SELF-CERT: "looks correct" / "should work" without test/OTEL keywords
- NARRATION: "should have" / "would be" / "probably" in completion context
- Completion claims: "done" / "complete" / "finished" without validation keywords

### session-start.sh

Injects live workspace state as structured context.
- Branch name and uncommitted change count
- Compile state (CLEAN / WARNINGS / ERRORS) via cargo check
- No ceremony, no emoji, just facts

### stop-receipt-check.sh

Runs before session close. Checks for uncommitted changes and reminds about evidence requirements.
- Counts uncommitted files via git diff/cached/ls-files
- Prints evidence requirements if work was done
- Always exit 0 (non-blocking)

## Wiring in settings.json

```json
{
  "hooks": {
    "PreToolUse": [
      { "matcher": "Bash", "command": "bash .claude/hooks/pre-tool-safety.sh \"$TOOL_INPUT\"" },
      { "matcher": "Edit|Write", "command": "bash .claude/hooks/pre-edit-test-guard.sh \"$TOOL_INPUT\"" }
    ],
    "PostToolUse": [
      { "matcher": "Bash", "command": "bash .claude/hooks/post-bash-validation.sh \"$TOOL_INPUT\" \"$EXIT_CODE\" \"$TOOL_OUTPUT\"" },
      { "matcher": "Write", "command": "bash .claude/hooks/post-write-test-scan.sh \"$TOOL_INPUT\"" }
    ],
    "UserPromptSubmit": [
      { "command": "bash .claude/hooks/user-prompt.sh \"$PROMPT\"" }
    ],
    "SessionStart": [
      { "command": "bash .claude/hooks/session-start.sh" }
    ],
    "Stop": [
      { "command": "bash .claude/hooks/stop-receipt-check.sh" }
    ]
  }
}
```

## Failure Mode Coverage

| Failure Mode | Detection Point | Enforcement |
|--------------|----------------|-------------|
| NARRATION | user-prompt.sh (prompt patterns), stop-receipt-check.sh (no evidence) | Advisory |
| SELF-CERT | user-prompt.sh ("looks correct") | Advisory |
| TEST MURDER | pre-edit-test-guard.sh (assertion weakening/deletion) | Blocking (exit 2) |
| SHALLOW GREEN | pre-edit-test-guard.sh (vacuous assertions), post-write-test-scan.sh | Blocking (exit 2) |
| MOCK COMFORT | pre-edit-test-guard.sh (mock imports), post-write-test-scan.sh | Blocking (exit 2) |
| LAZY JUDGE | pasadena-verify skill (hermetic verification) | Skill-level |

## Maintenance

All hooks use 3s timeout. All hooks use dynamic workspace root detection via `git rev-parse --show-toplevel`. No hardcoded paths.

## Version History

- v2.0.0 (2026-04-01) -- Pasadena TDD enforcement. 7 hooks (4 rewritten + 3 new). Blocking exit codes.
- v1.0.0 (2026-02-08) -- Initial advisory-only hooks. 4 scripts. Exit 0 on everything.
