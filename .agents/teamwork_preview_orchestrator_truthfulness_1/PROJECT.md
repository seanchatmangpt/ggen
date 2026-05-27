# Project: Agent Truthfulness GALL Protocol

## Architecture
- Target: `crates/ggen-graph/`
- Audit artifacts:
  - `crates/ggen-graph/audit/worktree_inventory.json`
  - `crates/ggen-graph/audit/agent_truthfulness.external_adjudication.json`
  - `crates/ggen-graph/audit/transcripts/` (directory containing command transcript JSONs)
- Verifier scripts:
  - `scripts/gall/external/20_capture_full_worktree_inventory.sh`
  - `scripts/gall/external/23_run_sabotage_suite.sh`
  - `scripts/gall/external/99_adjudicate_truthfulness.sh`
  - `verify_agent_truthfulness.sh` at workspace root.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Worktree Inventory | Implement 20_capture_full_worktree_inventory.sh and generate worktree_inventory.json | None | PLANNED |
| 2 | Command Transcripts | Capture command transcripts with metadata under crates/ggen-graph/audit/transcripts/ | M1 | PLANNED |
| 3 | Sabotage Suite | Implement 23_run_sabotage_suite.sh with temporary corrupted mutations and verify refusal | M2 | PLANNED |
| 4 | Adjudication & Integration | Implement 99_adjudicate_truthfulness.sh and verify_agent_truthfulness.sh | M3 | PLANNED |

## Interface Contracts

### Worktree Inventory JSON Schema
```json
{
  "timestamp": "2026-05-26T17:00:00Z",
  "files": [
    {
      "path": "crates/ggen-graph/src/lib.rs",
      "size_bytes": 1250,
      "modified_time": "2026-05-26T17:00:00Z",
      "sha256": "abcdef..."
    }
  ]
}
```

### Command Transcript JSON Schema
```json
{
  "command": "cargo test -p ggen-graph",
  "exit_code": 0,
  "duration_ms": 1250,
  "environment": {
    "PATH": "...",
    "INTEGRITY_MODE": "benchmark"
  },
  "stdout_sha256": "...",
  "stderr_sha256": "..."
}
```

### Agent Truthfulness Adjudication JSON Schema
```json
{
  "timestamp": "2026-05-26T17:00:00Z",
  "verdict": "Promoted",
  "reason": "All verifiers passed",
  "transcripts": [
    {
      "path": "crates/ggen-graph/audit/transcripts/00_capture_baseline.json",
      "exit_code": 0
    }
  ],
  "worktree_inventory": {
    "path": "crates/ggen-graph/audit/worktree_inventory.json",
    "sha256": "..."
  }
}
```
