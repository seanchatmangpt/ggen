# Stop Release Control System

Two-stage verification system for controlling when agents can stop work.

## Architecture

```
Stop Request → Stage 1 (Deterministic) → Stage 2 (Agent Verification) → Decision
                      ↓                           ↓
                Fast checks               Conformance, holds,
                (files, tests)            signatures, authority
```

## Stage 1: Deterministic Gate (Shell)

**File:** `.claude/hooks/stop_release_gate.sh`

**Purpose:** Fast, deterministic checks that don't require complex dependencies.

**Checks:**
1. **Artifacts** - README.md exists in project root or docs/
2. **Tests** - Test files present (`test_*.py` or `*_test.py`)
3. **Evidence Log** - Evidence log exists with entries
4. **Receipt** - Compliance ledger has receipts

**Input (JSON via stdin):**
```json
{
  "stop_reason": "user_request",
  "session_id": "session-123",
  "work_units": ["unit1", "unit2"]
}
```

**Output (JSON via stdout):**
```json
{
  "stop_allowed": true,
  "stage": "deterministic",
  "message": "All deterministic checks passed",
  "next_stage": "agent_verification"
}
```

**Exit codes:** 0 (allow), 1 (deny), 2 (error)

## Stage 2: Agent Verification (Python)

**File:** `.claude/hooks/stop_verifier.py`

**Purpose:** Agent-based verification with conformance checking and signature validation.

**Checks:**
1. **Conformance** - Process conformance score (≥0.95 required)
2. **Holds** - No active release blockers in database
3. **Authority** - Receipt from authorized module
4. **Signature** - Receipt signature valid (if available)

**Input (JSON via stdin):** Same as Stage 1

**Output (JSON via stdout):**
```json
{
  "stop_allowed": true,
  "stage": "agent_verification",
  "conformance": 0.95,
  "holds": [],
  "message": "Stop allowed: conformance 0.95, no active holds, authority verified",
  "authority_check": true,
  "signature_valid": null
}
```

**Exit codes:** 0 (allow), 1 (deny), 2 (error)

## Usage

### Manual Testing

```bash
# Test Stage 1 only
echo '{"stop_reason":"test"}' | .claude/hooks/stop_release_gate.sh

# Test Stage 2 only
echo '{"stop_reason":"test"}' | .claude/hooks/stop_verifier.py

# Test both stages (full pipeline)
/tmp/test_stop_gate.sh
```

### Integration with Agents

Agents should call both stages in sequence:

```python
import subprocess
import json

def request_stop(reason: str, session_id: str, work_units: list) -> bool:
    """Request to stop work, returns True if allowed."""
    request = {
        "stop_reason": reason,
        "session_id": session_id,
        "work_units": work_units
    }
    
    # Stage 1: Deterministic checks
    result1 = subprocess.run(
        [".claude/hooks/stop_release_gate.sh"],
        input=json.dumps(request),
        capture_output=True,
        text=True
    )
    
    response1 = json.loads(result1.stdout)
    if not response1["stop_allowed"]:
        return False
    
    # Stage 2: Agent verification
    result2 = subprocess.run(
        [".claude/hooks/stop_verifier.py"],
        input=json.dumps(request),
        capture_output=True,
        text=True
    )
    
    response2 = json.loads(result2.stdout)
    return response2["stop_allowed"]
```

## Compliance Ledger Schema

```sql
-- Receipts table
CREATE TABLE receipts (
    receipt_id TEXT PRIMARY KEY,
    timestamp TEXT NOT NULL,
    authority TEXT NOT NULL,
    attestation TEXT NOT NULL,
    fitness REAL,
    precision REAL,
    generalization REAL,
    deviation_count INTEGER,
    signature TEXT NOT NULL,
    report_data TEXT
);

-- Release blockers (holds)
CREATE TABLE release_blockers (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    reason TEXT NOT NULL,
    active INTEGER DEFAULT 1,
    created_at TEXT NOT NULL
);
```

## Adding Holds

To prevent stop, add a hold to the compliance ledger:

```python
import sqlite3
from datetime import datetime, timezone
from pathlib import Path

db_path = Path("artifacts/compliance_ledger.db")
conn = sqlite3.connect(str(db_path))
cursor = conn.cursor()

cursor.execute("""
    INSERT INTO release_blockers (reason, active, created_at)
    VALUES (?, 1, ?)
""", ("Critical bug fix pending", datetime.now(timezone.utc).isoformat()))

conn.commit()
conn.close()
```

## Removing Holds

To allow stop again, clear the hold:

```python
cursor.execute("UPDATE release_blockers SET active = 0 WHERE reason LIKE ?", ("%bug fix%",))
conn.commit()
```

## Design Decisions

### Why Two Stages?

1. **Stage 1 (Fast):** Shell script with minimal dependencies, catches obvious issues quickly
2. **Stage 2 (Thorough):** Python with full conformance checking, but gracefully degrades if ostar unavailable

### Why Shell for Stage 1?

- Fast execution (no Python startup overhead)
- Minimal dependencies (only Python 3 for JSON parsing)
- Easy to debug and modify
- Works even if project dependencies aren't installed

### Why Graceful Degradation?

Both stages are designed to work even if:
- ostar package isn't installed
- Compliance ledger is missing
- Conformance engine unavailable

This ensures the gate system is robust and doesn't become a single point of failure.

## Testing

Run the comprehensive test:

```bash
/tmp/test_stop_gate.sh
```

Expected output:
```
=== Testing Stop Release Control System ===

1. Testing Stage 1 (Deterministic Checks)
   ✓ Stage 1 passed - proceeding to Stage 2

2. Testing Stage 2 (Agent Verification)
   ✓ Stage 2 passed - stop is ALLOWED

=== RESULT: Stop Request Approved ===
```

## Files

- `.claude/hooks/stop_release_gate.sh` - Stage 1 deterministic gate
- `.claude/hooks/stop_verifier.py` - Stage 2 agent verification
- `.claude/hooks/STOP_RELEASE_CONTROL.md` - This documentation
- `/tmp/test_stop_gate.sh` - Integration test script

## Exit Code Summary

| Exit Code | Meaning |
|-----------|---------|
| 0 | Stop allowed |
| 1 | Stop denied (missing items, holds, low conformance) |
| 2 | Error (invalid JSON, system error) |
