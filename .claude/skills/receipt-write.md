---
name: receipt-write
description: Write structured receipt to memory after proof-stack verification passes
user-invocable: false
---

# Receipt Writer

## Purpose
After pasadena-verify or proof-stack passes, write a structured receipt to the memory ledger.

## Receipt Format
```markdown
| DATE | FEATURE | EVIDENCE | PROOF STACK | STATUS |
|------|---------|----------|-------------|--------|
| 2026-04-01 | Feature X | PROVEN | check+test+lint+OTEL | Verified |
```

## Fields
- DATE: ISO 8601 timestamp
- FEATURE: What was verified
- EVIDENCE: PROVEN / OBSERVED / INFERRED / UNVERIFIED
- PROOF STACK: Which layers passed (check, test, lint, OTEL, Weaver)
- STATUS: Verified / Pending / Rejected

## When to Invoke
- After pasadena-verify returns APPROVED
- After proof-stack completes all three layers
- NOT invoked for UNVERIFIED or INFERRED results
