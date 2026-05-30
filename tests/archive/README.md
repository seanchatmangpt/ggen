# Test Archive

This directory contains tests that have been **reclassified** from the active test suite, not deleted. Tests are moved here when they no longer match a named invariant or fail the ProofPack criteria.

## Non-Deletion Doctrine

Per the project's constitution:
- **Code is fossil evidence**: Nothing is deleted.
- **Tests are classified**: Every test is assigned a lane (A, B, C, or D).
- **Promotion is explicit**: A test returns to the active suite only when a new invariant is named for it and it passes the promotion gate.

## Directory Structure

- `pre-replacement/` — Tests from the original test estate (before ProofPack migration)

## Promotion Path

To activate an archived test:
1. Name the invariant it validates
2. Move to appropriate lane (A for release-critical, B for contracts, C for integration)
3. Run `cargo make test-<lane>` to verify
4. Document the promotion reason

## References

- ProofPack architecture: `tests/LANE_STRUCTURE.md`
- Named invariants: `.claude/CLAUDE.md` (OTEL validation section)
