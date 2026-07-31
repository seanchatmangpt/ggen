# Standing vocabulary

The corpus uses the repository standing vocabulary without synonym drift.

- `UNKNOWN`: evidence is absent, stale, contradictory, or unmapped.
- `PARTIAL_ALIVE`: one or more bounded checkpoints passed; the crown claim remains open.
- `ALIVE`: all required conjuncts executed and an external verifier admitted the exact aggregate receipt.
- `BLOCKED`: an admitted dependency prevents execution.
- `BUILD_BROKEN`: the relevant verifier cannot be reached because the source or generated output fails to build.
- `UNSUPPORTED`: the capability is outside the admitted boundary.

A typed refusal explains why execution stopped. It does not independently establish ALIVE or UNSUPPORTED standing.

For the ggen-legacy sunset, `UNKNOWN`, `PARTIAL_ALIVE`, `BLOCKED`, and `BUILD_BROKEN` all block final retirement unless the affected capability is explicitly refused and the incompatibility is approved with migration and recovery evidence.
