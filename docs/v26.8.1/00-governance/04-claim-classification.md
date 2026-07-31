# Claim classification

Every material statement in the v26.8.1 corpus must be classified.

- **Observed:** directly read from exact source, manifest, generated surface, receipt, or execution evidence.
- **Derived:** mathematically or logically calculated from observed values with assumptions shown.
- **Proposed:** target architecture, requirement, or implementation choice not yet executed.
- **Blocked:** execution cannot proceed because an admitted dependency is unavailable.
- **Refused:** intentionally excluded by an enforced boundary or approved incompatibility decision.

## Prohibited collapse

A proposed throughput model cannot become an observed benchmark. A source-code path cannot become an observed behavior without execution. A passing internal test cannot become a production claim without the declared boundary crossing. A generated receipt cannot witness itself as ALIVE.

## Required document metadata

Each final document must identify source head, observation date, claim classes, implementation owners, verifier, falsifier, replay command, legacy disposition, and standing.
