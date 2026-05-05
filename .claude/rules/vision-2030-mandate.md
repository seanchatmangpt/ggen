# Vision 2030 Mandates

The Vision 2030 architecture sets a non-negotiable standard for implementation completeness, test quality, and performance validation across all SemanticOS capabilities.

## 1. Absolute Chicago TDD (Zero Mocks)
- **Real Collaborators Only**: Never use `mockall::mock!`, Python's `unittest.mock`, `MagicMock`, or behavior verification (`.times()`, `.expect()`).
- **No Test Doubles**: Do not fake state or use in-memory stubs if a real system boundary exists. 

## 2. Observable State & Evidence Emitters
- **Execution Evidence**: Assertions must verify actual observable state changes (e.g., physical file removal, database row deletion, JSON field mutation, event log generation).
- **AAA Pattern**: Strictly follow Arrange (real data structures) -> Act (real collaborator operations) -> Assert (observable results verification).

## 3. Strict Performance SLOs
- All new implementations must conform to predefined Service Level Objectives (SLOs).
- You must always check your work against the operational budget (e.g. latency bounds, API limits, execution times).
- Where available, use provided testing targets (like `cargo make slo-check` or integration tests) to explicitly validate your performance constraints.

## 4. End-to-End (E2E) Boundary Crossing
- All tasks must be validated across the complete lifecycle or system pipeline. 
- Unit testing without boundary crossing is considered incomplete and fraudulent. Tests must cause real transitions between the application layers.
