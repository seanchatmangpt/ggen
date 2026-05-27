## 2026-05-26T23:30:33Z

You are teamwork_preview_worker_self_audit_1.
Your working directory is /Users/sac/ggen/.agents/teamwork_preview_worker_self_audit_1/.
Your mission is:
1. Implement the self-audit log generator in `crates/ggen-graph/src/ocel/self_audit.rs`. It must construct an `OcelLog` with all required objects, events, qualifiers, and attributes representing the development process.
   Required Object Types:
   - `RustCrate`, `PRDRequirement`, `ARDRequirement`, `GALLCheckpoint`, `PublicOntology`, `OntologyTerm`, `SourceFile`, `TestFile`, `ExampleFile`, `FixtureFile`, `ScriptFile`, `Command`, `CommandRun`, `EvidenceArtifact`, `GraphReceipt`, `CoverageMatrix`, `PromotionDecision`, `UnsupportedCapability`
   Required Event Types:
   - `RequirementDeclared`, `OntologyMapped`, `FileEmitted`, `ImplementationChanged`, `FixtureCreated`, `CommandExecuted`, `TestPassed`, `TestFailed`, `ForbiddenSurfaceScanned`, `AntiFakeScanned`, `ReceiptEmitted`, `ReplayVerified`, `CoverageEvaluated`, `CheckpointEvaluated`, `CheckpointPromoted`, `CheckpointRefused`, `UnsupportedCapabilityDeclared`
   Required Qualifiers in relationships:
   - `--checks-->`, `--produces-->`, `--verifies-->`, `--satisfied_by-->`, `--decides-->`
   Make sure all strings are genuine. No placeholders like "TODO", "uuid_placeholder", or empty strings.
2. Implement `crates/ggen-graph/src/ocel/gall_projection.rs` using `EvidenceProjector` to project this log to and extract it from the deterministic graph, and add any queries necessary to verify relationships.
3. Expose the new modules in `crates/ggen-graph/src/ocel/mod.rs`.
4. Verify that the project compiles cleanly by running `cargo check -p ggen-graph --all-targets` and `cargo test -p ggen-graph`.
5. Write your implementation report in `/Users/sac/ggen/.agents/teamwork_preview_worker_self_audit_1/changes.md` and a handoff report at `/Users/sac/ggen/.agents/teamwork_preview_worker_self_audit_1/handoff.md`.
6. Send a message to the orchestrator (f5c30d73-c213-4935-bd9d-5c2ce4c67f3e) with a link to your handoff.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
