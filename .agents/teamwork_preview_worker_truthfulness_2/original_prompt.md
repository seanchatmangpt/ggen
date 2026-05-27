## 2026-05-27T00:56:00Z

Your objectives:
1. Implement the Witnessed Agent Truthfulness GALL protocol in the `ggen` repository using the new **Witnessed Code Evaluation / Knowledge Hook Actuation** model.
2. Under `crates/ggen-graph/src/bin/`, implement the following Rust boundary observers:
   - `gall_observe_worktree.rs`: Observe worktree inventory. Generate `crates/ggen-graph/audit/worktree_inventory.full.json` (path, size, SHA256, BLAKE3, inclusion status/reason) and corresponding RDF/Turtle facts.
   - `gall_observe_commands.rs`: Execute scripts 00-13 under `scripts/gall/external/` using std::process::Command and capture transcripts (exit code, duration, stdout/stderr hashes) into `crates/ggen-graph/audit/transcripts/` as JSON and separate stdout/stderr log files.
   - `gall_observe_sabotage.rs`: Run negative-control sabotage checks for the 12 required cases, assert verifier failures, restore workspace, and write `crates/ggen-graph/audit/sabotage_results.json`.
   - `gall_observe_clean_room.rs`: Copy codebase to a temporary directory, run cargo build and cargo test, and write `crates/ggen-graph/audit/clean_room_rebuild.json`.
   - `gall_observe_docs_tree.rs`: Verify existence of required docs in R4 and emit `docs/docs.tree.json` and `docs/docs.tree.ttl`.
   - `gall_observe_doctests.rs`: Observe and verify cargo doctest coverage/verification, writing `crates/ggen-graph/audit/doctest_results.json`.
   - `gall_materialize_evidence_graph.rs`: Aggregate individual observer outputs into a cohesive RDF/Turtle evidence graph `crates/ggen-graph/audit/gall_evidence.ttl` and `crates/ggen-graph/audit/gall_evidence.ocel.json`.
   - `gall_actuate_code_evaluation.rs`: Actuate the evaluation runtime. Loads `crates/ggen-graph/hooks/gall-code-evaluation.ttl` and `crates/ggen-graph/audit/gall_evidence.ttl` into an Oxigraph store, runs trigger CONSTRUCT queries for each hook, writes the decision delta to `crates/ggen-graph/audit/gall_decision.delta.ttl`, and outputs cryptographic receipts in `crates/ggen-graph/audit/gall_code_evaluation.receipt.ttl` and `crates/ggen-graph/audit/gall_code_evaluation.final.ttl`.
   - `gall_adjudicate_witnessed_truthfulness.rs`: Determines the final promotion verdict based on the code evaluation output and produces `crates/ggen-graph/audit/witnessed_truthfulness.external_adjudication.json`.
3. Create `crates/ggen-graph/hooks/gall-code-evaluation.ttl` defining the evaluation law for checkpoints W0-W9. Each hook must have a trigger query (SPARQL ASK/SELECT) and action (SPARQL CONSTRUCT query).
4. Update/write `docs/VISION_2030_GALL_PROOF.md` to shift from the old 5-rule shape to the W0–W9/T0-T10 Witnessed Agent Truthfulness GALL checkpoints.
5. Create thin launcher `scripts/gall/run_witnessed_truthfulness.sh` and root orchestrator `verify_agent_truthfulness.sh` at workspace root.
6. Verify boundary rules: make sure no production library surface uses `std::process::Command`, network, or LLM.
7. Ensure all public modules have doctest coverage in compliance with R5/R6, and that they all compile/pass with `cargo test -p ggen-graph --doc`.
8. Delete any old/redundant binaries under `src/bin/` if necessary.
9. Deliver your handoff.md with passing build/test results, list of files created, command outputs, and the generated audit artifacts.
