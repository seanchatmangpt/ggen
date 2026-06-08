# Original User Request

## 2026-06-06T14:08:32-07:00

You are the Sub-orchestrator for Milestone M2 (Core Projection Model & Defect Resolution) of the Implementation Track.
Your working directory is: /Users/sac/ggen/.agents/sub_orch_m2
Your parent conversation ID is: 4b5478bf-08ac-49b9-81dd-00793a75d992

Your mission:
Implement and verify Milestone M2 as specified in /Users/sac/ggen/.agents/sub_orch_m2/SCOPE.md.
Specifically, verify the core models in `ggen-projection` and resolve the following five defects:
1. Concurrency race condition in bidirectional symbol lookup (`SymbolTable::insert_custom` in `crates/genesis-construct8/src/models.rs`).
2. Broken git commit chain history in receipt storage (`LockchainStorage::append_to_git` in `crates/genesis-lockchain/src/storage.rs`).
3. Silent mapping range overwrites on file collision (`ProjectionMap::add_mapping` in `crates/ggen-projection/src/mapping.rs`).
4. Non-deterministic indexes caused by random UUIDs (`ReceiptIndex::add_receipt` in `crates/ggen-projection/src/receipt.rs`).
5. Any pre-existing workspace compile blocker in `ggen-core` test targets due to missing `packs` fields in `GgenManifest` initializations.

You MUST execute the Explorer -> Worker -> Reviewer -> Challenger -> Auditor cycle directly:
1. Spawn 3 Explorers (teamwork_preview_explorer) to analyze the five defect locations in the codebase and recommend fixes.
2. Spawn a Worker (teamwork_preview_worker) to apply the fixes to the files and run check/test commands. Remember: DO NOT CHEAT. All implementations must be genuine.
3. Spawn 2 Reviewers (teamwork_preview_reviewer) to verify correctness and safety.
4. Spawn 2 Challengers (teamwork_preview_challenger) to verify behavior and test edge cases.
5. Spawn a Forensic Auditor (teamwork_preview_auditor) to run integrity checks.

Remember:
- Do not write/modify code yourself.
- Run builds/tests via workers only.
- Write progress.md and BRIEFING.md in your working directory.
- Update your parent regularly. Once complete, write handoff.md and send a message back to 4b5478bf-08ac-49b9-81dd-00793a75d992.
