## 2026-05-26T17:17:15-07:00
You are Worker 1. Your working directory is `/Users/sac/ggen/.agents/teamwork_preview_worker_truthfulness_1`.

Your task is to implement the Agent Truthfulness GALL protocol for `ggen-graph` by creating and modifying the necessary files.

Requirements:
1. Implement `scripts/gall/external/20_capture_full_worktree_inventory.sh` which generates `crates/ggen-graph/audit/worktree_inventory.json` containing the metadata (path, size, modification time, sha256 hash) of all tracked files. Exclude VCS files (`.git/`), agent metadata files (`.agents/`), temporary state (`.gemini/`, `.antigravitycli/`), and the audit files themselves (`crates/ggen-graph/audit/worktree_inventory.json` and transcripts under `crates/ggen-graph/audit/transcripts/`) to prevent infinite looping and instability. Use Python for high performance and speed.
2. Implement command transcript capture in `crates/ggen-graph/audit/transcripts/` capturing metadata (duration, environment, exit code, stdout/stderr hashes) for every executed verifier command.
   To achieve this:
   - Create `scripts/gall/external/run_with_transcript.sh` as a generic wrapper to capture transcripts.
   - You can modify the verifier scripts to auto-wrap themselves using:
     ```bash
     if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
         export TRANSCRIPT_WRAPPED="true"
         exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
     fi
     ```
     Or execute them via the wrapper in the orchestrator/adjudicator. Note that if you modify existing scripts in `scripts/gall/external/`, you must update their SHA-256 digests in `scripts/gall/external/manifest.sha256` to pass the integrity checks!
3. Implement `scripts/gall/external/23_run_sabotage_suite.sh` which applies temporary corrupted worktree mutations (features in Cargo.toml, TODO in source, std::process::Command, receipt tampering, missing requirement link, file deletion) and proves verification refusal.
   Ensure that:
   - The script runs under a safe `cleanup` trap to restore the worktree to a clean state upon completion, interrupt, or failure.
   - It tests each mutation in isolation and asserts that the corresponding verifier script exits with a non-zero code.
4. Implement `scripts/gall/external/99_adjudicate_truthfulness.sh` which output the final external adjudication JSON `crates/ggen-graph/audit/agent_truthfulness.external_adjudication.json` ONLY if T0-T9 checks pass.
   Ensure that:
   - It validates minimum evidence cardinality (e.g. number of requirements, source files, command runs, events) and causal completion/chronological progression in `crates/ggen-graph/audit/vision2030.self_audit.ocel.json`.
5. Implement `verify_agent_truthfulness.sh` at the workspace root to orchestrate everything: building the crate, running baseline and worktree inventory captures, running the sabotage suite, running clean adjudication, and exiting with 0 on success.
6. Verify everything works by executing `verify_agent_truthfulness.sh` and ensuring it passes and exits with 0 under clean conditions.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.

Please do not use `sed`, `awk` or other stream editors for source files modifications; use python or write the files directly. Make sure all scripts are made executable (`chmod +x`).

Write a handoff report to `handoff.md` inside your working directory when done, including the test command/results, and send a message back.

## 2026-05-26T17:53:12-07:00
You are teamwork_preview_worker.
Your workspace is /Users/sac/ggen.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.

Your objectives:
1. Read the explorer's detailed analysis report at `/Users/sac/ggen/.agents/teamwork_preview_explorer_truthfulness_3/analysis_witnessed.md`.
2. Update `/Users/sac/ggen/scripts/gall/external/run_with_transcript.sh` to output stdout and stderr to separate files, capture the environment, and output a JSON transcript containing the correct metadata fields (argv, cwd, exit_code, duration_ms, stdout_sha256, stderr_sha256, etc.).
3. Implement the following external verifier scripts under `/Users/sac/ggen/scripts/gall/external/` with the exact logic provided in the explorer analysis (make sure they are executable):
   - `20_capture_full_worktree_inventory.sh`
   - `21_verify_command_transcripts.sh`
   - `22_verify_script_adequacy.sh`
   - `23_run_sabotage_suite.sh`
   - `24_run_clean_room_rebuild.sh`
   - `25_verify_cross_artifact_consistency.sh`
   - `26_verify_ocel_causal_sufficiency.sh`
   - `27_verify_contradiction_supersession.sh`
   - `99_adjudicate_witnessed_truthfulness.sh`
4. Implement `/Users/sac/ggen/verify_agent_truthfulness.sh` at the workspace root as the workspace root orchestrator for the Witnessed Agent Truthfulness validation.
5. Update `/Users/sac/ggen/docs/VISION_2030_GALL_PROOF.md` to shift from the old 5-rule shape to the W0–W9/T0-T10 Witnessed Agent Truthfulness GALL checkpoints.
6. Execute `/Users/sac/ggen/verify_agent_truthfulness.sh` using run_command, verify that the compilation is clean, all tests pass, and it outputs 0.
7. Verify that the script `scripts/gall/external/23_run_sabotage_suite.sh` runs successfully, proving that every injected mutation correctly triggers a verification refusal and the cleanup trap restores the workspace correctly.
8. Make sure that no TODO or FIXME or stubs are introduced in the source code or scripts, as per AGENTS.md constitutional constraints.
9. Deliver your handoff.md with passing build/test results, list of files created, command outputs, and the generated audit artifacts.
