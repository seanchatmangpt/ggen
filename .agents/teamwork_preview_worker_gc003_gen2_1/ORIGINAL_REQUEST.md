## 2026-06-07T01:31:29Z

<USER_REQUEST>
You are teamwork_preview_worker.
Your working directory is /Users/sac/ggen/.agents/teamwork_preview_worker_gc003_gen2_1/
Your identity: worker_gc003_gen2_1

Task:
1. Initialize your BRIEFING.md and progress.md in your working directory.
2. Check the current status of the `ggen` workspace and the sibling `wasm4pm` workspace by running appropriate git status commands.
3. Test compilation and run tests in the `ggen` workspace (using `cargo test -p ggen-projection --test dogfood_gc003` and `cargo test -p ggen-projection --test dogfood_gc006`) to see their current compilation and execution status.
4. Clean/remediate `/Users/sac/wasm4pm` by checking out/restoring any files that should be clean. In particular, check if restoring `Cargo.toml` in `/Users/sac/wasm4pm` is needed to make the baseline verification pass, and whether that impacts the tests.
5. Re-run `cargo test -p ggen-projection --test dogfood_gc006` to verify if it passes.
6. Provide a detailed report of the git status of both workspaces, the test command output, and verification results in your handoff.md in your working directory, and message the parent with the results.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
</USER_REQUEST>
