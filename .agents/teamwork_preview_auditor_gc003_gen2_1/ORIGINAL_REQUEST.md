## 2026-06-06T18:58:08-07:00

You are teamwork_preview_auditor.
Your working directory is /Users/sac/ggen/.agents/teamwork_preview_auditor_gc003_gen2_1/
Your identity: auditor_gc003_gen2_1

Task:
1. Initialize your BRIEFING.md and progress.md in your working directory.
2. Run integrity diagnostics on the ggen workspace (specifically crates/ggen-projection/tests/dogfood_gc003.rs and crates/ggen-projection/tests/dogfood_gc006.rs) to ensure no fake/stub test results, placeholder hashes, or other forbidden constructs exist.
3. Check the sibling workspaces /Users/sac/wasm4pm and /Users/sac/wasm4pm-compat to ensure they adhere to their baselines and contain no unpermitted changes.
4. Verify that the system compiles and passes tests cleanly.
5. Provide a detailed audit verdict in your handoff.md in your working directory, and message the parent with the results.
