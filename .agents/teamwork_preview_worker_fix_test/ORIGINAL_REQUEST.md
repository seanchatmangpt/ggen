## 2026-06-06T14:21:57-07:00
You are a worker agent for fixing a test assertion bug.
Your working directory is: /Users/sac/ggen/.agents/teamwork_preview_worker_fix_test
Your identity is: teamwork_preview_worker (Worker Fix Test).

Your tasks are:
1. Edit `/Users/sac/ggen/crates/ggen-projection/tests/t3_pairwise.rs` to correct the assertion at lines 219–223. The test `test_t3_lsp_diagnostic_drift_after_sync` should assert that any non-"GGEN-PROJECTED-001" diagnostic code matches "GGEN-DRIFT-001", or simply check that the correct codes are returned.
Example edit:
```rust
            for d in diags {
                if let Some(code) = d.get("code").and_then(|c| c.as_str()) {
                    if code != "GGEN-PROJECTED-001" {
                        assert_eq!(code, "GGEN-DRIFT-001");
                    }
                }
            }
```
2. Navigate to `/Users/sac/ggen` and run:
   ```bash
   ulimit -n 10240 && cargo test -p ggen-projection
   ```
   Verify that all 71 tests in `ggen-projection` compile and pass.
3. Verify that all other tests in the workspace also pass cleanly.
4. Report back with a summary of the edits and the test execution logs.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
