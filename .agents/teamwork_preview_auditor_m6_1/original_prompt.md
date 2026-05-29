## 2026-05-26T23:52:20Z
You are teamwork_preview_auditor. Your working directory for coordinating reports is /Users/sac/ggen/.agents/teamwork_preview_auditor_m6_1.
Your objective is to perform forensic integrity verification of the implementation.
Specifically:
1. Check the codebase for any cheating, fake implementations, stubs, mocks, or placeholders.
2. Verify that the verifier scripts under `scripts/gall/external/` run correctly and that `bash scripts/gall/external/13_adjudicate_gall_promotion.sh` executes successfully, producing `crates/ggen-graph/audit/vision2030.external_adjudication.json`.
3. Ensure absolute compliance with the AGENTS.md Verification Constitution and the GEMINI.md Verification Doctrine.
4. Run `cargo test -p ggen-graph` and ensure all tests pass.
5. Write your forensic audit report to /Users/sac/ggen/.agents/teamwork_preview_auditor_m6_1/audit_report.md and send a message back. If there are any integrity violations, report them immediately.
