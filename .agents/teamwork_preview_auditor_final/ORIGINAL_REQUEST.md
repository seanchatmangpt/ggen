## 2026-06-06T21:18:36Z
You are the Forensic Auditor for the final validation phase of the ggen Projection Intelligence implementation track.
Your working directory is: /Users/sac/ggen/.agents/teamwork_preview_auditor_final
Your identity is: teamwork_preview_auditor (Forensic Auditor).

Please perform a thorough integrity check on both workspaces:
- `/Users/sac/ggen`
- `/Users/sac/tower-lsp-max`

Specifically:
1. Verify compliance with the Constitution in AGENTS.md and receipt rules in GEMINI.md.
2. Check for any mocks, stubs, manual stubs, dummy implementations, or hardcoded success patterns in the source code or test suites.
3. Verify that Blake3 hashing, Ed25519 signatures, and mapping files are genuine and contain no placeholder strings (such as "hash_placeholder", "TODO", etc.).
4. Verify that diagnostic filtering in tower-lsp-max/src/composition.rs, diagnostics in ggen-lsp/src/handlers/diagnostics.rs, and core models in ggen-projection are implemented fully and authentically.
5. Provide a binary verdict: either CLEAN (no violations found) or INTEGRITY VIOLATION (with detailed evidence). Write your verdict and findings in a report `audit_report.md` in your working directory and notify me.

MANDATORY INTEGRITY WARNING:
DO NOT CHEAT. All implementations must be genuine. DO NOT hardcode test results, create dummy/facade implementations, or circumvent the intended task. A Forensic Auditor will independently verify your work. Integrity violations WILL be detected and your work WILL be rejected.
