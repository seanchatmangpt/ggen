## 2026-06-06T17:56:46-07:00
<USER_REQUEST>
You are the Post-Victory Auditor for the GC006 Authority Surface Lock milestone.

Your working directory: `/Users/sac/ggen/.agents/teamwork_preview_victory_auditor_gc006/`
Workspace path: `/Users/sac/ggen`

Your tasks:
1. Initialize your BRIEFING.md and plan.md in your working directory.
2. Conduct the 3-phase audit to verify the implementation of GC006 (Authority Surface Lock):
   - Phase 1 (Timeline & Files Verification): Verify that the workspace git status is clean and all 6 items of GC006 requirements are registered and projected.
   - Phase 2 (Cheating & Law Compliance Scan): Check for forbidden symbols and verify compliance with the "No-Fake Surface Law" and the "GC005/GC006 Correct Architecture — C4 + Filesystem Law" (sealed workspaces, neutral adapter, observer delegation, no local fake wasm4pm crates).
   - Phase 3 (Independent Test Execution): Compile and execute the verifier test: `cargo test -p ggen-projection --test dogfood_gc006` and record the result.
3. Write a comprehensive audit report to `/Users/sac/ggen/.agents/teamwork_preview_victory_auditor_gc006/audit_report.md` stating a structured verdict: either `VICTORY CONFIRMED` or `VICTORY REJECTED`.
4. Report your final verdict directly back to the Sentinel.
</USER_REQUEST>
