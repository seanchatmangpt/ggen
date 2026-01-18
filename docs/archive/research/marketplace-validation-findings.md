<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Marketplace Validation Findings (current workspace)](#marketplace-validation-findings-current-workspace)
  - [Context](#context)
  - [What happened](#what-happened)
  - [Blocking issues (Andon signals)](#blocking-issues-andon-signals)
  - [Evidence](#evidence)
  - [Proposed minimal fix for `ggen-test-audit`](#proposed-minimal-fix-for-ggen-test-audit)
  - [Proposed fix for `ggen-marketplace` warning](#proposed-fix-for-ggen-marketplace-warning)
  - [Next steps (ordered)](#next-steps-ordered)
- [Marketplace Validation Findings (Next.js Ontology CRUD)](#marketplace-validation-findings-nextjs-ontology-crud)
  - [Context](#context-1)
  - [What we attempted](#what-we-attempted)
  - [Observed issues](#observed-issues)
  - [Minimal fixes applied](#minimal-fixes-applied)
  - [Recommended 80/20 validation](#recommended-8020-validation)
  - [Next steps to harden](#next-steps-to-harden)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Marketplace Validation Findings (current workspace)

## Context
- Goal: run `cargo make marketplace-validate` to check all marketplace packages.
- Preconditions: `cargo make build-release` must succeed (andon gate) because validation script builds the CLI.

## What happened
- `cargo make marketplace-validate` failed immediately because the CLI build failed.
- Subsequent `cargo make build-release` runs surfaced compiler errors in `ggen-test-audit` (and one warning in `ggen-marketplace`).

## Blocking issues (Andon signals)
1) `ggen-test-audit` compilation fails:
   - `AuditError` derive not resolving conversions for `?` on `std::io::Error` and `serde_json::Error` in:
     - `crates/ggen-test-audit/src/assertion_analyzer.rs`
     - `crates/ggen-test-audit/src/false_positive_detector.rs`
     - `crates/ggen-test-audit/src/mutation_analyzer.rs`
     - `crates/ggen-test-audit/src/report_generator.rs`
   - Current `AuditError` enum (in `crates/ggen-test-audit/src/types.rs`) has `#[derive(Debug, Error)]` but is missing reliable `From` conversions and `Display` via the derive (derive is blocked when the macro import is misconfigured).
   - Result: `?` cannot convert I/O and serde_json errors to `AuditError`.

2) `ggen-marketplace` warning (non-blocking for now):
   - Unused import `thiserror::Error` in `crates/ggen-marketplace/src/error.rs` after switching to `#[derive(Error)]`.

## Evidence
- Full build log (latest failure): `/Users/sac/.cursor/projects/Users-sac-ggen/agent-tools/50d056ce-1049-4907-8aa9-4ee4f5f52489.txt`
- Representative error: `the trait From<std::io::Error> is not implemented for AuditError` (multiple sites listed above).

## Proposed minimal fix for `ggen-test-audit`
- In `crates/ggen-test-audit/src/types.rs`:
  - Ensure a single `use thiserror::Error;`.
  - Keep `#[derive(Debug, Error)]` on `AuditError`.
  - Add `#[error("...")]` and `#[from]` on I/O and serde_json variants:
    - `IoError(#[from] std::io::Error)`
    - `JsonError(#[from] serde_json::Error)`
  - Remove redundant manual `From`/`Display` impls (the derive handles them).
  - Keep a single `pub type AuditResult<T> = Result<T, AuditError>;`.
- Re-run `cargo make build-release` to clear the Andon signal, then rerun `cargo make marketplace-validate`.

## Proposed fix for `ggen-marketplace` warning
- In `crates/ggen-marketplace/src/error.rs`, either:
  - Keep `#[derive(Debug, Error)]` and retain `use thiserror::Error;`, or
  - Switch derive back to `#[derive(Debug, thiserror::Error)]` and drop the import.
  - Ensure import/derive combination matches to avoid the unused import warning.

## Next steps (ordered)
1. Apply the `AuditError` cleanup above.
2. Re-run `cargo make build-release` (andon gate).
3. Run `cargo make marketplace-validate`.
4. If needed, run `cargo make marketplace-emit-receipts` and `cargo make marketplace-generate-artifacts`.
5. Tidy the marketplace warning once validation passes.
# Marketplace Validation Findings (Next.js Ontology CRUD)

## Context
- Target: `marketplace/packages/io.ggen.nextjs.ontology-crud` (full-stack Next.js sample).
- Goal: 80/20 validation to catch breakage when files change.

## What we attempted
- Added `scripts/validate.sh` and `npm run validate` to run lint + build with `NEXT_TELEMETRY_DISABLED=1` and `CI=1`.
- Added local override for missing dependency (`baseline-browser-mapping@2.9.0`) via `vendor/baseline-browser-mapping` and `overrides` in `package.json`.
- Install path: `npm install` (falls back if no lockfile), then `npm run lint`, `npm run build`.

## Observed issues
- No `package-lock.json` present. `npm ci` fails; fallback `npm install` pulls transitive `baseline-browser-mapping@^2.9.0` that is not available on the registry. Added a local stub override to unblock install.
- Lint failed because a user-level `~/.eslintrc.js` (parser + @typescript-eslint recommended) was picked up, not the app config.

## Minimal fixes applied
- Local override stub for `baseline-browser-mapping@2.9.0` so installs succeed even without a lockfile.
- Validation script now installs (with fallback) then runs lint/build.
- Lint still reads the user-level ESLint config; to isolate, add a project-local `.eslintrc.json` that extends `next/core-web-vitals`.

## Recommended 80/20 validation
Run from the package root:
```bash
# one-time: add project-local eslint config to avoid user-level bleed
cat > .eslintrc.json <<'EOF'
{ "extends": ["next/core-web-vitals"] }
EOF

npm run validate
```
- This validates:
  - Dependencies resolve (or fail fast if new transitive gaps appear)
  - ESLint for Next.js (no user-level overrides)
  - `next build` (type-check + production bundle)

## Next steps to harden
- Commit a `package-lock.json` to make installs deterministic.
- Add `.eslintrc.json` to the repo to pin lint config.
- Optionally vendor the exact missing dependency or update transitive versions to avoid the override.
- Wire `npm run validate` into CI for this package.




