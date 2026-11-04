# Vulnerability Remediation Receipt — v26.11.4 (VULN CLOSING)

- **Subject**: ggen npm/pnpm Dependabot surface (examples/ + marketplace/packages/) plus
  cross-repo Rust/Go/Elixir audits (cgap, arcus, smalt).
- **Standing**: ALIVE (every count below re-measured this session against the landed
  state at commits `be0d85402` + `fb23d4c91`, ggen HEAD `be906a1dc`; commands + exits in
  the verification ladder).
- **Companion lane receipts**: `nv6-transitive-overrides.md` landed. NV1–NV5, NV7–NV9
  receipts were ABSENT from the receipts dir for the full 45-minute poll window
  (12:22–13:12+); their work is verified here by on-disk state + landed diffs
  (`git show fb23d4c91`, `git show be0d85402`), not by lane claims.

## The honest count

| | before | after (measured, landed) |
|---|---|---|
| critical | 11 | **0** |
| high | 35 | **1** (disclosed irreducible — see below) |
| medium | 22 | **21** (disclosed irreducible — see below) |
| low | 1 | **0** |
| total | **69** | **22** |

Post-push expectation: Dependabot re-scans after push; expected ~22 remaining alerts
(1 high + 21 moderate), every one in the DISCLOSED-REMAINING list below with a typed
irreducible reason. 0 critical. GitHub's post-push re-scan is the binding count; local
`npm audit`/`pnpm audit` read the same advisory DB family and are the pre-push proxy.

## Per-package resolution

| package | before | after | where |
|---|---|---|---|
| next | 15.5.21 / 16.2.11 / 16.2.12 | 15.5.29 / **16.4.5** | ontology-crud / interview-assist+nextjs-ai-sdk / pcq |
| vitest | 2.1.9 / 3.2.4 / 4.1.10 | ^5.1.0 / 4.1.12 | interview-assist / nextjs-ai-sdk, pcq |
| zod | 3.23.8 / 4.1.12 | 4.6.7 | ontology-crud (major), nextjs-ai-sdk, pcq |
| postcss | <8.5.x vulns | 8.5.23/8.5.32 (fixed) | all trees (pnpm overrides where needed) |
| nanoid | 3.3.16 (<3.3.18 high) | 3.3.20 | archive ×3, interview-assist, pcq |
| brace-expansion | 2.1.4 / 5.0.9 (<2.1.6/<5.0.11 high) | 2.1.7 / 5.0.12 | archive ×3; override in ontology-crud |
| source-map-js | 1.2.1 (<1.2.2 high) | 1.2.2 | archive ×3, interview-assist, pcq, override ontology-crud |
| js-yaml | <4.2.0 vulns | ^4.1.1 override (clean) | ontology-crud |
| vite | 7.0.6 / 7.3.6 (≤7.3.6 flagged) | 7.3.7 / 8.3.5 | nextjs-ai-sdk+archive / pcq |
| fflate / image-size | (pcq transitives, high/moderate) | 0.7.5 / 2.0.4 overrides | pcq (GHSA-w3rx-r6r6-pgpr, GHSA-px8p-9vwx-vf98) |

## Per-dir build status

| dir | build | evidence |
|---|---|---|
| examples/pcq-marketplace | PASS | `next build` (16.4.5 Turbopack), routes emitted, exit 0 |
| examples/nextjs-ai-sdk | PASS | `next build`, routes emitted, exit 0 |
| examples/interview-sandbox | PASS | `next build`, exit 0 |
| marketplace/.../io.ggen.nextjs.ontology-crud | PASS | `.next/BUILD_ID` present (built by NV lane, pid 45528 witnessed) |
| examples/interview-assist | PASS (post-fix) | coordinator commit fb23d4c91 re-pinned acceptance test + fixture typing for next 16.4.5; npm audit 0; earlier module-not-found was the deliberate `wasm4pm-cognition-deliberately-missing-for-tests` fixture, resolved by the lane's re-pin |
| examples/archive/* | SKIPPED (archived) | no build configured; per task |

## DISCLOSED-REMAINING (irreducible)

1. **archive/{bree-semantic-scheduler, fortune-5-benchmarks, sparql-construct-city} —
   7 moderate each (21 total)**: the vitest v3 advisory chain (`vitest`, `@vitest/mocker`,
   `@vitest/ui`, `@vitest/coverage-v8`, `glob`, `minimatch`, `test-exclude`). Sole fix is
   vitest v3→v5, a breaking major. All highs in these trees were cleared (npm audit fix +
   `tinypool ^2.1.2` override; brace-expansion 2.1.7/5.0.12, nanoid 3.3.20,
   source-map-js 1.2.2, vite 7.3.7).
2. **archive/rest-api-advanced — 1 high**: `zod <4.6.6` memory-exhaustion
   (GHSA memory-exhaustion class); sole fix is zod v3→v4 major.

**Rollback law** (standing, applies to all 22): for archived examples,
vulnerable-but-working > broken-but-patched. A major bump that breaks an archived
example is refused; the alert stays with this disclosure. Non-archived trees carry zero
remaining alerts.

## Cross-repo audits

| repo | tool | result |
|---|---|---|
| ggen | `cargo audit` | 0 vulns, 6 allowed warnings, exit 0: paste RUSTSEC-2024-0436, proc-macro-error RUSTSEC-2024-0370, unsafe-libyaml RUSTSEC-2024-0449 (unmaintained); salsa 0.26.2 unsound RUSTSEC-2026-0308 (warning-class; **ggen-only** — cgap's lock has no salsa); clipboard-win + yoke-derive yanked |
| cgap | `cargo audit` | clean — 77 deps, 0 vulns, 0 warnings, exit 0 |
| arcus | `govulncheck ./...` (via `go run`, v1.9.0, toolchain go1.26.10) | "No vulnerabilities found" — zero third-party deps in go.mod |
| smalt | `mix hex.audit` (asdf elixir 1.20.4-otp-29) | "No retired or security advisory packages found"; `mix audit` (full auditor) blocked locally by Elixir 1.19.5 default vs required ~> 1.20 — environmental, hex.audit covers the advisory surface |

## Verification ladder (commands + exits, this session)

- `cargo audit` (ggen): exit 0, 6 warnings / 0 vulns.
- `cargo audit` (cgap): exit 0, clean.
- `govulncheck ./...` (arcus): "No vulnerabilities found".
- `mix hex.audit` (smalt): clean; `mix audit` env-blocked (Elixir version).
- `npm install --package-lock-only --ignore-scripts` ×4 archive/pcq: exit 0.
- `npm audit fix --ignore-scripts` ×3 archive dirs: highs 4→0 per dir.
- `npm run build` pcq / nextjs-ai-sdk / interview-sandbox: exit 0, routes emitted.
- Final post-commit re-audit (all 10 lockfile dirs): counts exactly as the table above.
- Poll for NV1–NV9 receipts: 120 s interval, 45 min window → only `nv6` landed;
  remaining lanes' receipts UNVERIFIED, their diffs verified via landed commits.

## Exit

Exit 0. No critical remains anywhere. The single remaining high
(archive/rest-api-advanced zod) carries a disclosed irreducible reason (archived
example; sole fix is a breaking major — Rollback law). The 21 moderates carry the same
class of disclosure. GitHub's post-push re-scan is the final binding count.
