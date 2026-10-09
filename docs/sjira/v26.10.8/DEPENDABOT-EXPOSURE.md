# Dependabot Exposure — seanchatmangpt/ggen

As-of: 2026-10-09, at HEAD `10a17c2c6c0b18e7be98524dc9c1d6d9741f17b0` (spec-integration).

Source: `gh api repos/seanchatmangpt/ggen/dependabot/alerts --paginate` (state=open), queried 2026-10-09. Counts and every row below are quoted from that response.

**Totals: 52 open alerts** — 9 critical, 26 high, 16 medium, 1 low. (R109's 52/9 figures confirmed.)

Manifest paths are as GitHub reports them. Several alerted example manifests were subsequently moved under `examples/archive/`, so some alert paths are stale relative to disk (`examples/archive/sparql-construct-city`, `examples/archive/bree-semantic-scheduler`, `examples/archive/fortune-5-benchmarks`).

## Criticals (9) — classification

Current versions read from the manifests at HEAD; first-patched versions from `security_vulnerability.first_patched_version`.

| Alert #s | Package | Current | First patched | Same major? | Class |
|---|---|---|---|---|---|
| 75, 76, 77, 78 | next (io.ggen.nextjs.ontology-crud, pkg + lock, runtime) | 15.5.21 | 15.5.24 | yes (15.x) | fixable-by-lockfile bump |
| 9, 11 | vitest (bree-semantic-scheduler, dev) | ^2.1.9 | 3.2.6 | no (2 → 3) | needs-major-upgrade |
| 8, 10 | vitest (sparql-construct-city, dev) | ^2.1.9 | 3.2.6 | no (2 → 3) | needs-major-upgrade |
| 7 | vitest (fortune-5-benchmarks, dev) | ^2.1.9 | 3.2.6 | no (2 → 3) | needs-major-upgrade |

4 of 9 critical rows (all `next`) clear with a semver-compatible 15.5.21 → 15.5.24 bump.
5 of 9 (all `vitest`) require the vitest 2 → 3 major upgrade.

The only runtime-exposed critical is `next` in `marketplace/packages/io.ggen.nextjs.ontology-crud/`. All vitest criticals are development-scope in archived example apps.

## Full open-alert table

FP = first patched version. All FPs read from `security_vulnerability.first_patched_version` (the top-level `security_advisory.first_patched_version` is null on every row).

### next — io.ggen.nextjs.ontology-crud (pkg + lock), runtime scope

| Severity | Alerts | Vulnerable ranges | FP |
|---|---|---|---|
| critical | 75, 76, 77, 78 | >= 13.4.0 < 15.5.24; >= 10.0.0 < 15.5.24 | 15.5.24 |
| high | 21, 22, 36, 37, 38, 39 | >= 14.1.1, >= 13.0.0, >= 12.0.0 — all < 15.5.21 | 15.5.21 |
| medium | 28, 29, 30, 31, 32, 33, 34, 35 | >= 13.0.0 < 15.5.21 (4 advisories, pkg + lock each) | 15.5.21 |

All 16 next alerts collapse to one action: bump next to >= 15.5.24.

### vitest — dev scope (examples, now archived)

| Severity | Alerts | Manifests | Vulnerable range | FP |
|---|---|---|---|---|
| critical | 7, 8, 9, 10, 11 | bree-semantic-scheduler, sparql-construct-city, fortune-5-benchmarks (pkg + lock) | < 3.2.6 | 3.2.6 |

### Other packages

| Severity | Alerts | Package | Manifests | Scope | FP |
|---|---|---|---|---|---|
| high | 26, 24, 25 | brace-expansion (< 1.1.16) | bree, sparql, ontology-crud locks | dev | 1.1.16 |
| high | 62 | brace-expansion (>= 2.0.0 < 2.1.2) | ontology-crud lock | dev | 2.1.2 |
| high | 74 | browserslist (<= 4.28.6) | ontology-crud lock | dev | 4.28.7 |
| medium 19, high 20/50/79 | js-yaml (4.x ranges) | ontology-crud lock | dev | 4.3.2 clears all |
| high | 71, 72, 70, 55, 60 | nanoid (< 3.3.12 / < 3.3.16 / < 3.3.18) | 3 locks | dev (lock), runtime (71, 55, 60) | 3.3.18 clears all |
| high/medium | 41–44, 23, 27, 57, 58, 46, 1 | postcss (<= 8.5.x ranges) | 3 locks + ontology-crud lock (runtime, alert 1) | dev/runtime | 8.5.23 clears all |
| high/medium | 15, 16, 17, 18 | vite (<= 6.4.2) | bree, sparql locks | dev | 6.4.3 clears all |
| low | 67 | postcss-selector-parser (>= 6.1.0 < 6.1.3) | ontology-crud lock | dev | 6.1.3 |

## Proposed update batch (proposal only — no PRs, no manifest edits)

One batch, runtime-critical first:

1. **`marketplace/packages/io.ggen.nextjs.ontology-crud/`** — floor `next >= 15.5.24` (from 15.5.21; same major, lockfile-compatible). This single floor clears all 16 next alerts (4 critical + 8 high + 4 medium) including the only runtime-critical exposure, and pulls the dev-scope transitive surface (js-yaml 4.3.2, nanoid 3.3.18, postcss 8.5.23, browserslist 4.28.7, brace-expansion 2.1.2/1.1.16, postcss-selector-parser 6.1.3) via a lockfile regen (`npm install` in the package dir).
2. **`examples/{bree-semantic-scheduler,sparql-construct-city,fortune-5-benchmarks}/`** (on disk under `examples/archive/`) — floor `vitest >= 3.2.6`. This is the vitest 2 → 3 major upgrade; expect config/API breakage (workspace config, `coverage.all`, et al.), so batch it as its own commit with the example test suites run as the gate. All other example dev-deps (postcss, vite, nanoid, brace-expansion) clear via the same lockfile regen.

Excluded from the batch: nothing — 52/52 alerts are covered by the two floors above plus lockfile regen.

Risk note: batch 1 is low risk (patch-level within 15.x, Next.js 15.5.x line). Batch 2 is the only major upgrade; it touches archived examples only, no marketplace or core surface.
