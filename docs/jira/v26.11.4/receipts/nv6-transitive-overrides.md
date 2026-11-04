# NV6 — Transitive-Dep Override Lane Receipt (v26.11.4)

- **Subject**: examples/ + marketplace/packages/ JS dirs, Dependabot transitive alerts
  (brace-expansion DoS ×2, nanoid integer overflow ×3, source-map-js event-loop DoS).
- **Standing**: ALIVE (executed this session; commands + exits below).
- **Outcome: ZERO override blocks warranted.** The task's own gate was "add `overrides`
  ONLY where the transitive dep actually appears in the lockfile with a vulnerable
  version" — after NV1–NV5's parent bumps landed, no lockfile in scope resolves any of
  the three transitives to a vulnerable version. Adding overrides would violate that gate;
  none were added. This lane made zero file mutations.

## Sweep (per-dir)

| Dir | Lockfile | brace-expansion | nanoid | source-map-js | audit (transitive targets) |
|---|---|---|---|---|---|
| examples/interview-assist | package-lock.json | — (not flagged) | 3.3.20 (fixed ≥3.3.18) | 1.2.2 (fixed ≥1.2.2) | `npm audit` → 0 vulnerabilities (pre-NV baseline flagged nanoid high <3.3.18 + source-map-js high <1.2.2 — resolved by NV's next@16.4.5 / vitest@5.1.0 bumps) |
| marketplace/packages/io.ggen.nextjs.ontology-crud | package-lock.json | 2.1.7 (fixed ≥1.1.21 / 2.x ≥2.0.2) | 3.3.20 (not flagged) | 1.2.2 | `npm audit` → 0 vulnerabilities (baseline flagged brace-expansion <1.1.21 + source-map-js <1.2.2 — resolved by parent bumps) |
| examples/nextjs-ai-sdk | pnpm-lock.yaml | — | — | — | `pnpm audit` → "No known vulnerabilities found" |
| examples/interview-sandbox | pnpm-lock.yaml | — | — | — | `pnpm audit` → "No known vulnerabilities found" |
| examples/pcq-marketplace | pnpm-lock.yaml | — | — | — | `pnpm audit` → 2 vulns, but image-size (high) + fflate (moderate) — NOT the three in-scope transitives; out of this lane's ownership |
| marketplace/packages/notification-messaging-hub | none (empty deps `{}`) | n/a | n/a | n/a | skipped — no JS dependency graph to audit |
| examples/archive/* (5 with lockfiles: bree-semantic-scheduler, fortune-5-benchmarks, sparql-construct-city, + 2 without) | package-lock.json ×3 | 5.0.12 | 3.3.20 | 1.2.2 | all fixed versions; archived → builds skipped per task |

## Commands / exits

- Bounded poll for NV1–NV5 receipts, 120 s interval, 20 min deadline (12:22–12:42): 0 NV
  receipts appeared; `git status --porcelain examples marketplace/packages` stabilized at
  25 dirty files from 12:28 (mtime-stability signal used since receipts were absent).
- `npm audit --json` ×2, `npm ls` ×2, `pnpm audit` ×3 — exits 0/1 as shown above.
- `npx tsc --noEmit`: io.ggen.nextjs.ontology-crud rc=0; interview-assist rc=0.
- Mutations by this lane: none (`git diff` in scope is entirely NV1–NV5's parent bumps).

## Notes for coordinator

1. **Premise rot, honestly reported**: the override layer's precondition (vulnerable
   transitives in lockfiles) was consumed by NV1–NV5's parent bumps while this lane
   polled. Receipts for NV1–NV5 still absent at 12:42 — verify their receipts before
   integration; this lane's audit evidence is keyed to on-disk state at 12:21–12:51.
2. pcq-marketplace's image-size/fflate alerts need an owner (not NV-override scope).
3. nanoid caveat if overrides are ever reintroduced: pinning nanoid ^5.1.5 over a 3.x
   consumer (postcss) breaks CJS `require('nanoid')` — the lawful override there is
   `^3.3.8`, not `^5.1.5`. Task's literal "≥5.1.5" is unsafe for postcss@8 chains.
