# InterviewAssist v26.7.24-deferred — Deferred/follow-on backlog index

## Honest framing

This is **not** a "the epic failed" document. Every real production epic accumulates a deferred
backlog — its absence is the red flag, not its presence. This backlog exists because the
v26.7.23 InterviewAssist epic's own 57 tickets were honest about their own limits: "Disclosed,
unfixed risk," "Implementation notes," and "N/A with reason" fields already in those ticket
files are the raw material cataloged here, not gaps this document discovered by catching
v26.7.23 in an omission.

Three explicit non-claims, stated once here and carried through every ticket below:

- This backlog does **not** claim v26.7.23 should have scoped workstream L's items
  (TICKET-D015..D025) — a demo/proof-of-concept manufacturing epic reasonably excludes
  production hardening (container isolation, auth, CI/CD, deployment, observability, i18n, load
  testing), and v26.7.23's own `EPIC.md` never claims production readiness.
- This backlog does **not** claim workstream K's items (TICKET-D001..D014) are defects — several
  are genuine engine-capability limits (D002) or timing artifacts of the concurrently-running
  implementation workflow (D012, D013) that resolve on their own as that workflow proceeds.
- This backlog does **not** claim workstream M's items (TICKET-D026..D031) mean the underlying
  tests are wrong — they are real and pass; they cover one concrete instance of a broader claim,
  a normal, disclosed verification-maturity gap, not a fabrication.

This backlog does **not** make, and is not authorized to make, any ALIVE/PARTIAL_ALIVE/BLOCKED
determination about the v26.7.23 epic itself. That determination belongs exclusively to
v26.7.23's own TICKET-057 (Final Verifier Report), per that epic's own `EPIC.md` "Completion
state criteria." See TICKET-D014 for this backlog's own tracking of that report's status.

## Source reports

Three independent, read-only research passes over `docs/jira/v26.7.23/`,
`packs/wasm4pm-interview-assist-pack/`, and `examples/interview-assist/`, each generating one of
this backlog's three workstreams:

1. **Disclosed Gaps Catalog** → Workstream K (TICKET-D001..D014) — limitations the 57-ticket
   epic's own ticket files already state in their own words.
2. **Out-of-Scope Production Work Report** → Workstream L (TICKET-D015..D025) — production
   hardening confirmed absent from the 57-ticket epic by direct grep/read, never scoped at all.
3. **Thin Verification Report** → Workstream M (TICKET-D026..D031) — real, passing tests whose
   coverage is narrower than the property their ticket's own prose implies.

See `EPIC.md` for the full program framing, priority legend, and numbering rationale.

## Ticket table

| ID | Title | Priority | Report | Relates to |
|---|---|---|---|---|
| D001 | Subprocess sandbox has no OS-level isolation beyond workspace-escape check | P0 | Disclosed Gaps (a) | TICKET-035 |
| D002 | lib/planes/index.ts emits one file, not one folder per plane (engine capability gap) | P2 | Disclosed Gaps (a) | TICKET-012 |
| D003 | 5 of 14 SHACL shapes have no SPARQL-gate equivalent enforced at sync time | P1 | Disclosed Gaps (a) | TICKET-009 |
| D004 | next.config.ts / tsconfig.json project templates not yet written | P1 | Disclosed Gaps (a) | TICKET-011 |
| D005 | policy-check-stub.ts default-allow placeholder called by 5 of 6 workstream-H adapters | P0 | Disclosed Gaps (a) | TICKET-034/035/036/037/039 |
| D006 | policy-check-stub.ts is an orphan file with no owning ticket in the custom-code manifest | P2 | Disclosed Gaps (a) | TICKET-054 |
| D007 | Capability/route projection: unreconciled 7-vs-9 count + refusal.ts not wired into CapabilityResponse | P1 | Disclosed Gaps (a) | TICKET-026/027 |
| D008 | Playwright + accessibility harness BLOCKED — no app-shell dev server exists to drive it | P1 | Disclosed Gaps (b) | TICKET-039 |
| D009 | Monaco editor DOM mount is UNVERIFIED — never run against a real browser | P1 | Disclosed Gaps (b) | TICKET-034 |
| D010 | Persistence adapter's real browser IndexedDB/localStorage behavior never exercised | P1 | Disclosed Gaps (b) | TICKET-036 |
| D011 | TypeScript/test config generation BLOCKED on cross-workstream package.json conflict | P1 | Disclosed Gaps (b) | TICKET-014 |
| D012 | Projection idempotency only exercised for 1 of N eventual templates | P2 | Disclosed Gaps (b) | TICKET-010 |
| D013 | Projection receipt missing TICKET-010's output_manifest input + stale workstream-H cross-check | P2 | Disclosed Gaps (b) | TICKET-054 |
| D014 | TICKET-057 Final Verifier Report not started — no epic-level ALIVE determination exists yet | P1 | Disclosed Gaps (b) | TICKET-057 |
| D015 | Real container/microVM sandbox isolation for candidate code execution | P0 | Out-of-Scope #1 | — |
| D016 | Multi-tenant session isolation, authentication, and per-tenant resource quotas | P0 | Out-of-Scope #2 | — |
| D017 | Real server-side database-backed persistence | P2 | Out-of-Scope #3 | — |
| D018 | CI/CD pipeline wiring for examples/interview-assist | P1 | Out-of-Scope #4 | — |
| D019 | Production deployment target, environment-variable management, secrets handling | P1 | Out-of-Scope #5 | — |
| D020 | OpenTelemetry observability/instrumentation | P1 | Out-of-Scope #6 | — |
| D021 | Rate limiting and DoS protection on the subprocess-dispatching API route | P0 | Out-of-Scope #7 | — |
| D022 | Security review / adversarial penetration testing of the sandbox execution boundary | P0 | Out-of-Scope #8 | — |
| D023 | Accessibility certification by real assistive-technology users | P1 | Out-of-Scope #9 | — |
| D024 | Internationalization / localization (entirely unstated, not merely deferred) | P2 | Out-of-Scope #10 | — |
| D025 | Load/stress testing at realistic concurrent-user scale | P1 | Out-of-Scope #11 | — |
| D026 | Tamper detection: real coverage narrower than TICKET-049's BLAKE3 hash-divergence target | P1 | Thin Verification #1 | TICKET-025/049 |
| D027 | Self-play admission verified via exactly one live Ollama call; no repeated-trial coverage | P2 | Thin Verification #2 | TICKET-037/052 |
| D028 | Component tests SSR-string-only; zero interaction/keyboard/ARIA testing | P1 | Thin Verification #3 | TICKET-030/031/032/033 |
| D029 | Sandbox negative tests miss fork-bomb/disk-fill/memory-exhaustion/network-egress attack classes | P0 | Thin Verification #4 | TICKET-035 |
| D030 | Chaos/Stress ladder rows mostly N/A; several non-N/A rows have no corresponding real test | P2 | Thin Verification #5 | TICKET-023/025/029/034-039/040-053 |
| D031 | Idempotency verified via exactly 2 consecutive syncs only; no N-run/concurrent testing | P2 | Thin Verification #6 | TICKET-010/017/018/030 |

## Priority summary

- **P0 (7 tickets)** — D001, D005, D015, D016, D021, D022, D029. All seven cluster around one
  real production risk: the subprocess sandbox has no OS-level isolation, no adversarial testing
  against that gap, no authentication in front of it, no rate limiting, and every adapter's
  policy-enforcement point is a default-allow stub. These block any real exposure beyond a
  single trusted local operator.
- **P1 (14 tickets)** — D003, D004, D007, D008, D009, D010, D011, D014, D018, D019, D020, D023,
  D025, D028. Real hardening or verification-completeness work needed before scaling past a
  single-operator demo.
- **P2 (10 tickets)** — D002, D006, D012, D013, D017, D024, D026, D027, D030, D031. Nice-to-have
  completeness; several (D012, D013) are expected to self-resolve as the concurrently-running
  v26.7.23 implementation workflow lands more tickets.

Total: **31 tickets** across 3 workstreams (K: 14, L: 11, M: 6), each grounded in a direct
citation from one of the three source research reports — see each ticket's own `## Source`
section for the exact quote and file:line.

## What this backlog does not do

- Does not write, edit, or delete anything under `packs/wasm4pm-interview-assist-pack/`,
  `examples/interview-assist/`, or `docs/jira/v26.7.23/`.
- Does not invent gaps beyond what the three source research reports identified — every ticket
  traces to a specific, quoted finding.
- Does not assign an implementation schedule or owners.
- Does not judge v26.7.23's own completion status.

## See also

- `EXECUTIVE-SUMMARY.md` — plain-language status for a technical decision-maker: what this
  release's real vertical slice demonstrates (with test evidence) versus what still separates it
  from Fortune-5/enterprise deployment readiness (the 7 P0 tickets, re-verified 2026-07-24)
- `EPIC.md` — full program framing, priority legend, numbering rationale
- `docs/jira/v26.7.23/EPIC.md` — the predecessor epic this backlog defers from
- `docs/jira/v26.7.23/README.md` — the predecessor epic's own 57-ticket index
