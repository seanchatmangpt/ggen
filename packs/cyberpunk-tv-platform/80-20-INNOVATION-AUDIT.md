# Cyberpunk Television Platform — 80/20 Innovation Audit

## Audit subject

- Repository: `seanchatmangpt/ggen`
- Base head: `eea5265eed9b790e54df3aae436a98a3b6b115f2`
- Parent work: PR #555, Cyberpunk Television Platform + NASA Dark Mode
- Audit rule: close declared-but-unwired product loops before adding more capability surface.

## Finding

The repository already had the difficult substrate: semantic authority, deterministic manufacture, Rust/WASM bodies, browser projections, doctor/wizard records, 8,640-profile enumeration, receipts, replay, and independent verification.

The largest remaining innovation gap was not another runtime. It was **product legibility and lawful progression**. A developer could prove the system, but could not yet ask one command:

> What exists, what can run here, why was it selected, what evidence do I have, and what may I truthfully claim?

## Weighted gap analysis

| Gap | User consequence | Existing substrate | Weight | Closure |
|---|---|---|---:|---|
| No one-screen orientation | Capability density appears as complexity | Capability graph | 18 | `orient` groups inventory, reports command coverage, names next lawful actions |
| Declared search/explain were unwired | Users must read ontology and generated code manually | `CapCapabilitySearch`, `CapExplain` | 15 | Grounded `search` and source-to-output `explain` |
| Doctor was all-or-nothing | One missing file blocked every check and obscured repair locality | Declarative doctor records | 14 | Per-check required surfaces, independent state, localized repairs |
| Wizard silently accepted invalid values | Profiles could look admitted without belonging to the authority vocabulary | Wizard choices | 12 | Strict refusal, explicit defaults, legacy alias normalization, profile digest |
| Pareto capability had no executable frontier | Combinatorial maximalism enumerated but did not help select | Transport profiles and scores | 10 | Ontology-scored nondominated frontier |
| Evidence was scattered | Receipts existed but users could not navigate claim support | `.ggen` evidence/plan/receipt corpus | 12 | Byte-grounded evidence index and observed aliases |
| No fail-closed release claim | `PARTIAL_ALIVE` depended on expert interpretation | Standing axis | 12 | Ontology-owned release targets and missing-proof refusals |
| No one-command product loop | Verification required tribal sequencing knowledge | All prior capabilities | 7 | `demo` composes orientation → doctor → wizard → plan → frontier → evidence → preview gate |

**Closed weight: 100/100 of the audited 80/20 frontier.**

## New product loop

```text
npm run orient
→ npm run search -- "desired outcome"
→ npm run explain -- <capability>
→ npm run demo
→ npm run evidence
→ npm run release -- --target=<preview|local-alive|device-alive|federated-alive>
```

Each transition emits JSON evidence. Unknown commands, capability IDs, wizard choices, and release targets are typed refusals.

## Claim ceiling

This PR improves product operation and local evidence management. It does not itself establish physical-device or federated execution. Release targets remain fail-closed:

- `preview` requires doctor, wizard, and plan evidence.
- `local-alive` additionally requires verify, replay, and browser evidence.
- `device-alive` additionally requires physical-device evidence.
- `federated-alive` additionally requires transport and federated evidence.

## Remaining 20%

The next frontier is deliberately outside this PR:

1. Real physical Roku execution receipts.
2. Federated multi-node transport execution.
3. A generalized evidence ontology shared across all ggen packs rather than this generated control plane.
4. A native interactive wizard UI; the current command surface is deterministic and scriptable.
