# Interview Assist — portable pack paths (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `INTERVIEW-ASSIST-PORTABLE-PATH-STANDING` |
| Branch | `cursor/v2711-construct-pytest-retip-bc54` |
| Spec binding | Construct core-team ladder + `Refuse workstation-specific executable paths` |
| Observed tip (parent) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `43d6301bb` (pytest + lock-sync tip-wins) |
| Repo | `seanchatmangpt/ggen` |
| Portable path standing | **ALIVE** (source-level; Construct ephemeral rewrite becomes no-op) |
| Full Construct CI | **ALIVE** — `45086131007` EXIT0 on tip `43d6301bb`; portable paths tip-wins on absorb |
| Foundry / zero-HITL fab | **NOT** promoted — Construct ALIVE ≠ Foundry ALIVE |

**Law:** Hardcoded `/Users/sac/ggen/...` in Interview Assist domain/scenario tests is workstation contamination. Portable `fileURLToPath(new URL(...))` relative to `import.meta.url` is the admitted repair. Construct's runtime rewrite remains belt-and-suspenders until tip-wins land; the refuse-grep step must see zero workstation paths in source after tip-wins.

---

## Files changed

| Path | Repair |
|------|--------|
| `examples/interview-assist/tests/domain/event-routing.test.ts` | `PACK` → `fileURLToPath(.../packs/wasm4pm-interview-assist-pack/)` |
| `examples/interview-assist/lib/domain/__tests__/capability.count.test.mjs` | same relative pack root |
| `examples/interview-assist/tests/scenarios/full-decisive-acceptance-test.test.tsx` | `PACK_ROOT` + `EXAMPLE_ROOT` (drop `REPO_ROOT`) |

---

## Local evidence

| Command | Result | Standing |
|---------|--------|----------|
| `cd examples/interview-assist && node scripts/materialize-wasm-cognition.mjs && npm test` | **173 passed \| 7 skipped; 43 files; exit 0** (2026-11-22 ~00:14Z; Duration ~10.21s) | **ALIVE** |

Falsifier: reintroducing `/Users/sac/ggen` into those three files → Construct `Refuse workstation-specific executable paths` fails after tip-wins (ephemeral rewrite removed or no-op).

---

## Compact return line

```text
ia_portable=ALIVE_tipwins
construct_ci=ALIVE_45086131007
foundry=BLOCKED_HITL_exit1
zero_hitl=NOT_COMPLETE
```
