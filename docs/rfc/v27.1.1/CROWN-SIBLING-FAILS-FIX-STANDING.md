# Crown sibling fails — fix in flight (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CROWN-SIBLING-FAILS-FIX-STANDING` |
| Branch | `cursor/v2711-sibling-fails-fix-bc54` |
| Base | crown tip-wins `fe0a8acd6` (Construct ALIVE@45098816614 on prior tip `99514910c`) |
| TCPS | scaffold copies `evidence-pack/pack.toml` (was stale `evidence/pack.toml` → NotFound) |
| Cyberpunk | surface `WASM REFUSED:…` on boot failure; e2e dumps state/console on WASM READY block (90s) |
| Standing | **PARTIAL** — TCPS path fix landed; cyberpunk root cause still needs CI re-run evidence |
| Foundry / zero-HITL | **NOT** promoted |

Prior fail pin: [`CROWN-SIBLING-FAILS-99514910-STANDING.md`](./CROWN-SIBLING-FAILS-99514910-STANDING.md).
