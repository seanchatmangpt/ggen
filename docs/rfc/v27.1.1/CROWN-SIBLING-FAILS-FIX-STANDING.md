# Crown sibling fails — fix in flight (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CROWN-SIBLING-FAILS-FIX-STANDING` |
| Branch | `cursor/v2711-sibling-fails-fix-bc54` @ live tip |
| Base | crown tip-wins `fe0a8acd6` |
| TCPS | scaffold → `evidence-pack/{pack.toml,ontology.ttl,templates}` + minimal `ea.graph.json` (FM-LAW-015) |
| TCPS local proof | `cargo test --test tcps_conformance_e2e` → **7**/EXIT:**0** (`/tmp/tcps-e2e-fix`) |
| Cyberpunk | `WASM REFUSED` boot surface + e2e waits READY\|REFUSED then asserts READY with dump |
| Standing | TCPS **ALIVE** (local); cyberpunk **PARTIAL** (await CI typed dump) |
| Foundry / zero-HITL | **NOT** promoted |
