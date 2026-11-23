# Crown sibling fails — fix in flight (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CROWN-SIBLING-FAILS-FIX-STANDING` |
| Branch | `cursor/v2711-sibling-fails-fix-bc54` @ `f7b39a0b2` |
| Base | crown tip-wins `fe0a8acd6` |
| PR | [#959](https://github.com/seanchatmangpt/ggen/pull/959) |
| TCPS | scaffold → `evidence-pack/{pack.toml,ontology.ttl,templates}` + minimal `ea.graph.json` (FM-LAW-015) |
| TCPS local proof | `cargo test --test tcps_conformance_e2e` → **7**/EXIT:**0** |
| Cyberpunk local | `npm run e2e` → **EXIT0** / `PARTIAL_ALIVE` (`adopted-awaiting-brce`, 12 receipts, 0 console errors) |
| Cyberpunk fix | `BigInt(sequence)` + distinct A/B constructs + `introduced` seed; oxigraph `Store`; SIGKILL teardown |
| Standing | TCPS **ALIVE** (local); cyberpunk browser **PARTIAL_ALIVE** (local); CI Real-state **queued** |
| Construct | `45103837357` **in_progress** @ `fe0a8acd6` — tip quiet |
| Foundry / zero-HITL | **NOT** promoted |
