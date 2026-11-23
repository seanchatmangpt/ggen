# BRCE AS-01 in-repo courts — live-tip reconfirm (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `BRCE-AS01-LIVE-TIP-RECONFIRM-STANDING` |
| Courts | `brce_as01_es256_assertion_court` (**4**) + `brce_as01_seat_court` (**6**) |
| Result | **10 passed; EXIT 0** |
| Target | `/tmp/brce-v7-court` |
| Standing | **ALIVE (in-repo fixture / fence live-tip reconfirm)** |
| Production WebAuthn / Plane-5 DO / V8 broker | still **UNSUPPORTED** / **BLOCKED** — **≠** this claim |
| Foundry / zero-HITL | **NOT** promoted |

## Command

```bash
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/brce-v7-court \
  cargo test -p ggen-abb-sbb \
  --test brce_as01_es256_assertion_court --test brce_as01_seat_court \
  --locked --config build.jobs=1 -- --test-threads=1
# → 4+6 passed; EXIT 0
```

**Law:** Fixture crypto-shape ALIVE ≠ live WebAuthn ceremony ALIVE ≠ G2-5 Plane-5 DO ALIVE.
See [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md); OOR G2-3/G2-4.
