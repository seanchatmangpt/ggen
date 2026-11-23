# R-01…R-07 risk-register bodies — exact next verifier pin (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `R01-R07-BODIES-NEXT-VERIFIER-STANDING-27.1.1` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §11 / D7; OOR **G3-5** / RC-D7 |
| Companions | [`R01-R07-STANDING.md`](./R01-R07-STANDING.md); [`fixtures/r01-r07-sovereign-risk-register.md`](./fixtures/r01-r07-sovereign-risk-register.md) |
| Admitted register | Verbatim zcode-cli `docs/ecosystem-spec-27.1.1.md` §4 @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Status | Bodies **ADMITTED**; **R-06 CLOSED**; R-01…R-05+R-07 **OPEN** (R-05 PARTIAL aod ≠ CLOSED); contract fence **ALIVE**; **no** Foundry `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

## 1. Standing pin

| Claim | Standing | Evidence |
|-------|----------|----------|
| Sovereign risk-register mount | **ADMITTED** | Fixture mount + provenance |
| R-01, R-03…R-05, R-07 body closure | **OPEN** (`AdmittedOpen`) | R-02 **CLOSED**. R-03: ALPN `aoq/1` + UDP-443. R-04: chromium WebCrypto↔SessionActor bind court (SessionActor PRESENT; bind ABSENT). R-05: `#154` → main. Others: Mandatory Resolution Contract |
| R-06 body closure | **CLOSED** | `0cff19a`/`7b2c181` + chicago **11**/EXIT:**0** |
| Ungrounded “closed” | **REFUSED_UNGROUNDED** | `resolve_closure(..., false)` |
| Contract fence | tip-cargo **ALIVE** | `r01_r07_resolution_court` |

## 2. Exact next (open IDs)

Per open ID in [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2: execute Mandatory Resolution Contract → typed receipt or `REFUSED_*` before **2027-01-01**.

Replay fence:

```bash
CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test r01_r07_resolution_court --locked \
  --config build.jobs=1 -- --test-threads=1
```

**Refuse:** inventing CLOSED; inventing Foundry / zero-HITL ALIVE from R-06.
