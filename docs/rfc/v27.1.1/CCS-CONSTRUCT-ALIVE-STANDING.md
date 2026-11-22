# CCS construct_hash_gate — tip-cargo ALIVE receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CCS-CONSTRUCT-ALIVE-STANDING` |
| Branch | `cursor/v2711-ccs-construct-alive-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 CCS #3 (`construct_hash_gate`) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c329b7ee5c15936f662f4eef996967bd66a6daa8` |
| Repo | `seanchatmangpt/ggen` |
| CCS #3 path (`construct_hash_gate` in-lib courts) | **ALIVE** |
| Clause-2 / A2A lifecycle crown | **PARTIAL_ALIVE** (`lifecycle.rs` **ABSENT**) |

**Law:** Tip-cargo exit 0 on `--lib construct_hash_gate` promotes CCS #3 refuse-court path to **ALIVE** on the admitted tip. BRCE V7/V1 replay and broker rows remain in [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md).

---

## Tip-cargo evidence

| Command | Result | Standing |
|---------|--------|----------|
| `CARGO_BUILD_JOBS=1 cargo test -p ggen-lsp --lib construct_hash_gate --target-dir /tmp/ggen-construct-hash-target --locked` | **4 passed; exit 0** (shared-pod window; log `/tmp/construct_hash_gate.out` 2026-11-22T13:03Z) | **ALIVE** |
| `cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court` | **not run this land** (loadavg_1m>10 → spared) | **PARTIAL_ALIVE** |
| `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` | **not run this land** (loadavg_1m>10 → spared) | **PARTIAL_ALIVE** |

Tests exercised (all **ok**):

- `empty_expected_hash_is_refused`
- `fabricated_hash_is_refused`
- `matching_hash_admits_construct_seal`
- `whitespace_expected_hash_is_refused`

---

## Replay

```bash
git checkout cursor/ecosystem-spec-27-1-1-bc54  # @ c329b7ee5+
CARGO_BUILD_JOBS=1 cargo test -p ggen-lsp --lib construct_hash_gate \
  --target-dir /tmp/ggen-construct-hash-target --locked
# expect: 4 passed; exit 0
```

---

## Compact return line

```text
construct_hash_gate=ALIVE(4;exit=0)
brce_v7_v1=spared(load>10)
tip=c329b7ee5c15936f662f4eef996967bd66a6daa8
landed=cursor/v2711-ccs-construct-alive-bc54
next=quiet load → brce_as01_es256_assertion_court + brce_as01_seat_court → 0
```