# `verify_witness` size court standing (tip-cargo ALIVE)

```text
exit=0 standing=ALIVE(size_court) passed=6 tip=513704d048fcf19dffe5fad82ba23a8b647b6830
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-verify-witness-alive-bc54` |
| Ecosystem tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `513704d048fcf19dffe5fad82ba23a8b647b6830` (witness surface ≡ `678a6c106` … crown) |
| Court | `crates/ggen-graph/tests/verify_witness_size_court.rs` — **6** `#[test]` **PRESENT** |
| API | `crates/ggen-graph/src/witness/mod.rs` **PRESENT** (`court_verify_witness_size`, budget `262_144`) |
| Placeholder | `crates/ggen-graph/fixtures/witness/verify_witness.placeholder.wasm` **35** bytes (`< 262144` / &lt;250KB) |
| Tip-cargo | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-verify-witness-target cargo test -p ggen-graph --test verify_witness_size_court --locked` → **6 passed; 0 failed; exit 0** (loadavg_1m ≤10; `/tmp/ggen-verify-witness-cargo.log`) |
| Ancestor cargo | `0478fde59` — **6 passed; exit 0** (retained) |
| Standing (size court) | **ALIVE** (lineage tip-cargo on crown `513704d04`; size court only) |
| Production WASM | **UNSUPPORTED** — [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) — never production-artifact `ALIVE` from placeholder fence |
| ManagePullRequest | not used |

## Replay

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-verify-witness-target \
  cargo test -p ggen-graph --test verify_witness_size_court --locked
```

Expect: **6 passed; exit 0** on tip witness sources (production path remains **UNSUPPORTED**).