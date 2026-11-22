# `verify_witness` size court standing (tip-cargo ALIVE)

```text
exit=0 standing=ALIVE(size_court) passed=6 tip=c91b225614a3ca0472303c05bab25601db1d51e1
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-verify-witness-tipalive-bc54` |
| Ecosystem tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c91b225614a3ca0472303c05bab25601db1d51e1` |
| Court | `crates/ggen-graph/tests/verify_witness_size_court.rs` — **6** `#[test]` **PRESENT** |
| API | `crates/ggen-graph/src/witness/mod.rs` **PRESENT** (`court_verify_witness_size`, budget `262_144`) |
| Placeholder | `crates/ggen-graph/fixtures/witness/verify_witness.placeholder.wasm` **35** bytes (`< 262144` / &lt;250KB) |
| Lock precondition | tip `59c090984` absorbed `Cargo.lock` `ggen-lsp` → `ggen-abb-sbb` path edge (`cursor/v2711-verify-witness-lock-alive-bc54`) so `--locked` resolves |
| Tip-cargo (cold) | subject `cd7c2433b` — `set -o pipefail; cargo test -p ggen-graph --test verify_witness_size_court --locked --target-dir /tmp/ggen-verify-witness-tip-ebf4 --config build.jobs=1 -- --test-threads=1` → **6 passed; EXIT=0** (`/tmp/cursor/verify-witness-tip-cd7c-build.log`) |
| Tip-cargo (reconfirm) | subject `c91b22561` (exact origin tip) — same command → **6 passed; EXIT=0** in **0.58s** (`/tmp/cursor/verify-witness-tip-c91b-reconfirm.log`) |
| Ancestor cargo | `0478fde59` — **6 passed; exit 0** (retained) |
| Prior non-tip EXIT:0 | `/workspace` / `/tmp/ggen-verify-witness-cargo.log` — **not** tip ALIVE (recorded; superseded) |
| Standing (size court) | **ALIVE** (exact tip-cargo on `c91b22561`; size court only) |
| Production WASM | **UNSUPPORTED** — [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) — never production-artifact `ALIVE` from placeholder fence |
| ManagePullRequest | not used |

## Replay

```bash
set -o pipefail
cd "$(git rev-parse --show-toplevel)"   # @ origin/cursor/ecosystem-spec-27-1-1-bc54 tip
CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --target-dir /tmp/ggen-verify-witness-tip-ebf4 \
  --config build.jobs=1 -- --test-threads=1
echo EXIT=$?
```

Expect: **6 passed; exit 0** on tip witness sources (production path remains **UNSUPPORTED**).
