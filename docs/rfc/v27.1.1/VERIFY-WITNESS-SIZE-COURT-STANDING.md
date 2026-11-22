# `verify_witness` size court standing (Clause 3 retip)

```text
exit=skipped standing=PRESENT size=35 tip=8955b592183a2907749a50ece1ebbfd194fb2ad5
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-absorb-934-936-9130` (source `cursor/v2711-crown-progress2-bc54` / verify_witness ledger @ `0e25df932`) |
| Ecosystem tip | absorb onto tip `8cee4246f8939a84f75427f6468284f7932880e0` (~`8cee4246f+ / f350cb78e+`; tip-wins: ui_layout tip-cargo **ALIVE** @ `ca52e9c16`; chicago mirror evidence `f350cb78e`) |
| Court | `crates/ggen-graph/tests/verify_witness_size_court.rs` — **6** `#[test]` **PRESENT** |
| API | `crates/ggen-graph/src/witness/mod.rs` **PRESENT** (`court_verify_witness_size`, budget `262_144`) |
| Placeholder | `fixtures/witness/verify_witness.placeholder.wasm` **35** bytes (`< 262144` / &lt;250KB) |
| Ancestor cargo | `0478fde59` — `verify_witness_size_court` **6 passed; exit 0** (sources unchanged on tip for witness surface) |
| Prior session binary | `/tmp/gate1-finalbin-verify_witness_size_court.log` — **6 passed; exit 0** (warm binary path; not tip-exclusive package rebuild this pass) |
| Tip exclusive flock | `/tmp/ggen-uilayout-tip-alive-protected` **HELD** — cargo not taken |
| loadavg_1m at probe | ≤8 (would allow cargo if flock free) |
| Standing (size court) | **PRESENT** (docs-only tip-tree reconfirm). Tip-cargo package `ALIVE` **PENDING**. |
| Production WASM | **UNSUPPORTED** — [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) |
| ManagePullRequest | not used |

## Falsifier (when tip exclusive free)

```bash
CARGO_TARGET_DIR=/tmp/ggen-verify-witness-target CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test verify_witness_size_court --locked
```

Expect: **6 passed; exit 0** → standing promotes to tip-cargo **ALIVE** (size court only; production remains **UNSUPPORTED**).
