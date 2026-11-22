# `verify_witness` size court standing (Clause 3 retip)

```text
exit=skipped standing=PRESENT size=35 tip=84909a70c9f0bb66c2f3e6c8e4fe4d03397ee609
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-absorb-vw-927-928-1028` (source `cursor/v2711-verify-witness-retip-bc54` @ `34631db8a`) |
| Ecosystem tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `84909a70c9f0bb66c2f3e6c8e4fe4d03397ee609` (absorb onto `2fd897141+` / ≥ `27436608c` / `120598fb3`; #927/#928 already on tip) |
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
