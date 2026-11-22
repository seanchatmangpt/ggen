# `verify_witness` size court standing (Clause 3 retip)

```text
exit=skipped standing=PRESENT size=35 tip=3ec33983528ae0c168ffbd56f80cbd6df5353dff
```

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-verify-witness-absorb-c322` |
| Source | #929 / `cursor/v2711-verify-witness-retip-bc54` @ `34631db8a57baae32fed51e6ba811b337d639d2e` |
| Ecosystem tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2fd8971414519f5a1c51f6b04c1f7a85831def52` (≥ `46a026ae9` / `27436608c` / `120598fb3`) |
| Court | `crates/ggen-graph/tests/verify_witness_size_court.rs` — **6** `#[test]` **PRESENT** |
| API | `crates/ggen-graph/src/witness/mod.rs` **PRESENT** (`court_verify_witness_size`, budget `262_144`) |
| Placeholder | `crates/ggen-graph/fixtures/witness/verify_witness.placeholder.wasm` **35** bytes (`< 262144` / &lt;250KB) |
| Ancestor cargo | `0478fde59` — `verify_witness_size_court` **6 passed; exit 0** (sources unchanged on tip for witness surface) |
| Prior session binary | `/tmp/gate1-finalbin-verify_witness_size_court.log` — **6 passed; exit 0** (warm binary path; not tip-exclusive package rebuild this pass) |
| Tip exclusive flock | `/tmp/ggen-uilayout-tip-alive-protected` **HELD** — cargo not taken (preserve ui_layout tip-cargo **ALIVE**) |
| Standing (size court) | **PRESENT** (docs-only absorb onto tip past `46a026ae9`). Tip-cargo package `ALIVE` **PENDING**. |
| Tip-wins | absorb #936/#937 onto `15ae6ef16+`; ui_layout tip-cargo **ALIVE** retained; verify_witness stays **PRESENT** (not tip-cargo `ALIVE`) |
| Production WASM | **UNSUPPORTED** — [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) |
| ManagePullRequest | not used |
| Cargo this absorb | **no** |

## Falsifier (when tip exclusive free)

```bash
CARGO_TARGET_DIR=/tmp/ggen-verify-witness-target CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test verify_witness_size_court --locked
```

Expect: **6 passed; exit 0** → standing promotes to tip-cargo **ALIVE** (size court only; production remains **UNSUPPORTED**).
