# brand_toml_hotswap_court — exact-retip4 honesty (v27.1.1)

```text
tip_base_admitted=8c8b1e6e5ed7314a4e37f3fdc6078e4383aecd26
cargo_tip=c4e757a14c91c6512438a8a94bbde304b0a6e921
land_tip=376d0d28350f275c6265bd91bd0f98e3eee94787
branch=cursor/v2711-brand-exact-retip4-bc54
court=brand_toml_hotswap_court
passed=6
failed=0
EXIT=0
standing=ALIVE (exact tip-cargo)
fence_ceiling=PARTIAL_ALIVE
production=UNSUPPORTED
foundry=not ALIVE
```

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING` |
| Branch | `cursor/v2711-brand-exact-retip4-bc54` (`bc-479d54bf`) |
| Admitted tip base | `8c8b1e6e5ed7314a4e37f3fdc6078e4383aecd26` (`~8c8b1e6e5+`) |
| Cargo evidence tip | `c4e757a14c91c6512438a8a94bbde304b0a6e921` — ancestor of land tip |
| Land tip | `376d0d28350f275c6265bd91bd0f98e3eee94787` (docs-only tip-wins OK; court blob ≡) |
| Court blob | `22e417ab307428fdcaf228059923fec644ce57c3` (≡ lineage `19c08a3b0`) |
| Target | `/tmp/only-fafc690f-tip3-courts` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; `--locked`; `SCCACHE_DISABLE=1` |
| Observed | **6 passed; 0 failed; EXIT:0** @ 2026-11-22T18:44:13Z |
| Fence ceiling | **PARTIAL_ALIVE** (receipts never claim `ALIVE`) |
| Production theme | **UNSUPPORTED** / `REFUSED_PRODUCTION` |
| Foundry / zero-HITL | **not ALIVE** |
| Crown absorb | **none** |
| ManagePullRequest | **not used** |
| Force-push | **none** |

**Authority.** Exact-tip `brand_toml_hotswap_court` EXIT **0** seals tip-cargo
**ALIVE (exact tip-cargo)**. See canonical pin
[`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md).
Does **not** mint production-theme `ALIVE` or Foundry crown `ALIVE`.

## Replay

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
# cargo evidence @ c4e757a14
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test brand_toml_hotswap_court --locked \
  --target-dir /tmp/ggen-brand-hotswap-exact-retip4 \
  --config build.jobs=1 -- --test-threads=1
# expect: 6 passed; EXIT:0
```

## Compact return

```text
tip_base=8c8b1e6e5ed7314a4e37f3fdc6078e4383aecd26
cargo_tip=c4e757a14c91c6512438a8a94bbde304b0a6e921
EXIT=0
standing=ALIVE (exact tip-cargo)
passed=6
production=UNSUPPORTED
foundry=not ALIVE
crown_absorb=none ManagePullRequest=not_used
```
