# shui_public_prefix_court — exact-tip standing (v27.1.1)

```text
observed_crown_tip=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
court=shui_public_prefix_court
package=ggen-graph
cargo_attempt_tip=49ebdb27c35ca8c666796482510de481fde7fcd9
passed=UNKNOWN
failed=UNKNOWN
exit=101
standing=BUILD_BROKEN (exact tip-cargo attempt) — NOT ALIVE
lineage=ALIVE (lineage tip-cargo) @ 008a81d21 retained
```

**Do not promote false `ALIVE`.** Exact-tip cargo on the live crown did **not**
produce `test result: ok. 4 passed` under exclusive tip-cargo. Observed EXIT
**101** was `oxrocksdb-sys` custom build killed by **SIGTERM** under multi-agent
cargo thrash — not a court falsifier and not tip-subject fence `ALIVE`.

| Field | Value |
|-------|-------|
| Document ID | `SHUI-EXACT-TIP-STANDING` |
| Evidence branch | `cursor/v2711-shui-exact-tip-bc54` |
| Spec binding | Gate-1 / Truex fence — `shui:` public Turtle prefix |
| Live crown (doc land) | `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` |
| Cargo attempt tip | `49ebdb27c35ca8c666796482510de481fde7fcd9` (sources ≡ later docs-only tip-wins for `shui*` / `public_prefixes`) |
| Lineage evidence | **ALIVE (lineage tip-cargo)** — **4 passed; exit 0** @ crown `008a81d21` (`cursor/v2711-shui-prefix-alive-bc54`; `/tmp/ggen-shui-prefix-target2`; `/tmp/shui_public_prefix_court4.log`) |
| Exact-tip attempt | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip-9dd1` → EXIT **101** |
| Failure mode | `failed to run custom build command for oxrocksdb-sys v0.5.11` — build-script **signal 15 (SIGTERM)** |
| Log | `/tmp/cursor/shui-exact-tip-9dd1.log` · status `/tmp/cursor/shui-exact-tip-9dd1.status` |
| Prior stuck worker | `bc-7a769de9` / local `SHUI_EXIT=143` (also SIGTERM) — remote `cursor/v2711-shui-exact-tip-bc54` was **ABSENT** at coordination start |
| False EXIT:0 | `/tmp/cursor/shui-exact-tip-court.status` claimed `EXIT:0` but log has **no** `test result:` line — **refused** as evidence |
| ManagePullRequest | **not used** (parent owns #849) |
| Crown absorb | **not performed** (sidebranch only) |

**Law:** Lineage tip-cargo EXIT **0** does **not** equal exact tip-cargo `ALIVE`.
SIGTERM / thrash during `oxrocksdb-sys` is **BUILD_BROKEN** for the tip attempt,
not a vocabulary refusal. Truex product admit-court remains a separate subject.

---

## Source identity (fence blobs ≡ through tip-wins)

| Path | Blob @ live crown |
|------|-------------------|
| `crates/ggen-graph/tests/shui_public_prefix_court.rs` | `3cda065f6564917504f241ac3626c111cd2dfb6a` |
| `crates/ggen-graph/src/vocab/public_prefixes.rs` | `4e1dc0008ebdc44ec1d5078170e9c9ee0ae201ea` |
| `crates/ggen-graph/src/vocab/shui.rs` | `91ed7161b6f9381854fe79f0e36ea7823676bbf8` |

---

## Replay (exact tip-cargo)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
TIP=$(git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54)
# exclusive target; jobs=1; wait for cargo thrash to drain first
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --target-dir /tmp/ggen-shui-exact-tip-9dd1 \
  --config build.jobs=1 \
  -- --test-threads=1
# promote to ALIVE (exact tip-cargo) only on: 4 passed; EXIT:0 with test result line
```

---

## Compact return line

```text
crown_tip=167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f
cargo_attempt_tip=49ebdb27c35ca8c666796482510de481fde7fcd9
EXIT=101
standing=BUILD_BROKEN (SIGTERM oxrocksdb-sys) — NOT ALIVE (exact tip-cargo)
lineage=ALIVE @ 008a81d21
landed=cursor/v2711-shui-exact-tip-bc54
next=exclusive tip-cargo replay after thrash drain → expect 4 passed; EXIT:0
```


---

**Superseded for tip-subject standing by** [`SHUI-PUBLIC-PREFIX-TIP-CARGO-STANDING.md`](./SHUI-PUBLIC-PREFIX-TIP-CARGO-STANDING.md) (**STALLED** EXIT 137/143 on later tip). This BUILD_BROKEN pin retained as earlier thrash receipt.
