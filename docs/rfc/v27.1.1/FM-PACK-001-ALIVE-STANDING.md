# FM-PACK-001 composed e2e — ALIVE (lineage tip-cargo) receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `FM-PACK-001-ALIVE-STANDING` |
| Branch | `cursor/v2711-fm-pack-001-alive-bc54` @ `ce613b05d` (court land); tip-wins absorb onto crown |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` Gate-1 Real-state pack (`packs/gh-terraform-pack`) |
| Evidence tip | `19c08a3b0c80b79d76f05dd59e61d1dc4c25fbdc` (past crown pin `11b0f9116`) |
| Court tip (cargo observed) | `a508e6bae9e61d7894ad98df8c507fdb1fb28eb2` (`bc-18c00b5e`) |
| Absorb tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `fbfaa3ad97bca1e14d4351480af0a37877acc0d8` (tip-wins; OOR retip9 advanced past court tip) |
| Repo | `seanchatmangpt/ggen` |
| Subject (`composed_packs_e2e` + `packs/gh-terraform-pack`) | **ALIVE** (lineage tip-cargo) |
| Presence court | **ALIVE** (`test -d packs/gh-terraform-pack` → **0**; tree `c7799cd6`) |

**Law:** Quiet tip-cargo `composed_packs_e2e` **2 passed; exit 0** under `CARGO_BUILD_JOBS=1` promotes FM-PACK-001 from **PRESENT** / **PARTIAL_ALIVE** to crown **ALIVE**. Label **ALIVE (lineage tip-cargo)** — cargo SHA / court tip `a508e6bae` ⊆ live crown; absorb tip advanced docs-only via OOR retip9 (`fbfaa3ad9`). Pack tree blob for `composed_packs_e2e.rs` is identical on evidence tip, court tip, and absorb tip (`07b828c377135f467a9739bc37de6818330958e5`).

**Falsifier:** inventing `ALIVE` from path presence alone; promoting while `loadavg_1m ≥ 10` (thrash); claiming production fleet DO from hermetic compose; inventing exact-tip-cargo after tip advanced past the cargo SHA.

---

## Tip-cargo evidence

| Command | Result | Standing |
|---------|--------|----------|
| Preflight: `loadavg_1m=5.01` (&lt;10) | gate passed | run admitted |
| `CARGO_BUILD_JOBS=1 cargo build -p ggen-cli-lib --bin ggen --target-dir /tmp/ggen-fm-pack-001-alive-target --config build.jobs=1 --config 'build.rustc-wrapper=""'` | **BUILD_EXIT=0** (23m 17s; log `/tmp/fm-pack-001-composed-e2e.log`) | bin for CliHarness |
| `CARGO_BUILD_JOBS=1 cargo test -p ggen-engine --test composed_packs_e2e --target-dir /tmp/ggen-fm-pack-001-alive-target --config build.jobs=1 --config 'build.rustc-wrapper=""' -- --nocapture` | **2 passed; 0 failed; exit 0** (test wall 25.18s; suite finish after cold compile) | **ALIVE** (lineage tip-cargo) |

Tests exercised (all **ok**):

- `thirty_packs_compose_into_one_consumer`
- `thirty_one_packs_compose_with_verify_gates_active`

Source: `crates/ggen-engine/tests/composed_packs_e2e.rs` (includes `gh-terraform-pack` in the composed set).

Env: `CXX=g++` `SCCACHE_DISABLE=1` `CARGO_INCREMENTAL=0`; rustc-wrapper cleared (repo `.cargo/config.toml` defaults to sccache). Worktree: `/home/ubuntu/ggen-fm-pack-001-alive-wt` (protected; `/tmp` WT was deleted mid-build by concurrent cleanup).

---

## Replay (exact)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
# evidence SHA 19c08a3b0; court tip a508e6bae; sources ≡ absorb tip for composed_packs_e2e.rs
test "$(awk '{print $1}' /proc/loadavg)" = "$(python3 -c 'import os; print("ok" if float(open("/proc/loadavg").read().split()[0])<10 else "BLOCKED_THRASH")')"
# if loadavg_1m >= 10: stop — BLOCKED_THRASH; do not thrash
CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 \
  cargo build -p ggen-cli-lib --bin ggen \
  --target-dir /tmp/ggen-fm-pack-001-alive-target \
  --config build.jobs=1 --config 'build.rustc-wrapper=""'
CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 \
  cargo test -p ggen-engine --test composed_packs_e2e \
  --target-dir /tmp/ggen-fm-pack-001-alive-target \
  --config build.jobs=1 --config 'build.rustc-wrapper=""' \
  -- --nocapture
# expect: 2 passed; exit 0
```

Documented short form (docs inventory): `timeout 600s cargo test -p ggen-engine --test composed_packs_e2e -- --nocapture` → exit **0** (extend timeout on cold target).

---

## Compact return line

```text
tip_before=fbfaa3ad97bca1e14d4351480af0a37877acc0d8
court_tip=a508e6bae9e61d7894ad98df8c507fdb1fb28eb2
evidence=19c08a3b0c80b79d76f05dd59e61d1dc4c25fbdc
loadavg_1m_start=5.01
filter=composed_packs_e2e
result=2_passed_exit_0
standing=ALIVE (lineage tip-cargo)
landed=cursor/v2711-fm-pack-001-alive-bc54@ce613b05d
absorb=tip-wins onto fbfaa3ad9 (OOR retip9 advanced past court tip; sources ≡)
next=parent owns #849 body; OOR remains BLOCKED (out of scope here)
```
