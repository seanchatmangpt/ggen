# CCS `ggen_write_apply` — tip-cargo ALIVE receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CCS-WRITE-APPLY-ALIVE-STANDING` |
| Branch | `cursor/v2711-write-apply-alive-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 CCS #2 (`ggen_write_apply` + Gall CP17) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c91b225614a3ca0472303c05bab25601db1d51e1` |
| Evidence land | `b5cb632c629538d8244432a82246dd302f695f1d` (FM-LAW-015 empty `ea.graph.json` fixture repair on tip) |
| Repo | `seanchatmangpt/ggen` |
| CCS #2 path (confirm + CP17 courts) | **ALIVE** |
| Clause-2 / Plane-5 / BRCE crown | **PARTIAL_ALIVE** — **V8**/**V9** OOR **BLOCKED** unchanged |

**Law:** Tip-cargo exit **0** on the three CCS #2 courts promotes the MCP `ggen_write_apply` CP17 path to **ALIVE** on the admitted tip. Fixture presence alone was never enough. BRCE / broker / Plane-5 rows remain in [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md). CCS #3 `construct_hash_gate` tip-cargo **ALIVE** is retained (see [`CCS-CONSTRUCT-ALIVE-STANDING.md`](./CCS-CONSTRUCT-ALIVE-STANDING.md)).

**Collision repair:** WO-01 / FM-LAW-015 refuses sync when `ea.graph.json` is absent. Courts seed the same minimal empty-graph fixture as `ggen-engine` `combinatorial_matrix` / `config_schema_dispatch_e2e` (zero candidate SBBs) — confirm/CP17 gates, not EA admission.

---

## Tip-cargo evidence

Target dir: `/tmp/ggen-write-apply-target`. Jobs=1. `--locked`. Tip base `c91b22561` + land `b5cb632c6`.

| # | Command | Result | Standing |
|---|---------|--------|----------|
| 1 | `CARGO_BUILD_JOBS=1 cargo test -p ggen-mcp --test write_apply_confirm_test --locked --config build.jobs=1 -- --test-threads=1` | **7 passed; 0 failed; exit 0** | **ALIVE** |
| 2 | `CARGO_BUILD_JOBS=1 cargo test -p ggen-mcp --test pipeline_tools_test --locked --config build.jobs=1 -- write_apply_refuses_a_fabricated_graph_hash_bypass --test-threads=1` | **1 passed; 10 filtered; exit 0** | **ALIVE** |
| 3 | `CARGO_BUILD_JOBS=1 cargo test -p ggen-mcp --test pipeline_tools_test --locked --config build.jobs=1 -- write_apply_accepts_a_real_dry_run_graph_hash --test-threads=1` | **1 passed; 10 filtered; exit 0** | **ALIVE** |

Log: `/tmp/write-apply-tip-c91b.log` (2026-11-22 restart `bc-f8c0be78`; EXIT=0; EXIT1=0 EXIT2a=0 EXIT2b=0; warm target `/tmp/ggen-write-apply-target`; prior `/tmp/cursor/write-apply-replay.out` retained).

Sources:

- `crates/ggen-mcp/tests/write_apply_confirm_test.rs`
- `crates/ggen-mcp/tests/pipeline_tools_test.rs`
- `crates/ggen-mcp/src/tools/write_apply.rs`

---

## Replay (exact)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
# tip @ c91b22561 + land fixture b5cb632c6 (or successor absorb)
export CARGO_TARGET_DIR=/tmp/ggen-write-apply-target
export CARGO_BUILD_JOBS=1
cargo test -p ggen-mcp --test write_apply_confirm_test --locked --config build.jobs=1 -- --test-threads=1
# expect: 7 passed; exit 0
cargo test -p ggen-mcp --test pipeline_tools_test --locked --config build.jobs=1 -- \
  write_apply_refuses_a_fabricated_graph_hash_bypass --test-threads=1
# expect: 1 passed; exit 0
cargo test -p ggen-mcp --test pipeline_tools_test --locked --config build.jobs=1 -- \
  write_apply_accepts_a_real_dry_run_graph_hash --test-threads=1
# expect: 1 passed; exit 0
```

---

## Compact return line

```text
tip=c91b225614a3ca0472303c05bab25601db1d51e1
evidence=b5cb632c629538d8244432a82246dd302f695f1d
v1=write_apply_confirm_test 7_passed_exit_0
v2a=write_apply_refuses_a_fabricated_graph_hash_bypass 1_passed_exit_0
v2b=write_apply_accepts_a_real_dry_run_graph_hash 1_passed_exit_0
landed=cursor/v2711-write-apply-alive-bc54
restart=bc-f8c0be78 EXIT=0 log=/tmp/write-apply-tip-c91b.log
next=lifecycle package courts / V8 V9 OOR BLOCKED; goal ACTIVE; OOR stays BLOCKED
```
