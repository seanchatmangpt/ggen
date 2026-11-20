# ZD-128 — Misc CI typed-fail census remediation

Standing: 4 FIXED, 1 WAIVER-RECOMMENDED (env-missing)

Branch: v26-11-3-wave · heads witnessed: e4f231a55 / c6425e3cb · date 2026-11-04
Logs pulled via `gh run view --log-failed` / `--log` on the freshest runs at head.

## Disposition table

| Check | Run | Disposition | Root cause | Evidence |
|---|---|---|---|---|
| PCQ Marketplace ALIVE | 41861108617 (c6425e3cb) | **FIXED (workflow)** | `cargo build` died exit 101: repo-root `.cargo/config.toml` sets `rustc-wrapper = "sccache"`, no sccache binary on runner ("could not execute process `sccache`... os error 2"). The "evidence upload found nothing" is downstream — no artifacts exist because build never ran. | `.github/workflows/pcq-marketplace-alive.yml` job env now `RUSTC_WRAPPER: ""` + `SCCACHE_DISABLE: "1"` (cyberpunk-tv-platform.yml precedent comment pattern). Local: `RUSTC_WRAPPER="" cargo build --release -p ggen-cli-lib --bin ggen -v` from `examples/pcq-marketplace` → 0 sccache exec lines. |
| Book Chicago TDD | 41856385312 (e4f231a55) | **FIXED (regen-by-delete)** | `FM-WRITE-005` inside `test_discoverable_current_consumers_are_two_sync_idempotent`: `examples/cargo-cicd-verify/docs/cargo_cicd_reference.md` existed with differing content (committed copy missing trailing newline vs template output). | Regen-by-delete: `git rm` → `ggen sync run --skip-ea-admission` (ggen 26.11.3) → file rewritten (+ trailing newline). Post-fix dry-run: 0 planned writes; `ggen receipt verify` exit 0. |
| GBB Certification Program | 41856384907 (e4f231a55) | **FIXED (path fix)** | `verify.sh` step runs under `CARGO_TARGET_DIR=$GITHUB_WORKSPACE/target/tai-enterprise-rebuild`; `verify_cmd.py:153` hardcoded `crate/target/debug/tai-rebuild` → `BUILD_BROKEN: [Errno 2] No such file or directory: '...consumer/tai-enterprise-rebuild/target/debug/tai-rebuild'`, exit 3. | `verify_cmd.py` `blake()` now resolves the binary via new `target_dir()` helper honoring `CARGO_TARGET_DIR` (absolute or crate-relative). Verified with the real binary: built `tai-rebuild` under redirected `CARGO_TARGET_DIR` and called `blake()` → real digest `dbec2c987df0b622...` returned. |
| Cyberpunk TV Platform | 41856385085 (e4f231a55) | **FIXED (pin fix)** | npm install exit 1, silent (no error body in GHA log). Reproduced locally and bisected: `@unrdf/oxigraph@26.5.5` publishes `"@unrdf/v6-core": "workspace:*"` unresolved in its registry metadata → `npm error EUNSUPPORTEDPROTOCOL Unsupported URL Type "workspace:*"` — broken upstream publish. 26.4.8 pins `@unrdf/v6-core 26.4.4` (real version). | `packs/cyberpunk-tv-platform/templates/package.json.tera` pin 26.5.5 → 26.4.8; regen via `ggen sync` (receipt advanced); local `npm install` exit 0 (only an allowScripts warning). |
| ggen Self-Host Retrofit | 41856385303 (e4f231a55) | **WAIVER-RECOMMENDED (env-missing)** | Log unavailable ("log not found"); annotation: `System.IO.IOException: No space left on device` on the Actions runner — runner disk exhaustion, not repo content. Rerun issued on fresh runner (run 41856385303 re-queued 2026-11-04T16:11:33Z; sibling run 41861108480 also in progress at head c6425e3cb). | Runbook §6 waiver row fields: check=ggen Self-Host Retrofit; root_cause=env-missing; not_blocking_because=runner ENOSPC infra, repo content uninvolved, rerun dispatched; recorded_by=ZD-128 lane; date 2026-11-04. Coordinator confirms green rerun before merge gate. |

## Files touched (lane-owned)

- `.github/workflows/pcq-marketplace-alive.yml` (sccache bypass env)
- `packs/cyberpunk-tv-platform/templates/package.json.tera` (+ `.ggen-v2/receipt{,-log}.jsonl` sync advance)
- `examples/cargo-cicd-verify/docs/cargo_cicd_reference.md` (regen, trailing newline)
- `packs/tai-enterprise-rebuild-pack/templates/scripts/verify_cmd.py.tmpl` (source; the `consumer/` copy is generated and gitignored — regen-by-delete + re-lock advanced `examples/tai-enterprise-rebuild/ggen.lock`)
- this receipt

## Verification ladder

1. PCQ: verbose cargo build from the consumer dir with the new env — zero sccache execs (real cargo run, fresh/cached).
2. Book: regen-by-delete + sync; dry-run 0 planned writes; receipt verify exit 0.
3. GBB: template fix landed at the pack source; consumer tree regenerated (`ggen sync run` clean, receipt verify exit 0, ggen.lock re-locked); real binary build under redirected `CARGO_TARGET_DIR` + real `blake()` call returning a real digest (`dbec2c987df0b622...`).
4. Cyberpunk: template edit → `ggen sync` regen → real `npm install` exit 0.
5. Self-host: rerun dispatched; classification env-missing; unverified pending rerun conclusion (UNVERIFIED until coordinator confirms).
